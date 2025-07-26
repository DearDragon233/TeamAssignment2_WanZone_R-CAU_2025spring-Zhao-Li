# 加载必要的库
library(terra)

# -------------------------------
# 1. 处理降水数据
# -------------------------------
# 指定包含TIF文件的目录
tif_path <- "Data/Resource/2000prec"
tif_files <- list.files(path = tif_path, pattern = "\\.tif$", full.names = TRUE)
r_list <- lapply(tif_files, rast)
r_stack <- rast(r_list)
precip_tif <- app(r_stack, fun = sum, na.rm = TRUE)
print("1. 降水数据处理完成")


# -------------------------------
# 2. 处理最高温度数据
# -------------------------------
tif_path <- "Data/Resource/2000tmax"
tif_files <- list.files(path = tif_path, pattern = "\\.tif$", full.names = TRUE)
rast_list <- lapply(tif_files, rast)
r_stack <- rast(rast_list)
temp_max_tif <- app(r_stack, fun = mean, na.rm = TRUE)
print("2. 最高温度数据处理完成")


# -------------------------------
# 3. 处理最低温度数据
# -------------------------------
tif_path <- "Data/Resource/2000tmin"
tif_files <- list.files(path = tif_path, pattern = "\\.tif$", full.names = TRUE)
rast_list <- lapply(tif_files, rast)
r_stack <- rast(rast_list)
temp_min_tif <- app(r_stack, fun = mean, na.rm = TRUE)
print("3. 最低温度数据处理完成")


# -------------------------------
# 4. 处理海拔数据
# -------------------------------
dem_files <- list.files("Data/Resource/DEM", pattern = "\\.tif$", full.names = TRUE)
dem_tiles <- lapply(dem_files, rast)
elev_tif <- do.call(mosaic, dem_tiles)
# 将无效值设为NA
elev_tif[elev_tif == -9999] <- NA
print("4. 海拔数据处理完成")


# ----------------------------------------------
# 5. 新增：处理人口密度数据
# ----------------------------------------------
# 载入人口密度 TIF 文件
pop_file <- "Data/Resource/Population/gpw_v4_population_density_rev11_2020_30_sec.tif"
pop_tif <- rast(pop_file)
# GPW v4 数据集通常用负值表示水体或无数据区域，将其设置为NA
pop_tif[pop_tif < 0] <- NA
print("5. 人口密度数据处理完成")


# -------------------------------
# 6. 导入并处理参考图像 (采用中心点法)
# -------------------------------
# 读取参考 TIFF 文件
ref_rast_raw <- rast("Data/Raw/glc2000_v1_1.tif")
# 手动添加坐标系信息
crs(ref_rast_raw) <- "EPSG:4326"

# 3 倍降采样，减少数据量。
# 使用中心点法 (取3x3网格的第5个像元)，以保留小比例或线性地物的原始信息。
ref_rast <- aggregate(ref_rast_raw, fact = 3, fun = function(x, ...) x[5])
print("7. 参考图像处理完成 (使用中心点采样法)")


# ----------------------------------------------------
# 7. 重采样所有图像并转换为矩阵
# ----------------------------------------------------
# 将所有待处理的栅格对象放入一个列表
rasters_to_process <- list(
  precipitation = precip_tif,
  tmax = temp_max_tif,
  tmin = temp_min_tif,
  elevation = elev_tif,
  population = pop_tif
)

# 定义每种数据的重采样方法
# 'bilinear' 用于连续数据, 'near' 用于分类数据
resample_methods <- c(
  precipitation = "bilinear",
  tmax = "bilinear",
  tmin = "bilinear",
  elevation = "bilinear",
  population = "bilinear"
)

# 初始化列表存储所有重采样后的矩阵
resampled_mats_list <- vector("list", length(rasters_to_process))
names(resampled_mats_list) <- names(rasters_to_process)

# 对每个栅格对象依次进行处理
for (i in seq_along(rasters_to_process)) {
  # 获取当前栅格对象、名称和方法
  current_name <- names(rasters_to_process)[i]
  current_rast <- rasters_to_process[[i]]
  current_method <- resample_methods[current_name]
  
  print(paste("开始重采样:", current_name, "使用方法:", current_method))
  
  # 统一投影坐标系
  if (!same.crs(current_rast, ref_rast)) {
    print(paste("... 投影不一致，正在重投影到", crs(ref_rast, proj=TRUE)))
    current_rast <- project(current_rast, ref_rast, method = current_method)
  }
  
  # 使用resample()函数使栅格网格与参考对象对齐
  r_resampled <- resample(current_rast, ref_rast, method = current_method)
  
  # 裁剪以确保范围完全一致
  r_resampled <- crop(r_resampled, ext(ref_rast))
  
  # 转换为矩阵
  mat <- as.matrix(r_resampled, wide = TRUE)
  
  # 计算并设置经纬度作为矩阵的列名和行名
  longitudes <- terra::xFromCol(r_resampled, 1:ncol(r_resampled))
  latitudes <- terra::yFromRow(r_resampled, 1:nrow(r_resampled))
  colnames(mat) <- longitudes
  rownames(mat) <- latitudes
  
  # 将处理好的矩阵存入列表
  resampled_mats_list[[current_name]] <- mat
  print(paste("完成:", current_name))
}
print("8. 所有数据重采样并转换为矩阵完成")


# -------------------------------
# 8. 存储矩阵
# -------------------------------
# 定义输出文件名，与列表名称对应
output_files <- c(
  precipitation = "precipitation_mat.RData",
  tmax = "tmax_mat.RData",
  tmin = "tmin_mat.RData",
  elevation = "elevation_mat.RData",
  population = "population_mat.RData"
)

# 循环存储为 .RData 文件
for (name in names(resampled_mats_list)) {
  mat <- resampled_mats_list[[name]]
  output_path <- paste0("Data/Processed/", output_files[name])
  save(mat, file = output_path)
  print(paste("矩阵已保存到:", output_path))
}
print("9. 所有矩阵存储完成！")