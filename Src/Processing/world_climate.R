library(terra)


# -------------------------------
# 1. 处理降水数据
# -------------------------------
# 指定包含TIF文件的目录
tif_path <- "Data/Resource/2000prec"

# 列出所有 TIF 文件
tif_files <- list.files(path = tif_path, pattern = "\\.tif$", full.names = TRUE)

# 载入所有 TIF 文件
r_list <- lapply(tif_files, rast)

# 将所有栅格图层合并成一个栅格堆栈
r_stack <- rast(r_list)

# 对每个格点求和
precip_tif <- app(r_stack, fun = sum, na.rm = TRUE)


# -------------------------------
# 2. 处理最高温度数据
# -------------------------------
# 指定包含TIF文件的目录路径
tif_path <- "Data/Resource/2000tmax"

# 列出目录中所有的 TIF 文件（确保文件后缀为 .tif）
tif_files <- list.files(path = tif_path, pattern = "\\.tif$", full.names = TRUE)

# 载入所有 TIF 文件
rast_list <- lapply(tif_files, rast)

# 将所有栅格图层合并成一个栅格堆栈
r_stack <- rast(rast_list)

# 计算每个格点的平均值
temp_max_tif <- app(r_stack, fun = mean, na.rm = TRUE)


# -------------------------------
# 3. 处理最低温度数据
# -------------------------------
# 指定包含TIF文件的目录路径
tif_path <- "Data/Resource/2000tmin"

# 列出目录中所有的 TIF 文件（确保文件后缀为 .tif）
tif_files <- list.files(path = tif_path, pattern = "\\.tif$", full.names = TRUE)

# 载入所有 TIF 文件
rast_list <- lapply(tif_files, rast)

# 将所有栅格图层合并成一个栅格堆栈
r_stack <- rast(rast_list)

# 计算每个格点的平均值
temp_min_tif <- app(r_stack, fun = mean, na.rm = TRUE)


# -------------------------------
# 4. 处理海拔数据
# -------------------------------
# 载入所有 TIF 文件
dem_files <- list.files("Data/Resource/DEM", pattern = "\\.tif$", full.names = TRUE)

# 将所有栅格图层合并成一个栅格堆栈
dem_tiles <- lapply(dem_files, rast)

# 合并图像
elev_tif <- do.call(mosaic, dem_tiles)

elev_tif[elev_tif == -9999] <- NA


# -------------------------------
# 5. 导入并处理参考图像
# -------------------------------
# 读取参考 TIFF 文件（第一个文件）
r <- rast("Data/Raw/glc2000_v1_1.tif")

# 手动添加坐标系信息
crs(r) <- "EPSG:4326"

# 3 倍降采样，减少数据量（使用3倍点采样法，保证不改变面积比例与相对位置）
ref_rast <- aggregate(r, fact=3, fun = function(x) x[5])


# -------------------------------
# 6. 重采样另外四个图像并转换为矩阵
# -------------------------------
# 将另外四个 raster 对象放入一个列表
other_rasters <- list(precip_tif, temp_max_tif, temp_min_tif, elev_tif)

# 初始化列表存储重采样后的结果
resampled_list <- vector("list", length(other_rasters))

# 对每个对象依次进行处理
for (i in seq_along(other_rasters)) {
  # 当前对象
  r <- other_rasters[[i]]
  
  # 检查当前 raster 的投影和几何信息（分辨率、范围）的对齐情况
  if (!compareGeom(r, ref_rast, stopOnError = FALSE)) {
    # 如果几何信息不一致，则通过 project() 函数转换至参考对象的投影系统
    r <- project(r, ref_rast)
  }
  
  # 采用重采样的方法使当前对象与参考对象网格对齐
  # 注意：连续数据推荐使用 "bilinear" 双线性插值；若为分类数据，请改用 "near"
  r_resampled <- resample(r, ref_rast, method = "bilinear")
  
  # 裁剪（如果需要）使范围与参考对象一致
  r_resampled <- crop(r_resampled, ext(ref_rast))
  
  # 转换为矩阵，保留图像中的排列格式，注意应按行填充
  mat <- matrix(r_resampled, nrow = nrow(r_resampled), byrow = T)
  
  # 计算每行中心经度、纬度
  longitudes <- terra::xFromCol(r_resampled, 1:ncol(r_resampled))
  latitudes <- terra::yFromRow(r_resampled, 1:nrow(r_resampled))
  
  # 将经纬度作为矩阵行列名
  colnames(mat) <- longitudes
  rownames(mat) <- latitudes
  
  # 存入列表
  resampled_list[[i]] <- mat
}


# -------------------------------
# 7. 存储矩阵
# -------------------------------
# 将文件名存为向量
output_names <- c("precipitation_mat.RData",
                  "tmax_mat.RData",
                  "tmin_mat.RData",
                  "elevation_matrix_reduced.RData")

# 用循环分别存储为.RData
for (i in seq_along(resampled_list)){
  mat <- resampled_list[[i]]  # 先赋值到具体变量，保证正常save
  save(mat, file = paste0("Data/Processed/",output_names[i]))
}
