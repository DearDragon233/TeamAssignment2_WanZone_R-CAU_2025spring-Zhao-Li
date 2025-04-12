library(terra)
library(dplyr)

# === 1. 快速合并所有 DEM 图层 ===
dem_files <- list.files("Data/Resource/DEM", pattern = "\\.tif$", full.names = TRUE)
dem_tiles <- lapply(dem_files, rast)
dem_merged <- do.call(mosaic, dem_tiles)  # 一步完成合并！

# === 2. 降低 DEM 分辨率（降低内存+文件体积，若不需要可删）===
dem_lowres <- aggregate(dem_merged, fact = 2, fun = mean)  # fact越大分辨率越低

# === 3. 读取并处理气候数据函数 ===
read_climate_stack <- function(folder, stat = c("mean", "sum")) {
  files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
  s <- rast(files)
  result <- if (stat == "mean") mean(s) else sum(s)
  resample(result, dem_lowres, method = "bilinear")  # 重采样到 DEM 分辨率
}

# === 4. 加载气候数据 ===
tmin_2000 <- read_climate_stack("Data/Resource/2000tmin", "mean")
tmax_2000 <- read_climate_stack("Data/Resource/2000tmax", "mean")
prec_2000 <- read_climate_stack("Data/Resource/2000prec", "sum")

# === 5. 合并所有图层并转为 data.frame ===
stack <- c(dem_lowres, tmin_2000, tmax_2000, prec_2000)
names(stack) <- c("elevation", "tmin", "tmax", "precipitation")


for (start_row in seq(1, nrows_total, by = chunk_size)) {
  end_row <- min(start_row + chunk_size - 1, nrows_total)
  cat("📦 正在处理行", start_row, "到", end_row, "...\n")
  
  ymin <- yFromRow(stack, end_row)
  ymax <- yFromRow(stack, start_row)
  
  # 重建一个新的 extent（经度范围保持不变）
  orig_ext <- ext(stack)
  ext_block <- ext(orig_ext[1], orig_ext[2], ymin, ymax)
  
  # 裁剪栅格数据
  stack_block <- crop(stack, ext_block)
  
  # 转为 dataframe 并精简数值
  df_block <- as.data.frame(stack_block, xy = TRUE, na.rm = TRUE) %>%
    mutate(across(elevation:precipitation, ~round(., 1)))
  
  result_list[[length(result_list) + 1]] <- df_block
  gc()  # 清理内存
}


# === 3. 合并保存为压缩 RData ===
climate_df <- bind_rows(result_list)
save(climate_df, file = "Data/Processed/climate_2000_final.RData", compress = "xz")
str(climate_df)      # 查看结构：列名、类型、示例值
names(climate_df)    # 查看列名
head(climate_df)     # 查看前几行
nrow(climate_df)     # 查看行数
ncol(climate_df)     # 查看列数
summary(climate_df)  # 简要统计（可看是否有 NA、极值等）
