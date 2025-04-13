# 加载所需的包
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggplot2)
library(devtools)

# 检查并安装 rnaturalearthhires 包
if (!requireNamespace("rnaturalearthhires", quietly = TRUE)) {
  devtools::install_github("ropensci/rnaturalearthhires")
}
library(rnaturalearthhires)

# 定义处理栅格数据的函数
process_raster_data <- function(raster_path, fact = 5) {
  tryCatch({
    # 加载 TIFF 栅格文件
    raster_data <- rast(raster_path)
    
    # 检查栅格数据是否成功加载
    if (!is(raster_data, "SpatRaster")) {
      stop("未能成功加载栅格数据。")
    }
    
    # 降低栅格数据分辨率
    raster_data <- aggregate(raster_data, fact = fact)
    
    return(raster_data)
  }, error = function(e) {
    message(paste("处理栅格数据时出现错误:", e$message))
    return(NULL)
  })
}

# 主程序部分
# 读取 TIFF 地图并处理分辨率
raster_path <- "D:/r course/learnR/Topic4/glc2000_v1_1_Tiff/Tiff/rgb_image.tif"
processed_raster <- process_raster_data(raster_path)

if (!is.null(processed_raster)) {
  # 读取中国.geojson数据
  china_boundary <- st_read("D:/r course/learnR/Topic4/glc2000_v1_1_Tiff/Tiff/中国.geojson")
  
  # 检查并转换坐标系为WGS 84（EPSG:4326），如果需要的话
  if (st_crs(china_boundary) != 4326) {
    china_boundary <- st_transform(china_boundary, crs = 4326)
  }
  
  # 提取中国部分（使用读取的geojson数据）
  china_agri <- mask(processed_raster, vect(china_boundary))
  
  # 将栅格数据转换为数据框（用于 ggplot2）
  raster_df <- as.data.frame(china_agri, xy = TRUE, na.rm = TRUE)
  colnames(raster_df) <- c("x", "y", "value")
  
  # 绘制地图
  ggplot() +
    # 绘制提取并降低分辨率后的农业区分布地图
    geom_raster(data = raster_df, aes(x = x, y = y, fill = value)) +
    # 绘制省区划线（使用读取的geojson数据）
    geom_sf(data = china_boundary, fill = NA, color = "black") +
    scale_fill_viridis_c() +
    theme_minimal() +
    labs(title = "中国农业区分布（降低分辨率）", fill = "农业区指数")
}