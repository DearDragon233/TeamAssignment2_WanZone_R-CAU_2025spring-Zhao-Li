# 加载所需的包
library(stars)
library(tidyverse)

# 定义草地相关区域的 VALUE
grassland_values <- c(13, 14, 18)

# 定义颜色数据
color_data <- data.frame(
  VALUE = c(1:23),
  CLASSNAMES = c("Tree Cover, broadleaved, evergreen",
                 "Tree Cover, broadleaved, deciduous, closed",
                 "Tree Cover, broadleaved, deciduous, open",
                 "Tree Cover, needle-leaved, evergreen",
                 "Tree Cover, needle-leaved, deciduous",
                 "Tree Cover, mixed leaf type",
                 "Tree Cover, regularly flooded, fresh water",
                 "Tree Cover, regularly flooded, saline water",
                 "Mosaic: Tree Cover / Other natural vegetation",
                 "Tree Cover, burnt",
                 "Shrub Cover, closed - open, evergreen",
                 "Shrub Cover, closed - open, deciduous",
                 "Herbaceous Cover, closed - open",
                 "Sparse herbaceous or sparse shrub cover",
                 "Regularly flooded shrub and/or herbaceous cover",
                 "Cultivated and managed areas",
                 "Mosaic: Cropland / Tree Cover / Other natural vege",
                 "Mosaic: Cropland / Shrub and/or grass cover",
                 "Bare Areas",
                 "Water Bodies",
                 "Snow and Ice",
                 "Artificial surfaces and associated areas",
                 "No data"),
  Red = c(0, 0, 0.682352941, 0.541176471, 0.8, 0.545098039, 0.466666667, 0, 0, 0, 0.996078431, 0.996078431, 0.996078431, 0.866666667, 0, 0.996078431, 0.996078431, 0.788235294, 0.701960784, 0.537254902, 0.937254902, 0.996078431, 0.996078431),
  Green = c(0.39, 0.584313725, 0.996078431, 0.266666667, 0.494117647, 0.741176471, 0.584313725, 0.274509804, 0.898039216, 0, 0.462745098, 0.698039216, 0.91372549, 0.788235294, 0.584313725, 0.874509804, 0.454901961, 0.537254902, 0.701960784, 0.88627451, 0.937254902, 0, 0.996078431),
  Blue = c(0, 0, 0.384313725, 0.070588235, 0.37254902, 0, 0.996078431, 0.780392157, 0, 0, 0, 0, 0.615686275, 0.62745098, 0.584313725, 0.894117647, 0.905882353, 0.996078431, 0.701960784, 0.996078431, 0.937254902, 0, 0.996078431)
)

# 提取草地的颜色信息
grassland_colors <- color_data %>% 
  filter(VALUE %in% grassland_values) %>% 
  select(Red, Green, Blue)

# 读取已上色的 tif 文件
tif_file <- "Data/Raw/glc2000_v1_1.tif"  # 请替换为实际的 tif 文件路径
raster_data <- read_stars(tif_file)

# 检查波段数量
num_bands <- length(st_get_dimensions(raster_data)$band)
if (num_bands < 3) {
  stop(paste("tif 文件的波段数量不足 3 个，当前波段数量为", num_bands))
}

# 假设 tif 文件有三个波段分别对应红、绿、蓝通道
raster_r <- raster_data[1]
raster_g <- raster_data[2]
raster_b <- raster_data[3]

# 创建一个空的逻辑矩阵来存储草地分布
grassland_mask <- matrix(FALSE, nrow = dim(raster_r)[1], ncol = dim(raster_r)[2])

# 遍历草地颜色
for (i in 1:nrow(grassland_colors)) {
  red_value <- grassland_colors$Red[i]
  green_value <- grassland_colors$Green[i]
  blue_value <- grassland_colors$Blue[i]
  
  # 找到与当前草地颜色匹配的像素
  current_mask <- (raster_r == red_value) & (raster_g == green_value) & (raster_b == blue_value)
  
  # 更新草地分布矩阵
  grassland_mask <- grassland_mask | current_mask
}

# 将逻辑矩阵转换为 stars 对象，并设置维度信息
grassland_distribution <- st_as_stars(grassland_mask, dimensions = st_dimensions(raster_r))

# 保存结果为新的 tif 文件
write_stars(grassland_distribution, "grassland_distribution.tif", overwrite = TRUE)
