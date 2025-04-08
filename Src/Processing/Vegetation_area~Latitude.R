# 加载R包
library(readxl)
library(reshape2) # 用于转换为长格式
library(ggplot2)  # 伟大，无需多言（
library(scales)   # 用于颜色转换
library(dplyr)    #用于区间分类

# 加载像素面积信息和植被分布信息
load("Data/Processed/Pixel_area_Latitude.RData")  # 像素面积，对象为area_vector
load("Data/Processed/TIF_2DMatrix.RData")         # 植被分布，对象为mat

# 加载图例文件
leg <- read_excel("Data/Raw/Global_Legend.xls")

# 计算每种植被在各纬度所占像素数
row_counts <- apply(mat, 1, function(x) table(factor(x, levels = 1:23)))

# 转置矩阵，保留原格式
row_counts <- t(row_counts)

# 新建一个矩阵，用于存储每种植被在各维度所占面积
lat_area <- matrix(0,nrow = nrow(row_counts),ncol = ncol(row_counts))
colnames(lat_area) <- leg$CLASSNAMES

# 每行乘以面积向量的对应元素算出面积
for(rows in 1:nrow(row_counts))
    lat_area[rows,] <- row_counts[rows,] * area_vector[rows]

# 计算各纬度总面积，用于统计
lat_area <- cbind(lat_area,area_vector * ncol(mat))
colnames(lat_area)[24] <- "Gross areas"

# 计算各纬度总陆地面积（总面积-海洋面积）
lat_area <- cbind(lat_area,lat_area[,24]-lat_area[,20])
colnames(lat_area)[25] <- "Land areas"

# 计算占比
land_fraction <- lat_area[,c(1:19,21:23)] / lat_area[,25]

# 转换为数据框，并添加纬度信息
land_fraction_df <- as.data.frame(land_fraction)
land_fraction_df$Latitude <- as.numeric(rownames(lat_area))  # 直接使用行名作为纬度

# 保存为.RData文件
save(lat_area, file = "Data/Processed/lat_area.RData")                  # 植被类型面积~纬度
save(land_fraction_df, file = "Data/Processed/land_fraction_df.RData")  # 植被类型占陆地比例~纬度
