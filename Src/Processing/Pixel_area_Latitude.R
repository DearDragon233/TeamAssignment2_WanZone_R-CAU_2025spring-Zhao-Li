# 加载R包
library(terra)   # 用于处理栅格数据

# 读取.tif文件
tif_file <- rast("Data/Raw/glc2000_v1_1.tif")

# 手动添加坐标系信息
crs(tif_file) <- "EPSG:4326"

# 手动指定经纬度范围与像素对应信息（来自文件"glc2000_v1_1_projinfo.hdr"）
# 经纬度范围
ext(tif_file) <- ext(-180.000000, 179.991070, -56.008928, 89.991071)
# 像素对应范围
res(tif_file) <- c(0.0089285714, 0.0089285714)

# 2 倍降采样，减少数据量
tif_file <- aggregate(tif_file, fact=2)

# 返回每个像素的面积（单位：平方米）
pixel_area <- cellSize(tif_file)

# 可视化检查（导出PNG）
png("Plots/pixel_area_map.png", width=2000, height=1500, res=300)
plot(pixel_area/1e6,main="像素面积分布图(km²)",
     xlab="经度/°", ylab="纬度/°")
dev.off()  # 关闭设备

# 注意到并经了解，此投影下同纬度下像素对应面积相同
# 为提升存储效率，只存第一列（一维向量）
area_vector <- unlist(pixel_area[,1],use.names = F)

# 保存为.RData文件
save(area_vector, file = "Data/Processed/Pixel_area_Latitude.RData")
