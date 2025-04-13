library(terra) 

# 读取tif
tif_file <- rast("Data/Raw/glc2000_v1_1.tif")

# 手动添加坐标系信息
crs(tif_file) <- "EPSG:4326"

# 手动指定经纬度范围
ext(tif_file) <- ext(-180.000000, 179.991070, -56.008928, 89.991071)

# 降倍采样
tif_file <- aggregate(tif_file, fact=2)

# 返回每个像素的面积
pixel_area <- cellSize(tif_file)

# 可视化
png("Plots/pixel_area_map.png", width=2000, height=1500, res=300)
plot(pixel_area/1e6,main="像素面积分布图(km²)",
     xlab="经度/°", ylab="纬度/°")
dev.off()  # 关闭设备

# 提升存储效率，只存第一列（一维向量）
area_vector <- unlist(pixel_area[,1],use.names = F)

# 计算每行中心纬度
latitudes <- terra::yFromRow(pixel_area, 1:nrow(pixel_area))

# 将纬度作为向量元素名称
names(area_vector) <- latitudes

# 保存为.RData文件
save(area_vector, file = "Data/Processed/Pixel_area_Latitude.RData")
