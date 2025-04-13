#加载R包
library(reshape2)
library(terra)     # 用于处理栅格数据

# 读取 .tif 
r <- rast("Data/Raw/glc2000_v1_1.tif")

# 手动添加坐标系信息
crs(r) <- "EPSG:4326"

# 手动指定经纬度范围与像素对应信息（来自文件"glc2000_v1_1_projinfo.hdr"）
# 经纬度范围
ext(r) <- ext(-180.000000, 179.991070, -56.008928, 89.991071)

# 2 倍降采样，减少数据量（使用众数方法，防止出现小数或错误）
r_lowres <- aggregate(r, fun = "modal", fact=2)


# 默认输出是小数，进行取整
r_lowres <- round(r_lowres)

# 转换为矩阵，保留图像中的排列格式，注意应按行填充
mat <- matrix(r_lowres,nrow = nrow(r_lowres),byrow = T)

# 计算每行中心经度、纬度
longitudes <- terra::xFromCol(r_lowres, 1:ncol(r_lowres))
latitudes <- terra::yFromRow(r_lowres, 1:nrow(r_lowres))

# 将经纬度作为矩阵行列名
colnames(mat) <- longitudes
rownames(mat) <- latitudes

# 保存为.RData文件
save(mat, file = "Data/Processed/TIF_2DMatrix.RData")
