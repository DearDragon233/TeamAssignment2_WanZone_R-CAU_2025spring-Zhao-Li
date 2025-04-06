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
# 像素对应范围
res(r) <- c(0.0089285714, 0.0089285714)

# 2 倍降采样，减少数据量
r <- aggregate(r, fact=2)

# 转换为矩阵，保留图像中的排列格式，注意应按行填充
mat <- matrix(r,nrow = nrow(r),byrow = T)

# 处理在转化过程中出现的小数，进行取整
mat <- round(mat)

# 查看矩阵的维度
dim(mat)

# 保存为.RData文件
save(mat, file = "Data/Processed/TIF_2DMatrix.RData")
