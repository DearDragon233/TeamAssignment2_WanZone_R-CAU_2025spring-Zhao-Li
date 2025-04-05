#加载R包
library(reshape2)
library(terra)     # 用于处理栅格数据

# 读取 .tif 并降低分辨率
r <- rast("Data/Raw/glc2000_v1_1.tif")

# 2 倍降采样，减少数据量
r <- aggregate(r, fact=2)

# 转换为矩阵，保留图像中的排列格式
mat <- matrix(r,nrow = nrow(r))

# 处理在转化过程中出现的小数，进行取整
mat <- round(mat)

# 查看矩阵的维度
dim(mat)

# 保存为.RData文件
save(mat, file = "Data/Processed/TIF_2DMatrix.RData")
