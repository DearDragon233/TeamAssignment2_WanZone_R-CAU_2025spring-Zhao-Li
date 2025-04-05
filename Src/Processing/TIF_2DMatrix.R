#加载R包
library(reshape2)
library(terra)     # 用于处理栅格数据

# 读取 .tif 并降低分辨率
r <- rast("Data/Raw/glc2000_v1_1.tif")

# 2 倍降采样，减少数据量
r <- aggregate(r, fact=2)

# 转换为数据框
df <- as.data.frame(r, xy = TRUE)

# 确保列名正确
names(df) <- c("x", "y", "value")

# 转换为二维矩阵 (y 为行，x 为列)
mat <- acast(df, y ~ x, value.var = "value")

# 查看矩阵的维度
dim(mat)

# 保存为.RData文件
save(mat, file = "Data/Processed/TIF_2DMatrix.RData")
