library(ggplot2)
library(reshape2)

# 加载数据
load("Data/Processed/TIF_2DMatrix.RData")

# 找出草地对应的 VALUE
grassland_values <- c(13,14,15)

# 匹配矩阵中的草地值
is_grassland <- mat %in% grassland_values

# 确保 is_grassland 是矩阵
if (!is.matrix(is_grassland)) {
  is_grassland <- as.matrix(is_grassland)
}

# 将矩阵转换为数据框
melted_mat <- melt(is_grassland, varnames = c("latitude", "longitude"), value.name = "is_grassland")

# 检查 melted_mat 的结构
print(str(melted_mat))
print(dim(melted_mat))

# 绘制草地分布热力图
ggplot(melted_mat, aes(x = longitude, y = latitude, fill = is_grassland)) +
  geom_raster() +
  scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "green"), name = "是否为草地") +
  labs(x = "经度", y = "纬度", title = "草地分布热力图") +
  theme_minimal()

