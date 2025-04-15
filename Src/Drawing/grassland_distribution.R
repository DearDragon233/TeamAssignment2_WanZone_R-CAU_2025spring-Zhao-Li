library(ggplot2)
library(reshape2)

# 加载数据
load("Data/Processed/TIF_2DMatrix.RData")
# 草地类别值
grassland_values <- c(13, 14, 15)
# 设置采样的间隔
sampling_interval <- 10
# 采样矩阵
mat_sampled <- mat[seq(1, nrow(mat), by = sampling_interval),
                   seq(1, ncol(mat), by = sampling_interval)]
# 转换为布尔矩阵
is_grassland <- matrix(mat_sampled %in% grassland_values,
                       nrow = nrow(mat_sampled),
                       ncol = ncol(mat_sampled))
# 添加行列名用于经纬度映射
rownames(is_grassland) <- rownames(mat_sampled)
colnames(is_grassland) <- colnames(mat_sampled)
# 转为长格式数据框
melted_mat <- melt(is_grassland, varnames = c("latitude", "longitude"), value.name = "is_grassland")
# 转换为数值型经纬度
melted_mat$latitude <- as.numeric(as.character(melted_mat$latitude))
melted_mat$longitude <- as.numeric(as.character(melted_mat$longitude))

# 导出为PNG
png("Plots/grassland_distribution.png", width = 924, height = 684, res = 150)

# 绘图
ggplot(melted_mat, aes(x = longitude, y = latitude, alpha = is_grassland)) +
  geom_raster(fill = "forestgreen") +
  scale_alpha_manual(values = c("FALSE" = 0, "TRUE" = 1),  name = "是否为草地") +
  labs(x = "经度", y = "纬度", title = "世界草地分布图（低分辨率）") +
  coord_fixed(ratio = 1.3) +
  theme_minimal()

# 关闭画图设备
dev.off()
