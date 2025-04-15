library(ggplot2)
library(reshape2)

# 加载数据
load("Data/Processed/TIF_2DMatrix.RData") #植被分布信息，变量mat

# 草地类别值
cultivated_values <- c(16)

# 设置采样间隔
sampling_interval <- 10

# 采样矩阵
mat_sampled <- mat[seq(1, nrow(mat), by = sampling_interval),
                   seq(1, ncol(mat), by = sampling_interval)]

# 转换为布尔矩阵（注意保留矩阵维度）
is_cultivated <- matrix(mat_sampled %in% cultivated_values,
                       nrow = nrow(mat_sampled),
                       ncol = ncol(mat_sampled))

# 添加行列名用于经纬度映射
rownames(is_cultivated) <- rownames(mat_sampled)
colnames(is_cultivated) <- colnames(mat_sampled)

# 转为长格式数据框
melted_mat <- melt(is_cultivated, varnames = c("latitude", "longitude"), value.name = "is_cultivated")

# 转换为数值型经纬度
melted_mat$latitude <- as.numeric(as.character(melted_mat$latitude))
melted_mat$longitude <- as.numeric(as.character(melted_mat$longitude))

# 导出为PNG
png("Plots/cultivated_distribution.png", width = 924, height = 684, res = 150)

# 绘图
ggplot(melted_mat, aes(x = longitude, y = latitude, alpha = is_cultivated)) +
  geom_raster(fill = "#F4A300") +
  scale_alpha_manual(values = c("FALSE" = 0, "TRUE" = 1), name = "是否为农田") +
  labs(x = "经度", y = "纬度", title = "世界农田分布图（低分辨率）") +
  coord_fixed(ratio = 1.3) +
  #scale_x_continuous(expand = c(0, 0)) +
  #scale_y_continuous(expand = c(0, 0)) +  # 移除坐标轴额外留白
  theme_minimal()

# 关闭画图设备
dev.off()
