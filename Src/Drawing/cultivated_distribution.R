# 加载R包
library(ggplot2)
library(reshape2)
library(rnaturalearth)
library(rnaturalearthdata)

# 加载数据
load("Data/Processed/TIF_2DMatrix.RData") # 植被分布信息，变量 mat

# 地形值值
cultivated_values <- c(16)

# 设置采样间隔
sampling_interval <- 30

# 采样矩阵
mat_sampled <- mat[seq(1, nrow(mat), by = sampling_interval),
                   seq(1, ncol(mat), by = sampling_interval)]

# 转换为布尔矩阵
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

# 获取大陆轮廓数据
world <- ne_countries(scale = "medium", returnclass = "sf")

# 导出为 PNG
png("Plots/cultivated_distribution.png", width = 924, height = 684, res = 150)

# 绘图
ggplot() +
  geom_sf(data = world, fill = "#ECECEC", color = NA, size = 0.5) +
  geom_raster(data = melted_mat, aes(x = longitude, y = latitude, alpha = is_cultivated), fill = "#F4A300") +
  scale_alpha_manual(
    values = c("FALSE" = 0, "TRUE" = 1),
    name = "是否为农田"
  ) +
  labs(
    x = "经度",
    y = "纬度",
    title = "世界农田分布图（低分辨率）"
  ) +
  scale_x_continuous(
    breaks = seq(-180, 180, by = 30),
    labels = function(x) {
      ifelse(x >= 0, paste0(x, "°E"), paste0(abs(x), "°W"))
    },
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(-90, 90, by = 30),
    labels = function(y) {
      ifelse(y >= 0, paste0(y, "°N"), paste0(abs(y), "°S"))
    },
    expand = c(0, 0)
  ) +
  coord_sf(expand = FALSE) +
  theme_minimal() +
  theme(
    legend.position = "right",  # ✅ 图例位置设置为右侧
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.8, "cm"),
    panel.grid.major = element_line(color = "gray80", linewidth = 0.3),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.1),
    panel.ontop = TRUE
  )

# 关闭画图设备
dev.off()



