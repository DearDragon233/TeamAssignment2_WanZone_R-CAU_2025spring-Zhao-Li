library(ggplot2)
library(reshape2)
library(rnaturalearth)
library(rnaturalearthdata)

# 加载数据
load("Data/Processed/TIF_2DMatrix.RData")

# 森林类别值（1 - 10）
forest_values <- 1:10

# 对应 RGB 颜色
forest_colors <- c(
  "1" = rgb(0, 0.39, 0),
  "2" = rgb(0, 0.584313725, 0),
  "3" = rgb(0.682352941, 0.996078431, 0.384313725),
  "4" = rgb(0.541176471, 0.266666667, 0.070588235),
  "5" = rgb(0.8, 0.494117647, 0.37254902),
  "6" = rgb(0.545098039, 0.741176471, 0),
  "7" = rgb(0.466666667, 0.584313725, 0.996078431),
  "8" = rgb(0, 0.274509804, 0.780392157),
  "9" = rgb(0, 0.898039216, 0),
  "10" = rgb(0, 0, 0)
)

# 采样以减少图像大小
sampling_interval <- 10
mat_sampled <- mat[seq(1, nrow(mat), by = sampling_interval),
                   seq(1, ncol(mat), by = sampling_interval)]

# 保留森林区域
is_forest <- matrix(mat_sampled %in% forest_values,
                    nrow = nrow(mat_sampled),
                    ncol = ncol(mat_sampled))

forest_indices <- which(is_forest, arr.ind = TRUE)
forest_values_at_indices <- mat_sampled[forest_indices]

# 纬度经度（转换为数值索引）
latitudes <- as.numeric(rownames(mat_sampled))[forest_indices[, 1]]
longitudes <- as.numeric(colnames(mat_sampled))[forest_indices[, 2]]

# 创建数据框
melted_mat <- data.frame(
  latitude = latitudes,
  longitude = longitudes,
  forest_value = forest_values_at_indices
)
melted_mat$color <- forest_colors[as.character(melted_mat$forest_value)]

# 图例
legend_data <- data.frame(
  forest_value = factor(forest_values, levels = 1:10),
  forest_name = c("T,b,e", "T,b,d,c", "T,b,d,o", "T,n,e", "T,n,d",
                  "T,m", "T,r,f", "T,r,s", "M", "burnt"),
  color = forest_colors[as.character(1:10)]
)

# 大陆轮廓
world <- ne_countries(scale = "medium", returnclass = "sf")

# 导出为 PNG
png("Plots/forest_distribution.png", width = 924, height = 684, res = 150)

# 绘图
ggplot() +
  geom_sf(data = world, fill = "#ECECEC", color = NA, size = 0.5) +
  geom_tile(data = melted_mat, aes(x = longitude, y = latitude, fill = factor(forest_value))) +
  scale_fill_manual(
    values = forest_colors,
    name = "森林类型",
    breaks = legend_data$forest_value,
    labels = legend_data$forest_name
  ) +
  labs(x = "经度", y = "纬度", title = "世界森林分布图（低分辨率）") +
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
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "white", color = "gray50"),
    legend.key.size = unit(0.8, "cm"),
    panel.grid.major = element_line(color = "gray80", linewidth = 0.3),
    panel.grid.minor = element_line(color = "gray90", linewidth = 0.1),
    panel.ontop = TRUE
  )

# 关闭画图设备
dev.off()

