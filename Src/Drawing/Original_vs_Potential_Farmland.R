# -------------------------------
# 1. 加载必要 R 包
# -------------------------------
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(reshape2)

# 2. 获取世界大陆地图数据，用于作图
world <- ne_countries(scale = "medium", returnclass = "sf")

# 3. 定义阈值
threshold <- potential_threshold

# 4. 构造分类矩阵
# 初始化与 veg_mat 同尺寸的矩阵
class_mat <- matrix(NA, nrow = nrow(veg_mat), ncol = ncol(veg_mat), dimnames = list(rownames(veg_mat),colnames(veg_mat)))

# 原始农田（veg_mat == 16）标记为 1
class_mat[veg_mat == 16] <- 1

# 非农田区域中，预测潜力大于阈值标记为 2（潜在农田）
class_mat[(farm_potential_matrix >= threshold) & (veg_mat != 16)] <- 2

# 5. 数据采样与转换格式
# 为了加快绘图速度，这里采用采样步骤
sampling_interval <- 10
mat_sampled <- class_mat[seq(1, nrow(class_mat), by = sampling_interval),
                         seq(1, ncol(class_mat), by = sampling_interval)]

# 将采样后的矩阵转为长格式数据框
df_plot <- melt(mat_sampled)
names(df_plot) <- c("latitude", "longitude", "Category")

# 移除 NA 值（既不属于原始农田也不属于潜在农田的区域）
df_plot <- df_plot[!is.na(df_plot$Category), ]

# 将 Category 转换为因子，并赋予标签
df_plot$Category <- factor(df_plot$Category, levels = c(1, 2),
                           labels = c("原始农田", "潜在农田"))

# 6. 直接绘图
p <- ggplot() +
  # 绘制大陆底图
  geom_sf(data = world, fill = "#ECECEC", color = NA, size = 0.5) +
  # 叠加农田类别信息
  geom_raster(data = df_plot, mapping = aes(x = longitude, y = latitude, fill = Category)) +
  # 手动设置颜色：绿色表示原始农田，红色表示潜在农田
  scale_fill_manual(values = c("原始农田" = "darkgreen", "潜在农田" = "#D55E00")) +
  labs(title = "S5.2.2 原始农田与潜在农田分布图",
       x = "经度",
       y = "纬度") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "inside",               # 将图例置于图形内部
        legend.position.inside = c(0.05, 0.2),       # 指定内部位置：靠左下
        legend.justification = c("left", "bottom")) +  # 图例对齐方式
  geom_sf(data = world, fill = NA, color = "grey", size = 0.5)

# 显示图形
print(p)

# -------------------------------
# 7. 保存图形到本地
# -------------------------------
ggsave("Plots/原始农田vs潜在农田.png", plot = p, width = 9, height = 5)
