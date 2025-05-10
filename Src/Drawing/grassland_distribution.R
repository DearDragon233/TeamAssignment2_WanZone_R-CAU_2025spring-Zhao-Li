# 加载R包
library(ggplot2)
library(reshape2)
library(rnaturalearth)
library(rnaturalearthdata)

# 加载数据
load("Data/Processed/TIF_2DMatrix.RData") # 变量名mat

# 草地类别值
grassland_values <- c(13, 14, 15)

# 转换为布尔矩阵
is_grassland <- matrix(mat %in% grassland_values,
                       nrow = nrow(mat),
                       ncol = ncol(mat))

# 添加行列名用于经纬度映射
rownames(is_grassland) <- rownames(mat)
colnames(is_grassland) <- colnames(mat)

# 获取大陆轮廓数据，用于作图
world <- ne_countries(scale = "medium", returnclass = "sf")

# 设置采样的间隔
sampling_interval <- 10

# 采样矩阵
mat_sampled <- is_grassland[seq(1, nrow(is_grassland), by = sampling_interval),
                            seq(1, ncol(is_grassland), by = sampling_interval)]

# 转化为长格式
df <- melt(mat_sampled)
names(df) <- c("latitude", "longitude", "是否为草地")

# 使用 ggplot2 绘制热图
ggplot() +
  geom_sf(data = world, fill = "#ECECEC", color = NA, size = 0.5) +
  geom_raster(data = df, mapping = aes(x = longitude, y = latitude, fill = 是否为草地)) +
  scale_fill_manual(values = c("FALSE" = NA, "TRUE" = "Green"), na.value = NA) +
  labs(title = "S4.4(b)世界草地分布图（低分辨率）",
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

# 保存为图像
ggsave("Plots/grassland_distribution.png", width = 9, height = 5)
