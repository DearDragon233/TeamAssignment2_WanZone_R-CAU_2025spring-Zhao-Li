# 加载R包
library(ggplot2)
library(reshape2)
library(rnaturalearth)
library(rnaturalearthdata)  # 用于加载地图数据

# 加载数据
load("Data/Processed/TIF_2DMatrix.RData") # 植被分布信息，变量 mat

# 地形值值
cultivated_values <- c(16)

# 转换为布尔矩阵
is_cultivated <- matrix(mat %in% cultivated_values,
                        nrow = nrow(mat),
                        ncol = ncol(mat))

# 添加行列名用于经纬度映射
rownames(is_cultivated) <- rownames(mat)
colnames(is_cultivated) <- colnames(mat)

# 获取大陆轮廓数据，用于作图
world <- ne_countries(scale = "medium", returnclass = "sf")


# 设置采样的间隔
sampling_interval <- 10

# 采样矩阵
mat_sampled <- is_cultivated[seq(1, nrow(is_cultivated), by = sampling_interval),
                             seq(1, ncol(is_cultivated), by = sampling_interval)]

# 转化为长格式
df <- melt(mat_sampled)
names(df) <- c("latitude", "longitude", "是否为农田")

# 使用 ggplot2 绘制热图
cd <- ggplot() +
  geom_sf(data = world, fill = "#ECECEC", color = NA, size = 0.5) +
  geom_raster(data = df, mapping = aes(x = longitude, y = latitude, fill = 是否为农田)) +
  scale_fill_manual(values = c("FALSE" = NA, "TRUE" = "#FF73E7"), na.value = NA) +
  labs(title = "S4.4(c)世界农田分布图（低分辨率）",
       x = "经度",
       y = "纬度") +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(limits = c(min(df$latitude), max(df$latitude)), expand = c(0, 0)) +
  theme_minimal() +
  theme(legend.position = "inside",               # 将图例置于图形内部
        legend.position.inside = c(0.05, 0.2),       # 指定内部位置：靠左下
        legend.justification = c("left", "bottom")) +  # 图例对齐方式
  geom_sf(data = world, fill = NA, color = "grey", size = 0.5)

# 保存为图像
ggsave("Plots/cultivated_distribution.png", plot = cd, width = 9, height = 5)
