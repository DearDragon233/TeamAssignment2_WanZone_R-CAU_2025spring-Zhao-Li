# 1. 加载数据
load("Data/Processed/TIF_2DMatrix.RData")

# 2. 获取矩阵维度
n_lat <- dim(mat)[1]  # 8176 (纬度方向)
n_lon <- dim(mat)[2]  # 20160 (经度方向)

# 3. 生成真实的经纬度序列
lon_seq <- seq(from = -180, to = 180, length.out = n_lon)  # 经度
lat_seq <- seq(from = -90, to = 90, length.out = n_lat)    # 纬度

# 4. 确保索引严格匹配
library(reshape2)
df <- melt(mat)  # 展开为 long format

# 5. 重命名列名
colnames(df) <- c("lat_index", "lon_index", "value")

# 6. 强制 `df$lon_index` 和 `df$lat_index` 在合法范围内

df$lon_index <- pmax(1, pmin(df$lon_index, n_lon))  # 限制在 [1, n_lon]
df$lat_index <- pmax(1, pmin(df$lat_index, n_lat))  # 限制在 [1, n_lat]

# 7. 使用 `match()` 强制索引匹配经纬度
df$lon <- lon_seq[df$lon_index]
df$lat <- lat_seq[df$lat_index]

# 8. 检查数据一致性
print(dim(df))  # 应该等于 8176 × 20160
summary(df$lon)
summary(df$lat)

# 9. 画图
library(ggplot2)
ggplot(df, aes(x = lon, y = lat, fill = value)) +
  geom_raster() +
  scale_fill_viridis_c() +
  labs(title = "2D 地理数据可视化", x = "经度", y = "纬度") +
  theme_minimal()
