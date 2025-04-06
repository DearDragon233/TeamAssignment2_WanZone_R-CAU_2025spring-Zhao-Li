# ==== 1. 加载数据 ====
load("Data/Processed/Pixel_area_Latitude.RData")  # 确保文件路径正确
# 读取 Excel 文件，假设你已将 Global_Legend.xls 放在合适路径下
library(readxl)
terrain_data <- read_excel("Global_Legend.xls")

# 检查 area_matrix 和 terrain_data 的长度是否匹配
if (length(area_matrix) > nrow(terrain_data)) {
  area_matrix <- area_matrix[1:nrow(terrain_data)]
  warning("area_matrix 长度超过地形数据行数，已截取前 ", nrow(terrain_data), " 个元素。")
} else if (length(area_matrix) < nrow(terrain_data)) {
  terrain_data <- terrain_data[1:length(area_matrix), ]
  warning("area_matrix 长度小于地形数据行数，已截取地形数据前 ", length(area_matrix), " 行。")
}

# ==== 2. 生成极坐标变量 ====
theta <- seq(0, 6 * pi, length.out = length(area_matrix))  # 控制旋转圈数（这里是 3 圈）
r <- scale(area_matrix)[, 1] + 5  # 对数据标准化并整体上移，避免负值

# ==== 3. 转换为笛卡尔坐标系 ====
polar_x <- r * cos(theta)
polar_y <- r * sin(theta)

# 为每个点关联地形信息
associated_terrain <- terrain_data$CLASSNAMES

# 定义颜色映射，每种地形对应一种颜色
terrain_colors <- rainbow(length(unique(associated_terrain)))
names(terrain_colors) <- unique(associated_terrain)
point_colors <- terrain_colors[match(associated_terrain, names(terrain_colors))]

# ==== 4. 绘图 ====
plot(polar_x, polar_y, type = "l",
     col = point_colors, lwd = 2,
     xlab = "从南向北的纬度映射 - 水平方向",
     ylab = "从南向北的纬度映射 - 垂直方向",
     asp = 1,
     main = "极坐标中的纬度像素面积与地形之舞")
grid(col = "gray80", lty = 2)

# 添加图例
legend("topright", legend = names(terrain_colors), fill = terrain_colors, title = "地形类型")

