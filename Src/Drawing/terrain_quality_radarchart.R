# 加载R包
library(terra)
library(fmsb)
library(gridGraphics)
library(grid)

# -------------------------------
# 1. 加载 TIF 文件，提取像素值及计算各项指标
# -------------------------------
# 加载 TIF 文件
tif <- rast("Data/Raw/glc2000_v1_1.tif")
terrain_values <- values(tif)

# 统计每个类别的频数和概率分布
terrain_freq <- table(terrain_values)
terrain_prob <- terrain_freq / sum(terrain_freq)

# 计算 Shannon 熵
entropy <- -sum(terrain_prob * log2(terrain_prob))

# 计算最大类别占比
dominance_ratio <- max(terrain_prob)

# 计算 Simpson 多样性指数（值越高表示多样性越大）
simpson <- 1 - sum(terrain_prob^2)

# 计算 Gini 系数
gini_index <- function(p) {
  n <- length(p)
  diff_matrix <- outer(p, p, FUN = function(x, y) abs(x - y))
  sum(diff_matrix) / (2 * n)
}
gini <- gini_index(as.numeric(terrain_prob))

# -------------------------------
# 2. 构造绘图数据（只保留四个指标）
# -------------------------------
# 这里熵归一化时假定理论最大熵为 log2(24)，可根据实际情况调整
quality_metrics <- data.frame(
  Gini     = gini,
  最大占比 = dominance_ratio,
  熵       = entropy / log2(24),
  Simpson  = simpson
)

# 构造 radarchart() 所需的数据格式：
# 第一行：所有指标的最大值（这里设定均为 1）
# 第二行：所有指标的最小值（这里设定均为 0）
# 第三行：实际的指标值
quality_metrics <- rbind(rep(1, 4), rep(0, 4), quality_metrics)

# -------------------------------
# 3. 使用 radarchart 绘制雷达图
# -------------------------------
# 导出图片
png("Plots/数据质量雷达图.png", width = 924, height = 682, res = 150)

# 绘图
radarchart(quality_metrics,
           axistype = 1,
           pcol = "darkorange", 
           pfcol = rgb(1, 0.5, 0.2, 0.3),
           plwd = 2,
           cglcol = "grey80", 
           cglty = 1,
           axislabcol = "grey40", 
           vlcex = 0.9,
           title = "S4.2地形质量指标雷达图")

dev.off()

# 储存为grid图像用于拼接
radarchart(quality_metrics,
           axistype = 1,
           pcol = "darkorange", 
           pfcol = rgb(1, 0.5, 0.2, 0.3),
           plwd = 2,
           cglcol = "grey80", 
           cglty = 1,
           axislabcol = "grey40", 
           vlcex = 0.9,
           title = "S4.2地形质量指标雷达图")

# 将刚才的 base 图形复制到 grid 图形系统中
grid.echo()
tqr <- grid.grab()

# grid.draw(tqr)
