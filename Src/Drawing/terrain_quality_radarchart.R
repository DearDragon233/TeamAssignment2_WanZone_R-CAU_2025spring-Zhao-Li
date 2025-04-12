library(terra)  # 或 raster
tif <- rast("Data/Raw/glc2000_v1_1.tif")  # 加载 TIF

# 提取像素值并统计频率
terrain_values <- values(tif)
terrain_freq <- table(terrain_values)
terrain_prob <- terrain_freq / sum(terrain_freq)

# Shannon 熵
entropy <- -sum(terrain_prob * log2(terrain_prob))

# 最大类别占比
dominance_ratio <- max(terrain_prob)

# 有效类别数
effective_classes <- 2^entropy

library(ggplot2)

entropy_val <- entropy
effective_classes <- 2^entropy

ggplot(data.frame(x = c("熵", "有效类别数"),
                  y = c(entropy_val, effective_classes)),
       aes(x = x, y = y, fill = x)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(y, 2)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("#6baed6", "#fd8d3c")) +
  theme_minimal(base_size = 14) +
  labs(title = "地形多样性指标", x = NULL, y = NULL)


# Gini系数（不严格，但可参考）
terrain_freq <- table(terrain_values)
terrain_prob <- terrain_freq / sum(terrain_freq)  # 归一化

# 正确计算 Gini 系数
gini_index <- function(p) {
  n <- length(p)
  sum_i <- sum(outer(p, p, function(x, y) abs(x - y)))
  sum_i / (2 * n)
}
gini <- gini_index(as.numeric(terrain_prob))

library(fmsb)

# 指标准备
quality_metrics <- data.frame(
  Gini = gini,
  最大占比 = dominance_ratio,
  熵 = entropy / log2(24),  # 归一化
  有效类别 = effective_classes / 24  # 归一化
)
# 添加最大值和最小值行作为雷达图格式要求
quality_metrics <- rbind(rep(1, 4), rep(0, 4), quality_metrics)

radarchart(quality_metrics,
           axistype = 1,
           pcol = "darkorange", pfcol = rgb(1, 0.5, 0.2, 0.3),
           plwd = 2,
           cglcol = "grey80", cglty = 1,
           axislabcol = "grey40", vlcex = 0.9,
           title = "地形质量指标雷达图")

