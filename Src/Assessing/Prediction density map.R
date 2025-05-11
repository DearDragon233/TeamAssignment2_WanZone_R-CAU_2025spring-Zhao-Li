# 加载R包
library(dplyr)  # 用于整理语法

# 构造数据框：包含预测的“farm”概率和真实标签
density_df <- data.frame(prob_farm = pred_farm_prob,
                         label    = test_df$farm)

# 定义 x 轴完整范围和想绘制的区间
full_xlim <- c(0, 1)
lower <- min(density_df$prob_farm[density_df$label == "farm"])
upper <- max(density_df$prob_farm[density_df$label == "nonfarm"])

# 预计算各类别的核密度
density_data <- density_df %>%
  group_by(label) %>%
  do({
    dens <- density(.$prob_farm, from = full_xlim[1], to = full_xlim[2], adjust = 1.5)
    data.frame(x = dens$x, y = dens$y)
  }) %>%
  ungroup()

# 仅保留 x 在 [lower, upper] 内的数据
density_data_filtered <- density_data %>%
  filter(x >= lower, x <= upper)

PredictionDensityMap1 <- ggplot() +
  # 绘制填充区域
  geom_area(
    data = density_data_filtered, 
    aes(x = x, y = y, fill = label), 
    alpha = 0.5,
    position = 'identity'
  ) +
  geom_line(
    data = density_data_filtered, 
    aes(x = x, y = y, color = label, group = label),
    size = 1
  ) +
  labs(title = element_blank(),
       x = "",
       y = "") +
  # 显示完整的 0~1 范围
  scale_x_continuous(limits = full_xlim) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#print(PredictionDensityMap1)


# 绘制 density 图纵坐标待用
PredictionDensityAixs1 <- ggplot() +
  # 绘制填充区域
  geom_area(data = density_data_filtered, 
            aes(x = x, y = y, fill = label), 
            alpha = 0,
            position = 'identity') +
  scale_x_continuous(limits = full_xlim, expand = c(0, 0)) +
  labs(title = element_blank(),
       x = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none",
        title = element_text(color = "transparent"),
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# 绘制 density 图，展示全部
PredictionDensityMap2 <- ggplot(density_df, aes(x = prob_farm, fill = label)) +
  geom_density(alpha = 0.5) +
  labs(title = "预测 'farm' 概率的密度图",
       x = "预测农田概率",
       y = "密度") +
  theme_minimal() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.8, 0.2),
        legend.justification = c("left", "bottom"),
        axis.text.y = element_blank())

#print(PredictionDensityMap2)

# 绘制 density 图纵坐标待用
PredictionDensityAixs <- ggplot(density_df, aes(x = prob_farm, fill = label)) +
  geom_density(alpha = 0, colour = NA) +
  labs(title = "",
       x = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none",
        title = element_text(color = "transparent"),
        axis.text.x = element_text(color = "transparent"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#print(PredictionDensityAixs)
