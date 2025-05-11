# 利用 test_df 中已经包含 obs_farm 和 farm 字段，以及预测概率变量（pred_farm_prob）：
test_df$pred_farm_prob <- pred_farm_prob

predictbox <- ggplot(test_df, aes(x = farm, y = pred_farm_prob, fill = farm, colour = farm)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  labs(title = element_blank(),x = "",y = "") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),) +
  coord_flip()
