library(ggplot2)
pie_plot <- ggplot(area_df, aes(x = "", y = Area, fill = Category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  # 添加标签：显示百分比和面积，文本颜色根据 label_color 列设置
  geom_text(aes(label = label, color = label_color),
            position = position_stack(vjust = 0.5), size = 4,
            show.legend = FALSE) +
  scale_color_identity() +
  coord_polar(theta = "y") +
  labs(title = "S5.5.1 总陆地面积内农田与潜在农田占比", fill = "类别") +
  theme_void() +
  scale_fill_manual(values = c("原始农田" = "darkgreen",
                               "潜在农田" = "red",
                               "其他土地" = "grey"))

# 保存饼图
ggsave("Plots/farm_area_pie_chart.png", plot = pie_plot, width = 6, height = 6)
