# 筛选农田数据
df_farm <- subset(df, farm == "farm")

# 定义函数：根据分箱数计算中点后生成带中点标签的因子
convert_bin_mid <- function(x, bins = 30) {
  breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = bins + 1)
  mids <- round((head(breaks, -1) + tail(breaks, -1)) / 2, 2)
  # 利用中点作为 labels
  cut(x, breaks = breaks, include.lowest = TRUE, labels = mids)
}

# 利用 convert_bin_mid() 生成各变量的分箱，并直接用中点数字作为因子标签
df_farm$lon_bin        <- convert_bin_mid(df_farm$lon, bins = 30)
df_farm$lat_bin        <- convert_bin_mid(df_farm$lat, bins = 30)
df_farm$precip_bin     <- convert_bin_mid(df_farm$precip, bins = 30)
df_farm$elev_bin       <- convert_bin_mid(df_farm$elev, bins = 30)
df_farm$temp_min_bin   <- convert_bin_mid(df_farm$temp_min, bins = 30)
df_farm$temp_max_bin   <- convert_bin_mid(df_farm$temp_max, bins = 30)
df_farm$temp_range_bin <- convert_bin_mid(df_farm$temp_range, bins = 30)

# 定义需要绘制的分箱变量名
bins <- c("lon_bin", "lat_bin", "precip_bin", "elev_bin",
          "temp_min_bin", "temp_max_bin", "temp_range_bin")

# 为每个变量生成一个柱状图，使用aes映射中增加weight=area，实现以农田面积为直方图高度
plot_list <- lapply(bins, function(var) {
  # 取出该变量的因子水平向量
  levels_vec <- levels(df_farm[[var]])
  # 隔7个水平取一次刻度
  selected_breaks <- levels_vec[seq(1, length(levels_vec), by = 7)]
  
  ggplot(df_farm, aes_string(x = var, weight = df_farm$area)) +
    geom_bar(fill = "coral", alpha = 1, color = "black") +
    labs(title = var) +
    scale_x_discrete(drop = FALSE, breaks = selected_breaks) +  # 仅显示选定刻度
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.title = element_blank())
})

# 将各图合并显示
combined_plot <- wrap_plots(plot_list)
print(combined_plot)

ggsave(filename = "Plots/农田各维度柱状图.png", width = 10, height = 10)