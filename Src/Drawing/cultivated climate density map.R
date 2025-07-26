# 加载必要包
library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)  # 用于百分比格式化

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
df_farm$lon_bin         <- convert_bin_mid(df_farm$lon, bins = 30)
df_farm$lat_bin         <- convert_bin_mid(df_farm$lat, bins = 30)
df_farm$precip_bin      <- convert_bin_mid(df_farm$precip, bins = 30)
df_farm$elev_bin        <- convert_bin_mid(df_farm$elev, bins = 30)
df_farm$pop_bin         <- convert_bin_mid(df_farm$pop, bins = 30)
df_farm$slope_bin       <- convert_bin_mid(df_farm$slope, bins = 30)
df_farm$temp_min_bin    <- convert_bin_mid(df_farm$temp_min, bins = 30)
df_farm$temp_max_bin    <- convert_bin_mid(df_farm$temp_max, bins = 30)
df_farm$temp_range_bin  <- convert_bin_mid(df_farm$temp_range, bins = 30)

# 定义需要绘制的分箱变量名
bins <- c("lon_bin", "lat_bin", "precip_bin", "elev_bin", "pop_bin", "slope_bin",
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
combined_plot <- wrap_plots(plot_list) +
  plot_annotation(title = "S5.2.1 农田各维度柱状图")

ggsave(filename = "Plots/农田各维度柱状图.png", plot = combined_plot, width = 10, height = 10)



# 1. 对全数据生成分箱变量
df$lon_bin        <- convert_bin_mid(df$lon, bins = 30)
df$lat_bin        <- convert_bin_mid(df$lat, bins = 30)
df$precip_bin     <- convert_bin_mid(df$precip, bins = 30)
df$elev_bin       <- convert_bin_mid(df$elev, bins = 30)
df$pop_bin        <- convert_bin_mid(df$pop, bins = 30)
df$slope_bin      <- convert_bin_mid(df$slope, bins = 30)
df$temp_min_bin   <- convert_bin_mid(df$temp_min, bins = 30)
df$temp_max_bin   <- convert_bin_mid(df$temp_max, bins = 30)
df$temp_range_bin <- convert_bin_mid(df$temp_range, bins = 30)

# 定义需要绘制的分箱变量名
bins <- c("lon_bin", "lat_bin", "precip_bin", "elev_bin", "pop_bin", "slope_bin",
          "temp_min_bin", "temp_max_bin", "temp_range_bin")

# 2. 为每个分箱变量计算各分箱中农田面积占总体面积的比例，并生成柱状图
plot_list <- lapply(bins, function(var) {
  
  # 对每个分箱统计：总面积（全部陆地）与农田面积（farm=="farm"）  
  df_summary <- df %>%
    group_by(!!sym(var)) %>%
    summarise(total_area = sum(area, na.rm = TRUE),
              farm_area  = sum(if_else(farm == "farm", area, 0), na.rm = TRUE)) %>%
    mutate(ratio = farm_area / total_area)
  
  # 提取完整的分箱标签（因子水平），并选择部分作为 x 轴刻度显示（你原代码中每隔 7 个显示一次）
  levels_vec <- levels(df[[var]])
  selected_breaks <- levels_vec[seq(1, length(levels_vec), by = 7)]
  
  # 生成柱状图（使用 stat="identity" 直接绘制预先计算的比例）
  ggplot(df_summary, aes_string(x = var, y = "ratio")) +
    geom_bar(stat = "identity", fill = "coral", alpha = 1, color = "black") +
    labs(title = paste0(var, " - 农田占比"),
         y = "农田面积占比") +
    scale_x_discrete(drop = FALSE, breaks = selected_breaks) +
    scale_y_continuous(labels = percent) +
    theme_minimal() +
    theme(axis.text.y = element_text())
})

# 3. 合并所有图并保存
combined_plot <- wrap_plots(plot_list) +
  plot_annotation(title = "S5.2.1 农田各维度面积比例柱状图")

ggsave(filename = "Plots/农田各维度面积比例柱状图.png", plot = combined_plot, width = 10, height = 10)
