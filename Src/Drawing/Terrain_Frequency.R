library(ggplot2)
library(readxl)
library(cowplot)

# 读取上传文件获取 value 和对应的十六进制颜色代码
excel_data <- read_excel('Data/Resource/色彩.xlsx')

# 直接用列索引提取列
value <- as.numeric(as.character(excel_data[[1]]))
hex_colors <- as.character(excel_data[[8]])

# 加载地形数据
terrain_data <- read_excel("Data/Raw/Global_Legend.xls")

# 加载 TIF_2DMatrix.RData 文件
load("Data/Processed/TIF_2DMatrix.RData")

# 统计地形值频率
terrain_freq <- table(as.vector(mat))

# 创建数据框
df_freq <- data.frame(terrain = names(terrain_freq), frequency = as.numeric(terrain_freq))
df_freq$terrain <- as.numeric(as.character(df_freq$terrain))

# 创建颜色映射
color_mapping <- setNames(hex_colors, value)

# 添加颜色列到 df_freq 数据框
df_freq$color <- sapply(df_freq$terrain, function(x) color_mapping[as.character(x)])

# 创建地形类型映射
terrain_type_mapping <- setNames(terrain_data$CLASSNAMES, terrain_data$VALUE)

# 为数据框添加地形类型列
df_freq$terrain_type <- sapply(df_freq$terrain, function(x) terrain_type_mapping[as.character(x)])

# 绘制主要柱状图（去掉图例）
main_plot <- ggplot(df_freq, aes(x = as.factor(terrain), y = frequency, fill = as.factor(terrain))) +
  geom_col() +
  scale_fill_manual(values = df_freq$color, guide = "none") +
  labs(x = "地形值", y = "频率", title = "地形值频率柱状图") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

#####this part wrong
# 绘制仅包含图例的图
#legend_plot <- ggplot(df_freq, aes(x = 1, y = 1, fill = as.factor(terrain))) +
#  geom_point(shape = 15, size = 10) +
#  scale_fill_manual(values = df_freq$color, labels = df_freq$terrain_type, name = "地形类型") +
#  theme_void() +
#  theme(legend.position = "center")

# 保存主要图像
#ggsave("main_plot.png", main_plot, width = 10, height = 6)

# 保存图例图像
#ggsave("legend_plot.png", legend_plot, width = 6, height = 8)

