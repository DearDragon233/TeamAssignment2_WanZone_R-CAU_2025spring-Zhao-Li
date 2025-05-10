library(ggplot2)
library(readxl)
library(cowplot)

# 获取RGB值
excel_data <- read_excel('Data/Resource/色彩.xlsx')

# 写入植被类型的对应简化翻译，用于作图
translations <- c(
  "阔叶常绿",
  "阔叶落叶，封闭",
  "阔叶落叶，开放",
  "针叶常绿",
  "针叶落叶",
  "混合叶",
  "定期淹没，淡水",
  "定期淹没，咸水",
  "未知或其它",
  "烧毁",
  "常绿",
  "落叶",
  "草本",
  "稀疏灌木或草本",
  "定期淹没灌木/草本",
  "耕种和管理区域",
  "农田：树木或其它",
  "农田：灌木或草本",
  "裸地",
  "水域",
  "雪和冰",
  "人造及相关",
  "无数据"
)

# 用列索引提取列
value <- as.numeric(as.character(excel_data[[1]]))
hex_colors <- as.character(excel_data[[8]])

# 加载文件
terrain_data <- read_excel("Data/Raw/Global_Legend.xls")
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
df_freq$terrain_type <- sapply(df_freq$terrain, function(x) terrain_type_mapping[as.character(x)])

# 加入翻译
df_freq$terrain <- translations
df_freq$terrain <- factor(df_freq$terrain, levels = unique(df_freq$terrain))

# 绘制主要柱状图
tf <- ggplot(df_freq, aes(x = as.factor(terrain), y = frequency, fill = as.factor(terrain))) +
  geom_col() +
  scale_fill_manual(values = df_freq$color, guide = "none") +
  labs(x = "地形值", y = "频率", title = "地形值频率柱状图") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# 导出为PNG
ggsave("Plots/地形值频率柱状图.png",plot = tf, width = 10, height = 6)
