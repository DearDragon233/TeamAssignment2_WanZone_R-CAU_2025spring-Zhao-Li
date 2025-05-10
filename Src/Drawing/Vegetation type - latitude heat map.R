# 加载R包
library(reshape2) # 用于转换为长格式
library(ggplot2)  # 伟大，无需多言（
library(dplyr)    # 用于区间分类

# 加载统计数据
load("Data/Processed/land_fraction_df.RData") # 植被纬度分布信息，变量land_fraction_df

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

# 调整对应并应用翻译
colnames(land_fraction_df) <- c(translations[-20],"Latitude")

# 数据框长格式转换
land_fraction_long <- melt(land_fraction_df, id.vars = "Latitude",
                           variable.name = "Vegetation", value.name = "Fraction",na.rm = T)

# 将纬度按区间分类（以每 5° 为一个区间）
land_fraction_long <- land_fraction_long %>%
  mutate(Latitude_Group = cut(Latitude, breaks = seq(-90, 90, by = 5), include.lowest = TRUE))

# ggplot，启动！
vtlhm <- ggplot(land_fraction_long, aes(x = Latitude_Group, y = Vegetation, fill = Fraction)) +
  geom_tile() + 
  scale_fill_viridis_c(limits = c(0, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "植被类型-纬度分布热图",
       x = "纬度区间",
       y = "植被类型",
       fill = "植被占比")+
  coord_flip()

# 存储为PNG
ggsave("Plots/植被类型-纬度分布热图.png",plot = vtlhm, width = 6, height = 7)
