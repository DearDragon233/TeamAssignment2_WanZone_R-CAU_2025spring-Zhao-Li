# 加载R包
library(reshape2) # 用于转换为长格式
library(ggplot2)  # 伟大，无需多言（
library(dplyr)    # 用于区间分类

# 加载统计数据
load("Data/Processed/land_fraction_df.RData")

# 数据框长格式转换
land_fraction_long <- melt(land_fraction_df, id.vars = "Latitude",
                           variable.name = "Vegetation", value.name = "Fraction",na.rm = T)

# 将纬度按区间分类（以每 5° 为一个区间）
land_fraction_long <- land_fraction_long %>%
  mutate(Latitude_Group = cut(Latitude, breaks = seq(-90, 90, by = 5), include.lowest = TRUE))

# ggplot，启动！
ggplot(land_fraction_long, aes(x = Latitude_Group, y = Vegetation, fill = Fraction)) +
  geom_tile() + 
  scale_fill_viridis_c(limits = c(0, 0.75)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "植被类型-纬度分组热图",
       x = "纬度区间",
       y = "植被类型",
       fill = "植被占比")+
  coord_flip()

# 存储图像
ggsave("Plots/植被类型-纬度分组热图.png",width = 6, height = 9)
