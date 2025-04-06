# 加载R包
library(reshape2) # 用于转换为长格式
library(ggplot2)  # 伟大，无需多言（

# 加载数据
load("Data/Processed/land_fraction_df.RData") # 植被分布信息
load("Data/Processed/plant_colors.RData")     # 图例颜色信息

# 数据框长格式转换
land_fraction_long <- melt(land_fraction_df, id.vars = "Latitude",
                           variable.name = "Vegetation", value.name = "Fraction")

# ggplot，启动！
ggplot(land_fraction_long, aes(x = Latitude, y = Fraction, fill = Vegetation)) +
  geom_density(stat = "identity", position = "stack",color = NA) +  # 颜色NA为无边框，stack为堆叠图
  scale_fill_manual(values = plant_colors) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0)) +  # 移除 x 轴额外留白
  labs(title = "植被类型占陆地比例-纬度密度图",
       x = "纬度/°",
       y = "植被类型占陆地比例",
       fill = "植被类型")+
  theme(legend.position = "none")+  # 移除图例，便于拼接
  coord_flip()                      # 旋转图像匹配地图方向

# 存储图像
ggsave("Plots/植被类型占陆地比例-纬度密度图.png",width = 3.11, height = 7)
