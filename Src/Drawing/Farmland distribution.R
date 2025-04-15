# 加载R包
library(reshape2) # 用于转换为长格式
library(ggplot2)  # 伟大，无需多言（

# 加载数据
load("Data/Processed/land_fraction_df.RData") # 植被分布信息，变量land_fraction_df
load("Data/Processed/lat_area.RData")         # 植被面积信息，变量lat_area

row_sums <- lat_area[,16] + lat_area[,17] + lat_area[,18]

row_sums_fra <- land_fraction_df[,16] + land_fraction_df[,17] + land_fraction_df[,18]

# 将统计结果转换为数据框，同时保存行名和对应的计数
df_row <- data.frame(
  rowname = as.numeric(names(row_sums)),
  count = row_sums
)

# 加载 ggplot2，并绘制柱状图
library(ggplot2)
ggplot(df_row, aes(x = rowname, y = count)) +
  geom_area(stat = "identity", fill = "#FF73E7") +
  geom_smooth()+
  labs(title = "各纬度农田面积图(m²)",
       x = "纬度",
       y = "农田总面积") +
  scale_x_continuous(limits = c(-56.008928, 89.991071)) + # 设置 x 轴范围
  theme_minimal() +
  coord_flip()                      # 旋转图像匹配地图方向

# 存储图像
ggsave("Plots/农田面积-纬度图.png",width = 3, height = 6)


# 将统计结果转换为数据框，同时保存行名和对应的计数
df_row_fra <- data.frame(
  rowname = as.numeric(names(row_sums)),
  count = row_sums_fra
)

# 加载 ggplot2，并绘制柱状图
library(ggplot2)
ggplot(df_row_fra, aes(x = rowname, y = count)) +
  geom_area(stat = "identity", fill = "#FF73E7") +
  geom_smooth()+
  labs(title = "农田占陆地比例-纬度图",
       x = "纬度",
       y = "农田占陆地比例") +
  scale_y_continuous(limits = c(0, 0.65)) +                # 设置 y 轴范围
  scale_x_continuous(limits = c(-56.008928, 89.991071)) + # 设置 x 轴范围
  theme_minimal() +
  coord_flip()                      # 旋转图像匹配地图方向

# 存储图像
ggsave("Plots/农田占陆地比例-纬度图.png",width = 3, height = 6)
