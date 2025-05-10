# 加载R包
library(reshape2) # 用于转换为长格式
library(ggplot2)  # 伟大，无需多言（

# 加载数据
load("Data/Processed/land_fraction_df.RData") # 植被分布信息，变量land_fraction_df
load("Data/Processed/plant_colors.RData")     # 图例颜色信息，变量plant_colors
load("Data/Processed/New_class.RData")        # 植被分类信息，变量data

# 数据框长格式转换
land_fraction_long <- melt(land_fraction_df, id.vars = "Latitude",
                           variable.name = "Vegetation", value.name = "Fraction")

# ggplot，启动！
vtldm1 <- ggplot(land_fraction_long, aes(x = Latitude, y = Fraction, fill = Vegetation)) +
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
ggsave("Plots/植被类型占陆地比例-纬度密度图.png",plot = vtldm1, width = 3.11, height = 7)

# 将分类与矩阵匹配
categories <- data$class[-20]
unique_categories <- unique(categories)

# 删除纬度列便于数据分类
land_fraction_df <- land_fraction_df[, -23]

# 创建一个按分类汇总的矩阵
result <- sapply(unique_categories, function(cat) {
  col_indices <- which(categories == cat)
  rowSums(land_fraction_df[, col_indices, drop = F])
})

# 转置结果
rownames(result) <- rownames(land_fraction_df)

# 数据框长格式转换
land_fraction_long <- melt(result, id.vars = "latitude",
                           variable.name = "Vegetation", value.name = "Fraction")

# ggplot，启动！
vtldm2 <- ggplot(land_fraction_long, aes(x = Var1, y = Fraction, fill = Var2)) +
  geom_density(stat = "identity", position = "stack",color = NA) +  # 颜色NA为无边框，stack为堆叠图
  scale_fill_manual(values = c("#006300","#FF7600","#009595","#FF73E7","#B3B3B3","#FF0000","#FFFFFF")) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0)) +  # 移除 x 轴额外留白
  labs(title = "植被分类占陆地比例-纬度密度图",
       x = "纬度/°",
       y = "植被类型占陆地比例",
       fill = "植被类型")+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  )+                                # 隐藏y轴便于拼接
  coord_flip()                      # 旋转图像匹配地图方向

# 存储图像
ggsave("Plots/植被分类占陆地比例-纬度密度图.png",plot = vtldm2, width = 4, height = 7)
