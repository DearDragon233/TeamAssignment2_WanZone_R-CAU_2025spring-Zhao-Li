#加载R包
library(reshape2)
library(terra)     # 用于处理栅格数据
library(raster)
library(ggplot2)


# 读取 .tif 
r <- rast("Data/Raw/glc2000_v1_1.tif")

# 写入植被类型的对应简化翻译，用于简化图例
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

# 加载数据
load("Data/Processed/plant_colors.RData") #植被颜色数据，变量plant_colors
names(plant_colors) <- translations

# 2. 降低分辨率
# 这里我们采用 aggregate 函数，每 10×10 个像素聚合为1个像素
# 使用众数作为代表值，计算每个块中出现频率最高的类别
# 注意：raster包中默认已经包含 modal 函数
tif_img_lowres <- aggregate(r, fact = 10, fun = "modal")

# 3. 将 SpatRaster 转换为数据框（保留 x, y 坐标）
df <- as.data.frame(tif_img_lowres, xy = TRUE)
colnames(df) <- c("x", "y", "veg")  # veg 列存储植被类型

# 添加翻译，美化做图
df$veg <- factor(df$veg, levels = 1:23, labels = translations)

# 5. 使用ggplot2绘图
ggplot(df, aes(x = x, y = y, fill = factor(veg))) +
  geom_raster() +  # geom_raster绘制规则网格图形
  scale_fill_manual(values = plant_colors, name = "植被类型") +
  coord_equal() +  # 保持x、y坐标比例一致
  theme_minimal()  +
  scale_x_continuous(expand = c(0, 0)) +  # 移除 x 轴额外留白
  scale_y_continuous(expand = c(0, 0)) +  # 移除 y 轴额外留白
  labs(x = "经度/°", y = "纬度/°", title = "植被分布图") +
  theme(legend.position="none")

# 存储为PNG
ggsave("Plots/原始图像.png",width = 12, height = 6)
