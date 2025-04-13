# 加载R包
library(ggplot2)  # 伟大，无需多言（
library(dplyr)    # 数据处理

# 加载数据
load("Data/Processed/lat_area.RData")     # 植被纬度面积信息，变量 lat_area
load("Data/Processed/New_class.RData")      # 植被分类信息，变量 data
load("Data/Processed/plant_colors.RData")   # 图例颜色信息，变量 plant_colors

# 写入植被类型的对应简化翻译，用于作图
translations <- c(
  "阔叶常绿",
  "阔叶落叶，封闭",
  "阔叶落叶，开放",
  "针叶常绿",
  "针叶落叶",
  "混合叶",
  "定期淹没，淡水\n",
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
  "无数据\n\n"
)

# 选取需要的数据内容
lat_area <- lat_area[, c(1:19, 21:23)]
data <- data$class[-20]
plant_colors <- plant_colors[-20]
translations <- translations[-20]

# 计算总面积
gross_area <- colSums(lat_area)

# 合并为数据框，注意这里 variable 为列名，category 为分类信息
df_in <- data.frame(variable = names(gross_area),
                    area = gross_area,
                    category = data,
                    stringsAsFactors = FALSE)

# 构建内外层数据，设定因子按照数据第一次出现排序
df <- df_in %>%
  mutate(
    category = factor(category, levels = unique(category)),
    variable = factor(variable, levels = unique(variable))
  )

# 翻译替换，便于作图
levels(df$variable) <- translations
names(plant_colors) <- translations  #同时修改颜色映射变量，保证仍能对应

# 内圈：按分类汇总，计算累计面积用于确定标签位置
category_data <- df %>%
  group_by(category) %>%
  summarise(area = sum(area), .groups = "drop") %>%
  mutate(
    pct = area / sum(area) * 100,
    midpoint = sum(area) - cumsum(area) + area / 2
  )

# 外圈：保留原数据，计算每个变量在外圈的累计面积及比例
df <- df %>%
  mutate(
    category_area = ave(area, category, FUN = sum),
    pct = area / category_area * 100
  )

# 外圈：计算的标签位置
total_area <- sum(df$area)
df <- df %>%
  arrange(variable) %>%  # 保证顺序与因子水平一致
  mutate(
    outer_cumsum = cumsum(area),
    outer_midpoint = sum(area) - outer_cumsum + area / 2,
    raw_angle = 360 * outer_midpoint / total_area - 90, # 如果角度小于 -90 度，则旋转180度，使文字正向
    hjust = ifelse(raw_angle > 90, 1, 0),
    angle = ifelse(raw_angle > 90, 180- raw_angle, -raw_angle)
  )


# 创建双层饼图
ggplot() +
  # 内圈饼图（分类）
  geom_bar(data = category_data, 
           aes(x = 0, y = area, fill = category), 
           stat = "identity", width = 1) +
  # 内圈标签，在饼图内
  geom_text(data = category_data, 
            aes(x = c(0.05,0.05,0.05,0.05,0.05,0.15,0.3), y = midpoint, label = category), 
            size = 4, color = "white") +
  # 外圈饼图（原数据）
  geom_bar(data = df, 
           aes(x = 1, y = area, fill = variable), 
           stat = "identity", width = 0.6) +
  # 外圈标签，在圈外侧
  geom_text(data = df,
            aes(x = 1.35, y = outer_midpoint, 
                label = variable, angle = angle, hjust = hjust),
            size = 3) +
  # 转换极坐标为饼图
  coord_polar(theta = "y") +
  # 保证颜色按照设定映射
  scale_fill_manual(
    breaks = c(levels(df$category), levels(df$variable)),
    values = c("Forest" = "#006300",
               "Shrub" = "#FF7600",
               "Grass" = "#009595",
               "Cultivated" = "#FF73E7",
               "Noplant" = "#B3B3B3",
               "Artificial" = "#FF0000",
               "Nodata" = "#FFFFFF",
               plant_colors)
  ) +
  # 隐藏图例并修改主题
  labs(title = "植被与分类面积占比图")+
  theme_void() +
  theme(legend.position = "none") +
  labs(fill = "Legend") +
  coord_polar(theta = "y", clip = "off")  # 防止标题位置裁剪

# 储存为PNG
ggsave("Plots/植被与分类面积占比图.png", plot = last_plot(), width = 6, height = 5)
