library(ggplot2)
library(reshape2)

# 加载数据
load("Data/Processed/TIF_2DMatrix.RData")

# 森林类别值（1-10）
forest_values <- 1:10

# 对应RGB颜色（按照你提供的值）
forest_colors <- c(
  "1" = rgb(0, 0.39, 0),                # Tree Cover, broadleaved, evergreen
  "2" = rgb(0, 0.584313725, 0),         # Tree Cover, broadleaved, deciduous, closed
  "3" = rgb(0.682352941, 0.996078431, 0.384313725),  # Tree Cover, broadleaved, deciduous, open
  "4" = rgb(0.541176471, 0.266666667, 0.070588235),  # Tree Cover, needle-leaved, evergreen
  "5" = rgb(0.8, 0.494117647, 0.37254902),  # Tree Cover, needle-leaved, deciduous
  "6" = rgb(0.545098039, 0.741176471, 0),  # Tree Cover, mixed leaf type
  "7" = rgb(0.466666667, 0.584313725, 0.996078431),  # Tree Cover, regularly flooded, fresh water
  "8" = rgb(0, 0.274509804, 0.780392157),  # Tree Cover, regularly flooded, saline water
  "9" = rgb(0, 0.898039216, 0),          # Mosaic: Tree Cover / Other natural vegetation
  "10" = rgb(0, 0, 0)                   # Tree Cover, burnt
)

# 设置采样间隔，增大采样间隔来降低数据量
sampling_interval <- 50  # 增大采样间隔，减少样本数量

# 采样矩阵
mat_sampled <- mat[seq(1, nrow(mat), by = sampling_interval),
                   seq(1, ncol(mat), by = sampling_interval)]

# 转换为布尔矩阵（只保留森林类型值的位置）
is_forest <- mat_sampled %in% forest_values

# 获取仅包含森林类型的行列索引
forest_indices <- which(is_forest, arr.ind = TRUE)

# 检查 forest_indices 是否为空或没有有效行
if (length(forest_indices) == 0) {
  stop("没有找到任何森林区域，请检查数据是否正确")
}

# 提取对应的森林值（在源数据中对应点的值）
forest_values_at_indices <- mat_sampled[forest_indices]

# 提取纬度和经度的索引
latitudes <- as.numeric(rownames(mat_sampled))[forest_indices[, 1]]
longitudes <- as.numeric(colnames(mat_sampled))[forest_indices[, 2]]

# 创建数据框以便绘制
melted_mat <- data.frame(
  latitude = latitudes,
  longitude = longitudes,
  forest_value = forest_values_at_indices
)

# 将森林值映射为颜色
melted_mat$color <- forest_colors[as.character(melted_mat$forest_value)]

# 创建图例标签数据框
legend_data <- data.frame(
  forest_value = factor(forest_values, levels = 1:10),
  forest_name = c(
    "T,b,e",#Tree Cover, broadleaved, evergreen
    "T,b,d,c",#Tree Cover, broadleaved, deciduous, closed
    "T,b,d,o",#Tree Cover, broadleaved, deciduous, open
    "T,n,e",#Tree Cover, needle-leaved, evergreen
    "T,n,d",#Tree Cover, needle-leaved, deciduous
    "T,m",#Tree Cover, mixed leaf type
    "T,r,f",#Tree Cover, regularly flooded, fresh water
    "T,r,s",#Tree Cover, regularly flooded, saline water
    "M",#Mosaic: Tree Cover / Other natural vegetation
    "burnt"#Tree Cover, burnt
  ),
  color = forest_colors[as.character(1:10)]
)

# 绘图
ggplot(melted_mat, aes(x = longitude, y = latitude, fill = factor(forest_value))) +
  geom_raster() +
  scale_fill_manual(
    values = forest_colors,
    name = "森林类型",
    breaks = legend_data$forest_value,
    labels = legend_data$forest_name
  ) +
  labs(x = "经度", y = "纬度", title = "世界森林分布图（低分辨率）") +
  coord_fixed(ratio = 1.3) +
  theme_minimal() +
  theme(legend.position = "bottom")  # 调整图例位置

