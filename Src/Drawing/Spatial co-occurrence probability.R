# 加载R包
library(readxl)
library(reshape2)   # 用于转换为长格式
library(ggplot2)    # 伟大，无需多言（
library(gridExtra)  # 用于拼接图像

# -------------------------------
# 1. 数据准备与参数设定
# -------------------------------
# 加载数据
load("Data/Processed/TIF_2DMatrix.RData")         # 植被分布，对象为mat

# 定义植被种类总数与所需数据
n_types <- 23
nrow_mat <- nrow(mat)
ncol_mat <- ncol(mat)

# -------------------------------
# 2. 利用向量化方法构造共现矩阵（八邻域）
# -------------------------------
# 初始化共现矩阵：行为中心植被类型，列为邻域中实际出现的植被类型
co_occurrence <- matrix(0, nrow = n_types, ncol = n_types)

# 构造 8 个邻域偏移量（3x3 范围，除去中心）
offsets <- expand.grid(dr = -1:1, dc = -1:1)
offsets <- offsets[!(offsets$dr == 0 & offsets$dc == 0), ]

# 对每个偏移量一次性处理整个有效区域，提高效率
for(idx in 1:nrow(offsets)){
  dr <- offsets$dr[idx]
  dc <- offsets$dc[idx]
  
  # 确定当前偏移下的中心区域索引（确保移位后不越界）
  i_from <- max(1, 1 - dr)
  i_to   <- min(nrow_mat, nrow_mat - dr)
  j_from <- max(1, 1 - dc)
  j_to   <- min(ncol_mat, ncol_mat - dc)
  
  # 取出中心区域及对应的邻域区域
  central_block <- mat[i_from:i_to, j_from:j_to]
  neighbor_block <- mat[(i_from + dr):(i_to + dr), (j_from + dc):(j_to + dc)]
  
  # 转换为向量，统计二者配对次数
  central_vec <- as.vector(central_block)
  neighbor_vec <- as.vector(neighbor_block)
  counts <- table(factor(central_vec, levels = 1:n_types),
                  factor(neighbor_vec, levels = 1:n_types))
  
  # 累加当前偏移所贡献的计数
  co_occurrence <- co_occurrence + as.matrix(counts)
}

# -------------------------------
# 3. 计算邻域概率矩阵
# -------------------------------
# 对每行归一化，得到每个中心植被的邻域概率
prob_matrix <- matrix(0, nrow = n_types, ncol = n_types)
for(i in 1:n_types){
  total_neighbors <- sum(co_occurrence[i, ])
  if(total_neighbors > 0){
    prob_matrix[i, ] <- co_occurrence[i, ] / total_neighbors
  }
}
# 完全随机分布时，各类型概率应为 1/n_types，作为基准
expected_value <- 1 / n_types

# -------------------------------
# 4. 计算偏差矩阵（实际概率减去期望值）
# -------------------------------
deviation_matrix <- prob_matrix - expected_value

# -------------------------------
# 5. 分离成对角线与非对角线两部分，并分别归一化
# -------------------------------
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

# 对角线部分：种内共现偏差
diag_values <- diag(deviation_matrix)

# 单独归一化对角线，使得最大绝对值等于1
diag_max <- max(abs(diag_values))
norm_diag_values <- diag_values / diag_max

# 构造数据框用于柱状图
diag_df <- data.frame(Vegetation = translations,
                      Normalized_Deviation = norm_diag_values)

# 手动因子化植被名，保证作图时顺序不变
diag_df$Vegetation <- factor(diag_df$Vegetation, levels = unique(diag_df$Vegetation))

# 非对角线部分：种间共现偏差
off_diag_matrix <- deviation_matrix
diag(off_diag_matrix) <- NA  # 去掉对角线
off_diag_max <- max(abs(off_diag_matrix), na.rm = TRUE)
norm_off_diag_matrix <- off_diag_matrix / off_diag_max

# 写入翻译
colnames(norm_off_diag_matrix) <- translations
rownames(norm_off_diag_matrix) <- translations

# 转换为长格式数据，仅保留非NA（非对角线）数据
if(!require(reshape2)) install.packages("reshape2", dependencies = TRUE)
library(reshape2)
melted_off_diag <- melt(norm_off_diag_matrix, varnames = c("Center_Type", "Neighbor_Type"),
                        na.rm = TRUE)
colnames(melted_off_diag) <- c("Center_Type", "Neighbor_Type", "Normalized_Deviation")

# -------------------------------
# 6. 分别作图：先单独作图，再联合显示
# -------------------------------
# 绘制对角线部分（种内共现）的柱状图
p_diag <- ggplot(diag_df, aes(x = Vegetation, y = Normalized_Deviation, fill = Normalized_Deviation)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limits = c(-1, 1)) +
  labs(title = "标准化种内共现偏差（归一化后）",
       x = "植被类型", y = "归一化偏差") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)) +
  guides(fill=FALSE) +
  theme(axis.text.x = element_text(angle = 90)) # 旋转标签防止重叠

# 绘制非对角线部分（种间共现）的热图
p_off <- ggplot(melted_off_diag, aes(x = factor(Neighbor_Type), y = factor(Center_Type),
                                     fill = Normalized_Deviation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       limits = c(-1, 1)) +
  labs(title = "标准化种间共现偏差（归一化后）",
       x = "邻域植被类型", y = "中心植被类型",
       fill = "归一化偏差") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)) +
  theme(axis.text.x = element_text(angle = 90)) # 旋转标签防止重叠

# 拼接图像并储存为PNG
png("Plots/空间共线性分析.png", width = 1920, height = 1080,res = 170)
grid.arrange(p_diag, p_off, ncol = 2, widths = c(0.4, 0.6))
dev.off()

