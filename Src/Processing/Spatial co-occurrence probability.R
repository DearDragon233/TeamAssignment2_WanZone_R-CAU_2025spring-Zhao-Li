# 加载R包
library(readxl)
library(reshape2) # 用于转换为长格式
library(ggplot2)  # 伟大，无需多言（

# 加载数据
load("Data/Processed/TIF_2DMatrix.RData")         # 植被分布，对象为mat

n_types <- 23  # 植被类型数

# 初始化一个 23x23 的共现矩阵
prob_matrix <- matrix(0, nrow = 23, ncol = 23)

# 定义 8 个邻域偏移量（不包含中心像元）
offsets <- expand.grid(dx = -1:1, dy = -1:1)
offsets <- offsets[!(offsets$dx == 0 & offsets$dy == 0), ]

# 对于每一种邻域偏移，统计中心像元与邻居的类型配对
for(k in 1:nrow(offsets)){
  dx <- offsets$dx[k]
  dy <- offsets$dy[k]
  
  # 为了保证索引有效，需要确定两个矩阵切片的起止位置
  i_from <- max(1, 1 - dx)
  i_to   <- min(nrow(mat), nrow(mat) - dx)
  j_from <- max(1, 1 - dy)
  j_to   <- min(ncol(mat), ncol(mat) - dy)
  
  # 中心像元和其对应邻居
  central <- mat[i_from:i_to, j_from:j_to]
  neighbor <- mat[(i_from + dx):(i_to + dx), (j_from + dy):(j_to + dy)]
  
  # 将矩阵转为向量进行配对统计
  central_vec <- as.vector(central)
  neighbor_vec <- as.vector(neighbor)
  
  # 累加每一对出现的次数
  for(idx in seq_along(central_vec)){
    type_center <- central_vec[idx]
    type_neighbor <- neighbor_vec[idx]
    prob_matrix[type_center, type_neighbor] <- prob_matrix[type_center, type_neighbor] + 1
  }
}

# 计算每个植被类型的邻域概率分布
# 即对每一行归一化，得到当中心为某类型时，邻居为某类型的概率
prob_matrix <- t(apply(prob_matrix, 1, function(x) {
  if(sum(x) > 0) x / sum(x) else rep(0, n_types)
}))

# 查看概率矩阵
print(prob_matrix)

# 若未安装以下包，请首先安装：
# install.packages(c("ggplot2", "reshape2"))

library(ggplot2)
library(reshape2)

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

colnames(prob_matrix) <- translations
rownames(prob_matrix) <- translations

# 自身共现：对角线数据
self_matrix <- diag(prob_matrix)   # 一个向量，各植被自身出现的频次
df_self <- data.frame(Vegetation = factor(1:n_types),
                      Frequency = self_matrix)

# 非对角线共现：将对角线设为 NA
other_matrix <- prob_matrix
diag(other_matrix) <- NA
# 转换为长格式用于绘制热图
if(!require(reshape2)) install.packages("reshape2")
library(reshape2)
melted_other <- melt(other_matrix, na.rm = TRUE)
colnames(melted_other) <- c("Center_Type", "Neighbor_Type", "Frequency")

## 绘图（使用 ggplot2）
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# 图1：显示各植被自身共现（对角线），用柱状图
p1 <- ggplot(df_self, aes(x = Vegetation, y = Frequency)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "各植被自身共现（对角线）",
       x = "植被类型", y = "共现频次") +
  theme_minimal()

# 图2：显示不同植被间的共现（非对角线），用热图
p2 <- ggplot(melted_other, aes(x = factor(Neighbor_Type), y = factor(Center_Type), fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue", na.value = "grey90") +
  labs(title = "不同植被间共现（非对角线）",
       x = "邻域植被类型", y = "中心植被类型") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90))

# 打印两幅图
print(p1)
print(p2)
