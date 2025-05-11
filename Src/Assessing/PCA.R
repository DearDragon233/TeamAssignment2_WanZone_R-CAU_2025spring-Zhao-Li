# 加载R包
library(reshape2)
library(ggplot2)
library(gridGraphics)
library(grid)         # 图像类型转换

# 选取可能影响农田形成的六个维度数据
df_farm <- subset(df, farm == "farm")
pca_vars <- df_farm[, c("lon", "lat", "precip", "elev", "temp_min", "temp_max")]

# 执行主成分分析
# center = TRUE 对各变量进行中心化，scale. = TRUE 则对其进行标准化（因为各变量量纲不同）
pca_result <- prcomp(pca_vars, center = TRUE, scale. = TRUE)

# 输出主成分分析结果摘要，观察各主成分解释的方差比例
print(summary(pca_result))

# 查看变量载荷矩阵
# pca_result$rotation 中的每一列表示对应主成分，每一行为原始变量的载荷
print(pca_result$rotation)

# 计算各主成分的方差（即特征值）及其比例
pca_var <- pca_result$sdev^2          # 各主成分的方差（特征值）
prop_var <- pca_var / sum(pca_var)      # 各主成分的方差比例

# 构造数据框
df_var <- data.frame(
  PC = factor(paste0("PC", 1:length(prop_var)), levels = paste0("PC", 1:length(prop_var))),
  Variance = prop_var,
  Cumulative = cumsum(prop_var)
)

# 使用 ggplot2 绘制碎石图
scree_plot <- ggplot(df_var, aes(x = PC, y = Variance)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Variance, 2)), vjust = -0.5, size = 3) +
  labs(title = "S5.2.2(a) 碎石图：各主成分方差贡献比例", 
       x = "主成分", 
       y = "方差贡献比例") +
  theme_minimal()

# 存为PNG
ggsave("Plots/PCA碎石图.png", plot = scree_plot, width = 4, height = 4)

# 绘制 PCA biplot
pca_biplot <- autoplot(pca_result, data = df_farm, 
                       loadings = TRUE, 
                       loadings.label = TRUE,  
                       colour = 'steelblue',
                       size = 0.1,
                       alpha = 0.2) +
  labs(title = "S5.2.2(c) PCA 双变量图") +
  theme_minimal()

#print(pca_biplot)

# 存为PNG
ggsave("Plots/PCA双变量图.png", plot = pca_biplot, width = 6, height = 5)

# 先对载荷取平方
rotation2 <- pca_result$rotation^2

# 对于每个变量（行），归一化为其所有主成分贡献的比例：
cos2_vars <- rotation2 / rowSums(rotation2)

# 利用 corrplot 绘制平方余弦热图
library(corrplot)

png("Plots/PCA平方余弦热图.png", width = 500, height = 450)

corrplot(cos2_vars, 
         is.corr = FALSE, 
         addCoef.col = "black",# 在图中显示具体数值，黑色字体
         tl.col = "black",     # 向量标签的颜色
         col = colorRampPalette(c("white","steelblue"))(200),
         main = "S5.2.2(b) 变量-主成分平方余弦热图",
         mar = c(0,0,1,0))     # 调整图形边距，以便显示标题

dev.off()

