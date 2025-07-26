# PCA.R - 修正版

# 1. 加载必要的R包
library(ggplot2)
library(corrplot)

# 2. 选取用于PCA的预测变量
#    在主脚本中已经清洗过的完整df数据框上进行操作
predictor_vars <- c("lon", "lat", "precip", "elev", "pop", "slope", "temp_min", "temp_max")
pca_data <- df[, predictor_vars]

# 3. 执行主成分分析 (中心化和标准化)
pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)

# 4. 准备下游逻辑回归模型需要的主成分得分
pca_scores <- as.data.frame(pca_result$x)

# 5. 输出关键结果到控制台
#    包括每个主成分的贡献率和变量载荷
print(summary(pca_result))
print(pca_result$rotation)

# 6. 可视化 - 碎石图 (Scree Plot)
pca_var <- pca_result$sdev^2
prop_var <- pca_var / sum(pca_var)
df_var <- data.frame(
  PC = factor(paste0("PC", 1:length(prop_var)), levels = paste0("PC", 1:length(prop_var))),
  Variance = prop_var
)
scree_plot <- ggplot(df_var, aes(x = PC, y = Variance)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Variance, 2)), vjust = -0.5, size = 3) +
  labs(title = "S5.2.2(a) 碎石图：各主成分方差贡献比例", 
       x = "主成分", 
       y = "方差贡献比例") +
  theme_minimal()
ggsave("Plots/PCA碎石图.png", plot = scree_plot, width = 4, height = 4)


# 7. 可视化 - 变量贡献热图 (平方余弦)
cos2_vars <- pca_result$rotation^2 / rowSums(pca_result$rotation^2)
png("Plots/PCA平方余弦热图.png", width = 500, height = 450)
corrplot(cos2_vars, 
         is.corr = FALSE, 
         addCoef.col = "black",
         tl.col = "black",
         col = colorRampPalette(c("white","steelblue"))(200),
         main = "S5.2.2(b) 变量-主成分平方余弦热图",
         mar = c(0,0,1,0))
dev.off()