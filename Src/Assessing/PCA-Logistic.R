# 1. 加载必要的R包
library(ggplot2)
library(broom)
library(brglm2)  # 加载用于偏差修正逻辑回归的包

# 2. 准备用于逻辑回归模型的数据
#    这部分与之前完全相同，合并因变量和主成分得分
#    安全检查，确保行数匹配
if (nrow(df) != nrow(pca_scores)) {
  stop("错误：主数据框 df 和 pca_scores 行数不匹配！")
}

# 选择前5个主成分进行建模
pcs_to_use <- 1:5
df_model_pca <- cbind(farm = df$farm, pca_scores[, pcs_to_use])

# 3. 构建并运行偏差修正的逻辑回归模型
#    确保因变量的参考水平是 "nonfarm"
df_model_pca$farm <- factor(df_model_pca$farm, levels = c("nonfarm", "farm"))

#    【核心修正】: 使用glm()函数，但通过method参数指定brglm2的拟合方法
#    method = "brglmFit" 会调用偏差修正的算法，处理分离问题
#    这步会消耗大量时间和内存，请耐心等待
cat("开始运行偏差修正逻辑回归模型，这可能需要很长时间...\n")
logit_brglm_model <- glm(farm ~ ., 
                         data = df_model_pca, 
                         family = binomial,
                         method = "brglmFit")
cat("模型运行完成！\n")


# 4. 输出模型摘要到控制台
#    你会发现系数估计值与标准GLM略有不同，这是偏差修正的结果
summary(logit_brglm_model)

# 5. 整理模型结果并计算优势比 (Odds Ratios)
#    【关键】: tidy()现在可以安全地计算置信区间，因为它会调用brglm2的专用方法
#    这一步现在应该不会再报错
tidy_logit_brglm <- broom::tidy(logit_brglm_model, conf.int = TRUE, exponentiate = TRUE)

# 打印整理好的结果
print(tidy_logit_brglm)

# 6. 可视化优势比
#    这部分代码与之前的完全相同，只是输入的数据框变了
#    移除截距项
tidy_logit_brglm_no_int <- tidy_logit_brglm[tidy_logit_brglm$term != "(Intercept)", ]

#    添加显著性标记
tidy_logit_brglm_no_int$signif <- cut(tidy_logit_brglm_no_int$p.value, 
                                      breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                                      labels = c("***", "**", "*", ""),
                                      right = FALSE)

#    绘制图形
coef_plot_brglm <- ggplot(tidy_logit_brglm_no_int, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(color = "darkred", size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "darkblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") + # 添加参考线
  geom_text(aes(label = signif), vjust = -0.8, size = 5, color = "black") +
  coord_flip() +
  labs(title = "偏差修正逻辑回归优势比 (基于主成分)",
       subtitle = "优势比 > 1 表示正向影响, < 1 表示负向影响",
       x = "主成分 (Principal Component)", 
       y = "优势比 (Odds Ratio)") +
  theme_minimal(base_size = 14)

#    打印图像
print(coef_plot_brglm)

#    保存图像
ggsave(filename = "Plots/逻辑斯蒂回归优势比_brglm_PCA.png", plot = coef_plot_brglm, width = 8, height = 5)