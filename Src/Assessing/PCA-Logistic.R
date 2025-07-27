# 1. 加载必要的R包
library(ggplot2)
library(broom)
library(brglm2)

# 2. 准备用于逻辑回归模型的数据
#    安全检查，确保行数匹配
if (nrow(df) != nrow(pca_scores)) {
  stop("错误：主数据框 df 和 pca_scores 行数不匹配！")
}

#    选择前5个主成分进行建模
pcs_to_use <- 1:5
df_model_pca <- cbind(farm = df$farm, pca_scores[, pcs_to_use])

# 3. 构建并运行偏差修正的逻辑回归模型
df_model_pca$farm <- factor(df_model_pca$farm, levels = c("nonfarm", "farm"))
logit_brglm_model <- glm(farm ~ ., 
                         data = df_model_pca, 
                         family = binomial,
                         method = "brglmFit")

# 4. 输出模型摘要
summary(logit_brglm_model)

# 5. 整理模型结果并计算优势比
tidy_logit_brglm <- broom::tidy(logit_brglm_model, conf.int = TRUE, exponentiate = TRUE)

# 6. 可视化优势比
#    移除截距项
tidy_logit_brglm_no_int <- tidy_logit_brglm[tidy_logit_brglm$term != "(Intercept)", ]

#    【核心修正】: 设置因子的顺序，以控制ggplot的绘图顺序
#    由于coord_flip()会反转顺序，所以我们这里设置的level是反向的
tidy_logit_brglm_no_int$term <- factor(tidy_logit_brglm_no_int$term, 
                                       levels = paste0("PC", 5:1))

#    添加显著性标记
tidy_logit_brglm_no_int$signif <- cut(tidy_logit_brglm_no_int$p.value, 
                                      breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                                      labels = c("***", "**", "*", ""),
                                      right = FALSE)

#    绘制图形
#    【核心修正】: aes()中的x轴直接使用term，不再用reorder()
coef_plot_brglm <- ggplot(tidy_logit_brglm_no_int, aes(x = term, y = estimate)) +
  geom_point(color = "darkred", size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "darkblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  geom_text(aes(label = signif), vjust = -0.8, size = 5, color = "black") +
  coord_flip() +
  labs(title = "偏差修正逻辑回归优势比 (基于主成分)",
       subtitle = "优势比 > 1 表示正向影响, < 1 表示负向影响",
       x = "主成分 (Principal Component)", 
       y = "优势比 (Odds Ratio)") +
  theme_minimal(base_size = 14)

#    保存图像
ggsave(filename = "Plots/PCA-逻辑斯蒂回归优势比.png", plot = coef_plot_brglm, width = 8, height = 5)