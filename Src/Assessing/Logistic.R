# 议将参考水平设为 nonfarm
df$farm <- factor(df$farm, levels = c("nonfarm", "farm"))

# 构建 logistic 回归模型（使用 binomial 家族）
logit_model <- glm(farm ~ lon + lat + precip + elev + temp_min + temp_max, 
                   data = df, 
                   family = binomial)

# 输出模型摘要，观察各系数的估计、标准误、z值及 p-value
summary(logit_model)

# 计算优势比（Odds Ratio）及置信区间
# tidy() 函数返回含有估计值、标准误、置信区间、p-value 等信息，
# 设置 exponentiate = TRUE 使得估计值以优势比（Odds Ratio）的形式展示
tidy_logit <- tidy(logit_model, conf.int = TRUE, exponentiate = TRUE)
print(tidy_logit)

# 使用 ggplot2 可视化各变量优势比及其 95% 置信区间
# 去除截距项，仅对各自变量进行展示
tidy_logit_no_int <- tidy_logit[tidy_logit$term != "(Intercept)", ]

# 根据 p-value 添加显著性标记：P<0.001：***, P<0.01：**, P<0.05：*, 否则为空
tidy_logit_no_int$signif <- cut(tidy_logit_no_int$p.value, 
                                breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
                                labels = c("***", "**", "*", ""),
                                right = FALSE)

# 绘制优势比条形图
coef_plot <- ggplot(tidy_logit_no_int, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point(color = "darkred", size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "darkblue") +
  geom_text(aes(label = signif), vjust = 0, size = 5, color = "black") +
  coord_flip() +  # 交换 x 与 y 轴，使变量名称更易阅读
  labs(title = "S5.2.3 Logistic 回归优势比",
       x = "变量", 
       y = "优势比 (Odds Ratio)") +
  theme_minimal()

ggsave(filename = "Plots/逻辑斯蒂回归优势比.png", width = 6, height = 3)
