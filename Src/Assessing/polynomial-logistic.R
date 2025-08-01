# -------------------------------
# (1) 进行标准化
# -------------------------------
df$lon_scaled       <- scale(df$lon)
df$lat_scaled       <- scale(df$lat)
df$precip_scaled    <- scale(df$precip)
df$elev_scaled      <- scale(df$elev)
df$pop_scaled       <- scale(df$pop)
df$slope_scaled     <- scale(df$slope)
df$temp_min_scaled  <- scale(df$temp_min)
df$temp_max_scaled  <- scale(df$temp_max)


# -------------------------------
# (2) 欠采样（Down Sampling）处理
# -------------------------------
library(caret)  # 用于欠采样函数 downSample()

# 选取标准化后的预测变量和目标变量
data_for_sampling <- df[, c("lon_scaled", "lat_scaled", "precip_scaled", 
                            "elev_scaled", "pop_scaled","slope_scaled", 
                            "temp_min_scaled", "temp_max_scaled", "farm")]

# 使用 downSample() 进行欠采样，x 为自变量，y 为因变量
df_balanced <- downSample(x = data_for_sampling[, -ncol(data_for_sampling)], 
                          y = data_for_sampling$farm)

# downSample 返回的因变量列名默认为 "Class"，这里改回 "farm"
names(df_balanced)[names(df_balanced) == "Class"] <- "farm"


# -------------------------------
# (4) Logistic 多项式回归（正则化 - 使用 glmnet，LASSO 惩罚）
# -------------------------------
library(glmnet)

# 构建设计矩阵，注意 model.matrix 会自动生成截距列，现将其去掉
x <- model.matrix(farm ~ lon_scaled + I(lon_scaled^2) +
                    lat_scaled + I(lat_scaled^2) +
                    precip_scaled + I(precip_scaled^2) +
                    elev_scaled + I(elev_scaled^2) +
                    pop_scaled + I(pop_scaled^2) +
                    slope_scaled + I(slope_scaled^2) +
                    temp_min_scaled + I(temp_min_scaled^2) +
                    temp_max_scaled + I(temp_max_scaled^2),
                  data = df_balanced)[,-1]

# 构建二值响应变量：farm 为 "farm" 则记为 1，否则记为 0
y <- ifelse(df_balanced$farm == "farm", 1, 0)

# 设置随机种子确保结果可重现
set.seed(123)

# 利用交叉验证构建惩罚性逻辑回归模型（LASSO：alpha=1）
cv_model <- cv.glmnet(x, y, family = "binomial", alpha = 1)
best_lambda <- cv_model$lambda.min
cat("最佳 lambda:", best_lambda, "\n")

# 根据最佳 lambda 拟合最终正则化模型
reg_model <- glmnet(x, y, family = "binomial", alpha = 1, lambda = best_lambda)

# 查看正则化模型的系数
coef_reg <- coef(reg_model)
coef_reg_df <- data.frame(term = rownames(coef_reg), estimate = as.numeric(coef_reg))
coef_reg_df$odds_ratio <- exp(coef_reg_df$estimate)
print(coef_reg_df)

# -------------------------------
# (5) 可视化正则化模型结果（优势比）
# -------------------------------
# 去除截距项，仅展示各预测变量的系数
coef_reg_plot <- coef_reg_df[coef_reg_df$term != "(Intercept)", ]
coef_reg_plot <- coef_reg_plot[order(coef_reg_plot$estimate), ]
# 仅保留 term 列中以 "I" 开头的行
coef_reg_plot <- coef_reg_df[grepl("^I", coef_reg_df$term), ]

reg_plot <- ggplot(coef_reg_plot, aes(x = reorder(term, estimate), y = odds_ratio)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
  annotate("text", x = 1, y = max(coef_reg_plot$odds_ratio) * 1.05, 
           label = "x = 1", color = "red", size = 5, vjust = 0) +
  coord_flip() +
  labs(
    title = "S5.2.4 正则化 Logistic 多项式回归优势比",
    x = "变量", 
    y = "优势比 (Odds Ratio)"
  ) +
  theme_minimal()

ggsave(filename = "Plots/逻辑斯蒂多项式回归优势比.png", plot = reg_plot, width = 6, height = 3)

