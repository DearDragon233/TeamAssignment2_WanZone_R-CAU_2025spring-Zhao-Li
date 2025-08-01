# =================================================================
# 模型解释性分析：变量重要性(VIP)与偏依赖图(PDP) - 修正版
# =================================================================

# 1. 加载必要的R包
library(vip)
library(cowplot)
library(DALEX)
library(ggplot2)


# ----------------------------------------------------
# 分析一：变量置换重要性 (Permutation Importance)
# ----------------------------------------------------
cat("正在生成变量重要性图...\n")
vip_plot <- vip(rf_model, 
                num_features = 8, 
                bar = TRUE, 
                aesthetics = list(fill = "steelblue")) +
  labs(title = "变量置换重要性",
       subtitle = "基于模型预测性能的下降程度",
       x = "变量",
       y = "重要性") +
  theme_minimal(base_size = 14)

ggsave("Plots/Variable_Importance_Plot.png", plot = vip_plot, width = 8, height = 6)
cat("变量重要性图已保存。\n")

# ----------------------------------------------------
# 分析二：偏依赖图 (PDP) - 【核心修正】
# ----------------------------------------------------
cat("正在生成偏依赖图 (使用 DALEX 稳定版)...\n")

# 使用数据子集
set.seed(123)
pdp_sample_size <- 100000
if (nrow(df) > pdp_sample_size) {
  df_pdp_sample <- df[sample(nrow(df), size = pdp_sample_size), ]
} else {
  df_pdp_sample <- df
}

# 创建预测函数包装器 (不变)
p_fun <- function(object, newdata) {
  predict(object, data = newdata)$predictions[, "farm"]
}

# 【关键修正】: 准备用于 explainer 的数据时，严格按照模型的变量名和顺序
# 1. 获取模型训练时使用的确切自变量名
model_vars <- rf_model$independent.variable.names

# 2. 从我们的样本数据框中，只选取这些变量，并严格按照这个顺序
explainer_data <- df_pdp_sample[, model_vars]

# 3. 创建 explainer 对象，现在 data 参数使用的是我们精确准备好的 explainer_data
explainer_rf <- DALEX::explain(
  model = rf_model,
  data = explainer_data, # <-- 使用这个精确准备的数据框
  y = as.numeric(df_pdp_sample$farm == "farm"),
  predict_function = p_fun,
  label = "Random Forest"
)


# 选择要分析的变量 (不变)
features_to_plot <- c("precip", "slope", "temp_max", "pop")

# 现在 model_profile 应该能正确找到列了
# 因为 explainer_data 的列名和顺序与模型内部记录的完全一致
pdp_dalex <- model_profile(
  explainer = explainer_rf, 
  variables = features_to_plot,
  type = "partial"
)

# 后续的绘图和保存代码保持不变
pdp_plot_dalex <- plot(pdp_dalex) +
  ggtitle("主要环境变量的偏依赖图", subtitle = "使用 DALEX 稳定版计算") +
  theme_minimal(base_size = 14)

ggsave("Plots/Partial_Dependence_Plots_DALEX_stable.png", plot = pdp_plot_dalex, width = 12, height = 9)
cat("偏依赖图已保存。\n")

# 清理内存
rm(df_pdp_sample, explainer_data, explainer_rf, pdp_dalex, pdp_plot_dalex, p_fun)
gc()

cat("模型解释性分析完成。\n")