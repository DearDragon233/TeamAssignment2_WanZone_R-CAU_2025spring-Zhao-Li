# 加载R包
library(pROC)         # 用于ROC及AUC计算

# 计算 ROC 曲线
# 因为 ROC 分析中需要真实二分类情况，假设"farm"为正类，"nonfarm"为负类，
# 确保 test_df$farm 的因子水平顺序为：c("nonfarm", "farm")
roc_obj <- roc(response = test_df$farm, 
               predictor = pred_farm_prob,
               levels = c("nonfarm", "farm"),  # 指定负类与正类
               direction = "<")

# 打印 AUC 值
auc_value <- auc(roc_obj)
cat("AUC 值 =", auc_value, "\n")

# 利用 ggroc() 绘制 ROC 曲线，并添加斜对角参考线与图形美化
ROCcurse <- ggroc(roc_obj, colour = "blue", size = 1.2) +
  ggtitle(sprintf("ROC 曲线 (AUC = %.3f)", auc_value)) +
  geom_abline(intercept = 1, slope = 1,color = "gray") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_minimal() +
  # 设置固定比例，使得 x 和 y 轴单位相同，形成正方形的框
  coord_fixed() +
  # 添加正方形的边框，并同时删除 panel 外的坐标线及网格线
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1))

print(ROCcurse)
