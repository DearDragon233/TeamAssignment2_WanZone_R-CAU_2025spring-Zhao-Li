# -------------------------------
# 1. 加载必要的 R 包
# -------------------------------
library(ggplot2)      # 用于数据可视化
library(reshape2)     # 用于矩阵与数据框之间的转换
library(ranger)       # 用于构建大规模随机森林
library(tidytext)
library(patchwork)    # 用于拼合图像
library(broom)        # 用于 tidy 模型结果
library(ggfortify)
library(doParallel)   # 用于并行计算
library(foreach)      # 用于并行循环

# -------------------------------
# 2. 数据准备
# -------------------------------
# 加载数据
load("Data/Processed/TIF_2DMatrix.RData")             # 植被类型信息，变量mat
veg_mat <- mat                                        # 储存为veg_mat
load("Data/Processed/precipitation_mat.RData")        # 年均降雨信息，变量mat
precip_mat <- mat                                     # 储存为precip_mat
load("Data/Processed/tmin_mat.RData")                 # 最低气温信息，变量mat
temp_min_mat <- mat                                   # 储存为temp_min_mat
load("Data/Processed/tmax_mat.RData")                 # 最高气温信息，变量mat
temp_max_mat <- mat                                   # 储存为temp_max_mat
load("Data/Processed/elevation_matrix_reduced.RData") # 海拔信息，变量mat
elev_mat <- mat                                       # 储存为elev_mat
load("Data/Processed/Pixel_area_Latitude.RData")      # 像素面积信息，变量area_vector

# 假设所有矩阵均具有相同的维度
dims <- dim(veg_mat)  # dims[1]: 行数（纬度方向）；dims[2]: 列数（经度方向）

# 将各个矩阵的空间信息转为数据框
# 构造一个数据框，每一行对应一个网格单元，包括其行、列索引和所有变量值
df <- data.frame(
  # 生成行和列的索引（注意：R 中矩阵默认按照列优先存储）
  row_index = rep(1:dims[1], times = dims[2]),
  col_index = rep(1:dims[2], each = dims[1]),
  
  # 提取各个矩阵中的数据，as.vector() 默认按照列的顺序提取
  veg       = as.vector(veg_mat),
  precip    = as.vector(precip_mat),
  temp_min  = as.vector(temp_min_mat),
  temp_max  = as.vector(temp_max_mat),
  elev      = as.vector(elev_mat),
  lon       = rep(as.numeric(colnames(veg_mat)), each = dims[1]),
  lat       = rep(as.numeric(rownames(veg_mat)), times = dims[2])
)

# 这里根据每个网格单元所属的行号添加对应的像素面积
df$area <- area_vector[df$row_index]

# 计算温度范围（因为线性，故仅用于作图，不用于分析）
df$temp_range <- df$temp_max - df$temp_min


# -------------------------------
# 3. 构造目标变量：农田分布
# -------------------------------
# 当植被类型（veg）为 16 时，认定该网格为农田，否则为非农田
# 转换为因子类型有助于随机森林进行分类建模
df$farm <- factor(ifelse(df$veg == 16, "farm", "nonfarm"))

# 查看NA存在的情况与研究对象的分布
#table(rowSums(is.na(df))>0, df$veg == 20)

# 注意到NA基本都存在于海洋处，可直接移除
df <- na.omit(df)


# -------------------------------
# 4. 绘制直方图：以农田面积作为直方图，直观查看分布情况
# -------------------------------
source("Src/Drawing/cultivated climate density map.R")


# -------------------------------
# 5. 主成分分析 (PCA)
# -------------------------------
source("Src/Assessing/PCA.R")


# -------------------------------
# 6. Logistic 回归分析各个维度对农田形成的贡献
# -------------------------------

# 如果还未设置 reference level（默认可能按字母顺序），建议将参考水平设为 nonfarm
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
  labs(title = "各维度对农田形成的贡献（Logistic 回归优势比）",
       x = "变量", 
       y = "优势比 (Odds Ratio)") +
  theme_minimal()
print(coef_plot)

ggsave(filename = "Plots/逻辑斯蒂回归优势比.png", width = 6, height = 3)


# -------------------------------
# 8. 数据集划分：训练集与测试集
# -------------------------------
# 为了评估模型性能，将数据随机划分成 70% 的训练集和 30% 的测试集
set.seed(123)  # 固定随机种子以便结果可复现
sample_index <- sample(1:nrow(df), size = 0.7 * nrow(df))
train_df <- df[sample_index, ]
test_df  <- df[-sample_index, ]

# -------------------------------
# 9. 随机森林模型构建
# -------------------------------
# 模型目的：利用环境变量预测每个网格是否适宜形成农田
# 此处使用的预测变量为：precip, temp_min, temp_max, elev, lon 与 lat
rf_model <- ranger(farm ~ precip + temp_min + temp_max + elev + lon + lat,
                   data = df,
                   num.trees = 100,
                   probability = TRUE,
                   num.threads = parallel::detectCores()) #并行优化

# 保存为.RData文件待用
save(rf_model, file = "Data/Processed/rf_model.RData")

# 查看模型基本信息
print(rf_model)

# -------------------------------
# 10. 模型评估
# -------------------------------
# 对测试集 test_df 得到预测的概率矩阵
pred_obj <- predict(rf_model, data = test_df)
# 检查预测结果结构
str(pred_obj$predictions)  # 应该为一个矩阵，每列为一个类别

# 提取“farm”这个类别的概率
# 注意：确保 test_df$farm 的因子水平与训练时一致（例："farm", "nonfarm"）
pred_farm_prob <- pred_obj$predictions[, "farm"]

# ------------------------------
# 方案1：用 ROC 曲线及 AUC 评估模型性能
# ------------------------------
source("Src/Assessing/ROC curse.R")


# ------------------------------
# 方案2：density 图（直观和细节）
# ------------------------------
source("Src/Assessing/Prediction density map.R")  


# -------------------------------
# 方案4：箱线图
# -------------------------------
# 利用 test_df 中已经包含 obs_farm 和 farm 字段，以及预测概率变量（pred_farm_prob）：
test_df$pred_farm_prob <- pred_farm_prob

predictbox <- ggplot(test_df, aes(x = farm, y = pred_farm_prob, fill = farm, colour = farm)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  stat_boxplot(geom = "errorbar", width = 0.2) +
  labs(title = element_blank(),x = "",y = "") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),) +
  coord_flip()
  
#print(predictbox)
  
  
# 利用cowplot包拼接图像
combined_plot <- ggdraw() +
  draw_plot(PredictionDensityMap2, 0, 0, 1, 1) +
  draw_plot(PredictionDensityMap1, 0, 0.1, 1, 0.5) +
  draw_plot(predictbox, 0, 0.4, 1, 0.6) +
  draw_plot(PredictionDensityAixs, 0.01, 0.01, 1, 1) +
  draw_plot(PredictionDensityAixs1, 0.21, 0.1, 1, 0.5)
  
  
  #print(combined_plot)
  
ggsave("Plots/Access_combined_plot.png", plot = combined_plot, width = 8, height = 6)


# -------------------------------
# 方案5：Loss 曲线检验：训练与测试误差随树数量变化
# -------------------------------
tree_seq <- seq(10, 150, by = 20)
train_error <- c()
test_error <- c()

for (nt in tree_seq) {
  cat(nt, "\n")
  # 在训练集上构建模型
  temp_model <- ranger(farm ~ precip + temp_min + temp_max + elev + lon + lat, 
                       data = train_df,
                       num.trees = nt,
                       probability = TRUE,
                       num.threads = parallel::detectCores())
  
  # 在训练集上预测
  train_pred <- predict(temp_model, data = train_df)$predictions
  train_pred_class <- ifelse(train_pred[, "farm"] > 0.5, "farm", "nonfarm")
  train_error <- c(train_error, mean(train_pred_class != train_df$farm))
  
  # 在测试集上预测
  test_pred <- predict(temp_model, data = test_df)$predictions
  test_pred_class <- ifelse(test_pred[, "farm"] > 0.5, "farm", "nonfarm")
  test_error <- c(test_error, mean(test_pred_class != test_df$farm))
}

# 整理数据便于画图
error_df <- data.frame(Trees = tree_seq, 
                       Train_Error = train_error, 
                       Test_Error = test_error)
error_df_melt <- melt(error_df, id.vars = "Trees", 
                      variable.name = "Dataset", 
                      value.name = "Error")

library(ggbreak)
# 使用 ggplot2 绘制误差曲线
ggplot(error_df_melt, aes(x = Trees, y = Error, color = Dataset)) +
  scale_y_break(c(0.0155, 0.051)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "Loss 曲线检验：训练集与测试集误差",
       x = "树的数量",
       y = "误差率")

# 存储图像
ggsave("Plots/Loss curve.png", width = 5, height = 5)


# -------------------------------
# 方案6：k折检验
# -------------------------------
library(ranger)
library(pROC)

set.seed(123)
k <- 5
n <- nrow(df)
folds <- sample(rep(1:k, length.out = n))

# 用以记录各折的 AUC 值
cv_auc <- data.frame(Fold = integer(), AUC = numeric())
roc_list <- list()

for (i in 1:k) {
  # 划分训练集与测试集
  train_data <- df[folds != i, ]
  test_data  <- df[folds == i, ]
  
  # 构建随机森林模型
  rf_cv <- ranger(farm ~ precip + temp_min + temp_max + elev + lon + lat, 
                  data = train_data,
                  num.trees = 100,
                  probability = TRUE,
                  num.threads = parallel::detectCores()) #并行优化
  
  # 对测试集预测
  preds_cv <- predict(rf_cv, data = test_data)$predictions
  predicted_prob <- preds_cv[, "farm"]
  
  # 真实标签转换为 0/1
  actual <- ifelse(test_data$farm == "farm", 1, 0)
  
  # 计算 ROC 曲线和 AUC 值
  roc_result <- roc(actual, predicted_prob, quiet = TRUE)
  auc_val <- auc(roc_result)
  
  # 保存当前折的 AUC
  cv_auc <- rbind(cv_auc, data.frame(Fold = i, AUC = as.numeric(auc_val)))
}

print(cv_auc)
mean_auc <- mean(cv_auc$AUC)
cat("平均AUC：", round(mean_auc, 3), "\n")


# -------------------------------
# 11. 全区域预测农田分布潜力
# -------------------------------
# 对数据框中全体网格进行预测
pred_prob <- predict(rf_model, data = df)

# 提取“farm”这个类别的概率，作为农田适宜性（潜力）的度量
df$pred_farm_prob <- pred_prob$predictions[, "farm"]

# 将预测好的农田潜力（概率）转换为矩阵形式
# 创建一个全为 NA 的空矩阵，尺寸与原始数据一致
farm_potential_matrix <- matrix(NA, nrow = dims[1], ncol = dims[2])

# 使用 cbind 生成索引矩阵，将 df 中预测值放回对应行列位置
farm_potential_matrix[cbind(df$row_index, df$col_index)] <- df$pred_farm_prob

# 添加经纬度标记
colnames(farm_potential_matrix) <- colnames(veg_mat)
rownames(farm_potential_matrix) <- rownames(veg_mat)

# 保存为.RData文件待用
save(farm_potential_matrix, file = "Data/Processed/Potential prediction.RData")
# -------------------------------
# 12. 可视化农田分布潜力地图
# -------------------------------
# 读取作图函数文件
source("Src/Drawing/world raster map.R")


### （1）第一张图：潜力预测图
# 直接作图
worldraster(farm_potential_matrix, "农田分布潜力预测图")


### （2）第二张图：除去已有农田
# 复制农田潜力矩阵并将已有农田区域设为 0
farm_potential_no_farmland <- farm_potential_matrix
farm_potential_no_farmland[veg_mat == 16] <- 0

# 作图
worldraster(farm_potential_no_farmland, "农田分布潜力预测图(去除原有农田)")


### （3）第三张图：除去已有农田和人造地形（veg_mat 中编号16和22）
# 同样复制农田潜力矩阵，并将已有农田（16）和人造地形（22）区域设为 0
farm_potential_no_farmland_manmade <- farm_potential_matrix
farm_potential_no_farmland_manmade[veg_mat == 16 | veg_mat == 22] <- 0

# 作图
worldraster(farm_potential_no_farmland_manmade, "农田分布潜力预测图(去除原有农田和人造地形)")


### （4）第四张图：插值与外推范围分析
# 计算训练数据中各变量的最小值与最大值（对训练集 train_df）
vars <- c("precip", "temp_min", "temp_max", "elev", "lon", "lat")
train_range <- data.frame(
  var = vars,
  min = sapply(vars, function(v) min(train_df[[v]], na.rm = TRUE)),
  max = sapply(vars, function(v) max(train_df[[v]], na.rm = TRUE))
)

# 定义函数，检查每个网格单元各变量是否落在训练范围内，计算外推比例
check_extrap <- function(x) {
  count <- 0
  for(v in vars) {
    if(x[[v]] < train_range$min[train_range$var == v] ||
       x[[v]] > train_range$max[train_range$var == v]) {
      count <- count + 1
    }
  }
  return(count / length(vars))  # 返回超出比例
}
df$extrapolation_ratio <- apply(df[, vars], 1, check_extrap)

table(df$extrapolation_ratio)

