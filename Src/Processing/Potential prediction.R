# -------------------------------
# 1. 加载必要的 R 包
# -------------------------------
library(ggplot2)      # 用于数据可视化
library(reshape2)     # 用于矩阵与数据框之间的转换
library(ranger)       # 用于构建大规模随机森林
library(tidytext)
library(cowplot)      # 用于拼合图像
library(broom)        # 用于 tidy 模型结果
library(ggfortify)
library(doParallel)   # 用于并行计算
library(foreach)      # 用于并行循环
library(terra)
library(pROC)
library(dplyr)

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
load("Data/Processed/elevation_mat.RData")            # 海拔信息，变量mat
elev_mat <- mat                                       # 储存为elev_mat
load("Data/Processed/population_mat.RData")           # 人口密度信息，变量mat
pop_mat <- mat                                        # 储存为pop_mat
load("Data/Processed/Pixel_area_Latitude.RData")      # 像素面积信息，变量area_vector

# 所有矩阵均具有相同的维度
dims <- dim(veg_mat)  # dims[1]: 行数（纬度方向）；dims[2]: 列数（经度方向）

# 将各个矩阵的空间信息转为数据框
# 构造数据框，每一行对应一个网格单元，包括其行、列索引和所有变量值
df <- data.frame(
  # 生成行和列的索引（注意 R 中矩阵默认按照列优先存储）
  row_index = rep(1:dims[1], times = dims[2]),
  col_index = rep(1:dims[2], each = dims[1]),
  
  # 提取各个矩阵中的数据，as.vector() 默认按照列的顺序提取
  veg       = as.vector(veg_mat),
  precip    = as.vector(precip_mat),
  temp_min  = as.vector(temp_min_mat),
  temp_max  = as.vector(temp_max_mat),
  elev      = as.vector(elev_mat),
  pop       = as.vector(pop_mat),
  lon       = rep(as.numeric(colnames(veg_mat)), each = dims[1]),
  lat       = rep(as.numeric(rownames(veg_mat)), times = dims[2])
)

# 每个网格单元所属的行号添加对应的像素面积
df$area <- area_vector[df$row_index]

# 计算温度范围（因为线性，故仅用于作图，不用于分析）
df$temp_range <- df$temp_max - df$temp_min

# 计算并添加坡度（Slope）变量
# 获取经纬度信息
lon_coords <- as.numeric(colnames(elev_mat))
lat_coords <- as.numeric(rownames(elev_mat))

# 创建栅格对象
elev_rast <- rast(t(elev_mat), crs = "EPSG:4326") # 使用 t() 转置矩阵

# 设置栅格的地理范围 (extent)
ext(elev_rast) <- c(min(lon_coords), max(lon_coords), min(lat_coords), max(lat_coords))

# 3. 使用 terrain() 函数计算坡度
# unit="degrees" 表示坡度单位为度
# neighbors=8 表示使用8个邻近像元进行计算，更精确
slope_rast <- terra::terrain(elev_rast, v = "slope", unit = "degrees", neighbors = 8)

# 4. 将计算出的坡度栅格转换回矩阵，然后添加到主数据框 df 中
slope_mat <- t(as.matrix(slope_rast, wide = TRUE)) # 使用 t() 再次转置回来

# 检查维度是否与原始矩阵一致 (可选，但推荐)
# dim(slope_mat)
# dim(elev_mat)

# 将坡度矩阵向量化，并添加到 df 数据框中
# 确保使用与创建 df 时相同的向量化顺序（按列）
df$slope <- as.vector(slope_mat)

# 5. 清理和检查
# 计算坡度时，栅格的边缘像元可能会产生 NA 值，因为它们没有足够的邻居
# 我们需要检查并处理这些新产生的 NA
cat("添加坡度变量前的数据行数:", nrow(df), "\n")
df <- na.omit(df) # 移除包含任何 NA 的行（这也会处理坡度计算产生的 NA）
cat("移除坡度计算中产生的 NA 后，剩余数据行数:", nrow(df), "\n")

# 查看新变量的摘要信息
summary(df$slope)

# (可选) 绘制坡度地图，直观感受一下结果
# slope_map_plot <- ggplot(df, aes(x = lon, y = lat, fill = slope)) +
#   geom_raster() +
#   scale_fill_viridis_c(name = "Slope (Degrees)", direction = -1) +
#   coord_quickmap() +
#   theme_minimal() +
#   ggtitle("Global Slope Distribution")
# print(slope_map_plot)

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
# 6. Logistic 回归分析各个主成分对农田形成的贡献
# -------------------------------
source("Src/Assessing/PCA-Logistic.R")


# -------------------------------
# 7. Logistic 回归分析各个维度对农田形成的贡献（多项式回归 - 引入二次项）
# -------------------------------
source("Src/Assessing/polynomial-logistic.R")


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
rf_model <- ranger(farm ~ precip + temp_min + temp_max + elev + lon + lat + pop + slope,
                   data = df,
                   num.trees = 100,
                   probability = TRUE,
                   importance = 'permutation',
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
#str(pred_obj$predictions)  # 应该为一个矩阵，每列为一个类别

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


# ------------------------------
# 方案3：箱线图
# ------------------------------
source("Src/Assessing/boxplot.R")

# 利用cowplot包拼接图像(density 图和箱线图)
combined_plot <- ggdraw() +
  draw_plot(PredictionDensityMap2, 0, 0, 1, 1) +
  draw_plot(PredictionDensityMap1, 0, 0.1, 1, 0.5) +
  draw_plot(predictbox, 0, 0.4, 1, 0.6) +
  draw_plot(PredictionDensityAixs, 0.01, 0.01, 1, 1) +
  draw_plot(PredictionDensityAixs1, 0.21, 0.1, 1, 0.5)
  
ggsave("Plots/Access_combined_plot.png", plot = combined_plot, width = 8, height = 6)


# -------------------------------
# 方案3：Loss 曲线检验：训练与测试误差随树数量变化
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
# 方案4：k折检验
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
  
  # 计算 ROC 曲线
  roc_obj <- roc(response = test_data$farm, 
                 predictor = predicted_prob,
                 levels = c("nonfarm", "farm"),  # 指定负类与正类
                 direction = "<")
  
  # 打印 AUC 值
  auc_val <- auc(roc_obj)
  
  # 保存当前折的 AUC
  cv_auc <- rbind(cv_auc, data.frame(Fold = i, AUC = as.numeric(auc_val)))
}

print(cv_auc)
mean_auc <- mean(cv_auc$AUC)
cat("平均AUC：", round(mean_auc, 3), "\n")


# -------------------------------
# 方案6：偏依赖图
# -------------------------------
source("Src/Assessing/VIP-PDP.R")


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

save(df, file = "Data/Processed/df.RData")

# -------------------------------
# 12. 可视化农田分布潜力地图
# -------------------------------
# 读取作图函数文件
source("Src/Drawing/world raster map.R")


### （1）第一张图：潜力预测图
# 直接作图
worldraster(farm_potential_matrix, "S5.4(a) 农田分布潜力预测图")


### （2）第二张图：除去已有农田
# 复制农田潜力矩阵并将已有农田区域设为 0
farm_potential_no_farmland <- farm_potential_matrix
farm_potential_no_farmland[veg_mat == 16] <- 0

# 作图
worldraster(farm_potential_no_farmland, "S5.4(b) 农田分布潜力预测图(去除原有农田)")


### （3）第三张图：除去已有农田和人造地形（veg_mat 中编号16和22）
# 同样复制农田潜力矩阵，并将已有农田（16）和人造地形（22）区域设为 0
farm_potential_no_farmland_manmade <- farm_potential_matrix
farm_potential_no_farmland_manmade[veg_mat == 16 | veg_mat == 22] <- 0

# 作图
worldraster(farm_potential_no_farmland_manmade, "S5.4(c) 农田分布潜力预测图(去除原有农田和人造地形)")


# ==============================================================================
# 13: 潜在农田分布分析
# ==============================================================================
# 加载 pROC 包
library(pROC)

# ------------------------------------------
# (1) 在测试集上计算最优阈值
# ------------------------------------------

# 创建 ROC 对象
roc_obj <- roc(response = test_df$farm, 
               predictor = pred_farm_prob,
               levels = c("nonfarm", "farm"))

# 计算最佳阈值。"youden"方法旨在最大化(灵敏度+特异度-1)
# 这是一个在分类问题中平衡两类错误的常用方法
optimal_threshold <- coords(roc_obj, "best", ret = "threshold", best.method = "youden")$threshold

# 打印AUC值和计算出的最优阈值
cat("模型的 AUC 值为:", round(auc(roc_obj), 4), "\n")
cat("基于Youden指数计算出的最优潜力阈值为:", optimal_threshold, "\n")


# -------------------------------
# (2) 全球范围内面积统计
# -------------------------------
# 总陆地面积
total_area <- sum(df$area, na.rm = TRUE)

# 原始农田面积
original_farm_area <- sum(df$area[df$veg == 16], na.rm = TRUE)

# 使用 optimal_threshold 进行计算
potential_farm_area <- sum(df$area[(df$pred_farm_prob >= optimal_threshold) & (df$veg != 16)], na.rm = TRUE)

# 其他土地面积
other_land_area <- total_area - (original_farm_area + potential_farm_area)

cat("原始农田面积 =", format(original_farm_area, scientific = FALSE, big.mark = ","), "m²\n")
cat("潜在农田面积 (基于最优阈值) =", format(potential_farm_area, scientific = FALSE, big.mark = ","), "m²\n")

# 创建用于饼图的数据框
area_df <- data.frame(
  Category = c("原始农田", "潜在农田", "其他土地"),
  Area = c(original_farm_area, potential_farm_area, other_land_area)
)

# 计算百分比
area_df$Pct <- area_df$Area / total_area * 100

# 定义单位换算因子
unit_conversion <- 1e12 # 百万km²

# 生成新的标签，包含新单位
area_df$label <- sprintf("%.1f%%\n%.1f 百万km²", 
                         area_df$Pct, 
                         area_df$Area / unit_conversion)

# 标签颜色逻辑保持不变
area_df$label_color <- ifelse(area_df$Category == "其他土地", "black", "white")

# 调用饼图绘制脚本
source("Src/Drawing/Potential farm area pie chart.R")


# -------------------------------
# (3) 每个国家的面积统计
# -------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

# 利用 dplyr 按国家统计面积
country_areas <- df %>%
  filter(!is.na(country)) %>%
  group_by(country) %>%
  summarise(
    Original_Farm_Area = sum(area[veg == 16], na.rm = TRUE),
    Potential_Farm_Area = sum(area[(pred_farm_prob >= optimal_threshold) & (veg != 16)], na.rm = TRUE)
  )

# 计算总面积并排序，选取前10名
top_10_countries <- country_areas %>%
  mutate(Total_Farm_Area = Original_Farm_Area + Potential_Farm_Area) %>%
  arrange(desc(Total_Farm_Area)) %>%
  slice(1:10)

# 转换为长格式以便绘图
country_long <- top_10_countries %>%
  select(-Total_Farm_Area) %>% # 移除总面积列，以免干扰 pivot
  pivot_longer(cols = c("Original_Farm_Area", "Potential_Farm_Area"),
               names_to = "Farm_Type",
               values_to = "Area")

# 调整因子水平以保持排序
country_long$country <- factor(country_long$country,
                               levels = top_10_countries$country)

# 1. 定义单位换算因子
unit_conversion <- 1e11

# 2. 准备用于 geom_text 的标签数据
label_data <- country_long %>%
  filter(Area > 0) %>%
  group_by(country) %>%
  arrange(country, desc(Farm_Type)) %>%
  mutate(
    Area_in_new_unit = Area / unit_conversion,
    y_pos = cumsum(Area) - 0.5 * Area,
    label_text = sprintf("%.1f", Area_in_new_unit)
  )

# 3. 绘制堆叠柱状图
country_potential <- ggplot(country_long, aes(x = country, y = Area, fill = Farm_Type)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = label_data, 
            aes(y = y_pos, label = label_text), 
            color = "white", # 标签颜色
            size = 3.5) +   # 标签大小
  labs(title = "前10名国家农田面积（现有+潜在）分布",
       subtitle = "基于最优ROC阈值进行潜力评估",
       x = "国家",
       # 【修改】更新Y轴标题以反映新单位
       y = "面积（单位：十万平方千米）",
       fill = "农田类型") +
  scale_y_continuous(
    labels = function(x) {
      x / unit_conversion
    }
  ) +
  
  scale_fill_manual(values = c("Original_Farm_Area" = "#2E7D32", 
                               "Potential_Farm_Area" = "#E31A1C"),
                    labels = c("现有农田", "潜在农田")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# 打印图像
print(country_potential)

# 保存
ggsave(filename = "Plots/国家农田格局.png", plot = country_potential, width = 10, height = 7, dpi = 300)