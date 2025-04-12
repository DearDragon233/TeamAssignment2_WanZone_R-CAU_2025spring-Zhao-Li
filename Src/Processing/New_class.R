# 加载必要的包
library(readxl)

# 读取 .xls 文件
file_path <- "Data/Raw/Global_Legend.xls"
data <- read_excel(file_path)

# 提取前两列
data <- data[, 1:2]

# 手动输入一个向量用于给 class 列赋值
manual_class_vector <- c('Forest', 'Forest', 'Forest', 'Forest', 'Forest', 'Forest', 'Forest', 'Forest', 'Forest', 'Forest', 'Shrub', 'Shrub', 'Grass', 'Grass', 'Grass', 'Cultivated', 'Cultivated', 'Cultivated', 'Noplant', 'Noplant', 'Noplant', 'Artificial', 'Nodata') 

# 在数据框右侧新建一列 'class' 并赋值
data$class <- manual_class_vector

# 保存处理后的数据到 RData 文件
save(data, file = "Data/Processed/New_class.RData")
