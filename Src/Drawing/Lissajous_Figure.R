library('ggplot2')
library('minpack.lm')
library('signal')
# 加载数据
load("Data/Processed/TIF_2DMatrix.RData") 

# 选择两个纬度索引

lat_index1 <- 100
lat_index2 <- 50

# 提取对应经度上的 value 变化
x_data <- mat[, lat_index1]  # 第一个纬度
y_data <- mat[, lat_index2]  # 第二个纬度
lon <- 1:nrow(mat)  # 生成经度索引

# 利用傅里叶变换进行简谐近似
fft_smooth <- function(data, keep=5) {
  fft_result <- fft(data)
  fft_result[(keep+1):(length(data)-keep)] <- 0  # 只保留前 keep 个频率成分
  Re(fft(fft_result, inverse = TRUE) / length(data))  # 逆变换
}

x_smooth <- fft_smooth(x_data, keep = 5)
y_smooth <- fft_smooth(y_data, keep = 5)

# 绘制李萨如图
df <- data.frame(x = x_smooth, y = y_smooth)

ggplot(df, aes(x, y)) +
  geom_path(color = "blue", size = 1) +
  theme_minimal() +
  labs(title = "李萨如图（经度方向的简谐近似）", x = "纬度索引 1", y = "纬度索引 2") +
  coord_fixed()
