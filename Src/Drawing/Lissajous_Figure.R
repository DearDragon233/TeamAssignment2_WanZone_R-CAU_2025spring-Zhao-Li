library('ggplot2')
library('minpack.lm')
library('signal')
# 加载数据
load("Data/Processed/TIF_2DMatrix.RData") 

# 选择两个经度索引
lat_index1 <- 4300
lat_index2 <- 7500

# 查看对应经度
colnames(mat)[c(lat_index1, lat_index2)]

# 提取对应纬度上的 value 变化
x_data <- mat[, lat_index1]  # 第一个经度
y_data <- mat[, lat_index2]  # 第二个经度
lon <- 1:nrow(mat)  # 生成纬度索引

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
  geom_abline(linewidth = 1, colour = "red") +
  annotate("text", x = 12, y = 13.2, label = "y = x", color = "red", size = 5) +
  geom_path(color = "blue", size = 1) +
  theme_minimal() +
  labs(title = "李萨如图（纬度方向的简谐近似）",
       x = paste0("纬度（经度：",colnames(mat)[lat_index1], "）"),
       y = paste0("纬度（经度：",colnames(mat)[lat_index2], "）")) +
  coord_fixed()

# 储存图像
ggsave("Plots/Lissajous_Figure.png", width = 5, height = 5)
