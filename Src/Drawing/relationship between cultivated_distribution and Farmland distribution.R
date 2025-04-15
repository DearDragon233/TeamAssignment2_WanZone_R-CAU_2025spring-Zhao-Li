# 加载必要的包
library(png)
library(ggplot2)

# 读取图像
img1 <- readPNG("Plots/农田面积-纬度图.png")
img2 <- readPNG("Plots/农田占陆地比例-纬度图.png")

# 将图像转换为灰度矩阵
convert_to_gray <- function(img) {
  gray_img <- apply(img[,,1:3], c(1,2), function(x) sum(x * c(0.299, 0.587, 0.114)))
  return(gray_img)
}

gray_img1 <- convert_to_gray(img1)
gray_img2 <- convert_to_gray(img2)

# 调整图像大小以匹配
min_nrow <- min(nrow(gray_img1), nrow(gray_img2))
min_ncol <- min(ncol(gray_img1), ncol(gray_img2))
gray_img1 <- gray_img1[1:min_nrow, 1:min_ncol]
gray_img2 <- gray_img2[1:min_nrow, 1:min_ncol]

# 计算相关性
correlation <- cor(as.vector(gray_img1), as.vector(gray_img2))

# 创建用于绘制散点图的数据框
df <- data.frame(
  img1_pixel = as.vector(gray_img1),
  img2_pixel = as.vector(gray_img2)
)

# 绘制优化后的散点图
ggplot(df, aes(x = img1_pixel, y = img2_pixel)) +
  geom_point(alpha = 0.3, color = "#69b3a2", size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  labs(title = paste("两幅图像像素值散点图，相关性: ", round(correlation, 2)),
       x = "农田面积 - 纬度图 像素值",
       y = "农田占陆地比例 - 纬度图 像素值") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "#d3d3d3", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )

# 输出结果
cat("两幅图像的相关性为:", correlation, "\n")
