# 加载R包
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)  # 用于加载地图数据

# 获取大陆轮廓数据，用于作图
world <- ne_countries(scale = "medium", returnclass = "sf")

# 定义作图函数
worldraster <- function(Data, title){
  
  # 设置采样的间隔
  sampling_interval <- 10
  
  # 采样矩阵
  mat_sampled <- Data[seq(1, nrow(Data), by = sampling_interval),
                      seq(1, ncol(Data), by = sampling_interval)]
  
  # 转化为长格式
  df <- melt(mat_sampled)
  names(df) <- c("latitude", "longitude", "FarmPotential")
  
  # 使用 ggplot2 绘制热图
  ggplot() +
    geom_sf(data = world, fill = "#ECECEC", color = NA, size = 0.5) +
    geom_raster(data = df, mapping = aes(x = longitude, y = latitude, fill = FarmPotential)) +
    scale_fill_gradient2(mid = "darkgreen", low = "#ECECEC", high = "darkgreen", midpoint = 0.5, na.value = NA) +
    labs(title = title,
         x = "经度",
         y = "纬度") +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) +
    theme_minimal() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.position = "inside",               # 将图例置于图形内部
          legend.position.inside = c(0.05, 0.2),       # 指定内部位置：靠左下
          legend.justification = c("left", "bottom")) +  # 图例对齐方式
    geom_sf(data = world, fill = NA, color = "grey", size = 0.5)
  
  # 保存为图像
  ggsave(paste0("Plots/",title,".png"), width = 9, height = 5)
}