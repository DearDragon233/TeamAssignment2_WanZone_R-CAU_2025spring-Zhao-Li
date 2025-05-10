#数据处理部分
source("Src/Processing/TIF_2DMatrix.R")
source("Src/Processing/Color_conversion.R")
source("Src/Processing/New_class.R")
source("Src/Processing/Vegetation_area~Latitude.R")
source("Src/Processing/Pixel_area_Latitude.R")
source("Src/Processing/world_climate.R")

#作图部分
source("Src/Drawing/Vegetation type - latitude density map.R")  # vtldm1 vtldm2
source("Src/Drawing/Vegetation type - latitude heat map.R")     # vtlhm
source("Src/Drawing/Terrain_Frequency.R")                       # tf
source("Src/Drawing/terrain_quality_radarchart.R")              # tqr
source("Src/Drawing/Area pie chart.R")                          # apc
source("Src/Drawing/cultivated_distribution.R")                 # cd
source("Src/Drawing/grassland_distribution.R")
source("Src/Drawing/Lissajous_Figure.R")                        # lf
source("Src/Drawing/Spatial co-occurrence probability.R")       # scp
source("Src/Drawing/Farmland distribution.R")                   # fd1 fd2

# 拼合图像用于展示
library(cowplot)
co_plot1 <- ggdraw() +
  draw_plot(tqr, -0.06, 0.1, 0.28, 0.4) +
  draw_plot(apc, 0.15, 0, 0.3, 0.6) +
  draw_plot(scp, 0.45, 0, 0.55, 0.6) +
  draw_plot(vtlhm, 0, 0.59, 0.2, 0.41) +
  draw_plot(cd, 0.2, 0.6, 0.52, 0.4) +
  draw_plot(vtldm2, 0.72, 0.6, 0.08, 0.4) +
  draw_plot(fd1, 0.8, 0.6, 0.1, 0.4) +
  draw_plot(fd2, 0.9, 0.6, 0.1, 0.4)

ggsave("Plots/原数据信息挖掘汇总.png", plot = co_plot1, width = 32, height = 18, bg = "white")
  