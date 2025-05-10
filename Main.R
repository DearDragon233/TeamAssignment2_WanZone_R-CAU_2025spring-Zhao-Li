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