# 加载必要的包
library(readxl)
library(terra)

# 读取 Excel 图例文件
legend_file <- "Data/raw/Global_Legend.xls"
legend <- read_excel(legend_file)
colnames(legend) <- c("VALUE", "CLASSNAMES", "Red", "Green", "Blue")

# 检查颜色值范围（关键步骤！）
# 如果颜色值已经是0-255，无需乘以255；若为0-1则需转换
# 示例假设颜色值为0-1（根据数据实际情况调整）
legend$Red <- legend$Red * 255
legend$Green <- legend$Green * 255
legend$Blue <- legend$Blue * 255

# 读取原始分类栅格
tif_file <- "Data/raw/glc2000_v1_1.tif"
r <- rast(tif_file)

# 方法：使用分类替换生成RGB各波段
# -------------------------------------------------
# 步骤1：创建颜色查找表（LUT）
color_lut <- legend[, c("VALUE", "Red", "Green", "Blue")]

# 步骤2：分别替换每个波段
red_layer <- subst(r, from = color_lut$VALUE, to = color_lut$Red)
green_layer <- subst(r, from = color_lut$VALUE, to = color_lut$Green)
blue_layer <- subst(r, from = color_lut$VALUE, to = color_lut$Blue)

# 步骤3：合并为RGB栅格
rgb_raster <- c(red_layer, green_layer, blue_layer)
names(rgb_raster) <- c("Red", "Green", "Blue")

# 步骤4：设置数据类型为8位无符号整型（0-255）
rgb_raster <- clamp(rgb_raster, lower=0, upper=255)  # 确保值在0-255
rgb_raster <- round(rgb_raster)  # 转换为整数
rgb_raster <- as.int(rgb_raster)  # 设置数据类型

# 可视化与输出
plot(rgb_raster)
output_file <- "Plots/rgb_image.tif"
writeRaster(rgb_raster, filename = output_file, 
            datatype = "INT1U",  # 指定8位整型
            overwrite = TRUE)
