library(readxl)
library(scales)   # 用于颜色转换

# 加载图例文件
leg <- read_excel("Data/Raw/Global_Legend.xls")

# 将图例RGB信息转换为16进制颜色代码
plant_colors <- rgb(leg[[3]], leg[[4]], leg[[5]]) # 3,4,5 列对应 R,G,B值
names(plant_colors) <- leg$CLASSNAMES             # 将颜色名称映射到植被类别

# 保存为.RData文件
save(plant_colors, file = "Data/Processed/plant_colors.RData")  # 图例颜色
