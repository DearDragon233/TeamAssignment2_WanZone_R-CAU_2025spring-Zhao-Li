library(terra)

# 读取 .tif 并降低分辨率
r <- rast("Data/Raw/glc2000_v1_1.tif")
r <- aggregate(r, fact=4)  # n倍降采样，减少数据量

# 转换为数据框
df <- as.data.frame(r, xy = TRUE)

# 保存为 Excel
write.xlsx(df, "Data/Processed/TIFtransfertoXLSX.xlsx")
