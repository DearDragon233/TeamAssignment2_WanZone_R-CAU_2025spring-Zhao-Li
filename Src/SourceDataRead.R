# 自动检查并安装所需的 R 包
dependencies <- c("stars","sf","sp", "raster", "readxl", "tidyverse",'openxlsx')
for (pkg in dependencies) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# 读取 .clr 文件
read_clr <- function(file) {
  data <- readLines(file)
  return(data)
}

# 读取 .tfw 文件
read_tfw <- function(file) {
  data <- readLines(file)
  return(data)
}

# 读取 .tif 文件
library(stars)  # 加载 stars 包

# 读取 .tif 文件
read_tif <- function(file) {
  raster_data <- read_stars(file)  # 直接调用，不加 sf::
  return(raster_data)
}


# 读取 .hdr 文件
read_hdr <- function(file) {
  data <- readLines(file)
  return(data)
}

# 读取 .avl 文件
read_avl <- function(file) {
  data <- readLines(file)
  return(data)
}

# 读取 .xls 文件
read_xls <- function(file) {
  sheets <- excel_sheets(file)
  data_list <- lapply(sheets, function(sheet) {
    read_excel(file, sheet = sheet)
  })
  names(data_list) <- sheets
  return(data_list)
}

# 读取所有文件的主函数
read_all_files <- function(files) {
  result <- list()
  
  for (file in files) {
    ext <- tools::file_ext(file)
    
    if (ext == "clr") {
      result[[file]] <- read_clr(file)
    } else if (ext == "tfw") {
      result[[file]] <- read_tfw(file)
    } else if (ext == "tif") {
      result[[file]] <- read_tif(file)
    } else if (ext == "hdr") {
      result[[file]] <- read_hdr(file)
    } else if (ext == "avl") {
      result[[file]] <- read_avl(file)
    } else if (ext %in% c("xls", "xlsx")) {
      result[[file]] <- read_xls(file)
    } else {
      result[[file]] <- paste("未知文件类型:", ext)
    }
  }
  
  return(result)
}

# 设置 RProject 的相对路径
project_path <- "D:/R Language/WanZone-TW2-Rproj"
setwd(project_path)

# 示例文件路径（相对于 RProject 目录）
files <- c("Data/Raw/glc2000_v1_1.clr", "Data/Raw/glc2000_v1_1.tfw", "Data/Raw/glc2000_v1_1.tif", 
           "Data/Raw/glc2000_v1_1_projinfo.hdr", "Data/Raw/glc2000_v1_legend.avl", "Data/Raw/Global_Legend.xls")

# 读取所有文件
file_contents <- read_all_files(files)

# 打印结果
print(file_contents)

# 创建一个新的工作簿
wb <- createWorkbook()

# 遍历每个文件的内容并写入工作表
for (file in names(file_contents)) {
  # 为每个文件创建一个新的工作表
  addWorksheet(wb, sheetName = basename(file))
  
  content <- file_contents[[file]]
  
  if (is.list(content)) {
    # 如果是列表（如 .xls 文件），遍历每个数据框并写入不同的工作表
    for (sheet_name in names(content)) {
      sheet <- content[[sheet_name]]
      writeData(wb, sheet = basename(file), x = sheet, startCol = 1, startRow = ifelse(sheet_name == names(content)[1], 1, nrow(sheet) + 3))
      if (sheet_name != names(content)[1]) {
        writeData(wb, sheet = basename(file), x = sheet_name, startCol = 1, startRow = nrow(sheet) + 1)
      }
    }
  } else if (class(content) == "stars") {
    # 如果是 stars 对象（如 .tif 文件），写入元数据
    meta_data <- as.data.frame(st_get_dimensions(content))
    writeData(wb, sheet = basename(file), x = meta_data)
  } else {
    # 其他情况，将内容作为文本写入
    writeData(wb, sheet = basename(file), x = data.frame(Content = content))
  }
}

# 保存工作簿为 Excel 文件
saveWorkbook(wb, "Data/Processed/SourceDataRead.xlsx", overwrite = TRUE)
