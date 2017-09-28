#读取整个文件夹下的csv文件，编码正常
library("data.table")
filesread = function(x = "./data/原始数据/"){
  files <- dir(x, full.names = TRUE) 
  list <- lapply(files, fread,header = TRUE,data.table = F) 
  data <- do.call(rbind, list) #得到一个data.frame
}
#读取整个文件夹下的csv文件，utf-8编码
filesread_utf8 = function(x = "./data/原始数据/"){
  files <- dir(x, full.names = TRUE) 
  list <- lapply(files, fread,header = TRUE,fileEncoding = "utf-8",data.table = F) 
  data <- do.call(rbind, list) #得到一个data.frame
}
