
# 如果需要的library没有的话，先下载
if (!"optparse" %in% installed.packages()[, "Package"]) {
  # 这个包用来解析命令行选项
  install.packages("optparse", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
}
library(optparse)
# 解析第一个选项
args <- commandArgs(trailingOnly = TRUE)
command <- args[1]



if (command == "analyse") {
  # 如果需要的library没有的话，先下载
  packages <- c("reshape2","tidyverse","jsonlite","readr","RPostgreSQL")
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) {
    install.packages(new_packages, repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
  }
  
  ######包调用#####
  suppressMessages(library(tidyverse))
  library(jsonlite)  
  library(readr)
  library(reshape2)
  library(RPostgreSQL)
  #命令行选项
  option_list <- list(
    make_option(c("--config"), type = "character", default = NULL, help = "Path to JSON config file", metavar = "character")
   )
  
  opt_parser <- OptionParser(option_list = option_list)
  opts <- parse_args(opt_parser, args = args[-1])
  json_file_path <- opts$config

  
abricate <- function(json_file_path) {
  #####文件夹切换#####
  if (!file.exists(json_file_path)) {
    stop(paste("JSON file not found:", json_file_path))
  }
  
  tryCatch(
    {
      config <- fromJSON(json_file_path)
    },
    error = function(e) {
      stop(paste("Error reading JSON file:", e))
    }
  )
  # 从JSON文件中读取列配置
  column_config <- config$columns
  
  # 从JSON中读取数据文件path
  data_file_path <- config$dataFile$dataFilePath
    #切换文件夹
  setwd(config$others$path)  
   #数据库操作
  host <- "192.168.1.225"
  port <- 5432
  dbname <- "bacterial"
  user <- "bacterial"
  password <- "VastbaseG200"
  con <- dbConnect(PostgreSQL(), host = host, port = port, dbname = dbname, user = user, password = password)
  sql=paste0("UPDATE sys_bacterial_analysis SET status = '1',output_zip='",config$others$zippath,"' WHERE id = '",config$others$ID,"';")
  sql2=paste0("UPDATE sys_bacterial_analysis SET status = '2' WHERE id = '",config$others$ID,"';")
  print(sql) 
  print(sql2)
  # 检查输入文件是否存在
  for (i in data_file_path) {
    if (!file.exists(i)) {
      print("no file")
      dbSendQuery(con, sql2) 
      stop(paste("rawdata file not found:", i))
    }
  }
  # 复制原始文件到temp文件夹下
  system("mkdir -p temp")
  for (i in data_file_path){
    temp <- paste0("cp ", i, " temp/")
    system(temp)
  }
  ##新建文件夹../output
  system("mkdir -p ../output")

  temp=paste0("conda run -n amr abricate --nopath --threads ",config$general$threads,
  " --minid ",config$columns$minid,
  " --mincov ",config$columns$mincov,
  " --db ",config$general$db,
  " temp/* > temp/1.tab")
  ##执行命令-分析基因表
  tryCatch({
    system(temp)
  }, error = function(e) {
    dbSendQuery(con, sql2) 
    stop("An error occurred while running abricate")
  })
    tryCatch({
    system("conda run -n amr abricate --summary temp/1.tab > ../output/summary.tab")
  }, error = function(e) {
    dbSendQuery(con, sql2) 
    stop("An error occurred while running abricate")
  })

  #压缩summary.tab 成output.zip
  system("zip ../output/output.zip ../output/summary.tab")
  ##删除temp文件夹
  system("rm -rf temp")
  dbSendQuery(con, sql) 
 
}
    abricate(json_file_path)
    }
