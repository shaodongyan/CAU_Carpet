
# 如果需要的library没有的话，先下载
if (!"optparse" %in% installed.packages()[, "Package"]) {
  # 这个包用来解析命令行选项
  install.packages("optparse", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
}
library(optparse)
# 解析第一个选项
args <- commandArgs(trailingOnly = TRUE)
command <- args[1]



if (command == "plot") {
  # 如果需要的library没有的话，先下载
  packages <- c("reshape2","tidyverse","jsonlite","readr","RPostgreSQL","ComplexHeatmap","RColorBrewer")
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) {
    install.packages(new_packages, repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
  }
  
  ######包调用#####
  suppressMessages(library(tidyverse))
  library(jsonlite)  
  library(readr)
  library(reshape2)
  library(RColorBrewer)
  library(RPostgreSQL)
  library(ComplexHeatmap)
  #命令行选项
  option_list <- list(
    make_option(c("--config"), type = "character", default = NULL, help = "Path to JSON config file", metavar = "character")
  )
  
  opt_parser <- OptionParser(option_list = option_list)
  opts <- parse_args(opt_parser, args = args[-1])
  json_file_path <- opts$config
  
  
  plot <- function(json_file_path) {
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
    host <- "localhost"
    port <- 5432
    dbname <- "bacterial"
    user <- "bacterial"
    password <- "VastbaseG100"
    con <- dbConnect(PostgreSQL(), host = host, port = port, dbname = dbname, user = user, password = password)
    sql=paste0("UPDATE sys_bacterial_analysis SET view_status = '1',view_png = '",config$others$pngpath,"',view_pdf='",config$others$pdfpath,"' WHERE id = '",config$others$ID,"';")
    sql2=paste0("UPDATE sys_bacterial_analysis SET view_status = '2' WHERE id = '",config$others$ID,"';")
    #print(sql)
    #print(sql2)
    data_file_path=paste0(data_file_path,"/summary.tab")
    # 检查输入文件是否存在
      if (!file.exists(data_file_path)) {
        dbSendQuery(con, sql2) 
        stop(paste("analyse file not found:", data_file_path))
      }
    #读取数据，删除奇怪值，删除不必要的列
    data=read_tsv(data_file_path)
    data <- data[!is.na(data$`#FILE`), ]
    data <- data[, !grepl("^\\.\\.\\.", colnames(data))]
    data <- data[, !grepl("NUM_FOUND", colnames(data))]
    ##数据处理，表格转换
    tryCatch({
      if(config$columns$fanzhuan){  
        data2=t(data)
        data2=as.data.frame(data2)
        colnames(data2) = data2[1,]
        data3 <- data2[rownames(data2) != "#FILE", ]
        data3=as.data.frame(data3)
        rownames(data3) <- rownames(data2)[rownames(data2) != "#FILE"]
        colnames(data3) <- colnames(data2)
        data2=data3
        data2[data2 == "."] <- NA
        data2=as.data.frame(data2)
        data2[] <- lapply(data2, as.numeric)
      }
    }, error = function(e) {
      dbSendQuery(con, sql2) 
      stop("An error occurred while deal with data")
    })
    ##是否进行反转
    if(config$columns$fanzhuan){
      data2=data2
    }else {
      data2=t(data2)##翻转回去
    }
    #判断是否显示数字（如果显示数字，将NA替换为空，并显示coverage）
    data2=as.matrix(data2)
    png(filename = config$others$filename1,type="cairo",res = 300,width =config$plot_settings$width*500,height = config$plot_settings$height*500)
    ht= ComplexHeatmap::Heatmap(data2,
                                cluster_columns = F,#列聚类
                                cluster_rows = F,#行聚类
                                height = nrow(data2)*unit(config$plot_settings$cellheight, "pt") ,#高度
                                width = ncol(data2)*unit(config$plot_settings$cellwidth, "pt"),
                                cell_fun = function(j, i, x, y, width, height, fill) {
                                  if(config$plot_settings$display_numbers)
                                    if(!is.na(data2[i,j]))
                                      grid.text(sprintf("%.1f", data2[i, j]), x, y, gp = gpar(
                                        fontsize = config$plot_settings$fontsize,#字体大小
                                        col=config$plot_settings$number_color))    #颜色))
                                },#展示数值
                                rect_gp = gpar(col = config$plot_settings$border_color),#边框颜色                                
                                column_title = config$general$title,#标题
                                na_col = config$plot_settings$nacolor,
                                color = rev(brewer.pal(n = 7, name = config$plot_settings$color_type)),#颜色                                
                                column_names_rot=config$plot_settings$angle_col,
                                row_names_gp = gpar(fontsize = config$plot_settings$fontsize),
                                column_names_gp = gpar(fontsize = config$plot_settings$fontsize),
                                heatmap_legend_param = list(title = "Identification")
    )
    draw(ht)
    dev.off()
    
    pdf(file = config$others$filename2,width =config$plot_settings$width,height = config$plot_settings$height )
    ComplexHeatmap::Heatmap(data2,
                            cluster_columns = F,#列聚类
                            cluster_rows = F,#行聚类
                            height = nrow(data2)*unit(config$plot_settings$cellheight, "pt") ,#高度
                            width = ncol(data2)*unit(config$plot_settings$cellwidth, "pt"),
                            cell_fun = function(j, i, x, y, width, height, fill) {
                              if(config$plot_settings$display_numbers)
                                if(!is.na(data2[i,j]))
                                  grid.text(sprintf("%.1f", data2[i, j]), x, y, gp = gpar(
                                  fontsize = config$plot_settings$fontsize,#字体大小
                                  col=config$plot_settings$number_color))    #颜色))
                            },#展示数值
                            rect_gp = gpar(col = config$plot_settings$border_color),#边框颜色                            
                            column_title = config$general$title,#标题
                            na_col = config$plot_settings$nacolor,
                            color = rev(brewer.pal(n = 7, name = config$plot_settings$color_type)),#颜色                            
                            column_names_rot=config$plot_settings$angle_col,
                            row_names_gp = gpar(fontsize = config$plot_settings$fontsize),
                            column_names_gp = gpar(fontsize = config$plot_settings$fontsize),
                            heatmap_legend_param = list(title = "Identification")
    )
    dev.off()
    dbSendQuery(con, sql)
  }
  plot(json_file_path)
}