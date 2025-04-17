library(ggplot2)
library(tidyr)
library(dplyr)
library(UpSetR)

setwd("/Users/yeguo/Desktop/GB文章返修/MAGdb_plot_V2/Figure_5/data/")
list.files()

# 读取文件数据
function_data <- read.delim("function_all_output_cat.emapper.annotations.txt", 
                            header = TRUE, 
                            stringsAsFactors = FALSE)


colnames(function_data) <- c("query", "COG_category", "GOs", "EC", "KEGG_Pathway","CAZy")

# 定义功能类别的列名
categories <- c("COG_category", "GOs", "EC", "KEGG_Pathway","CAZy")

# 创建一个空的数据框来存储每个蛋白是否属于某个功能类别
binary_data <- data.frame(query = function_data$query, row.names = function_data$query)

# 遍历每个类别，创建二进制列
for (category in categories) {
  binary_data[[category]] <- ifelse(function_data[[category]] == "-" | is.na(function_data[[category]]), 0, 1)
}
# 查看转换后的二进制数据
head(binary_data)


queries = list(
  list(query = intersects, params = list("COG_category"), color = "#6B8E23", active = TRUE),
  list(query = intersects, params = list("GOs"), color = "#3CB371", active = TRUE),
  list(query = intersects, params = list("EC"), color = "#20B2AA", active = TRUE),
  list(query = intersects, params = list("KEGG_Pathway"), color = "#2E8B57", active = TRUE),
  list(query = intersects, params = list("CAZy"), color = "#008080", active = TRUE))

#图形相关参数调整：
upset(
  binary_data,
  sets = categories,
  order.by = "freq",
  decreasing = T,
  set_size.show = F, #显示每个集合的总元素数(在左侧条形图中)
  main.bar.color = '#8E5B5B', #上方柱形图颜色
  sets.bar.color = c("#6B8E23", "#3CB371", "#20B2AA", "#2E8B57", "#008080"), #左方条形图颜色
  matrix.color = '#225EA8', #交点颜色
  point.size = 3.3, #交点大小
  line.size = 0.8, #交点连线粗细
  shade.color = 'grey', #交集矩阵中阴影行的颜色
  shade.alpha = 0.2, #阴影行的不透明度
  matrix.dot.alpha = 0.7, #交集矩阵中空交点的不透明度
  mb.ratio = c(0.7, 0.3), #上方柱形图和下方交集矩阵的占比
  scale.intersections = "log10",
  show.numbers = "yes",
  queries = queries
)



