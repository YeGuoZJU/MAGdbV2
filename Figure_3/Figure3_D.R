setwd("/Users/yeguo/Desktop/GB文章返修/MAGdb_plot_V2/Figure_3/data/")
library(tidyverse)
list.files()
df <- read.csv("sankey_top_20.csv")
dim(df)

df <- df %>%
  separate(Classification, into = c("domain", "phylum", "class", "order", "family", "genus", "species"), sep = ";", fill = "right")


#整合分类和频次的嵌套关系，构建桑基图 link 列表
catregory_phylum <- aggregate(df$Count, by = list(df$Catregory, df$phylum), FUN = sum)
phylum_family <- aggregate(df$Count, by = list(df$phylum, df$family), FUN = sum)
family_genus <- aggregate(df$Count, by = list(df$family, df$genus), FUN = sum)
genus_species <- aggregate(df$Count, by = list(df$genus, df$species), FUN = sum)
link_list <- rbind(catregory_phylum, phylum_family, family_genus, genus_species)
colnames(link_list) <- c('source',"target","value")

#构建 node 列表，并为 link 列表中的分类名称分配 id 指代
node_list <- data.frame(name=unique(c(link_list$source,link_list$target)))
head(node_list)
link_list$IDsource <- match(link_list$source, node_list$name) - 1
link_list$IDtarget <- match(link_list$target, node_list$name) - 1
head(link_list)

#networkD3 包的桑基图
library(networkD3)
p <- sankeyNetwork(Links = link_list, Nodes = node_list,
                   Source = 'IDsource', Target = 'IDtarget', Value = 'value',
                   NodeID = 'name',
                   fontSize = 12, sinksRight = FALSE) 
p
# 结果保存
htmlwidgets::saveWidget(p, file="Figure3_d.html")# 保存网页可交互式动态结果
webshot::webshot(url="Figure3_d.html", file="Figure3_d.jpg")# 以图片格式进行保存



