# 移除所有环境变量
rm(list = ls())

# 加载包
library(ggtreeExtra) 
library(dplyr)
library(ggtree)     
library(ggplot2) 
library(ggnewscale) 
library(ggstar) 
library(ape)            

# 读取 Newick 格式的进化树
# 设置工作目录
setwd("/Users/yeguo/Desktop/GB文章返修/MAGdb_plot_V2/Figure_5/tree_data/")
list.files()
tree <- read.tree("all_boot_bac_tree.nwk")

# 取出树的标签（Bin）
tree_labels <- tree$tip.label

write.table(tree$tip.label, file = "all_tip_labels.csv", sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)

# 读入注释文件
tree_annotation <- read.csv("MAG_tree_annoation.csv", header = TRUE, stringsAsFactors = FALSE)

# 提取与树标签匹配的注释数据
filtered_annotations <- tree_annotation %>%
  filter(Bin %in% tree_labels) %>% distinct(Bin, .keep_all = TRUE)

# 查看匹配的注释信息
head(filtered_annotations)

write.table(filtered_annotations, file = "all_tree_labels.csv", sep = ",", quote = FALSE, row.names = F, col.names = T)

# 手动处理，将门数量较少的统一称为others,重新命名文件为1000_tree_labels.txt
data_file_1 <- "all_tree_labels.txt"
data_1 <- data.table::fread(data_file_1)

cladegroup <- split(data_1$Bin,data_1$p)
tree2 <- groupOTU(tree,cladegroup)

# 设置颜色映射
pal.alb <- c("Others" = "#515D73",
             "p__Acidobacteriota" = "#BCD7E3",
             "p__Actinobacteriota" = "#99CFED",
             "p__Actinomycetota" = "#82A6C1",
             "p__Bacillota" = "#7778AE",
             "p__Bacillota_A" = "#88CAB9",
             "p__Bacillota_C" = "#E2AAC1",
             "p__Bacteroidota" = "#9ABB87",
             "p__Firmicutes"= "#EC9C46",
             "p__Firmicutes_A" ="#E8E49F",
             "p__Proteobacteria" = "#B7796E",
             "p__Pseudomonadota" = "#9b4692",
             "p__Verrucomicrobiota" = "#952B45")


# 设置颜色映射
legend.labels <- list("Others" = "Others",
             "p__Acidobacteriota" = "p__Acidobacteriota",
             "p__Actinobacteriota" = "p__Actinobacteriota",
             "p__Actinomycetota" = "p__Actinomycetota",
             "p__Bacillota" = "p__Bacillota",
             "p__Bacillota_A" = "p__Bacillota_A",
             "p__Bacillota_C" = "p__Bacillota_C",
             "p__Bacteroidota" = "p__Bacteroidota",
             "p__Firmicutes"= "p__Firmicutes",
             "p__Firmicutes_A" ="p__Firmicutes_A",
             "p__Proteobacteria" = "p__Proteobacteria",
             "p__Pseudomonadota" = "p__Pseudomonadota",
             "p__Verrucomicrobiota" = "p__Verrucomicrobiota")

legend.title.size = 8
legend.text.size = 6
legend.key.size = 0.4
tree.title = 8
tree.text = 7

# 图例统一样式设置
legend.theme <- theme( legend.position = 'right', legend.key.spacing.y = unit(x = 0.15, units = 'cm'),
                       legend.title = element_text(face = 'bold', size = legend.title.size, margin = margin(b = 5)),
                       legend.text = element_text(size = legend.text.size),
                       legend.key.size = unit(legend.key.size, 'cm'))

p<- ggtree(tree2, aes(color = group), layout="fan",size=0.5, open.angle = 15) 
  
p

p1 <- rotate_tree(p, angle = 0) +
  geom_aline(aes(color = group), linetype = 'longdash', linewidth = 0, size = 0.025, show.legend = TRUE) +
  ggplot2::scale_color_manual(values = pal.alb, na.value = '#000000') +
  guides(colour = "none") 

p1

p2 <- p1 + geom_fruit(
  data = data_1, #指定数据源
  stat = 'identity',
  aes(y = Bin,fill = p), #映射变量
  alpha = 0.6,
  width = 0.7,
  offset = 0.12,
  geom = geom_bar 
) + 
  scale_fill_manual(values = pal.alb,
                    breaks = names(legend.labels),
                    labels = legend.labels, 
                    guide = guide_legend(title = 'Phylum',
                                         reverse = FALSE,
                                         override.aes = list(size = legend.key.size)
                                         )) + legend.theme

p2

p3 <- p2 + guides(color = 'none') + new_scale_fill() + geom_fruit(     
    data = data_1,      
    geom = geom_bar,     
    stat = 'identity',  
    offset = -0.15,
    width = 0.5,       
    mapping = aes(y = Bin, x = Size, fill = "Size"),     
    pwidth = 0.5,     
    axis.params = list(       
      axis = "x",       
      text.size = 3,  # Set specific value       
      text.angle = 0,       
      title = "Size(Mb)",       
      title.size = 4,  # Set specific value       
      title.angle = 0,       
      line.size = 0,       
      line.color = "white",       
      limits = c(0, 5)
    )) +       
  scale_fill_manual(         
    name = "Size",          
    values = c("Size" = "#CACACA"),         
    guide = guide_legend()       
  ) +       
  theme(         
    legend.position = "right",         
    axis.text = element_text(colour = '#000000', size = 11),
    panel.grid.major = element_blank()
  )

p3

p4 <- p3 +
  guides(fill = 'none') +
  new_scale_fill() + 
  geom_fruit(
    data = data_1, 
    geom = geom_bar, 
    stat = 'identity',
    offset = -0.1,
    width = 1, 
    aes(y = Bin, x = 1, fill = p),
    pwidth = 0.05
  ) +
  scale_fill_manual(values = pal.alb)
p4

p5 <- p4 +
  guides(fill = 'none') +
  new_scale_fill() + 
  new_scale_color() +  # 添加新的颜色标度，避免替换警告
  geom_fruit(
    data = data_1, 
    geom = geom_point, 
    stat = 'identity',
    offset = 0.015,
    aes(
      y = Bin, 
      x = 0.1, 
      fill = ifelse(data_1$Annotated.at.the.species.level == "Yes", "yes", "no"),
      color = ifelse(data_1$Annotated.at.the.species.level == "Yes", "yes", "no")
    ),
    pwidth = 0.02, 
    size = 0.8,  # 调整点的大小
    shape = 21  # 21: 带边框的可填充圆点
  ) +
  scale_fill_manual(
    name = "Annotated at species level",
    values = c("yes" = "#c11d14", "no" = "white"),  # yes=浅红填充，no=白色填充
    labels = c("yes" = "Yes", "no" = "No") # 隐藏fill图例
  ) +
  scale_color_manual(
    values = c("yes" = "#c11d14", "no" = "black"),  # yes=红色边框，no=黑色边框
    name = "Annotated at species level",  # 图例标题
    labels = c("yes" = "Yes", "no" = "No")) + 
  theme(legend.position = "right") + 
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 3, color = c("black","#c11d14"))))

p5

p6 <- p5 +
  guides(fill = 'none') +
  new_scale_fill() + 
  geom_fruit(
    data = data_1, 
    geom = geom_bar, 
    stat = 'identity',
    offset = 0.05,
    width = 1, 
    aes(y = Bin, x = 0.1, fill = Category),
    pwidth = 0.05
  ) + scale_fill_manual(
    name = "Category",  # 图例标题
    values = c(
      "Environment" = "#4E79A7",  # 蓝色系
      "Clinical" = "#E15759",     # 红色系
      "Animal" = "#79706E"         # 灰色备用
    )) + legend.theme
p6




