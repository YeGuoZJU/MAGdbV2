library(ggplot2)
setwd("/Users/yeguo/Desktop/GB文章返修/MAGdb_plot_V2/Figure_2/data/")
list.files()
violin <- read.csv("All_merge_MAGs.csv")

p1 <- ggplot(violin, aes(x = Completeness, y = 0)) +
  geom_violin(trim = TRUE,bw = 0.4, width = 0.05, fill ="#FCB2AF") +
  geom_boxplot(width = 0.01,fill = "#E64A35")  +
  theme_classic() + 
  scale_x_continuous(limits = c(90, 100), breaks = seq(90, 100, by = 1),expand = c(0, 0)) +
  theme(axis.title.y = element_blank(),  # 隐藏纵坐标标题
        axis.text.x = element_text(size = 14),  # 调整 x 轴数字字体大小
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_text(size = 16, face = "bold")) + # 隐藏纵坐标文本
  xlab("Completeness(%)")
p1

p2 <- ggplot(violin, aes(x = Contamination, y = 0)) +
  geom_violin(trim = TRUE,bw = 0.2, width = 0.05, fill ="#B8E4EE") +
  geom_boxplot(width = 0.01,fill = "#4DBBD5")  +
  theme_classic() + 
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1),expand = c(0, 0)) +
  theme(axis.title.y = element_blank(),  # 隐藏纵坐标标题
        axis.text.x = element_text(size = 14),  # 调整 x 轴数字字体大小
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_text(size = 16, face = "bold")) + # 隐藏纵坐标文本
  xlab("Contamination(%)")

p2

p3 <- ggplot(violin, aes(x = as.numeric(Size/1000000), y = 0)) +
  geom_violin(trim = TRUE,bw = 0.02, width = 0.05, fill ="#99D9CF") +
  geom_boxplot(width = 0.01,fill = "#00A087")  +
  theme_classic() + 
  scale_x_continuous(limits = c(0.5, 13), breaks = seq(0.5, 12.5, by = 2),expand = c(0, 0)) +
  theme(axis.title.y = element_blank(),  # 隐藏纵坐标标题
        axis.text.x = element_text(size = 14),  # 调整 x 轴数字字体大小
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_text(size = 16, face = "bold")) + # 隐藏纵坐标文本
  xlab("Genome size(Mb)")
p3

p4 <- ggplot(violin, aes(x = GC_content*100, y = 0)) +
  geom_violin(trim = TRUE,bw = 0.8, width = 0.05, fill ="#B1BBCF") +
  geom_boxplot(width = 0.01,fill = "#3C5487")  +
  theme_classic() + 
  scale_x_continuous(limits = c(20, 75), breaks = seq(20,75 , by = 5),expand = c(0, 0)) + 
  theme(axis.title.y = element_blank(),  # 隐藏纵坐标标题
        axis.text.x = element_text(size = 14),  # 调整 x 轴数字字体大小
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_text(size = 16, face = "bold")) + # 隐藏纵坐标文本
  xlab("GC_content(%)")
p4

# 创建一些空白图来作为间隔
blank_plot <- ggplot() + theme_void()


library(cowplot)
library(tidyverse)
plot_grid(p1,blank_plot, p2,blank_plot, p3,blank_plot, p4,
          ncol=1,
          rel_heights = c(1, 0.1, 1, 0.1, 1, 0.1, 1))




