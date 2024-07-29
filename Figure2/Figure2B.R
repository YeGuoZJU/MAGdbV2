library(ggplot2)
setwd("/Users/yeguo/Desktop/宏基因组数据库(最终版)/公司版0418/Statistics_data/MAG/小提琴图/")
list.files()
violin <- read.csv("MAG_quality.csv")

p1 <- ggplot(violin, aes(x = Completeness, y = 0)) +
  geom_violin(trim = TRUE,bw = 0.4, width = 0.05, fill ="#FCB2AF") +
  geom_boxplot(width = 0.01,fill = "#E64A35")  +
  theme_classic() + 
  scale_x_continuous(limits = c(90, 100), breaks = seq(90, 100, by = 1),expand = c(0, 0)) +
  theme(axis.title.y = element_blank(),  # 隐藏纵坐标标题
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold")) + # 隐藏纵坐标文本
  xlab("Completeness(%)")
p1

p2 <- ggplot(violin, aes(x = Contamination, y = 0)) +
  geom_violin(trim = TRUE,bw = 0.2, width = 0.05, fill ="#B8E4EE") +
  geom_boxplot(width = 0.01,fill = "#4DBBD5")  +
  theme_classic() + 
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1),expand = c(0, 0)) +
  theme(axis.title.y = element_blank(),  # 隐藏纵坐标标题
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold")) + # 隐藏纵坐标文本
  xlab("Contamination(%)")



p3 <- ggplot(violin, aes(x = log10(as.numeric(N50)), y = 0)) +
  geom_violin(trim = TRUE,bw = 0.02, width = 0.05, fill ="#99D9CF") +
  geom_boxplot(width = 0.01,fill = "#00A087")  +
  theme_classic() + 
  scale_x_continuous(limits = c(3.5, 6), breaks = seq(3, 6, by = 0.5),expand = c(0, 0)) +
  theme(axis.title.y = element_blank(),  # 隐藏纵坐标标题
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold")) + # 隐藏纵坐标文本
  xlab("N50(log10)")

p4 <- ggplot(violin, aes(x = log10(as.numeric(N_contigs)), y = 0)) +
  geom_violin(trim = TRUE,bw = 0.1, width = 0.05, fill ="#B1BBCF") +
  geom_boxplot(width = 0.01,fill = "#3C5487")  +
  theme_classic() + 
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 1),expand = c(0, 0)) + 
  theme(axis.title.y = element_blank(),  # 隐藏纵坐标标题
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = "black"),
        panel.grid = element_blank(),
        axis.title.x = element_text(size = 12, face = "bold")) + # 隐藏纵坐标文本
  xlab("N_contigs(log10)")
p4


# 创建一些空白图来作为间隔
blank_plot <- ggplot() + theme_void()


library(cowplot)
library(tidyverse)
plot_grid(p1,blank_plot, p2,blank_plot, p3,blank_plot, p4,
          ncol=1,
          rel_heights = c(1, 0.1, 1, 0.1, 1, 0.1, 1))




