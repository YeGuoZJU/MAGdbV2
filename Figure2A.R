getwd()
setwd("/Users/yeguo/Desktop/宏基因组数据库(最终版)/公司版0418/Statistics_data/Rawdata/")
library(ggplot2)
library(ggbreak)
list.files()
data <- read.csv("all_country_count.csv")
dim(data)
head(data)

data$Country=factor(data$Country, levels = data$Country)
color <- c("#97B2CE", "#F89999", "#B3E1B3")

p1 <- ggplot(data=data, aes(x=Country,y=Count, fill=Category)) +
  geom_bar(stat="identity", width=0.8)  + 
  scale_fill_manual(values = color) + theme_bw() +
  xlab("Country") + ylab("# Run accession") + labs(title = "Samples in top 10 Countries")+ 
  theme(axis.text.x=element_text(face = "bold", color="black",size = 10, angle = 70,vjust = 1, hjust = 1),
        axis.text.y=element_text(face = "bold", color="black",size = 10, vjust = 1, hjust = 1),
        panel.background = element_blank(),
        axis.line=element_line(color="black",size = 0.5, linetype = "solid"),
        panel.border=element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(color = "black"),
        axis.title.y = element_text(face = "bold", color = "black", size = 12, vjust = -0.5, hjust = 0.5),
        axis.title.x = element_text(face = "bold", color = "black", size = 12)) 
p1

p2 <- p1 + scale_y_break(breaks = c(1500, 2000), #截断位置及范围
                       space = 0.001, #间距大小
                       scales = 0.3) #上下显示比例，大于1上面比例大，小于1下面比例大
p2

p3 <- p2 + coord_flip() + scale_x_discrete(limits= levels(data$Country)) + ylim(0,3500) + 
  theme(axis.line.x.top = element_blank(), axis.text.x.top = element_blank(),
        axis.ticks.x.top = element_blank(), axis.ticks.y.right = element_blank())
  

p3



