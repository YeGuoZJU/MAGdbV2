setwd("/Users/yeguo/Desktop/GB文章返修/MAGdb_plot_V2/Figure_2/data/")
library(tidyverse)
library(ggplot2)
library(moonBook)
library(webr)
list.files()
data <- read.csv("All_merge_MAGs.csv")
dim(data)

# 统计完整的Classification列频次
full_classification_counts <- table(data$Classification)
# 按频次降序排列
full_classification_counts <- sort(full_classification_counts, decreasing = TRUE)
# 转换为数据框并设置列名
result_df <- data.frame(
  Classification = names(full_classification_counts),
  Count = as.numeric(full_classification_counts)
)

# 查看前几行
head(result_df)

# 写入CSV文件
# write.csv(result_df, "classification_counts.csv", row.names = FALSE)

result_df <- result_df %>%
  separate(Classification, into = c("domain", "phylum", "class", "order", "family", "genus", "species"), sep = ";", fill = "right")

# 提取古菌
archaea <- result_df %>%
  filter(domain == "d__Archaea") %>%
  arrange(desc(Count)) %>%
  head(100)

# 计算每个 phylum 的数量并排序
phylum_counts <- archaea %>%
  count(phylum) %>%
  arrange(desc(n))

# 将 phylum 转换为因子，并按数量从大到小排序
archaea$phylum <- factor(archaea$phylum, levels = phylum_counts$phylum, ordered = TRUE)

# 计算每个 genus 的数量并排序
genus_counts <- archaea %>%
  count(genus) %>%
  arrange(desc(n))

# 将 genus 转换为因子，并按数量从大到小排序
archaea$genus <- factor(archaea$genus, levels = genus_counts$genus, ordered = TRUE)

PieDonut(archaea,aes(phylum, genus),
         ratioByGroup=F,
         showRatioDonut=F,
         showRatioThreshold = 0.001,
         showRatioPie=F,
         r0 =3, r1=13, r2=19.99,
         max=25,
         color = "white",
         pieAlpha = 0.8,
         explodePos = 0.2,
         pieLabelSize = 4,
         donutLabelSize = 3,
         titlesize = 5,
         start=8*pi/4)

# 提取细菌
bacteria <- result_df %>%
  filter(domain == "d__Bacteria") %>%
  arrange(desc(Count)) %>%
  head(100)

# 计算每个 细菌phylum 的数量并排序
phylum_counts <- bacteria %>%
  count(phylum) %>%
  arrange(desc(n))

# 将 phylum 转换为因子，并按数量从大到小排序
bacteria$phylum <- factor(bacteria$phylum, levels = phylum_counts$phylum, ordered = TRUE)

# 计算每个 genus 的数量并排序
genus_counts <- bacteria %>%
  count(genus) %>%
  arrange(desc(n))

# 将 genus 转换为因子，并按数量从大到小排序
bacteria$genus <- factor(bacteria$genus, levels = genus_counts$genus, ordered = TRUE)

PieDonut(bacteria,aes(phylum, genus),
         ratioByGroup=F,
         showRatioDonut=F,
         showRatioThreshold = 0.001,
         showRatioPie=F,
         r0 =3, r1=13, r2=19.99,
         max=25,
         color = "white",
         pieAlpha = 0.8,
         explodePos = 0.2,
         pieLabelSize = 4,
         donutLabelSize = 3,
         titlesize = 5,
         start=8*pi/4)



  
  
  
  
  
