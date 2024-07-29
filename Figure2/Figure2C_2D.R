setwd("/Users/yeguo/Desktop/宏基因组数据库(最终版)/公司版0418/Statistics_data/MAG/")
library(tidyverse)
library(ggplot2)
library(moonBook)
library(webr)
list.files()
df <- read.csv("MAG_count.csv")
dim(df)

df <- df %>%
  separate(taxonomy, into = c("domain", "phylum", "class", "order", "family", "genus", "species"), sep = ";", fill = "right")

# 提取古菌
archaea <- df %>%
  filter(domain == "d__Archaea") %>%
  arrange(desc(count)) %>%
  head(80)
# 提取细菌
bacteria <- df %>%
  filter(domain == "d__Bacteria") %>%
  arrange(desc(count)) %>%
  head(80)


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
         donutLabelSize = 2,
         pieLabelSize = 1.5,
         titlesize = 6,
         start=8*pi/4)


PieDonut2(archaea,aes(phylum, genus),
         ratioByGroup=F,
         showRatioDonut=F,
         showRatioThreshold = 0.001,
         showRatioPie=F,
         r0 =3, r1=13, r2=19.99,
         max=25,
         color = "white",
         man_color="Set3",
         pieAlpha = 0.8,
         explodePos = 0.2,
         donutLabelSize = 2,
         pieLabelSize = 1.5,
         titlesize = 6,
         start=8*pi/4)



  
  
  
  
  
