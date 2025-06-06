# All MAG in clincial,Environment, Animal 
getwd()
setwd("/Users/yeguo/Desktop/GB文章返修/MAGdb_plot_V2/Figure_3/data/")
library(tidyverse)
library(ggplot2)
library(ggforce)
library(ggrepel)
list.files()

# 创建一个函数来处理数据
process_data <- function(data) {
  
  species_counts <- data %>%
    mutate(
      species = str_split(paste0(";", Classification), ";") %>% map_chr(8)
    ) %>%
    mutate(species = str_replace(species, "^s__$", "Unknown")) %>%
    count(species) %>%
    top_n(10, n) %>%
    rename(taxon = species, count = n)
  
  genus_counts <- data %>%
    mutate(
      genus = str_split(paste0(";", Classification), ";") %>% map_chr(7)
    ) %>%
    mutate(genus = str_replace(genus, "^g__$", "Unknown")) %>%
    count(genus) %>%
    top_n(10, n) %>%
    rename(taxon = genus, count = n)
  
  family_counts <- data %>%
    mutate(
      family = str_split(paste0(";", Classification), ";") %>% map_chr(6)
    ) %>%
    mutate(family = str_replace(family, "^f__$", "Unknown")) %>%
    count(family) %>%
    arrange(desc(n)) %>%
    top_n(10, n) %>%
    rename(taxon = family, count = n)
  
  order_counts <- data %>%
    mutate(
      order = str_split(paste0(";", Classification), ";") %>% map_chr(5)
    ) %>%
    mutate(order = str_replace(order, "^o__$", "Unknown")) %>%
    count(order) %>%
    top_n(10, n) %>%
    rename(taxon = order, count = n)
  
  # 添加标识符列
  species_counts <- species_counts %>% mutate(Taxonomic_level = "species")
  genus_counts <- genus_counts %>% mutate(Taxonomic_level = "genus")
  family_counts <- family_counts %>% mutate(Taxonomic_level = "family")
  order_counts <- order_counts %>% mutate(Taxonomic_level = "order")
  
  # 追加结果
  combined_data <- bind_rows(species_counts, genus_counts, family_counts, order_counts)
  
  # 计算每个分类级别的总频数
  total_counts <- combined_data %>%
    group_by(Taxonomic_level) %>%
    summarise(total = sum(count))
  
  # 合并数据和总频数
  combined_data <- left_join(combined_data, total_counts, by = "Taxonomic_level")
  
  # 计算百分比
  combined_data <- combined_data %>%
    mutate(percentage = count / total * 100)
  
  # 重新排序因子级别
  combined_data$Taxonomic_level <- factor(combined_data$Taxonomic_level,
                                          levels = c("order", "family", "genus", "species"))
  
  # 绘制百分比图
  taxon_colors <- c("#515D73", "#1E4A5D", "#E39889", "#B4D6A8", "#000000", "#E9744E", "#80ACF9", "#E64B35FF", "#DE7833",
                    "#AEC0E0", "#FBDF94", "#E5E37A", "#C7AF3C", "#952B45", "#E5885D", "#8ACEDC", "#B09C85FF", "#F0B2EF",
                    "#B67D59", "#B8BADA", "#5796C1", "#B07F82", "#474747", "#8BBAAD", "#1399B2", "#3C5488FF", "#D9D1E3",
                    "#80d7e1", "#e4cbf2", "#ffb7ba", "#bf5046", "#D4E6BC", "#ece7a3", "#008B4599", "#EB9184", "#8FA2C3",
                    "#f5cbe1", "#e6e5e3", "#d2b5ab", "#d9e3f5", "#f29432", "#00A087FF", "#8491B4FF", "#4087B9", "#CDB25C",
                    "#8DD2C5", "#BFBCDA", "#F47F72", "#7FB2D5", "#FBB461", "#B5D66B", "#D9D9D9", "#B5CBE2", "#551C6E",
                    "#42BCB2", "#A37E7D", "#516770", "#EDD6D7", "#6FAE45", "#9b4692", "#AEB6CF", "#C58F84", "#9c9895",
                    "#B6AD9C", "#1E90FF", "#800000", "#F58383", "#43978F", "#9EC4BE", "#ABD0F1", "#DCE9F4", "#E56F5E",
                    "#F19685", "#F6C957", "#FFB77F", "#FBE8D5", "#912C2C", "#F2BB6B", "#C2ABC8", "#329845", "#AED185",
                    "#276C9E", "#A3C9D5", "#BFC1A5", "#F6C6F6", "#98CAF7", "#FFF8AB", "#CDC0DB", "#A0DDAF", "#F8D3A9",
                    "#F2BF9E", "#4091CF", "#A1C6E7", "#8CBA54", "#CCDDAE", "#97C8AF", "#8FC9E2", "#A797DA")
  
  ggplot(combined_data, aes(x = Taxonomic_level, y = percentage, fill = taxon)) +
    geom_bar(stat = "identity", width = 0.7) +
    labs(title = "", x = "Taxonomic Level", y = "Proportion(%)") +
    theme_minimal() +
    scale_fill_manual(values = taxon_colors, breaks = rev(unique(combined_data$taxon))) +
    theme(legend.position = "right", 
          panel.background = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_rect(fill = NA, color = "black", size = 0.75, linetype = "solid"),
          axis.line = element_line(color = "black", size = 0.05, linetype = "solid"),
          axis.text = element_text(color = "black"),
          axis.title = element_text(color = "black"),
          axis.ticks = element_line(color = "black"),
          legend.text = element_text(color = "black", size = 10),
          axis.text.x = element_text(color = "black", size = 12),
          axis.text.y = element_text(color = "black", size = 12),
          axis.title.x = element_text(color = "black", size = 14, face = "bold"),
          axis.title.y = element_text(color = "black", size = 14, face = "bold"),
          legend.title = element_text(size = 12, face = "bold")) +
    guides(fill = guide_legend(ncol = 2, byrow = F, title = "Taxon"))
}

# 处理三个数据集
animal_data <- read.csv("MAG_Animal.csv")
clinical_data <- read.csv("MAG_Clincial.csv")
environment_data <- read.csv("MAG_Environment.csv")

# 执行处理函数
process_data(animal_data)
process_data(clinical_data)
process_data(environment_data)
