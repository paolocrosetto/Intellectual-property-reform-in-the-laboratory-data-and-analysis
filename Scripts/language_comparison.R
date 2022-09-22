###
### Author: Paolo Crosetto
### Date:   September 2022
###
### Aim:    Analyse and compare the extendibility of each language (FR,DE,EN)
###
### Data:   From running the "Word_Extendibility.py" file for each of three languages
###

### 0. libraries
library(tidyverse)
library(patchwork)


### 1. importing data
df <- read_csv("Data/extendibility.csv")


### 2. tests

## t

t.test(df$extendibility[df$dictionary=="French"], df$extendibility[df$dictionary=="German"]) %>% 
  broom::tidy()


## ks

ks.test(df$extendibility[df$dictionary=="French"], df$extendibility[df$dictionary=="German"]) %>% 
  broom::tidy()

## mw

wilcox.test(df$extendibility[df$dictionary=="French"], df$extendibility[df$dictionary=="German"]) %>% 
  broom::tidy()



### 3. plotting

my_theme <- hrbrthemes::theme_ipsum()+
  theme(legend.position = "bottom", 
        panel.grid.minor = element_blank(), 
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust=0.5))

p1 <- df %>% 
  ggplot(aes(x = extendibility, color = dictionary, linetype = dictionary))+
  geom_density(alpha = 0, size = 0.7)+
  scale_color_brewer(name = "", palette = "Set1", )+
  scale_linetype_manual(name = "", values = c("dotdash", "solid", "dotted"))+
  guides(color = "none", linetype = "none")+
  labs(title = "Kernel density estimate")+
  my_theme


p2 <- df %>% 
  ggplot(aes(x = extendibility, color = dictionary, linetype = dictionary))+
  stat_ecdf(size = 0.7)+
  scale_color_brewer(name = "", palette = "Set1")+
  scale_linetype_manual(name = "", values = c("dotdash", "solid", "dotted"))+
  labs(title = "Cumulative density", y = "cumulative density")+
  my_theme+
  theme(legend.position = "bottom")

p1+p2+
  plot_layout(guides = "collect", ncol = 2 ) &
  theme(legend.position = "bottom")

ggsave("Figures/extendibility_languages_comparison.png", width = 12/1.5, height = 9/1.5, units = "in", dpi = 300) 

## cleanup
rm(df, df_DE, df_EN, df_FR, my_theme, p1, p2)

