
library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)
library(readr)


banco_final %>% 
  filter(format == "Serie") %>% 
  filter(season %in% c("1","2","3","4")) %>% 
  group_by(season,imdb,format) %>% 
  ggplot(aes(x=season , y=imdb) )+
  geom_boxplot(fill = c("#A11D21"), width = 0.5)+
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white")+
  labs(x="Temporada",y="Nota imdb",title = "") +
  theme_estat()
ggsave(filename =  file.path("resultados","análise2.pdf"), width = 158, height = 93, units = "mm")
