library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)
library(readr)


banco_final$date_aired <- as.Date(banco_final$date_aired)
banco_final$decade <- floor(as.numeric(format(banco_final$date_aired, "%Y"))/10)*10

frequency_terrain <- banco_final %>%
  filter(setting_terrain %in% c("Urban", "Rural","Forest"))%>%
  filter(trap_work_first %in% c(TRUE)) %>% 
  group_by(decade,trap_work_first,setting_terrain) %>% 
  summarise(count = n(), .groups = 'drop') %>%
  complete(decade, setting_terrain, fill = list(count = 0))

ggplot(frequency_terrain, aes(x = as.factor(decade), y = count, colour = setting_terrain, label = count, group = setting_terrain)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    x = "Década",
    y = "Número de ativações",
    colour = "Terreno:") +
  scale_x_discrete(labels = c("Anos 60", "Anos 70", "Anos 80", "Anos 90", "Anos 00", "Anos 10","Anos 20")) +
  theme_estat()
ggsave(filename =  file.path(caminho_vitor,"análise3-2.pdf"), width = 158, height = 93, units = "mm")
