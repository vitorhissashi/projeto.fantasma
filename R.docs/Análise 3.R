library(tidyverse)
library(readxl)

banco_final %>%
  filter(trap_work_first %in% c(TRUE, FALSE)) %>% 
  group_by( setting_terrain,trap_work_first) %>% 
  summarise(count = n(),.groups = 'drop') %>% 
  arrange(desc(count)) %>%
  mutate(setting_terrain = factor(setting_terrain, levels = unique(setting_terrain))) %>% 
  ggplot(aes(x=setting_terrain,y=count, fill =  trap_work_first, label=count)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text( position = position_stack(vjust = 0.6), size = 3, colour="black") +
  labs(x = "Terreno", y = "Frequência de ativação", fill = "Armadilha funcionou de primeira" ) +
  theme_estat()
ggsave(filename =  file.path("resultados","análise3-1.pdf"), width = 158, height = 93, units = "mm")
  