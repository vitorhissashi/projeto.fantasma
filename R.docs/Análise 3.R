library(tidyverse)
library(readxl)

banco_final %>%
  filter(trap_work_first %in% c(TRUE, FALSE)) %>% 
  filter(setting_terrain %in% c("Urban","Rural","Forest")) %>% 
  group_by( setting_terrain,trap_work_first) %>% 
  summarise(count = n(),.groups = 'drop') %>% 
  arrange(desc(count)) %>%
  mutate(setting_terrain = factor(setting_terrain, levels = unique(setting_terrain)),
         trap_work_first = recode(as.character(trap_work_first), 'TRUE'="Sim",'FALSE'="Não" )) %>% 
  ggplot(aes(x=setting_terrain,y=count, fill =  trap_work_first, label=count)) +
  geom_bar(stat = "identity", width = 0.7,position = position_dodge2(preserve = "single", padding = 0.05)) +
  geom_text( position = position_dodge(width = .7),
             vjust = -0.2, hjust = 0.5,
             size = 3) +
  labs(x = "Terreno", y = "Frequência de ativação", fill = "Armadilha funcionou de primeira" ) +
  theme_estat()
ggsave(filename =  file.path("resultados","análise3.pdf"), width = 158, height = 93, units = "mm")
  