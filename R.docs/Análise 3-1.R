library(tidyverse)
library(readxl)

banco_final %>%
  filter(trap_work_first==TRUE) %>% 
  group_by( setting_terrain,trap_work_first) %>% 
  summarise(count = n(),.groups = 'drop') %>% 
  arrange(desc(count)) %>%
  mutate(setting_terrain = factor(setting_terrain, levels = setting_terrain)) %>% 
  ggplot(aes(x=setting_terrain,y=count, label = count)) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text( position = position_dodge(width = .9), vjust = -0.5, size = 3) +
  labs(x = "Terreno", y = "Frequência de ativação") +
  theme_estat()
ggsave(filename =  file.path("resultados","análise3-1.pdf"), width = 158, height = 93, units = "mm")
  