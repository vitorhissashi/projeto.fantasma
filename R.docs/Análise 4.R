library(tidyverse)
library(readxl)

banco_final %>% 
  group_by(imdb,engagement) %>% 
  ggplot(aes(x=imdb,y=engagement))+
  geom_point(colour = "#A11D21", size = 2,alpha = 0.5)+
  geom_smooth(method = "lm", color = "#003366", se = FALSE,size=0.5)+
  labs(x= "Nota IMDB",y= "Classificação de engajamento")+
  theme_estat()
ggsave(filename =  file.path("resultados","análise4.pdf"), width = 158, height = 93, units = "mm")
