library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)
library(readr)


combined_data <- banco_final %>%
  pivot_longer(cols = starts_with("caught_"), names_to = "character", values_to = "caught") %>%
  filter(character != "caught_not") %>%  
  filter(caught %in% c(TRUE, FALSE))


combined_data$character <- gsub("caught_", "", combined_data$character)


combined_data$character <- factor(combined_data$character, levels = unique(combined_data$character))


ggplot(combined_data, aes(x = character, y = engagement, fill = factor(caught, levels = c(FALSE, TRUE), labels = c("NÃO", "SIM")))) +
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.75)) +
  stat_summary(aes(group = caught), fun = "mean", geom = "point", shape = 23, size = 3, fill = "white", position = position_dodge(width = 0.75)) +
  labs(x = "Personagem", y = "Engajamento", fill = "Capturou o monstro") +
  theme_estat() +
  scale_x_discrete(labels = c("Fred", "Daphnie", "Velma", "Salsicha", "Scooby", "Outro"))
ggsave(filename =  file.path(caminho_vitor,"análise5.pdf"), width = 158, height = 93, units = "mm")




    
    
    
    
    