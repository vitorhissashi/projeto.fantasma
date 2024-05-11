library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)
library(readr)

caminho_vitor<-"resultados"
# converter data em ano e extrair a década
banco_final$date_aired <- as.Date(banco_final$date_aired)
banco_final$decade <- floor(as.numeric(format(banco_final$date_aired, "%Y"))/10)*10

# contar o numero de lancamentos a cada década por formato
release_frequency <- banco_final %>%
  group_by(decade, format) %>%
  summarise(count = n(),.groups = 'drop') %>% 
  complete(decade,format, fill = list(count=0))

# plotar o grafico
ggplot(release_frequency, aes(x = as.factor(decade), y = count, colour = format, group = format)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    x = "Década",
    y = "Número de lançamentos",
    colour = "Formato:") +
  scale_x_discrete(labels = c("Anos 60", "Anos 70", "Anos 80", "Anos 90", "Anos 00", "Anos 10","Anos 20")) +
  theme_estat()
ggsave(filename =  file.path(caminho_vitor,"análise1.pdf"), width = 158, height = 93, units = "mm")


