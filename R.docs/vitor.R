library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)


# Convert date to year and extract the decade
banco_final$date_aired <- as.Date(banco_final$date_aired)
banco_final$decade <- floor(as.numeric(format(banco_final$date_aired, "%Y"))/10)*10

# Count the number of releases by decade and format
library(dplyr)
release_frequency <- banco_final %>%
  group_by(decade, format) %>%
  summarise(count = n(),.groups = 'drop')

freq_relat <- round(release_frequency$count/sum(release_frequency$count) * 100,1)

porcentagem<- str_c(freq_relat, "%") %>% str_replace("
\\.", ",")

legendas <- str_squish(str_c(release_frequency$count," ","(",porcentagem,")")
)

# Plot the data
ggplot(release_frequency, aes(x = as.factor(decade), y = count, fill = format, label = legendas)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_col(position = position_dodge2(preserve = "single", padding =
                                        0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(title = "Número de lançamentos a cada década por formato",
       x = "Década",
       y = "Número de lançamentos",
       fill = "Formato",) +
  estat_theme()
ggsave("colunas-bi-freq.pdf", width = 158, height = 93, units = "mm")


