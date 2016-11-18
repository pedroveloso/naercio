library(dplyr)
library(MatchIt)
library(ggplot2)


dados <- read.csv(file.choose(),sep=";",na.strings = ".")

# Ajustar esse selelct para conter as variaveis de pareamento
dados <- dados %>% select(.,c(quest,c(ProfComb:se4_n)))

dadosT <- dados %>% mutate(Alfab = ifelse(ProfComb <= 95,0,1)) %>% 
  mutate(Profic = ifelse(ProfComb > 137,1,0)) %>% 
  mutate(NCS1 = ifelse(c_thet > median(c_thet),1,0)) %>%
  mutate(NCS2 = ifelse(o_thet > median(o_thet),1,0)) %>% 
  mutate(NCS3 = ifelse(se_thet > median(se_thet),1,0))