## REFERENCE: http://stanford.edu/~ejdemyr/r-tutorials-archive/tutorial8.html

library(MatchIt)
library(dplyr)
library(ggplot2)


dados <- read.csv(file.choose(),sep=";",na.strings = ".")

# Ajustar esse selelct para conter as variaveis de pareamento
dados <- dados %>% select(.,c(quest,sexo,raca,id_real,c(p12:p13),c(ProfComb:se4_n)))

dadosT <- dados %>% mutate(Alfab = ifelse(ProfComb <= 95,0,1)) %>% 
  mutate(Profic = ifelse(ProfComb > 137,1,0)) %>% 
  mutate(NCS1 = ifelse(c_thet > median(c_thet),1,0)) %>%
  mutate(NCS2 = ifelse(o_thet > median(o_thet),1,0)) %>% 
  mutate(NCS3 = ifelse(se_thet > median(se_thet),1,0)) %>% 
  mutate(sexoT = ifelse(sexo == 1, 1, 0)) %>% 
  mutate(racaT = ifelse(raca == 1, 1, 0)) %>% 
  mutate(EscolPai = ifelse(p12 == 99, 0, p12)) %>% 
  mutate(EscolMae = ifelse(p13 == 99, 0, p13))