## REFERENCE: http://stanford.edu/~ejdemyr/r-tutorials-archive/tutorial8.html

library(MatchIt)
library(dplyr)
library(ggplot2)


dados <- read.csv(file.choose(),sep=";",na.strings = ".")

# Ajustar esse selelct para conter as variaveis de pareamento
dados <- dados %>% select(.,c(quest,sexo,raca,id_real,c(p12:p13),c(ProfComb:se4_n)))

dadosT <- dados %>% mutate(alfab = ifelse(ProfComb <= 95,0,1)) %>% 
  mutate(profic = ifelse(ProfComb > 137,1,0)) %>% 
  mutate(NCS1 = ifelse(c_thet > median(c_thet),1,0)) %>%
  mutate(NCS2 = ifelse(o_thet > median(o_thet),1,0)) %>% 
  mutate(NCS3 = ifelse(se_thet > median(se_thet),1,0)) %>% 
  mutate(sexoT = ifelse(sexo == 1, 1, 0)) %>% 
  mutate(racaT = ifelse(raca == 1, 1, 0)) %>% 
  mutate(escolPai = ifelse(p12 == 99, 0, p12)) %>% 
  mutate(escolMae = ifelse(p13 == 99, 0, p13))


#Comparacao sem pareamento para alfabetizados
vars_paream <- c('sexo','raca','id_real','escolPai','escolMae')

mediasSemParalfab <- dadosT %>% group_by(alfab) %>% summarise(n_particp = n(), 
                                                         mean_c = mean(c_thet), 
                                                         mean_o = mean(o_thet), 
                                                         mean_se = mean(se_thet))

tabelaVarsPareamalfab <- dadosT %>% group_by(alfab) %>%
  select(one_of(vars_paream)) %>% 
  summarise_all(funs(mean(.,na.rm=T)))

listaTestsalfab <- lapply(vars_paream, function(v){
  t.test(dadosT[,v] ~ dadosT[,'alfab'])
})

#Comparacao sem pareamento para proficientes
mediasSemParprofic <- dadosT %>% group_by(profic) %>% summarise(n_particp = n(), 
                                                                mean_c = mean(c_thet), 
                                                                mean_o = mean(o_thet), 
                                                                mean_se = mean(se_thet))

tabelaVarsPareamprofic <- dadosT %>% group_by(profic) %>%
  select(one_of(vars_paream)) %>% 
  summarise_all(funs(mean(.,na.rm=T)))

listaTestsprofic <- lapply(vars_paream, function(v){
  t.test(dadosT[,v] ~ dadosT[,'profic'])
})

# Propensity Score para alfabetizados
alfabPSModel <- glm(alfab ~ sexoT + racaT + escolPai + escolMae + id_real, family = binomial(), data = dadosT)
summary(alfabPSModel)

alfabPredicted <- data.frame(alfabPScore = predict(alfabPSModel, type = "response"),
                             alfab = alfabPSModel$model$alfab)

# Propensity Score para proficientes
proficPSModel <- glm(profic ~ sexoT + racaT + escolPai + escolMae + id_real , family = binomial(), data = dadosT)
summary(proficPSModel)

proficPredicted <- data.frame(proficPScore = predict(proficPSModel, type = "response"),
                             profic = proficPSModel$model$profic)

# Avaliacao da regiao de suporte comum para alfabetizados
rotulos <- paste("Categoria de alfatebitação: ", c("Alfabetizados","Analfabetos funcionais"))
alfabPredicted %>% mutate(alfab = ifelse(alfab == 1, rotulos[1],rotulos[2])) %>%
  ggplot(aes(x = alfabPScore)) +
  geom_histogram(color = "white") +
  facet_wrap(~alfab) +
  xlab("Probabilidade de ser alfabetizado") +
  theme_bw()

# Avaliacao da regiao de suporte comum para proficientes
rotulos <- paste("Categoria de alfatebitação: ", c("Proficientes","Não Proficientes"))
proficPredicted %>% mutate(profic = ifelse(profic == 1, rotulos[1],rotulos[2])) %>%
  ggplot(aes(x = proficPScore)) +
  geom_histogram(color = "white") +
  facet_wrap(~profic) +
  xlab("Probabilidade de ser proficiente") +
  theme_bw()



