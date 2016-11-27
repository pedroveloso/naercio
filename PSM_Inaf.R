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


#Comparacao sem pareamento para alfabetizados
vars_paream <- c('sexo','raca','id_real','EscolPai','EscolMae')

mediasSemParAlfab <- dadosT %>% group_by(Alfab) %>% summarise(n_particp = n(), 
                                                         mean_c = mean(c_thet), 
                                                         mean_o = mean(o_thet), 
                                                         mean_se = mean(se_thet))

tabelaVarsPareamAlfab <- dadosT %>% group_by(Alfab) %>%
  select(one_of(vars_paream)) %>% 
  summarise_all(funs(mean(.,na.rm=T)))

listaTestsAlfab <- lapply(vars_paream, function(v){
  t.test(dadosT[,v] ~ dadosT[,'Alfab'])
})

#Comparacao sem pareamento para proficientes
mediasSemParProfic <- dadosT %>% group_by(Profic) %>% summarise(n_particp = n(), 
                                                                mean_c = mean(c_thet), 
                                                                mean_o = mean(o_thet), 
                                                                mean_se = mean(se_thet))

tabelaVarsPareamProfic <- dadosT %>% group_by(Profic) %>%
  select(one_of(vars_paream)) %>% 
  summarise_all(funs(mean(.,na.rm=T)))

listaTestsProfic <- lapply(vars_paream, function(v){
  t.test(dadosT[,v] ~ dadosT[,'Profic'])
})

# Propensity Score para alfabetizados
alfabPSModel <- glm(Alfab ~ sexoT + racaT + EscolPai + EscolMae, family = binomial(), data = dadosT)
summary(alfabPSModel)

alfabPredicted <- data.frame(alfabPScore = predict(alfabPSModel, type = "response"),
                             Alfab = alfabPSModel$model$Alfab)

# Propensity Score para proficientes
proficPSModel <- glm(Profic ~ sexoT + racaT + EscolPai + EscolMae, family = binomial(), data = dadosT)
summary(proficPSModel)

proficPredicted <- data.frame(proficPScore = predict(proficPSModel, type = "response"),
                             Profic = proficPSModel$model$Profic)

# Avaliacao da regiao de suporte comum para alfabetizados
rotulos <- paste("Categoria de alfatebitação: ", c("Alfabetizados","Analfabetos funcionais"))
alfabPredicted %>% mutate(Alfab = ifelse(Alfab == 1, rotulos[1],rotulos[2])) %>%
  ggplot(aes(x = alfabPScore)) +
  geom_histogram(color = "white") +
  facet_wrap(~Alfab) +
  xlab("Probabilidade de ser alfabetizado") +
  theme_bw()

# Avaliacao da regiao de suporte comum para proficientes
rotulos <- paste("Categoria de alfatebitação: ", c("Proficientes","Não proficientes"))
proficPredicted %>% mutate(Profic = ifelse(Profic == 1, rotulos[1],rotulos[2])) %>%
  ggplot(aes(x = proficPScore)) +
  geom_histogram(color = "white") +
  facet_wrap(~Profic) +
  xlab("Probabilidade de ser alfabetizado") +
  theme_bw()



