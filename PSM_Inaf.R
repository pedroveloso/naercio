## REFERENCE: http://stanford.edu/~ejdemyr/r-tutorials-archive/tutorial8.html

library(MatchIt)
library(plyr)
library(dplyr)
library(ggplot2)

dados <- read.csv(file.choose(),sep=";",na.strings = ".")

dados <- dados %>% select(.,c(quest,sexo,raca,id_real,c(p12:p13),c(ProfComb:se4_n)))

dadosT <- dados %>% mutate(alfab = ifelse(ProfComb <= 95,0,1)) %>% 
  mutate(profic = ifelse(ProfComb > 137,1,0)) %>% 
  mutate(NCS1 = ifelse(c_thet > median(c_thet),1,0)) %>%
  mutate(NCS2 = ifelse(o_thet > median(o_thet),1,0)) %>% 
  mutate(NCS3 = ifelse(se_thet > median(se_thet),1,0)) %>% 
  mutate(sexoT = ifelse(sexo == 1, 1, 0)) %>% 
  mutate(racaT = ifelse(raca == 1, 1, 0)) %>%
  mutate(paiMedioCompleto = ifelse(p12 <= 5, 0, ifelse(p12 != 9 | p12 != 99,1,0))) %>% 
  mutate(naoTevePai = ifelse(p12 == 9, 1, 0)) %>%
  mutate(naoRespPai = ifelse(p12 == 99, 1, 0)) %>% 
  mutate(maeMedioCompleto = ifelse(p13 <= 5, 0, ifelse(p13 != 9 | p13 != 99,1,0))) %>% 
  mutate(naoTeveMae = ifelse(p13 == 9, 1, 0)) %>%
  mutate(naoRespMae = ifelse(p13 == 99, 1, 0))

#Comparacao sem pareamento para alfabetizados

vars_paream <- c('id_real','sexoT', 'racaT','paiMedioCompleto','naoTevePai',
                 'naoRespPai','maeMedioCompleto','naoTeveMae','naoRespMae')

mediasSemParalfab <- dadosT %>% group_by(alfab) %>% summarise(n_particp = n(), 
                                                         mean_c = mean(c_thet), 
                                                         mean_o = mean(o_thet), 
                                                         mean_se = mean(se_thet))

tabelaVarsPareamAlfab <- dadosT %>% group_by(alfab) %>%
  select(one_of(vars_paream)) %>% 
  summarise_all(funs(mean(.,na.rm=T)))

#Comparacao sem pareamento para proficientes

mediasSemParProfic <- dadosT %>% group_by(profic) %>% summarise(n_particp = n(), 
                                                                mean_c = mean(c_thet), 
                                                                mean_o = mean(o_thet), 
                                                                mean_se = mean(se_thet))

tabelaVarsPareamProfic <- dadosT %>% group_by(profic) %>%
  select(one_of(vars_paream)) %>% 
  summarise_all(funs(mean(.,na.rm=T)))


#Criando funcao para teste de medias e testando para alfabetizados e proficientes

listaTestsAlfab <- lapply(vars_paream, function(v){
  t.test(dadosT[,v] ~ dadosT[,'alfab'])
})

listaTestsProfic <- lapply(vars_paream, function(v){
  t.test(dadosT[,v] ~ dadosT[,'profic'])
})

# Propensity Score para alfabetizados

alfabPSModel <- glm(alfab ~ id_real + sexoT + racaT +
                    paiMedioCompleto + naoTevePai + naoRespPai +
                    maeMedioCompleto + naoTeveMae + naoRespMae, family = binomial(), data = dadosT)
summary(alfabPSModel)

alfabPredicted <- data.frame(alfabPScore = predict(alfabPSModel, type = "response"),
                             alfab = alfabPSModel$model$alfab)

# Propensity Score para proficientes
proficPSModel <- glm(profic ~ id_real + sexoT + racaT +
                           paiMedioCompleto + naoTevePai + naoRespPai +
                           maeMedioCompleto + naoTeveMae + naoRespMae, family = binomial(), data = dadosT)
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

# Matching para alfabetizados
alfabSemMissing <- dadosT %>% select(ProfComb, alfab, NCS1, NCS2, NCS3, one_of(vars_paream)) %>% na.omit()

mod_match <- matchit(alfab ~ id_real + sexoT + racaT +
                       paiMedioCompleto + naoTevePai + naoRespPai +
                       maeMedioCompleto + naoTeveMae + naoRespMae, 
                     method = "full", discard = 'both', data=alfabSemMissing)

matchedAlfab <- match.data(mod_match)

# Matching para proficientes
proficSemMissing <- dadosT %>% select(ProfComb, profic, NCS1, NCS2, NCS3, one_of(vars_paream)) %>% na.omit()

mod_match <- matchit(profic ~ id_real + sexoT + racaT +
                       paiMedioCompleto + naoTevePai + naoRespPai +
                       maeMedioCompleto + naoTeveMae + naoRespMae, 
                     method = "full", discard = 'both', data=proficSemMissing)

matchedProfic <- match.data(mod_match)

#Inspecao visual do matching
fn_bal <- function(matchedAlfab,variable) {
  matchedAlfab$variable <- matchedAlfab[,variable]
  matchedAlfab$alfab <- as.factor(matchedAlfab$alfab)
  support <- c(min(matchedAlfab$variable), max(matchedAlfab$variable))
  ggplot(matchedAlfab, aes(x = distance, y = variable, color = alfab)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = 'loess', se = F) +
    xlab('Propensity Score') +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

library(gridExtra)

grid.arrange(
  fn_bal(matchedAlfab, 'id_real'),
  fn_bal(matchedAlfab, 'sexoT'),
  fn_bal(matchedAlfab, 'racaT'),
  fn_bal(matchedAlfab, 'paiMedioCompleto'),
  fn_bal(matchedAlfab, 'maeMedioCompleto'),
  nrow = 3, widths = c(1, 0.8)
)

fn_bal <- function(matchedProfic,variable) {
  matchedProfic$variable <- matchedProfic[,variable]
  matchedProfic$profic <- as.factor(matchedProfic$profic)
  support <- c(min(matchedProfic$variable), max(matchedProfic$variable))
  ggplot(matchedProfic, aes(x = distance, y = variable, color = profic)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = 'loess', se = F) +
    xlab('Propensity Score') +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

grid.arrange(
  fn_bal(matchedProfic, 'id_real'),
  fn_bal(matchedProfic, 'sexoT'),
  fn_bal(matchedProfic, 'racaT'),
  fn_bal(matchedProfic, 'paiMedioCompleto'),
  fn_bal(matchedProfic, 'maeMedioCompleto'),
  nrow = 3, widths = c(1, 0.8)
)

# Testes de media pos-pareamento

mediaPosParAlfab <- matchedAlfab %>% group_by(alfab) %>% 
  select(one_of(vars_paream)) %>% 
  summarise_all(funs(mean))

mediaPosParProfic <- matchedProfic %>% group_by(profic) %>% 
  select(one_of(vars_paream)) %>% 
  summarise_all(funs(mean))

testeParAlfab <- lapply(vars_paream,function(v){
  t.test(matchedAlfab[,v] ~ matchedAlfab$alfab)
})

testeParProfic <- lapply(vars_paream,function(v){
  t.test(matchedProfic[,v] ~ matchedProfic$profic)
})

#Estimando os efeitos para os alfabetizados

lmAlfab <- lm(ProfComb ~ NCS1 + NCS2 + NCS3 + NCS1*NCS2 + NCS1*NCS3 +
                NCS2*NCS3, data = matchedAlfab)

lmProfic <- lm(ProfComb ~ NCS1 + NCS2 + NCS3 + NCS1*NCS2 + NCS1*NCS3 +
                NCS2*NCS3, data = matchedProfic)
