## REFERENCE: http://stanford.edu/~ejdemyr/r-tutorials-archive/tutorial8.html

library(MatchIt)
library(plyr)
library(dplyr)
library(ggplot2)
library(broom)

dados <- read.csv(file.choose(), sep=";", na.strings = ".")

dadosT <- dados %>% select(.,c(quest,sexo,raca,id_real,EST,p1:p3, p502,c(p12:p13),c(ProfComb:se4_n)))

dadosT <- dados %>% mutate(alfab = ifelse(ProfComb <= 95, 0, 1)) %>% 
  mutate(profic = ifelse(ProfComb > 137, 1, 0)) %>% 
  mutate(Autogestao = ifelse(c_thet > median(c_thet), 1, 0)) %>%
  mutate(Openness = ifelse(o_thet > median(o_thet), 1, 0)) %>% 
  mutate(Autoconceito = ifelse(se_thet > median(se_thet), 1, 0)) %>% 
  mutate(sexoT = ifelse(sexo == 1, 1, 0)) %>% 
  mutate(racaT = ifelse(raca == 1, 1, 0)) %>%
  mutate(paiMedioCompleto = ifelse(p12 <= 5, 0, ifelse(p12 != 9 | p12 != 99, 1, 0))) %>% 
  mutate(naoTevePai = ifelse(p12 == 9, 1, 0)) %>%
  mutate(naoRespPai = ifelse(p12 == 99, 1, 0)) %>% 
  mutate(maeMedioCompleto = ifelse(p13 <= 5, 0, ifelse(p13 != 9 | p13 != 99, 1, 0))) %>% 
  mutate(naoTeveMae = ifelse(p13 == 9, 1, 0)) %>%
  mutate(naoRespMae = ifelse(p13 == 99, 1, 0)) %>%
  mutate(centroSul = ifelse(EST >= 31, 1, 0)) %>% 
  mutate(ensinoFundCompleto = ifelse(p1 >= 1 & p1 <= 8, 0, 1)) %>% 
  mutate(ensinoMedioCompleto = ifelse(p1 >= 1 & p1 <= 11, 0, 1 )) %>% 
  mutate(aindaEstuda = ifelse(p3 == 1, 1, 0)) %>% 
  filter(p1 <= 12) %>% 
  filter(p502 != 2) %>% 
  mutate(idade1serie = ifelse(p1 == 1, 0, p2)) %>% 
  filter(id_real > 25)

# Pensar sobre a questão da idade de ingresos na escola...


dadosT <- dadosT %>% mutate(c4_n = as.character(c4_n)) %>% 
  mutate(o4_n = as.character(o4_n)) %>% 
  mutate(se4_n = as.character(se4_n)) %>% 
  filter(c4_n == "limited" | c4_n == "highly" | o4_n == "limited" | o4_n == "highly") %>% 
  mutate(Autogestao = ifelse(c4_n == 'highly', 1, 0)) %>% 
  mutate(Openness = ifelse(o4_n == 'highly', 1, 0))

#Comparacao sem pareamento para HSEs

vars_paream <- c('id_real', 'idade1serie','sexoT', 'racaT', 'maeMedioCompleto')


tabelaVarsPareamAutogestao <- dadosT %>% group_by(Autogestao) %>%
  select(one_of(vars_paream)) %>% 
  summarise_all(funs(mean(.,na.rm=T)))

tabelaVarsPareamOpenness <- dadosT %>% group_by(Openness) %>%
  select(one_of(vars_paream)) %>% 
  summarise_all(funs(mean(.,na.rm=T)))


#Criando funcao para teste de medias e testando para as HSEs

listaTestsAutogestao <- lapply(vars_paream, function(v){
  t.test(dadosT[,v] ~ dadosT[,'Autogestao'])
})

listaTestsOpenness <- lapply(vars_paream, function(v){
  t.test(dadosT[,v] ~ dadosT[,'Openness'])
})

# Propensity Score para Autogestao

AutogestaoPSModel <- glm(Autogestao ~ id_real + idade1serie+ sexoT + racaT +
                           maeMedioCompleto,
                         family = binomial(), data = dadosT)
summary(AutogestaoPSModel)

AutogestaoPredicted <- data.frame(AutogestaoPScore = predict(AutogestaoPSModel, type = "response"), 
                                  Autogestao = AutogestaoPSModel$model$Autogestao)

# Propensity Score para Openness
OpennessPSModel <- glm(Openness ~ id_real + idade1serie + sexoT + racaT +
                         maeMedioCompleto,
                       family = binomial(), data = dadosT)
summary(OpennessPSModel)

OpennessPredicted <- data.frame(OpennessPScore = predict(OpennessPSModel, type = "response"), 
                                Openness = OpennessPSModel$model$Openness)

# Avaliacao da regiao de suporte comum para Autogestao
rotulos <- paste("HSE - Autogestao: ", c("Acima da mediana","Abaixo da mediana"))
AutogestaoPredicted %>% mutate(Autogestao = ifelse(Autogestao == 1, rotulos[1],rotulos[2])) %>%
  ggplot(aes(x = AutogestaoPScore)) +
  geom_histogram(color = "white") +
  facet_wrap(~Autogestao) +
  xlab("Probabilidade de Autogestao acima da mediana") +
  theme_bw()

# Avaliacao da regiao de suporte comum para Openness
rotulos <- paste("HSE - Openness: ", c("Acima da media","Abaixo da mediana"))
OpennessPredicted %>% mutate(Openness = ifelse(Openness == 1, rotulos[1],rotulos[2])) %>%
  ggplot(aes(x = OpennessPScore)) +
  geom_histogram(color = "white") +
  facet_wrap(~Openness) +
  xlab("Probabilidade de Openness acima da mediana") +
  theme_bw()

# Matching para Autogestao
AutogestaoSemMissing <- dadosT %>% 
  select(ProfComb, Autogestao, Openness, Autoconceito, ensinoMedioCompleto, one_of(vars_paream)) %>% 
  na.omit()

modMatchAutogestao <- matchit(Autogestao ~ id_real + idade1serie + sexoT + racaT + maeMedioCompleto,
                              method = "nearest", discard = "both", data = AutogestaoSemMissing)

matchedAutogestao <- match.data(modMatchAutogestao)

# Matching para Openness
OpennessSemMissing <- dadosT %>% 
  select(ProfComb, Autogestao, Openness, Autoconceito, ensinoMedioCompleto, one_of(vars_paream)) %>% 
  na.omit()

modMatchOpenness <- matchit(Openness ~ id_real + idade1serie+ sexoT + racaT + maeMedioCompleto,
                            method = "nearest", discard = 'both', data = OpennessSemMissing)

matchedOpenness <- match.data(modMatchOpenness)

#Inspecao visual do matching
fn_bal <- function(matchedAutogestao,variable) {
  matchedAutogestao$variable <- matchedAutogestao[,variable]
  matchedAutogestao$Autogestao <- as.factor(matchedAutogestao$Autogestao)
  support <- c(min(matchedAutogestao$variable), max(matchedAutogestao$variable))
  ggplot(matchedAutogestao, aes(x = distance, y = variable, color = Autogestao)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = 'loess', se = F) +
    xlab('Propensity Score') +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

library(gridExtra)

grid.arrange(
  fn_bal(matchedAutogestao, 'id_real'),
  fn_bal(matchedAutogestao, 'sexoT'),
  fn_bal(matchedAutogestao, 'racaT'),
  fn_bal(matchedAutogestao, 'maeMedioCompleto'),
  nrow = 2, widths = c(1, 0.8)
)

fn_bal <- function(matchedOpenness,variable) {
  matchedOpenness$variable <- matchedOpenness[,variable]
  matchedOpenness$Openness <- as.factor(matchedOpenness$Openness)
  support <- c(min(matchedOpenness$variable), max(matchedOpenness$variable))
  ggplot(matchedOpenness, aes(x = distance, y = variable, color = Openness)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = 'loess', se = F) +
    xlab('Propensity Score') +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

grid.arrange(
  fn_bal(matchedOpenness, 'id_real'),
  fn_bal(matchedOpenness, 'sexoT'),
  fn_bal(matchedOpenness, 'racaT'),
  fn_bal(matchedOpenness, 'maeMedioCompleto'),
  nrow = 2, widths = c(1, 0.8)
)

# Testes de media pos-pareamento

mediaPosParAutogestao <- matchedAutogestao %>% group_by(Autogestao) %>% 
  select(one_of(vars_paream)) %>% 
  summarise_all(funs(mean))

mediaPosParOpenness <- matchedOpenness %>% group_by(Openness) %>% 
  select(one_of(vars_paream)) %>% 
  summarise_all(funs(mean))

testeParAutogestao <- lapply(vars_paream,function(v){
  t.test(matchedAutogestao[,v] ~ matchedAutogestao$Autogestao)
})

testeParOpenness <- lapply(vars_paream,function(v){
  t.test(matchedOpenness[,v] ~ matchedOpenness$Openness)
})

#Estimando os efeitos para HSE e salvando os resultados

didAutogestao <- lm(ProfComb ~ Autogestao + ensinoMedioCompleto + Autogestao*ensinoMedioCompleto, 
                    data = matchedAutogestao)
write.csv(tidy(didAutogestao), file = paste0(getwd(), '/Autogestao.csv'))

didOpenness <- lm(ProfComb ~ Openness + ensinoMedioCompleto + Openness*ensinoMedioCompleto, 
                  data = matchedOpenness)
write.csv(tidy(didOpenness), file = paste0(getwd(), '/Openess.csv'))
