## REFERENCE: http://stanford.edu/~ejdemyr/r-tutorials-archive/tutorial8.html

library(MatchIt)
library(plyr)
library(dplyr)
library(ggplot2)
library(broom)
library(kSamples)

dados <- read.csv(file.choose(), sep=";", na.strings = ".")

dadosT <- dados %>% select(.,c(quest,sexo,raca,id_real,EST,p1:p4, p502,c(p12:p13),c(ProfComb:se4_n)))

dadosT <- dados %>% mutate(alfab = ifelse(ProfComb <= 95, 0, 1)) %>% 
  mutate(profic = ifelse(ProfComb > 137, 1, 0)) %>% 
  mutate(Autogestao = ifelse(c_thet > median(c_thet), 1, 0)) %>%
  mutate(AberturaNovo = ifelse(o_thet > median(o_thet), 1, 0)) %>% 
  mutate(Autoconceito = ifelse(se_thet > median(se_thet), 1, 0)) %>% 
  mutate(sexoT = ifelse(sexo == 1, 1, 0)) %>% 
  mutate(racaT = ifelse(raca == 1, 1, 0)) %>%
  mutate(idadeConclusao = p4) %>%
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
  filter(p3 == 2) %>% 
  mutate(idade1serie = ifelse(p1 == 1, 0, p2)) %>% 
  filter(id_real > 18)


dadosW <- dadosT %>% mutate(c4_n = as.character(c4_n)) %>% 
  mutate(o4_n = as.character(o4_n)) %>% 
  mutate(se4_n = as.character(se4_n)) %>% 
  filter(c4_n == "limited" | c4_n == "highly") %>% 
  mutate(Autogestao = ifelse(c4_n == 'highly', 1, 0))


vars_paream <- c('id_real', 'sexoT', 'racaT', 'idadeConclusao', 'maeMedioCompleto')

#Comparacao sem pareamento para HSEs

tabelaVarsPareamAutogestao <- dadosW %>% group_by(Autogestao) %>%
  select(one_of(vars_paream)) %>% 
  summarise_all(funs(mean(.,na.rm=T)))

tabelaVarsPareamAutogestao <- as.data.frame(t(tabelaVarsPareamAutogestao))
colnames(tabelaVarsPareamAutogestao) <- c("ADesenvolver", "MuitoDesenvolvido")
tabelaVarsPareamAutogestao <- tabelaVarsPareamAutogestao %>% mutate(DiffMedia = round(MuitoDesenvolvido - ADesenvolver, 2))
tabelaVarsPareamAutogestao <- tabelaVarsPareamAutogestao[2:nrow(tabelaVarsPareamAutogestao), ]

#Criando funcao para teste de medias e testando para as HSEs

AutogestaoMatchResult <- data.frame(var = vars_paream, mediaSemPar = tabelaVarsPareamAutogestao$DiffMedia, 
                                    pValueSemPar = numeric(length(vars_paream)),
                                    pValueSemParDist = numeric(length(vars_paream)),
                                    mediaComPar = numeric(length(vars_paream)),
                                    pValueComPar = numeric(length(vars_paream)),
                                    pValueComParDist = numeric(length(vars_paream)))


listaTestsAutogestao <- lapply(vars_paream, function(v){
  teste <- ad.test(dadosW[,v] ~ dadosW[,'Autogestao'], method = 'exact', Nsim = 1e5)
  AutogestaoMatchResult[which(AutogestaoMatchResult$var == v), "pValueSemParDist"] <<- round(teste$ad[2,4], 4)
})

listaTestsAutogestao <- lapply(vars_paream, function(v){
  teste <- t.test(dadosW[,v] ~ dadosW[,'Autogestao'])
  AutogestaoMatchResult[which(AutogestaoMatchResult$var == v), "pValueSemPar"] <<- round(teste$p.value, 4)
  return(teste)
})

# Propensity Score para Autogestao

AutogestaoPSModel <- glm(Autogestao ~ id_real + idade1serie+ sexoT + racaT +
                           maeMedioCompleto,
                         family = binomial(), data = dadosW)
summary(AutogestaoPSModel)

AutogestaoPredicted <- data.frame(AutogestaoPScore = predict(AutogestaoPSModel, type = "response"), 
                                  Autogestao = AutogestaoPSModel$model$Autogestao)

# Avaliacao da regiao de suporte comum para Autogestao
rotulos <- paste("HSE - Autogestao: ", c("Muito Desenvolvido","A desenvolver"))
AutogestaoPredicted %>% mutate(Autogestao = ifelse(Autogestao == 1, rotulos[1], rotulos[2])) %>%
  ggplot(aes(x = AutogestaoPScore)) +
  geom_histogram(color = "white") +
  facet_wrap(~Autogestao) +
  xlab("Probabilidade de Autogestao muito desenvolvida") +
  theme_bw()

# Matching para Autogestao
AutogestaoSemMissing <- dadosW %>% 
  select(ProfComb, Autogestao, AberturaNovo, Autoconceito, ensinoMedioCompleto, one_of(vars_paream)) %>% 
  na.omit()

modMatchAutogestao <- matchit(Autogestao ~ id_real + idadeConclusao + sexoT + racaT + maeMedioCompleto,
                              method = "cem", discard = "both", data = AutogestaoSemMissing)

matchedAutogestao <- match.data(modMatchAutogestao)

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

# Testes de media pos-pareamento

mediaPosParAutogestao <- matchedAutogestao %>% group_by(Autogestao) %>% 
  select(one_of(vars_paream)) %>% 
  summarise_all(funs(mean))

mediaPosParAutogestao <- as.data.frame(t(mediaPosParAutogestao))
colnames(mediaPosParAutogestao) <- c("ADesenvolver", "MuitoDesenvolvido")
mediaPosParAutogestao <- mediaPosParAutogestao %>% mutate(DiffMedia = round(MuitoDesenvolvido - ADesenvolver, 2))
mediaPosParAutogestao <- mediaPosParAutogestao[2:nrow(mediaPosParAutogestao), ]

AutogestaoMatchResult$mediaComPar <- mediaPosParAutogestao$DiffMedia

testeParAutogestao <- lapply(vars_paream, function(v){
  teste <- ad.test(matchedAutogestao[,v] ~ matchedAutogestao$Autogestao, method = 'exact', Nsim = 1e5)
  AutogestaoMatchResult[which(AutogestaoMatchResult$var == v), "pValueComParDist"] <<- round(teste$ad[2,4], 4)
})

testeParAutogestao <- lapply(vars_paream,function(v){
  teste <- t.test(matchedAutogestao[,v] ~ matchedAutogestao$Autogestao)
  AutogestaoMatchResult[which(AutogestaoMatchResult$var == v), "pValueComPar"] <<- round(teste$p.value, 4)
  return(teste)
})

write.csv(AutogestaoMatchResult, file = paste0(getwd(), '/results/MatchingSummary_Autogestao_Full.csv'))

#Estimando os efeitos para HSE e salvando os resultados

didAutogestao <- lm(ProfComb ~ Autogestao + ensinoMedioCompleto + Autogestao*ensinoMedioCompleto, 
                    data = matchedAutogestao)
write.csv(tidy(didAutogestao), file = paste0(getwd(), '/results/Autogestao_Full.csv'))

############### --------------------> ABERTURA AO NOVO <------------------- ###############

dadosW <- dadosT %>% mutate(c4_n = as.character(c4_n)) %>% 
  mutate(o4_n = as.character(o4_n)) %>% 
  mutate(se4_n = as.character(se4_n)) %>% 
  filter(o4_n == "limited" | o4_n == "highly") %>% 
  mutate(AberturaNovo = ifelse(o4_n == 'highly', 1, 0))

#Comparacao sem pareamento para HSEs

tabelaVarsPareamAberturaNovo <- dadosW %>% group_by(AberturaNovo) %>%
  select(one_of(vars_paream)) %>% 
  summarise_all(funs(mean(.,na.rm=T)))

tabelaVarsPareamAberturaNovo <- as.data.frame(t(tabelaVarsPareamAberturaNovo))
colnames(tabelaVarsPareamAberturaNovo) <- c("ADesenvolver", "MuitoDesenvolvido")
tabelaVarsPareamAberturaNovo <- tabelaVarsPareamAberturaNovo %>%  mutate(DiffMedia = round(MuitoDesenvolvido - ADesenvolver, 2))
tabelaVarsPareamAberturaNovo <- tabelaVarsPareamAberturaNovo[2:nrow(tabelaVarsPareamAberturaNovo), ]

#Criando funcao para teste de medias e testando para as HSEs


AberturaNovoMatchResult <- data.frame(var = vars_paream, mediaSemPar = tabelaVarsPareamAberturaNovo$DiffMedia, 
                                      pValueSemPar = numeric(length(vars_paream)),
                                      pValueSemParDist = numeric(length(vars_paream)),
                                      mediaComPar = numeric(length(vars_paream)),
                                      pValueComPar = numeric(length(vars_paream)),
                                      pValueComParDist = numeric(length(vars_paream)))

listaTestsAberturaNovo <- lapply(vars_paream, function(v){
  teste <- ad.test(dadosW[,v] ~ dadosW[,'AberturaNovo'], method = 'exact', Nsim = 1e5)
  AberturaNovoMatchResult[which(AberturaNovoMatchResult$var == v), "pValueSemParDist"] <<- round(teste$ad[2,4], 4)
})


listaTestsAberturaNovo <- lapply(vars_paream, function(v){
  teste <- t.test(dadosW[,v] ~ dadosW[,'AberturaNovo'])
  AberturaNovoMatchResult[which(AberturaNovoMatchResult$var == v), "pValueSemPar"] <<- round(teste$p.value, 4)
  return(teste)
})

# Propensity Score para AberturaNovo
AberturaNovoPSModel <- glm(AberturaNovo ~ id_real + idadeConclusao + sexoT + racaT +
                             maeMedioCompleto,
                           family = binomial(), data = dadosW)
summary(AberturaNovoPSModel)

AberturaNovoPredicted <- data.frame(AberturaNovoPScore = predict(AberturaNovoPSModel, type = "response"), 
                                    AberturaNovo = AberturaNovoPSModel$model$AberturaNovo)

# Avaliacao da regiao de suporte comum para AberturaNovo
rotulos <- paste("HSE - AberturaNovo: ", c("Muito Desenvolvido","A desenvolver"))
AberturaNovoPredicted %>% mutate(AberturaNovo = ifelse(AberturaNovo == 1, rotulos[1],rotulos[2])) %>%
  ggplot(aes(x = AberturaNovoPScore)) +
  geom_histogram(color = "white") +
  facet_wrap(~AberturaNovo) +
  xlab("Probabilidade de AberturaNovo muito desenvolvida") +
  theme_bw()


# Matching para AberturaNovo
AberturaNovoSemMissing <- dadosW %>% 
  select(ProfComb, Autogestao, AberturaNovo, Autoconceito, ensinoMedioCompleto, one_of(vars_paream)) %>% 
  na.omit()

modMatchAberturaNovo <- matchit(AberturaNovo ~ id_real + idadeConclusao + sexoT + racaT + maeMedioCompleto,
                                method = "cem", discard = 'both', data = AberturaNovoSemMissing)

matchedAberturaNovo <- match.data(modMatchAberturaNovo)

#Inspecao visual do matching
fn_bal <- function(matchedAberturaNovo,variable) {
  matchedAberturaNovo$variable <- matchedAberturaNovo[,variable]
  matchedAberturaNovo$AberturaNovo <- as.factor(matchedAberturaNovo$AberturaNovo)
  support <- c(min(matchedAberturaNovo$variable), max(matchedAberturaNovo$variable))
  ggplot(matchedAberturaNovo, aes(x = distance, y = variable, color = AberturaNovo)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = 'loess', se = F) +
    xlab('Propensity Score') +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

grid.arrange(
  fn_bal(matchedAberturaNovo, 'id_real'),
  fn_bal(matchedAberturaNovo, 'sexoT'),
  fn_bal(matchedAberturaNovo, 'racaT'),
  fn_bal(matchedAberturaNovo, 'maeMedioCompleto'),
  nrow = 2, widths = c(1, 0.8)
)

# Testes de media pos-pareamento

mediaPosParAberturaNovo <- matchedAberturaNovo %>% group_by(AberturaNovo) %>% 
  select(one_of(vars_paream)) %>% 
  summarise_all(funs(mean))

mediaPosParAberturaNovo <- as.data.frame(t(mediaPosParAberturaNovo))
colnames(mediaPosParAberturaNovo) <- c("ADesenvolver", "MuitoDesenvolvido")
mediaPosParAberturaNovo <- mediaPosParAberturaNovo %>% mutate(DiffMedia = round(MuitoDesenvolvido - ADesenvolver, 2))
mediaPosParAberturaNovo <- mediaPosParAberturaNovo[2:nrow(mediaPosParAberturaNovo), ]


AberturaNovoMatchResult$mediaComPar <- mediaPosParAberturaNovo$DiffMedia

testeParAberturaNovo <- lapply(vars_paream, function(v){
  teste <- ad.test(matchedAberturaNovo[,v] ~ matchedAberturaNovo$AberturaNovo, method = 'exact', Nsim = 1e5)
  AberturaNovoMatchResult[which(AberturaNovoMatchResult$var == v), "pValueComParDist"] <<- round(teste$ad[2,4], 4)
})

testeParAberturaNovo <- lapply(vars_paream,function(v){
  teste <- t.test(matchedAberturaNovo[,v] ~ matchedAberturaNovo$AberturaNovo)
  AberturaNovoMatchResult[which(AberturaNovoMatchResult$var == v), "pValueComPar"] <<- round(teste$p.value, 4)
  return(teste)
})

write.csv(AberturaNovoMatchResult, file = paste0(getwd(), '/results/MatchingSummary_AberturaNovo_Full.csv'))

#Estimando os efeitos para HSE e salvando os resultados

didAberturaNovo <- lm(ProfComb ~ AberturaNovo + ensinoMedioCompleto + AberturaNovo*ensinoMedioCompleto, 
                      data = matchedAberturaNovo)
write.csv(tidy(didAberturaNovo), file = paste0(getwd(), '/results/Openess_Full.csv'))
