# Curso de Gestao de Risco
# Dados: Yahoo Finance

# Configuracoes ----
remove(list = ls())
ls()
options(scipen = 999)
options(max.print = 100000)

# Pacotes ----
library(readxl)
library(quantmod)
library(tidyverse)
library(timeSeries)
library(fPortfolio)
library(tseries)
library(BETS)
library(zoo)

# -----------------------------------------------------------------------------#
# Precos ----
# -----------------------------------------------------------------------------#


# Informacoees das empresas ----
infoEmpresas <- read_excel("EmpresasVPA.xlsx")

# Setores
infoSetor <- infoEmpresas # Todas as 25 Empresas
#infoSetor <- infoEmpresas[1:5,] # Energ
#infoSetor <- infoEmpresas[6:10,] # Imob
#infoSetor <- infoEmpresas[11:15,] # Trans
#infoSetor <- infoEmpresas[16:20,] # basico
#infoSetor <- infoEmpresas[21:25,] # Consumo


# Carteira
carteira <- c(infoSetor$cod_yahoo, "^BVSP")
dataInicio <- Sys.Date()-252
dataFinal <- "2021-10-29"
nomeEmpresas <- infoSetor$codigo

# Importacao de precos
ativos <- NULL
for (i in seq(length(carteira))){
  precosTemp  <- na.omit(getSymbols(carteira[i], verbose = TRUE, src = "yahoo", 
                                    from=dataInicio,to=dataFinal, auto.assign = FALSE))[,4]
  ativos <- cbind(ativos, precosTemp)
}
ibovespa  <- na.omit(getSymbols(carteira[6], verbose = TRUE, src = "yahoo", 
                                  from=dataInicio,to=dataFinal, auto.assign = FALSE))[,4]
names(ibovespa) <- "Ibovespa"
names(ativos)<-c(infoSetor$codigo, "Ibov")
ativos <- as_tibble(ativos)
rm(precosTemp,i)

backupPrecos <- ativos #backup

# Período Analisado ----
periodoAnalisado = 60

for (i in 1:length(carteira)) {
  ativos <- tail(ativos, periodoAnalisado)
}
ibovespa <- tail(ibovespa, periodoAnalisado)
rm(i)

# -----------------------------------------------------------------------------#
# PVPA ----
# -----------------------------------------------------------------------------#
acoes = ativos
acoes$Ibov <- NULL
pvpa <- 0

for (i in 1:length(acoes)) {
  pvpaTemp <- as.tibble(acoes[i]/infoSetor$VPA[i])
  pvpa <- cbind(pvpa, pvpaTemp)
}
pvpa$pvpa <- NULL
row.names(pvpa) <- row.names(as.timeSeries(ibovespa))

#pvpa <- t(pvpa)
pvpa

rm(acoes, pvpaTemp, i)

# -----------------------------------------------------------------------------#
# Estatística Descritiva dos Preços ----
# -----------------------------------------------------------------------------#

mediaPrecos <- ativos %>% 
  summarise(across(.fns = mean)) 

desvioPrecos <- ativos %>% 
  summarise(across(.fns = sd)) 

coeficientePrecos <- desvioPrecos/mediaPrecos*100

estatisticasPrecos <- as.data.frame(rbind(mediaPrecos, desvioPrecos, coeficientePrecos))
rownames(estatisticasPrecos) <- c("MediaPreco", "DesvioPreco", "CVPreco")
estatisticasPrecos <- as.data.frame(t(estatisticasPrecos))
estatisticasPrecos

# -----------------------------------------------------------------------------#
# Estatística Descritiva dos Retornos ----
# -----------------------------------------------------------------------------#

# Função que calcula os retornos
retorno = function(x){(diff(log(x)))*100}

# Retornos de todas as colunas
retornos <- ativos %>% 
  summarise(across(.fns = retorno)) 

# Estatisticas Descritivas dos Retornos

mediaRetornos <- retornos %>% 
  summarise(across(.fns = mean))

desvioRetornos <- retornos %>% 
  summarise(across(.fns = sd)) 

desvio_100 <- desvioRetornos/100

coeficienteRetornos <- desvioRetornos/mediaRetornos

mediaMes <- mediaRetornos *24

volatilidadeRetornos <- desvioRetornos*sqrt(252)

# Juntando as informações estatísticas em um só data frame
estatisticasRetornos <- as.data.frame(rbind(mediaRetornos, desvioRetornos,desvio_100, coeficienteRetornos,mediaMes, volatilidadeRetornos))

# Mudando o nome das linhas
rownames(estatisticasRetornos) <- c("Media", "Desvio","Desvio/100", "CV", "MediaMes", "Volatilidade")

# Transpondo o data frame
estatisticasRetornos <- as.data.frame(t(estatisticasRetornos))

# Juntando duas planilhas pela coluna "codigo"
#estatisticasRetornos["codigo"] <-row.names(estatisticasRetornos)
#info_estatisticasRetornos <- left_join(infoSetor,estatisticasRetornos, by = "codigo")
#info_estatisticasRetornos
estatisticasRetornos

# -----------------------------------------------------------------------------#
# Markowitz ----
# -----------------------------------------------------------------------------#

ativosSemIbov = ativos
ativosSemIbov$Ibov <- NULL
dados <- timeSeries(ativosSemIbov)

# Configurações
Spec = portfolioSpec()
setTargetReturn(Spec) = mean(0.0045)
portfolio.optim(dados,pm=mean(dados),riskless=FALSE, shorts=FALSE,rf=0.0,reslow=NULL,reshingh=NULL,covmat=cov(dados))
rcov=cov(dados)
rcov
target.return=0.1
averet=matrix(colMeans(dados),nrow=1)
port.sol=portfolio.optim(x=averet,pm=target.return,riskless=T,covmat=rcov,shorts=T,reslow=rep(0.0,5),reshigh=rep(1.0,5));port.sol
Constraints = "LongOnly"

# Carteiras
efficientPortfolio(dados, Spec, Constraints)

markowitz <- tangencyPortfolio(dados, Spec, Constraints);markowitz

minvariancePortfolio(dados, Spec, Constraints)

Frontier = portfolioFrontier(dados)
Frontier

##Gráfico da Fronteira Eficiente####
frontierPlot(Frontier, frontier = c("both"), col = c("blue", "red"), add =
               FALSE,labels = TRUE, return = c("mean"), risk = c("Cov"), auto = TRUE, title
             = TRUE)
##Determinação da Variância Mínima
minvariancePoints(Frontier, return = c("mean"), risk = c("Cov"), auto =
                    TRUE)

# Outras possibilidades:
cmlPoints(Frontier, return = c("mean"), risk = c("Cov"), auto = TRUE)

tangencyPoints(Frontier, return = c("mean"), risk = c("Cov"), auto = TRUE)
tangencyLines(Frontier, return = c("mean"), risk = c("Cov"), auto = TRUE)
equalWeightsPoints(Frontier, return = c("mean"), risk = c("Cov"), auto =
                     TRUE)
singleAssetPoints(Frontier, return = c("mean"), risk = c("Cov"), auto =
                    TRUE)

monteCarloPoints(Frontier, mcSteps = 50, return = c("mean"), risk =
                   c("Cov"), auto = TRUE)


# -----------------------------------------------------------------------------#
# Beta ----
# -----------------------------------------------------------------------------#

selic=BETSget(11, from = as.character(dataInicio), to = as.character(dataFinal))
row.names(selic) <- selic$date
names(selic) <- c("Data","Selic")
selic$Data <- NULL
selic <- tail(selic, periodoAnalisado)


# Retornos - Selic

carteiraBeta <- cbind(retornos, "Selic"= selic[-1,])
retornosMenosSelic <- carteiraBeta %>% select(-Selic, -Ibov)
retornosMenosSelic <- retornosMenosSelic - carteiraBeta$Selic

beta <- NULL
for (i in 1: length(retornosMenosSelic)) {
  rlTemp <- lm(retornosMenosSelic[[i]]~retornos$Ibov)
  betaTemp <- rlTemp$coef[2]
  beta <- append(beta, betaTemp)
}

names(beta)=nomeEmpresas
beta

# -----------------------------------------------------------------------------#
# Concatenando todas as informacoes ----
# -----------------------------------------------------------------------------#
ultimoPvpa <- as.data.frame(t(pvpa[as.numeric(count(pvpa)),]))
names(ultimoPvpa) <- "pvpa"
pesoMarkowitz <- markowitz@portfolio@portfolio[["weights"]]

concat <- estatisticasRetornos %>% select(Media, Desvio, CV, Volatilidade) %>%  slice(1:length(infoSetor$cod_yahoo)) %>% cbind(infoSetor,.) %>% select(-cod_yahoo)
concat <- as.data.frame(beta) %>% slice(1:length(infoSetor$cod_yahoo)) %>% cbind(concat, .)
concat <- ultimoPvpa %>% slice(1:length(infoSetor$cod_yahoo)) %>% cbind(concat, .)
concat <- as.data.frame(pesoMarkowitz) %>% slice(1:length(infoSetor$cod_yahoo)) %>% cbind(concat, .)
analiseAtivos = concat

#==============================================================
#.................RESUMO FINAL DOS CÁLCULOS.....................
#==============================================================
infoEmpresas
#--------------------------------------------------------------
nomeEmpresas
#--------------------------------------------------------------
pvpa
#--------------------------------------------------------------
estatisticasPrecos
#--------------------------------------------------------------
estatisticasRetornos
#--------------------------------------------------------------
markowitz
#--------------------------------------------------------------
beta
#--------------------------------------------------------------
analiseAtivos
