#UNIVERSIDADE FEDERAL DA PARAÍBA
#DEPARTAMENTO DE ECONOMIA
#CURSO DE GESTAO DE RISCO
#PROFESSOR SINÉZIO FERNANDES MAIA

#Programa: IMPORTAÇÃO DE DADOS DO YAHOO! FINANCE===================================================


remove(list=ls())
ls()
options(scipen=999)
options(max.print = 100000)
setwd("~/Documents/UFPB2021/TraineeGR/GestaoRisco")

#install.packages("tseries")
library(tseries)

cple6= na.omit(get.hist.quote("cple6.sa", quote = "Close", start = "2021-06-11", end = "2021-09-04"));length(cple6)
taee3= na.omit(get.hist.quote("taee3.sa", quote = "Close", start = "2021-06-11", end = "2021-09-04"));length(taee3)
cmig4= na.omit(get.hist.quote("cmig4.sa", quote = "Close", start = "2021-06-11", end = "2021-09-04"));length(cmig4)
eqtl3= na.omit(get.hist.quote("eqtl3.sa", quote = "Close", start = "2021-06-11", end = "2021-09-04"));length(eqtl3)
enbr3= na.omit(get.hist.quote("enbr3.sa", quote = "Close", start = "2021-06-11", end = "2021-09-04"));length(enbr3)
Ibov= na.omit(get.hist.quote("^BVSP", quote = "Close", start = "2021-06-11", end = "2021-09-04"));length(cple6)


par(mfrow=c(2,3))
plot(cple6)
plot(taee3)
plot(cmig4)
plot(eqtl3)
plot(enbr3)
plot(Ibov)

par(mfrow=c(2,3))
plot(diff(log(cple6)))
plot(diff(log(taee3)))
plot(diff(log(cmig4)))
plot(diff(log(eqtl3)))
plot(diff(log(enbr3)))
plot(diff(log(Ibov)))

par(mfrow=c(1,2))
plot(cple6)
plot(diff(log(cple6)))
plot(taee3)
plot(diff(log(taee3)))
plot(cmig4)
plot(diff(log(cmig4)))
plot(eqtl3)
plot(diff(log(eqtl3)))
plot(enbr3)
plot(diff(log(enbr3)))
plot(Ibov)
plot(diff(log(Ibov)))


par(mfrow=c(1,2))
plot(cple6)
hist(cple6, nclass=30, col="blue")
plot(taee3)
hist(cple6, nclass=30, col="blue")
plot(cmig4)
hist(cple6, nclass=30, col="blue")
plot(eqtl3)
hist(cple6, nclass=30, col="blue")
plot(enbr3)
hist(cple6, nclass=30, col="blue")
plot(Ibov)
hist(cple6, nclass=30, col="blue")


par(mfrow=c(1,2))
plot(diff(log(cple6)))
hist(diff(log(cple6)), nclass=30, col="blue")
plot(diff(log(taee3)))
hist(diff(log(taee3)), nclass=30, col="blue")
plot(diff(log(cmig4)))
hist(diff(log(cmig4)), nclass=30, col="blue")
plot(diff(log(eqtl3)))
hist(diff(log(eqtl3)), nclass=30, col="blue")
plot(diff(log(enbr3)))
hist(diff(log(enbr3)), nclass=30, col="blue")
plot(diff(log(Ibov)))
hist(diff(log(Ibov)), nclass=30, col="blue")

#=========================================================
## MEDIDAS DE ESTATÍSTICA DESCRITIVA
#=========================================================
cple6
Media.cple6=mean(cple6)
Media.cple6

Desvio.cple6=sd(cple6)
Desvio.cple6

Coef.Var.cple6<-(Desvio.cple6/Media.cple6)*100
Coef.Var.cple6

Media.Precos=c(mean(cple6),mean(taee3),mean(cmig4),mean(eqtl3),mean(enbr3), mean(Ibov))
Desvio.Padrao=c(sd(cple6),sd(taee3),sd(cmig4),sd(eqtl3),sd(enbr3), sd(Ibov))

names<-c("cple6","taee3","cmig4","eqtl3","enbr3", "Ibov")
Media.Precos
Desvio.Padrao

#============MEDIDA DE RISCO====================
cvcple6=sd(cple6)/mean(cple6)*100
cvtaee3=sd(taee3)/mean(taee3)*100
cvcmig4=sd(cmig4)/mean(cmig4)*100
cveqtl3=sd(eqtl3)/mean(eqtl3)*100
cvenbr3=sd(enbr3)/mean(enbr3)*100
cvIbov=sd(Ibov)/mean(Ibov)*100

Risco.Precos=c(cvcple6,cvtaee3,cvcmig4,cveqtl3,cvenbr3,cvIbov)
Risco.Precos
par(mfrow=c(1,1))
names<-c("cple6","taee3","cmig4","eqtl3","enbr3","Ibov")
barplot(Risco.Precos, main="Risco do Preco", ylim=c(0,7),names.arg=names, ylab="Percentual %", cex.names=1.2)
grid(nx=T, ny=NULL, col="grey")

#============MEDIDA DE RETORNO====================
rcple6=diff(log(cple6))
rtaee3=diff(log(taee3))
rcmig4=diff(log(cmig4))
reqtl3=diff(log(eqtl3))
renbr3=diff(log(enbr3))
ribov=diff(log(Ibov));ribov

mrcple=mean(rcple6)
mrcple*100
mrtaee=mean(rtaee3)
mrcmig=mean(rcmig4)
mreqtl=mean(reqtl3)
mrenbr=mean(renbr3)
mribov=mean(ribov)

#=============DESVIO DO RETORNO MEDIO MENSAL=================
Drcple=sd(rcple6)
Drcple
Drtaee=sd(rtaee3)
Drcmig=sd(rcmig4)
Dreqtl=sd(reqtl3)
Drenbr=sd(renbr3)
Dribov=sd(ribov)
#=============RISCO DO RETORNO MEDIO MENSAL=================

names<-c("cple6","taee3","cmig4","eqtl3","enbr3","Ibov")
Media.Retornos=c(mrcple*100,mrtaee*100,mrcmig*100,mreqtl*100,mrenbr*100,mribov*100)
Media.Retornos
Desvio.Retornos=c(Drcple,Drtaee,Drcmig,Dreqtl,Drenbr,Dribov)
Desvio.Retornos

CVcple<-Drcple/(mrcple*100)
CVcple*100
CVtaee<-Drtaee/(mrtaee*100)
CVtaee*100
CVcmig<-Drcmig/(mrcmig*100)
CVcmig*100
CVeqtl<-Dreqtl/(mreqtl*100)
CVeqtl*100
CVenbr<-Drenbr/(mrenbr*100)
CVenbr*100
CVIbov<-Dribov/(mribov*100)
CVIbov*100

Risco.Retorno=c(CVcple*100,CVtaee*100,CVcmig*100,CVeqtl*100,CVenbr*100,CVIbov*100)
Risco.Retorno
names<-c("cple6","taee3","cmig4","eqtl3","enbr3","Ibov")
barplot(Risco.Retorno, main="Risco do Retorno", ylim=c(-60,80),names.arg=names, ylab="Percentual %", cex.names=1.2)
grid(nx=T, ny=NULL, col="grey")

#==============================================================
#.................RESUMO FINAL DOS CÁLCULOS.....................
#==============================================================
names
#--------------------------------------------------------------
Media.Precos
#--------------------------------------------------------------
Desvio.Padrao
#--------------------------------------------------------------
Risco.Precos
#--------------------------------------------------------------
Media.Retornos
#--------------------------------------------------------------
Desvio.Retornos
#--------------------------------------------------------------
Risco.Retorno
#--------------------------------------------------------------

#=============MEDIDA DE VOLATILIDADE=================
Drcple=sd(rcple6)
Drcple
Drtaee=sd(rtaee3)
Drcmig=sd(rcmig4)
Dreqtl=sd(reqtl3)
Drenbr=sd(renbr3)
Dribov=sd(ribov)
Volcple<-(Drcple*100)*sqrt(252)
Volcple
Voltaee<-(Drtaee*100)*sqrt(252)
Volcmig<-(Drcmig*100)*sqrt(252)
Voleqtl<-(Dreqtl*100)*sqrt(252)
Volenbr<-(Drenbr*100)*sqrt(252)
Volibov<-(Dribov*100)*sqrt(252)

#===========================================================
#RESUMO FINAL DOS CÁLCULOS - VOLATILIDADE HISTÓRICA ANUAL
#===========================================================

Volatilidade=c(Volcple,Voltaee,Volcmig,Voleqtl,Volenbr,Volibov)
Volatilidade
names<-c("cple6","taee3","cmig4","eqtl3","enbr3","Ibov")
barplot(Volatilidade, main="Volatilidade", ylim=c(0,40),names.arg=names, ylab="Percentual %", cex.names=1.1)
grid(nx=T, ny=NULL, col="grey")

#===========================================================
## MODERNA TEORIA DA CARTEIRA ##
#  PRINCÍPIOS DA DIVERSIFICAÇÃO ##
#===========================================================


Energia=cbind(cple6,taee3,cmig4,eqtl3,enbr3);Energia
names(Energia)<-c("cple6","taee3","cmig4","eqtl3","enbr3");Energia
class(Energia)
Energia=cbind.data.frame(cple6,taee3,cmig4,eqtl3,enbr3);Energia
names(Energia)<-c("cple6","taee3","cmig4","eqtl3","enbr3");Energia
attach(Energia)
class(Energia)


#Cáculo do Coeficiente de Correlação 
# Pearson: Variáveis Numéricas e Paramétricas)
library(fBasics)
Corr01=correlationTest(cple6,taee3, method=c("pearson"));Corr01
plot(cple6,taee3)

# Spearman: Variáveis Numéricas e Não-Paramétricas / Sem Distr.Normal)
Corr02=correlationTest(cple6,taee3, method = c("spearman"));Corr02

# Kendall: Variáveis Numéricas e Não-Paramétricas / Sem Distr.Normal)
Corr03=correlationTest(cple6,taee3, method = c("kendall"));Corr03

#Matriz de correlação (5 Ativos)
cor(Energia)


#=================================================================
## PROCEDIMENTO DE OTIMIZAÇÃO DA CARTEIRA POR MARKOWITZ ###
#=================================================================


Energia<-data.frame(na.omit(merge(cple6,taee3,cmig4,eqtl3,enbr3)))
Energia
names(Energia)<-c("cple6","taee3","cmig4","eqtl3","enbr3");Energia
length(cple6)
write.table(Energia,"~/Documents/UFPB2021/TraineeGR/GestaoRisco/Energia.txt")


dados<-read.table("Energia.txt", head=T)
dados


dados <- timeSeries(dados)
dados
Spec = portfolioSpec()
setTargetReturn(Spec) = mean(0.0045)
Spec
require(fPortfolio)

portfolio.optim(dados,pm=mean(dados),riskless=FALSE, shorts=FALSE,rf=0.0,reslow=NULL,reshingh=NULL,covmat=cov(dados))
rcov=cov(dados)
rcov
target.return=0.1
target.return
averet=matrix(colMeans(dados),nrow=1)
averet

port.sol=portfolio.optim(x=averet,pm=target.return,riskless=T,covmat=rcov,shorts=T,reslow=rep(0.0,5),reshigh=rep(1.0,5));port.sol

Constraints = "LongOnly"
efficientPortfolio(dados, Spec, Constraints)
tangencyPortfolio(dados, Spec, Constraints)

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



