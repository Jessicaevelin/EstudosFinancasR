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

cple6

Media.cple6=mean(cple6)
Media.cple6

Desvio.cple6=sd(cple6)
Desvio.cple6

Coef.Var.cple6<-(Desvio.cple6/Media.cple6)*100
Coef.Var.cple6

Media.Precos=c(mean(cple6),mean(taee3),mean(cmig4),mean(eqtl3),mean(enbr3))
Desvio.Padrao=c(sd(cple6),sd(taee3),sd(cmig4),sd(eqtl3),sd(enbr3))

names<-c("cple6","taee3","cmig4","eqtl3","enbr3")
Media.Precos
Desvio.Padrao

#============MEDIDA DE RISCO====================
cvcple6=sd(cple6)/mean(cple6)*100
cvtaee3=sd(taee3)/mean(taee3)*100
cvcmig4=sd(cmig4)/mean(cmig4)*100
cveqtl3=sd(eqtl3)/mean(eqtl3)*100
cvenbr3=sd(enbr3)/mean(enbr3)*100

Risco.Precos=c(cvcple6,cvtaee3,cvcmig4,cveqtl3,cvenbr3)
Risco.Precos
plot(Risco.Precos, type="h")

#============MEDIDA DE RETORNO====================
rcple6=diff(log(cple6))
rtaee3=diff(log(taee3))
rcmig4=diff(log(cmig4))
reqtl3=diff(log(eqtl3))
renbr3=diff(log(enbr3))

mrcple=mean(rcple6)
mrcple*100
mrtaee=mean(rtaee3)
mrcmig=mean(rcmig4)
mreqtl=mean(reqtl3)
mrenbr=mean(renbr3)


#=============DESVIO DO RETORNO MEDIO MENSAL=================

Drcple=sd(rcple6)
Drcple
Drtaee=sd(rtaee3)
Drcmig=sd(rcmig4)
Dreqtl=sd(reqtl3)
Drenbr=sd(renbr3)

#=============RISCO DO RETORNO MEDIO MENSAL=================

names<-c("cple6","taee3","cmig4","eqtl3","enbr3")
Media.Retornos=c(mrcple*100,mrtaee*100,mrcmig*100,mreqtl*100,mrenbr*100)
Media.Retornos
Desvio.Retornos=c(Drcple,Drtaee,Drcmig,Dreqtl,Drenbr)
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

Risco.Retorno=c(CVcple*100,CVtaee*100,CVcmig*100,CVeqtl*100,CVenbr*100)
Risco.Retorno


plot(Risco.Retorno, type="h")

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

Volcple<-(Drcple*100)*sqrt(252)
Volcple
Voltaee<-(Drtaee*100)*sqrt(252)
Volcmig<-(Drcmig*100)*sqrt(252)
Voleqtl<-(Dreqtl*100)*sqrt(252)
Volenbr<-(Drenbr*100)*sqrt(252)


Volatilidade=c(Volcple,Voltaee,Volcmig,Voleqtl,Volenbr)
Volatilidade

#===========================================================
#RESUMO FINAL DOS CÁLCULOS - VOLATILIDADE HISTÓRICA ANUAL
#===========================================================

names=c("cple6","taee3","cmig4","eqtl3","enbr3")
names

Volatilidade=c(Volcple,Voltaee,Volcmig,Voleqtl,Volenbr)
Volatilidade





