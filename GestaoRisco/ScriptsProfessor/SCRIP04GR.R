#UNIVERSIDADE FEDERAL DA PARAÍBA
#DEPARTAMENTO DE ECONOMIA
#CURSO DE GESTAO DE RISCO

#Programa: IMPORTAÇÃO DE DADOS DO YAHOO! FINANCE===================================================


#install.packages("tseries")
library(tseries)

jhsf3= na.omit(get.hist.quote("jhsf3.sa", quote = "Close", start = "2021-02-11", end = "2021-09-04"));length(jhsf3)
taee3= na.omit(get.hist.quote("taee3.sa", quote = "Close", start = "2021-02-11", end = "2021-09-04"));length(taee3)
cmig4= na.omit(get.hist.quote("cmig4.sa", quote = "Close", start = "2021-02-11", end = "2021-09-04"));length(cmig4)
eqtl3= na.omit(get.hist.quote("eqtl3.sa", quote = "Close", start = "2021-02-11", end = "2021-09-04"));length(eqtl3)
enbr3= na.omit(get.hist.quote("enbr3.sa", quote = "Close", start = "2021-02-11", end = "2021-09-04"));length(enbr3)
Ibov= na.omit(get.hist.quote("^BVSP", quote = "Close", start = "2021-02-11", end = "2021-10-29"));length(jhsf3)
Ibov
library(BETS)
selic=BETSget(11, from = "2021-02-11", to = paste(Sys.Date()))
#selic2=BETSget(11, from = "2021-02-11", to = "2021-09-21");selic2
length(selic$value)
library(zoo)
Selic=zoo(selic$value)
index(Selic)=selic$date
options(max.print=99999)
Selic

dados2=na.omit(merge(Ibov,Selic))
dados2=data.frame(index(dados2), dados2)
names(dados2)=c("Data", "Ibovespa", "SELIC")
attach(dados2); dados2

library(zoo)
Ativos=merge(jhsf3,taee3,cmig4,eqtl3,enbr3,Ibov,Selic)
length(na.omit(Ativos[,1]))
Data1=index(na.omit(Ativos))
length(Data1)

Carteira10BB=data.frame(na.omit(Ativos),na.omit(Data1))
names(Carteira10BB)=c("jhsf3","taee3","cmig4","eqtl3","enbr3","Ibovespa","Selic", "Data1")
attach(Carteira10BB)
options(max.print=99999)
Carteira10BB
n=length(Carteira10BB[,1]); n
Carteira10BB[n,]
write.table(Carteira10BB, file="Carteira10BB.txt")


Carteira10<-read.table("Carteira10BB.txt", head=T)
names(Carteira10)<-c("jhsf3","taee3","cmig4","eqtl3","enbr3","Ibovespa","Selic", "Data")
attach(Carteira10)
Carteira10; length((Carteira10$Selic))

Carteira10$Ibovespa;length(Carteira10$Ibovespa)
Carteira10$jhsf3;length(Carteira10$jhsf3)
Carteira10$Selic;length(Carteira10$Selic)


rIbov=diff(log(Ibovespa))
rIbov; length(rIbov)
RIbov=rIbov*100
RIbov
RSelic=Carteira10$Selic[-1];length(Carteira10$Selic[-1])
RSelic

rjhsf3=diff(log(Carteira10$jhsf3[-1]));length(Carteira10$jhsf3[-1])
rtaee3=diff(log(Carteira10$taee3[-1]));length(Carteira10$taee3[-1])
rcmig4=diff(log(Carteira10$cmig4[-1]));length(Carteira10$cmig4[-1])
reqtl3=diff(log(Carteira10$eqtl3[-1]));length(Carteira10$eqtl3[-1])
renbr3=diff(log(Carteira10$enbr3[-1]));length(Carteira10$enbr3[-1])


#RIbov=(rIbov-Selic[-1])
RIbov=(rIbov-RSelic);length(RIbov)
RIbov
Rcple=rjhsf3-RSelic[-1]
Rcple;length(Rcple)
Rtaee=rtaee3-RSelic[-1]
Rcmig=rcmig4-RSelic[-1]
Reqtl=reqtl3-RSelic[-1]
Renbr=renbr3-RSelic[-1]

#Cálculo do Beta jhsf3
#------------------------------------------------------------------------------------------------------

Betajhsf=lm(Rjhsf~RIbov[-1])
Betacple
Betataee=lm(Rtaee~RIbov[-1]);Betataee
Betacmig=lm(Rcmig~RIbov[-1]);Betacmig
Betaeqtl=lm(Reqtl~RIbov[-1]);Betaeqtl
Betaenbr=lm(Renbr~RIbov[-1]);Betaenbr



#===========================================================
#RESUMO FINAL DOS CÁLCULOS
#===========================================================
names=c("jhsf3","taee3","cmig4","eqtl3","enbr3")
BETA.Mercado=c(Betacple$coef[2],Betataee$coef[2],Betacmig$coef[2],Betaeqtl$coef[2],Betaenbr$coef[2])
names(BETA.Mercado)=names
BETA.Mercado


