# Pacotes e Bibliotecas --------------------------------------------------------

library(BETS) # importar a selic
library(zoo) # manupulacao de dados
library(quantmod) # importar os precos do yahoo
library(tidyverse) # manupulacao de dados

# Configuracoes ----------------------------------------------------------------

remove(list=ls())
ls()
options(scipen=999)
options(max.print = 100000)

# Parametros -------------------------------------------------------------------

#codigos <- c("CPLE6.SA","TAEE3.SA","CMIG4.SA","EQTL3.SA","ENBR3.SA")
#codigos <- c("JHSF3.SA","CYRE3.SA","MRVE3.SA","EZTC3.SA","MULT3.SA")
#codigos <- c("VIVT3.SA","WEGE3.SA","LCAM3.SA","TIMS3.SA","RENT3.SA")
#codigos <- c("GGBR4.SA","VBBR4.SA","CSNA3.SA","CSAN3.SA","USIM5.SA")
codigos <- c("SOMA3.SA","HYPE3.SA","ABEV3.SA","QUAL3.SA","PCAR3.SA")
data.inicial <- "2021-01-01"
data.final <- "2021-11-05"
periodos <- seq(15,150,15) # De 15 a 150 observacoes

# Preços  ----------------------------------------------------------------------

# Laco de repeticao, roda esse bloco de codigo de acordo com o tamanho da carteira
ativos <- list()
precosTemp <- NULL

i <- 1
while(i <= length(codigos)) {
  precosTemp  <- na.omit(get.hist.quote(codigos[i], quote = "Close", start = data.inicial, end = data.final))
  ativos[[codigos[i]]] <- precosTemp
  i <- i + 1
  cat("importando os preços do cod.:", codigos[i])
}

# download ibovespa
ibov  <- na.omit(get.hist.quote("^BVSP", quote = "Close", start = data.inicial, end = data.final))

# download selic
Selic.hist <- na.omit(BETSget(11, from = data.inicial, to = data.final))
selic <- zoo(Selic.hist$value)
index(selic) <- Selic.hist$date

precos = as.data.frame(ativos)
names(precos) <- codigos

# data frame com os precos e o ibovespa
df.precos <- na.omit(merge(ibov, as.zoo(precos)))
names(df.precos) <- c("ibov", codigos)

# Retornos ---------------------------------------------------------------------

# função que calcula os retornos
retorno = function(x){(diff(log(x)))*100}

# retornos de todas as colunas
df.retornos <- as.data.frame(lapply(df.precos, retorno))

# retornos e selic
df.retornos.selic <- as.data.frame(na.omit(merge(as.zoo(df.retornos), selic)))

# selic
selic <- data.frame(selic = df.retornos.selic$selic)
row.names(selic) <- row.names(df.retornos.selic)

# Funcao para calcular o Beta --------------------------------------------------

nomes.colunas <- names(df.retornos)

# função beta
calcular_beta <- function(dados, i) {
  
  # objetos temporarios
  betas <- NULL
  interceptos <- NULL
  r2.ajustado <- NULL
  
  r2 <- function(x) {
    r2.temp <- summary(x)
    return(r2.temp$adj.r.squared)
  }
  
  empresa <- nomes.colunas[i]
  ret.excedente <- dados - dados$selic
  
  ret.excedente['selic'] <- NULL
  
  reg.temp <- lm(ret.excedente[[empresa]]~ret.excedente$ibov)
  
  betas[empresa] <- reg.temp$coef[2]
  interceptos[empresa] <- reg.temp$coef[1]
  
  # R2 - Medida da qualidade da regressão
  
  r2.ajustado[empresa] <- r2(reg.temp)
  
  return(list(Betas = betas, Intercepto = interceptos, R2 = r2.ajustado))
}

# N Periodos -------------------------------------------------------------------

# variaveis
qtd.cods <- seq(2,length(nomes.colunas),1)
Betas <- NULL
Interceptos <- NULL
R2s <- NULL
df.retornos.temp <- NULL
beta <- NULL
interc  <- NULL
r2 <- NULL

# Calculando o Beta para cada ativo e periodo

for (p in periodos){
  
  # separando o data frame de retornos de acordo com o tamanho do periodo
  df.retornos.temp <- tail(df.retornos.selic, p)
  
  for (c in qtd.cods){
    # calculando beta, com o auxilio da funcao "calcular_beta"
    resultado.temp <- calcular_beta(df.retornos.temp, c)
    
    # Separando as informacoes
    betas.temp <- resultado.temp$Betas
    beta[c] <- betas.temp
    
    interc.temp <- resultado.temp$Intercepto
    interc[c] <- interc.temp
    
    r2.temp <- resultado.temp$R2
    r2[c] <- r2.temp
    
  }
  
  # juntando todos os periodos
  Betas <- rbind(Betas, beta)
  Interceptos <- rbind(Interceptos, interc)
  R2s <- rbind(R2s, r2)
  
  Betas <- as.data.frame(Betas)
  Interceptos <- as.data.frame(Interceptos)
  R2s <- as.data.frame(R2s)
}

# organizando os dados
row.names(Betas) <- periodos
row.names(Interceptos) <- periodos
row.names(R2s) <- periodos

names(Betas) <- names(df.retornos)
names(Interceptos) <- names(df.retornos)
names(R2s) <- names(df.retornos)

# removendo as informacoes do inov
Betas$ibov <- NULL
Interceptos$ibov <- NULL
R2s$ibov <- NULL

# ------------------------------------------------------------------------------
# Resultado Final --------------------------------------------------------------
# ------------------------------------------------------------------------------

# Beta -------------------------------------------------------------------------
Betas

# Intercepto -------------------------------------------------------------------
Interceptos

# R2 ---------------------------------------------------------------------------
R2s

# Gráficos ---------------------------------------------------------------------

# Graf. retornos acumulados  ---------------------------------------------------
df.retornos.acum <- cumsum(df.retornos)
names(df.retornos.acum) <- c("Ibovespa", codigos)
Datas = as.Date(row.names(df.retornos.acum))

plot(df.retornos.acum[,1], 
     x= Datas,
     main="Comparação entre os ativos",
     ylab="Retornos",
     xlab = "Tempo",
     lwd = 3,
     type="l",
     col="black",
     ylim = c(min(df.retornos.acum),max(df.retornos.acum)))

lines(df.retornos.acum[,2], x = Datas, col="orange", lwd=3)
lines(df.retornos.acum[,3], x = Datas, col="purple", lwd=3)
lines(df.retornos.acum[,4], x = Datas, col="green", lwd=3)
lines(df.retornos.acum[,5], x = Datas, col="red", lwd=3)
lines(df.retornos.acum[,6], x = Datas, col="blue", lwd=3)

legend("topleft",
       c("IBOVESPA", codigos),
       fill=c("black","orange","purple", "green", "red", "blue"))

# Gráfico Betas  ---------------------------------------------------------------
plot(Betas[,1], 
     x= periodos,
     main="Comparação entre os ativos",
     ylab="Retornos",
     xlab = "Tempo",
     lwd = 3,
     type="l",
     col="orange",
     ylim = c(min(Betas),max(Betas)))

lines(Betas[,2], x = periodos, col="purple", lwd=3)
lines(Betas[,3], x = periodos, col="green", lwd=3)
lines(Betas[,4], x = periodos, col="red", lwd=3)
lines(Betas[,5], x = periodos, col="blue", lwd=3)

legend("topleft",
       c(codigos),
       fill=c("orange","purple", "green", "red", "blue"))

# Gráfico Retornos -------------------------------------------------------------
plot(df.retornos[,1], 
     x= Datas,
     main="Retornos",
     ylab="Retornos",
     xlab = "Tempo",
     lwd = 4,
     type="l",
     col="black",
     ylim = c(min(df.retornos),max(df.retornos)))

lines(df.retornos[,2], x = Datas, col="orange", lwd=2)
lines(df.retornos[,3], x = Datas, col="purple", lwd=2)
lines(df.retornos[,4], x = Datas, col="green", lwd=2)
lines(df.retornos[,5], x = Datas, col="red", lwd=2)
lines(df.retornos[,6], x = Datas, col="blue", lwd=2)

legend("topleft",
       c(codigos),
       fill=c("black","orange","purple", "green", "red", "blue"))

# Exportação ------------------------------------------------------------- 
write.table(df.retornos.acum,"RetornosAcumuladosC.csv")
write.table(df.retornos,"RetornosC.cs")
write.table(Betas, "betasC.csv")
write.table(Interceptos, "inter.csv")
write.table(R2s,'r2.csv')  


