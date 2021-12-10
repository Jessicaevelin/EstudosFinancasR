# Pacotes e Bibliotecas --------------------------------------------------------

library(BETS) # importar a selic
library(zoo) # manupulacao de dados
library(quantmod) # importar os precos do yahoo
library(tseries) # importar os precos do yahoo
library(tidyverse) # manupulacao de dados

remove(list=ls())

# Função

calcularbetasetores <- function(codigos) {
  # Laco de repeticao, roda esse bloco de codigo de acordo com o tamanho da carteira
  ativos <- list()
  precosTemp <- NULL
  
  i <- 1
  while(i <= length(codigos)) {
    precosTemp  <- na.omit(get.hist.quote(codigos[i], quote = "Close", start = data.inicial, end = data.final))
    ativos[[codigos[i]]] <- precosTemp
    i <- i + 1
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
  
  # Retornos Acumulados
  df.retornos.acum <- cumsum(df.retornos)
  names(df.retornos.acum) <- c("Ibovespa", codigos)
  Datas = as.Date(row.names(df.retornos.acum))
  
  return(list(Betas = Betas, Interceptos = Interceptos, R2s = R2s, RetornosAcumulados = df.retornos.acum, Retorno = df.retornos, Selic = selic))
}



