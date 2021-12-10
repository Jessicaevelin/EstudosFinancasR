
calcular <- function(codigos) {
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
  
  precos = as.data.frame(ativos)
  names(precos) <- codigos
  
  # data frame com os precos e o ibovespa
  df.precos <- as.data.frame(na.omit(merge(ibov, as.zoo(precos))))
  names(df.precos) <- c("ibov", codigos)
  
  # Retornos -------------------------------------------------------------------
  
  # função que calcula os retornos
  retorno = function(x){(diff(log(x)))*100}
  
  # retornos de todas as colunas
  df.retornos <- as.data.frame(lapply(df.precos, retorno))
  
  # Estatisticas Descritivas dos Retornos  --------------------------------------------------------------
  
  # média dos retornos
  mediaRetornos <- df.retornos %>% 
    summarise(across(.fns = mean))
  
  desvioRetornos <- df.retornos %>% 
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
  

  return(list(
    medias = mediaRetornos,
    desvios = desvioRetornos,
    coeficienteVariacao = coeficienteRetornos,
    mediaMensal = mediaMes,
    volatilidade = volatilidadeRetornos,
    precos = df.precos,
    retornos = df.retornos,
    dataframeRetornos = estatisticasRetornos
  ))
}