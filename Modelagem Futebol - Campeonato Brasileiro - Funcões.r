#Função que classifica os times com base nos resultados
classificacao <- function(dados){

  times <- as.character(unique(dados$Attack))

  jogos <- NROW(dados) / 2

  gols <- as.vector(dados$Goals)

  id <- as.vector(as.character(dados$Attack))

  Pts <- rep(0, length(times))

  J <- rep(0, length(times))
  
  V <- rep(0, length(times))

  SG <- rep(0, length(times))

  GP <- rep(0, length(times))

  for(i in 1:jogos){

    J[times == id[2 * i - 1]] <- J[times == id[2 * i - 1]] + 1 
    J[times == id[2 * i]] <- J[times == id[2 * i]] + 1
    
    if(gols[2 * i - 1] > gols[2 * i]){

    Pts[times == id[2 * i - 1]] <- 3 + Pts[times == id[2 * i - 1]]

    V[times == id[2 * i - 1]] <- 1 + V[times == id[2 * i - 1]]

    SG[times == id[2 * i - 1]] <- gols[2 * i - 1] - gols[2 * i] + SG[times == id[2 * i - 1]]
    SG[times == id[2 * i]] <- gols[2 * i] - gols[2 * i - 1] + SG[times == id[2 * i]]

    GP[times == id[2 * i - 1]] <- gols[2 * i - 1] + GP[times == id[2 * i - 1]]
    GP[times == id[2 * i]] <- gols[2 * i] + GP[times == id[2 * i]]

    }

    if(gols[2 * i - 1] == gols[2 * i]){

      Pts[times == id[2 * i]] <- 1 + Pts[times == id[2 * i]]
      Pts[times == id[2 * i - 1]] <- 1 + Pts[times == id[2 * i - 1]]

      GP[times == id[2 * i - 1]] <- gols[2 * i - 1] + GP[times == id[2 * i - 1]]
      GP[times == id[2 * i]] <- gols[2 * i] + GP[times == id[2 * i]]

    }

    if(gols[2 * i - 1] < gols[2 * i]){

      Pts[times == id[2 * i]] <- 3 + Pts[times == id[2 * i]]

      V[times == id[2 * i]] <- 1 + V[times == id[2 * i]]

      SG[times == id[2 * i - 1]] <- gols[2 * i - 1] - gols[2 * i] + SG[times == id[2 * i - 1]]
      SG[times == id[2 * i]] <- gols[2 * i] - gols[2 * i - 1] + SG[times == id[2 * i]]

      GP[times == id[2 * i - 1]] <- gols[2 * i - 1] + GP[times == id[2 * i - 1]]
      GP[times == id[2 * i]] <- gols[2 * i] + GP[times == id[2 * i]]

    }

  }
  
  
  dados <- data.frame(times, Pts, J, V, SG, GP)

  dados <- transform(dados, GC = GP - SG, Apr = round(100 * Pts / (3 * J), 1))
  
  dados <- dados[order(Pts, V, SG, GP, decreasing = TRUE),]

  row.names(dados) <- 1:NROW(dados)

  return(dados)

}

#Chances de ser campeão
campeao_prob <- function(base_sim){

  campeao <- NULL
  r <- length(base_sim)

  for (i in 1:r){

    campeao[i] <- as.character(base_sim[[i]][1,1])

  }

  rev(sort(table(campeao) / r)) * 100

}

#Chances de ir para libertadores
libertadores_prob <- function(base_sim, n_vagas = 6, excluir){

  libertadores <- matrix(NA, ncol = r, nrow = n_vagas)
  r <- length(base_sim)

  for (i in 1:r){

    dummy <- as.character(base_sim[[i]][1:(n_vagas + length(excluir)),1])

    dummy <- setdiff(dummy, excluir)

    libertadores[,i] <- dummy[1:n_vagas]

  }

  rev(sort(table(as.vector(libertadores)) / r)) * 100

}

#Chances de ser rebaixado
rebaixado_prob <- function(base_sim){

  r <- length(base_sim)
  rebaixado <- matrix(NA, ncol = r, nrow = 4)

  for (i in 1:r){

    rebaixado[,i] <- as.character(base_sim[[i]][17:20,1])

  }

  rev(sort(table(as.vector(rebaixado)) / r)) * 100

}

#Pontos para ser campeão
pts_campeao <- function(base_sim){

  pts <- NULL
  r <- length(base_sim)

  for (i in 1:r){

    pts[i] <- base_sim[[i]][1, 2]

  }

  truehist(pts, main = "Histograma de Pontos do Time Campeão", col = "grey")
  summary(pts)

}

#Pontos para escapar do rebaixamento
pts_rebaixamento <- function(base_sim){

  pts <- NULL
  r <- length(base_sim)

  for (i in 1:r){

    pts[i] <- base_sim[[i]][16, 2]

  }

  truehist(pts, main = "Histograma de Pontos do 16º Colocado", col = "grey")
  summary(pts)

}

#Avaliação da colacação de cada Time
avalia_time <- function(base_sim, time_sel){

  pos <- NULL
  pts <- NULL
  r <- length(base_sim)
  
  for (i in 1:r){

    pos[i] <- as.numeric(row.names(subset(base_sim[[i]], times == time_sel)))
    pts[i] <- subset(base_sim[[i]], times == time_sel)[, 2]

  }
  
  par(mfrow = c(2, 1))
  truehist(pos, main = paste("Histograma da Colacação do", time_sel), col = "grey")
  truehist(pts, main = paste("Histograma dos Pts do", time_sel), col = "grey")
  return(list(pos = summary(pos), pts = summary(pts)))

}

#Calculando as probabilidade de vitória de uma partida
jogo_prob <- function(modelo, Casa, Fora, goals.max = 10){
  
  dados_prev <- data.frame(Home = c("Home", "Away"), Attack = c(Casa, Fora), Defense = c(Fora, Casa))
  prev <- as.vector(predict(modelo, dados_prev, "response"))
  Home_p_Goals <- dpois(0:goals.max, lambda = prev[1])
  Away_p_Goals <- dpois(0:goals.max, lambda = prev[2])
  tabela <- matrix(0, nrow = goals.max + 1, ncol = goals.max + 1)
  
  for(i in 1:(goals.max + 1)){
    
    for(j in 1:(goals.max + 1)){
  
      tabela[i, j] <- 100 * Home_p_Goals[i] * Away_p_Goals[j]
    
    }
    
  }
  
  dimnames(tabela) <- list(0:(goals.max), 0:(goals.max))
  
  .V <- sum(tabela[lower.tri(tabela)])
  .E <- sum(diag(tabela))
  .D <- sum(tabela[upper.tri(tabela)])
  x <- c(.V, .E, .D)
  names(x) <- c(Casa, "Empate", Fora)

  return(list(results = round(tabela, 2), probs = round(x, 2)))

}

