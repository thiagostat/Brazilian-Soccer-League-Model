#Definindo o diretório de trabalho
setwd("C:\\Users\\g567746\\Desktop\\R Futebol")

#Carregando pacotes exigidos e scripts
library(MASS)
library(mvtnorm)
source("Modelagem Futebol - Campeonato Brasileiro - Funcões.r")

#Lendo os dados
dados <- read.csv2("base_brasileirao.csv", header = T)

#Classificação de todos os brasileiros a partir de 2003
classif0 <- classificacao(subset(dados, !is.na(Goals) & Year %in% 2016:2018))
classif0

#Fazendo o recorte necessário
dados <- subset(dados, Year %in% c(2018))

#Classificação atual do brasileiro
classif <- classificacao(subset(dados, !is.na(Goals) & Year == 2018))
classif

#Realizando os ajustes na base de dados
dados <- transform(dados, Attack = as.factor(as.character(Attack)))
dados <- transform(dados, Defense = as.factor(as.character(Defense)))

#Especificação do modelo
formula_modelo <- Goals ~ Home * Attack + Home:Defense

#Ajuste do modelo
modelo <- glm(formula_modelo, data = dados, subset = !is.na(Goals), family = poisson(link = log))

#Estatísticas dos modelos
summary(modelo)

#Extraindo o vetor de coeficientes estimados e a matriz de covarîancia dos estimadores estimada 
beta_hat <- coef(modelo)
beta_cov_hat <- vcov(modelo)

#Criando a base com os resultados estimados
base_prev <- transform(subset(dados, is.na(Goals)), Goals = 0)

#Construindo a matriz de regressores
X <- model.matrix(formula_modelo, data = base_prev)

#Simulando os resultados "r" vezes
r <- 25000

base_sim <- list()

for (i in 1:r){
  
  previsao <- as.vector(exp(X %*% t(rmvnorm(1, mean = beta_hat, sigma = beta_cov_hat))))
  base_sim[[i]] <- classificacao(rbind(subset(dados, !is.na(Goals) & Year == 2018), transform(base_prev, Goals = rpois(NROW(base_prev), lambda = previsao))))
  #barplot(100 * i / r, xlim = c(0, 100), horiz = TRUE, main = "Loading...", sub = paste(round(100 * i / r, 2), "% completed.", sep = ""))
  print(i)
  
}

#Calculando a probabilidade dos times serem campeões
c_prob <- campeao_prob(base_sim)
c_prob

#Calculando a probabilidade dos times irem para a Copa Libertadores
l_prob <- libertadores_prob(base_sim, n_vagas = 6, excluir = c("Cruzeiro-MG"))
l_prob

#Calculando a probabilidade dos times serem rebaixados
r_prob <- rebaixado_prob(base_sim)
r_prob

#Colocando as probabilidades junto com a classificação atual e salvando o arquivo
classif <- transform(classif, C = 0, L = 0, R = 0)
classif$C[match(names(c_prob), classif$times)] <- c_prob
classif$L[match(names(l_prob), classif$times)] <- l_prob
classif$R[match(names(r_prob), classif$times)] <- r_prob
classif

write.csv2(classif, "output_futebol.csv")

#Avaliando o número de pontos do time campeão
pts_campeao(base_sim)
                  
#Avaliando o número de pontos para um time escapar do rebaixamento
pts_rebaixamento(base_sim)

#Avaliando a distribuição do desempenho de cada time
avalia_time(base_sim, "Flamengo-RJ")

#Análise das probabilidades dos jogos
jogo_prob(modelo, "Flamengo-RJ", "Palmeiras-SP")
