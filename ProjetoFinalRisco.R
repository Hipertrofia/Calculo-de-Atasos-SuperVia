library(dplyr)

calcular_probabilidade_atraso_motivo_monte_carlo <- function(numero_reclamacoes, probabilidades_atraso_motivo) {
  reclamacoes <- sample(1:numero_reclamacoes, numero_reclamacoes, replace = TRUE)
  atrasos_motivo <- sum(reclamacoes <= round(numero_reclamacoes * probabilidades_atraso_motivo))
  proporcao_atrasos_motivo <- atrasos_motivo / numero_reclamacoes
  
  return(proporcao_atrasos_motivo)
}

numero_reclamacoes <- 1739
probabilidades_atraso_motivo <- c(0.126 , 0.061, 0.47, 0.02, 0.014)
simulacoes <- replicate(10000, calcular_probabilidade_atraso_motivo_monte_carlo(numero_reclamacoes, probabilidades_atraso_motivo))

resultados <- data.frame(
  motivo = rep(c("Pontualidade dos trens", "Parada na sinalização", "Mecânica", "Trem expresso", "Interrupção de circulação"), each = 1000),
  simulacao = simulacoes
)
resultados <- resultados %>%
  group_by(motivo) %>%
  summarise(
    media = mean(simulacao),
    q25 = quantile(simulacao, 0.25),
    q75 = quantile(simulacao, 0.75)
)
print(resultados)
