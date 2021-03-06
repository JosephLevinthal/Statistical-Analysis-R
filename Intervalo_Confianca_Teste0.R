# Leitura de arquivos TXT
data_txt <- read.delim(file = 'data.txt', header = FALSE, sep = '\n', dec = ',')
data_txt <- as.vector(data_txt[,'V1'])

#-------------INTERVALO DE CONFIAN�A----------------------------------
ic <- function(x, conf){
  n <- length(x)
  
  media <- mean(x)
  
  variancia <- var(x)
  
  quantis <- qt(c((1-conf)/2, 1 - (1-conf)/2), df = n-1)
  ic <- media + quantis * sqrt(variancia/n)
  return(ic)
}

meu_ic = ic(data_txt, 0.95)
meu_ic

#----------------TESTE M�DIA ZERO-------------------------------
if (meu_ic[1] < 0){
  print("Intervalo cont�m o 0, N�O � significativamente diferente")
} else if (meu_ic[1] > 0){
  print("Intervalo n�o cont�m o 0, � significativamente diferente")
}