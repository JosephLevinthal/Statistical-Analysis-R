# Leitura de arquivos TXT
data_txt <- read.delim(file = 'data.txt', header = FALSE, sep = '\n', dec = ',')
data_txt <- as.vector(data_txt[,'V1'])

#----------------AMPLITUDE-------------------------------

# Definição da amplitude
minha_amplitude <- function(v){
  sorted_data <- sort(data_txt, decreasing = FALSE)
  return (sorted_data[length(sorted_data)] - sorted_data[1])
}

# Teste da amplitude
minha_amplitude(data_txt)

#----------------VARIÂNCIA-------------------------------

media <- mean(data_txt)
desvio <- data_txt - media
desvio_quadrado <- desvio^2

minha_variancia = sum(desvio_quadrado) / (length(data_txt) - 1)

R_variancia = var(data_txt)

minha_variancia
R_variancia

#---------------DESVIO PADRÃO--------------------------------

meu_desvio_padrao = sqrt(minha_variancia)
R_desvio_padrao = sd(data_txt)

meu_desvio_padrao
R_desvio_padrao

#---------------COEFICIENTE DE VARIAÇÃO--------------------------------

meu_coeficiente_variacao = 100 * (meu_desvio_padrao / mean(data_txt))

meu_coeficiente_variacao

#---------------QUARTIS--------------------------------

meu_quartil <- function(dados) {
  # Ordenacao dos dados
  dados <- sort(dados)
  n <- length(dados)
  m <- (n+1)/2
  if (floor(m) != m) {
    l <- m-1/2; u <- m+1/2
  } else {
    l <- m-1; u <- m+1
  }
  # Quartis
  c(Q1=median(dados[1:l]), Q3=median(dados[u:n]))
}

meu_quartil(data_txt)

# Amplitude interquartil
IQR(data_txt)