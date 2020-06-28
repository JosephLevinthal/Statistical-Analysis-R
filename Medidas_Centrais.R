library("psych")

# Leitura de arquivos TXT
data_txt <- read.delim(file = 'data.txt', header = FALSE, sep = '\n', dec = ',')
data_txt <- as.vector(data_txt[,'V1'])

#-----------------MEDIA ARITMÉTICA------------------------------

#Definição das médias aritméticas
minha_media_aritmetica <- sum(data_txt) / length(data_txt)
R_media_aritmetica <- mean(data_txt)

# Comparação das médias aritméticas
minha_media_aritmetica
R_media_aritmetica

#----------------MÉDIA GEOMÉTRICA-------------------------------

# Definição das média geométricas
minha_media_geometrica <- exp(mean(log(data_txt)))
R_media_geometrica <- geometric.mean(data_txt)

# Comparação das médias geométricas
minha_media_geometrica
R_media_geometrica

#----------------MÉDIA HARMÔNICA-------------------------------

# Definição das médias harmônicas

n <- length(data_txt)

reciprocal <- function(x) {
  1 / x
}

minha_media_harmonica <- n / sum(reciprocal(data_txt))

R_media_harmonica <- harmonic.mean(data_txt)

# Comparacao das médias harmonicas
minha_media_harmonica
R_media_harmonica

#----------------MODA-------------------------------

minha_moda <- function(x) {
  moda_numero <- unique(x)
  tab <- tabulate(match(x, moda_numero))
  moda_numero[tab == max(tab)]
}

minha_moda(data_txt)

#----------------MEDIANA-------------------------------

# Definição das medianas

minha_mediana <- function(x) {
  n <- length(x)
  s <- sort(x)
  ifelse(n%%2==1,s[(n+1)/2],mean(s[n/2+0:1]))
}

R_mediana <- median(data_txt)

# Comparacao das medianas
minha_mediana(data_txt)
R_mediana
