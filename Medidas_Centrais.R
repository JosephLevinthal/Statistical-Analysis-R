library("psych")

# Leitura de arquivos TXT
data_txt <- read.delim(file = 'data.txt', header = FALSE, sep = '\n', dec = ',')
data_txt <- as.vector(data_txt[,'V1'])

#-----------------MEDIA ARITM�TICA------------------------------

#Defini��o das m�dias aritm�ticas
minha_media_aritmetica <- sum(data_txt) / length(data_txt)
R_media_aritmetica <- mean(data_txt)

# Compara��o das m�dias aritm�ticas
minha_media_aritmetica
R_media_aritmetica

#----------------M�DIA GEOM�TRICA-------------------------------

# Defini��o das m�dia geom�tricas
minha_media_geometrica <- exp(mean(log(data_txt)))
R_media_geometrica <- geometric.mean(data_txt)

# Compara��o das m�dias geom�tricas
minha_media_geometrica
R_media_geometrica

#----------------M�DIA HARM�NICA-------------------------------

# Defini��o das m�dias harm�nicas

n <- length(data_txt)

reciprocal <- function(x) {
  1 / x
}

minha_media_harmonica <- n / sum(reciprocal(data_txt))

R_media_harmonica <- harmonic.mean(data_txt)

# Comparacao das m�dias harmonicas
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

# Defini��o das medianas

minha_mediana <- function(x) {
  n <- length(x)
  s <- sort(x)
  ifelse(n%%2==1,s[(n+1)/2],mean(s[n/2+0:1]))
}

R_mediana <- median(data_txt)

# Comparacao das medianas
minha_mediana(data_txt)
R_mediana
