library("xlsx")

# 1 - Leitura de arquivos TXT ORDENADOS
data_txt <- read.delim(file = 'data.txt', header = FALSE, sep = '\n', dec = ',')
data_txt <- as.vector(data_txt[,'V1'])
data_txt <- sort(data_txt)

#-----------------------------------------------

# 2 - Calculo S2
s2 <- 0.0

media_aritmetica = mean(data_txt)
for (value in data_txt) {
  s2 <- s2 + (value - mean(data_txt))**2
}
s2

#-----------------------------------------------

# 3 - Calculo B

# Calculo do A
require(xlsx)
A <- read.xlsx("tabelaA.xlsx", sheetName = "coeficientesain")

A <- A[,length(data_txt)-1]
A <- A[!is.na(A)]
A

# Calculo B
B <- 0.0
i <- 1
for ( valor in data_txt[1:round(length(data_txt) / 2)] ) {
  B <- B + ((data_txt[length(data_txt)-i+1] - data_txt[i]) * A[i])
  i <- i + 1
}
B

#-----------------------------------------------

# 4 - Calculo W

W <- 0.0
W <- (B**2) / s2
W

#-----------------------------------------------

# 5 - Calculo W

alpha = 0.05

tabelaW <- read.xlsx("valoresw.xlsx", sheetName = "Sheet1")
tabelaW <- as.numeric(tabelaW[length(data_txt)-2, paste('X', alpha, sep= "")])

# Comparacao dos resultados
shapiro.test(data_txt)
W

if (W > tabelaW) {
  print("Hipótese nula ACEITA, a amostra vem de uma distribuição normal, com o alpha utilizado")
} else {
  print("Hipótese nula REJEITADA, a amostra não vem de uma distribuição, com o alpha utilizado")
}