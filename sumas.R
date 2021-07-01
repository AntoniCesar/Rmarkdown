suma <- c(1:1000)
sum(suma)


i <- NULL; aux <- NULL; r <- NULL
for (i in 0:10) {
  aux <- 2^i; r <- c(r, aux)}

dfclase <- data.frame(grupo, nota)
dfclase

load("ei1012-1516-la-s1-datos.Rdata")
length(dfclase$grupo)
1 + 2

which(dfclase$grupo == "A")
which(dfclase$nota > 7)

sort(dfclase$nota,decreasing = TRUE)
which.max(dfclase$nota)


grupoc <- dplyr::filter(dfclase, dfclase$grupo == "C")
grupoc
length(grupoc$grupo)


aprobados <- dplyr::filter(dfclase, dfclase$nota > 10.5)
aprobados
length(aprobados$nota)
which(dfclase$nota > 10.5)
length(aprobados)


menor4.9 <- dplyr::filter(dfclase, dfclase$nota <= 4.9)
(length(menor4.9$nota)/length(dfclase$nota))*100


mayor4.9 <- dplyr::filter(dfclase, dfclase$nota >= 4.9)
(length(mayor4.9$nota)/length(dfclase$nota))*100


boxplot(`nota` ~ `grupo`, dfclase, col = palette(rainbow(2)))
