# Função Moeda
moeda <- function(p) {
  return((runif(1)<=p)*1)
}

moeda(0.5)

#Função Moedas
moedas <- function(n,p) {
  return((runif(n)<=p)*1)
}

mean(moedas(1000000,0.4))

#Função proporcao
proporcao <- function(resultados){
  return(mean(resultados))
} 

proporcao(moedas(1000000,0.4))

#Função simulacao

# Uma função chamada simulacao que tem os parâmetro k, n e p.
# Essa função deverá repetir o seguinte processo k vezes:
#   
# simular o lançamento de n moedas com probabilidade p de ser cara.
# calcular a proporção de caras (1’s) obtidos nesses n lançamentos.
# salvar a proporção calculada em um elemento do vetor. A função deve retornar um vetor com todas as proporções obtidas.

simulacao <- function(k,n,p){
  x  <-  rep(0,k)
  for(i in 1:k){
    x[i]  <-  proporcao(moedas(n,p))
  }
  return(x)
} 

dados <- data.frame(simulacao(333,10000,0.5))
names(dados) <- "simul"
dados


library(ggplot2)

#Histogramas


n <-  10
p <- 0.5
descr <- paste("Histograma para (n,p) = (",n,",",p,")")
descr

qplot(dados$simul, geom="histogram") 

qplot(dados$simul,
      geom="histogram",
      main = descr, 
      xlab = "Proporção",  
      ylab = "Frequência",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2))

ggplot(data=dados, aes(dados$simul)) + geom_histogram()

# Looping

pp = c(100, 1000, 10000)
nn = c(1,1,1)
xx1=matrix(c(pp,0.5*nn),nrow=3,ncol=2)
xx2=matrix(c(pp,0.8*nn),nrow=3,ncol=2)
xx3 = rbind(xx1,xx2)
k = 1000


for (i in 1:6) {
  n <- xx3[i,1]
  p <- xx3[i,2]
  dados <- data.frame(simulacao(k,n,p))
  names(dados) <- "simul"
  descr <- paste("Histograma para (n,p) = (",n,",",p,")")
  plott <- qplot(dados$simul,
        geom="histogram",
        main = descr,
        bins = 50,
        xlab = "Proporção",  
        ylab = "Frequência",  
        fill=I("blue"), 
        col=I("red"), 
        alpha=I(.2))
  print(plott)
}