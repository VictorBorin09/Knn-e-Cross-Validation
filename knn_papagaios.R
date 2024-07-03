#Instalando pacotes necessarios
install.packages("stringr")
library(stringr)
install.packages("class")
library(class)

#Subindo o arquivo
papagaio <- read.csv("papagaio.csv")
str(papagaio)

#Verificamos que o peso esta como string irei retirar o "kg" do peso e transformar para numero
str_view(papagaio$peso, "kg")
papagaio$peso <- str_replace_all(papagaio$peso, pattern = "kg",replacement = "" )
papagaio$peso <- as.numeric(papagaio$peso)
str(papagaio)

#Trocando o codigo da especie pelo nome correto e transformando em factor
str_view(papagaio$especie_papagaio, "^\\.[F][0-9]{3}")
papagaio$especie_papagaio <-  str_replace_all(papagaio$especie_papagaio, pattern = "^\\.[F][0-9]{3}",replacement = "corniculata" )
str_view(papagaio$especie_papagaio, "^\\..[F][0-9]{2}")
papagaio$especie_papagaio <-  str_replace_all(papagaio$especie_papagaio, pattern = "^\\..[F][0-9]{2}",replacement = "cirrhata")
str_view(papagaio$especie_papagaio, "^\\..[F][0-9]|^\\.[F][0-9][0-9]")
papagaio$especie_papagaio <-  str_replace_all(papagaio$especie_papagaio, pattern = "^\\..[F][0-9]|^\\.[F][0-9][0-9]",replacement = "arctica")
papagaio$especie_papagaio <- as.factor(papagaio$especie_papagaio)
levels(papagaio$especie_papagaio)


str(papagaio)

#separando 80% para treino e 20% para teste 
n <- round(0.8 * nrow(papagaio))
papagaio <- papagaio[sample(nrow(papagaio)),]
treino.papagaio <- papagaio[1:n,]
teste.papagaio <- papagaio[-(1:n),]


#Dividindo o conjunto de dados em 5 partes iguais para o cross validation
indices <- seq(from = 0, to = 480, by = 96)

#Realizando o KNN e validação cruzada
medida.final <- c()
for (k in 1:30) {
  
  medidas <- c()
  
  
  for (j in 1:5) {
    
    teste.cv <- treino.papagaio[(indices[j]+1):(indices[j+1]), ]
    treino.cv <- treino.papagaio[-((indices[j]+1):(indices[j+1])), ]
    
    modelo.cv <- knn(train = scale(treino.cv[, -5]), test = scale(teste.cv[, -5]), cl = treino.cv$especie_papagaio, k = k)
    medidas[j] <- mean(modelo.cv == teste.cv$especie_papagaio)
  }
  medida.final[k] <- mean(medidas)
}
medida.final

dados <- data.frame((k = 1:30), medida.final)

plot(x= 1:30, y = medida.final)

modelo.final <- knn(train = treino.papagaio[,-5], test = teste.papagaio[,-5], cl = treino.papagaio$especie_papagaio, k = 1)

mean(modelo.final == teste.papagaio$especie_papagaio)

#O melhor k foi o k=3 com uma acuracia de 96,67%




