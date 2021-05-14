## ----setup, include=FALSE------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
library(cluster)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
n <- 150 # número de muestras
p <- 2   # dimensión

sigma <- 1 # varianza de la distribución
mean1 <- 0 # centro del primer grupo
mean2 <- 5 # centro del segundo grupo

n1 <- round(n/2) # número de muestras del primer grupo
n2 <- round(n/2) # número de muestras del segundo grupo

x1 <- matrix(rnorm(n1*p,mean=mean1,sd=sigma),n1,p)
x2 <- matrix(rnorm(n2*p,mean=mean2,sd=sigma),n2,p)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
x  <- rbind(x1,x2)
plot (x)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
fit2       <- kmeans(x, 2)
y_cluster2 <- fit2$cluster

fit4       <- kmeans(x, 4)
y_cluster4 <- fit4$cluster

fit8       <- kmeans(x, 8)
y_cluster8 <- fit8$cluster


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
y_cluster2


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
clusplot(x, fit2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
clusplot(x, fit4$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
clusplot(x, fit8$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
plot(x[y_cluster2==1,],col='blue', xlim=c(min(x[,1]), max(x[,1])), ylim=c(min(x[,2]), max(x[,2])))
points(x[y_cluster2==2,],col='red')


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------

plot(x[y_cluster4==1,],col='blue', xlim=c(min(x[,1]), max(x[,1])), ylim=c(min(x[,2]), max(x[,2])))
points(x[y_cluster4==2,],col='red')
points(x[y_cluster4==3,],col='green')
points(x[y_cluster4==4,],col='black')


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
plot(x[y_cluster8==1,],col='blue', xlim=c(min(x[,1]), max(x[,1])), ylim=c(min(x[,2]), max(x[,2])))
points(x[y_cluster8==2,],col='red')
points(x[y_cluster8==3,],col='green')
points(x[y_cluster8==4,],col='black')
points(x[y_cluster8==5,],col='yellow')
points(x[y_cluster8==6,],col='purple')
points(x[y_cluster8==7,],col='cyan')
points(x[y_cluster8==8,],col='orange')


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
d  <- daisy(x) 
sk2 <- silhouette(y_cluster2, d)
sk4 <- silhouette(y_cluster4, d)
sk8 <- silhouette(y_cluster8, d)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
mean(sk2[,3])
mean(sk4[,3])
mean(sk8[,3])


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
iris_data<-read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data", header=T, sep=",")
attach(iris_data)
colnames(iris_data) <- c("sepalLength", "sepalWidth", "petalLength", "petalWidth", "class")
summary(iris_data)



## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
x <- iris_data[,1:4]


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
d <- daisy(x) 
resultados <- rep(0, 10)
for (i in c(2,3,4,5,6,7,8,9,10))
{
  fit           <- kmeans(x, i)
  y_cluster     <- fit$cluster
  sk            <- silhouette(y_cluster, d)
  resultados[i] <- mean(sk[,3])
}



## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
plot(2:10,resultados[2:10],type="o",col="blue",pch=0,xlab="Número de clusters",ylab="Silueta")


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
resultados <- rep(0, 10)
for (i in c(2,3,4,5,6,7,8,9,10))
{
  fit           <- kmeans(x, i)
  resultados[i] <- fit$tot.withinss
}
plot(2:10,resultados[2:10],type="o",col="blue",pch=0,xlab="Número de clusters",ylab="tot.tot.withinss")


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
library(fpc)
fit_ch  <- kmeansruns(x, krange = 1:10, criterion = "ch") 
fit_asw <- kmeansruns(x, krange = 1:10, criterion = "asw") 


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
fit_ch$bestk
fit_asw$bestk

plot(1:10,fit_ch$crit,type="o",col="blue",pch=0,xlab="Número de clústers",ylab="Criterio Calinski-Harabasz")
plot(1:10,fit_asw$crit,type="o",col="blue",pch=0,xlab="Número de clústers",ylab="Criterio silueta media")



## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
iris3clusters <- kmeans(x, 3)

# sepalLength y sepalWidth
plot(x[c(1,2)], col=iris3clusters$cluster)
plot(x[c(1,2)], col=as.factor(iris_data$class))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
# petalLength y petalWidth
plot(x[c(3,4)], col=iris3clusters$cluster)
plot(x[c(3,4)], col=as.factor(iris_data$class))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
table(iris3clusters$cluster,iris_data$class)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
100*(36 + 48 + 49)/(133+(2+14))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
library(ggplot2)
library(dplyr) 
library(fpc)
library(cluster) 
library(factoextra)  
library(useful)
library(scales)
library(tidyverse)
library(gridExtra)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
# Se lee la información del dataset desde la web directamente y se nombran sus columnas
datos<-read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", header=FALSE, sep=",",  col.names=c('Id_Clase', 'Alcohol','Acido_malico', 'Ceniza', 'Alcalinidad_Ceniza', 'Magnesio', 'Fenoles_Totales', 'Flavonoides', 'Fenoles_No_Flavonoides','Proantocianinas', 'Intensidad_Color','Tono', 'OD280.OD315_Vinos_Diluidos', 'Prolina'))

# Visualizamos el contenido de los datos descargados
head (datos)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
# Revisamos la estructura
str(datos)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
# Resumen de las variables
summary(datos)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
# Estadísticas de valores vacíos
colSums(is.na(datos))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
# Quitamos la primera columna y estandarizamos con scale()
# Guardamos en variable input
input<-scale(datos[-1])
# Resumen estadístico de los datos estandarizados
summary(input)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
set.seed(123)
# Gráficos de los métodos de clustering
fviz_nbclust(input, FUN = hcut, method = "wss") + ggtitle("(A) Método Elbow/Codo")
fviz_nbclust(input, FUN = hcut, method = "silhouette", k.max = 10) + ggtitle("(B) Método Silhouette/Silueta")
fviz_nbclust(input, FUN = hcut, method = "gap_stat", k.max = 10) + ggtitle("(C) Gap statistic")


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
fit_ch  <- kmeansruns(input, krange = 1:10, criterion = "ch") 
fit_asw <- kmeansruns(input, krange = 1:10, criterion = "asw") 
fit_ch$bestk
fit_asw$bestk


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
plot(1:10,fit_ch$crit,type="o",col="red",pch=20,xlab="Número de clústers",ylab="Criterio Calinski-Harabasz", main = "Método Calinski-Harabasz")


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
plot(1:10,fit_asw$crit,type="o",col="red",pch=20,xlab="Número de clústers",ylab="Criterio silueta media", main = "Método Silueta media")


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
# Aplicamos kmeans con k=3
datos3clusters <- kmeans(input, 3)
plot(datos3clusters, data=input)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
table(datos3clusters$cluster,datos$Id_Clase)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
# Porcentaje de acierto
100*(59 + 65 + 48)/(172+(3+3))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
pam3clusters <- pam(input, 3)
plot(pam3clusters, data=input)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
table(pam3clusters$cluster,datos$Id_Clase)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
# Porcentaje de acierto
100*(59 + 55 + 48)/(162+(15+1))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
clara3clusters <- clara(input, 3)
plot(clara3clusters, data=input)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
table(clara3clusters$cluster,datos$Id_Clase)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
# Porcentaje de acierto
100*(59 + 60 + 48)/(167+(10+1))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
# install.packages("arules")
library(arules)
data("Groceries")


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
# ?Groceries
inspect(head(Groceries, 5))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
itemFrequencyPlot(Groceries,topN=20,type="absolute")


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
grocery_rules <- apriori(Groceries, parameter = list(support = 0.01, confidence = 0.5))

inspect(head(sort(grocery_rules, by = "confidence"), 3))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
inspect(head(sort(grocery_rules, by = "support"), 3))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
inspect(head(sort(grocery_rules, by = "lift"), 3))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
# Paquete arules para la creacion de modelos de reglas de asociacion
# install.packages('arules')
library(arules)

# Impostamos los datos y guardamos en variable music_data
music_data<-read.table('lastfm.csv',head=TRUE,sep=',', stringsAsFactors = TRUE, col.names=c('User', 'Artist', 'Sex', 'Country'))

# Revisamos contenido en music_data
head(music_data)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
# Revisamos la estructura de music_data
str(music_data)
# Resumen de las variables
summary(music_data)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
# Estadísticas de valores vacíos
colSums(is.na(music_data))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
# Guardamos el usuario y artista en playlist
playlist <- split(x=music_data$Artist,f=music_data$User)
# Nos quedamos con los valores únicos. Quitamos duplicados
playlist <- lapply(playlist,unique)
# Transformamos playlist en un conjunto de datos de transacciones que es lo que necesitamos para arules
# R tratará esto como una clase especial de transacciones de arules
playlist <- as(playlist,"transactions")


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
itemFrequencyPlot(playlist, topN=10,type="absolute", col = topo.colors(10), xlab='Artistas', ylab='Frecuencia')


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
# Construcción de las reglas de asociación
# Solo asociaciones con soporte > 0.01 y confianza > 0.5
music_rules <- apriori(playlist,parameter=list(support = 0.01, confidence = 0.5))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
# Resumen de las reglas creadas
summary(music_rules)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
inspect(head(sort(subset(music_rules,subset=lift>5), by = "confidence"), 5))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
inspect(head(sort(subset(music_rules,subset=lift>5), by = "support"), 5))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
inspect(head(sort(subset(music_rules,subset=lift>5), by = "lift"), 5))


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
# Construcción de las reglas de asociación
# Solo asociaciones con soporte > 0.01 y confianza > 0.6
music_rules2 <- apriori(playlist,parameter=list(support = 0.011, confidence = 0.6))
inspect(sort(music_rules2,by='confidence'))

