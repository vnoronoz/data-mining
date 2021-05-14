## ----setup, include=FALSE------------------------------------------------------------------------------------------
knitr::opts_chunk$set(eval=T, echo=T)


## ----message= FALSE, warning=FALSE---------------------------------------------------------------------------------
data<-read.csv("./titanic.csv",header=T,sep=",")
attach(data)


## ------------------------------------------------------------------------------------------------------------------
dim(data)


## ------------------------------------------------------------------------------------------------------------------
str(data)


## ------------------------------------------------------------------------------------------------------------------
summary(data)


## ------------------------------------------------------------------------------------------------------------------
if(!require(ggplot2)){
    install.packages('ggplot2', repos='http://cran.us.r-project.org')
    library(ggplot2)
}
if(!require(grid)){
    install.packages('grid', repos='http://cran.us.r-project.org')
    library(grid)
}
if(!require(gridExtra)){
    install.packages('gridExtra', repos='http://cran.us.r-project.org')
    library(gridExtra)
}



## ------------------------------------------------------------------------------------------------------------------
grid.newpage()
plotbyClass<-ggplot(data,aes(CLASS,fill=SURVIVED))+geom_bar() +labs(x="Class", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by Class")
plotbyAge<-ggplot(data,aes(AGE,fill=SURVIVED))+geom_bar() +labs(x="Age", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by Age")
plotbySex<-ggplot(data,aes(SEX,fill=SURVIVED))+geom_bar() +labs(x="Sex", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("black","#008000"))+ggtitle("Survived by Sex")
grid.arrange(plotbyClass,plotbyAge,plotbySex,ncol=2)



## ------------------------------------------------------------------------------------------------------------------
tabla_SST <- table(SEX, SURVIVED)
tabla_SST
prop.table(tabla_SST, margin = 1)


## ------------------------------------------------------------------------------------------------------------------
tabla_SCT <- table(CLASS,SURVIVED)
tabla_SCT
prop.table(tabla_SCT, margin = 1)


## ------------------------------------------------------------------------------------------------------------------
tabla_SAT <- table(AGE,SURVIVED)
tabla_SAT
prop.table(tabla_SAT, margin = 1) 


## ------------------------------------------------------------------------------------------------------------------
tabla_SAT.byClass <- table(AGE,SURVIVED,CLASS)
tabla_SAT.byClass


## ------------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(tabla_SCT, col = c("black","#008000"), main = "SURVIVED vs. CLASS")
plot(tabla_SAT, col = c("black","#008000"), main = "SURVIVED vs. AGE")
plot(tabla_SST, col = c("black","#008000"), main = "SURVIVED vs. SEX")


## ------------------------------------------------------------------------------------------------------------------
head(data,10)
tail(data,10)


## ------------------------------------------------------------------------------------------------------------------
set.seed(1)
data_random <- data[sample(nrow(data)),]


## ------------------------------------------------------------------------------------------------------------------
set.seed(666)
y <- data_random[,4] 
X <- data_random[,1:3] 


## ------------------------------------------------------------------------------------------------------------------
trainX <- X[1:1467,]
trainy <- y[1:1467]
testX <- X[1468:2201,]
testy <- y[1468:2201]


## ------------------------------------------------------------------------------------------------------------------
indexes = sample(1:nrow(data), size=floor((2/3)*nrow(data)))
trainX<-X[indexes,]
trainy<-y[indexes]
testX<-X[-indexes,]
testy<-y[-indexes]


## ------------------------------------------------------------------------------------------------------------------
trainy = as.factor(trainy)
model <- C50::C5.0(trainX, trainy,rules=TRUE )
summary(model)


## ------------------------------------------------------------------------------------------------------------------
model <- C50::C5.0(trainX, trainy)
plot(model)


## ------------------------------------------------------------------------------------------------------------------
predicted_model <- predict( model, testX, type="class" )
print(sprintf("La precisión del árbol es: %.4f %%",100*sum(predicted_model == testy) / length(predicted_model)))


## ------------------------------------------------------------------------------------------------------------------
mat_conf<-table(testy,Predicted=predicted_model)
mat_conf


## ------------------------------------------------------------------------------------------------------------------

porcentaje_correct<-100 * sum(diag(mat_conf)) / sum(mat_conf)
print(sprintf("El %% de registros correctamente clasificados es: %.4f %%",porcentaje_correct))



## ------------------------------------------------------------------------------------------------------------------
if(!require(gmodels)){
    install.packages('gmodels', repos='http://cran.us.r-project.org')
    library(gmodels)
}

## ------------------------------------------------------------------------------------------------------------------
CrossTable(testy, predicted_model,prop.chisq  = FALSE, prop.c = FALSE, prop.r =FALSE,dnn = c('Reality', 'Prediction'))


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Paquetes de R
library(ggplot2)
library(tidyverse)
library(grid)
library(gridExtra)
library(skimr)
library(C50)
library(partykit)
library(gmodels)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
car_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", 
                     header=FALSE)
colnames(car_data)<-c("buying","maint","doors","persons","lug_boot","safety","class")
attach(car_data)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
summary(car_data)

## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
str(car_data)

## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
skim(car_data)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
colSums(is.na(car_data))


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
grid.newpage()
p1<-ggplot(car_data,aes(x=class,fill=buying))+geom_histogram(stat="count")+labs(title="Clase Vs Precio compra",y="Precio",x="Clase")+ scale_fill_brewer(palette="Set1")
p2<-ggplot(car_data,aes(x=class,fill=maint))+geom_histogram(stat="count")+labs(title="Clase Vs Mantenimiento",y="Mantenimiento",x="Clase")+ scale_fill_brewer(palette="Set1")
p3<-ggplot(car_data,aes(x=class,fill=doors))+geom_histogram(stat="count")+labs(title="Clase Vs N°Puertas",y="Puertas",x="Clase") + scale_fill_brewer(palette="Set1")
p4<-ggplot(car_data,aes(x=class,fill=persons))+geom_histogram(stat="count")+labs(title="Clase Vs Personas",y="Personas",x="Clase") + scale_fill_brewer(palette="Set1")
p5<-ggplot(car_data,aes(x=class,fill=lug_boot))+geom_histogram(stat="count")+labs(title="Clase Vs Tamaño del maletero",y="Maletero",x="Clase") + scale_fill_brewer(palette="Set1")
p6<-ggplot(car_data,aes(x=class,fill=safety))+geom_histogram(stat="count")+labs(title="Clase Vs Seguridad",y="Seguridad",x="Clase") + scale_fill_brewer(palette="Set1")
grid.arrange(p1,p2,p3,p4,p5,p6,ncol=2)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
tabla1 <- table(buying,class)
tabla1_p <- prop.table(tabla1, margin = 1)
tabla1
tabla1_p


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
tabla2 <- table(maint, class)
tabla2_p <- prop.table(tabla2, margin = 1)
tabla2
tabla2_p


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
tabla3 <- table(doors, class)
tabla3_p <- prop.table(tabla3, margin = 1)
tabla3
tabla3_p


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
tabla4 <- table(persons, class)
tabla4_p <- prop.table(tabla4, margin = 1)
tabla4
tabla4_p


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
tabla5 <- table(lug_boot, class)
tabla5_p <- prop.table(tabla5, margin = 1)
tabla5
tabla5_p


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
tabla6 <- table(safety,class)
tabla6_p <- prop.table(tabla6, margin = 1)
tabla6
tabla6_p


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
head(car_data,10)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
set.seed(1)
car_des <- car_data[sample(nrow(car_data)),]


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
set.seed(666)
y <- car_des[,7] 
X <- car_des[,1:6] 


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
trainX <- X[1:1152,]
trainy <- y[1:1152]
testX <- X[1153:1728,]
testy <- y[1153:1728]


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
trainy = as.factor(trainy)
model <- C50::C5.0(trainX, trainy,rules=TRUE )


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
summary(model)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
model <- C50::C5.0(trainX, trainy)
#plot(model)
myTree2 <- C50:::as.party.C5.0(model)
plot(myTree2, type = c("extended"), gp = gpar(fontsize = 6),drop_terminal = TRUE, tnex=1,ip_args=list(id = FALSE,gp = gpar()),tp_args=list(id = FALSE)) 



## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
predicted_model <- predict(model, testX, type="class" )


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
print(sprintf("La precisión del árbol es: %.4f %%",100*sum(predicted_model == testy) / length(predicted_model)))


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
mat_conf<-table(Real=testy,Predicciones=predicted_model)
mat_conf


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
model_boost10 <- C50::C5.0(trainX, trainy,trials=10)
predicted_model_2 <- predict(model_boost10, testX, type="class" )


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
print(sprintf("La precisión del árbol es: %.4f %%",100*sum(predicted_model_2 == testy) / length(predicted_model_2)))
CrossTable(testy, predicted_model_2,prop.chisq  = FALSE, prop.c = FALSE, prop.r =FALSE,dnn = c('Reality', 'Prediction'))

