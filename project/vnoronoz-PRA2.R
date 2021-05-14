## ----setup, include=FALSE------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Load workspace back to RStudio
load("C:/Users/ProBook/all_data.RData")


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
head(df,10)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
library(tidyverse)
library(caret)
library(rpart)
# Hacemos copia de los datos
dataset_tree <- df


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
N=dim(dataset_tree)[1]
all=seq(1,N)

# seleccionar 2/3 al azar
train=sort(sample(N,N*2/3.0))
test=setdiff(all,train)

xtrain_1=dataset_tree[train,]
xtest_1=dataset_tree[test,]


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
prop.table(table(xtrain_1$Absentismo_horas_rango))
prop.table(table(xtest_1$Absentismo_horas_rango))


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
model1 <- rpart(Absentismo_horas_rango ~., data = xtrain_1[,c(2:19,21)], method = "class")


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
library(rpart.plot)
par(xpd = NA) 
rpart.plot(model1)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
print(model1, digits = 2)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
summary(model1)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
predicted_data_1 <- predict(model1, xtest_1, type = "class")


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
library(gmodels)
CrossTable(xtest_1$Absentismo_horas_rango,predicted_data_1,prop.chisq=FALSE,prop.c=FALSE,prop.r=FALSE,dnn = c('Absentismo actual','Absentismo predicho'))


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
print(sprintf("La precisión del árbol es: %.4f %%",100*sum(predicted_data_1 == xtest_1$Absentismo_horas_rango) / length(predicted_data_1)))


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
printcp(model1)
plotcp(model1)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
model1_pruned<- prune(model1, cp= model1$cptable[which.min(model1$cptable[,"xerror"]),"CP"])
rpart.plot(model1_pruned)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
print(model1_pruned, digits = 2)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
summary(model1_pruned)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
predicted_data_2 <- predict(model1_pruned, xtest_1, type = "class")
CrossTable(xtest_1$Absentismo_horas_rango,predicted_data_2,prop.chisq=FALSE,prop.c=FALSE,prop.r=FALSE,dnn = c('Absentismo actual','Absentismo predicho'))


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
print(sprintf("La precisión del árbol es: %.4f %%",100*sum(predicted_data_2 == xtest_1$Absentismo_horas_rango) / length(predicted_data_2)))


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Hacemos copia de los datos sin la variable que los clasifica
input <- df[,2:19]


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
library(optCluster)
optresult <- optCluster(input, 2:6, seed = 123,
                  clMethods  = c("hierarchical", "kmeans", "pam", "agnes", "diana", "clara", "model"),
                  validation = c("stability", "internal")
                )
summary(optresult)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
library(factoextra)
res.hc <- eclust(input, "hclust",  hc_metric = "euclidean", k= 4,  hc_method = "ward.D2", graph = FALSE, seed = 123)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
fviz_dend(res.hc, k = 4, palette = "jco",rect = TRUE, rect_border = "jco", cex = 0.3, rect_fill = TRUE) + labs(title = "Herarchical clustering",
       subtitle = "Distancia euclídea, Ward D2, K=4")


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
fviz_dend(res.hc, xlim = c(1, 20), ylim = c(0, 8))


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
res.hc <- eclust(input, "hclust", k = 4, graph = FALSE)
fviz_silhouette(res.hc, print.summary = TRUE, palette = "jco", ggtheme = theme_classic())


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
res.hc_2 <- eclust(input, "hclust",  hc_metric = "minkowski", k= 4,  hc_method = "complete", graph = FALSE, seed = 123)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
fviz_dend(res.hc_2, k = 4, palette = "jco",rect = TRUE, rect_border = "jco", cex = 0.3, rect_fill = TRUE) + labs(title = "Herarchical clustering",
       subtitle = "Minkowski, Complete, K=4")


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
fviz_silhouette(res.hc_2, print.summary = TRUE, palette = "jco", ggtheme = theme_classic())


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
x <- df
N=dim(x)[1]
all=seq(1,N)

# seleccionar 2/3 al azar
train=sort(sample(N,N*2/3.0))
test=setdiff(all,train)
set.seed(123)
xtrain=x[train,]
xtest=x[test,]


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
prop.table(table(xtrain$Absentismo_horas_rango))
prop.table(table(xtest$Absentismo_horas_rango))


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
library(C50)
model1 <- C50::C5.0(xtrain[,c(2:19)],as.factor(xtrain$Absentismo_horas_rango), rules=TRUE)
model1
summary(model1)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
Absentismo_pred <- predict(model1, xtest)
CrossTable(xtest$Absentismo_horas_rango,Absentismo_pred,prop.chisq=FALSE,prop.c=FALSE,prop.r=FALSE,dnn = c('Absentismo actual','Absentismo predicho'))


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
print(sprintf("La precisión del árbol es: %.4f %%",100*sum(Absentismo_pred == xtest$Absentismo_horas_rango) / length(Absentismo_pred)))


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
dataset_pca <- df
set.seed(123)
clasif_absentismo <- dataset_pca[,21] 
var_Absentismo <- dataset_pca[,2:19] 

pca_abs <- princomp(var_Absentismo)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
fviz_eig(pca_abs,addlabels=T)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
var <- get_pca_var(pca_abs)
head(var$contrib)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
xtrain$C1=predict(pca_abs,xtrain[,2:21])[,1]
xtest$C1=predict(pca_abs,xtest[,2:21])[,1]


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
model2 <- C50::C5.0(xtrain[,c(2,22)],as.factor(xtrain$Absentismo_horas_rango), rules=TRUE)
model22 <- C50::C5.0(xtrain[,c(2,22)],as.factor(xtrain$Absentismo_horas_rango))
summary(model2)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
options(repr.plot.width=4, repr.plot.height=3)
plot(model22)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
Absentismo_pred2 <- predict(model2, xtest)
CrossTable(xtest$Absentismo_horas_rango,Absentismo_pred2,prop.chisq=FALSE,prop.c=FALSE,prop.r=FALSE,dnn = c('Absentismo actual','Absentismo predicho'))


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
print(sprintf("La precisión del árbol es: %.4f %%",100*sum(Absentismo_pred2 == xtest$Absentismo_horas_rango) / length(Absentismo_pred2)))

