## ----setup, include=FALSE------------------------------------------------------------------------------------------
knitr::purl("vnoronoz-PEC1.Rmd")


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Cargamos los paquetes R que vamos a usar
library(ggplot2)
library(dplyr)

# Cargamos el fichero de datos
totalData <- read.csv('titanic.csv',stringsAsFactors = FALSE)
filas=dim(totalData)[1]

# Verificamos la estructura del conjunto de datos
str(totalData)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
#Estadísticas básicas
summary(totalData)

# Estadísticas de valores vacíos
colSums(is.na(totalData))
colSums(totalData=="")

# Tomamos valor "Desconocido" para los valores vacíos de la variable "country"
totalData$Embarked[totalData$country==""]="Desconocido"

# Tomamos la media para valores vacíos de la variable "Age"
totalData$Age[is.na(totalData$age)] <- mean(totalData$age,na.rm=T)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Añadimos una variable nueva a los datos. Este valor es la edad discretizada con un método simple de intervalos de igual amplitud.
# Vemos cómo se distribuyen los valore
summary(totalData[,"age"])
# Discretizamos
totalData["segmento_edad"] <- cut(totalData$age, breaks = c(0,10,20,30,40,50,60,70,100), labels = c("0-9", "10-19", "20-29", "30-39","40-49","50-59","60-69","70-79"))
# Observamos los datos discretizados.
head(totalData)
# Vemos como se agrupan los datos.
plot(totalData$segmento_edad)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Visualizamos la relación entre las variables "sex" y "survival":
ggplot(data=totalData[1:filas,],aes(x=gender,fill=survived))+geom_bar()

# Otro punto de vista. Survival como función de Embarked:
ggplot(data = totalData[1:filas,],aes(x=embarked,fill=survived))+geom_bar(position="fill")+ylab("Frecuencia")



## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
t<-table(totalData[1:filas,]$embarked,totalData[1:filas,]$survived)
for (i in 1:dim(t)[1]){
    t[i,]<-t[i,]/sum(t[i,])*100
}
t


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Ahora, podemos dividir el gráfico de Embarked por Pclass:
ggplot(data = totalData[1:filas,],aes(x=embarked,fill=survived))+geom_bar(position="fill")+facet_wrap(~class)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Survivial como función de SibSp y Parch
ggplot(data = totalData[1:filas,],aes(x=sibsp,fill=survived))+geom_bar()
ggplot(data = totalData[1:filas,],aes(x=parch,fill=survived))+geom_bar()
# Vemos como las forma de estos dos gráficos es similar. Este hecho nos puede indicar presencia de correlaciones altas.


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------

# Construimos un atributo nuevo: family size.
totalData$FamilySize <- totalData$sibsp + totalData$parch +1;
totalData1<-totalData[1:filas,]
ggplot(data = totalData1[!is.na(totalData[1:filas,]$FamilySize),],aes(x=FamilySize,fill=survived))+geom_histogram(binwidth =1,position="fill")+ylab("Frecuencia")

  


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Survival como función de age:
ggplot(data = totalData1[!(is.na(totalData[1:filas,]$age)),],aes(x=age,fill=survived))+geom_histogram(binwidth =3)
ggplot(data = totalData1[!is.na(totalData[1:filas,]$age),],aes(x=age,fill=survived))+geom_histogram(binwidth = 3,position="fill")+ylab("Frecuencia")


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Cargamos los paquetes R que vamos a usar
library(ggplot2)
library(dplyr)
library(scales)
# Cargamos los dos archivos del juego de datos
# Primero el archivo adult.data 
# Indicamos que el archivo no tiene cabecera, que las cadenas se lean como factores, que las celdas que no tienen valor estan con el caracter " ?" y que corte el espacio en blanco que hay delante de cada dato
adult_data <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data', stringsAsFactors = TRUE, header = FALSE, na.strings = '?', strip.white = TRUE)

colnames(adult_data) <- c('age', 'workclass', 'fnlwgt', 'education', 
                     'education_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex', 'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')

# Segundo el archivo adult.test y nombres de sus atributos
# Indicamos que salte la primera fila, que las cadenas se lean como factores, que las celdas que no tienen valor estan con el caracter "?" y que corte el espacio en blanco que hay delante de cada dato
adult_test <- read.csv('https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test',  stringsAsFactors = TRUE, header = FALSE, skip = 1, na.strings = '?', strip.white = TRUE)

colnames(adult_test) <- c('age', 'workclass', 'fnlwgt', 'education', 
                     'education_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex', 'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Unión de adult.data y adult.test
datos_adult <- rbind(adult_data, adult_test)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Usamos dim que nos devuelve las filas y columnas del objeto
dim(datos_adult)[1]


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
dim(datos_adult)[2]


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
str(datos_adult)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Estadísticas básicas
summary(datos_adult)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Cabecera
head(datos_adult)

## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Cola
tail(datos_adult)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Estadísticas de valores vacíos
colSums(is.na(datos_adult))


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
datos_adult <- na.omit(datos_adult)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
dim(datos_adult)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
datos_adult <- datos_adult[,-c(3,5,8)]


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
dim(datos_adult)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Creamos el campo capital
datos_adult <- mutate(datos_adult,capital = capital_gain - capital_loss)
# Borramos capital_gain y capital_loss
datos_adult <- datos_adult[,-c(8,9)]


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
dim(datos_adult)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Quitamos el punto final de los valores
datos_adult$income <- gsub('<=50K.', '<=50K', datos_adult$income)
datos_adult$income <- gsub('>50K.', '>50K', datos_adult$income)
datos_adult$income <- as.factor(datos_adult$income)

## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
levels(datos_adult$income)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Discretizamos
datos_adult['age_break'] <- cut(datos_adult$age, breaks = c(-Inf,19,29,39,49,59,69,Inf), labels = c('<20','20-29','30-39','40-49','50-59','60-69','>70'))

## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Vista del numero de registros en cada uno de los intervalos
summary(datos_adult$age_break)

## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Vemos como se agrupan los datos.
plot(datos_adult$age_break,xlab='Intervalos de edad', ylab='N° Registros', col='blue')


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Discretizamos
datos_adult['hours_break'] <- cut(datos_adult$hours_per_week, breaks = c(-Inf,19,39,44,49,59,Inf), labels = c('<20','20-39','40-44','45-49','50-59','>60'))

## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Vista del numero de registros en cada uno de los intervalos
plot(datos_adult$hours_break, xlab='Horas trabajadas a la semana', ylab='N° Registros', col=1:6)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
levels(datos_adult$native_country)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# creamos los nuevos valores que recogeran los nombres de paises por region
america <- c('Columbia', 'Cuba', 'Ecuador', 'Guatemala', 'Jamaica', 'Nicaragua', 
                     'Puerto-Rico', 'Dominican-Republic', 'El-Salvador', 
                     'Haiti', 'Honduras', 'Mexico', 'Peru', 'Trinadad&Tobago')
asia <- c('Cambodia', 'China', 'Hong', 'India', 'Iran', 'Laos', 'Thailand',
               'Japan', 'Taiwan', 'Vietnam')
europa <- c('England', 'Germany', 'Holand-Netherlands', 'Hungary', 'Ireland', 
                 'France', 'Greece', 'Italy', 'Poland', 'Portugal', 'Scotland', 'Yugoslavia')
eeuu <- c('Outlying-US(Guam-USVI-etc)', 'United-States')
# creamos la variable country_region y le damos los valores
country_region = ifelse(datos_adult$native_country %in% america, 'America',
                ifelse(datos_adult$native_country %in% asia, 'Asia',
                ifelse(datos_adult$native_country %in% europa, 'Europa', 'EEUU')))
datos_adult <- mutate(datos_adult,country_region)
# la pasamos a factor
datos_adult$country_region <- as.factor(datos_adult$country_region)

## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
summary(datos_adult$country_region)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Para el atributo workclass
ggplot(datos_adult, aes(x=workclass)) + ggtitle("Situación Laboral") + xlab("Tipo de trabajo") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Porcentaje") + coord_flip() + 
  scale_x_discrete(limits = rev(levels(datos_adult$workclass)))
# Para el atributo education
ggplot(datos_adult, aes(x=education)) + ggtitle("Educación") + xlab("Educacion") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Porcentaje") + coord_flip() +
  scale_x_discrete(limits = rev(levels(datos_adult$education)))
# Para el atributo occupation
ggplot(datos_adult, aes(x=occupation)) + ggtitle("Ocupación") + xlab("Ocupacion") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Porcentaje") + coord_flip() +
  scale_x_discrete(limits = rev(levels(datos_adult$occupation)))
# Para el atributo marital_status
ggplot(datos_adult, aes(x=marital_status)) + ggtitle("Estado civil") + xlab("Estado civil") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Porcentaje") + coord_flip() +
  scale_x_discrete(limits = rev(levels(datos_adult$marital_status)))
# Para el atributo country_region
ggplot(datos_adult, aes(x=country_region)) + ggtitle("Regiones") + xlab("Regiones") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..))) + ylab("Porcentaje") + coord_flip() + 
  scale_x_discrete(limits = rev(levels(datos_adult$country_region))) 


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Para el atributo race
ggplot(datos_adult, aes(x=factor(1), fill=datos_adult$race)) + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) + 
  xlab("") + ylab("") + ggtitle("Raza")
# Para el atributo sex
ggplot(datos_adult, aes(x=factor(1), fill=datos_adult$sex)) + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 1) + coord_polar(theta="y") + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.title=element_blank()) + 
  xlab("") + ylab("") + ggtitle("Sexo") 


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Primero revisamos la distribucion de la variable income sola
ggplot(datos_adult, aes(x=datos_adult$income, fill=datos_adult$income)) + 
  geom_bar(aes(y=100*(..count..)/sum(..count..))) +
  labs(x = "Ingresos",y = "Porcentaje", fill = "Income") + ggtitle("Nivel de ingresos") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                      y = (..count..)/sum(..count..) ), stat = "count", vjust=-5, size = 3) 


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
ggplot(datos_adult, aes(x=datos_adult$income, fill=datos_adult$income)) + 
  geom_bar(aes(y=100*(..count..)/sum(..count..))) +
  labs(x = "Ingresos",y = "Porcentaje", fill = "Income") + 
  ggtitle("Nivel de ingresos por sexo") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y = (..count..)/sum(..count..) ),   stat = "count", vjust=-5, size = 3) + facet_grid(~datos_adult$sex)


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
ggplot(datos_adult, aes(x=datos_adult$education, fill=datos_adult$income)) + 
  geom_bar(aes(y=100*(..count..)/sum(..count..))) +
  labs(x = "Educacion",y = "Porcentaje", fill = "Income") + 
  ggtitle("Nivel de ingresos vs Nivel de educación") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
ggplot(datos_adult, aes(x=datos_adult$occupation, fill=datos_adult$income)) + 
  geom_bar(aes(y=100*(..count..)/sum(..count..))) +
  labs(x = "Ocupación",y = "Porcentaje", fill = "Income") + 
  ggtitle("Nivel de ingresos vs Ocupación") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
ggplot(datos_adult, aes(x=datos_adult$country_region, fill=datos_adult$income)) + 
  geom_bar(aes(y=100*(..count..)/sum(..count..))) +
  labs(x = "Región",y = "Porcentaje", fill = "Income") + 
  ggtitle("Nivel de ingresos vs Región") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
ggplot(datos_adult, aes(x=datos_adult$age_break, fill=datos_adult$income)) + 
  geom_bar(aes(y=100*(..count..)/sum(..count..))) +
  labs(x = "Rango de edad",y = "Porcentaje", fill = "Income") + 
  ggtitle("Nivel de ingresos vs Edad") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
ggplot(datos_adult, aes(x = datos_adult$age, fill = datos_adult$income)) + 
  geom_density(alpha = 0.3) + 
  scale_x_continuous(breaks = seq(0, 95, 5)) + 
  labs(y='', x = 'Edad', fill= 'Income')


## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------
ggplot(datos_adult, aes(x=datos_adult$hours_break, fill=datos_adult$hours_break)) + 
  geom_bar(aes(y=100*(..count..)/sum(..count..))) +
  labs(x = "Horas trabajadas",y = "Porcentaje", fill = "Horas por semana") + 
  ggtitle("Horas trabajadas a la semana según Ingresos") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y = (..count..)/sum(..count..) ),   stat = "count", size = 2.5) + facet_grid(~datos_adult$income)

