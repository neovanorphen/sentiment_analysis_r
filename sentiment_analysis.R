#Cargamos librerias

install.packages('e1071')
install.packages('caret')
install.packages('caTools')
install.packages('tm')
install.packages('SnowballC')

library(e1071) 
library(rpart)
library(caret)
library(plyr)
library(tm)
library(SnowballC)
library(caTools)

# Se declara la URL de d?nde obtener los datos
theUrlMain <- "http://RAlize.RSolver.com/RAlize/data/small_sample2019clean.csv"

# Se declaran los nombres de las columnas
columnas <- c("texto","sentimiento")

# Se cargan datos principales a una estructura (commentsdataset), asignando nombres de atributos a las columnas
commentsdataset <- read.csv(file = theUrlMain, header = FALSE, sep = ";", col.names=columnas, skipNul = TRUE, encoding = 'UTF-8')

dim(commentsdataset)
str(commentsdataset)
head(commentsdataset, 2)


table(commentsdataset$sentimiento) #para conocer el total de sentimientos positivos y negativos

# usando Corpus de la libreria tm creamos el corpus con la columna texto del dataframe
corpus <- Corpus(VectorSource(commentsdataset$texto))

length(corpus)
content(corpus[[10]])  #para mostrar el contenido del corpus en la posicion 10 y como cambia despues del tratamiento

corpus <- tm_map(corpus, tolower) # bajamos todos los textos a minusculas

content(corpus[[10]])

corpus <- tm_map(corpus, removePunctuation)  # eliminamos todos los signos de puntuacion  y simbolos

content(corpus[[10]])

stopwords("spanish") [1:10] #cargamos las  10 primeras stopwords  en españolde la lista de la libreria tm

corpus <- tm_map(corpus, removeWords, c(stopwords("spanish"))) # removemos las stopwords del corpus

content(corpus[[10]])

corpus <- tm_map(corpus, stemDocument)

content(corpus[[10]])

frecuencias <- DocumentTermMatrix(corpus) # creamos la matriz de frecuencias de los terminos del corpus

frecuencias

inspect(frecuencias[750:800,505:515])

findFreqTerms(frecuencias, lowfreq = 1)

dispercion <- removeSparseTerms(frecuencias, 0.995) # removemos los terminos que con menos frecuencia

dispercion

# Posteriormente creamos  el dataframe con la matriz de terminos y le adjuntamos la columna correspondiente a los sentimientos
commentdispercion <- as.data.frame(as.matrix(dispercion))

colnames(commentdispercion) = make.names(colnames(commentdispercion))

commentdispercion$sentimiento <- commentsdataset$sentimiento

commentdispercion$sentimiento <- as.factor(commentdispercion$sentimiento)

str(commentdispercion$sentimiento)

colnames(commentdispercion)

# Se define un seed unico, para que la repeticion aleatoria, coincida entre corridas
set.seed(1)
split <- sample.split(commentdispercion$sentimiento, SplitRatio = 0.8)
Train = subset(commentdispercion, split==TRUE) 
Test  = subset(commentdispercion, split==FALSE)

# Como referencia se imprime la proporcion de datos del atributo de interess: sentimiento
table(Train$sentimiento)
table(Test$sentimiento)

bechmark = 680/914

# procedemos a correr los diferentes modelos de clasificacion SVM, DT, y NB

#SVM - Support Vector Machine
SVM <- svm(as.factor(sentimiento) ~ ., data = Train)
summary(SVM)
predicSVM <- predict(SVM, newdata = Test)
confusionMatrix(predicSVM, Test$sentimiento)

# Decision Tree
tree.model <- rpart(as.factor(sentimiento) ~ ., data=Train, method="class", minbucket=10)
tree.predict <- predict(tree.model, Test, type = "class")
print("Resultados ?rbol de Decisi?n")
confusionMatrix(tree.predict, Test$sentimiento) 

# Naive Bayes
NB_model <- naiveBayes(as.factor(sentimiento) ~ ., data=Train)
NB_predict <- predict(NB_model, Test[,-373])
print("Resultados Naive Bayes")
confusionMatrix(NB_predict, Test$sentimiento)
