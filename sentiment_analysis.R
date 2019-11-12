library(e1071) 
library(rpart)
library(caret)
library(tidyverse)
library(tokenizers)
library(tm)

# Se declara la URL de dónde obtener los datos
theUrlMain <- "http://RAlize.RSolver.com/RAlize/data/small_sample2019clean.csv"

# Se declaran los nombres de las columnas
columnas <- c("texto","sentimiento")

# Se cargan datos principales a una estructura (commentsdataset), asignando nombres de atributos a las columnas
commentsdataset <- read.csv(file = theUrlMain, header = FALSE, sep = ";", col.names=columnas, skipNul = TRUE)

# Se define in seed único, para que la repetición aleatoria, coincida entre corridas
set.seed(1)
ratio = sample(1:nrow(commentsdataset), size = 0.80 * nrow(commentsdataset)) # Proporción: 80%/20%
Train = commentsdataset[ratio,] 
Test = commentsdataset[-ratio,] 

# Como referencia se imprime la proporción de datos del atributo de interés: ingreso
table(Train$sentimiento)
table(Test$sentimiento)


# Aquí va la transformación del texto variable a un vocabulario 
full_text <- paste(commentsdataset[,1], collapse = " ")

#se puede hacer todo con la libreria tm https://stackoverflow.com/questions/32225770/r-tm-removewords-function-not-removing-words
clean_text <- tolower(full_text) # Todo el texto a minusculas
clean_text <- str_replace_all(clean_text,"http\\S*", "") # remover todas las palabras que tengan http
clean_text <- str_replace_all(clean_text,"https\\S*", "") # remover todas las palabras que tengan https
clean_text <- str_replace_all(clean_text,"[[:punct:]]", " ") # remover todos los signos de puntuacion
clean_text <- str_replace_all(clean_text,"[[:digit:]]", " ") # remover todos los numeros
clean_text <- str_replace_all(clean_text,"[\\s]+", " ") # remover todas las cadenas que tengan mas de 1 espacio

#falta remover los strings con una sola letra

# remover las stopwords del texto
clean_text <- removeWords(clean_text,stopwords("spanish"))
words <- tokenize_words(clean_text)

# Creo la tabla de frecuencias
tabla <- table(words[[1]])
tabla <- data_frame(word = names(tabla), count = as.numeric(tabla))
arrange(tabla,desc(count))



# Decision ., data=Train, method="class", minbucket=10)
tree.predict <- predict(tree.model, Test, type = "class")
print("Resultados Árbol de Decisión")
confusionMatrix(tree.predict, Test$sentimiento) 

# Naive Bayes
NB_model <- naiveBayes(sentimiento ~ ., data=Train)
NB_predict <- predict(NB_model, Test[,-12])
print("Resultados Naive Bayes")
confusionMatrix(NB_predict, Test$sentimiento) 
