setwd("C:/Users/José Manuel/Documents/ICD/mineria-de-datos-descriptiva-josemalvarezg1")

#Dataset Universidades

#Se lee el dataset Universidades
universidades = read.csv(file = "data/Universidades.csv", header = T, dec = ".", sep = ",")
#Se elimina la primera columna 
universidades = universidades[,-1]
#Se colocan como nombres de fila los ID de cada universidad
rownames(universidades) <- universidades[,1]
#Se elimina la primera columna del dataset
universidades = universidades[,-1]
#Sólo se trabajará con las columnas de las 12 a la 15 que contienen datos numéricos
universidades = universidades[,12:15]
#Se aplica el Codo de Jambu para obtener el K más adecuado al método de K-Medias
plot(universidades, pch = 19)
InerciaIC = rep(0,50)
for (k in 1:50) {
  grupos = kmeans(universidades, k)
  InerciaIC[k] = grupos$tot.withinss
}
#Se grafica la Inercia Inter-Clases
plot(InerciaIC, col = "blue", type = "b")
#Se puede observar que cambia muy poco a partir de K=2 y K=3
#Se calcula K-Medias con K=3 y 100 iteraciones
clusters <- kmeans(universidades, 3, iter.max = 100) 
#Se grafica el dataset inicial
plot(universidades, pch = 20)
#Se colorean los tres grupos en el gráfico
plot(universidades, col = clusters$cluster)

# Clasificación Jerárquica
#Se trabaja el dataset como una matriz
datos = as.matrix(universidades)
#Se calcula la matriz de distancia
distancia = dist(datos)
#Se aplica el Método Complete
cluster = hclust(distancia, method = "complete")
plot(cluster)
#Se determina la altura requerida con k clusters, cortando el dendograma con k clases:
corteD = cutree(cluster, k = 3)
#Observamos la cantidad de clusters
unique(corteD)
#Graficamos los clusters
plot(universidades, col = corteD, main = "COMPLETE")
#Se aplica el Método Single
cluster = hclust(distancia, method = "single")
plot(cluster)
#Se determina la altura requerida con k clusters, cortando el dendograma con k clases:
corteD = cutree(cluster, k = 3)
#Observamos la cantidad de clusters
unique(corteD)
#Graficamos los clusters
plot(universidades, col = corteD, main = "SINGLE")
#Se aplica el Método Average
cluster = hclust(distancia, method = "average")
plot(cluster)
#Se determina la altura requerida con k clusters, cortando el dendograma con k clases:
corteD = cutree(cluster, k = 3)
#Observamos la cantidad de clusters
unique(corteD)
#Graficamos los clusters
plot(universidades, col = corteD, main = "AVERAGE")
#Se aplica el Método Ward
cluster = hclust(distancia, method = "ward.D")
plot(cluster)
#Se determina la altura requerida con k clusters, cortando el dendograma con k clases:
corteD = cutree(cluster, k = 3)
#Observamos la cantidad de clusters
unique(corteD)
#Graficamos los clusters
plot(universidades, col = corteD, main = "WARD")

#Análisis Exploratorio
library(FactoMineR)
#Se muestras valores de interés del dataset
head(universidades)
dim(universidades)
names(universidades)
str(universidades)
attributes(universidades)
summary(universidades)
pca <- PCA(universidades)


#Dataset Estudiantes

#Se lee el dataset Estudiantes
estudiantes = read.csv(file = "data/Estudiantes.csv", header = F)
#Sólo se trabajará con las columnas 1 y 2 para no tratar la tercera (columna clase)
estudiantes = estudiantes[,1:2]
#Se grafica el dataset inicial
plot(estudiantes, pch = 20)
#Se aplica el Codo de Jambu para obtener el K más adecuado al método de K-Medias
InerciaIC = rep(0,50)
for (k in 1:50) {
  grupos = kmeans(estudiantes, k)
  InerciaIC[k] = grupos$tot.withinss
}
#Se grafica la Inercia Inter-Clases
plot(InerciaIC, col = "blue", type = "b")
#Se puede observar que cambia muy poco a partir de K=2 y K=3
#Se calcula K-Medias con K=3 y 100 iteraciones
clusters <- kmeans(estudiantes, 3, iter.max = 100) 
plot(estudiantes, pch = 20)
#Se especifican los tres grupos (puntos) en el gráfico
points(clusters$centers, pch = 19, col = "blue", cex = 2)
#Se colorean los tres grupos en el gráfico
plot(estudiantes, col = clusters$cluster)

# Clasificación Jerárquica
#Se trabaja el dataset como una matriz
datos = as.matrix(estudiantes)
distancia = dist(datos)
# Error Tamaño: "cannot allocate vector of size 335.3 Gb"

#Análisis Exploratorio
library(FactoMineR)
#Se muestras valores de interés del dataset
head(estudiantes)
dim(estudiantes)
names(estudiantes)
str(estudiantes)
attributes(estudiantes)
summary(estudiantes)
pca <- PCA(estudiantes)

#Dataset Alimentacion

#Reglas de Asociación
library(arules)
library(arulesViz)
#Se lee el dataset Alimentacion
alimentacion <- read.transactions("data/Alimentacion.csv", sep=',')
#Se transforma  dataframe en transaccional
reglas <- apriori(alimentacion, parameter=list(support=0.001, confidence = 0.65))
summary(reglas)
#Se muestran las las 10 transacciones con mayor frecuencia en el dataset
itemFrequencyPlot(alimentacion,topN=10,type="absolute")
#Se obtienen las subreglas que cumplan chicles -> refrescos
subreglas = subset(reglas, subset=lhs %pin% "chicles" & rhs %pin% "refrescos")
#Se muestra la probabilidad de que un estudiante consuma refrescos dado que consume chicles
quality(subreglas)$confidence
#Se ordenan las reglas por confianza
confianzaAlta <-sort(reglas, by="confidence", decreasing=TRUE)
inspect(head(confianzaAlta))
#Se ordenan las reglas por soporte
supportAlto <-sort(reglas, by="support", decreasing=TRUE)
inspect(head(supportAlto))
#Se ordenan las reglas por lift
liftAlto <-sort(reglas, by="lift", decreasing=TRUE)
inspect(head(liftAlto))
#Se obtienen las subreglas que cumplan ... -> Naranja
subreglas = subset(reglas, subset = rhs %ain% "Naranja")
#Se muestran las subreglas generadas
inspect(subreglas)
#Se obtienen las subreglas que cumplan Flips, Nutella, Vodka -> ...
subreglas = subset(reglas, subset=lhs %ain% c("Flips","Nutella","Vodka"))
#Se muestran las subreglas generadas
inspect(subreglas)