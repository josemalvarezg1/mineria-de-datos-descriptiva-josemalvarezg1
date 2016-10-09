setwd("C:/Users/José Manuel/Desktop/ICD-Tarea2")

#Dataset Universidades
#Se lee el dataset Universidades
universidades = read.csv(file = "data/Universidades.csv", header = F)
#Se elimina la primera columna 
universidades = universidades[,-1]
#Se colocan como nombres de fila los ID de cada universidad
rownames(universidades) <- universidades[,1]
#Se elimina la primera columna del dataset
universidades = universidades[,-1]
#Sólo se trabajará con las columnas de las 12 a la 15 que contienen datos numéricos
universidades = universidades[,12:15]
#Se elimina la primera fila del dataset
universidades = universidades[-1,]
#Se calcula K-Medias con K=2 y 100 iteraciones
clusters <- kmeans(universidades, 2, iter.max = 100) 
#Se grafica el dataset inicial
plot(universidades, pch = 20)
#Se especifican los dos grupos (puntos) en el gráfico
points(clusters$centers, pch =19, col = "blue", cex = 2)
#Se colorean los dos grupos en el gráfico
points(universidades, col = clusters$cluster+4, pch = 19)
#Se calcula K-Medias con un K=5 (y por defecto 10 iteraciones)
clusters <- kmeans(universidades, 5)
#Se grafica el dataset inicial
plot(universidades, pch = 20)
#Se especifican los cinco grupos (puntos) en el gráfico
points(clusters$centers, pch = 19, col = "blue", cex = 2)
#Se colorean los cinco grupos en el gráfico
points(universidades, col = clusters$cluster + 1, pch = 19)

# Clasificación Jerárquica
#Se trabaja el dataset como una matriz
datos = as.matrix(universidades)
#Se calcula la matriz de distancia
distancia = dist(datos)
distancia
min(distancia)
max(distancia)


#Dataset Estudiantes
#Se lee el dataset Estudiantes
estudiantes = read.csv(file = "data/Estudiantes.csv", header = F)
#Sólo se trabajará con las columnas 1 y 2 para no tratar la tercera (columna clase)
estudiantes = estudiantes[,1:2]
#Se calcula K-Medias con K=2 y 100 iteraciones
clusters <- kmeans(estudiantes, 2, iter.max = 100) 
#Se grafica el dataset inicial
plot(estudiantes, pch = 20)
#Se especifican los dos grupos (puntos) en el gráfico
points(clusters$centers, pch =19, col = "blue", cex = 2)
#Se colorean los dos grupos en el gráfico
points(estudiantes, col = clusters$cluster+4, pch = 19)
#Se calcula K-Medias con un K=5 (y por defecto 10 iteraciones)
clusters <- kmeans(estudiantes, 5)
#Se grafica el dataset inicial
plot(estudiantes, pch = 20)
#Se especifican los cinco grupos (puntos) en el gráfico
points(clusters$centers, pch = 19, col = "blue", cex = 2)
#Se colorean los cinco grupos en el gráfico
points(estudiantes, col = clusters$cluster + 1, pch = 19)

# Clasificación Jerárquica
#Se trabaja el dataset como una matriz
datos = as.matrix(estudiantes)
distancia = dist(datos)
# Error Tamaño