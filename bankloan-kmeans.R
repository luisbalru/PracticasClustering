# PRÁCTICA DE CLUSTERING
# Dataset: Bankloan
# Autor: Luis Balderas Ruiz
# Fecha: Diciembre 2019

library(cluster)
library(fpc)

# Lectura del fichero

bankloan = read.csv2("./data/bankloan-spss.csv",header=TRUE, sep=';')
str(bankloan)

x = data.frame(bankloan$edad,bankloan[,5:8],bankloan[,10:12])
#x = x[1:700,]
dist1=dist(x)

y=data.frame(bankloan[,2:4])
View(y)
dist2=dist(y,method="binary")

dista=(dist1+dist2)/2
kmeans.result=kmeans(dista,2)
kmeans.result$centers
grupo=kmeans.result$cluster
#Analsis de bondad, restrinjo valores para dibujar
idx=sample(1:dim(x)[1],250)
plotcluster(x[idx,],grupo[idx])
plotcluster(y[idx,],grupo[idx])
d1=dist(x[idx,])
d2=dist(y[idx,])
d3=(d1+d2)/2
shi= silhouette(grupo[idx],d3)
plot(shi,col=1:3)
cluster.stats(dista,grupo)


