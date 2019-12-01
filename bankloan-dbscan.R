# PRÁCTICA DE CLUSTERING
# Dataset: Bankloan
# Autor: Luis Balderas Ruiz
# Fecha: Diciembre 2019

library(fpc)

bankloan = read.csv2("./data/bankloan-spss.csv",header=TRUE, sep=';')

#Me quedo con las instancias que no tienen NA
bankloan_nna = bankloan[1:700,]
bankloan_nna1 = bankloan_nna
bankloan_nna1$impago = NULL

ds1 = dbscan(bankloan_nna1,eps=0.00001,MinPts=10)
table(ds1$cluster,bankloan_nna$impago)
x = plot(ds1,bankloan_nna1)
plot(ds1, bankloan_nna1[c(5,6)])
plot(ds1, bankloan_nna1[c(1,5)])
nc = length(x[,1])
shi1 = silhouette(ds1$cluster,dist(bankloan_nna1))
plot(shi1,col=1:nc)


for (j in 1:4) {x=iris2[,j] ; v=(x-mean(x))/sqrt(var(x)); iris2[,j]=v}
ds=dbscan(iris2,eps=1.0,MinPts=5)
table(ds$cluster,iris$Species)
plot(ds,iris2)
plot(ds,iris2[c(1,4)])
plot(ds,iris2[c(3,4)])
x=table(ds$cluster,iris$Species)
nc=length(x[,1])
nc
shi= silhouette(ds$cluster,dist(iris2))
plot(shi,col=1:nc)
