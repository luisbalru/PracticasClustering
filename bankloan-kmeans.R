# PRÁCTICA DE CLUSTERING
# Dataset: Bankloan
# Autor: Luis Balderas Ruiz
# Fecha: Diciembre 2019

library(cluster)
library(fpc)

# Lectura del fichero

bankloan = read.csv2("./data/bankloan-spss.csv",header=TRUE, sep=';')
str(bankloan)

# Hay muchos valores perdidos en la variable impago
any(is.na(bankloan$impago))

#Me quedo con las instancias que no tienen NA
bankloan_nna = bankloan[1:700,]

# Divido el contenido en variables económicas y personales
economia = data.frame(bankloan_nna$ingresos, bankloan_nna$deudaingr, bankloan_nna$deudacred,bankloan_nna$deudaotro, bankloan_nna$morapred1, bankloan_nna$morapred2, bankloan_nna$morapred3)
personal = data.frame(bankloan_nna$edad,bankloan_nna$educ,bankloan_nna$empleo,bankloan_nna$direccion,bankloan_nna$impago)


#normalizo las variables numéricas (economía)
for (j in 1:7) {x=economia[,j] ; v=(x-mean(x))/sqrt(var(x)); economia[,j]=v}

# cluster sobre la economía y con dos clusters
kmeans.result=kmeans(economia,2)
kmeans.result
table(personal$bankloan_nna.impago,kmeans.result$cluster)
#Analisis de bondad
plot(economia[c("bankloan_nna.ingresos","bankloan_nna.deudaingr")], col=kmeans.result$cluster)
points(kmeans.result$centers[,c("bankloan_nna.ingresos","bankloan_nna.deudaingr")],col=1:3,pch=8,cex=2)
x=kmeans.result$cluster
plotcluster(economia,x)
shi= silhouette(kmeans.result$cluster,dist(economia))
plot(shi,col=1:3)
#Calculo de algunas otras medidas de bondad del agrupamiento
#(Ver descripcion de la funcion)
group=kmeans.result$cluster
cluster.stats(dist(economia),group,alt.clustering=personal$bankloan_nna.impago)

# CON VARIABLES NUMÉRICAS Y CATEGÓRICAS
# Defino las distancias
dist1 = dist(economia)
dist2 = dist(personal)
dista = (dist1+dist2)/2
kmeans.result2=kmeans(dista,2)
kmeans.result2$centers
grupo2=kmeans.result2$cluster
plotcluster(economia,grupo2)
plotcluster(personal,grupo2)
shi2 = silhouette(grupo2,dista)
plot(shi2,col=1:3)
cluster.stats(dista,grupo2)

# TODOS LOS DATOS JUNTOS. INTENTANDO PREDECIR IMPAGO
kmeans.result3=kmeans(bankloan_nna,2)
kmeans.result3
table(personal$bankloan_nna.impago,kmeans.result3$cluster)
plot(bankloan_nna[c("ingresos","deudaingr")], col=kmeans.result3$cluster)
points(kmeans.resul3t$centers[,c("ingresos","deudaingr")],col=1:3,pch=8,cex=2)
x3=kmeans.result3$cluster
plotcluster(bankloan_nna,x3)
shi3= silhouette(kmeans.result3$cluster,dist(bankloan_nna))
plot(shi3)

# SOLO PERSONALES

personal2 = personal
personal2$bankloan_nna.impago = NULL
kmeans.result.personales = kmeans(personal2,2)
kmeans.result.personales
table(personal$bankloan_nna.impago,kmeans.result.personales$cluster)
plot(personal[c("bankloan_nna.edad","bankloan_nna.empleo")], col=kmeans.result.personales$cluster)
plot(personal[c("bankloan_nna.edad","bankloan_nna.direccion")], col=kmeans.result.personales$cluster)
xper=kmeans.result.personales$cluster
plotcluster(personal2,xper)
ship= silhouette(kmeans.result.personales$cluster,dist(personal2))
plot(ship)