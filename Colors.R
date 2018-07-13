#para crear una gama de colores usamos la paleta especificando los colores que queremos combinar:
paletacolores<-c("red","yellow","green")
a<-colorRampPalette(paletacolores)

#hacemos el plot marcando tantas repeticiones como gama queremos:
numgamas<-6
plot(rep(1,numgamas),col=a(3),pch=19,cex=3)
