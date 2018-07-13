## Este script dibuja la parte de contornos y de zoom de las neuronas con sus mediana y media circulares ##


data<-circular(SoloDendritas$angleR)
cases<-SoloDendritas$ID==CentrosNeur$ID[i] & SoloDendritas$segment_length<10
cases2<-list(SoloDendritas$orientacion,SoloDendritas$Dist_centro)
distsegmentos<-0.5 #0.25 #Distancia entre las q dividir los segmentos
vectorrepet<-(ceiling((SoloDendritas$segment_length)*(1/distsegmentos))/(1/distsegmentos))/distsegmentos #Vector con el numero de segmentos que dividen el segmento original
distradios<-25 #Distancia entre las q dividir las distancias al centro de los puntos
Neurona<-CentrosNeur$ID[i]
Colores<-c("blue","blueviolet","darkgreen","darkorange")
EtiquetasAreasCerebro<-c("Posterior","Anterior","Ventral","Dorsal")
AreasCerebro<-c("P","A","V","D")


#Dibujamos el contorno del mapa
for (i in 1:nrow(ContornoAreas)){
	postscript(file=paste0(".//Art3Orientation//Graficos//MapPlot_",ContornoAreas$map_id[i],".eps"), onefile=TRUE, horizontal=FALSE, width = 14,height = 7, pointsize = 12)
	#jpeg(file=paste0(".//Art3Orientation//Graficos//MapPlot_",ContornoAreas$map_id[i],".jpg"),units="in",res=600,width = 14,height = 7, pointsize = 12)
	par(mar=c(5,5,4,2))
	plot(Contorno$x[Contorno$map_id==ContornoAreas$map_id[i]],Contorno$y[Contorno$map_id==ContornoAreas$map_id[i]],type="b",pch=20,main=paste0(ContornoAreas$ID[i]),xlab="x",ylab="y")
	points(ContornoAreas[i,c("xPosterior","xAnterior","xDorsal","xVentral","map_center_x","Px_lim_izq","Ax_lim_izq","Dx_lim_izq","Vx_lim_izq","Px_lim_izq","Ax_lim_izq","Dx_lim_izq","Vx_lim_izq")],ContornoAreas[i,c("yPosterior","yAnterior","yDorsal","yVentral","map_center_y","Py_lim_izq","Ay_lim_izq","Dy_lim_izq","Vy_lim_izq","Py_lim_izq","Ay_lim_izq","Dy_lim_izq","Vy_lim_izq")],col=c("red","red","red","red","blue","green","green","green","green","blue","blueviolet","darkorange","darkgreen"),pch=c("*","*","*","*","*","*","*","*","*","P","A","D","V"),cex=2)
	neurmap<-unique(CentrosNeur$cell_id[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i]])
	for (j in neurmap){
		rotation<-angularMeasures2d(vector=c(ContornoAreas[i,"xPosterior"]-CentrosNeur$centerXroot[ContornoAreas$individual[i]==CentrosNeur$individual & ContornoAreas$layer[i]==CentrosNeur$layer & CentrosNeur$cell_id==j],ContornoAreas[i,"yPosterior"]-CentrosNeur$centerYroot[ContornoAreas$individual[i]==CentrosNeur$individual & ContornoAreas$layer[i]==CentrosNeur$layer & CentrosNeur$cell_id==j]),axis=c(CentrosNeur$centerXroot[ContornoAreas$individual[i]==CentrosNeur$individual & ContornoAreas$layer[i]==CentrosNeur$layer & CentrosNeur$cell_id==j]+100-CentrosNeur$centerXroot[ContornoAreas$individual[i]==CentrosNeur$individual & ContornoAreas$layer[i]==CentrosNeur$layer & CentrosNeur$cell_id==j],CentrosNeur$centerYroot[ContornoAreas$individual[i]==CentrosNeur$individual & ContornoAreas$layer[i]==CentrosNeur$layer & CentrosNeur$cell_id==j]-CentrosNeur$centerYroot[ContornoAreas$individual[i]==CentrosNeur$individual & ContornoAreas$layer[i]==CentrosNeur$layer & CentrosNeur$cell_id==j]))
		#o lo que es lo mismo: 	rotation<-angularMeasures2d(vector=c(ContornoAreas[i,"xPosterior"]-CentrosNeur$centerXroot[ContornoAreas$individual[i]==CentrosNeur$individual & ContornoAreas$layer[i]==CentrosNeur$layer & CentrosNeur$cell_id==j],ContornoAreas[i,"yPosterior"]-CentrosNeur$centerYroot[ContornoAreas$individual[i]==CentrosNeur$individual & ContornoAreas$layer[i]==CentrosNeur$layer & CentrosNeur$cell_id==j]),axis=c(100,0))
		cases<-SoloDendritas$ID==CentrosNeur$ID[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j] & SoloDendritas$segment_length<10
		repres<-circular(rep(data[cases],vectorrepet[cases]))
		x0<-CentrosNeur$centerXroot[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j]
		y0<-CentrosNeur$centerYroot[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j]
		draw.circle(x=x0,y=y0,radius=100)
		points(x0,y0,pch=paste0(j),cex=0.5)
		#for (j in CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i],"cell_id"]){
		lines(c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],ContornoAreas$Px_lim_izq[i]),c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"],ContornoAreas$Py_lim_izq[i]),col="green")
		lines(c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],ContornoAreas$Ax_lim_izq[i]),c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"],ContornoAreas$Ay_lim_izq[i]),col="green")
		lines(c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],ContornoAreas$Dx_lim_izq[i]),c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"],ContornoAreas$Dy_lim_izq[i]),col="green")
		lines(c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],ContornoAreas$Vx_lim_izq[i]),c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"],ContornoAreas$Vy_lim_izq[i]),col="green")
		points(DataSinContour$x[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="P"],DataSinContour$y[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="P"],col=c("blue"),pch=c(19))
		points(DataSinContour$x[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="D"],DataSinContour$y[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="D"],col=c("darkorange"),pch=c(19))
		points(DataSinContour$x[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="A"],DataSinContour$y[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="A"],col=c("blueviolet"),pch=c(19))
		points(DataSinContour$x[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="V"],DataSinContour$y[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="V"],col=c("darkgreen"),pch=c(19))
	#}
		#points(mean(repres),pch=4,cex=1,bin=360,stack=FALSE,sep=0.1,shrink=1.6,col="darkblue",lwd=4,next.points=-0.065) 
		#points(median(repres),pch=4,cex=1,bin=360,stack=FALSE,sep=0.1,shrink=1.6,col="red",lwd=4,next.points=-0.065) 
		#arrows.circular(mean(repres),x0=x0,y0=y0,col="darkblue",lwd=1,shrink=1,zero=circular(rotation*pi/180))
		#arrows.circular(median.circular(repres),x0=x0,y0=y0,col="red",lwd=1,shrink=1,zero=rotation*pi/180)
	}
	dev.off()
}

#Dibujamos la parte de las neuronas
for (i in 1:nrow(ContornoAreas)){
	#postscript(file=paste0("C://Users//Ileguey//Documents//Revolution//Art3Orientation//Graficos//MapPlotZoom_",ContornoAreas$map_id[i],".eps"), onefile=TRUE, horizontal=FALSE, width = 14,height = 7, pointsize = 12)
	jpeg(file=paste0(".//Art3Orientation//Art3Orientation//Graficos//MapPlotZoom_",ContornoAreas$map_id[i],".jpg"),units="in",res=600,width = 14,height = 7, pointsize = 12)
	par(mar=c(5,5,4,2))
	Xcenters<-CentrosNeur$centerXroot[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i]]
	Ycenters<-CentrosNeur$centerYroot[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i]]
	plot(Xcenters,Ycenters,xlim=c(min(Xcenters)-200,max(Xcenters)+200),ylim=c(min(Ycenters)-200,max(Ycenters)+200),col=c("red"),pch=c(19),xlab="",ylab="")
	neurmap<-unique(CentrosNeur$cell_id[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i]])
	for (j in neurmap){
		cases<-SoloDendritas$ID==CentrosNeur$ID[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j] & SoloDendritas$segment_length<10
		repres<-circular(rep(data[cases],vectorrepet[cases]))
		##Representacion limites
		lines(c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],ContornoAreas$Px_lim_izq[i]),c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"],ContornoAreas$Py_lim_izq[i]),col="blue")
		lines(c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],ContornoAreas$Ax_lim_izq[i]),c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"],ContornoAreas$Ay_lim_izq[i]),col="blueviolet")
		lines(c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],ContornoAreas$Dx_lim_izq[i]),c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"],ContornoAreas$Dy_lim_izq[i]),col="darkorange")
		lines(c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],ContornoAreas$Vx_lim_izq[i]),c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"],ContornoAreas$Vy_lim_izq[i]),col="darkgreen")

		rotation<-angularMeasures2d(vector=c(ContornoAreas[i,"xPosterior"]-CentrosNeur$centerXroot[ContornoAreas$individual[i]==CentrosNeur$individual & ContornoAreas$layer[i]==CentrosNeur$layer & CentrosNeur$cell_id==j],ContornoAreas[i,"yPosterior"]-CentrosNeur$centerYroot[ContornoAreas$individual[i]==CentrosNeur$individual & ContornoAreas$layer[i]==CentrosNeur$layer & CentrosNeur$cell_id==j]),axis=c(CentrosNeur$centerXroot[ContornoAreas$individual[i]==CentrosNeur$individual & ContornoAreas$layer[i]==CentrosNeur$layer & CentrosNeur$cell_id==j]+100-CentrosNeur$centerXroot[ContornoAreas$individual[i]==CentrosNeur$individual & ContornoAreas$layer[i]==CentrosNeur$layer & CentrosNeur$cell_id==j],CentrosNeur$centerYroot[ContornoAreas$individual[i]==CentrosNeur$individual & ContornoAreas$layer[i]==CentrosNeur$layer & CentrosNeur$cell_id==j]-CentrosNeur$centerYroot[ContornoAreas$individual[i]==CentrosNeur$individual & ContornoAreas$layer[i]==CentrosNeur$layer & CentrosNeur$cell_id==j]))
		x0<-CentrosNeur$centerXroot[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j]
		y0<-CentrosNeur$centerYroot[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j]
		draw.circle(x=x0,y=y0,radius=100)
		#points(mean(repres),pch=4,cex=1,bin=360,stack=FALSE,sep=0.1,shrink=1.6,col="darkblue",lwd=4,next.points=-0.065) 
		#points(median(repres),pch=4,cex=1,bin=360,stack=FALSE,sep=0.1,shrink=1.6,col="red",lwd=4,next.points=-0.065) 
		arrows.circular(mean(repres),x0=x0,y0=y0,col="darkblue",lwd=1,shrink=80,zero=rotation*pi/180)
		arrows.circular(median.circular(repres),x0=x0,y0=y0,col="red",lwd=1,shrink=80,zero=rotation*pi/180)
		points(x0,y0,pch=paste0(j),cex=2)

	}
	dev.off()
}
##Representacion limites
#	lines(c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],ContornoAreas$Px_lim_izq[i]),c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"],ContornoAreas$Py_lim_izq[i]),col="green")
#	lines(c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],ContornoAreas$Ax_lim_izq[i]),c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"],ContornoAreas$Ay_lim_izq[i]),col="green")
#	lines(c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],ContornoAreas$Dx_lim_izq[i]),c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"],ContornoAreas$Dy_lim_izq[i]),col="green")
#	lines(c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],ContornoAreas$Vx_lim_izq[i]),c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"],ContornoAreas$Vy_lim_izq[i]),col="green")
##Representacion dendritas
#	points(DataSinContour$x[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="P"],DataSinContour$y[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="P"],col=c("blue"),pch=c(19))
#	points(DataSinContour$x[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="D"],DataSinContour$y[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="D"],col=c("darkorange"),pch=c(19))
#	points(DataSinContour$x[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="A"],DataSinContour$y[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="A"],col=c("blueviolet"),pch=c(19))
#	points(DataSinContour$x[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="V"],DataSinContour$y[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="V"],col=c("darkgreen"),pch=c(19))





PointsCirclesGraph <- function(data,distsegmentos,vectorrepet,cases,cases2,Colores,AreasCerebro,EtiquetasAreasCerebro,Neurona,lengths=TRUE){
#Esta función realiza una representación de la neurona mediante una circunferencia.
#data<-vector de datos, de tipo circular que vamos a tratar
#cases<-subcasos a reprentar
#cases2<-lista con las subvariables para realizar el filtrado por areas
#distsegmentos<-Distancia entre las q dividir los segmentos
#vectorrepet<-Vector con el numero de segmentos que dividen el segmento original
#Colores<-vector de colores para las diferentes areas
#EtiquetasAreasCerebro<-c("Posterior","Anterior","Ventral","Dorsal")
#AreasCerebro<-c("P","A","V","D") tal y como esten en el vector de datos
#Neurona<- El nombr de la neurona q estamos tratando
#lengths<-TRUE/FALSE que nos indica si queremos represetación por distancias o por puntos
	if (lengths==FALSE){
		plot(data[cases],shrink=1.25,main=paste0("Points: ",Neurona),xlab="",ylab="",col="white")#representamos densidad simulada en circunf
		for (o in 1:4){
			points(data[cases & cases2[[1]]==AreasCerebro[o]],pch=16,cex=0.1,bin=360,stack=TRUE,sep=0.035,shrink=1.6,col=Colores[o]) #añadimos los puntos reales
		}	
		ticks.circular(circular(seq(0,11*pi/6,pi/6)),zero=pi/2,rotation='clock', tcl=0.075)
		points(mean(data[cases]),pch=4,cex=1,bin=360,stack=TRUE,sep=0.1,shrink=1.6,col="black",lwd=4)		
		arrows.circular(mean(data[cases]),col="darkblue",lwd=2,shrink=0.9)
		legend("topright",EtiquetasAreasCerebro,fill=Colores,box.col = "transparent",ncol=1,text.width=0.4,x.intersp=0.2,cex=1.2)
	}else{
		repres<-circular(rep(data[cases],vectorrepet[cases]))
		plot(circular(1),tcl.text=0.3,shrink=1.3,main=paste0("Case ",Neurona),sub=paste0("Length represented by ",distsegmentos),xlab="",ylab="",col="white")#representamos densidad simulada en circunf
		draw.circle(x=0,y=0,radius=1)
		for (o in 1:4){
			repres2<-circular(rep(data[cases & cases2[[1]]==AreasCerebro[o]],vectorrepet[cases & cases2[[1]]==AreasCerebro[o]]))
			points(repres2,pch=16,cex=0.1,bin=360,stack=TRUE,sep=0.035,shrink=1.6,col=Colores[o]) #añadimos los puntos reales
		}	
	#representamos las partes del boxplot
		lengths<-0.9
		byvalue=0.001
		cuart<-quantile.circular(circular(repres,units="radians"),probs = c(0.25,0.75))
		if(cuart[[1]]<cuart[[2]]){angseq <- seq(cuart[[1]],cuart[[2]],byvalue)
		}else{angseq <- seq(cuart[[1]],2*pi,byvalue);angseq <- c(angseq,seq(0,cuart[[2]],byvalue))}
		lines(x=lengths*cos(angseq), y=lengths*sin(angseq), col="yellowgreen", lwd=4,lty=1)
		points(circular(c(cuart[[1]],cuart[[2]])),cex=2,bin=360,stack=FALSE,sep=0.1,shrink=1.6,col="yellowgreen",lwd=4,next.points=-0.065)
	#añadimos información de interés al plot	
		ticks.circular(circular(seq(0,11*pi/6,pi/6)),zero=pi/2,rotation='clock', tcl=0.075)
		points(mean(repres),pch=4,cex=1,bin=360,stack=FALSE,sep=0.1,shrink=1.6,col="darkblue",lwd=4,next.points=-0.065) 
		points(median(repres),pch=4,cex=1,bin=360,stack=FALSE,sep=0.1,shrink=1.6,col="red",lwd=4,next.points=-0.065) 
		arrows.circular(mean(repres),col="darkblue",lwd=2,shrink=0.85)
		arrows.circular(median.circular(repres),col="red",lwd=2,shrink=0.85)
		legend("topright",c(EtiquetasAreasCerebro,"Mean","Median","IQR"),fill=c(Colores,"darkblue","red","yellowgreen"),box.col = "transparent",ncol=1,text.width=0.4,x.intersp=0.2,cex=1.2)
		#legend("left",c("Mean","Median","IQR"),fill=c("darkblue","red","yellowgreen"),pch=1,box.col = "transparent",ncol=1,text.width=0.4,x.intersp=0.2,cex=1.2)
	}
}

ConcentricCirclesGraph <- function(data,distsegmentos,vectorrepet,cases,cases2,distradios,Colores,AreasCerebro,EtiquetasAreasCerebro,Neurona){
#Esta función realiza una representación de la neurona mediante círculos concéntricos.
#data<-vector de datos, de tipo circular que vamos a tratar
#cases<-subcasos a reprentar
#cases2<-lista con las subvariables para realizar el filtrado por areas
#distsegmentos<-Distancia entre las q dividir los segmentos
#vectorrepet<-Vector con el numero de segmentos que dividen el segmento original
#distradios<-Distancia entre las q dividir las distancias al centro de los puntos
#Colores<-vector de colores para las diferentes areas
#EtiquetasAreasCerebro<-c("Posterior","Anterior","Ventral","Dorsal")
#AreasCerebro<-c("P","A","V","D") tal y como esten en el vector de datos

	repres<-circular(rep(data[cases],vectorrepet[cases]))
	distmarg<-seq(0,ceiling(max(SoloDendritas$Dist_centro[cases]*1/distradios))*distradios,by=distradios)
	nextpoint<-(-0.3)
	plot.circular(1,shrink=3,tcl.text=0.4,cex=0.7,main=paste0("Case ",Neurona),sub=paste0("Dist= ",distsegmentos,", Circ space= ",distradios),xlab="",ylab="",col="white",tcl=0.075)#representamos densidad simulada en circunf
	#representamos las partes del boxplot
	lengths<-0.9
	cuart<-quantile.circular(circular(repres,units="radians"),probs = c(0.25,0.75))
	byvalue=0.001
	if(cuart[[1]]<cuart[[2]]){angseq <- seq(cuart[[1]],cuart[[2]],byvalue)
		}else{angseq <- seq(cuart[[1]],2*pi,byvalue);angseq <- c(angseq,seq(0,cuart[[2]],byvalue))}
	lines(x=lengths*cos(angseq), y=lengths*sin(angseq), col="yellowgreen", lwd=2,lty=1)
	points(circular(c(cuart[[1]],cuart[[2]])),cex=1,bin=360,stack=FALSE,sep=0.1,shrink=1.6,col="yellowgreen",lwd=2,next.points=-0.065)
	ticks.circular(circular(seq(0,11*pi/6,pi/6)),zero=pi/2,rotation='clock', tcl=0.075)
	points(mean(repres),pch=4,cex=1,bin=360,stack=FALSE,sep=0.1,shrink=1.6,col="darkblue",lwd=2,next.points=-0.065) 
	points(median(repres),pch=4,cex=1,bin=360,stack=FALSE,sep=0.1,shrink=1.6,col="red",lwd=2,next.points=-0.065) 
	arrows.circular(mean(repres),col="darkblue",lwd=2,shrink=0.85)
	arrows.circular(median.circular(repres),col="red",lwd=2,shrink=0.85)
	for (l in 2:length(distmarg)){
		nextpoint<-nextpoint+0.3 #la primera iteracion debe sumar 0
		#dibujar circunferencia en nextpoint
		#cir<-circular(seq(1,360,by=0.01),unit="degrees")
		#points(cir,lwd=4,pch=18,cex=0.1,bin=360,stack=FALSE,sep=0.035,shrink=1,col="black",next.points=nextpoint) #añadimos los puntos reales
		draw.circle(x=0,y=0,radius=nextpoint+1)
		for (o in 1:4){
			nextpoint2<-nextpoint
			cast2<-cases & cases2[[1]]==AreasCerebro[o] & cases2[[2]]<distmarg[l] & cases2[[2]]>distmarg[l-1]
			repetit<-vectorrepet[cast2]
			repres2<-circular(rep(data[cast2], repetit))	
			if (length(repetit)>0){
				for (p in 1:max(repetit)){
					points(repres2,pch=16,cex=0.1,bin=360,stack=FALSE,sep=0.035,shrink=1,col=Colores[o],next.points=nextpoint2) #añadimos los puntos reales
					nextpoint2<-nextpoint2+0.01
					elim<-match(unique(repres2),repres2)
					repres2<-repres2[-elim]
				}
			}
		}
	}
	legend("topright",c(EtiquetasAreasCerebro,"Mean","Median","IQR"),fill=c(Colores,"darkblue","red","yellowgreen"),box.col = "transparent",ncol=1,text.width=1,x.intersp=0.2,cex=1.2)
}
