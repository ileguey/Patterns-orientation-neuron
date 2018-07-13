#data<-circular(SoloDendritas$angleR)
#cases<-SoloDendritas$ID==CentrosNeur$ID[i] & SoloDendritas$segment_length<10
#cases2<-list(SoloDendritas$orientacion,SoloDendritas$Dist_centro)
#distsegmentos<-0.5 #0.25 #Distancia entre las q dividir los segmentos
#vectorrepet<-(ceiling((SoloDendritas$segment_length)*(1/distsegmentos))/(1/distsegmentos))/distsegmentos #Vector con el numero de segmentos que dividen el segmento original
#distradios<-25 #Distancia entre las q dividir las distancias al centro de los puntos
#Neurona<-CentrosNeur$ID[i]
#Colores<-c("blue","blueviolet","darkgreen","darkorange")
#EtiquetasAreasCerebro<-c("Posterior","Anterior","Ventral","Dorsal")
#AreasCerebro<-c("P","A","V","D")


PointsCirclesGraph <- function(data,distsegmentos,vectorrepet,cases,cases2,Colores,AreasCerebro,EtiquetasAreasCerebro,Neurona,lengths=TRUE,axess=FALSE){
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
		plot(data[cases],shrink=1.25,main=paste0("Points: ",Neurona),xlab="",ylab="",col="white",axes=axess)#representamos densidad simulada en circunf
		for (o in 1:4){
			points(data[cases & cases2[[1]]==AreasCerebro[o]],pch=16,cex=0.1,bin=360,stack=TRUE,sep=0.035,shrink=1.6,col=Colores[o]) #añadimos los puntos reales
		}	
		ticks.circular(circular(seq(0,11*pi/6,pi/6)),zero=pi/2,rotation='clock', tcl=0.075)
		points(mean(data[cases]),pch=4,cex=1,bin=360,stack=TRUE,sep=0.1,shrink=1.6,col="black",lwd=4)		
		arrows.circular(mean(data[cases]),col="darkblue",lwd=2,shrink=0.9)
		legend("topright",EtiquetasAreasCerebro,fill=Colores,box.col = "transparent",ncol=1,text.width=0.4,x.intersp=0.2,cex=1.2)
	}else{
		repres<-circular(rep(data[cases],vectorrepet[cases]))
		plot(circular(1),tcl.text=0.3,axes=axess,ticks=NA,shrink=1.3,main=paste0("Case ",Neurona),sub=paste0("Length represented by ",distsegmentos),xlab="",ylab="",col="white")#representamos densidad simulada en circunf
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

ConcentricCirclesGraph <- function(data,distsegmentos,vectorrepet,cases,cases2,distradios,Colores,AreasCerebro,EtiquetasAreasCerebro,Neurona,axess=FALSE){
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
	plot.circular(1,shrink=3,tcl.text=0.4,cex=0.7,axes=axess,main=paste0("Case ",Neurona),sub=paste0("Dist= ",distsegmentos,", Circ space= ",distradios),xlab="",ylab="",col="white",tcl=0.075)#representamos densidad simulada en circunf
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


PointsCirclesGraphShadow <- function(data,distsegmentos,vectorrepet,cases,cases2,Colores,AreasCerebro,EtiquetasAreasCerebro,Neurona,lengths=TRUE,datbase,i,axess=FALSE){
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
		plot(data[cases],shrink=1.25,main=paste0("Points: ",Neurona),xlab="",ylab="",col="white",axes=axess)#representamos densidad simulada en circunf
		for (o in 1:4){
			points(data[cases & cases2[[1]]==AreasCerebro[o]],pch=16,cex=0.1,bin=360,stack=TRUE,sep=0.035,shrink=1.6,col=Colores[o]) #añadimos los puntos reales
		}	
		ticks.circular(circular(seq(0,11*pi/6,pi/6)),zero=pi/2,rotation='clock', tcl=0.075)
		points(mean(data[cases]),pch=4,cex=1,bin=360,stack=TRUE,sep=0.1,shrink=1.6,col="black",lwd=4)		
		arrows.circular(mean(data[cases]),col="darkblue",lwd=2,shrink=0.9)
		legend("topright",EtiquetasAreasCerebro,fill=Colores,box.col = "transparent",ncol=1,text.width=0.4,x.intersp=0.2,cex=1.2)
	}else{
		repres<-circular(rep(data[cases],vectorrepet[cases]))
		plot(circular(1), shrink=1.3,main=paste0("Case ",Neurona),sub=paste0("Length represented by ",distsegmentos),xlab="",ylab="",col="white",axes=axess)#representamos densidad simulada en circunf
		draw.circle(x=0,y=0,radius=1)
	if (datbase$RatioP[i]>1){#Para añadir el sombreado habría que toquetear este código:
		coloret<-"lightcyan"
		if (datbase$RatioP[i]>1.1){coloret<-"dodgerblue1"}
		if (datbase$RatioP[i]>1.2){coloret<-"dodgerblue3"}
		if (datbase$RatioP[i]>1.3){coloret<-Colores[1]}
		repres222<-circular(unique(floor((180/pi)*rep(data[cases & cases2[[1]]=="P"],vectorrepet[cases & cases2[[1]]=="P"]))))
		repres22<-circular(seq(from=min(repres222), to=max(repres222), by=1),units="degrees")
		rose.diag(repres22,col=coloret,bins=360,add=TRUE,tcl.text=0.2,shrink=1,tcl=0,prop=5,border=NA,axes=axess)
	}
	if (datbase$RatioA[i]>1){#Para añadir el sombreado habría que toquetear este código:
		coloret<-"plum1"
		if (datbase$RatioP[i]>1.1){coloret<-"darkorchid1"}
		if (datbase$RatioP[i]>1.2){coloret<-"darkorchid3"}
		if (datbase$RatioP[i]>1.3){coloret<-Colores[2]}
		repres222<-circular(unique(floor((180/pi)*rep(data[cases & cases2[[1]]=="A"],vectorrepet[cases & cases2[[1]]=="A"]))))
		repres22<-circular(seq(from=min(repres222), to=max(repres222), by=1),units="degrees")
		rose.diag(repres22,col=coloret,bins=360,add=TRUE,tcl.text=0.2,shrink=1,tcl=0,prop=5,border=NA,axes=axess)
	}
	if (datbase$RatioV[i]>1){#Para añadir el sombreado habría que toquetear este código:
		coloret<-"lightgreen"
		if (datbase$RatioP[i]>1.1){coloret<-"limegreen"}
		if (datbase$RatioP[i]>1.2){coloret<-"forestgreen"}
		if (datbase$RatioP[i]>1.3){coloret<-Colores[3]}
		repres222<-circular(unique(floor((180/pi)*rep(data[cases & cases2[[1]]=="V"],vectorrepet[cases & cases2[[1]]=="V"]))))
		repres22<-circular(seq(from=min(repres222), to=max(repres222), by=1),units="degrees")
		rose.diag(repres22,col=coloret,bins=360,add=TRUE,tcl.text=0.2,shrink=1,tcl=0,prop=5,border=NA,axes=axess)
	}
	if (datbase$RatioD[i]>1){#Para añadir el sombreado habría que toquetear este código:
		coloret<-"antiquewhite"
		if (datbase$RatioP[i]>1.1){coloret<-"orange"}
		if (datbase$RatioP[i]>1.2){coloret<-"orange1"}
		if (datbase$RatioP[i]>1.3){coloret<-Colores[4]}
		repres222<-circular(unique(floor((180/pi)*rep(data[cases & cases2[[1]]=="D"],vectorrepet[cases & cases2[[1]]=="D"]))))
		repres22<-circular(seq(from=min(repres222), to=max(repres222), by=1),units="degrees")
		rose.diag(repres22,col=coloret,bins=360,add=TRUE,tcl.text=0.2,shrink=1,tcl=0,prop=5,border=NA,axes=axess)
	}
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
##Ejemplo de uso: 
#i=1
#data<-circular(SoloDendritas$angleR)
#cases<-SoloDendritas$ID==CentrosNeur$ID[i] & SoloDendritas$angleR<10
#cases2<-list(SoloDendritas$orientacion,SoloDendritas$Dist_centro)
#distsegmentos<-0.5 #0.25 #Distancia entre las q dividir los segmentos
#vectorrepet<-(ceiling((SoloDendritas$segment_length)*(1/distsegmentos))/(1/distsegmentos))/distsegmentos #Vector con el numero de segmentos que dividen el segmento original
#distradios<-25 #Distancia entre las q dividir las distancias al centro de los puntos
#Colores<-c("blue","blueviolet","darkgreen","darkorange")
#EtiquetasAreasCerebro<-c("Posterior","Anterior","Ventral","Dorsal")
#AreasCerebro<-c("P","A","V","D")
#lengths=FALSE
#
#ConcentricCirclesGraph(data,distsegmentos,vectorrepet,cases,cases2,distradios,Colores,AreasCerebro,EtiquetasAreasCerebro)
#x11()
#PointsCirclesGraph(data,distsegmentos,vectorrepet,cases,cases2,Colores,AreasCerebro,EtiquetasAreasCerebro,lengths)
