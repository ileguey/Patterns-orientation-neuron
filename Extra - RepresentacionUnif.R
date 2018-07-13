#Generamos una uniforme perfecta, con por ejemplo 4 datos de cada grado de 1 a 360
Unif<-seq(pi/180,2*pi,by=pi/180)
#Unif<-seq(1,360,by=1)
Unif4<-c(Unif, Unif, Unif, Unif)
#View(`Unif4`)
Unif4C<-circular(Unif4,type="angles",units="radians")
#Unif4C<-circular(Unif4,type="angles",units="degrees")

		repres<-Unif4C
		plot(circular(1),tcl.text=0.3,shrink=1,main=paste0("Case ",Neurona),sub=paste0("Length represented by ",distsegmentos),xlab="",ylab="",col="white")#representamos densidad simulada en circunf
		draw.circle(x=0,y=0,radius=1)
		points(repres,pch=16,cex=0.1,bin=360,stack=TRUE,sep=0.035,shrink=1.6,) #añadimos los puntos reales	
	#representamos las partes del boxplot
		lengths<-0.9
		byvalue=0.001
		cuart<-quantile.circular(circular(repres,units="radians"),probs = c(0.25,0.75))
		#cuart<-quantile.circular(circular(repres,units="degrees"),probs = c(0.25,0.75))
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
		#legend("left",c("Mean","Median","IQR"),fill=c("darkblue","red","ye