#Para construir la regresión comenzamos aislando una dendrita
#rata<-1;capa<-"II";neurona<-2;dendrita<-2#Para los ejemplos
	RectaRegDen<-function(data,datacentros,rata,capa,neurona,dendrita,colour){
		
	DendritaPrueba<-DataSinContour[DataSinContour$type=="dendrite" & DataSinContour$individual==rata & DataSinContour$layer==capa & DataSinContour$cell_id==neurona & DataSinContour$dendrite_id==dendrita,] 
	CentroDendritaPrueba<-CentrosNeur[CentrosNeur$individual==rata & CentrosNeur$layer==capa & CentrosNeur$cell_id==neurona,]
	#Solo una dendrita
	x = matrix(data=c(DendritaPrueba$x,CentroDendritaPrueba$centerXroot),nrow=length(DendritaPrueba$x)+1,ncol=1)
	y = matrix(data=c(DendritaPrueba$y,CentroDendritaPrueba$centerYroot),nrow=length(DendritaPrueba$y)+1,ncol=1)
	z = matrix(data=c(DendritaPrueba$z,CentroDendritaPrueba$centerZroot),nrow=length(DendritaPrueba$z)+1,ncol=1)
	#Representación nube de puntos
	#rgl.points(x=x,y=y,z=z,col=veccolores)
	#Desplazamos los puntos para centrar en el 0,0,0 el punto origen de la regresion:
	x0<-DendritaPrueba[1,"x"];y0<-DendritaPrueba[1,"y"];z0<-DendritaPrueba[1,"z"]
	DendritaPrueba$x<-DendritaPrueba$x-x0;DendritaPrueba$y<-DendritaPrueba$y-y0;DendritaPrueba$z<-DendritaPrueba$z-z0
	xdesp<-DendritaPrueba$x
	ydesp<-DendritaPrueba$y
	zdesp<-DendritaPrueba$z
	veccolores<-rep(colour,length(DendritaPrueba$x))
	#rgl.points(x=xdesp,y=ydesp,z=zdesp,col=veccolores)
	#par3d(windowRect=c(0,22,1920,1060)) #Para que se vea grande
	##Trazamos una recta para representar el desplazamiento
	#segments3d(c(x0,0),c(y0,0),c(z0,0),col="blue") #la recta se traza por coordenadas (x,y,z) a pares
	#axes3d() #Que dibuje los ejes
	#title3d(main=paste0("Rata",rata,"Capa",capa,"Neurona",neurona,""),xlab="x",ylab="y",zlab="z") #Que introduzca titulos

	#Para calcular la recta:
	for (i in 1:nrow(DendritaPrueba)){
		DendritaPrueba[i,"Norma"]<-sqrt((DendritaPrueba[i,"x"]^2)+(DendritaPrueba[i,"y"]^2)+(DendritaPrueba[i,"z"]^2))
	}
	alfa<-sum(DendritaPrueba$x)/sum(DendritaPrueba$Norma)
	betta<-sum(DendritaPrueba$y)/sum(DendritaPrueba$Norma)
	gamma<-sum(DendritaPrueba$z)/sum(DendritaPrueba$Norma)
	#Calculamos un punto cualquiera de la recta para extenderla y dibujarla
	lambda<-200
	xrecta<-lambda*alfa;yrecta<-lambda*betta;zrecta<-lambda*gamma
	#segments3d(c(0,xrecta),c(0,yrecta),c(0,zrecta),col="red") #la recta se traza por coordenadas (x,y,z) a pares
	return(list(xdesp,ydesp,zdesp,veccolores,xrecta,yrecta,zrecta,alfa,betta,gamma))
}

###REGRESION DE LOS PLANOS
##Plano de regresion sobre Y (omitimos el interpect ya que estamos forzando a empezar en el (0,0,0)
#rectaregresiony<-lm( DendritaPrueba$y ~ 0 + DendritaPrueba$x+DendritaPrueba$z,data=DendritaPrueba)
#summary(rectaregresiony)
#coefsy<-rectaregresiony$coefficients;alphay<-coefsy[1];betay<-coefsy[2]
#planoy<-matrix(data=,nrow=7480,ncol=3)
#colnames(planoy)<-c("y","x","z")
#ind<-1
#for (i in seq(from=min(DendritaPrueba$x),to=max(DendritaPrueba$x),b=1)){
	#for (j in seq(from=min(DendritaPrueba$z),to=max(DendritaPrueba$z),by=1)){
		#planoy[ind,"y"]<-alphay*i+betay*j
		#planoy[ind,"x"]<-i
		#planoy[ind,"z"]<-j
		#ind<-ind+1
	#}
#}
#regcolory<-rep("green",ind-1)
#rgl.points(x=planoy[,"x"],y=planoy[,"y"],z=planoy[,"z"],col=regcolory)
#
##Plano de regresion sobre X (omitimos el interpect ya que estamos forzando a empezar en el (0,0,0)
#rectaregresionx<-lm( DendritaPrueba$x ~ 0 + DendritaPrueba$y+DendritaPrueba$z,data=DendritaPrueba)
#summary(rectaregresionx)
#coefsx<-rectaregresionx$coefficients;alphax<-coefsx[1];betax<-coefsx[2]
#planox<-matrix(data=,nrow=1000000,ncol=3)
#colnames(planox)<-c("x","y","z")
#ind<-1
#for (i in seq(from=min(DendritaPrueba$y),to=max(DendritaPrueba$y),b=1)){
	#for (j in seq(from=min(DendritaPrueba$z),to=max(DendritaPrueba$z),by=1)){
		#planox[ind,"x"]<-alphax*i+betax*j
		#planox[ind,"y"]<-i
		#planox[ind,"z"]<-j
		#ind<-ind+1
	#}
#}
#regcolorx<-rep("pink",ind-1)
#rgl.points(x=planox[,"x"],y=planox[,"y"],z=planox[,"z"],col=regcolorx)
#
##Plano de regresion sobre Z (omitimos el interpect ya que estamos forzando a empezar en el (0,0,0)
#rectaregresionz<-lm( DendritaPrueba$z ~ 0 + DendritaPrueba$x+DendritaPrueba$y,data=DendritaPrueba)
#summary(rectaregresionz)
#coefsz<-rectaregresionz$coefficients;alphaz<-coefsz[1];betaz<-coefsz[2]
#planoz<-matrix(data=,nrow=17340,ncol=3)
#colnames(planoz)<-c("z","x","y")
#ind<-1
#for (i in seq(from=min(DendritaPrueba$x),to=max(DendritaPrueba$x),b=1)){
	#for (j in seq(from=min(DendritaPrueba$y),to=max(DendritaPrueba$y),by=1)){
		#planoz[ind,"z"]<-alphaz*i+betaz*j
		#planoz[ind,"x"]<-i
		#planoz[ind,"y"]<-j
		#ind<-ind+1
	#}
#}
#regcolorz<-rep("yellow",ind-1)
#rgl.points(x=planoz[,"x"],y=planoz[,"y"],z=planoz[,"z"],col=regcolorz)



##Neurona completa sin contorno soma
#DendritaCompletaPrueba<-DataSinContour[DataSinContour$type=="dendrite" & DataSinContour$individual==rata & DataSinContour$layer==capa & DataSinContour$cell_ID==neurona,] 
#NodosRaizDendritaPrueba<-Solonodosraiz[Solonodosraiz$type=="dendrite" & Solonodosraiz$individual==rata & Solonodosraiz$layer==capa & Solonodosraiz$cell_ID==neurona,] 
#x = matrix(data=c(DendritaCompletaPrueba$x,CentroDendritaPrueba$centerXroot),nrow=length(DendritaCompletaPrueba$x)+1,ncol=1)
#y = matrix(data=c(DendritaCompletaPrueba$y,CentroDendritaPrueba$centerYroot),nrow=length(DendritaCompletaPrueba$y)+1,ncol=1)
#z = matrix(data=c(DendritaCompletaPrueba$z,CentroDendritaPrueba$centerZroot),nrow=length(DendritaCompletaPrueba$z)+1,ncol=1)
#veccolores<-c(rep("white",length(x)-1),"red")
##Representación nube de puntos
#rgl.points(x=x,y=y,z=z,col=veccolores)
#axes3d()
#title3d(main="Prueba",xlab="x",ylab="y",zlab="z")
###Dibujar un segmento, a partir de 6 puntos (x,y,z), x=(x1,x2) y=(y1,y2) z=(z1,z2)
#for (i in NodosRaizDendritaPrueba$dendrite.ID){
	#segments3d(c(CentroDendritaPrueba$centerXroot,NodosRaizDendritaPrueba[NodosRaizDendritaPrueba$dendrite.ID==i,"x"]),c(CentroDendritaPrueba$centerYroot,NodosRaizDendritaPrueba[NodosRaizDendritaPrueba$dendrite.ID==i,"y"]),c(CentroDendritaPrueba$centerZroot,NodosRaizDendritaPrueba[NodosRaizDendritaPrueba$dendrite.ID==i,"z"]),col="blue")
#}
#
