
#Para calcular angulo entre vectores (primero alternativa en 2d)
angularMeasures2d <- function(vector,axis)
{
 #vector tendra 2 elementos x e y, así como axis (de la forma c(1,2))
dot<-axis[1]*vector[1]+axis[2]*vector[2]
det<-axis[1]*vector[2]-vector[1]*axis[2]
angle<-atan2(det,dot)
if (angle<0){angle<-angle+(2*pi)}
#angle<-(angle+(2*pi))%% (2*pi)
return(angle*180/pi)
}

###
# Para calcular angulo entre vectores con matrices
### el original
#angularMeasures2 <- function(vector,axis,inclination.axis = c(0,0,1) )
#{
  #if(is.vector(vector))
  #{
    #azimuth <- (atan2(axis[1]*vector[2] - axis[2]*vector[1], t(axis)%*%vector) + 2*pi) %% (2*pi)
    #inclination <- pi/2 - atan2(vector[1]*inclination.axis[3] - vector[3]*inclination.axis[1], t(vector)%*%inclination.axis) %% (pi)
    #return(c(azimuth,inclination))
  #}
  #else
  #{
    #azimuth <- (atan2(axis[,1]*vector[,2] - axis[,2]*vector[,1],
                      #rowMeans(axis*vector)*3)
                #+ 2*pi) %% (2*pi)
    #aux_inc <- matrix(rep(inclination.axis,nrow(vector)),nrow=nrow(vector),byrow=T)
    #inclination <- pi/2 - atan2(vector[,1]*aux_inc[,3] - vector[,3]*aux_inc[,1],
                                #rowMeans(vector * aux_inc) *3) %% (pi)
    #return(list(azimuth=azimuth,inclination=inclination))
  #}
#}

angularMeasures2 <- function(vector,axis)
{
  if(is.vector(vector))
  {
	dot<-axis[1]*vector[1]+axis[2]*vector[2]
	det<-axis[1]*vector[2]-vector[1]*axis[2]
    azimuth <- atan2(det, dot)
	if (azimuth<0){azimuth<-azimuth+(2*pi)}
    return(c(azimuth))
  }
  else
  {
	dot<-axis[,1]*vector[,1]+axis[,2]*vector[,2]
	det<-axis[,1]*vector[,2]-vector[,1]*axis[,2]
    azimuth <- atan2(det,dot)
	angle<-lapply(azimuth,function(x) {if (x<0){x<-x+(2*pi)}else{x<-x}})
    return(list(azimuth=angle))
  }
}

param_recta<- function (px1,py1,px2,py2){
	#Sacamos la recta Y=mX+d ó Y+mX+d=0 que pasa por los puntos
	m<-(py2-py1)/(px2-px1)
	d<-(-px1*(py2-py1)/(px2-px1))+py1
	params<-list(m,d)
	names(params)<-c("m","d")
	return(params)
}

bisectriz_punto<- function (px1,py1,px2,py2,x1,y1,ID){
	#Puntos medio de los extremos
	x2<-(px1+px2)/2;y2<-(py1+py2)/2
	#Sacamos la recta Y=mX+d ó mX-Y+d=0 que pasa por el centro y el punto medio
	m<-(y2-y1)/(x2-x1)
	d<-(-x1*(y2-y1)/(x2-x1))+y1
	#Medimos distancias a la recta
	if (px1==min(ContornoAreas[ContornoAreas$ID==ID,c("xPosterior","xAnterior","xDorsal","xVentral")])){#<-
		if (py2==min(ContornoAreas[ContornoAreas$ID==ID,c("yPosterior","yAnterior","yDorsal","yVentral")])){ #sur
		 	Puntos<-Contorno[Contorno$ID==ID & (Contorno$x<px2 & Contorno$y<py1),]
			represent<-paste0("lim izq 2")#variable de px2)
		}else{
		 	Puntos<-Contorno[Contorno$ID==ID & (Contorno$x<px2 & Contorno$y>py1),]
			represent<-paste0("lim izq 1")#variable de py1)
		}	
	}	
	if (px1==max(ContornoAreas[ContornoAreas$ID==ID,c("xPosterior","xAnterior","xDorsal","xVentral")])){#->
		if (py2==min(ContornoAreas[ContornoAreas$ID==ID,c("yPosterior","yAnterior","yDorsal","yVentral")])){ #sur
		 	Puntos<-Contorno[Contorno$ID==ID & (Contorno$x>px2 & Contorno$y<py1),]
			represent<-paste0("lim izq 1")#variable de py1)
		}else{
		 	Puntos<-Contorno[Contorno$ID==ID & (Contorno$x>px2 & Contorno$y>py1),]
			represent<-paste0("lim izq 2")#variable de px2)
		}	
	}
	if (py1==min(ContornoAreas[ContornoAreas$ID==ID,c("yPosterior","yAnterior","yDorsal","yVentral")])){# punto Sur
		if (px2==min(ContornoAreas[ContornoAreas$ID==ID,c("xPosterior","xAnterior","xDorsal","xVentral")])){ #sur
		 	Puntos<-Contorno[Contorno$ID==ID & (Contorno$x<px1 & Contorno$y<py2),]
			represent<-paste0("lim izq 1")#variable de px1)
		}else{
		 	Puntos<-Contorno[Contorno$ID==ID & (Contorno$x>px1 & Contorno$y<py2),]
			represent<-paste0("lim izq 2")#variable de py2)
		}	
	}
	if (py1==max(ContornoAreas[ContornoAreas$ID==ID,c("yPosterior","yAnterior","yDorsal","yVentral")])){# punto Norte
		if (px2==min(ContornoAreas[ContornoAreas$ID==ID,c("xPosterior","xAnterior","xDorsal","xVentral")])){ #sur
		 	Puntos<-Contorno[Contorno$ID==ID & (Contorno$x<px1 & Contorno$y>py2),]
			represent<-paste0("lim izq 2")#variable de py2)
		}else{
		 	Puntos<-Contorno[Contorno$ID==ID & (Contorno$x>px1 & Contorno$y>py2),]
			represent<-paste0("lim izq 1")#variable de px1)
		}	
	}
	if (ID=="5VaNA"){Puntos<-Contorno[Contorno$ID==ID & (Contorno$x<px1 & Contorno$y<py2),];represent<-paste0("lim izq 1")}
	for (j in 1:nrow(Puntos)){
		p1<-Puntos$x[j]
		p2<-Puntos$y[j]
		Puntos[j,"distP_Rect"]<-abs(m*p1+(-1)*p2+d)/sqrt(m^2+(-1)^2)
	}
	distP_Rectlim<-min(Puntos$distP_Rect)
	plimx<-unique(Puntos$x[Puntos$distP_Rect==distP_Rectlim]);plimy<-unique(Puntos$y[Puntos$distP_Rect==distP_Rectlim])
	return(list(plimx,plimy,distP_Rectlim,represent))
}