source(".//Art3Orientation//Art3Orientation/F - ClockwiseAngle.R")

ContornoAreas<-Contorno[1,]
ContornoAreas$cell_id<-NULL;ContornoAreas$point_id<-NULL;ContornoAreas$dendrite_id<-NULL;ContornoAreas$x<-NULL;
ContornoAreas$y<-NULL;ContornoAreas$z<-NULL;ContornoAreas$parent<-NULL;ContornoAreas$branch_id<-NULL;
ContornoAreas$descendants<-NULL;ContornoAreas$segment_length<-NULL

#Calculamos las areas de los mapas
tolx<-2000;toly<-1000 #definimos una tolerancia para las variables
Means<-matrix(,nrow=4,ncol=3)
k<-1
for (i in ratas){
	for (j in unique(Contorno[Contorno$individual==i,"layer"])){
		#Determinamos los puntos que identifican el supuesto centro de cada area del contorno
		if (i==1 || (i==5 && j=="VI") || (i==6 && j=="III") || (i==7 && j=="III") || (i==9 && j=="II") || (i==9 && j=="VI") || i==10 || i==11 || (i==13 && j=="III") || (i==14 && j=="III") || (i==14 && j=="Va") || (i==15 && j=="Va") || (i==15 && j=="Vb") || i==16 || i==17){
			P<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$x<min(Contorno[Contorno$individual==i & Contorno$layer==j,"x"])+tolx,c("x","y","z")]
			A<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$x>max(Contorno[Contorno$individual==i & Contorno$layer==j,"x"])-tolx,c("x","y","z")]
			D<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$y<min(Contorno[Contorno$individual==i & Contorno$layer==j,"y"])+toly,c("x","y","z")]
			V<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$y>max(Contorno[Contorno$individual==i & Contorno$layer==j,"y"])-toly,c("x","y","z")]
			Means[1,1]<-mean(P$x);Means[1,2]<-mean(P$y);Means[1,3]<-mean(P$z)
			Means[2,1]<-mean(A$x);Means[2,2]<-mean(A$y);Means[2,3]<-mean(A$z)
			Means[3,1]<-mean(D$x);Means[3,2]<-mean(D$y);Means[3,3]<-mean(D$z)
			Means[4,1]<-mean(V$x);Means[4,2]<-mean(V$y);Means[4,3]<-mean(V$z)
			ContornoAreas[k,"xPosterior"]<-Means[1,1];ContornoAreas[k,"yPosterior"]<-Means[1,2];ContornoAreas[k,"zPosterior"]<-Means[1,3]
			ContornoAreas[k,"xAnterior"]<-Means[2,1];ContornoAreas[k,"yAnterior"]<-Means[2,2];ContornoAreas[k,"zAnterior"]<-Means[2,3]
			ContornoAreas[k,"xDorsal"]<-Means[3,1];ContornoAreas[k,"yDorsal"]<-Means[3,2];ContornoAreas[k,"zDorsal"]<-Means[3,3]
			ContornoAreas[k,"xVentral"]<-Means[4,1];ContornoAreas[k,"yVentral"]<-Means[4,2];ContornoAreas[k,"zVentral"]<-Means[4,3]
			ContornoAreas[k,"species"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"species"])
			ContornoAreas[k,"individual"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"individual"])
			ContornoAreas[k,"layer"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"layer"])
			ContornoAreas[k,"map_id"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"map_id"])
			ContornoAreas[k,"type"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"type"])
			ContornoAreas[k,"ID"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"ID"])
			ContornoAreas[k,"represents"]<-"centro del area"
			k<-k+1
			#Dibujo de cada seleccion
			#rgl.points(P$x,P$y,0);					
			#par3d(windowRect=c(0,22,1920,1060)) #Para que se vea grande
			#rgl.points(A$x,A$y,0)
			#rgl.points(D$x,D$y,0)
			#rgl.points(V$x,V$y,0)
			#Sys.sleep(2)
			#rgl.close()
			#rgl.points(Means[,1],Means[,2]);axes3d();par3d(windowRect=c(0,22,1920,1060)) #Para que se vea grande
			#Sys.sleep(2)
			#rgl.close()
		}
		if ((i==3 && j=="Va") || (i==4 && j=="Vb") || (i==6 && j=="II") || (i==8 && j=="II") || (i==12) || (i==13 && j=="Va") || (i==13 && j=="Vb")){
			P<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$y>max(Contorno[Contorno$individual==i & Contorno$layer==j,"y"])-toly,c("x","y","z")]
			A<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$y<min(Contorno[Contorno$individual==i & Contorno$layer==j,"y"])+toly,c("x","y","z")]
			D<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$x<min(Contorno[Contorno$individual==i & Contorno$layer==j,"x"])+tolx,c("x","y","z")]
			V<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$x>max(Contorno[Contorno$individual==i & Contorno$layer==j,"x"])-tolx,c("x","y","z")]
			Means[1,1]<-mean(P$x);Means[1,2]<-mean(P$y);Means[1,3]<-mean(P$z)
			Means[2,1]<-mean(A$x);Means[2,2]<-mean(A$y);Means[2,3]<-mean(A$z)
			Means[3,1]<-mean(D$x);Means[3,2]<-mean(D$y);Means[3,3]<-mean(D$z)
			Means[4,1]<-mean(V$x);Means[4,2]<-mean(V$y);Means[4,3]<-mean(V$z)
			ContornoAreas[k,"xPosterior"]<-Means[1,1];ContornoAreas[k,"yPosterior"]<-Means[1,2];ContornoAreas[k,"zPosterior"]<-Means[1,3]
			ContornoAreas[k,"xAnterior"]<-Means[2,1];ContornoAreas[k,"yAnterior"]<-Means[2,2];ContornoAreas[k,"zAnterior"]<-Means[2,3]
			ContornoAreas[k,"xDorsal"]<-Means[3,1];ContornoAreas[k,"yDorsal"]<-Means[3,2];ContornoAreas[k,"zDorsal"]<-Means[3,3]
			ContornoAreas[k,"xVentral"]<-Means[4,1];ContornoAreas[k,"yVentral"]<-Means[4,2];ContornoAreas[k,"zVentral"]<-Means[4,3]
			ContornoAreas[k,"species"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"species"])
			ContornoAreas[k,"individual"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"individual"])
			ContornoAreas[k,"layer"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"layer"])
			ContornoAreas[k,"map_id"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"map_id"])
			ContornoAreas[k,"type"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"type"])
			ContornoAreas[k,"ID"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"ID"])
			ContornoAreas[k,"represents"]<-"límite izq del area"
			k<-k+1
		}
		if ((i==3 && j=="Vb")){
			P<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$y>max(Contorno[Contorno$individual==i & Contorno$layer==j,"y"])-toly,c("x","y","z")]
			A<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$y<min(Contorno[Contorno$individual==i & Contorno$layer==j,"y"])+toly,c("x","y","z")]
			D<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$x<min(Contorno[Contorno$individual==i & Contorno$layer==j,"x"])+tolx,c("x","y","z")]
			V<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$x>max(Contorno[Contorno$individual==i & Contorno$layer==j,"x"])-tolx,c("x","y","z")]
			Means[1,1]<-mean(P$x);Means[1,2]<-mean(P$y);Means[1,3]<-mean(P$z)
			Means[2,1]<-mean(A$x);Means[2,2]<-mean(A$y);Means[2,3]<-mean(A$z)
			Means[3,1]<-mean(D$x);Means[3,2]<-mean(D$y);Means[3,3]<-mean(D$z)
			Means[4,1]<-mean(V$x);Means[4,2]<-mean(V$y);Means[4,3]<-mean(V$z)
			ContornoAreas[k,"xPosterior"]<-Means[1,1];ContornoAreas[k,"yPosterior"]<-Means[1,2];ContornoAreas[k,"zPosterior"]<-Means[1,3]
			ContornoAreas[k,"xAnterior"]<-Means[2,1];ContornoAreas[k,"yAnterior"]<-Means[2,2];ContornoAreas[k,"zAnterior"]<-Means[2,3]
			ContornoAreas[k,"xDorsal"]<-Means[3,1];ContornoAreas[k,"yDorsal"]<-Means[3,2];ContornoAreas[k,"zDorsal"]<-Means[3,3]
			ContornoAreas[k,"xVentral"]<-Means[4,1];ContornoAreas[k,"yVentral"]<-Means[4,2];ContornoAreas[k,"zVentral"]<-Means[4,3]
			ContornoAreas[k,"species"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"species"])
			ContornoAreas[k,"individual"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"individual"])
			ContornoAreas[k,"layer"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"layer"])
			ContornoAreas[k,"map_id"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"map_id"])
			ContornoAreas[k,"type"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"type"])
			ContornoAreas[k,"ID"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"ID"])
			ContornoAreas[k,"represents"]<-"centro del area"
			k<-k+1
		}
		if ((i==6 && j=="VI") || (i==8 && j=="Va") || (i==8 && j=="VI") || (i==14 && j=="Vb") || (i==15 && j=="III")){
			D<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$y>max(Contorno[Contorno$individual==i & Contorno$layer==j,"y"])-toly,c("x","y","z")]
			V<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$y<min(Contorno[Contorno$individual==i & Contorno$layer==j,"y"])+toly,c("x","y","z")]
			P<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$x<min(Contorno[Contorno$individual==i & Contorno$layer==j,"x"])+tolx,c("x","y","z")]
			A<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$x>max(Contorno[Contorno$individual==i & Contorno$layer==j,"x"])-tolx,c("x","y","z")]
			Means[1,1]<-mean(P$x);Means[1,2]<-mean(P$y);Means[1,3]<-mean(P$z)
			Means[2,1]<-mean(A$x);Means[2,2]<-mean(A$y);Means[2,3]<-mean(A$z)
			Means[3,1]<-mean(D$x);Means[3,2]<-mean(D$y);Means[3,3]<-mean(D$z)
			Means[4,1]<-mean(V$x);Means[4,2]<-mean(V$y);Means[4,3]<-mean(V$z)
			ContornoAreas[k,"xPosterior"]<-Means[1,1];ContornoAreas[k,"yPosterior"]<-Means[1,2];ContornoAreas[k,"zPosterior"]<-Means[1,3]
			ContornoAreas[k,"xAnterior"]<-Means[2,1];ContornoAreas[k,"yAnterior"]<-Means[2,2];ContornoAreas[k,"zAnterior"]<-Means[2,3]
			ContornoAreas[k,"xDorsal"]<-Means[3,1];ContornoAreas[k,"yDorsal"]<-Means[3,2];ContornoAreas[k,"zDorsal"]<-Means[3,3]
			ContornoAreas[k,"xVentral"]<-Means[4,1];ContornoAreas[k,"yVentral"]<-Means[4,2];ContornoAreas[k,"zVentral"]<-Means[4,3]
			ContornoAreas[k,"species"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"species"])
			ContornoAreas[k,"individual"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"individual"])
			ContornoAreas[k,"layer"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"layer"])
			ContornoAreas[k,"map_id"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"map_id"])
			ContornoAreas[k,"type"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"type"])
			ContornoAreas[k,"ID"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"ID"])
			ContornoAreas[k,"represents"]<-"centro del area"
			k<-k+1
		}	
		if ((i==9 && j=="Vb") || (i==5 && j=="III") || (i==5 && j=="II")){
			D<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$y>max(Contorno[Contorno$individual==i & Contorno$layer==j,"y"])-toly,c("x","y","z")]
			V<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$y<min(Contorno[Contorno$individual==i & Contorno$layer==j,"y"])+toly,c("x","y","z")]
			P<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$x<min(Contorno[Contorno$individual==i & Contorno$layer==j,"x"])+tolx,c("x","y","z")]
			A<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$x>max(Contorno[Contorno$individual==i & Contorno$layer==j,"x"])-tolx,c("x","y","z")]
			Means[1,1]<-mean(P$x);Means[1,2]<-mean(P$y);Means[1,3]<-mean(P$z)
			Means[2,1]<-mean(A$x);Means[2,2]<-mean(A$y);Means[2,3]<-mean(A$z)
			Means[3,1]<-mean(D$x);Means[3,2]<-mean(D$y);Means[3,3]<-mean(D$z)
			Means[4,1]<-mean(V$x);Means[4,2]<-mean(V$y);Means[4,3]<-mean(V$z)
			ContornoAreas[k,"xPosterior"]<-Means[1,1];ContornoAreas[k,"yPosterior"]<-Means[1,2];ContornoAreas[k,"zPosterior"]<-Means[1,3]
			ContornoAreas[k,"xAnterior"]<-Means[2,1];ContornoAreas[k,"yAnterior"]<-Means[2,2];ContornoAreas[k,"zAnterior"]<-Means[2,3]
			ContornoAreas[k,"xDorsal"]<-Means[3,1];ContornoAreas[k,"yDorsal"]<-Means[3,2];ContornoAreas[k,"zDorsal"]<-Means[3,3]
			ContornoAreas[k,"xVentral"]<-Means[4,1];ContornoAreas[k,"yVentral"]<-Means[4,2];ContornoAreas[k,"zVentral"]<-Means[4,3]
			ContornoAreas[k,"species"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"species"])
			ContornoAreas[k,"individual"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"individual"])
			ContornoAreas[k,"layer"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"layer"])
			ContornoAreas[k,"map_id"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"map_id"])
			ContornoAreas[k,"type"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"type"])
			ContornoAreas[k,"ID"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"ID"])
			ContornoAreas[k,"represents"]<-"límite izq del area"
			k<-k+1
		}				
		if ((i==4 && j=="II") || (i==7 && j=="II") || (i==8 && j=="III") || i==18 || i==19 || i==20){
			D<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$y>max(Contorno[Contorno$individual==i & Contorno$layer==j,"y"])-toly,c("x","y","z")]
			V<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$y<min(Contorno[Contorno$individual==i & Contorno$layer==j,"y"])+toly,c("x","y","z")]
			A<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$x<min(Contorno[Contorno$individual==i & Contorno$layer==j,"x"])+tolx,c("x","y","z")]
			P<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$x>max(Contorno[Contorno$individual==i & Contorno$layer==j,"x"])-tolx,c("x","y","z")]
			Means[1,1]<-mean(P$x);Means[1,2]<-mean(P$y);Means[1,3]<-mean(P$z)
			Means[2,1]<-mean(A$x);Means[2,2]<-mean(A$y);Means[2,3]<-mean(A$z)
			Means[3,1]<-mean(D$x);Means[3,2]<-mean(D$y);Means[3,3]<-mean(D$z)
			Means[4,1]<-mean(V$x);Means[4,2]<-mean(V$y);Means[4,3]<-mean(V$z)
			ContornoAreas[k,"xPosterior"]<-Means[1,1];ContornoAreas[k,"yPosterior"]<-Means[1,2];ContornoAreas[k,"zPosterior"]<-Means[1,3]
			ContornoAreas[k,"xAnterior"]<-Means[2,1];ContornoAreas[k,"yAnterior"]<-Means[2,2];ContornoAreas[k,"zAnterior"]<-Means[2,3]
			ContornoAreas[k,"xDorsal"]<-Means[3,1];ContornoAreas[k,"yDorsal"]<-Means[3,2];ContornoAreas[k,"zDorsal"]<-Means[3,3]
			ContornoAreas[k,"xVentral"]<-Means[4,1];ContornoAreas[k,"yVentral"]<-Means[4,2];ContornoAreas[k,"zVentral"]<-Means[4,3]
			ContornoAreas[k,"species"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"species"])
			ContornoAreas[k,"individual"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"individual"])
			ContornoAreas[k,"layer"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"layer"])
			ContornoAreas[k,"map_id"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"map_id"])
			ContornoAreas[k,"type"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"type"])
			ContornoAreas[k,"ID"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"ID"])
			ContornoAreas[k,"represents"]<-"centro del area"
			k<-k+1
		}	
		if ((i==5 && j=="Va")){
			P<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$y>max(Contorno[Contorno$individual==i & Contorno$layer==j,"y"])-toly,c("x","y","z")]
			A<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$y<min(Contorno[Contorno$individual==i & Contorno$layer==j,"y"])+500,c("x","y","z")]
			D<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$x<min(Contorno[Contorno$individual==i & Contorno$layer==j,"x"])+3000,c("x","y","z")];D<-D[D$y<=D[D$x==min(D$x),"y"],c("x","y","z")]
			V<-Contorno[Contorno$individual==i & Contorno$layer==j & Contorno$x>max(Contorno[Contorno$individual==i & Contorno$layer==j,"x"])-tolx,c("x","y","z")]
			Means[1,1]<-mean(P$x);Means[1,2]<-mean(P$y);Means[1,3]<-mean(P$z)
			Means[2,1]<-mean(A$x);Means[2,2]<-mean(A$y);Means[2,3]<-mean(A$z)
			Means[3,1]<-mean(D$x);Means[3,2]<-mean(D$y);Means[3,3]<-mean(D$z)
			Means[4,1]<-mean(V$x);Means[4,2]<-mean(V$y);Means[4,3]<-mean(V$z)
			ContornoAreas[k,"xPosterior"]<-Means[1,1];ContornoAreas[k,"yPosterior"]<-Means[1,2];ContornoAreas[k,"zPosterior"]<-Means[1,3]
			ContornoAreas[k,"xAnterior"]<-Means[2,1];ContornoAreas[k,"yAnterior"]<-Means[2,2];ContornoAreas[k,"zAnterior"]<-Means[2,3]
			ContornoAreas[k,"xDorsal"]<-Means[3,1];ContornoAreas[k,"yDorsal"]<-Means[3,2];ContornoAreas[k,"zDorsal"]<-Means[3,3]
			ContornoAreas[k,"xVentral"]<-Means[4,1];ContornoAreas[k,"yVentral"]<-Means[4,2];ContornoAreas[k,"zVentral"]<-Means[4,3]
			ContornoAreas[k,"species"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"species"])
			ContornoAreas[k,"individual"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"individual"])
			ContornoAreas[k,"layer"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"layer"])
			ContornoAreas[k,"map_id"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"map_id"])
			ContornoAreas[k,"type"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"type"])
			ContornoAreas[k,"ID"]<-unique(Contorno[Contorno$individual==i & Contorno$layer==j,"ID"])
			ContornoAreas[k,"represents"]<-"P y V lim izquierdo, A y D centro"
			k<-k+1
		}
	}
}
View(`ContornoAreas`)

#Calculamos los centros de cada mapa
for (i in 1:nrow(ContornoAreas)){
	ContornoAreas[i,"map_center_x"]<-mean(c(ContornoAreas$xPosterior[i],ContornoAreas$xAnterior[i],ContornoAreas$xDorsal[i],ContornoAreas$xVentral[i]))
	ContornoAreas[i,"map_center_y"]<-mean(c(ContornoAreas$yPosterior[i],ContornoAreas$yAnterior[i],ContornoAreas$yDorsal[i],ContornoAreas$yVentral[i]))
	ContornoAreas[i,"map_center_z"]<-mean(c(ContornoAreas$zPosterior[i],ContornoAreas$zAnterior[i],ContornoAreas$zDorsal[i],ContornoAreas$zVentral[i]))
}

#Calculamos los limites de las areas mediante bisectrices para los que no estén ya
for (i in 1:nrow(ContornoAreas)){
	if (ContornoAreas$represents[i]=="límite izq del area"){ #Son los mismos puntos
		ContornoAreas[i,"Px_lim_izq"]<-ContornoAreas[i,"xPosterior"];ContornoAreas[i,"Py_lim_izq"]<-ContornoAreas[i,"yPosterior"];ContornoAreas[i,"Pz_lim_izq"]<-ContornoAreas[i,"zPosterior"]
		ContornoAreas[i,"Ax_lim_izq"]<-ContornoAreas[i,"xAnterior"];ContornoAreas[i,"Ay_lim_izq"]<-ContornoAreas[i,"yAnterior"];ContornoAreas[i,"Az_lim_izq"]<-ContornoAreas[i,"zAnterior"]
		ContornoAreas[i,"Dx_lim_izq"]<-ContornoAreas[i,"xDorsal"];ContornoAreas[i,"Dy_lim_izq"]<-ContornoAreas[i,"yDorsal"];ContornoAreas[i,"Dz_lim_izq"]<-ContornoAreas[i,"zDorsal"]
		ContornoAreas[i,"Vx_lim_izq"]<-ContornoAreas[i,"xVentral"];ContornoAreas[i,"Vy_lim_izq"]<-ContornoAreas[i,"yVentral"];ContornoAreas[i,"Vz_lim_izq"]<-ContornoAreas[i,"zVentral"]
	}
	if(ContornoAreas$represents[i]=="centro del area"){ #Hay que hacer bisectrices
		puntoslimPV<-bisectriz_punto(px1=ContornoAreas[i,"xPosterior"],py1=ContornoAreas[i,"yPosterior"],px2=ContornoAreas[i,"xVentral"],py2=ContornoAreas[i,"yVentral"],x1=ContornoAreas[i,"map_center_x"],y1=ContornoAreas[i,"map_center_y"],ID=ContornoAreas[i,"ID"])
		puntoslimPD<-bisectriz_punto(px1=ContornoAreas[i,"xPosterior"],py1=ContornoAreas[i,"yPosterior"],px2=ContornoAreas[i,"xDorsal"],py2=ContornoAreas[i,"yDorsal"],x1=ContornoAreas[i,"map_center_x"],y1=ContornoAreas[i,"map_center_y"],ID=ContornoAreas[i,"ID"])
		puntoslimAV<-bisectriz_punto(px1=ContornoAreas[i,"xAnterior"],py1=ContornoAreas[i,"yAnterior"],px2=ContornoAreas[i,"xVentral"],py2=ContornoAreas[i,"yVentral"],x1=ContornoAreas[i,"map_center_x"],y1=ContornoAreas[i,"map_center_y"],ID=ContornoAreas[i,"ID"])
		puntoslimAD<-bisectriz_punto(px1=ContornoAreas[i,"xAnterior"],py1=ContornoAreas[i,"yAnterior"],px2=ContornoAreas[i,"xDorsal"],py2=ContornoAreas[i,"yDorsal"],x1=ContornoAreas[i,"map_center_x"],y1=ContornoAreas[i,"map_center_y"],ID=ContornoAreas[i,"ID"])		
		if(puntoslimPV[[4]]=="lim izq 1"){
			ContornoAreas[i,"Px_lim_izq"]<-puntoslimPV[[1]];ContornoAreas[i,"Py_lim_izq"]<-puntoslimPV[[2]];ContornoAreas[i,"Pz_lim_izq"]<-ContornoAreas[i,"zPosterior"]
			ContornoAreas[i,"Vx_lim_izq"]<-puntoslimAV[[1]];ContornoAreas[i,"Vy_lim_izq"]<-puntoslimAV[[2]];ContornoAreas[i,"Vz_lim_izq"]<-ContornoAreas[i,"zVentral"]		
			ContornoAreas[i,"Ax_lim_izq"]<-puntoslimAD[[1]];ContornoAreas[i,"Ay_lim_izq"]<-puntoslimAD[[2]];ContornoAreas[i,"Az_lim_izq"]<-ContornoAreas[i,"zAnterior"]
			ContornoAreas[i,"Dx_lim_izq"]<-puntoslimPD[[1]];ContornoAreas[i,"Dy_lim_izq"]<-puntoslimPD[[2]];ContornoAreas[i,"Dz_lim_izq"]<-ContornoAreas[i,"zDorsal"]
		
		}else{
			ContornoAreas[i,"Px_lim_izq"]<-puntoslimPD[[1]];ContornoAreas[i,"Py_lim_izq"]<-puntoslimPD[[2]];ContornoAreas[i,"Pz_lim_izq"]<-ContornoAreas[i,"zPosterior"]
			ContornoAreas[i,"Vx_lim_izq"]<-puntoslimPV[[1]];ContornoAreas[i,"Vy_lim_izq"]<-puntoslimPV[[2]];ContornoAreas[i,"Vz_lim_izq"]<-ContornoAreas[i,"zVentral"]
			ContornoAreas[i,"Ax_lim_izq"]<-puntoslimAV[[1]];ContornoAreas[i,"Ay_lim_izq"]<-puntoslimAV[[2]];ContornoAreas[i,"Az_lim_izq"]<-ContornoAreas[i,"zAnterior"]
			ContornoAreas[i,"Dx_lim_izq"]<-puntoslimAD[[1]];ContornoAreas[i,"Dy_lim_izq"]<-puntoslimAD[[2]];ContornoAreas[i,"Dz_lim_izq"]<-ContornoAreas[i,"zDorsal"]
		}
	}
	if(ContornoAreas$represents[i]=="P y V lim izquierdo, A y D centro"){ #Son los mismos puntos para P y V, pero hay q calcular para A y D
		ContornoAreas[i,"Px_lim_izq"]<-ContornoAreas[i,"xPosterior"];ContornoAreas[i,"Py_lim_izq"]<-ContornoAreas[i,"yPosterior"];ContornoAreas[i,"Pz_lim_izq"]<-ContornoAreas[i,"zPosterior"]
		ContornoAreas[i,"Vx_lim_izq"]<-ContornoAreas[i,"xVentral"];ContornoAreas[i,"Vy_lim_izq"]<-ContornoAreas[i,"yVentral"];ContornoAreas[i,"Vz_lim_izq"]<-ContornoAreas[i,"zVentral"]
		ContornoAreas[i,"Dx_lim_izq"]<-ContornoAreas[i,"xDorsal"];ContornoAreas[i,"Dy_lim_izq"]<-ContornoAreas[i,"yDorsal"];ContornoAreas[i,"Dz_lim_izq"]<-ContornoAreas[i,"zDorsal"]
		puntoslimAD<-bisectriz_punto(px1=ContornoAreas[i,"xAnterior"],py1=ContornoAreas[i,"yAnterior"],px2=ContornoAreas[i,"xDorsal"],py2=ContornoAreas[i,"yDorsal"],x1=ContornoAreas[i,"map_center_x"],y1=ContornoAreas[i,"map_center_y"],ID=ContornoAreas[i,"ID"])		
		puntoslimAD2<-bisectriz_punto(px1=ContornoAreas[i,"xAnterior"],py1=ContornoAreas[i,"yAnterior"],px2=puntoslimAD[[1]],py2=puntoslimAD[[2]],x1=ContornoAreas[i,"map_center_x"],y1=ContornoAreas[i,"map_center_y"],ID=ContornoAreas[i,"ID"])				
		if (puntoslimAD[[4]]=="lim izq 1"){
			ContornoAreas[i,"Ax_lim_izq"]<-puntoslimAD2[[1]];ContornoAreas[i,"Ay_lim_izq"]<-puntoslimAD2[[2]];ContornoAreas[i,"Az_lim_izq"]<-ContornoAreas[i,"zAnterior"]
		}else{
			#ContornoAreas[i,"Dx_lim_izq"]<-puntoslimAD[[1]];ContornoAreas[i,"Dy_lim_izq"]<-puntoslimAD[[2]];ContornoAreas[i,"Dz_lim_izq"]<-ContornoAreas[i,"zDorsal"]
			#ContornoAreas[i,"Ax_lim_izq"]<-;ContornoAreas[i,"Ay_lim_izq"]<-;ContornoAreas[i,"Az_lim_izq"]<-ContornoAreas[i,"zAnterior"]
		}
	}
}
write.table(ContornoAreas,file=".//Art3Orientation//Art3Orientation//tablas//ContornoAreas",append=FALSE)



#View(`Puntos`)

#for (i in 1:nrow(ContornoAreas)){
	#x11()
	#plot(Contorno$x[Contorno$map_id==ContornoAreas$map_id[i]],Contorno$y[Contorno$map_id==ContornoAreas$map_id[i]],type="b",pch=20,main=paste0(ContornoAreas$ID[i]))
	#points(ContornoAreas[i,c("xPosterior","xAnterior","xDorsal","xVentral","map_center_x","Px_lim_izq","Ax_lim_izq","Dx_lim_izq","Vx_lim_izq")],ContornoAreas[i,c("yPosterior","yAnterior","yDorsal","yVentral","map_center_y","Py_lim_izq","Ay_lim_izq","Dy_lim_izq","Vy_lim_izq")],col=c("red","red","red","red","blue","green","green","green","green"),pch=c(19,19,19,19,18,19,19,19,19))
	##points(puntoslimAD2[[1]],puntoslimAD2[[2]],col=c("red","red"),pch=c(19,19))
	##abline(a=d,b=m) #Dibuja la recta con pendiente m y constante d
#}