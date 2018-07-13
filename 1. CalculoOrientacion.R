#Leemos tablas
TodosMapas<-read.table(file=".//Art3Orientation//tablas//TodosMapas")#Todo de los mapas
Contorno<-read.table(file=".//Art3Orientation//tablas//Contorno") #Contorno
DataSinContour<-read.table(file=".//Art3Orientation//tablas//DatosSinContorno2") #Soma+dendritas
Solonodosraiz<-read.table(file=".//Art3Orientation//tablas//NodosRaiz") #Puntos raiz dendriticos
CentrosNeur<-read.table(file=".//Art3Orientation//tablas//CentrosNeuronas") #Centros de las neuronas
SoloDendritas<-read.table(file=".//Art3Orientation//tablas//SoloDendritas") #Centros de las neuronas
ContornoAreas<-read.table(file=".//Art3Orientation//tablas//ContornoAreas")
NeurOrient<-read.table(file=".//Art3Orientation//tablas//NeuronOrientation")
MapOrient<-read.table(file=".//Art3Orientation//tablas//MapOrientation")

#View(`TodosMapas`);View(`Contorno`);View(`ContornoAreas`);View(`DataSinContour`);View(`Solonodosraiz`)
#View(`SoloDendritas`);View(`CentrosNeur`);View(`NeurOrient`);View(`MapOrient`)

#Librerías usadas
library(rgl) #install.packages("rgl")
library(circular) #install.packages("circular")
library(reshape) #install.packages("reshape")
library(plotrix) #install.packages("plotrix")
library(grid) #install.packages("grid")
library(plyr) #install.packages("plyr")
library(xlsx) #install.packages("xlsx")
library(matrixStats) #install.packages("matrixStats")

#Editamos directorio de trabajo
setwd("C:/Users/Ileguey/Documents/Revolution")

#Funciones o scripts adjuntos
source(".//Art3Orientation//Extra - RectaRegresionDendrita.R")
source(".//Art3Orientation//F - ClockwiseAngle.R")
source(".//Art3Orientation//F - GraphicFunctions.R")

#Variables que vamos a necesitar
ratas<-sort(unique(DataSinContour$individual))
capas<-sort(unique(DataSinContour$layer))

#Para generar imagenes de todas las regresiones y guardar los parametros de cada recta:
#source("C:/Users/Ileguey/Documents/Revolution/Art3Orientation/Gráficos3D.R")

#Calculamos la orientación y dibujamos las neuronas con un color por cad orientación
#source("C:/Users/Ileguey/Documents/Revolution/Art3Orientation/CalculoOrientacion.R")


#Creamos la variable dist.anterior para calcular la longitud de cada dendrita
for (i in 249505:nrow(DataSinContour)){
	if (is.na(DataSinContour[i,"parent"])){
		DataSinContour[i,"DistParent"]<-0
	}else{
	    #Parent<-DataSinContour[DataSinContour$individual==DataSinContour[i,"individual"] & DataSinContour$layer==DataSinContour[i,"layer"] & DataSinContour$point_id==DataSinContour[i,"parent"],c()
		DataSinContour[i,"DistParent"]<-sqrt(((DataSinContour[i,"x"]-DataSinContour[DataSinContour$individual==DataSinContour[i,"individual"] & DataSinContour$layer==DataSinContour[i,"layer"] & DataSinContour$point_id==DataSinContour[i,"parent"],"x"])^2)+((DataSinContour[i,"y"]-DataSinContour[DataSinContour$individual==DataSinContour[i,"individual"] & DataSinContour$layer==DataSinContour[i,"layer"] & DataSinContour$point_id==DataSinContour[i,"parent"],"y"])^2)+((DataSinContour[i,"z"]-DataSinContour[DataSinContour$individual==DataSinContour[i,"individual"] & DataSinContour$layer==DataSinContour[i,"layer"] & DataSinContour$point_id==DataSinContour[i,"parent"],"z"])^2))
	}
}

#Añadimos la longitud total de cada dendrita
for (i in 1:nrow(Solonodosraiz)){
	Solonodosraiz[i,"dendrite_length"]<-sum(DataSinContour[DataSinContour$type=="dendrite" & DataSinContour$map_id==Solonodosraiz[i,"map_id"] & DataSinContour$cell_id==Solonodosraiz[i,"cell_id"] & DataSinContour$dendrite_id==Solonodosraiz[i,"dendrite_id"],"segment_length"])
}

#Comenzamos a unir cada límite del contorno con los centros de las neuronas
for (i in 1:nrow(ContornoAreas)){
	for (j in CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i],"cell_id"]){
		#Calculamos las pendientes (m) y terminos indep(d) de las rectas y+mx+d=0
		centro_P<-param_recta(ContornoAreas$Px_lim_izq[i],ContornoAreas$Py_lim_izq[i],CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"])
		centro_A<-param_recta(ContornoAreas$Ax_lim_izq[i],ContornoAreas$Ay_lim_izq[i],CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"])
		centro_V<-param_recta(ContornoAreas$Vx_lim_izq[i],ContornoAreas$Vy_lim_izq[i],CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"])
		centro_D<-param_recta(ContornoAreas$Dx_lim_izq[i],ContornoAreas$Dy_lim_izq[i],CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"])

		CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"mP"]<-centro_P$m
		CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"dP"]<-centro_P$d
		CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"mA"]<-centro_A$m
		CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"dA"]<-centro_A$d		
		CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"mD"]<-centro_D$m
		CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"dD"]<-centro_D$d
		CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"mV"]<-centro_V$m
		CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"dV"]<-centro_V$d
	}
}

#Obtenemos angulo de las areas
for (i in 1:nrow(CentrosNeur)){
	CentrosNeur[i,"alphaP-D"]<-angularMeasures2d(vector=c(ContornoAreas$Dx_lim_izq[ContornoAreas$individual==CentrosNeur$individual[i] & ContornoAreas$layer==CentrosNeur$layer[i]]-CentrosNeur$centerXroot[i],ContornoAreas$Dy_lim_izq[ContornoAreas$individual==CentrosNeur$individual[i] & ContornoAreas$layer==CentrosNeur$layer[i]]-CentrosNeur$centerYroot[i],0),axis=c(ContornoAreas$Px_lim_izq[ContornoAreas$individual==CentrosNeur$individual[i] & ContornoAreas$layer==CentrosNeur$layer[i]]-CentrosNeur$centerXroot[i],ContornoAreas$Py_lim_izq[ContornoAreas$individual==CentrosNeur$individual[i] & ContornoAreas$layer==CentrosNeur$layer[i]]-CentrosNeur$centerYroot[i],0))
	CentrosNeur[i,"alphaP-A"]<-angularMeasures2d(vector=c(ContornoAreas$Ax_lim_izq[ContornoAreas$individual==CentrosNeur$individual[i] & ContornoAreas$layer==CentrosNeur$layer[i]]-CentrosNeur$centerXroot[i],ContornoAreas$Ay_lim_izq[ContornoAreas$individual==CentrosNeur$individual[i] & ContornoAreas$layer==CentrosNeur$layer[i]]-CentrosNeur$centerYroot[i],0),axis=c(ContornoAreas$Px_lim_izq[ContornoAreas$individual==CentrosNeur$individual[i] & ContornoAreas$layer==CentrosNeur$layer[i]]-CentrosNeur$centerXroot[i],ContornoAreas$Py_lim_izq[ContornoAreas$individual==CentrosNeur$individual[i] & ContornoAreas$layer==CentrosNeur$layer[i]]-CentrosNeur$centerYroot[i],0))
	CentrosNeur[i,"alphaP-V"]<-angularMeasures2d(vector=c(ContornoAreas$Vx_lim_izq[ContornoAreas$individual==CentrosNeur$individual[i] & ContornoAreas$layer==CentrosNeur$layer[i]]-CentrosNeur$centerXroot[i],ContornoAreas$Vy_lim_izq[ContornoAreas$individual==CentrosNeur$individual[i] & ContornoAreas$layer==CentrosNeur$layer[i]]-CentrosNeur$centerYroot[i],0),axis=c(ContornoAreas$Px_lim_izq[ContornoAreas$individual==CentrosNeur$individual[i] & ContornoAreas$layer==CentrosNeur$layer[i]]-CentrosNeur$centerXroot[i],ContornoAreas$Py_lim_izq[ContornoAreas$individual==CentrosNeur$individual[i] & ContornoAreas$layer==CentrosNeur$layer[i]]-CentrosNeur$centerYroot[i],0))
}
n <- nrow(DataSinContour)
tabaux<-merge(DataSinContour[,c("individual", "layer","cell_id","ID", "x","y")],CentrosNeur[,c("individual", "layer","cell_id","ID", "centerXroot","centerYroot","alphaP.D","alphaP.A","alphaP.V")],by="ID")
tabaux2<-merge(DataSinContour[,c("individual", "layer","cell_id", "x","y")],ContornoAreas[,c("individual", "layer", "Px_lim_izq","Py_lim_izq")],by=c("individual","layer"))
tabaux<-tabaux[order(tabaux$individual.x,tabaux$layer.x,tabaux$cell_id.x),]
tabaux2<-tabaux2[order(tabaux2$individual,tabaux2$layer,tabaux2$cell_id),]
#View(`tabaux`);View(`tabaux2`)
distcentr<-sqrt(((tabaux$x-tabaux$centerXroot)^2)+((tabaux$y-tabaux$centerYroot)^2))
aux_az <-as.numeric(angularMeasures2(vector=matrix(c(tabaux$x-tabaux$centerXroot, tabaux$y-tabaux$centerYroot, rep(0,n)) ,nrow=n,ncol=3),
									 axis=matrix(c(tabaux2$Px_lim_izq-tabaux$centerXroot,tabaux2$Py_lim_izq-tabaux$centerYroot,rep(0,n)),ncol=3,nrow=n)
									)$azimuth)
aux_alpha2	<- cbind(tabaux[,],aux_az,aux_az*180/pi)											
aux_alpha <- cbind(tabaux[,c("alphaP.D","alphaP.A","alphaP.V")],aux_az*180/pi,aux_az)
#View(`aux_alpha`)
  orientacion <- apply(aux_alpha,1,function(x){
    if (x[4]<=min(x[1:3])){
      return("P")
    }else if (x[4]>max(x[1:3])){
      if (max(x[1:3])==x[3]){
        return("V")
      }else{
        return("D")
      }
    }else if (x[4]>x[2]){
      return("A")
    }else{
      if (max(x[1:3])==x[3]){
        return("D")
      }else{
        return("V")
      }
    }
  })
aux_alpha2<-cbind(aux_alpha2,orientacion)
DataSinContour<-cbind(DataSinContour,distcentr)
DataSinContour<-rename(DataSinContour,c(distcentr="Dist_centro"))
DataSinContour<-cbind(DataSinContour,aux_az,orientacion)
DataSinContour<-rename(DataSinContour,c(aux_az="angleR"))
DataSinContour[,"angleGrad"]<-DataSinContour$angleR*180/pi
CentrosNeur<-na.omit(CentrosNeur)
#Situamos las longitudes de cada area
for (i in 1:nrow(na.omit(CentrosNeur))){
	print(i)
	CentrosNeur[i,"total_dendrites_length"]<-sum(DataSinContour$segment_length[DataSinContour$ID==CentrosNeur$ID[i] & DataSinContour$type=="dendrite" & DataSinContour$cell_id==CentrosNeur$cell_id[i]])
	CentrosNeur[i,"P"]<-sum(DataSinContour$segment_length[DataSinContour$ID==CentrosNeur$ID[i] & DataSinContour$type=="dendrite" & DataSinContour$cell_id==CentrosNeur$cell_id[i] & DataSinContour$orientacion=="P"])
	CentrosNeur[i,"V"]<-sum(DataSinContour$segment_length[DataSinContour$ID==CentrosNeur$ID[i] & DataSinContour$type=="dendrite" & DataSinContour$orientacion=="V"])
	CentrosNeur[i,"A"]<-sum(DataSinContour$segment_length[DataSinContour$ID==CentrosNeur$ID[i] & DataSinContour$type=="dendrite" & DataSinContour$orientacion=="A"])
	CentrosNeur[i,"D"]<-sum(DataSinContour$segment_length[DataSinContour$ID==CentrosNeur$ID[i] & DataSinContour$type=="dendrite" & DataSinContour$orientacion=="D"])
}
CentrosNeur[,"%P"]<-CentrosNeur[,"P"]/CentrosNeur[,"total_dendrites_length"]
CentrosNeur[,"%V"]<-CentrosNeur[,"V"]/CentrosNeur[,"total_dendrites_length"]
CentrosNeur[,"%A"]<-CentrosNeur[,"A"]/CentrosNeur[,"total_dendrites_length"]
CentrosNeur[,"%D"]<-CentrosNeur[,"D"]/CentrosNeur[,"total_dendrites_length"]
	
for (i in 1:nrow(ContornoAreas)){
	print(i)
	ContornoAreas[i,"total_neurons_length"]<-sum(DataSinContour$segment_length[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$type=="dendrite"])
	ContornoAreas[i,"P"]<-sum(DataSinContour$segment_length[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$type=="dendrite" & DataSinContour$orientacion=="P"])
	ContornoAreas[i,"V"]<-sum(DataSinContour$segment_length[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$type=="dendrite" & DataSinContour$orientacion=="V"])
	ContornoAreas[i,"A"]<-sum(DataSinContour$segment_length[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$type=="dendrite" & DataSinContour$orientacion=="A"])
	ContornoAreas[i,"D"]<-sum(DataSinContour$segment_length[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$type=="dendrite" & DataSinContour$orientacion=="D"])
}
ContornoAreas[,"%P"]<-ContornoAreas[,"P"]/ContornoAreas[,"total_neurons_length"]
ContornoAreas[,"%V"]<-ContornoAreas[,"V"]/ContornoAreas[,"total_neurons_length"]
ContornoAreas[,"%A"]<-ContornoAreas[,"A"]/ContornoAreas[,"total_neurons_length"]
ContornoAreas[,"%D"]<-ContornoAreas[,"D"]/ContornoAreas[,"total_neurons_length"]

################## REPRESENTACIÓN GRÁFICA ##################

#Vamos a dibujar los mapita para ver como queda
for (i in 1:nrow(ContornoAreas)){
	#x11()
	#postscript(file=paste0(".//Art3Orientation//Graficos//MapLines_",ContornoAreas$map_id[i],".eps"), onefile=TRUE, horizontal=FALSE, width = 14,height = 7, pointsize = 12)
	#jpeg(file=paste0(".//Art3Orientation//Graficos//MapLines_",ContornoAreas$map_id[i],".jpg"),units="in",res=600,width = 14,height = 7, pointsize = 12)
	plot(Contorno$x[Contorno$map_id==ContornoAreas$map_id[i]],Contorno$y[Contorno$map_id==ContornoAreas$map_id[i]],type="b",pch=20,main=paste0(ContornoAreas$ID[i]))
	points(ContornoAreas[i,c("map_center_x","Px_lim_izq","Ax_lim_izq","Dx_lim_izq","Vx_lim_izq")],ContornoAreas[i,c("map_center_y","Py_lim_izq","Ay_lim_izq","Dy_lim_izq","Vy_lim_izq")],col=c("blue","green","green","green","green"),pch=c(18,19,19,19,19))
	points(DataSinContour$x[DataSinContour$map_id==ContornoAreas$map_id[i]],DataSinContour$y[DataSinContour$map_id==ContornoAreas$map_id[i]],col=c("red"),pch=c(19))
	points(ContornoAreas[i,c("Px_lim_izq","Ax_lim_izq","Dx_lim_izq","Vx_lim_izq")]+c(1000,1000,4000,-4000),ContornoAreas[i,c("Py_lim_izq","Ay_lim_izq","Dy_lim_izq","Vy_lim_izq")]+c(-3000,2000,0,-1000),col=c("blue","blueviolet","darkorange","darkgreen"),pch=c("P","A","D","V"),cex=2)
	#plot(DataSinContour$x[DataSinContour$map_id==ContornoAreas$map_id[i]],DataSinContour$y[DataSinContour$map_id==ContornoAreas$map_id[i]],col=c("red"),pch=c(19))
	for (j in CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i],"cell_id"]){
		lines(c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],ContornoAreas$Px_lim_izq[i]),c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"],ContornoAreas$Py_lim_izq[i]),col="green")
		lines(c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],ContornoAreas$Ax_lim_izq[i]),c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"],ContornoAreas$Ay_lim_izq[i]),col="green")
		lines(c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],ContornoAreas$Dx_lim_izq[i]),c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"],ContornoAreas$Dy_lim_izq[i]),col="green")
		lines(c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],ContornoAreas$Vx_lim_izq[i]),c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"],ContornoAreas$Vy_lim_izq[i]),col="green")
		points(DataSinContour$x[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="P"],DataSinContour$y[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="P"],col=c("blue"),pch=c(19))
		points(DataSinContour$x[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="D"],DataSinContour$y[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="D"],col=c("darkorange"),pch=c(19))
		points(DataSinContour$x[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="A"],DataSinContour$y[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="A"],col=c("blueviolet"),pch=c(19))
		points(DataSinContour$x[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="V"],DataSinContour$y[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="V"],col=c("darkgreen"),pch=c(19))
	}
dev.off()
}
axis(1,pos=0);axis(2,pos=0)
#Si nos centramos en dibujar solamente las neuronas
for (i in (nrow(ContornoAreas)-1):nrow(ContornoAreas)){
	for (j in CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i],"cell_id"]){
		plot(DataSinContour$x[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j],DataSinContour$y[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j],col=c("red"),pch=c(19))
		lines(c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],ContornoAreas$Px_lim_izq[i]),c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"],ContornoAreas$Py_lim_izq[i]),col="green")
		lines(c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],ContornoAreas$Ax_lim_izq[i]),c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"],ContornoAreas$Ay_lim_izq[i]),col="green")
		lines(c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],ContornoAreas$Dx_lim_izq[i]),c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"],ContornoAreas$Dy_lim_izq[i]),col="green")
		lines(c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerXroot"],ContornoAreas$Vx_lim_izq[i]),c(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$cell_id==j,"centerYroot"],ContornoAreas$Vy_lim_izq[i]),col="green")
		points(DataSinContour$x[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="P"],DataSinContour$y[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="P"],col=c("blue"),pch=c(19))
		points(DataSinContour$x[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="D"],DataSinContour$y[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="D"],col=c("darkorange"),pch=c(19))
		points(DataSinContour$x[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="A"],DataSinContour$y[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="A"],col=c("blueviolet"),pch=c(19))
		points(DataSinContour$x[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="V"],DataSinContour$y[DataSinContour$map_id==ContornoAreas$map_id[i] & DataSinContour$cell_id==j & DataSinContour$orientacion=="V"],col=c("darkgreen"),pch=c(19))
	Sys.sleep(1)
	}
}


CentrosNeur[,"AngP"]<-pmin(CentrosNeur$alphaP.D,CentrosNeur$alphaP.V)
CentrosNeur[,"AngA"]<-pmax(CentrosNeur$alphaP.D,CentrosNeur$alphaP.V)-CentrosNeur$alphaP.A
for (i in 1:nrow(CentrosNeur)){
	if (CentrosNeur$alphaP.D[i]>CentrosNeur$alphaP.V[i]){
		CentrosNeur[i,"AngD"]<-360-CentrosNeur$alphaP.D[i]
		CentrosNeur[i,"AngV"]<-CentrosNeur$alphaP.A[i]-CentrosNeur$alphaP.V[i]
	}else{
		CentrosNeur[i,"AngD"]<-CentrosNeur$alphaP.A[i]-CentrosNeur$alphaP.D[i]
		CentrosNeur[i,"AngV"]<-360-CentrosNeur$alphaP.V[i]
		}
}

CentrosNeur[,"PorcCircunfP"]<-CentrosNeur$AngP/360
CentrosNeur[,"PorcCircunfD"]<-CentrosNeur$AngD/360
CentrosNeur[,"PorcCircunfA"]<-CentrosNeur$AngA/360
CentrosNeur[,"PorcCircunfV"]<-CentrosNeur$AngV/360
for (i in 1:nrow(CentrosNeur)){
	CentrosNeur[i,"n_dendr"]<-length(unique(SoloDendritas$dendrite_id[CentrosNeur$ID[i]==SoloDendritas$ID]))
}

for (i in 1:nrow(CentrosNeur)){
	CentrosNeur[i,"RatioP"]<-CentrosNeur[i,"X.P"]/CentrosNeur[i,"PorcCircunfP"]
	CentrosNeur[i,"RatioV"]<-CentrosNeur[i,"X.V"]/CentrosNeur[i,"PorcCircunfV"]
	CentrosNeur[i,"RatioA"]<-CentrosNeur[i,"X.A"]/CentrosNeur[i,"PorcCircunfA"]
	CentrosNeur[i,"RatioD"]<-CentrosNeur[i,"X.D"]/CentrosNeur[i,"PorcCircunfD"]
}

for (i in 1:nrow(ContornoAreas)){
	ContornoAreas[i,"numP"]<-nrow(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$RatioP>1,])	
	ContornoAreas[i,"numV"]<-nrow(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$RatioV>1,])	
	ContornoAreas[i,"numA"]<-nrow(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$RatioA>1,])	
	ContornoAreas[i,"numD"]<-nrow(CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i] & CentrosNeur$RatioD>1,])
}

#DataSinContour<-DataSinContour[order(DataSinContour$individual,DataSinContour$layer,DataSinContour$cell_id),]
### Escritura de tablas una vez modificadas ### 
write.table(Contorno,file=".//Art3Orientation//tablas//Contorno",append=FALSE)
write.table(ContornoAreas,file=".//Art3Orientation//tablas//ContornoAreas",append=FALSE)
write.table(DataSinContour,file=".//Art3Orientation//tablas//DatosSinContorno",append=FALSE)
write.table(Solonodosraiz,file=".//Art3Orientation//tablas//NodosRaiz",append=FALSE)
write.table(CentrosNeur,file=".//Art3Orientation//tablas//CentrosNeuronas",append=FALSE)

#Extraemos la información que vamos a usar en los analisis
NeurOrient<-CentrosNeur[,c("individual","layer","cell_id","ID","total_dendrites_length","X.P","X.V","X.A","X.D")]
MapOrient<-ContornoAreas[,c("individual","layer","map_id","ID","total_neurons_length","X.P","X.V","X.A","X.D")]
SoloDendritas<-DataSinContour[DataSinContour$type=="dendrite",]

write.table(NeurOrient,file=".//Art3Orientation//tablas//NeuronOrientation",append=FALSE)
write.table(MapOrient,file=".//Art3Orientation//tablas//MapOrientation",append=FALSE)
write.table(SoloDendritas,file=".//Art3Orientation//tablas//SoloDendritas",append=FALSE)



#View(`TodosMapas`)#View(`Contorno`)#View(`ContornoAreas`)#View(`DataSinContour`)#View(`Solonodosraiz`)
#View(`SoloDendritas`)#View(`CentrosNeur`)#View(`NeurOrient`)#View(`MapOrient`)


##Vamos a guardar en un excell las tablas con los % y los índices de esperado/obersvado##

#View(`TodosMapas`)#View(`Contorno`)#View(`ContornoAreas`)#View(`DataSinContour`)#View(`Solonodosraiz`)
#View(`SoloDendritas`)#View(`CentrosNeur`)#View(`NeurOrient`)#View(`MapOrient`)

#Queremos de cada mapa: La rata, capa, célula, nº dendritas, longitud total, %observadoP,V,A,D, %esperadoP,V,A,D, ratio observado/esperado P,V,A,D
arxiv<-"TablasOrient"
source(".//Art3Orientation//F - ExportarExcell.R")
#Creamos el excell y la primera pestaña
i=1
pestanya<-paste0("",ContornoAreas$map_id[i])
table<-CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i],c("individual","layer","cell_id","n_dendr","total_dendrites_length","X.P","X.V","X.A","X.D","PorcCircunfP","PorcCircunfV","PorcCircunfA","PorcCircunfD","RatioP","RatioV","RatioA","RatioD")]
table[,6]<-table[,6]*100;table[,7]<-table[,7]*100;table[,8]<-table[,8]*100;table[,9]<-table[,9]*100;table[,10]<-table[,10]*100;table[,11]<-table[,11]*100;table[,12]<-table[,12]*100;table[,13]<-table[,13]*100
#escribimos en el excell "arxiv" con nombre de pestaña "pestanya"
ExportarExcell(table,arxiv,pestanya,FALSE)
for (i in 2:nrow(ContornoAreas)){
	pestanya<-paste0("",ContornoAreas$map_id[i])
	table<-CentrosNeur[CentrosNeur$individual==ContornoAreas$individual[i] & CentrosNeur$layer==ContornoAreas$layer[i],c("individual","layer","cell_id","n_dendr","total_dendrites_length","X.P","X.V","X.A","X.D","PorcCircunfP","PorcCircunfV","PorcCircunfA","PorcCircunfD","RatioP","RatioV","RatioA","RatioD")]
	table[,6]<-table[,6]*100;table[,7]<-table[,7]*100;table[,8]<-table[,8]*100;table[,9]<-table[,9]*100;table[,10]<-table[,10]*100;table[,11]<-table[,11]*100;table[,12]<-table[,12]*100;table[,13]<-table[,13]*100
	#escribimos en el excell "arxiv" con nombre de pestaña "pestanya"
	ExportarExcell(table,arxiv,pestanya,TRUE)
}






	#################################################################################
########################### DIBUJO DE LAS CIRCUNFERENCIAS ###############################
	#################################################################################
	
#Variables a utilizar para la representación en circunferencias
data<-circular(SoloDendritas$angleR)
cases2<-list(SoloDendritas$orientacion,SoloDendritas$Dist_centro)
distsegmentos<-0.5 #0.25 #Distancia entre las q dividir los segmentos
vectorrepet<-(ceiling((SoloDendritas$segment_length)*(1/distsegmentos))/(1/distsegmentos))/distsegmentos #Vector con el numero de segmentos que dividen el segmento original
distradios<-25 #Distancia entre las q dividir las distancias al centro de los puntos
Colores<-c("blue","blueviolet","darkgreen","darkorange")
EtiquetasAreasCerebro<-c("Posterior","Anterior","Ventral","Dorsal")
AreasCerebro<-c("P","A","V","D")

######## Proyeccion de los puntos en una circúnferencia usando longitudes #########
#Para guardar en EPS (alta calidad) comentar la linea de jpeg y descomentar el postscript
for (i in 1:nrow(CentrosNeur)){
	cases<-SoloDendritas$ID==CentrosNeur$ID[i] & SoloDendritas$segment_length<10
	jpeg(file=paste0(".//Art3Orientation//Graficos//ShollLengthRepresent_",CentrosNeur$ID[i],".jpg"),units="in",res=600,width = 14,height = 7, pointsize = 12)
	#postscript(file=paste0("C://Users//Ileguey//Documents//Revolution//Art3Orientation//Graficos//ShollLengthRepresent_",CentrosNeur$ID[i],".eps"), onefile=TRUE, horizontal=FALSE, width = 14,height = 7, pointsize = 12)
	par(mar=c(5,5,4,2))
	PointsCirclesGraph(data,distsegmentos,vectorrepet,cases,cases2,Colores,AreasCerebro,EtiquetasAreasCerebro,Neurona=CentrosNeur$ID[i],lengths=TRUE,axess=FALSE)
	dev.off()
}

############## Representación neuronas en círculos concéntricos ##############
#Para guardar en EPS (alta calidad) comentar la linea de jpeg y descomentar el postscript
for (i in 1:nrow(CentrosNeur)){
	cases<-SoloDendritas$ID==CentrosNeur$ID[i] & SoloDendritas$segment_length<10
	jpeg(file=paste0(".//Art3Orientation//Graficos//Concentric_",CentrosNeur$ID[i],".jpg"),units="in",res=600,width = 14,height = 7, pointsize = 12)
	#postscript(file=paste0("C://Users//Ileguey//Documents//Revolution//Art3Orientation//Graficos//Concentric_",CentrosNeur$ID[i],".eps"), onefile=TRUE, horizontal=FALSE, width = 14,height = 7, pointsize = 12)
	par(mar=c(5,5,4,2))
	ConcentricCirclesGraph(data,distsegmentos,vectorrepet,cases,cases2,distradios,Colores,AreasCerebro,EtiquetasAreasCerebro,Neurona=CentrosNeur$ID[i],axess=FALSE)
	dev.off()
}


############## Representación neuronas con su orientación sombreada ##############
data<-circular(SoloDendritas$angleR)
cases2<-list(SoloDendritas$orientacion,SoloDendritas$Dist_centro)
distsegmentos<-0.5 #0.25 #Distancia entre las q dividir los segmentos
vectorrepet<-(ceiling((SoloDendritas$segment_length)*(1/distsegmentos))/(1/distsegmentos))/distsegmentos #Vector con el numero de segmentos que dividen el segmento original
distradios<-25 #Distancia entre las q dividir las distancias al centro de los puntos
Colores<-c("blue","blueviolet","darkgreen","darkorange")
EtiquetasAreasCerebro<-c("Posterior","Anterior","Ventral","Dorsal")
AreasCerebro<-c("P","A","V","D")
#Para guardar en EPS (alta calidad) comentar la linea de jpeg y descomentar el postscript
for (i in 1:nrow(CentrosNeur)){
	cases<-SoloDendritas$ID==CentrosNeur$ID[i] & SoloDendritas$segment_length<10
	#jpeg(file=paste0(".//Art3Orientation//Graficos//ShadowNeuron_",CentrosNeur$ID[i],".jpg"),units="in",res=600,width = 14,height = 7, pointsize = 12)
	postscript(file=paste0(".//Art3Orientation//Graficos//ShadowNeuron_",CentrosNeur$ID[i],".eps"), onefile=TRUE, horizontal=FALSE, width = 14,height = 7, pointsize = 12)
	par(mar=c(5,5,4,2))
	PointsCirclesGraphShadow(data,distsegmentos,vectorrepet,cases,cases2,Colores,AreasCerebro,EtiquetasAreasCerebro,Neurona=CentrosNeur$ID[i],lengths=TRUE,datbase=CentrosNeur,i=i,axess=FALSE)
	
	dev.off()
}

#Dibujamos los plots dentro de cada mapa
#source("C:/Users/Ileguey/Documents/Revolution/Art3Orientation/MapasPlots.R")
