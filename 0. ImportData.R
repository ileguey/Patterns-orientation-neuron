		###### CÓDIGO DE PREPARACIÓN DE LAS TABLAS PARA EL CÁLCULO DE LA ORIENTACIÓN DE NEURONAS ######

#TodosMapas<-read.table(file="C://Users//Ileguey//Documents//Revolution//Art3Orientation//tablas//all_rats.csv",header=TRUE,sep="")
TodosMapas<-read.table(file=".//Art3Orientation//Art3Orientation//tablas//all_rats_conIV.csv",header=TRUE,sep="")
# para trabajar con cualquier ruta poner getwd(), y el .// noslleva a esa ruta cuando cargamos
#Creamos variable ID
TodosMapas <- cbind(TodosMapas,paste0(TodosMapas[,"individual"],TodosMapas[,"layer"],TodosMapas[,"cell_id"]))
names(TodosMapas)[ncol(TodosMapas)]<-"ID"
TodosMapas<-transform(TodosMapas, branch_id = as.character(branch_id))
TodosMapas$w<-NULL;TodosMapas$color<-NULL;TodosMapas[is.na(TodosMapas$parent) & TodosMapas$type=="dendrite","branch_id"]<-"ORoot"
#Creamos archivo de contorno
Contorno<-TodosMapas[TodosMapas$type=="contour",]
#View(`CentrosNeur`)
order.indiv<-order(Contorno$individual)
Contorno<-Contorno[order.indiv,] #Ordenamos el contorno
#Creamos archivo con resto de individuos
DataSinContour<-TodosMapas[TodosMapas$type!="contour",]

#View(`TodosMapas`)
#View(`Contorno`)
#View(`DataSinContour`)

#Archivo con los p.raiz de cada neurona:
Solonodosraiz<-DataSinContour[DataSinContour$type=="dendrite",]
Solonodosraiz<-Solonodosraiz[Solonodosraiz$branch_id=="ORoot",]
#View(`Solonodosraiz`)

#Calculamos centros de las neuronas en el mapa:
CentrosNeur<-Solonodosraiz[1,] #para inicializar el dataframe CentrosNeur
CentrosNeur$map_id<-NULL;CentrosNeur$segment_length<-NULL;CentrosNeur$point_id<-NULL;CentrosNeur$dendrite_id<-NULL;CentrosNeur$x<-NULL;CentrosNeur$y<-NULL;CentrosNeur$z<-NULL;CentrosNeur$parent<-NULL;CentrosNeur$descendants<-NULL
ratas<-sort(unique(DataSinContour$individual))
capas<-sort(unique(DataSinContour$layer))
ind<-1
for (i in ratas){
	capasdisp<-sort(unique(DataSinContour[DataSinContour$individual==i,"layer"]))
	for (j in capasdisp){
		numcel<-unique(DataSinContour[DataSinContour$individual==i & DataSinContour$layer==j,"cell_id"])
		for (k in numcel){
			CentrosNeur[ind,"species"]<-"rat"
			CentrosNeur[ind,"individual"]<-i
			CentrosNeur[ind,"layer"]<-j
			CentrosNeur[ind,"cell_id"]<-k
			CentrosNeur[ind,"ID"]<-unique(Solonodosraiz[Solonodosraiz$individual==i & Solonodosraiz$layer==j & Solonodosraiz$cell_id==k,"ID"])
			CentrosNeur[ind,"type"]<-unique(Solonodosraiz[Solonodosraiz$individual==i & Solonodosraiz$layer==j & Solonodosraiz$cell_id==k,"type"])
			CentrosNeur[ind,"branch_id"]<-unique(Solonodosraiz[Solonodosraiz$individual==i & Solonodosraiz$layer==j & Solonodosraiz$cell_id==k,"branch_id"])
			CentrosNeur[ind,"centerXroot"]<-mean(Solonodosraiz[Solonodosraiz$individual==i & Solonodosraiz$layer==j & Solonodosraiz$cell_id==k,"x"])
			CentrosNeur[ind,"centerYroot"]<-mean(Solonodosraiz[Solonodosraiz$individual==i & Solonodosraiz$layer==j & Solonodosraiz$cell_id==k,"y"])
			CentrosNeur[ind,"centerZroot"]<-mean(Solonodosraiz[Solonodosraiz$individual==i & Solonodosraiz$layer==j & Solonodosraiz$cell_id==k,"z"])
			CentrosNeur[ind,"centerXsoma"]<-mean(DataSinContour[DataSinContour$individual==i & DataSinContour$layer==j & DataSinContour$cell_id==k & DataSinContour$type=="soma","x"])
			CentrosNeur[ind,"centerYsoma"]<-mean(DataSinContour[DataSinContour$individual==i & DataSinContour$layer==j & DataSinContour$cell_id==k & DataSinContour$type=="soma","y"])
			CentrosNeur[ind,"centerZsoma"]<-mean(DataSinContour[DataSinContour$individual==i & DataSinContour$layer==j & DataSinContour$cell_id==k & DataSinContour$type=="soma","z"])
			ind<-ind+1			
		}
	}
}

write.table(TodosMapas,file=".//Art3Orientation//Art3Orientation//tablas//TodosMapas",append=FALSE)
write.table(Contorno,file=".//Art3Orientation//Art3Orientation//tablas//Contorno",append=FALSE)
write.table(DataSinContour,file=".//Art3Orientation//Art3Orientation//tablas//DatosSinContorno",append=FALSE)
write.table(Solonodosraiz,file=".//Art3Orientation//Art3Orientation//tablas//NodosRaiz",append=FALSE)
write.table(CentrosNeur,file=".//Art3Orientation//Art3Orientation//tablas//CentrosNeuronas",append=FALSE)

##Añadido por Luis
#
###
## Return a table with the center of each cell calculated via somas or Dendrite origins
###
#computeCellCenters <- function(table, center.names = list("species","individual","layer","cell_id","x_s","y_s","z_s","x_d","y_d","z_d") ){
    #
  ##Speed-up: Prefetch somas and dendrites roots
  #somas <- table[table$type=="soma",]
  #dendRoots <- table[is.na(table$parent) & table$type=="dendrite",]
#
  ## Get soma and dend roots for selected cells
  #somas_c <- aggregate(cbind(somas$x,somas$y,somas$z),by=list(somas$species,somas$individual,somas$layer,somas$cell_id),FUN="mean")
  #dend_c <- aggregate(cbind(dendRoots$x,dendRoots$y,dendRoots$z),by=list(dendRoots$species,dendRoots$individual,dendRoots$layer,dendRoots$cell_id),FUN="mean")
#
  ## Agggregate results and change colnames
  #centers <- cbind(somas_c,dend_c[,c(5,6,7)])
  #colnames(centers) <- center.names
  #centers
#}