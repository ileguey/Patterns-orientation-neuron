col<-rainbow(n=max(DataSinContour[DataSinContour$type=="dendrite","dendrite_id"]))
rgl.points(1,11,1)
for (i in ratas){
	for (j in unique(DataSinContour[DataSinContour$individual==i,"layer"])){
		for (k in unique(DataSinContour[DataSinContour$individual==i & DataSinContour$layer==j,"cell_id"])){
			rgl.close()
			for (w in unique(DataSinContour[DataSinContour$type=="dendrite" & DataSinContour$individual==i & DataSinContour$layer==j & DataSinContour$cell_id==k,"dendrite_id"])){
				Reg1<-RectaRegDen(DataSinContour,CentrosNeur,i,j,k,w,col[w])
				#Reg1 es tipo list donde: [1] puntos x [2] puntos y [3] puntos z [4] colores [5]coef x recta [6]coef y recta[7] coef z recta [8],[9],[10] parametro  alfa beta gamma respectivamente de la recta, partiendo de 0,0,0 en nodo raiz
				#Guardamos las regresiones en el data de nodos raiz
				Solonodosraiz[Solonodosraiz$individual==i & Solonodosraiz$layer==j & Solonodosraiz$cell_id==k & Solonodosraiz$dendrite_id==w,"alfa.recta"]<-Reg1[[8]]
				Solonodosraiz[Solonodosraiz$individual==i & Solonodosraiz$layer==j & Solonodosraiz$cell_id==k & Solonodosraiz$dendrite_id==w,"beta.recta"]<-Reg1[[9]]
				Solonodosraiz[Solonodosraiz$individual==i & Solonodosraiz$layer==j & Solonodosraiz$cell_id==k & Solonodosraiz$dendrite_id==w,"gamma.recta"]<-Reg1[[10]]				
				if (w==min(unique(DataSinContour[DataSinContour$type=="dendrite" & DataSinContour$individual==i & DataSinContour$layer==j & DataSinContour$cell_id==k,"dendrite_id"]))){
					rgl.points(x=Reg1[[1]],y=Reg1[[2]],z=Reg1[[3]],col=Reg1[[4]])
					par3d(windowRect=c(0,22,1920,1060)) #Para que se vea grande
					segments3d(c(0,Reg1[[5]]),c(0,Reg1[[6]]),c(0,Reg1[[7]]),col="green") #la recta se traza por coordenadas (x,y,z) a pares
					#axes3d() #Que dibuje los ejes
					#title3d(main=paste0("Rata",i,"Capa",j,"Neurona",k,""),xlab="x",ylab="y",zlab="z") #Que introduzca titulos
				}else{
					rgl.points(x=Reg1[[1]],y=Reg1[[2]],z=Reg1[[3]],col=Reg1[[4]])
					segments3d(c(0,Reg1[[5]]),c(0,Reg1[[6]]),c(0,Reg1[[7]]),col="green") #la recta se traza por coordenadas (x,y,z) a pares
				}
			}
			title3d(main=paste0("Rata",i,"Capa",j,"Neurona",k,"")) #Que introduzca titulos
			Sys.sleep(3)
		}
	}
}