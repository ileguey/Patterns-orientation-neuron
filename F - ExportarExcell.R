ExportarExcell <- function(nombrearchivo,nombreexcell,nombrepestana,append)

{

#Exportacion a Excell
R1 <-paste0(".//Art3Orientation//",nombreexcell,".xlsx") #Creamos un archivo de excell con los datos reestructurados

#R2 <-"C://Users//Ileguey//Documents//centroneuronize.xlsx"
library(xlsx)
write.xlsx(nombrearchivo,R1,sheetName=nombrepestana,append=append)
#write.table(nombrearchivo,R1,sheetName=nombrepestana,append=append)
#write.xlsx(aux,R2,"neuronize")
}