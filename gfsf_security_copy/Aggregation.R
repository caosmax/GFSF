#Programa para agregar los rendimientos a nivel de pixel de los modelos de c
# ultivos a  rendimientos a nivel de FPU para ser usados en el modelo IMPACT.
# Benjamin test

#Especificar cultivo
cultivos = c('arroz','maiz','frijol','trigo','soya')
cultivos.en = c('rice','maize','bean','wheat','soybean')
treat = c('riego','secano')  #riego o secano

grd1<-c("C:/Workspace/PruebaIMPACT/BID_Evaluacion_Economica/")
grd2<-c("C:/Workspace/PruebaIMPACT/BID_Evaluacion_Economica/12-Resultados/")
grd3<-c("C:/Workspace/PruebaIMPACT/BID_Evaluacion_Economica/08-Cells_toRun/matrices_cultivo/")
grd4<-c("C:/Workspace/PruebaIMPACT/BID_Evaluacion_Economica/Resultados_agregados/")

for (c in 1:length(cultivos)) {
    
    for (t in 1:length(treat)) {
        #Cargar lista de GCM's
        models <- read.csv(paste(grd1,"ModelosGCM.csv",sep=""),header=T,stringsAsFactors=F)
        models <- rbind(models,'WFD')
       
        #Cargar informaci�n de latitud, longitud, area de spam, fpu, etc.
        load(paste(grd3,cultivos.en[c],'_',treat[t],'.RDat',sep=''))
        
        #Ciclo por GCM y WFD
        for ( j in 1:dim(models)[1])  {
            
            #Cargar informaci�n de los resultados de los modelos de cultivo
            load(paste(grd2,cultivos[c],'/',cultivos[c],'-ENTREGA4','/','_',cultivos[c],'_',treat[t],'_',models[j,],'.RDat',sep=''))
            
            #Crear matriz que contenga en sus filas los rendimientos de cada lugar (pixel) en cada a�o (columnas)
            rend<-matrix(nrow=length(Run), ncol=24)
            for (i in 1:length(Run))
            {rend[i,]<- Run[[i]] [,"HWAH"][1:24]  #solo incluir a�os 1 a 24
            } 
            
            #descartar pixeles con demasiadas fallas
            rend[rend==-99] = 0  #convertir -99 a 0
            
            #find areas where consistently failing and discard these from aggregation
            zeros.wfd.r = apply(rend,1,function(x) sum(x==0,na.rm=T))
            ind.falla = which(zeros.wfd.r>=13)
            
            #Convertir rend en un data.frame
            rend<-data.frame(rend)
            
            #Asignar nombres a el data frame de rendimientos
            colnames(rend)<-paste0("Rend_20",22:45)
            
            #Crear un data frame con toda la informaci�n que necesitamos Longitud, latitud, ID, Area, FPU, y rendientos de 2022 a 2045
            eval(parse(text=paste('md<-data.frame(long=crop_',treat[t],'[,"x"],lat=crop_',treat[t],'[,"y"],Area=crop_',treat[t],'[,"',treat[t],'.area"],FPU=crop_',treat[t],'[,"New_FPU"], rend)',sep='')))
            
            #Agregar columnas de producci�n de 2022 a 2046
            md[,paste0("Prod_20",22:45)]<-md[,"Area"]*md[,paste0("Rend_20",22:45)]
            md[,'ones'] = 1
            #Eliminar las columnas de los rendimientos
            md<-md[,!names(md) %in% (paste0("Rend_20",22:45))]
            
            # Descartar pixeles con m�s de 13 a�os con fallas en la linea base
            if(sum(ind.falla) == 0)
            {
                md<-md
            } else {
                md<-md[-ind.falla,]
            }
            
            #Agregar producci�n y area a nivel de fpu
            md_fpu<-aggregate(md[,c("ones","Area",paste0("Prod_20",22:45))],by=list(md[,"FPU"]),FUN= function(x) {sum(x, na.rm=TRUE)} )
            
            #Agregar Rendimientos a nivel de fpu (rendimiento ponderado)
            md_fpu[,paste0("Rend_fpu_20",22:45)]<-md_fpu[,paste0("Prod_20",22:45)]/md_fpu[,"Area"]
            
            #Crear un data frame con s�lo FPU y rend a nivel de fpu 
            rend_fpu<-md_fpu[,c("Group.1","ones",paste0("Rend_fpu_20",22:45))]
            
            #Asignar nombres apropiados a las columnas
            colnames(rend_fpu)<-c("FPU","num_pixels",paste0("Rend_fpu_20",22:45))
            
            #Count pixels per FPU originalmente
            pixel.FPU = eval(parse(text=paste('table(crop_',treat[t],'$New_FPU)',sep='')))
            pixel.FPU = pixel.FPU[pixel.FPU>0]
            
            #Create big matrix with rows for original FPU's
            rend_fpu2 = array(NA,dim=c(length(pixel.FPU),25))  #24 a�os + numero de pixeles en la agregaci�n
            ind.fpu = match(rend_fpu$FPU,names(pixel.FPU))
            rend_fpu2[ind.fpu,] = as.matrix(rend_fpu[,2:26])
            colnames(rend_fpu2) = colnames(rend_fpu)[2:26]
            
            #create data frame
            rend_fpu3 = data.frame(pixels.original = pixel.FPU,rend_fpu2)
            
            #Computador personal
            write.csv(rend_fpu3,paste(grd4,cultivos[c],'_',treat[t],'_',models[j,],'_FPU.csv',sep=''),row.names=T)
            
        }
        
        print(c)
    }
    
}




