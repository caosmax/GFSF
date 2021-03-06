### graphs 
#### sce3 Vs sce1
#### Codigo para procesar resultados de WB

### libraries 
## limpiar consola
g=gc;rm(list = ls())

### librerias 
suppressMessages(library(reshape))
suppressMessages(library(ggplot2))
suppressMessages(library(plyr))
suppressMessages(library(tidyr))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))
suppressMessages(library(dplyr))
suppressMessages(library(Hmisc))
suppressMessages(library(rgdal))
suppressMessages(library(RColorBrewer))
suppressMessages(library(maptools))
suppressMessages(library(sp))
suppressMessages(library(maps))
suppressMessages(library(raster))
suppressMessages(library(cumplyr))
suppressMessages(library(scales))



### directories
#Definir directorio de trabajo
setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/WB")

### Load data
### cargar los datos area
### groups scenarios
g1<- c("_SSP1"); g2<- c("_SSP2"); g3<- c("_SSP3"); g4<- c("_SSP4");g5<- c("_SSP5")
rcp<- list(g1,g2,g3,g4,g5)


### groups of variables 
# datos por sistema de produccion  riego y secano
datasys<- c("YldXAgg -- Yield", "AreaXAgg -- Area" )
# datos categorias totales
datatotal<- c("TYldXAgg -- Total Yield", "TAreaXAgg -- Total Area")
# datos categorias agregados 
dataagg<- c("FoodAvailXAgg", "QDXAgg -- Total Demand",
            "QEXAgg -- Export","QINTXAgg -- Intermediate Demand","QMXAgg -- Import",
            "QFXAgg -- Household Demand","QSupXAgg -- Commodity Supply",
            "QSXAgg -- Total Production")


# datos categorias animales
dataanimal<- c("AnmlNumXAgg -- Animal Numbers" , "AnmlYldXAgg -- Animal Yield")
# data tratamiento especial
dataespecial<- c("QNXAgg -- Net Trade")


cropsout<- c("Other", "Other Pulses", "Other meals","Other Cereals", "Other Oilseeds",
             "Other Oils", "Other Roots", "Soybean Meal", "Temperate Fruit" ,"Rapeseed Oil" ,
             "Groundnut Oil", "Groundnut meal",  "Rapeseed Meal", "Soybean Oil","Soybean Meal",  
             "Palm Kernel Oil" ,"Palm Kernel Meal", "Sunflower Meal", "Sugar", "Palm Fruit Oil" )




################### TOPICS ################################################
ca<- c("RCP4.5_SSP2", "RCP8.5_SSP2", "RCP6.0_SSP2","RCP2.6_SSP2")

cropsIvory<- c("Yams","Cassava","Plantain","Palm Fruit","Sugarcane") ## almost 90% production
# , "Cacao","Maize","Vegetables","Rice","Banana"
cropsMali<- c("Millet", "Vegetables","Sorghum", "Rice","Sugarcane", "Maize")# almost 87% production
# ,"Sugarcane","Dairy
# "Tropical Fruit","Groundnut", "Cotton"
pots<- unique(xx$region)
p=2 #region
i=1 #parameter
for(i in 1:length(datatotal)){
      xx<- list.files( path = "./tables/",pattern ="csv", full.names = T)
      kk<- unlist(xx)
      kk<- kk[grep(pattern= datatotal[[i]], x = kk)]
      pp<- lapply(kk,read.csv) ### load data
      xx<- do.call(rbind, pp)
      xx$X<- NULL
      
      xx$impactparameter<- as.character(xx$impactparameter)
      xx$sce<- as.character(xx$sce)
      xx$commodity<- as.character(xx$commodity)
      xx$region<- as.character(xx$region)
      xx$ssp<- as.character(xx$ssp)
      xx$rcp<- as.character(xx$rcp)
      ll<- xx %>% group_by(.,impactparameter, commodity, region, year)%>%
            summarise(mean=mean(perc_change)) 
      ll<- as.data.frame(ll)
      lmax<- xx %>% group_by(.,impactparameter, commodity, region, year)%>%
            summarise(upperlimit=max(perc_change)) 
      lmax<- as.data.frame(lmax)
      lmin<- xx %>% group_by(.,impactparameter, commodity, region, year)%>%
            summarise(lowerlimit=min(perc_change)) 
      lmin<- as.data.frame(lmin)
      
      extremos<-merge(lmin,lmax)
      datost<-merge(extremos,ll)
      require(dplyr)
      require(ggplot2)
      test<- datost %>% filter(.,region==pots[p])%>% filter(.,commodity %in% cropsMali)
      
      
      pic<- ggplot(test,
             aes(year,mean,group=commodity,color=commodity))+
            geom_line(linetype="dashed",size=1)+
            geom_ribbon(aes(ymin=lowerlimit,ymax=upperlimit,
                            fill=commodity,colour=commodity,
                            linetype=NA),
                        alpha=0.1)+
            labs(y="Percentage Change\n(%)",x="Year", title=paste(datatotal[i],"\n", pots[p], sep = ""))+ 
            theme(legend.position="bottom", 
                  legend.text=element_text(size=11),
                  axis.text= element_text(size = 11),
                  axis.title=element_text(size = 11),
                  legend.title=element_blank())+
            scale_x_continuous(breaks =c(seq(2020,2050,by = 5)) )    +
            scale_y_continuous(breaks =c(seq(-30,30,by = 2)) )      
            ggsave(file= paste("./pic/",datatotal[i], "_", pots[p],".png",sep=""),pic,width = 9,height = 7)
      dev.off()
      
     
}

##################################### dataagg ##########################
## demand, foodava
cropsIvory<- c("Yams","Cassava","Palm Fruit","Plantain","Sugarcane","Rice","Vegetables")
cropsMali<- c("Millet","Dairy" ,"Vegetables","Sorghum", "Rice","Sugarcane", "Maize")
## production
cropsIvory<- c("Yams","Cassava","Palm Fruit","Plantain","Sugarcane","Cacao","Vegetables")
cropsMali<- c("Millet","Dairy" ,"Vegetables","Sorghum", "Rice","Sugarcane", "Maize")


p=2
i=8
for(i in 1:length(dataagg)){
      xx<- list.files( path = "./tables/",pattern ="csv", full.names = T)
      kk<- unlist(xx)
      kk<- kk[grep(pattern= dataagg[[i]], x = kk)]
      pp<- lapply(kk,read.csv) ### load data
      xx<- do.call(rbind, pp)
      xx$X<- NULL
      
      xx$impactparameter<- as.character(xx$impactparameter)
      xx$sce<- as.character(xx$sce)
      xx$commodity<- as.character(xx$commodity)
      xx$region<- as.character(xx$region)
      xx$ssp<- as.character(xx$ssp)
      xx$rcp<- as.character(xx$rcp)
      ll<- xx %>% group_by(.,impactparameter, commodity, region, year)%>%
            summarise(mean=mean(perc_change)) 
      ll<- as.data.frame(ll)
      lmax<- xx %>% group_by(.,impactparameter, commodity, region, year)%>%
            summarise(upperlimit=max(perc_change)) 
      lmax<- as.data.frame(lmax)
      lmin<- xx %>% group_by(.,impactparameter, commodity, region, year)%>%
            summarise(lowerlimit=min(perc_change)) 
      lmin<- as.data.frame(lmin)
      
      extremos<-merge(lmin,lmax)
      datost<-merge(extremos,ll)
      require(dplyr)
      require(ggplot2)
      test<- datost %>% filter(.,region==pots[p])%>% filter(.,commodity %in% cropsMali)
      
      
      pic<- ggplot(test,
                   aes(year,mean,group=commodity,color=commodity))+
            geom_line(linetype="dashed",size=1)+
            geom_ribbon(aes(ymin=lowerlimit,ymax=upperlimit,
                            fill=commodity,colour=commodity,
                            linetype=NA),
                        alpha=0.1)+
            labs(y="Percentage Change\n(%)",x="Year", title=paste(dataagg[i],"\n", pots[p], sep = ""))+ 
            theme(legend.position="bottom", 
                  legend.text=element_text(size=11),
                  axis.text= element_text(size = 11),
                  axis.title=element_text(size = 11),
                  legend.title=element_blank())+
            scale_x_continuous(breaks =c(seq(2020,2050,by = 5)) )    +
            scale_y_continuous(breaks =c(seq(-30,30,by = 2)) )      
      ggsave(file= paste("./pic/",dataagg[i], "_", pots[p],".png",sep=""),pic,width = 9,height = 7)
      dev.off()
      
      
}


#### net trade
cropsIvory<- c("Yams","Cassava","Palm Fruit","Plantain","Sugarcane","Rice","Vegetables")
cropsMali<- c("Millet","Dairy" ,"Vegetables","Sorghum", "Rice","Sugarcane", "Maize")
## production
cropsIvory<- c("Yams","Cassava","Palm Fruit","Plantain","Sugarcane","Cacao","Vegetables")
cropsMali<- c("Millet","Dairy" ,"Vegetables","Sorghum", "Rice","Sugarcane", "Maize")

joincrops<- c(cropsIvory,cropsMali)

xx<- list.files( path = "./tables/",pattern ="csv", full.names = T)
kk<- unlist(xx)
kk<- kk[grep(pattern= dataespecial, x = kk)]
pp<- lapply(kk,read.csv) ### load data
xx<- do.call(rbind, pp)
xx$X<- NULL

xx$impactparameter<- as.character(xx$impactparameter)
xx$sce<- as.character(xx$sce)
xx$commodity<- as.character(xx$commodity)
xx$region<- as.character(xx$region)
xx$ssp<- as.character(xx$ssp)
xx$rcp<- as.character(xx$rcp)

ll<- xx %>% group_by(.,impactparameter, commodity, region, year, trend)%>%
      summarise(mean=mean(perc_change)) 
ll<- as.data.frame(ll)
lmax<- xx %>% group_by(.,impactparameter, commodity, region, year, trend)%>%
      summarise(upperlimit=max(perc_change)) 
lmax<- as.data.frame(lmax)
lmin<- xx %>% group_by(.,impactparameter, commodity, region, year, trend)%>%
      summarise(lowerlimit=min(perc_change)) 
lmin<- as.data.frame(lmin)


extremos<-merge(lmin,lmax)
datost<-merge(extremos,ll)
require(dplyr)
require(ggplot2)

test<- datost  %>% filter(.,commodity %in% joincrops)%>% filter(., trend!="Transition from X to M") #%>% filter(.,region==pots[p])
test$trend<- as.character(test$trend)

pic<- ggplot(test,
             aes(year,mean,group=region,color=region))+
      geom_line(linetype="dashed",size=1)+ facet_grid(commodity~trend,scales = "free")+
      geom_ribbon(aes(ymin=lowerlimit,ymax=upperlimit,
                      fill=region,colour=region,
                      linetype=NA),
                  alpha=0.1)+
      labs(y="Percentage Change\n(%)",x="Year", title=paste(dataespecial," by regiones ",  sep = ""))+ 
      theme(legend.position="bottom",
            legend.text=element_text(size=11),
            axis.text= element_text(size = 11),
            axis.title=element_text(size = 11),
            legend.title=element_blank())+
      scale_x_continuous(breaks =c(seq(2020,2050,by = 5))) +
      theme(strip.text.y = element_text(angle = 0,size = 11)) +theme_bw()

      # scale_y_continuous(breaks =c(seq(-30,30,by = 2)) ) 

ggsave(file= paste("./pic/",dataespecial,".png",sep=""),pic,width = 9,height = 7)
dev.off()
