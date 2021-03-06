# Rendimientos y serie de tiempo, arroz. Analisis destinado paper de arroz y otros usos. 
# carlos Eduardo

## clean data
g=gc;rm(list = ls())

#libraries
# R options
options(warn = -1)
options(scipen = 999)

# Load packages
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(ggplot2))
suppressMessages(library(networkD3))
suppressMessages(library(jsonlite))
suppressMessages(library(circlize))
suppressMessages(library(curl))
suppressMessages(library(shiny))
suppressMessages(library(TTR))
suppressMessages(library(stats))

suppressMessages(library(igraph))
suppressMessages(library(network))
suppressMessages(library(sna))
suppressMessages(library(visNetwork))
suppressMessages(library(threejs))
suppressMessages(library(networkD3))
suppressMessages(library(ndtv))
suppressMessages(library(tcltk))
suppressMessages(library(rgl))
suppressMessages(library(ape))
suppressMessages(library(reshape))
suppressMessages(library(forecast))



############ Yields 

setwd("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/BID_2/Comtrade")
faoStat<- read.csv(file = "./Production_Crops_E_All_Data_(Normalized)/Production_Crops_E_All_Data_(Normalized).csv", header = T)
str(faoStat)
faoStat$Item<- as.character(faoStat$Item); faoStat$Area<- as.character(faoStat$Area)
faoStat$Element<- as.character(faoStat$Element)
faoStat$Unit<- as.character(faoStat$Unit)


faoStatTrade<- read.csv(file = "./Trade_Crops_Livestock_E_All_Data_(Normalized)/Trade_Crops_Livestock_E_All_Data_(Normalized).csv", header = T)
faoStat$Element<- plyr::revalue(faoStat$Element, c("Area harvested"="AreaH"))

var<- unique(faoStat$Element)
crops<- unique(faoStat$Item)
faoStat$Area.Code<- NULL; faoStat$Item.Code<- NULL; faoStat$Element.Code<- NULL; faoStat$Year.Code<- NULL; faoStat$Flag<- NULL
faoStat$Unit<- NULL


# calculando rendimientos y filtrando
cas<-  filter(faoStat,Item=="Cassava") %>% filter(., Element!="Seed") %>% filter(., Value!=0)
# rice <- rice %>% spread(Element,Value) %>% mutate(.,Ys= (.$Production/.$AreaH)*1000)
# rice<- rice[c(1,3,7)]

cas$Area<- plyr::revalue(cas$Area, c("Democratic People's Republic of Korea"="Democratic Republic of Korea",
                                       "Lao People's Democratic Republic"="Lao Democratic Republic", 
                                       "C�te d'Ivoire"="Ivory Coast",
                                       "China, Hong Kong SAR"='Hong Kong',
                                       "China, Taiwan Province of"='Taiwan'))


lac<- c( "Argentina", "Mexico", "Venezuela (Bolivarian Republic of)","Paraguay", "Peru", "Brazil","Chile", "Guatemala", "Cuba", "Jamaica","Trinidad and Tobago",
         "Barbados","Panama", "El Salvador", "Suriname", "Uruguay", "Honduras","Martinique", "Nicaragua", "Haiti","Colombia", "Costa Rica", 
         "Aruba", "Belize","Ecuador", "Guatemala","Chile", "Bolivia (Plurinational State of)", "Guyana", "Dominican Republic", "Bahamas")

potsOut<- c("Least Developed Countries" ,"Land Locked Developing Countries","Small Island Developing States",
            "Low Income Food Deficit Countries","Net Food Importing Developing Countries","European Union",
            "World","Africa","Eastern Africa","Middle Africa" ,"Northern Africa","Southern Africa","Western Africa",
            "Central America", "Caribbean","South America" ,"Asia", "Central Asia", "Eastern Asia" ,"Southern Asia",
            "Europe","Eastern Europe","Southern Europe" ,"Western Europe" , "Oceania","Australia & New Zealand","Melanesia",
            "Micronesia", "Americas","Northern America","South-Eastern Asia","Western Asia","China, Macao SAR","China, mainland", 
            "Mauritius") #"Hong Kong"

###################
##    LAC      ####
###################
lacRice<- cas %>% dplyr::filter( Area %in% lac)
pots<-unique(lacRice$Area)

###################
##    World    ####
###################
WorldRice<- cas %>%  dplyr::filter(!Area %in% potsOut) %>% filter(Area!="Syrian Arab Republic")

##################
## Regions    ####
##################

##AMERICA
caribbean<- c("Cuba", "Dominican Rep.", "Haiti", "Jamaica", "Dominican Rep.","N. Mariana Isds",
              "Aruba","Saint Lucia","Dominica", "Saint Vincent and the Grenadines", "Puerto Rico",
              "Anguilla", "Antigua and Barbuda", "Saint Kitts and Nevis", "Montserrat",
              "Martinique", "Barbados", "Trinidad and Tobago", "Montserrat", "Grenada", 
              "Neth. Antilles", "Bahamas", "Bermuda", "Turks and Caicos Isds", "Bunkers" , 
              "Cayman Isds", "Br. Virgin Isds", "Curaçao", "Saint Maarten", "Bonaire","Saint Barthélemy" )
Central_America<- c( "Belize","Costa Rica","Guatemala","Honduras","Mexico",
                     "Nicaragua","Panama","El Salvador" )
Northern_America<-c("Canada","Greenland","USA", "Saint Pierre and Miquelon")
South_America<- c("Argentina","Bolivia (Plurinational State of)","Brazil","Chile","Colombia","Ecuador","Guyana","Peru",
                  "Paraguay","Uruguay","Venezuela","Suriname", "Falkland Isds (Malvinas)")


### ASIA
Central_Asia<- c("Kazakhstan","Kyrgyzstan","Tajikistan","Turkmenistan","Uzbekistan")
Eastern_Asia<- c("China","Japan","Rep. of Korea","Mongolia","North Korea", "Taiwan","China, Hong Kong SAR", 
                 "China, Macao SAR","China, mainland", "Dem. People's Rep. of Korea")
South_Eastern_Asia<- c("Indonesia","Cambodia","Lao People's Dem. Rep.","Myanmar","Malaysia","Other Southeast Asia","Philippines","Thailand",
                       "Timor-Leste","Viet Nam", "Singapore", "Brunei Darussalam")
Southern_Asia<- c("Afghanistan","Bangladesh","Bhutan","India","Sri Lanka","Maldives", "Sri Lanka",
                  "Nepal","Other Indian Ocean","Pakistan")
Western_Asia<-c("Armenia","Azerbaijan","Cyprus", "Georgia", "Iraq", "Israel", "Jordan", "Armenia",
                "State of Palestine", "Rest of Arabia","Saudi Arabia","Iran","Bahrain","Kuwait","Oman", "Bahrain", "Qatar","Saudi Arabia",
                "Syria", "Turkey","Yemen", "Armenia","Azerbaijan", "United Arab Emirates", "Georgia", "South Ossetia",
                "Lebanon", "Lebanon")



##OCENIA
Australia_and_New_Zealand<- c( "New Zealand", "Australia", "New Caledonia")

micro_poli_melanesia<- c("Fiji", "Papua New Guinea","Solomon Isds","Vanuatu", "Tonga","Palau", "Kiribati", "French Polynesia", 
                         "FS Micronesia", "Wallis and Futuna Isds", "Tuvalu", "Samoa", "Cook Isds", "Christmas Isds",
                         "American Samoa", "Marshall Isds", "Nauru", "Niue")


## EUROPA
Eastern_Europe<- c("Bulgaria","Belarus","Czechia","Hungary","Rep. of Moldova","Other Balkans","Poland",
                   "Russia","Slovakia","Ukraine", "Russian Federation", "Latvia",
                   "TFYR of Macedonia", "Kosovo", "Poland", "Serbia", "Romania", "Lithuania")
Southern_Europe<- c("Albania","Greece", "Croatia","Italy","Portugal","Spain","Slovenia", "Serbia and Montenegro", "Andorra",
                    "Malta", "San Marino", "Montenegro", "Bosnia Herzegovina", "Gibraltar")
Northern_Europe<- c("Baltic States","Denmark", "Finland","Ireland","Iceland","Norway","Sweden","United Kingdom", "Estonia", 
                    "Sweden", "Faeroe Isds")
Western_Europe<- c("Austria","Belgium-Luxembourg","Switzerland","Germany","France","Belgium","Netherlands","Other Atlantic", "Luxembourg")

## AFRICA

Middle_Africa<- c("Angola","Central African Rep.","Cameroon","Dem. Rep. of the Congo","Congo","Gabon","Equatorial Guinea","Chad",
                  "S�o Tom� and Pr�ncipe")
Northern_Africa<-c("Algeria","Egypt","Libya","Morocco","Sudan (former)","Tunisia", "South Sudan", "Sudan", "Fmr Sudan") #"Sudan,"
Southern_Africa<- c("Botswana", "Lesotho","Namibia","Swaziland","South Africa")
Western_Africa<- c("Benin","Burkina Faso","Côte d'Ivoire","Ghana","Guinea","Gambia","Guinea-Bissau","Liberia", "Sierra Leone",
                   "Mali","Mauritania","Niger","Nigeria","Senegal","Sierra Leon","Togo", "Saint Helena", 
                   "Sao Tome and Principe", "Cabo Verde")
Eastern_Africa<- c("Burundi", "Djibouti","Eritrea","Ethiopia", "Kenya","Madagascar",
                   "Mozambique","Malawi","Rwanda","Somalia", "United Rep. of Tanzania","Comoros",
                   "Uganda","Zambia","Zimbabwe", "Mauritius", "Seychelles", "Mayotte")



sacar<- c("Pacific Islands Trust Territory",  "Wallis and Futuna Islands" ,"Niue", "Saint Vincent and the Grenadines" )

WorldRice<- WorldRice %>% dplyr::filter(., !Area %in% sacar) 
pots<-unique(WorldRice$Area)


c=78
####LAC----------------
tryCatch(lapply(1:length(pots), function(c){
      
      #################################################Country###############################################################
      workdi<- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/RTBFiles"
      cfiles<- dplyr::filter(WorldRice, Area==pots[[c]]) %>% filter(., Element=="Yield")
      
      cfiles$Value<- (cfiles$Value)*0.1
      nf<- cfiles[nrow(cfiles),]
      #nf$Year
      ns<- cfiles[1,]
      # ns$Year
      test_i<- ts(cfiles[5], start =c(ns$Year), end =c(nf$Year) ) # , end =c()
      plot(test_i)
      
      #smooth using Holt Winter
      hw<-HoltWinters(x = test_i,gamma =F) # beta is for exponential smoothing, and gamma parameter to define seasonal
      #       hw2<-HoltWinters(x = test_i,gamma = F) # beta is for exponential smoothing, and gamma parameter to define seasonal
            plot(hw)
      #       plot(fitted(hw2))
      #       plot(fitted(hw))
      #       
      
      
      
      HWplot<- function(ts_objet, n.ahead=10, CI=0.95, error.ribbon="green", line.size=1){
            
            hw_object<- HoltWinters(ts_objet,gamma = F)
            forecast<- predict(hw_object, n.ahead=n.ahead, prediction.interval=T, level=CI)
            for_values<- data.frame(time=round(time(forecast), 3),value_forecast=as.data.frame(forecast)$fit, dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
            fitted_values<- data.frame(time=round(time(hw_object$fitted),3), value_fitted= as.data.frame(hw_object$fitted)$xhat)
            actual_values<- data.frame(time=round(time(hw_object$x),3), Actual=c(hw_object$x))
            
            save(for_values, fitted_values, actual_values,file = paste(workdi,"/ShocksYields/",pots[[c]], ".RData", sep = ""))
            
            
            graphset<- merge(actual_values, fitted_values, by='time', all=TRUE)
            graphset<- merge(graphset, for_values, all=TRUE, by='time')
            graphset[is.na(graphset$dev), ]$dev<-0
            graphset$Fitted<- c(rep(NA, NROW(graphset)-(NROW(for_values)+ NROW(fitted_values))),fitted_values$value_fitted, for_values$value_forecast)
            
            graphset.met<- melt(graphset[,c('time', 'Actual', 'Fitted')], id='time')
            p<- ggplot(graphset.met, aes(x=time, y=value))+
                  geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev, ymax=Fitted+dev), alpha=0.2, fill=error.ribbon)+
                  geom_line(aes(colour=variable), size=line.size)+ labs(title= paste(pots[[c]], ", Time series",sep=""))+
                  geom_vline(xintercept = max(actual_values$time), lty=2)+ xlab("Time")+ ylab("Value")
            
            return(p)
            
      }
      
      
      # Grafico 
      #       tiff(filename= paste(workdi,pots[[c]],"_graph2HW.tiff",sep=""), 
      #            width = 10, height = 7, units = 'in', res = 100)
      HWplot(test_i) # usando la funcion 
      # dev.off()
      
      
      # cargar datos 
      load(paste(workdi,"/ShocksYields/", pots[[c]], ".RData", sep = ""))
      
      names(actual_values)[2]<-"val"
      names(fitted_values)[2]<-"val"
      names(for_values)[2]<-"val"
      
      actual_values$des<- "actual"
      fitted_values$des<- "fitted"
      for_values$des<- "forecast"
      for_values$dev<- NULL
      v<- data.frame(for_values)
      j<- data.frame(actual_values)
      h<- data.frame(fitted_values)
      
      
      v$time<- as.character(v$time)
      j$time<- as.character(j$time)
      h$time<- as.character(h$time)
      bdList<- list(v,h,j)
      db<- do.call(rbind, bdList)
      write.csv(db,file = paste(workdi,"/ShocksYields/", pots[[c]], "OutPutHWFAO.csv", sep = ""))
      db<- filter(db,des!="forecast") %>%  filter(.,time<=2016) %>% spread(des, val)
      db$fitted[is.na(db$fitted)]<- db$actual
      
      localShock<- db$actual-db$fitted
      
      
      #smooth other techniques
      test_a<- ts(cfiles[5], start =c(ns$Year), end=c(nf$Year))
      plot.ts(test_a)
      
      ## test seasonal
      fit<- tbats(test_a)
      seasonal<- !is.null(fit$seasonal.periods)
      cat(paste("The country ", pots[[c]], " has/hasnot seasonality ==", seasonal, ", done!!\n", sep = "")) 
      
      # decomposing non-seasonal data
      ## order 3
      test_a_sma<- SMA(test_a,n = 3) # smooth using simple movil average
      #       png(filename= paste(workdi,pots[[c]],"_graphSMA.png",sep=""), 
      #           width = 10, height = 7, units = 'in', res = 100)
      #       plot.ts(test_a_sma)
      #       dev.off()
      
      
      ## graph smooth using exponential movil average      
      test_a_ema<- EMA(test_a, n = 3) # smooth using exponential movil average
      #       png(filename= paste(workdi,pots[[c]],"_graphEMA.png",sep=""), 
      #           width = 10, height = 7, units = 'in', res = 100)
      #       plot.ts(test_a_ema)
      #       dev.off()
      
      # dat frame     
      data_sma<- data.frame(time=time(test_a_sma), val=as.matrix(test_a_sma))
      data_sma$des<- "SMA"
      data_ema<- data.frame(time=time(test_a_ema), val=as.matrix(test_a_ema))
      data_ema$des<- "EMA"
      data_sma$time<- as.character(data_sma$time)
      data_ema$time<- as.character(data_ema$time)
      bdList_smooth<- list(data_sma,data_ema,j)
      db_smooth<- do.call(rbind, bdList_smooth)
      write.csv(db_smooth,file = paste(workdi,"/ShocksYields/", pots[[c]], "OutPutSmoothFAO.csv", sep = ""))
      db_smooth<- db_smooth %>% spread(des, val)
      db_smooth$EMA[is.na(db_smooth$EMA)]<- db_smooth$actual
      db_smooth$SMA[is.na(db_smooth$SMA)]<- db_smooth$actual
      
      #ema
      localShock_smooth_ema<- db_smooth %>% group_by(time) %>% summarise(localShock_smooth_ema=actual-EMA) %>% 
            select(localShock_smooth_ema)
      
      #sma
      localShock_smooth_sma<- db_smooth %>% group_by(time) %>% summarise(localShock_smooth_sma=actual-SMA) %>% 
            select(localShock_smooth_sma)
      
      #deviations
      brief<- data.frame(time= db_smooth$time,country= pots[[c]], localHV= localShock, localsma=localShock_smooth_sma, localema=localShock_smooth_ema)
      
      #average production
      cfilesPro1<- filter(WorldRice, Element=="Production") %>% group_by(Year)%>% summarise(totalQ=sum(Value))
      totalQ<- cfilesPro1$totalQ
      
      cfilesPro2<- dplyr::filter(WorldRice, Area==pots[[c]]) %>% filter(., Element=="Production")
      
      temQ<- cfilesPro2  #%>% spread(Year, Value) %>% mutate(total=rowSums(.[4:ncol(.)],na.rm=T ))%>% select(Area, Element, total)
      colnames(temQ)[4]<- "time"; temQ$time<- as.character(temQ$time); temQ$time<- as.numeric(temQ$time)
      colnames(cfilesPro1)[1]<- "time"
      
      # brief$totalQ<- cfilesPro1$totalQ
      brief$time<- as.character(brief$time)
      brief$time<- as.numeric(brief$time)
      
      brief<- left_join(brief,temQ, by="time")
      brief<- brief[c("time","country","localHV", "localShock_smooth_sma","localShock_smooth_ema", "Value")]
      colnames(brief)[6]<- "Qave"
      
      cfilesPro1$time<- as.character(cfilesPro1$time); cfilesPro1$time<- as.numeric(cfilesPro1$time)
      brief<- left_join(brief,cfilesPro1, by="time")
      
      brief$localHV_W<- (brief$Qave/brief$totalQ)* brief$localHV
      brief$localsma_W<- (brief$Qave/brief$totalQ)* brief$localShock_smooth_sma
      brief$localema_W<- (brief$Qave/brief$totalQ)* brief$localShock_smooth_ema  
      
      
      write.csv(brief,file = paste(workdi,"/ShocksYields/", pots[[c]], "briefDesy.csv", sep = ""))
      
      
      return(c)
      
      
})) 

### load data 
shockW<- list.files(path= paste(workdi,"/ShocksYields/", sep = ""), pattern = "briefDesy", all.files = T,full.names = T)
shockW<- lapply(shockW, read.csv ,header=T)
shockW<- do.call(rbind, shockW)
shockW$X<- NULL 
yr<-unique(shockW$time)

# c=91;y=20
lapply(1:length(pots), function(c){
      
      workdi<- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/RTBFiles/ShocksYields/temporal/"
      foreign<- filter(shockW, country!=pots[[c]]) %>% group_by(time) %>% mutate(sumlocalHV_W=sum(localHV_W, na.rm=T),
                                                                                 sumlocalsma_W=sum(localsma_W, na.rm=T),
                                                                                 sumlocalema_W=sum(localema_W, na.rm=T)) 
      
      local<- filter(shockW, country==pots[[c]])
      yytemp<- list()
      for(y in 1:length(yr)){
            yy<- filter(foreign, time==yr[y])
            yy1<- unique(yy$sumlocalHV_W)
            yy2<- unique(yy$sumlocalsma_W)
            yy3<- unique(yy$sumlocalema_W)
            yytemp[[y]]<- data.frame(time=yr[y],country=pots[[c]],f_hvW=yy1,f_smaW=yy2,f_emaW= yy3)
      }
      xxtemp<- do.call(rbind, yytemp)
      
      local<- left_join(local,xxtemp, by = c("time", "country"))
      write.csv(local,file = paste(workdi, pots[[c]], "_shocksLocal&ForeighFAO.csv", sep = ""))
      
      
      return(c)
      
})

### load data 
workdi<- "//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/RTBFiles/ShocksYields/temporal/"

shockComplete<- list.files(path="//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/RTBFiles/ShocksYields/temporal",
                           pattern = "_shocksLocal&ForeighFAO", all.files = T,full.names = T)
shockComplete<- lapply(shockComplete, read.csv ,header=T)
shockComplete<- do.call(rbind, shockComplete)
shockComplete$X<- NULL 



#### getting zscore 
## local 

var<- c("localHV","localShock_smooth_sma","localShock_smooth_ema","f_hvW", "f_smaW","f_emaW")


# v=1; x=3
z_dataL<- list()
z_yL<- list()
for(v in 1:length(var)){
      
      tempZ<- shockComplete %>% select(., "time","country",var[[v]] )
      tempZ$time<- as.character(tempZ$time); tempZ$time<- as.numeric(tempZ$time)
      tempZ$country<- as.character(tempZ$country)
      
      for(x in 1:length(yr)){
            tt<- filter(tempZ, time==yr[[x]])
            #             sd_var<- sd(tt[,3])
            #             length_var<- length(tt[,3])
            #             a<- sd_var
            #             b<- mean(tt[,3])
            #             z<- (tt[,3]-b)/a
            a<- quantile(tt[,3], probs = c(0.4))
            b<- quantile(tt[,3], probs = c(0.6))
            # b<- quantile(tt[,3])
            
            z_dataL[[x]]<- data.frame(year=yr[[x]],country=unique(tt$country),per40=a,per60=b,val=tt[,3] ,Z_shock=var[[v]])
            z_temp<- do.call(rbind,z_dataL)
            
      }
      z_yL[[v]]<- z_temp
      
      cat(paste("Shock", var[[v]], "it was done!!!\n"))
      
}

gdata<- do.call(rbind,z_yL)
gdata<- as.data.frame(gdata)
# gdata<- gdata %>% spread(Z_shock, zscore)
write.csv(gdata,file = paste("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/RTBFiles/ShocksYields/temporal/",
                             "NumericCategoriesFAO.csv", sep = ""))

temp_data<- list()
# v=1
for ( v in 1:length(var)){
      
      tempv<- filter(gdata, Z_shock==var[v])
      ## logica de los valores 
      nn<-  which(tempv[,"val"]< tempv[,"per40"])
      pp<-  which(tempv[,"val"]> tempv[,"per60"])
      mr<-  which(tempv[,"val"]>= tempv[,"per40"] & tempv[,"val"]<= tempv[,"per60"])
      
      # desempe�o 
      Negative<-  c(nn)
      Positive<- c(pp) 
      middleRange<- c(mr) 
      
      
      # copia
      tanz<- tempv
      tanz$trend<- NA
      tanz$trend[Negative] <- "Negative"
      tanz$trend[Positive]<- "Positive"
      tanz$trend[middleRange]<- "Middle_Range"
      tanz<-tanz[,c("year","country","trend")]
      # tanz$shock<- var[v]
      colnames(tanz)[3]<- paste("cat_",var[v],sep = "")
      
      temp_data[[v]]<- tanz
      
      
      cat(paste("Shock", var[v], "it was done!!!\n"))
      
}


### processing 
sdata<- do.call(cbind, temp_data)
sdata<- sdata[,-c(4,5,7,8,10,11,13,14,16,17)]
cols<- c("cat_localHV","cat_localShock_smooth_sma","cat_localShock_smooth_ema","cat_f_hvW","cat_f_smaW","cat_f_emaW")
# sdata<- sdata %>% dplyr::mutate_at(funs(factor(.)), cols)
sdata$cat_localHV<- as.character(sdata$cat_localHV)
sdata$cat_localShock_smooth_sma<- as.character(sdata$cat_localShock_smooth_sma)
sdata$cat_localShock_smooth_ema<- as.character(sdata$cat_localShock_smooth_ema)
sdata$cat_f_hvW<- as.character(sdata$cat_f_hvW)
sdata$cat_f_smaW<- as.character(sdata$cat_f_smaW)
sdata$cat_f_emaW<- as.character(sdata$cat_f_emaW)
sdata$country<- as.character(sdata$country)
sdata<- na.omit(sdata)
rownames(sdata)<- 1:nrow(sdata)

### America
treal1<- sdata %>% dplyr::filter(country %in% caribbean) %>% dplyr::mutate(., zone="caribbean")
treal2<- sdata %>% dplyr::filter(country %in% Central_America) %>% dplyr::mutate(., zone="Central_America")
treal3<- sdata %>% dplyr::filter(country %in% South_America) %>% dplyr::mutate(., zone="South_America")
treal4<- sdata %>% dplyr::filter(country %in% Northern_America) %>% dplyr::mutate(., zone="Northern_America")

##Asia
treal5<- sdata %>% dplyr::filter(country %in% Eastern_Asia) %>% dplyr::mutate(., zone="Eastern_Asia")
treal6<- sdata %>% dplyr::filter(country %in% South_Eastern_Asia) %>% dplyr::mutate(., zone="South_Eastern_Asia")
treal7<- sdata %>% dplyr::filter(country %in% Southern_Asia) %>% dplyr::mutate(., zone="Southern_Asia")
treal8<- sdata %>% dplyr::filter(country %in% Western_Asia) %>% dplyr::mutate(., zone="Western_Asia")
treal9<- sdata %>% dplyr::filter(country %in% Central_Asia) %>% dplyr::mutate(., zone="Central_Asia")


##Africa
treal10<- sdata %>% dplyr::filter(country %in% Eastern_Africa) %>% dplyr::mutate(., zone="Eastern_Africa")
treal11<- sdata %>% dplyr::filter(country %in% Middle_Africa) %>% dplyr::mutate(., zone="Middle_Africa")
treal12<- sdata %>% dplyr::filter(country %in% Northern_Africa) %>% dplyr::mutate(., zone="Northern_Africa")
treal13<- sdata %>% dplyr::filter(country %in% Southern_Africa) %>% dplyr::mutate(., zone="Southern_Africa")
treal14<- sdata %>% dplyr::filter(country %in% Western_Africa) %>% dplyr::mutate(., zone="Western_Africa")

##Oceania
treal15<- sdata %>% dplyr::filter(country %in% Australia_and_New_Zealand) %>% dplyr::mutate(., zone="Australia_and_New_Zealand")
treal16<- sdata %>% dplyr::filter(country %in% micro_poli_melanesia) %>% dplyr::mutate(., zone="micro_poli_melanesia")


##EUROPA
treal17<- sdata %>% dplyr::filter(country %in% Eastern_Europe) %>% dplyr::mutate(., zone="Eastern_Europe")
treal18<- sdata %>% dplyr::filter(country %in% Northern_Europe) %>% dplyr::mutate(., zone="Northern_Europe")
treal19<- sdata %>% dplyr::filter(country %in% Southern_Europe) %>% dplyr::mutate(., zone="Southern_Europe")
treal20<- sdata %>% dplyr::filter(country %in% Western_Europe) %>% dplyr::mutate(., zone="Western_Europe")

##### apilan los subsets por regiones
r<- rbind(treal1,treal2,treal3,treal4,treal5,treal6,treal7,treal8,treal9,treal10,treal11,treal12,treal13,treal14,treal15,
          treal16,treal17,treal18,treal19, treal20)


# sdata<- sdata %>%  mutate(., zone=ifelse((country %in% lac),"LAC", "WORLD"))

### holt and winter
t1<- table(r$cat_localHV,r$cat_f_hvW)
flocal<- margin.table(t1,1)
fforeign<- margin.table(t1,2)

hv<- prop.table(t1)
write.csv(hv,file = paste("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/RTBFiles/",
                          "Years_hv_shocksLocal&ForeighFAO.csv", sep = ""))

#### sma 
t1<-table(r$cat_localShock_smooth_sma,r$cat_f_smaW)
flocal<- margin.table(t1,1)
fforeign<- margin.table(t1,2)

sma_t<- prop.table(t1)
write.csv(sma_t,file = paste("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/RTBFiles/",
                             "Years_sma_shocksLocal&ForeighFAO.csv", sep = ""))

### ema
t1<-table(r$cat_localShock_smooth_ema,r$cat_f_emaW)
flocal<- margin.table(t1,1)
fforeign<- margin.table(t1,2)

ema_t<- prop.table(t1)
write.csv(sma_t,file = paste("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/RTBFiles/",
                             "Years_ema_shocksLocal&ForeighFAO.csv", sep = ""))



###################################### 

write.csv(r,file = paste("//dapadfs/workspace_cluster_6/Socioeconomia/GF_and_SF/RTBFiles/",
                             "Categories_shocksLocal&ForeighFAO.csv", sep = ""))

