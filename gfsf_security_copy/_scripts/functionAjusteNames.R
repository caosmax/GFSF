
ajusteNames<- function(data){
      data$Crop<- plyr::revalue(data$Crop, c("Consumption|Agriculture|All Crops"="All crops",          
                                                 "Consumption|Agriculture|Energy|All Crops"="Energy all crops",
                                                 "Consumption|Agriculture|Energy|Oil Crops"="Energy oil crops",   
                                                 "Consumption|Agriculture|Energy|Sugar Crops"="Energy sugar crops",
                                                 "Consumption|Agriculture|Feed|All Crops"="Feed all crops",     
                                                 "Consumption|Agriculture|Feed|Maize"="Feed maize",
                                                 "Consumption|Agriculture|Feed|Oil Crops"="Feed oil crops",     
                                                 "Consumption|Agriculture|Feed|Other Crops"="Feed other crops",
                                                 "Consumption|Agriculture|Feed|Wheat"="Feed wheat",         
                                                 "Consumption|Agriculture|Food|All Crops"="Food all crops",
                                                 "Consumption|Agriculture|Food|Maize"="Food maize",         
                                                 "Consumption|Agriculture|Food|Oil Crops"="Food oil crops",
                                                 "Consumption|Agriculture|Food|Other Crops"="Food other crops",   
                                                 "Consumption|Agriculture|Food|Other Grain"="Food other grain",
                                                 "Consumption|Agriculture|Food|Sugar Crops"="Food sugar crops",   
                                                 "Consumption|Agriculture|Food|Wheat"="Food wheat",
                                                 "Consumption|Agriculture|Maize"="Maize",              
                                                 "Consumption|Agriculture|Oil Crops"="Oil crops",
                                                 "Consumption|Agriculture|Other Crops"="Other crops",        
                                                 "Consumption|Agriculture|Other Grain"="Other grain",
                                                 "Consumption|Agriculture|Other|All Crops"="Other all crops",    
                                                 "Consumption|Agriculture|Other|Energy Crops"="Other energy crops",
                                                 "Consumption|Agriculture|Other|Maize"="Other maize",        
                                                 "Consumption|Agriculture|Other|Oil Crops"="Other oil crops",
                                                 "Consumption|Agriculture|Other|Other Crops"="Other other crops",  
                                                 "Consumption|Agriculture|Other|Other Grain"="Other Other grain",
                                                 "Consumption|Agriculture|Other|Sugar Crops"="Other sugar crops",  
                                                 "Consumption|Agriculture|Other|Wheat"="Other wheat",
                                                 "Consumption|Agriculture|Sugar Crops"="Sugar crops",        
                                                 "Consumption|Agriculture|Wheat"="wheat",
                                                 "Consumption|Agriculture|Energy|Other Crops"="Energy other crops",
                                                 "Consumption|Agriculture|Feed|Other Grain"="Feed other grain",   
                                                 "Consumption|Agriculture|Feed|Sugar Crops"="Feed sugar crops",
                                                 "Consumption|Agriculture|Energy|Energy Crops"="Energy energy crops",
                                                 "Consumption|Agriculture|Energy|Fiber Crops"="Energy fiber crops",
                                                 "Consumption|Agriculture|Energy|Maize"="Energy maize",
                                                 "Consumption|Agriculture|Energy|Other Grain"="Energy other grain",
                                                 "Consumption|Agriculture|Energy|Rice"="Energy rice",        
                                                 "Consumption|Agriculture|Energy|Roots Tuber"="Energy roots tubers",
                                                 "Consumption|Agriculture|Energy|Wheat"="Energy wheat",       
                                                 "Consumption|Agriculture|Feed|Energy Crops"="Feed energy crops",
                                                 "Consumption|Agriculture|Feed|Fiber Crops"="Feed fiber crops",   
                                                 "Consumption|Agriculture|Feed|Rice"="Feed rice",
                                                 "Consumption|Agriculture|Feed|Roots Tuber"="Feed roots tubers",   
                                                 "Consumption|Agriculture|Fiber Crops"="Fiber crops",
                                                 "Consumption|Agriculture|Food|Energy Crops"="Food energy crops",  
                                                 "Consumption|Agriculture|Food|Fiber Crops"="Food fiber crops",
                                                 "Consumption|Agriculture|Food|Rice"="Food rice",          
                                                 "Consumption|Agriculture|Food|Roots Tuber"="Food roots tubers",
                                                 "Consumption|Agriculture|Other|Fiber Crops"="Other fiber crops",  
                                                 "Consumption|Agriculture|Other|Rice"="Other rice",
                                                 "Consumption|Agriculture|Other|Roots Tuber"="Other roots tuber",  
                                                 "Consumption|Agriculture|Rice"="Rice",
                                                 "Consumption|Agriculture|Roots Tuber"="Roots Tuber",
                                                 "Food Security|Food Production Value|Agricultural Sector"="ValueAgrSector",
                                                 "Food Security|Food Production Value|Food Processing Sector"="ValueFoodProcSector",
                                                 "Production|Agriculture|All Crops"="All crops",
                                                 "Production|Agriculture|Maize"="Maize",                              
                                                 "Production|Agriculture|Oil Crops"="Oil crops",
                                                 "Production|Agriculture|Other Crops"="Other crops",                        
                                                 "Production|Agriculture|Other Grain"="Other grain",
                                                 "Production|Agriculture|Sugar Crops"="Sugar crops",                        
                                                 "Production|Agriculture|Wheat"="Wheat",
                                                 "Production|Agriculture|Energy Crops"="Energy crops",                       
                                                 "Production|Agriculture|Fiber Crops"="Fiber crops",
                                                 "Production|Agriculture|Rice"="Rice",                               
                                                 "Production|Agriculture|Roots Tuber"="Roots tuber",
                                                 "Agriculture|All Crops|Export|Gross"="All crops gross",
                                                 "Agriculture|All Crops|Export|Net"="All crops net",                  
                                                 "Agriculture|Maize|Export|Gross"="Maize gross",
                                                 "Agriculture|Maize|Export|Net"="Maize net",                      
                                                 "Agriculture|Oil Crops|Export|Gross"="Oil crops gross",
                                                 "Agriculture|Oil Crops|Export|Net"="Oil crops net",                  
                                                 "Agriculture|Other Grain|Export|Gross"="Other grain gross",
                                                 "Agriculture|Other Grain|Export|Net"="Other grain net",                
                                                 "Agriculture|Sugar Crops|Export|Gross"="Sugar crops gross",
                                                 "Agriculture|Sugar Crops|Export|Net"="Sugar crops net",                
                                                 "Agriculture|Wheat|Export|Gross"="Wheat gross",
                                                 "Agriculture|Wheat|Export|Net"="Wheat net",                      
                                                 "Export|Gross"="Gross",
                                                 "Export|Net"="Net",                                        
                                                 "Agriculture|Energy Crops|Export|Gross"="Energy crops gross",             
                                                 "Agriculture|Energy Crops|Export|Net"="Energy crops net",
                                                 "Agriculture|Fiber Crops|Export|Gross"="Fiber crops gross",              
                                                 "Agriculture|Fiber Crops|Export|Net"="Fiber crops net",
                                                 "Agriculture|Rice|Export|Gross"="Rice gross",                     
                                                 "Agriculture|Rice|Export|Net"="Rice net",
                                                 "Agriculture|Roots Tuber|Export|Gross"="Roots tuber gross",              
                                                 "Agriculture|Roots Tuber|Export|Net"="Roots tuber net",
                                                 "Agriculture|All Crops|Import|Gross"="All crops gross",
                                                 "Agriculture|Maize|Import|Gross"="Maize gross",                    
                                                 "Agriculture|Oil Crops|Import|Gross"="Oil crops gross" ,
                                                 "Agriculture|Other Grain|Import|Gross"="Other grain gross",              
                                                 "Agriculture|Sugar Crops|Import|Gross"="Sugar crops gross",
                                                 "Agriculture|Wheat|Import|Gross"="Wheat gross",                    
                                                 "Import|Gross"="Gross", 
                                                 "Agriculture|Energy Crops|Import|Gross"="Energy crops gross",             
                                                 "Agriculture|Fiber Crops|Import|Gross"="Fiber crops gross",
                                                 "Agriculture|Rice|Import|Gross"="Rice gross",                     
                                                 "Agriculture|Roots Tuber|Import|Gross"="Root tuber gross"))
      return(data)
      
}

ajusteNamesYield<- function(data){
      data$Crop<- plyr::revalue(data$Crop, c("Agriculture|YEXO|Maize"="Maize",
                                             "Agriculture|YEXO|Oil Crops"="Oil crops",
                                             "Agriculture|YEXO|Other Grain"="Other grain", 
                                             "Agriculture|YEXO|Sugar Crops"="Sugar crops",
                                             "Agriculture|YEXO|Wheat"="Wheat",
                                             "Agriculture|YEXO|All Crops"="All crops",   
                                             "Agriculture|YEXO|Energy Crops"="Energy crops",
                                             "Agriculture|YEXO|Fiber Crops"="Fiber crops",
                                             "Agriculture|YEXO|Rice"="Rice",        
                                             "Agriculture|YEXO|Roots Tuber"="Roots tuber" ))
      return(data)
      
}


ajusteNamesLand<- function(data){
      data$Crop<- plyr::revalue(data$Crop, c("Land Cover|Cropland"="Cropland",  
                                             "Land Cover|Cropland|All Crops"="All crops",   
                                             "Land Cover|Cropland|Maize"="Maize",  
                                             "Land Cover|Cropland|Oil Crops"= "Oil crops",
                                             "Land Cover|Cropland|Other Crops"="Other crops", 
                                             "Land Cover|Cropland|Other Grain"="Other grain",
                                             "Land Cover|Cropland|Sugar Crops"="Sugar crops", 
                                             "Land Cover|Cropland|Wheat"="Wheat" ,      
                                             "Land Cover|Forest"="forest",   
                                             "Land Cover|Other Arable Land"="Other arable land",   
                                             "Land Cover|Other Land"="Other land" ,          
                                             "Land Cover|Pasture"="Pasture",  
                                             "Land Cover|Cropland|Energy Crops"="Energy crops", 
                                             "Land Cover|Cropland|Fiber Crops"="Fiber crops", 
                                             "Land Cover|Cropland|Rice"="Rice",   
                                             "Land Cover|Cropland|Roots Tuber"="Roots tuber" ))
      return(data)
}


ajusteNamesPrice<- function(data){
      data$Crop<- plyr::revalue(data$Crop, c( "Price|Agriculture|All Crops"="All crops", 
                                                "Price|Agriculture|Maize"="Maize",       
                                                "Price|Agriculture|Oil Crops"="Oil crops",   
                                                "Price|Agriculture|Other Crops"="Other crops",
                                                "Price|Agriculture|Other Grain"="Other grain",
                                                "Price|Agriculture|Sugar Crops"="Sugar crops", 
                                                "Price|Agriculture|Wheat"="Wheat", 
                                                "Price|Carbon"="Carbon",
                                                "Price|Primary Energy|Coal"="Primary energy coal",     
                                                "Price|Primary Energy|Gas"="Primary energy gas", 
                                                "Price|Primary Energy|Oil"="Primary energy",
                                                "Price|Agriculture|Energy Crops"="Energy crops",
                                                "Price|Agriculture|Fiber Crops"="Fiber crops", 
                                                "Price|Agriculture|Rice"="Rice",
                                                "Price|Agriculture|Roots Tuber"="Roots tuber", 
                                                "Price|Primary Energy|Biomass"="Biomass" ))
      return(data)
}

