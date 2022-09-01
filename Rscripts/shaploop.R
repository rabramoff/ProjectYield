######Shap loop#####
do_shaps <- function(Delta_temp_level, CO2.ppm_level, Adaptation_level, mod.rf.map, do_shap, TAB_in, levelindex, k){

  Adaptation_levels = c("No","Yes")
  
  if(k==2 | k==4){ 
  Adaptation_levels = c(1,2)
  }
  
    NewData <- data.frame(Delta_temp=rep(Delta_temp_level, nrow(TAB_in)),
                        CO2.ppm=rep(CO2.ppm_level,nrow(TAB_in)),
                        Adaptation=rep(Adaptation_levels[levelindex],nrow(TAB_in)),
                        TempAvg=TAB_in$Tave, 
                        Latitude=TAB_in$Latitude, 
                        Longitude=TAB_in$Longitude, 
                        Precip=TAB_in$Pr_avg, 
                        Delta_precip=TAB_in$PrChange)
  
  #Prediction RF
  if(k==2 | k==4){
  Pred<-predict(mod.rf.map,data=NewData)$predictions
  }else{
    if(k==3){
      Pred<-predict(mod.rf.map,newdata=NewData)
    }else{
      drop <- c("Latitude","Longitude")
      NewData = NewData[,!(names(NewData) %in% drop)]
      Pred<-predict(mod.rf.map,newdata=NewData)
    }
  }
  
    NewData$Effect <- Pred
    if (k==1){
      NewData$Latitude <- TAB_in$Latitude
      NewData$Longitude <- TAB_in$Longitude
    }
    save(NewData, file=paste0(outputdir,crop_species[k], "_Nb",1,"_Adapt",levelindex,"_predicted_effect_4.5.RData"))
    
  if(do_shap){
    set.seed(1234)
    explain_rf <- DALEX::explain(model = mod.rf.map,  
                                 data = Training_map, 
                                 y = Training_map$Effect, 
                                 label = crop_species_map)  
    
    shap_co2ppm <- vector(length=dim(NewData)[1])
    shap_TempAvg <- vector(length=dim(NewData)[1])
    shap_Delta_temp <- vector(length=dim(NewData)[1])
    shap_Adaptation <- vector(length=dim(NewData)[1])
    shap_Precip <- vector(length=dim(NewData)[1])
    shap_Delta_precip <- vector(length=dim(NewData)[1])
    start.time <- Sys.time()
    for (m in 1:dim(NewData)[1]){
      shap_mean <- predict_parts(explainer = explain_rf, 
                                 new_observation = NewData[m , -which(names(NewData) %in% c("Effect"))],
                                 type = "shap",
                                 B = 1)
      shap_co2ppm[m] <- shap_mean[shap_mean$variable_name=="CO2.ppm",]$contribution[1]
      shap_TempAvg[m] <- shap_mean[shap_mean$variable_name=="TempAvg",]$contribution[1]
      shap_Delta_temp[m] <- shap_mean[shap_mean$variable_name=="Delta_temp",]$contribution[1]
      shap_Adaptation[m] <- shap_mean[shap_mean$variable_name=="Adaptation",]$contribution[1]
      shap_Precip[m] <- shap_mean[shap_mean$variable_name=="Precip",]$contribution[1]
      shap_Delta_precip[m] <- shap_mean[shap_mean$variable_name=="Delta_precip",]$contribution[1]
    }
    end.time <- Sys.time()
    end.time-start.time
    
    save(shap_co2ppm, file=paste0(outputdir,crop_species[k], "_Nb",1,"_Adapt",levelindex,"_shap_co2ppm_4.5.RData"))
    save(shap_TempAvg, file=paste0(outputdir,crop_species[k],"_Nb",1,"_Adapt",levelindex,"_shap_TempAvg_4.5.RData"))
    save(shap_Delta_temp, file=paste0(outputdir,crop_species[k],"_Nb",1,"_Adapt",levelindex,"_shap_Delta_temp_4.5.RData"))
    save(shap_Adaptation, file=paste0(outputdir,crop_species[k],"_Nb",1,"_Adapt",levelindex,"_shap_Adaptation_4.5.RData"))
    save(shap_Precip, file=paste0(outputdir,crop_species[k],"_Nb",1,"_Adapt",levelindex,"_shap_Precip_4.5.RData"))
    save(shap_Delta_precip, file=paste0(outputdir,crop_species[k],"_Nb",1,"_Adapt",levelindex,"_shap_Delta_precip_4.5.RData"))
    
    return("shaps done")
  }
}


map_of_scenario <- function(AdaptLevel){
  
  NewData <- data.frame(Delta_temp=rep(2,nrow(TAB_in)),CO2.ppm=rep(116,nrow(TAB_in)),Adaptation=rep(AdaptLevel,nrow(TAB_in)),TempAvg=TAB_in$Tave, Latitude=TAB_in$Latitude, Longitude=TAB_in$Longitude, Precip=TAB_in$Pr_avg, Delta_precip=TAB_in$PrChange)
  
  LAT<-seq(83.75,-55.75, by= -0.5)
  LONG<-seq(-179.75,179.75,by=0.5)
  
  MAT <- raster(ncol=720, nrow=280, xmn=-179.75, xmx=179.75, ymn=-55.75, ymx=83.75)
  U <- matrix(NA, nrow=280, ncol=720)
  V <- matrix(NA, nrow=280, ncol=720)
  W <- matrix(NA, nrow=280, ncol=720)
  X <- matrix(NA, nrow=280, ncol=720)
  Y <- matrix(NA, nrow=280, ncol=720)
  Z <- matrix(NA, nrow=280, ncol=720)
  
  List_of_latitude<-unique(NewData$Latitude)
  
  for (i in List_of_latitude) {
    
    LONG_i=(1:720)[LONG%in%NewData$Longitude[NewData$Latitude==i]]
    LAT_i=(1:280)[LAT==i]
    
    U[LAT_i, LONG_i]<-shap_Delta_precip[NewData$Latitude==i]
    V[LAT_i, LONG_i]<-shap_Precip[NewData$Latitude==i]
    W[LAT_i, LONG_i]<-shap_TempAvg[NewData$Latitude==i]
    X[LAT_i, LONG_i]<-shap_Delta_temp[NewData$Latitude==i]
    Y[LAT_i, LONG_i]<-shap_co2ppm[NewData$Latitude==i]
    Z[LAT_i, LONG_i]<-shap_Adaptation[NewData$Latitude==i]
    
  }
  
  MATU<-setValues(MAT, U)
  MATV<-setValues(MAT, V)
  MATW<-setValues(MAT, W)
  MATX<-setValues(MAT, X)
  MATY<-setValues(MAT, Y)
  MATZ<-setValues(MAT, Z)
  
  test_dfU <- as.data.frame(as(MATU, "SpatialPixelsDataFrame"))
  colnames(test_dfU) <- c("Delta_precip", "x", "y") 
  test_dfV <- as.data.frame(as(MATV, "SpatialPixelsDataFrame"))
  colnames(test_dfV) <- c("Precip", "x", "y") 
  test_dfW <- as.data.frame(as(MATW, "SpatialPixelsDataFrame"))
  colnames(test_dfW) <- c("TempAvg", "x", "y")
  test_dfX <- as.data.frame(as(MATX, "SpatialPixelsDataFrame"))
  colnames(test_dfX) <- c("Delta_temp", "x", "y")
  test_dfY <- as.data.frame(as(MATY, "SpatialPixelsDataFrame"))
  colnames(test_dfY) <- c("co2ppm", "x", "y")
  test_dfZ <- as.data.frame(as(MATZ, "SpatialPixelsDataFrame"))
  colnames(test_dfZ) <- c("Adaptation", "x", "y")
  
  test_df <- test_dfU %>% inner_join(test_dfV) %>% inner_join(test_dfW) %>%inner_join(test_dfX) %>% inner_join(test_dfY) %>% inner_join(test_dfZ) %>% tidyr::pivot_longer(c("Delta_precip","Precip","TempAvg","Delta_temp","co2ppm","Adaptation"), names_to = "Variable", values_to = "Effect")
  
  test_df$Delta_temp_level <- rep(2, dim(test_df)[1])
  test_df$CO2.ppm_level <- rep(116, dim(test_df)[1])
  test_df$Adaptation_level <- rep(AdaptLevel, dim(test_df)[1])
  
  return(test_df)
}
