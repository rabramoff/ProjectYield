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
    #NewData <- NewData[,-c(7)]
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
    }
    end.time <- Sys.time()
    end.time-start.time
    
    save(shap_co2ppm, file=paste0(outputdir,crop_species[k], "_Nb",1,"_Adapt",levelindex,"_shap_co2ppm_4.5.RData"))
    save(shap_TempAvg, file=paste0(outputdir,crop_species[k],"_Nb",1,"_Adapt",levelindex,"_shap_TempAvg_4.5.RData"))
    save(shap_Delta_temp, file=paste0(outputdir,crop_species[k],"_Nb",1,"_Adapt",levelindex,"_shap_Delta_temp_4.5.RData"))
    save(shap_Adaptation, file=paste0(outputdir,crop_species[k],"_Nb",1,"_Adapt",levelindex,"_shap_Adaptation_4.5.RData"))
    save(shap_Precip, file=paste0(outputdir,crop_species[k],"_Nb",1,"_Adapt",levelindex,"_shap_Precip_4.5.RData"))
    
    return("shaps done")
  }
}

# do_rasters <- function(rcp45,do_shap){
# # #Delta_temp_level = 2; CO2.ppm_level=0; Adaptation_level=1
# if(rcp45){
#   Delta_temp_levels = c(2,2)
#   CO2.ppm_levels = c(116,116)
#   Adaptation_levels = c("No","Yes") #c(1,2)
#   #Delta_precip_levels = predict(mod.pr.change, )
# }else{
#   Delta_temp_levels = rep(c(1.5, 2, 3), 4)
#   CO2.ppm_levels = rep(c(rep(0,3), rep(210,3)),2)
#   Adaptation_levels = c(rep("No",6), rep("Yes",6))#c(rep(1,6), rep(2,6))
# }
# 
# map_df <- NULL
# map_df_all <- NULL
# map_df_all_crop <- NULL
# 
# ######Projection mapping#####
# #approx 3.5 mins
# start <- Sys.time()
# numbers <- c(1,2,3,4)
# for(k in numbers){ 
#   crop_species_map = crop_species[[k]]
#   #Filter by cropping areas
#   Area_crop<-read.csv(paste0(mdatadir,crop_species_map,"_country.csv"), header=T)
#   names(Area_crop)<-c("ID","Longitude","Latitude","IRC","RFC","Total","Country","Flag")
#   TAB_in<-right_join(Area_crop,InputData, by=c("Latitude","Longitude"))
#   TAB_in<-TAB_in[TAB_in$Total*TAB_in$Flag>0,]
#   #N<-nrow(TAB_in)
#   
#     mod.rf.map <- save.mod.rf[[1]][[k]]
#     Training_map <- save.Training.rf[[1]][[k]]
# 
#     for(i in 1:length(Delta_temp_levels)){
#       map_df[[i]] <- do_shaps(Delta_temp_levels[i], CO2.ppm_levels[i], Adaptation_levels[i], mod.rf.map, do_shap, TAB_in, levelindex,k)
#     }
#    
# 
#     map_df_all[[1]] <- do.call(rbind.data.frame,map_df)
# 
#     map_df_all_crop[[k]] <- map_df_all[[1]]
#   
# }
# end <- Sys.time()
# save.time <- end - start
# 
# # #Effect is the only different column - rbind to combine all Nbs, labelling the crop
# # 
# # map_df_all_maize <- do.call(rbind.data.frame,map_df_all_crop[[1]])
# # map_df_all_maize$Crop <- rep(crop_species[1], dim(map_df_all_maize)[1])
# # 
# # map_df_all_rice <- do.call(rbind.data.frame,map_df_all_crop[[2]])
# # map_df_all_rice$Crop <- rep(crop_species[2], dim(map_df_all_rice)[1])
# # 
# # map_df_all_wheat <- do.call(rbind.data.frame,map_df_all_crop[[3]])
# # map_df_all_wheat$Crop <- rep(crop_species[3], dim(map_df_all_wheat)[1])
# # 
# # map_df_all_soybean <- do.call(rbind.data.frame,map_df_all_crop[[4]])
# # map_df_all_soybean$Crop <- rep(crop_species[4], dim(map_df_all_soybean)[1])
# # 
# # #need to figure out why it is the size it is
# # #2515081+1139760+937920+2296440
# # map_combined <- rbind(map_df_all_maize, map_df_all_rice, map_df_all_wheat, map_df_all_soybean)
# # map_combined$Effect <- as.numeric(map_combined$Effect)
# # map_combined$x <- as.numeric(map_combined$x)
# # map_combined$y <- as.numeric(map_combined$y)
# # map_combined$CO2.ppm_level <- as.numeric(map_combined$CO2.ppm_level)
# # map_combined$Delta_temp_level <- as.numeric(map_combined$Delta_temp_level)
# # 
# # rm(map_df, map_df_all, map_df_all_crop, map_df_all_maize, map_df_all_rice, map_df_all_wheat, map_df_all_soybean)
# # 
# # #average Nbs for each crop
# # map_summary <- map_combined %>%
# #   group_by(x, y, Delta_temp_level, CO2.ppm_level, Adaptation_level, Crop) %>%
# #   dplyr::summarise(Mean_Effect = mean(Effect), 
# #                    SE_Effect = sd(Effect)/sqrt(n()),
# #                    n = n())
# # 
# # # New facet label names for Adaptation variable
# # adpt.labs <- c("No Adaptation", "Adaptation")
# # names(adpt.labs) <- c("1", "2")
# # 
# # # New facet label names for CO2 variable
# # co2.labs <- c("390ppm", "600ppm")
# # names(co2.labs) <- c("0", "210")
# # 
# # map_summary$Crop <- as.factor(map_summary$Crop)
# # levels(map_summary$Crop) <- c("Maize", "Rice", "Wheat", "Soybean")
# # 
# # plot_it_noleg <- function(mapNum, gglabel, crop_map) {gg1 + geom_tile(data=mapNum[mapNum$Crop==crop_map,], aes(x=x, y=y, fill=Mean_Effect), alpha=0.8) + scale_fill_gradient2(midpoint=0, low="brown", mid="white", high="darkgreen", limits=c(-100,150), guide = F) + 
# #     ggtitle(paste0(crop_map,": +", gglabel, "°C")) +
# #     facet_grid(Adaptation_level ~ CO2.ppm_level, labeller = labeller(Adaptation_level = adpt.labs, CO2.ppm_level = co2.labs)) +
# #     theme(#text=element_text(size=8), #change font size of all text
# #       axis.text.x=element_text(size=8, angle = 45, vjust = 0.5), #change font size of axis text
# #       axis.text.y=element_text(size=8), #change font size of axis titles
# #       plot.title=element_text(size=8), #change font size of plot title
# #       legend.text=element_text(size=8), #change font size of legend text
# #       legend.title=element_text(size=8)) #change font size of legend title
# # }
# # 
# # plot_it <- function(mapNum, gglabel, crop_map) {gg1 + geom_tile(data=mapNum[mapNum$Crop==crop_map,], aes(x=x, y=y, fill=Mean_Effect), alpha=0.8) + scale_fill_gradient2(midpoint=0, low="brown", mid="white", high="darkgreen", limits=c(-100,100), name="Yield Change (%)") + 
# #     ggtitle(paste0(crop_map,": +", gglabel, "°C")) +
# #     facet_grid(Adaptation_level ~ CO2.ppm_level, labeller = labeller(Adaptation_level = adpt.labs, CO2.ppm_level = co2.labs)) +
# #     theme(#text=element_text(size=8), #change font size of all text
# #       axis.text.x=element_text(size=8, angle = 45, vjust = 0.5), #change font size of axis text
# #       axis.text.y=element_text(size=8), #change font size of axis titles
# #       plot.title=element_text(size=8), #change font size of plot title
# #       legend.text=element_text(size=8), #change font size of legend text
# #       legend.title=element_text(size=8)) #change font size of legend title
# # }
# # 
# # ###Plot####
# # if(rcp45){
# #     pdf(file=paste0(figoutdir,"allcrops_rcp45",rcp45,".pdf"), height=6, width=9)
# #     print(gg1 + geom_tile(data=map_summary, aes(x=x, y=y, fill=Mean_Effect), alpha=0.8) + scale_fill_gradient2(midpoint=0, low="brown", mid="white", high="darkgreen", limits=c(-100,100), name="Yield Change (%)") + 
# #       ggtitle(paste0("RCP 4.5")) +
# #       facet_grid(Adaptation_level ~ Crop, labeller = labeller(Adaptation_level = adpt.labs)) +
# #       theme(#text=element_text(size=8), #change font size of all text
# #         axis.text.x=element_text(size=8, angle = 45, vjust = 0.5), #change font size of axis text
# #         axis.text.y=element_text(size=8), #change font size of axis titles
# #         plot.title=element_text(size=8), #change font size of plot title
# #         legend.text=element_text(size=8), #change font size of legend text
# #         legend.title=element_text(size=8)) #change font size of legend title
# #     )
# #     dev.off()
# # }else{
# #   map1.5 <- map_summary[map_summary$Delta_temp_level == "1.5",]
# #   map2 <- map_summary[map_summary$Delta_temp_level == "2",]
# #   map3 <- map_summary[map_summary$Delta_temp_level == "3",]
# #   
# #   for(i in 1:length(crop_species)){
# #     gg1.5 <- plot_it_noleg(map1.5, "1.5", crop_species[i])
# #     gg2 <- plot_it_noleg(map2, "2", crop_species[i])
# #     gg3 <- plot_it_noleg(map3, "3", crop_species[i])
# #     
# #     pdf(file=paste0(figoutdir,crop_species[i],"_rcp45",rcp45,".pdf"), height=6, width=8)
# #     multiplot(gg1.5, gg3, gg2, cols=2)
# #     dev.off()
# #   }
# #   
# #   pdf(file=paste0(figoutdir,"legend.pdf"), height=11, width=8)
# #   plot_it(map1.5, "1.5", "Maize")
# #   dev.off()
# # }
# # 
# # return(map_summary)
# }

#2 = Yes, 1 = No (Yes is default??) #If adapt looks same then forgot to run model as Yes/No

map_of_scenario <- function(AdaptLevel){
  
  NewData <- data.frame(Delta_temp=rep(2,nrow(TAB_in)),CO2.ppm=rep(116,nrow(TAB_in)),Adaptation=rep(AdaptLevel,nrow(TAB_in)),TempAvg=TAB_in$Tave, Latitude=TAB_in$Latitude, Longitude=TAB_in$Longitude, Precip=TAB_in$Pr_avg, Delta_precip=TAB_in$PrChange)
  
  LAT<-seq(83.75,-55.75, by= -0.5)
  LONG<-seq(-179.75,179.75,by=0.5)
  
  MAT <- raster(ncol=720, nrow=280, xmn=-179.75, xmx=179.75, ymn=-55.75, ymx=83.75)
  V <- matrix(NA, nrow=280, ncol=720)
  W <- matrix(NA, nrow=280, ncol=720)
  X <- matrix(NA, nrow=280, ncol=720)
  Y <- matrix(NA, nrow=280, ncol=720)
  Z <- matrix(NA, nrow=280, ncol=720)
  
  List_of_latitude<-unique(NewData$Latitude)
  
  for (i in List_of_latitude) {
    
    LONG_i=(1:720)[LONG%in%NewData$Longitude[NewData$Latitude==i]]
    LAT_i=(1:280)[LAT==i]
    
    V[LAT_i, LONG_i]<-shap_Precip[NewData$Latitude==i]
    W[LAT_i, LONG_i]<-shap_TempAvg[NewData$Latitude==i]
    X[LAT_i, LONG_i]<-shap_Delta_temp[NewData$Latitude==i]
    Y[LAT_i, LONG_i]<-shap_co2ppm[NewData$Latitude==i]
    Z[LAT_i, LONG_i]<-shap_Adaptation[NewData$Latitude==i]
    
  }
  
  MATV<-setValues(MAT, V)
  MATW<-setValues(MAT, W)
  MATX<-setValues(MAT, X)
  MATY<-setValues(MAT, Y)
  MATZ<-setValues(MAT, Z)
  
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
  
  test_df <- test_dfV %>% inner_join(test_dfW) %>%inner_join(test_dfX) %>% inner_join(test_dfY) %>% inner_join(test_dfZ) %>% tidyr::pivot_longer(c("Precip","TempAvg","Delta_temp","co2ppm","Adaptation"), names_to = "Variable", values_to = "Effect")
  
  test_df$Delta_temp_level <- rep(2, dim(test_df)[1])
  test_df$CO2.ppm_level <- rep(116, dim(test_df)[1])
  test_df$Adaptation_level <- rep(AdaptLevel, dim(test_df)[1])
  
  return(test_df)
}
