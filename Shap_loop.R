######Shap loop#####
######Load libraries#####
library(mapdata)
library(dplyr)
library(ggplot2)
library(ranger)
library(raster)
library(DALEX)
source('multiplot.R')

#####Switches#####
same_locations = F
crop_species = c("Maize","Rice","Wheat","Soybean")
adaptation_types = F
rcp45 = F
do_shap = F

######Load models and data#####
load(file=paste0("saved_MLs_samelocs",same_locations,"_adaptationtypes",adaptation_types,"_modrf_allTRUE.RData"))
load(file=paste0("saved_MLs_samelocs",same_locations,"_adaptationtypes",adaptation_types,"_trainingrf_allTRUE.RData"))
mdatadir <- "/Users/rzabramoff/ownCloud/Collaborations/Makowski_yield/Data/"
TAB_p<-read.csv(paste0(mdatadir,"tave_201120.csv"), header=T)
TAB_p<-na.omit(TAB_p)

#####Maps prep#####
makelabelsEW <- function(x) {ifelse(x < 0, parse(text=paste0(x,"^o", "*W")), ifelse(x > 0, parse(text=paste0(x,"^o", "*E")),x))}
makelabelsNS <- function(x) {ifelse(x < 0, parse(text=paste0(x,"^o", "*S")), ifelse(x > 0, parse(text=paste0(x,"^o", "*N")),x))}
wrld <- map_data("world")
xbreaks <- seq(-180,180,60)
xlabels <- makelabelsEW(xbreaks)
ybreaks <- seq(-90,90,30)
ylabels <- makelabelsNS(ybreaks)
gg1 <- ggplot() + 
  geom_polygon(data = wrld, aes(x=long, y = lat, group = group), fill = "lightgray", color = "lightgray") + 
  coord_fixed(1.3) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.ticks = element_blank(), legend.position="bottom",legend.title=element_text(size=15), 
        legend.text=element_text(size=12), axis.text = element_text(size=12))+
  scale_x_continuous("", breaks = xbreaks, labels = xlabels) +
  scale_y_continuous("", breaks = ybreaks, labels = ylabels, limits=c(-60,90))

map_of_scenario <- function(Delta_temp_level, CO2.ppm_level, Adaptation_level, mod.rf.map){
  NewData <- data.frame(Delta_temp=rep(Delta_temp_level, N),CO2.ppm=rep(CO2.ppm_level,N),Adaptation=rep(Adaptation_level,N),TempAvg=TAB_p_f$Tave, Latitude=TAB_p_f$y, Longitude=TAB_p_f$x)
  
  #Prediction RF
  Pred_rf<-predict(mod.rf.map,data=NewData)
  
  #Whole map
  Pred=Pred_rf$predictions
  
  if(do_shap){
    NewData$Effect <- Pred
    #NewData <- NewData[,-c(7)]
  
    set.seed(1234)
    explain_rf <- DALEX::explain(model = mod.rf.map,  
                                 data = Training_map, 
                                 y = Training_map$Effect, 
                                 label = crop_species_map)  
    
    shap_co2ppm <- vector(length=dim(NewData)[1])
    shap_TempAvg <- vector(length=dim(NewData)[1])
    shap_Delta_temp <- vector(length=dim(NewData)[1])
    shap_Adaptation <- vector(length=dim(NewData)[1])
    start.time <- Sys.time()
    for (m in 1:dim(NewData)[1]){
      shap_mean <- predict_parts(explainer = explain_rf, 
                                 new_observation = NewData[m,-7], 
                                 type = "shap",
                                 B = 1)
      shap_co2ppm[m] <- shap_mean[shap_mean$variable_name=="CO2.ppm",]$contribution[1]
      shap_TempAvg[m] <- shap_mean[shap_mean$variable_name=="TempAvg",]$contribution[1]
      shap_Delta_temp[m] <- shap_mean[shap_mean$variable_name=="Delta_temp",]$contribution[1]
      shap_Adaptation[m] <- shap_mean[shap_mean$variable_name=="Adaptation",]$contribution[1]
    }
    end.time <- Sys.time()
    end.time-start.time
    
    save(shap_co2ppm, file=paste0(crop_species[k], "_Nb",j,"_Adapt",i,"_shap_co2ppm_4.5.RData"))
    save(shap_TempAvg, file=paste0(crop_species[k],"_Nb",j,"_Adapt",i,"_shap_TempAvg_4.5.RData"))
    save(shap_Delta_temp, file=paste0(crop_species[k],"_Nb",j,"_Adapt",i,"_shap_Delta_temp_4.5.RData"))
    save(shap_Adaptation, file=paste0(crop_species[k],"_Nb",j,"_Adapt",i,"_shap_Adaptation_4.5.RData"))

    return("shaps done")
  }else{
    LAT<-seq(83.75,-55.75, by= -0.5)
    LONG<-seq(-179.75,179.75,by=0.5)
    
    MAT <- raster(ncol=720, nrow=280, xmn=-179.75, xmx=179.75, ymn=-55.75, ymx=83.75)
    Z <- matrix(NA, nrow=280, ncol=720)
    
    List_of_latitude<-unique(NewData$Latitude)
    
    for (i in List_of_latitude) {
      
      LONG_i=(1:720)[LONG%in%NewData$Longitude[NewData$Latitude==i]]
      LAT_i=(1:280)[LAT==i]
      
      Z[LAT_i, LONG_i]<-Pred[NewData$Latitude==i]
      
    }
    
    MAT<-setValues(MAT, Z)
    
    test_spdf <- as(MAT, "SpatialPixelsDataFrame")
    test_df <- as.data.frame(test_spdf)
    colnames(test_df) <- c("Effect", "x", "y")
    test_df$Delta_temp_level <- rep(Delta_temp_level, dim(test_df)[1])
    test_df$CO2.ppm_level <- rep(CO2.ppm_level, dim(test_df)[1])
    test_df$Adaptation_level <- rep(Adaptation_level, dim(test_df)[1])
    
    return(test_df)
  }
}

# #Delta_temp_level = 2; CO2.ppm_level=0; Adaptation_level=1
if(rcp45){
  Delta_temp_levels = c(2,2)
  CO2.ppm_levels = c(116,116)
  Adaptation_levels = c(1,2)
}else{
  Delta_temp_levels = rep(c(1.5, 2, 3), 4)
  CO2.ppm_levels = rep(c(rep(0,3), rep(210,3)),2)
  Adaptation_levels = c(rep(1,6), rep(2,6))
}

map_df <- NULL
map_df_all <- NULL
map_df_all_crop <- NULL

######Projection mapping#####
#approx 3.5 mins
start <- Sys.time()
for(k in 1:4){
  crop_species_map = crop_species[[k]]
  #Filter by cropping areas
  Area_crop<-read.csv(paste0(mdatadir,crop_species_map,"_country.csv"), header=T)
  TAB_p_f<-right_join(Area_crop,TAB_p, by=c("x","y"))
  TAB_p_f<-TAB_p_f[TAB_p_f$Total*TAB_p_f$Flag>0,]
  N<-nrow(TAB_p_f)
  
  for (j in 1:1){
    mod.rf.map <- save.mod.rf[[j]][[k]]
    Training_map <- save.Training.rf[[j]][[k]]

    for(i in 1:length(Delta_temp_levels)){
      map_df[[i]] <- map_of_scenario(Delta_temp_levels[i], CO2.ppm_levels[i], Adaptation_levels[i], mod.rf.map)
    }
   

    map_df_all[[j]] <- do.call(rbind.data.frame,map_df)
      #rbind(map_df[[1]], map_df[[2]], map_df[[3]], map_df[[4]], map_df[[5]], 
                            # map_df[[6]], map_df[[7]], map_df[[8]], map_df[[9]], map_df[[10]], 
                            # map_df[[11]], map_df[[12]]) 
  }

    map_df_all_crop[[k]] <- do.call(list,map_df_all)
      #list(map_df_all[[1]], map_df_all[[2]], map_df_all[[3]], map_df_all[[4]], 
                                   #map_df_all[[5]], map_df_all[[6]], map_df_all[[7]], map_df_all[[8]], 
                                   #map_df_all[[9]], map_df_all[[10]]) 
  
}
end <- Sys.time()
save.time <- end - start

#Effect is the only different column - rbind to combine all Nbs, labelling the crop

map_df_all_maize <- do.call(rbind.data.frame,map_df_all_crop[[1]])
map_df_all_maize$Crop <- rep(crop_species[1], dim(map_df_all_maize)[1])

map_df_all_rice <- do.call(rbind.data.frame,map_df_all_crop[[2]])
map_df_all_rice$Crop <- rep(crop_species[2], dim(map_df_all_rice)[1])

map_df_all_wheat <- do.call(rbind.data.frame,map_df_all_crop[[3]])
map_df_all_wheat$Crop <- rep(crop_species[3], dim(map_df_all_wheat)[1])

map_df_all_soybean <- do.call(rbind.data.frame,map_df_all_crop[[4]])
map_df_all_soybean$Crop <- rep(crop_species[4], dim(map_df_all_soybean)[1])

#need to figure out why it is the size it is
#2515081+1139760+937920+2296440
map_combined <- rbind(map_df_all_maize, map_df_all_rice, map_df_all_wheat, map_df_all_soybean)
map_combined$Effect <- as.numeric(map_combined$Effect)
map_combined$x <- as.numeric(map_combined$x)
map_combined$y <- as.numeric(map_combined$y)
map_combined$CO2.ppm_level <- as.numeric(map_combined$CO2.ppm_level)
map_combined$Delta_temp_level <- as.numeric(map_combined$Delta_temp_level)

rm(map_df, map_df_all, map_df_all_crop, map_df_all_maize, map_df_all_rice, map_df_all_wheat, map_df_all_soybean)

#average Nbs for each crop
map_summary <- map_combined %>%
  group_by(x, y, Delta_temp_level, CO2.ppm_level, Adaptation_level, Crop) %>%
  dplyr::summarise(Mean_Effect = mean(Effect), 
                   SE_Effect = sd(Effect)/sqrt(n()),
                   n = n())

# New facet label names for Adaptation variable
adpt.labs <- c("No Adaptation", "Adaptation")
names(adpt.labs) <- c("1", "2")

# New facet label names for CO2 variable
co2.labs <- c("390ppm", "600ppm")
names(co2.labs) <- c("0", "210")

map_summary$Crop <- as.factor(map_summary$Crop)
levels(map_summary$Crop) <- c("Maize", "Rice", "Wheat", "Soybean")

plot_it_noleg <- function(mapNum, gglabel, crop_map) {gg1 + geom_tile(data=mapNum[mapNum$Crop==crop_map,], aes(x=x, y=y, fill=Mean_Effect), alpha=0.8) + scale_fill_gradient2(midpoint=0, low="brown", mid="white", high="darkgreen", limits=c(-100,100), guide = F) + 
    ggtitle(paste0(crop_map,": +", gglabel, "°C")) +
    facet_grid(Adaptation_level ~ CO2.ppm_level, labeller = labeller(Adaptation_level = adpt.labs, CO2.ppm_level = co2.labs)) +
    theme(#text=element_text(size=8), #change font size of all text
      axis.text.x=element_text(size=8, angle = 45, vjust = 0.5), #change font size of axis text
      axis.text.y=element_text(size=8), #change font size of axis titles
      plot.title=element_text(size=8), #change font size of plot title
      legend.text=element_text(size=8), #change font size of legend text
      legend.title=element_text(size=8)) #change font size of legend title
}

plot_it <- function(mapNum, gglabel, crop_map) {gg1 + geom_tile(data=mapNum[mapNum$Crop==crop_map,], aes(x=x, y=y, fill=Mean_Effect), alpha=0.8) + scale_fill_gradient2(midpoint=0, low="brown", mid="white", high="darkgreen", limits=c(-100,100), name="Yield Change (%)") + 
    ggtitle(paste0(crop_map,": +", gglabel, "°C")) +
    facet_grid(Adaptation_level ~ CO2.ppm_level, labeller = labeller(Adaptation_level = adpt.labs, CO2.ppm_level = co2.labs)) +
    theme(#text=element_text(size=8), #change font size of all text
      axis.text.x=element_text(size=8, angle = 45, vjust = 0.5), #change font size of axis text
      axis.text.y=element_text(size=8), #change font size of axis titles
      plot.title=element_text(size=8), #change font size of plot title
      legend.text=element_text(size=8), #change font size of legend text
      legend.title=element_text(size=8)) #change font size of legend title
}

###Plot####
if(rcp45){
    pdf(file=paste0("allcrops_rcp45",rcp45,".pdf"), height=6, width=9)
    print(gg1 + geom_tile(data=map_summary, aes(x=x, y=y, fill=Mean_Effect), alpha=0.8) + scale_fill_gradient2(midpoint=0, low="brown", mid="white", high="darkgreen", limits=c(-100,100), name="Yield Change (%)") + 
      ggtitle(paste0("RCP 4.5")) +
      facet_grid(Adaptation_level ~ Crop, labeller = labeller(Adaptation_level = adpt.labs)) +
      theme(#text=element_text(size=8), #change font size of all text
        axis.text.x=element_text(size=8, angle = 45, vjust = 0.5), #change font size of axis text
        axis.text.y=element_text(size=8), #change font size of axis titles
        plot.title=element_text(size=8), #change font size of plot title
        legend.text=element_text(size=8), #change font size of legend text
        legend.title=element_text(size=8)) #change font size of legend title
    )
    dev.off()
}else{
  map1.5 <- map_summary[map_summary$Delta_temp_level == "1.5",]
  map2 <- map_summary[map_summary$Delta_temp_level == "2",]
  map3 <- map_summary[map_summary$Delta_temp_level == "3",]
  
  for(i in 1:length(crop_species)){
    gg1.5 <- plot_it_noleg(map1.5, "1.5", crop_species[i])
    gg2 <- plot_it_noleg(map2, "2", crop_species[i])
    gg3 <- plot_it_noleg(map3, "3", crop_species[i])
    
    pdf(file=paste0(crop_species[i],"_rcp45",rcp45,".pdf"), height=6, width=8)
    multiplot(gg1.5, gg3, gg2, cols=2)
    dev.off()
  }
  
  pdf(file="legend.pdf", height=11, width=8)
  plot_it(map1.5, "1.5", "Maize")
  dev.off()
}

#2 = Yes, 1 = No (Yes is default??) #If adapt looks same then forgot to run model as Yes/No

######Summary stats#####
summary_stats <- map_summary %>%
  group_by(Delta_temp_level, CO2.ppm_level, Adaptation_level, Crop) %>%
  dplyr::summarise(Mean = mean(Mean_Effect), 
                   SE = sd(Mean_Effect)/sqrt(n()),
                   n = n())
summary_stats

map_summary %>%
  group_by(Adaptation_level, Crop) %>%
  dplyr::summarise(Mean = mean(Mean_Effect), 
                   SE = sd(Mean_Effect)/sqrt(n()),
                   n = n())
#Not RCP
# -19.1--3.81
# -6.74-0.357
# -9.83--3.58
# -13.1--11.5
#RCP
-23.4--8.69
-5.88-2.65
-22.7--20.1
-11.9--6.49
map_summary %>%
  group_by(Crop) %>%
  dplyr::summarise(Mean = mean(Mean_Effect), 
                   SE = sd(Mean_Effect)/sqrt(n()),
                   n = n(), max=max(Mean_Effect), min=min(Mean_Effect))

map_summary %>%
  group_by(Delta_temp_level, Crop) %>%
  dplyr::summarise(Mean = mean(Mean_Effect), 
                   SE = sd(Mean_Effect)/sqrt(n()),
                   n = n())
