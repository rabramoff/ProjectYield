######Yield loop#####
######Load libraries#####
library(openxlsx)
library(raster)
library(rgdal)
library(rgeos)
library(maps)
library(mapdata)
library(dplyr)
library(lme4)
library(lmerTest)
library(spaMM)
library(ranger)
library(ggplot2)
library(pdp)
library(caret)
library(iml)
library(randomForest)
library(gbm)
library(DALEX)
require(MuMIn)

#####Switches#####
same_locations = F
crop_species = c("Maize","Rice","Wheat","Soybean")
adaptation_types = F
do_iml = F
Nb<-10
run_all_mods = F

start <- Sys.time()

#####Analysis Functions#####
do_GBM <- function(mldfTrain){
  fitControl <- caret::trainControl( #10 fold CV
    method = "repeatedcv",
    number = 10,
    repeats = 10 )
  set.seed(1234)
  gbmFit1 <- caret::train( Effect ~ ., data = mldfTrain,
                           method = "gbm",
                           trControl = fitControl,
                           verbose = FALSE )
  return(gbmFit1)
}

pfun <- function(object, newdata) predict(object, data = newdata)$predictions

do_iml_rf <- function(predictor, mod_ml, Training, Testing, PlotTitle){
  
  if(do_iml){
    imp <- FeatureImp$new(predictor, loss = "mae")
    
    #strength of interactions
    interact <- Interaction$new(predictor)
    
    #feature effects
    effs <- FeatureEffects$new(predictor, method="pdp")
    #plot(effs$results$Latitude$.borders, effs$results$Latitude$.value, type="l") #can recreate to rename y ax but not sure how to recreate rug
    
    #shapley
    shapley <- Shapley$new(predictor, x.interest = Training[1,-1])
    
    #plots
    print(plot(imp) + ggtitle(paste0(PlotTitle," ",crop_species[i])))
    print(plot(interact) + ggtitle(paste0(PlotTitle," ",crop_species[i])))
    print(plot(effs) + ggtitle(paste0(PlotTitle," ",crop_species[i])))
    print(shapley$plot() + ggtitle(paste0(PlotTitle," ",crop_species[i])))
  }
  
  Pred_ml<-predict(mod_ml, data=Testing)$predictions
  return(Pred_ml)
}

do_iml_gbm <- function(predictor, mod_ml, Training, Testing, PlotTitle){
  imp <- FeatureImp$new(predictor, loss = "mae")
  
  #strength of interactions
  interact <- Interaction$new(predictor)
  
  #specify two-way interactions
  # interact <- Interaction$new(predictor, feature = "Longitude")
  # plot(interact)
  
  #feature effects
  effs <- FeatureEffects$new(predictor, method="pdp")
  #plot(effs$results$Latitude$.borders, effs$results$Latitude$.value, type="l") #can recreate to rename y ax but not sure how to recreate rug
  
  #shapley
  shapley <- Shapley$new(predictor, x.interest = Training[1,-1])
  
  #plots
  print(plot(imp) + ggtitle(paste0(PlotTitle," ",crop_species)))
  print(plot(interact) + ggtitle(paste0(PlotTitle," ",crop_species)))
  print(plot(effs) + ggtitle(paste0(PlotTitle," ",crop_species)))
  print(shapley$plot() + ggtitle(paste0(PlotTitle," ",crop_species)))
  
  Pred_ml<-predict(mod_ml, newdata=Testing)
  return(Pred_ml)
}

#####Data Import#####
datadir <- "/Users/rzabramoff/ownCloud/Collaborations/Makowski_yield/14691579/"
mdatadir <- "/Users/rzabramoff/ownCloud/Collaborations/Makowski_yield/Data/"
TAB<-read.xlsx(paste0(datadir,"Projected_Impacts_datasheet.xlsx"), sheet=1)
head(TAB)

#Extraction of columns
Species<-as.factor(TAB$Crop)
Ref<-TAB$Ref.No
Delta_temp<-as.numeric(TAB$Global.delta.T.from.2005)
Effect<-as.numeric(TAB$"Climate.impacts.relative.to.2005")
CO2<-TAB$CO2
CO2[CO2=="Yes" | CO2=="yes"]<-"Yes"
CO2[CO2=="No"]<-"No"
CO2<-as.factor(CO2)
CO2.ppm<-TAB$CO2.ppm-390
CO2.ppm[CO2=="No"]<-0
Adaptation<-as.factor(TAB$Adaptation)
RCP<-as.factor(TAB$Climate.scenario)
TempAvg<-as.numeric(TAB$Current.Average.Temperature)
Latitude<-TAB$latitude
Longitude<-TAB$longitude
Baseline<-TAB$"Baseline_Mid-point"
Future<-TAB$"Future_Mid-point"
Future_s<-scale(Future)

if(adaptation_types){
  #Adaptation <- as.factor(TAB$Adaptation.type)
  Fertiliser <- as.factor(TAB$Fertiliser)
  Irrigation <- as.factor(TAB$Irrigation)
  TAB$Cultivar[TAB$Cultivar=="Yes "] <- "Yes"
  Cultivar<-as.factor(TAB$Cultivar)
  Soil_organic_matter_management <- as.factor(TAB$Soil.organic.matter.management)
  Planting_time <- as.factor(TAB$Planting.time)
  Tillage <- as.factor(TAB$Tillage)
  Others <- as.factor(TAB$Others)
}

#####Start loop#####
RMSEP_rf_xy_vec<-matrix(nrow=Nb,ncol=4)
RMSEP_rf_0_vec<-matrix(nrow=Nb,ncol=4)
RMSEP_rf_1_vec<-matrix(nrow=Nb,ncol=4)
RMSEP_gbm_xy_vec<-matrix(nrow=Nb,ncol=4)
RMSEP_gbm_0_vec<-matrix(nrow=Nb,ncol=4)
RMSEP_gbm_1_vec<-matrix(nrow=Nb,ncol=4)
RMSEP_lm_vec<-matrix(nrow=Nb,ncol=4)
RMSEP_lm_ints_vec<-matrix(nrow=Nb,ncol=4)
RMSEP_spa_0_vec<-matrix(nrow=Nb,ncol=4)
RMSEP_spa_lm_vec<-matrix(nrow=Nb,ncol=4)
RMSEP_spa_rf_vec<-matrix(nrow=Nb,ncol=4)

R2_rf_xy_vec<-matrix(nrow=Nb,ncol=4)
R2_rf_0_vec<-matrix(nrow=Nb,ncol=4)
R2_rf_1_vec<-matrix(nrow=Nb,ncol=4)
R2_gbm_xy_vec<-matrix(nrow=Nb,ncol=4)
R2_gbm_0_vec<-matrix(nrow=Nb,ncol=4)
R2_gbm_1_vec<-matrix(nrow=Nb,ncol=4)
R2_lm_vec<-matrix(nrow=Nb,ncol=4)
R2_lm_ints_vec<-matrix(nrow=Nb,ncol=4)
R2_spa_0_vec<-matrix(nrow=Nb,ncol=4)
R2_spa_lm_vec<-matrix(nrow=Nb,ncol=4)
R2_spa_rf_vec<-matrix(nrow=Nb,ncol=4)

ObsPred <- NULL
PDPeffs <- NULL
save.mod.rf <- NULL
save.Training.rf <- NULL

for (j in 1:Nb) {
  print(j)
  
  set.seed(j)
  
#####Prep and Split Data#####
if(adaptation_types){
  if(same_locations){
    Loca<-paste(Longitude,Latitude, sep="_") 
    DATA_lm<-data.frame(Ref,Effect,Species,Delta_temp,CO2.ppm,TempAvg, y=Latitude, x=Longitude, Fertiliser,Irrigation,Cultivar,Soil_organic_matter_management,Planting_time,Tillage,Others,Loca)
  } else {
    DATA_lm<-data.frame(Ref,Effect,Species,Delta_temp,CO2.ppm,TempAvg, y=Latitude, x=Longitude, Fertiliser,Irrigation,Cultivar,Soil_organic_matter_management,Planting_time,Tillage,Others)
  }
  DATA<-data.frame(Effect,Species,Delta_temp,CO2.ppm,TempAvg, Latitude, Longitude,Fertiliser,Irrigation,Cultivar,Soil_organic_matter_management,Planting_time,Tillage,Others)
}else{
  if(same_locations){
    Loca<-paste(Longitude,Latitude, sep="_") 
    DATA_lm<-data.frame(Ref,Effect,Species,Delta_temp,CO2.ppm,Adaptation,TempAvg, y=Latitude, x=Longitude, Loca)
  } else {
    DATA_lm<-data.frame(Ref,Effect,Species,Delta_temp,CO2.ppm,Adaptation,TempAvg, y=Latitude, x=Longitude)
  }
  DATA<-data.frame(Effect,Species,Delta_temp,CO2.ppm,Adaptation,TempAvg, Latitude, Longitude)
}

df <- NULL
df_lm <- NULL
df_xy <- NULL
Training_xy <- NULL
Testing_xy <- NULL
Training_1 <- NULL
Testing_1 <- NULL
Training_2 <- NULL
Testing_2 <- NULL
Locs <- NULL
bestPred <- NULL
bestEffs <- NULL

for (i in 1:length(crop_species)){
  df[[i]]<-DATA[DATA$Species==crop_species[i],]
  df[[i]]<-na.omit(df[[i]])
  df_lm[[i]]<-DATA_lm[DATA_lm$Species==crop_species[i],]
  df_lm[[i]]<-na.omit(df_lm[[i]])
  #Remove Species
  df[[i]]<-df[[i]][ , -which(names(df[[i]]) %in% c("Species"))]
  df_lm[[i]]<-df_lm[[i]][ , -which(names(df_lm[[i]]) %in% c("Species"))]
  
  #Remove where N<24 (instance of N=23 removed)
  if(adaptation_types){
    df[[i]]<-df[[i]][ , -which(names(df[[i]]) %in% c("Tillage","Soil_organic_matter_management"))]
    df_lm[[i]]<-df_lm[[i]][ , -which(names(df_lm[[i]]) %in% c("Tillage","Soil_organic_matter_management"))]
    
    if(i==1|i==4){
      #Remove SOM management and Tillage and Others
      df[[i]]<-df[[i]][ , -which(names(df[[i]]) %in% c("Others"))]
      df_lm[[i]]<-df_lm[[i]][ , -which(names(df_lm[[i]]) %in% c("Others"))]
    }
    
    if(i==2|i==4){
      df[[i]]<-df[[i]][ , -which(names(df[[i]]) %in% c("Irrigation"))]
      df_lm[[i]]<-df_lm[[i]][ , -which(names(df_lm[[i]]) %in% c("Irrigation"))]
    }
  }
  
  #Only x and y
  df_xy[[i]]<-df[[i]][ , which(names(df[[i]]) %in% c("Effect","Latitude","Longitude"))]
  
  #set.seed(123)
  Loc<-paste(df[[i]]$Longitude,df[[i]]$Latitude, sep="_")
  Locs[[i]] <- Loc
  
  #Split at the same locations
  if(same_locations){
    ID<-1:length(Loc)
    DATA_sample<-cbind(ID,Loc,df[[i]])
    #Remove locations including one data only
    Loc_1<-names(table(DATA_sample$Loc))[as.numeric(table(DATA_sample$Loc))<2]
    DATA_sample2<-DATA_sample
    if (length(Loc_1)>0) {
      ID_1<-ID[DATA_sample$Loc%in%Loc_1==TRUE]
      DATA_sample2<-DATA_sample[-ID_1,]
    }
    #Sample one data per location
    new_data <- DATA_sample2 %>% group_by(Loc) %>% sample_n(1)
    ID<-new_data$ID
  } else {
    #Split using different locations
    UnLoc<-unique(Loc)
    ID_loc<-sample(x=UnLoc, size=round(0.25*length(UnLoc)))
    ID<-(1:nrow(df[[i]]))[Loc%in%ID_loc==TRUE]
  }
  
  Training_xy[[i]]<-df_xy[[i]][-ID,]
  Testing_xy[[i]]<-df[[i]][ID,]
  Training_1[[i]]<-df[[i]][-ID,]
  Testing_1[[i]]<-df[[i]][ID,]
  Training_2[[i]]<-df_lm[[i]][-ID,]
  Testing_2[[i]]<-df_lm[[i]][ID,]
  
}

#####Training and Testing#####

if(run_all_mods){
#####Spatial RF#####
mod.rf.xy <- NULL
RMSEP_rf_xy <- NULL
R2_rf_xy_int <- NULL
R2_rf_xy <- NULL
Pred_rf_xy <- NULL
for(i in 1:length(crop_species)){
  set.seed(1234)
  mod.rf.xy[[i]] <- ranger(Effect~., data=Training_xy[[i]], num.trees=1000)
  #print(crop_species[[i]])
  #print(mod.rf.xy[[i]])
  
  predictor.rf.xy <- Predictor$new(model = mod.rf.xy[[i]], data = Training_xy[[i]][-1], y = Training_xy[[i]]$Effect, predict.fun = pfun)
  
  Pred_rf_xy[[i]] <- do_iml_rf(predictor.rf.xy, mod.rf.xy[[i]], Training_xy[[i]], Testing_xy[[i]], PlotTitle="Spatial RF")
  RMSEP_rf_xy[[i]]=sqrt(mean((Testing_xy[[i]]$Effect-Pred_rf_xy[[i]])^2))
  #print(plot(Testing_xy[[i]]$Effect,Pred_rf_xy[[i]]))
  R2_rf_xy_int[[i]] <- mod.rf.xy[[i]]$r.squared
  R2_rf_xy[[i]] <- 1 - sum( (Testing_xy[[i]]$Effect-Pred_rf_xy[[i]])^2 ) / sum( (Testing_xy[[i]]$Effect - mean(Testing_xy[[i]]$Effect))^2 )
}

#####Spatial GBM#####
mod.gbm.xy <- NULL
RMSEP_gbm_xy <- NULL
Pred_gbm_xy <- NULL
R2_gbm_xy_int <- NULL
R2_gbm_xy <- NULL
for(i in 1:length(crop_species)){
  mod.gbm.xy[[i]] <- do_GBM(Training_xy[[i]])
  #print(crop_species[[i]])
  #print(mod.gbm.xy[[i]])
  
  predictor.gbm.xy <- Predictor$new(model = mod.gbm.xy[[i]], data = Training_xy[[i]][-1], y = Training_xy[[i]]$Effect)
  
  Pred_gbm_xy[[i]] <- do_iml_gbm(predictor.gbm.xy, mod.gbm.xy[[i]], Training_xy[[i]], Testing_xy[[i]], PlotTitle="Spatial GBM")
  RMSEP_gbm_xy[[i]]=sqrt(mean((Testing_xy[[i]]$Effect-Pred_gbm_xy[[i]])^2))
  #print(plot(Testing_xy[[i]]$Effect,Pred_gbm_xy[[i]]))
  R2_gbm_xy_int[[i]] <- mod.gbm.xy[[i]][4]$results$Rsquared[which.min(mod.gbm.xy[[i]][4]$results$RMSE)]
  R2_gbm_xy[[i]] <- 1 - sum( (Testing_xy[[i]]$Effect-Pred_gbm_xy[[i]])^2 ) / sum( (Testing_xy[[i]]$Effect - mean(Testing_xy[[i]]$Effect))^2 )
  
}
}

#####Climate RF#####
mod.rf.0 <- NULL
Pred_rf_0 <- NULL
R2_rf_0_int <- NULL
RMSEP_rf_0 <- NULL
R2_rf_0 <- NULL
Training_0 <- NULL
Training_2_bis <- NULL
Pred_rf_0 <- NULL
for(i in 1:length(crop_species)){
  Training_0[[i]] <- Training_1[[i]][,-which(names(Training_1[[i]]) %in% c("Latitude","Longitude"))]
  mod.rf.0[[i]] <- ranger(Effect~., data=Training_0[[i]], num.trees=1000)
  predictor.rf.0 <- Predictor$new(mod.rf.0[[i]], data = Training_0[[i]][-1], y = Training_0[[i]]$Effect, predict.fun = pfun)
  
  Pred_rf_0[[i]] <- do_iml_rf(predictor.rf.0, mod.rf.0[[i]], Training_0[[i]], Testing_1[[i]], PlotTitle="Climate RF")
  RMSEP_rf_0[[i]]=sqrt(mean((Testing_1[[i]]$Effect-Pred_rf_0[[i]])^2))
  R2_rf_0_int[[i]] <- mod.rf.0[[i]]$r.squared
  R2_rf_0[[i]] <- 1 - sum( (Testing_1[[i]]$Effect-Pred_rf_0[[i]])^2 ) / sum( (Testing_1[[i]]$Effect - mean(Testing_1[[i]]$Effect))^2 )
  
  X0<- predict(mod.rf.0[[i]], data=Training_1[[i]])$predictions
  Training_2_bis[[i]]<-cbind(Training_2[[i]],X0)
  
  if(do_iml){
    imp <- FeatureImp$new(predictor.rf.0, loss = "mae")
    
    #strength of interactions
    interact <- Interaction$new(predictor.rf.0)

    #feature effects
    effs <- FeatureEffects$new(predictor.rf.0, method="pdp")
    #ales <- FeatureEffects$new(predictor.rf.0, method="ale")
    # make_effect_plots(effs,"PDPs")
    # make_effect_plots(ales,"ALEs")
  
  if(i==1|i==4){
    bestPred[[i]] <- as.data.frame(cbind(Pred_rf_0[[i]], Testing_1[[i]]$Effect, rep(crop_species[i], length(Pred_rf_0[[i]])), row.names(Testing_1[[i]])))
    bestPred[[i]][,1] <- as.numeric(bestPred[[i]][,1])
    bestPred[[i]][,2] <- as.numeric(bestPred[[i]][,2])
    names(bestPred[[i]]) <- c("Predicted", "Observed", "Crop", "Site")
    bestEffs[[i]] <- effs
  }
  }
  
}

if(run_all_mods){
####Climate GBM#####
mod.gbm.0 <- NULL
Pred_gbm_0 <- NULL
RMSEP_gbm_0 <- NULL
R2_gbm_0_int <- NULL
R2_gbm_0 <- NULL
for(i in 1:length(crop_species)){
  mod.gbm.0[[i]] <- do_GBM(Training_0[[i]])
  
  predictor.gbm.0 <- Predictor$new(mod.gbm.0[[i]], data = Training_0[[i]][-1], y = Training_0[[i]]$Effect)
  
  Pred_gbm_0[[i]] <- do_iml_gbm(predictor.gbm.0, mod.gbm.0[[i]], Training_0[[i]], Testing_1[[i]], PlotTitle="Climate GBM")
  RMSEP_gbm_0[[i]]=sqrt(mean((Testing_1[[i]]$Effect-Pred_gbm_0[[i]])^2))
  R2_gbm_0_int[[i]] <- mod.gbm.0[[i]][4]$results$Rsquared[which.min(mod.gbm.0[[i]][4]$results$RMSE)]
  R2_gbm_0[[i]] <- 1 - sum( (Testing_1[[i]]$Effect-Pred_gbm_0[[i]])^2 ) / sum( (Testing_1[[i]]$Effect - mean(Testing_1[[i]]$Effect))^2 )
}
}

####Climate+longlat RF#####
mod.rf.1 <- NULL
RMSEP_rf_1 <- NULL
Pred_rf_1 <- NULL
R2_rf_1_int <- NULL
R2_rf_1 <- NULL
for(i in 1:length(crop_species)){
  set.seed(1234)
  if(same_locations){
    mod.rf.1[[i]] <- ranger(Effect~., data=Training_1[[i]], num.trees=1000, importance="permutation")
  } else {
    mod.rf.1[[i]] <- ranger(Effect~., data=Training_1[[i]], num.trees=1000)
  }
  
  predictor.rf.1 <- Predictor$new(mod.rf.1[[i]], data = Training_1[[i]][-1], y = Training_1[[i]]$Effect, predict.fun = pfun)
  
  Pred_rf_1[[i]] <- do_iml_rf(predictor.rf.1, mod.rf.1[[i]], Training_1[[i]], Testing_1[[i]], PlotTitle="Climate+LongLat RF")
  RMSEP_rf_1[[i]]=sqrt(mean((Testing_1[[i]]$Effect-Pred_rf_1[[i]])^2))
  R2_rf_1_int[[i]] <- mod.rf.1[[i]]$r.squared
  R2_rf_1[[i]] <- 1 - sum( (Testing_1[[i]]$Effect-Pred_rf_1[[i]])^2 ) / sum( (Testing_1[[i]]$Effect - mean(Testing_1[[i]]$Effect))^2 )
  
  if(do_iml){
    imp <- FeatureImp$new(predictor.rf.1, loss = "mae")
    
    #strength of interactions
    interact <- Interaction$new(predictor.rf.1)
    
    #feature effects
    effs <- FeatureEffects$new(predictor.rf.1, method="pdp")
    #ales <- FeatureEffects$new(predictor.rf.0, method="ale")
  
  if(i==2|i==3){
    bestPred[[i]] <- as.data.frame(cbind(Pred_rf_1[[i]], Testing_1[[i]]$Effect, rep(crop_species[i], length(Pred_rf_1[[i]])), row.names(Testing_1[[i]])))
    bestPred[[i]][,1] <- as.numeric(bestPred[[i]][,1])
    bestPred[[i]][,2] <- as.numeric(bestPred[[i]][,2])
    names(bestPred[[i]]) <- c("Predicted", "Observed", "Crop", "Site")
    bestEffs[[i]] <- effs
  }
  }
  
}

if(run_all_mods){
#####Climate+longlat GBM#####
mod.gbm.1 <- NULL
RMSEP_gbm_1 <- NULL
Pred_gbm_1 <- NULL
R2_gbm_1_int <- NULL
R2_gbm_1 <- NULL
for(i in 1:length(crop_species)){
  mod.gbm.1[[i]] <- do_GBM(Training_1[[i]])

  predictor.gbm.1 <- Predictor$new(mod.gbm.1[[i]], data = Training_1[[i]][-1], y = Training_1[[i]]$Effect)
  
  Pred_gbm_1[[i]] <- do_iml_gbm(predictor.gbm.1, mod.gbm.1[[i]], Training_1[[i]], Testing_1[[i]], PlotTitle="Climate+LongLat GBM")
  RMSEP_gbm_1[[i]]=sqrt(mean((Testing_1[[i]]$Effect-Pred_gbm_1[[i]])^2))
  R2_gbm_1_int[[i]] <- mod.gbm.1[[i]][4]$results$Rsquared[which.min(mod.gbm.1[[i]][4]$results$RMSE)]
  R2_gbm_1[[i]] <- 1 - sum( (Testing_1[[i]]$Effect-Pred_gbm_1[[i]])^2 ) / sum( (Testing_1[[i]]$Effect - mean(Testing_1[[i]]$Effect))^2 ) 
}

#####Mixed models#####
####Climate#####
mod_lm <- NULL
RMSEP_lm <- NULL
R2_lm <- NULL
Pred_lm <- NULL
for(i in 1:length(crop_species)){
  #Linear model climate
  if(adaptation_types){
    if(same_locations){
      if(i==1){
        mod_lm[[i]]<-lmer(Effect~Delta_temp+TempAvg+CO2.ppm+Irrigation+Fertiliser+Cultivar+Planting_time+(1|Loca), data=Training_2[[i]])
      }
      if(i==2){
        mod_lm[[i]]<-lmer(Effect~Delta_temp+TempAvg+CO2.ppm+Fertiliser+Cultivar+Planting_time+Others+(1|Loca), data=Training_2[[i]])
      }
      if(i==3){
        mod_lm[[i]]<-lmer(Effect~Delta_temp+TempAvg+CO2.ppm+Irrigation+Fertiliser+Cultivar+Planting_time+Others+(1|Loca), data=Training_2[[i]])
      }
      if(i==4){
        mod_lm[[i]]<-lmer(Effect~Delta_temp+TempAvg+CO2.ppm+Fertiliser+Cultivar+Planting_time+(1|Loca), data=Training_2[[i]])
      }
    } else {
      if(i==1){
        mod_lm[[i]]<-lmer(Effect~Delta_temp+TempAvg+CO2.ppm+Irrigation+Fertiliser+Cultivar+Planting_time+(1|Ref), data=Training_2[[i]])
      }
      if(i==2){
        mod_lm[[i]]<-lmer(Effect~Delta_temp+TempAvg+CO2.ppm+Fertiliser+Cultivar+Planting_time+Others+(1|Ref), data=Training_2[[i]])
      }
      if(i==3){
        mod_lm[[i]]<-lmer(Effect~Delta_temp+TempAvg+CO2.ppm+Irrigation+Fertiliser+Cultivar+Planting_time+Others+(1|Ref), data=Training_2[[i]])
      }
      if(i==4){
        mod_lm[[i]]<-lmer(Effect~Delta_temp+TempAvg+CO2.ppm+Fertiliser+Cultivar+Planting_time+(1|Ref), data=Training_2[[i]])
      }
    }
  }else{
    if(same_locations){
      mod_lm[[i]]<-lmer(Effect~Delta_temp+TempAvg+Adaptation+CO2.ppm+(1|Loca), data=Training_2[[i]])
    } else {
      mod_lm[[i]]<-lmer(Effect~Delta_temp+TempAvg+Adaptation+CO2.ppm+(1|Ref), data=Training_2[[i]])
    }
  }
  
  #print(crop_species[[i]])
  #print(summary(mod_lm[[i]]))
  
  if(same_locations){
    Pred_lm[[i]]<-predict(mod_lm[[i]], newdata=Testing_2[[i]]) #removed random effect on Jul 5, 2021
  } else {
    Pred_lm[[i]]<-predict(mod_lm[[i]], newdata=Testing_2[[i]], re.form=NA) #NA means include no random effects
  }
  
  RMSEP_lm[[i]]=sqrt(mean((Testing_2[[i]]$Effect-Pred_lm[[i]])^2))
  R2_lm[[i]] <- 1 - sum( (Testing_2[[i]]$Effect-Pred_lm[[i]])^2 ) / sum( (Testing_2[[i]]$Effect - mean(Testing_2[[i]]$Effect))^2 )
}

#####Climate+Interactions#####
mod_lm_ints <- NULL
RMSEP_lm_ints <- NULL
R2_lm_ints <- NULL
Pred_lm_ints <- NULL
for(i in 1:length(crop_species)){
  #Linear model climate
  if(adaptation_types){
    if(same_locations){
      if(i==1){
        mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*TempAvg*CO2.ppm*(Irrigation+Fertiliser+Cultivar+Planting_time)+(1|Loca), data=Training_2[[i]])
      }
      if(i==2){
        mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*TempAvg*CO2.ppm*(Fertiliser+Cultivar+Planting_time+Others)+(1|Loca), data=Training_2[[i]])
      }
      if(i==3){
        mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*TempAvg*CO2.ppm*(Irrigation+Fertiliser+Cultivar+Planting_time+Others)+(1|Loca), data=Training_2[[i]])
      }
      if(i==4){
        mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*TempAvg*CO2.ppm*(Fertiliser+Cultivar+Planting_time)+(1|Loca), data=Training_2[[i]])
      }
    } else {
      if(i==1){
        mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*TempAvg*CO2.ppm*(Irrigation+Fertiliser+Cultivar+Planting_time)+(1|Ref), data=Training_2[[i]])
      }
      if(i==2){
        mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*TempAvg*CO2.ppm*(Fertiliser+Cultivar+Planting_time+Others)+(1|Ref), data=Training_2[[i]])
      }
      if(i==3){
        mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*TempAvg*CO2.ppm*(Irrigation+Fertiliser+Cultivar+Planting_time+Others)+(1|Ref), data=Training_2[[i]])
      }
      if(i==4){
        mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*TempAvg*CO2.ppm*(Fertiliser+Cultivar+Planting_time)+(1|Ref), data=Training_2[[i]])
      }
    }
  }else{  
    
    if(same_locations){
      mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*CO2.ppm*Adaptation*TempAvg+(1|Loca), data=Training_2[[i]])
    } else {
      #mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*CO2.ppm + Delta_temp*Adaptation + Delta_temp*TempAvg+Adaptation+CO2.ppm+(1|Ref), data=Training_2[[i]])
      mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*CO2.ppm*Adaptation*TempAvg+(1|Ref), data=Training_2[[i]])
    }
  }
  
  if(same_locations){
    Pred_lm_ints[[i]]<-predict(mod_lm_ints[[i]], newdata=Testing_2[[i]], re.form=NA) #removed random effect on Jul 5, 2021
  } else {
    Pred_lm_ints[[i]]<-predict(mod_lm_ints[[i]], newdata=Testing_2[[i]], re.form=NA)
  }
  
  RMSEP_lm_ints[[i]]=sqrt(mean((Testing_2[[i]]$Effect-Pred_lm_ints[[i]])^2))
  R2_lm_ints[[i]] <- 1 - sum( (Testing_2[[i]]$Effect-Pred_lm_ints[[i]])^2 ) / sum( (Testing_2[[i]]$Effect - mean(Testing_2[[i]]$Effect))^2 )
}

#####Spatial#####
mod_spa_0 <- NULL
RMSEP_spa_0 <- NULL
R2_spa_0 <- NULL
Pred_spa_0 <- NULL
for(i in 1:length(crop_species)){
  #Spatial linear model
  mod_spa_0[[i]]<-fitme(Effect~1+Matern(1|x+y), data=Training_2[[i]])
  
  Pred_spa_0[[i]]<-predict(mod_spa_0[[i]], newdata=Testing_2[[i]])
  RMSEP_spa_0[[i]]=sqrt(mean((Testing_2[[i]]$Effect-Pred_spa_0[[i]])^2))
  R2_spa_0[[i]] <- 1 - sum( (Testing_2[[i]]$Effect-Pred_spa_0[[i]])^2 ) / sum( (Testing_2[[i]]$Effect - mean(Testing_2[[i]]$Effect))^2 )
}

#####Spatial climate#####
mod_spa_lm <- NULL
RMSEP_spa_lm <- NULL
R2_spa_lm <- NULL
Pred_spa_lm <- NULL
for(i in 1:length(crop_species)){
  #Spatial linear model + climate + CO2
  
  if(adaptation_types){
    if(i==1){
      mod_spa_lm[[i]]<-fitme(Effect~Delta_temp+TempAvg+CO2.ppm+Irrigation+Fertiliser+Cultivar+Planting_time+Matern(1|x+y), data=Training_2[[i]])
    }
    if(i==2){
      mod_spa_lm[[i]]<-fitme(Effect~Delta_temp+TempAvg+CO2.ppm+Fertiliser+Cultivar+Planting_time+Others+Matern(1|x+y), data=Training_2[[i]])
    }
    if(i==3){
      mod_spa_lm[[i]]<-fitme(Effect~Delta_temp+TempAvg+CO2.ppm+Irrigation+Fertiliser+Cultivar+Planting_time+Others+Matern(1|x+y), data=Training_2[[i]])
    }
    if(i==4){
      mod_spa_lm[[i]]<-fitme(Effect~Delta_temp+TempAvg+CO2.ppm+Fertiliser+Cultivar+Matern(1|x+y), data=Training_2[[i]]) #For some reason planting time doesn't work here so removed
    }
  }else{
    mod_spa_lm[[i]]<-fitme(Effect~Delta_temp+TempAvg+Adaptation+CO2.ppm+Matern(1|x+y), data=Training_2[[i]])
  }
  
  Pred_spa_lm[[i]]<-predict(mod_spa_lm[[i]], newdata=Testing_2[[i]])
  RMSEP_spa_lm[[i]]=sqrt(mean((Testing_2[[i]]$Effect-Pred_spa_lm[[i]])^2))
  R2_spa_lm[[i]] <- 1 - sum( (Testing_2[[i]]$Effect-Pred_spa_lm[[i]])^2 ) / sum( (Testing_2[[i]]$Effect - mean(Testing_2[[i]]$Effect))^2 )
}

#####Spatial+RF outputs#####
mod_spa_rf <- NULL
RMSEP_spa_rf <- NULL
Testing_2_bis <- NULL
R2_spa_rf <- NULL
Pred_spa_rf <- NULL
for(i in 1:length(crop_species)){
  #Sptial linear model + RF outputs
  mod_spa_rf[[i]]<-fitme(Effect~X0+Matern(1|x+y), data=Training_2_bis[[i]])
  
  Testing_2_bis[[i]]<-cbind(Testing_2[[i]], X0=Pred_rf_0[[i]])
  Pred_spa_rf[[i]]<-predict(mod_spa_rf[[i]], newdata=Testing_2_bis[[i]])
  RMSEP_spa_rf[[i]]=sqrt(mean((Testing_2_bis[[i]]$Effect-Pred_spa_rf[[i]])^2))
  R2_spa_rf[[i]] <- 1 - sum( (Testing_2[[i]]$Effect-Pred_spa_rf[[i]])^2 ) / sum( (Testing_2[[i]]$Effect - mean(Testing_2[[i]]$Effect))^2 )
}

}
#####End Training and Testing#####
  ##WORKING - issue with putting them all together for adaptation types T, but not with running it
for(i in 1:length(crop_species)){  

  if(run_all_mods){
  RMSEP_rf_xy_vec[j,i]<-RMSEP_rf_xy[[i]]
  }
  RMSEP_rf_0_vec[j,i]<-RMSEP_rf_0[[i]]
  RMSEP_rf_1_vec[j,i]<-RMSEP_rf_1[[i]]
  if(run_all_mods){
  RMSEP_gbm_xy_vec[j,i]<-RMSEP_gbm_xy[[i]]
  RMSEP_gbm_0_vec[j,i]<-RMSEP_gbm_0[[i]]
  RMSEP_gbm_1_vec[j,i]<-RMSEP_gbm_1[[i]]
  RMSEP_lm_vec[j,i]<-RMSEP_lm[[i]]
  RMSEP_lm_ints_vec[j,i]<-RMSEP_lm_ints[[i]]
  RMSEP_spa_0_vec[j,i]<-RMSEP_spa_0[[i]]
  RMSEP_spa_lm_vec[j,i]<-RMSEP_spa_lm[[i]]
  RMSEP_spa_rf_vec[j,i]<-RMSEP_spa_rf[[i]]
  }

  if(run_all_mods){
  R2_rf_xy_vec[j,i]<-R2_rf_xy[[i]]
  }
  R2_rf_0_vec[j,i]<-R2_rf_0[[i]]
  R2_rf_1_vec[j,i]<-R2_rf_1[[i]]
  if(run_all_mods){
  R2_gbm_xy_vec[j,i]<-R2_gbm_xy[[i]]
  R2_gbm_0_vec[j,i]<-R2_gbm_0[[i]]
  R2_gbm_1_vec[j,i]<-R2_gbm_1[[i]]
  R2_lm_vec[j,i]<-R2_lm[[i]]
  R2_lm_ints_vec[j,i]<-R2_lm_ints[[i]]
  R2_spa_0_vec[j,i]<-R2_spa_0[[i]]
  R2_spa_lm_vec[j,i]<-R2_spa_lm[[i]]
  R2_spa_rf_vec[j,i]<-R2_spa_rf[[i]]
  }
}

ObsPred[[j]] <- rbind(bestPred[[1]], bestPred[[2]], bestPred[[3]], bestPred[[4]])
if(do_iml){
PDPeffs[[j]] <- list(bestEffs[[1]], bestEffs[[2]], bestEffs[[3]], bestEffs[[4]])
}
save.mod.rf[[j]] <- list(mod.rf.0[[1]], mod.rf.1[[2]], mod.rf.1[[3]], mod.rf.0[[4]]) 
save.Training.rf[[j]] <- list(Training_0[[1]], Training_1[[2]], Training_1[[3]], Training_0[[4]])
}

RESULT <- NULL
if(run_all_mods){
NAME<-c("RF long lat","GBM long lat",
        "RF climate","GBM climate",
        "RF climate + long lat","GBM climate + long lat",
        "Linear model climate",
        "Linear model climate + interactions",
        "Spatial linear model","Spatial linear model + climate","Spatial linear model + RF outputs")
}else{
  NAME<-c("RF climate",
          "RF climate + long lat") 
}
for(i in 1:length(crop_species)){

  if(run_all_mods){
  RMSEP_MEAN <- c(
    mean(RMSEP_rf_xy_vec[,i]),
    mean(RMSEP_gbm_xy_vec[,i]),
    mean(RMSEP_rf_0_vec[,i]),
    mean(RMSEP_gbm_0_vec[,i]),
    mean(RMSEP_rf_1_vec[,i]),
    mean(RMSEP_gbm_1_vec[,i]),
    mean(RMSEP_lm_vec[,i]),
    mean(RMSEP_lm_ints_vec[,i]),
    mean(RMSEP_spa_0_vec[,i]),
    mean(RMSEP_spa_lm_vec[,i]),
    mean(RMSEP_spa_rf_vec[,i]))
  
  RMSEP_SE<-c(
    sd(RMSEP_rf_xy_vec[,i])/sqrt(Nb),
    sd(RMSEP_gbm_xy_vec[,i])/sqrt(Nb),
    sd(RMSEP_rf_0_vec[,i])/sqrt(Nb),
    sd(RMSEP_gbm_0_vec[,i])/sqrt(Nb),
    sd(RMSEP_rf_1_vec[,i])/sqrt(Nb),
    sd(RMSEP_gbm_1_vec[,i])/sqrt(Nb),
    sd(RMSEP_lm_vec[,i])/sqrt(Nb),
    sd(RMSEP_lm_ints_vec[,i])/sqrt(Nb),
    sd(RMSEP_spa_0_vec[,i])/sqrt(Nb),
    sd(RMSEP_spa_lm_vec[,i])/sqrt(Nb),
    sd(RMSEP_spa_rf_vec[,i])/sqrt(Nb))
  
  R2_MEAN <- c(
    mean(R2_rf_xy_vec[,i]),
    mean(R2_gbm_xy_vec[,i]),
    mean(R2_rf_0_vec[,i]),
    mean(R2_gbm_0_vec[,i]),
    mean(R2_rf_1_vec[,i]),
    mean(R2_gbm_1_vec[,i]),
    mean(R2_lm_vec[,i]),
    mean(R2_lm_ints_vec[,i]),
    mean(R2_spa_0_vec[,i]),
    mean(R2_spa_lm_vec[,i]),
    mean(R2_spa_rf_vec[,i]))
  
  R2_SE<-c(
    sd(R2_rf_xy_vec[,i])/sqrt(Nb),
    sd(R2_gbm_xy_vec[,i])/sqrt(Nb),
    sd(R2_rf_0_vec[,i])/sqrt(Nb),
    sd(R2_gbm_0_vec[,i])/sqrt(Nb),
    sd(R2_rf_1_vec[,i])/sqrt(Nb),
    sd(R2_gbm_1_vec[,i])/sqrt(Nb),
    sd(R2_lm_vec[,i])/sqrt(Nb),
    sd(R2_lm_ints_vec[,i])/sqrt(Nb),
    sd(R2_spa_0_vec[,i])/sqrt(Nb),
    sd(R2_spa_lm_vec[,i])/sqrt(Nb),
    sd(R2_spa_rf_vec[,i])/sqrt(Nb))
  }else{
    RMSEP_MEAN <- c(
      mean(RMSEP_rf_0_vec[,i]),
      mean(RMSEP_rf_1_vec[,i]))
    
    RMSEP_SE<-c(
      sd(RMSEP_rf_0_vec[,i])/sqrt(Nb),
      sd(RMSEP_rf_1_vec[,i])/sqrt(Nb))
    
    R2_MEAN <- c(
      mean(R2_rf_0_vec[,i]),
      mean(R2_rf_1_vec[,i]))
    
    R2_SE<-c(
      sd(R2_rf_0_vec[,i])/sqrt(Nb),
      sd(R2_rf_1_vec[,i])/sqrt(Nb))
  }
  
  crop_species_col <- rep(crop_species[[i]], length(R2_MEAN))
  
  RESULT[[i]]<-data.frame(NAME,RMSEP_MEAN, RMSEP_SE, R2_MEAN, R2_SE, crop_species_col)
  print(crop_species[[i]])
  print(RESULT[[i]])
}

RESULT_ALL <- rbind(RESULT[[1]], RESULT[[2]],RESULT[[3]], RESULT[[4]])
write.csv(file=paste0("samelocs",same_locations,"_adaptationtypes",adaptation_types,"_stats_loop.csv"),RESULT_ALL)
save.image(file=paste0("saved_MLs_samelocs",same_locations,"_adaptationtypes",adaptation_types,"_loop.RData"))
save(ObsPred, file=paste0("saved_MLs_samelocs",same_locations,"_adaptationtypes",adaptation_types,"_store_predictions.RData"))
if(do_iml){
save(PDPeffs, file=paste0("saved_MLs_samelocs",same_locations,"_adaptationtypes",adaptation_types,"_store_PDPs.RData"))
#PDPeffs[[Nb 1-10]][[Crop 1-4]]
}
save(save.mod.rf, file=paste0("saved_MLs_samelocs",same_locations,"_adaptationtypes",adaptation_types,"_modrf.RData"))
save(save.Training.rf,file=paste0("saved_MLs_samelocs",same_locations,"_adaptationtypes",adaptation_types,"_trainingrf.RData"))

end <- Sys.time()
end - start

