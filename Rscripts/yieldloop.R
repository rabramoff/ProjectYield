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

loop_func <- function(same_locations,
                      adaptation_types,
                      do_iml,
                      Nb,
                      run_all_mods,
                      train_on_all){
  
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

AIC_rf_xy_vec<-matrix(nrow=Nb,ncol=4)
AIC_rf_0_vec<-matrix(nrow=Nb,ncol=4)
AIC_rf_1_vec<-matrix(nrow=Nb,ncol=4)
AIC_gbm_xy_vec<-matrix(nrow=Nb,ncol=4)
AIC_gbm_0_vec<-matrix(nrow=Nb,ncol=4)
AIC_gbm_1_vec<-matrix(nrow=Nb,ncol=4)
AIC_lm_vec<-matrix(nrow=Nb,ncol=4)
AIC_lm_ints_vec<-matrix(nrow=Nb,ncol=4)
AIC_spa_0_vec<-matrix(nrow=Nb,ncol=4)
AIC_spa_lm_vec<-matrix(nrow=Nb,ncol=4)
AIC_spa_rf_vec<-matrix(nrow=Nb,ncol=4)

ObsPred <- NULL
PDPeffs <- NULL
PDP2way <- NULL
save.mod.rf <- NULL
save.Training.rf <- NULL
save.mod.out.lm <- NULL
save.mod.out.lm.ints <- NULL
bestPred <- NULL
bestEffs <- NULL
best2way <- NULL

for (j in 1:Nb) {
  print(j)
  
  set.seed(j)
  
  #####Prep and Split Data#####
  if(adaptation_types){
    if(same_locations){
      DATA_lm<- FTAB %>%
        dplyr::select(Effect,Crop,Delta_temp,CO2.ppm,TempAvg,Precip,Delta_precip,Latitude,Longitude,Fertiliser,Irrigation,Cultivar,Soil.organic.matter.management,Planting.time,Tillage,Others,Loca) %>%
        dplyr::rename(y=Latitude, x=Longitude)
    } else {
      DATA_lm<- FTAB %>%
        dplyr::select(Ref.No,Effect,Crop,Delta_temp,CO2.ppm,TempAvg,Precip,Delta_precip,Latitude,Longitude,Fertiliser,Irrigation,Cultivar,Soil.organic.matter.management,Planting.time,Tillage,Others) %>%
        dplyr::rename(y=Latitude, x=Longitude)
    }
    DATA<- FTAB %>%
      dplyr::select(Effect,Crop,Delta_temp,CO2.ppm,TempAvg,Precip,Delta_precip,Latitude,Longitude,Fertiliser,Irrigation,Cultivar,Soil.organic.matter.management,Planting.time,Tillage,Others)
  }else{
    if(same_locations){
      DATA_lm<- FTAB %>%
        dplyr::select(Effect,Crop,Delta_temp,CO2.ppm,Adaptation,TempAvg,Precip,Delta_precip,Latitude,Longitude,Loca) %>%
        dplyr::rename(y=Latitude, x=Longitude)
    } else {
      DATA_lm<- FTAB %>%
        dplyr::select(Ref.No,Effect,Crop,Delta_temp,CO2.ppm,Adaptation,TempAvg,Precip,Delta_precip,Latitude,Longitude) %>%
        dplyr::rename(y=Latitude, x=Longitude)
    }
    DATA<- FTAB %>%
      dplyr::select(Effect,Crop,Delta_temp,CO2.ppm,Adaptation,TempAvg,Precip,Delta_precip,Latitude,Longitude)
   }
  
  df <- NULL
  df_lm <- NULL
  df_xy <- NULL
  Training_xy <- NULL
  Testing_xy <- NULL
  Training_0 <- NULL
  Training_1 <- NULL
  Testing_1 <- NULL
  Training_2 <- NULL
  Testing_2 <- NULL
  Locs <- NULL
  
  for (i in 1:length(crop_species)){

      completeFun <- function(data, desiredCols) {
        completeVec <- complete.cases(data[, desiredCols])
        return(data[completeVec, ])
      }
    
    df[[i]]<-DATA[DATA$Crop==crop_species[i],]
    df[[i]]<-completeFun(df[[i]], names(df[[i]]))
    df_lm[[i]]<-DATA_lm[DATA_lm$Crop==crop_species[i],]
    df_lm[[i]]<-completeFun(df_lm[[i]], names(df_lm[[i]]))
    
    #Remove Crop
    df[[i]]<-df[[i]][ , -which(names(df[[i]]) %in% c("Crop"))]
    df_lm[[i]]<-df_lm[[i]][ , -which(names(df_lm[[i]]) %in% c("Crop"))]
    
    #Remove where N<24 (instance of N=23 removed)
    if(adaptation_types){
      df[[i]]<-df[[i]][ , -which(names(df[[i]]) %in% c("Tillage","Soil.organic.matter.management"))]
      df_lm[[i]]<-df_lm[[i]][ , -which(names(df_lm[[i]]) %in% c("Tillage","Soil.organic.matter.management"))]
      
      if(i==1|i==4){
        #Remove SOM management and Tillage and Others
        df[[i]]<-df[[i]][ , -which(names(df[[i]]) %in% c("Others"))]
        df_lm[[i]]<-df_lm[[i]][ , -which(names(df_lm[[i]]) %in% c("Others"))]
      }
      
      if(i==2|i==4){
        df[[i]]<-df[[i]][ , -which(names(df[[i]]) %in% c("Irrigation"))]
        df_lm[[i]]<-df_lm[[i]][ , -which(names(df_lm[[i]]) %in% c("Irrigation"))]
      }
      
      if(i==1){
        df[[i]]<-df[[i]][ , -which(names(df[[i]]) %in% c("Fertiliser"))]
      df_lm[[i]]<-df_lm[[i]][ , -which(names(df_lm[[i]]) %in% c("Fertiliser"))]
      }
      
      if(i==4){
        df[[i]]<-df[[i]][ , -which(names(df[[i]]) %in% c("Planting.time"))]
        df_lm[[i]]<-df_lm[[i]][ , -which(names(df_lm[[i]]) %in% c("Planting.time"))]
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
    
    if(train_on_all){
      Training_xy[[i]]<-df_xy[[i]]
      Testing_xy[[i]]<-df_xy[[i]]
      Training_1[[i]]<-df[[i]]
      Testing_1[[i]]<-df[[i]]
      Training_2[[i]]<-df_lm[[i]]
      Testing_2[[i]]<-df_lm[[i]]
    }else{
      Training_xy[[i]]<-df_xy[[i]][-ID,]
      Testing_xy[[i]]<-df_xy[[i]][ID,]
      Training_1[[i]]<-df[[i]][-ID,]
      Testing_1[[i]]<-df[[i]][ID,]
      Training_2[[i]]<-df_lm[[i]][-ID,]
      Testing_2[[i]]<-df_lm[[i]][ID,]
    }

    Training_0[[i]] <- Training_1[[i]][,-which(names(Training_1[[i]]) %in% c("Latitude","Longitude"))]
    
  }
  
  #####Training and Testing#####
  
  if(run_all_mods){
    #####Spatial RF#####
    mod.rf.xy <- NULL
    RMSEP_rf_xy <- NULL
    R2_rf_xy_int <- NULL
    R2_rf_xy <- NULL
    Pred_rf_xy <- NULL
    AIC_rf_xy <- NULL
    for(i in 1:length(crop_species)){
      set.seed(1234)
      #mod.rf.xy[[i]] <- ranger(Effect~., data=Training_xy[[i]], num.trees=1000)
      mod.rf.xy[[i]] <- ranger(Effect~., data=Training_xy[[i]], num.trees=1000, importance="permutation") #RZA ADD
      
      predictor.rf.xy <- Predictor$new(model = mod.rf.xy[[i]], data = Training_xy[[i]][-1], y = Training_xy[[i]]$Effect, predict.fun = pfun)
      
      Pred_rf_xy[[i]]<-predict( mod.rf.xy[[i]], data=Testing_xy[[i]])$predictions
      RMSEP_rf_xy[[i]]=sqrt(mean((Testing_xy[[i]]$Effect-Pred_rf_xy[[i]])^2))
      #print(plot(Testing_xy[[i]]$Effect,Pred_rf_xy[[i]]))
      R2_rf_xy_int[[i]] <- mod.rf.xy[[i]]$r.squared
      R2_rf_xy[[i]] <- 1 - sum( (Testing_xy[[i]]$Effect-Pred_rf_xy[[i]])^2 ) / sum( (Testing_xy[[i]]$Effect - mean(Testing_xy[[i]]$Effect))^2 )
      AIC_rf_xy[[i]] <- mean((Testing_xy[[i]]$Effect-Pred_rf_xy[[i]])^2) + 2*dim(Training_xy[[i]][-1])[2]
    }
    
    #####Spatial GBM#####
    mod.gbm.xy <- NULL
    RMSEP_gbm_xy <- NULL
    Pred_gbm_xy <- NULL
    R2_gbm_xy_int <- NULL
    R2_gbm_xy <- NULL
    AIC_gbm_xy <- NULL
    for(i in 1:length(crop_species)){
      set.seed(1234)
      mod.gbm.xy[[i]] <- do_GBM(Training_xy[[i]])
      
      predictor.gbm.xy <- Predictor$new(model = mod.gbm.xy[[i]], data = Training_xy[[i]][-1], y = Training_xy[[i]]$Effect)
      
      Pred_gbm_xy[[i]] <- predict(mod.gbm.xy[[i]], newdata=Testing_xy[[i]])
      RMSEP_gbm_xy[[i]]=sqrt(mean((Testing_xy[[i]]$Effect-Pred_gbm_xy[[i]])^2))
      R2_gbm_xy_int[[i]] <- mod.gbm.xy[[i]][4]$results$Rsquared[which.min(mod.gbm.xy[[i]][4]$results$RMSE)]
      R2_gbm_xy[[i]] <- 1 - sum( (Testing_xy[[i]]$Effect-Pred_gbm_xy[[i]])^2 ) / sum( (Testing_xy[[i]]$Effect - mean(Testing_xy[[i]]$Effect))^2 )
      AIC_gbm_xy[[i]] <- mean((Testing_xy[[i]]$Effect-Pred_gbm_xy[[i]])^2) + 2*dim(Training_xy[[i]][-1])[2]
    }
    
    
    #####Climate RF#####
    mod.rf.0 <- NULL
    Pred_rf_0 <- NULL
    R2_rf_0_int <- NULL
    RMSEP_rf_0 <- NULL
    R2_rf_0 <- NULL
    Training_2_bis <- NULL
    Pred_rf_0 <- NULL
    AIC_rf_0 <- NULL
    for(i in 1:length(crop_species)){
      Training_0[[i]] <- Training_1[[i]][,-which(names(Training_1[[i]]) %in% c("Latitude","Longitude"))]
      set.seed(1234)
      #mod.rf.0[[i]] <- ranger(Effect~., data=Training_0[[i]], num.trees=1000)
      mod.rf.0[[i]] <- ranger(Effect~., data=Training_0[[i]], num.trees=1000, importance="permutation") #RZA ADD
      predictor.rf.0 <- Predictor$new(mod.rf.0[[i]], data = Training_0[[i]][-1], y = Training_0[[i]]$Effect, predict.fun = pfun)
      
      Pred_rf_0[[i]]<-predict( mod.rf.0[[i]], data=Testing_1[[i]])$predictions
      RMSEP_rf_0[[i]]=sqrt(mean((Testing_1[[i]]$Effect-Pred_rf_0[[i]])^2))
      R2_rf_0_int[[i]] <- mod.rf.0[[i]]$r.squared
      R2_rf_0[[i]] <- 1 - sum( (Testing_1[[i]]$Effect-Pred_rf_0[[i]])^2 ) / sum( (Testing_1[[i]]$Effect - mean(Testing_1[[i]]$Effect))^2 )
      AIC_rf_0[[i]] <- mean((Testing_1[[i]]$Effect-Pred_rf_0[[i]])^2) + 2*dim(Training_0[[i]][-1])[2]
      
      X0<- predict(mod.rf.0[[i]], data=Training_1[[i]])$predictions
      Training_2_bis[[i]]<-cbind(Training_2[[i]],X0)
    }
    
  }
  
    ####Climate GBM#####
    mod.gbm.0 <- NULL
    Pred_gbm_0 <- NULL
    RMSEP_gbm_0 <- NULL
    R2_gbm_0_int <- NULL
    R2_gbm_0 <- NULL
    AIC_gbm_0 <- NULL
    
    for(i in 1:length(crop_species)){
      set.seed(1234)
      mod.gbm.0[[i]] <- do_GBM(Training_0[[i]])
      predictor.gbm.0 <- Predictor$new(mod.gbm.0[[i]], data = Training_0[[i]][-1], y = Training_0[[i]]$Effect)
      
      Pred_gbm_0[[i]] <- predict(mod.gbm.0[[i]], newdata=Testing_1[[i]])
      RMSEP_gbm_0[[i]]=sqrt(mean((Testing_1[[i]]$Effect-Pred_gbm_0[[i]])^2))
      R2_gbm_0_int[[i]] <- mod.gbm.0[[i]][4]$results$Rsquared[which.min(mod.gbm.0[[i]][4]$results$RMSE)]
      R2_gbm_0[[i]] <- 1 - sum( (Testing_1[[i]]$Effect-Pred_gbm_0[[i]])^2 ) / sum( (Testing_1[[i]]$Effect - mean(Testing_1[[i]]$Effect))^2 )
      AIC_gbm_0[[i]] <- mean((Testing_1[[i]]$Effect-Pred_gbm_0[[i]])^2) + 2*dim(Training_0[[i]][-1])[2]
    
      if(i==1){
        bestPred[[i]] <- as.data.frame(cbind(Pred_gbm_0[[i]], Testing_1[[i]]$Effect, rep(crop_species[i], length(Pred_gbm_0[[i]])), row.names(Testing_1[[i]])))
        bestPred[[i]][,1] <- as.numeric(bestPred[[i]][,1])
        bestPred[[i]][,2] <- as.numeric(bestPred[[i]][,2])
        names(bestPred[[i]]) <- c("Predicted", "Observed", "Crop", "Site")
      }
      
      if(do_iml){
        imp <- FeatureImp$new(predictor.gbm.0, loss = "mae")
        
        #strength of interactions
        interact <- Interaction$new(predictor.gbm.0)
        
        #feature effects
        effs <- FeatureEffects$new(predictor.gbm.0, method="pdp")
        
        twoway <- FeatureEffect$new(predictor.gbm.0, feature = c("Delta_temp", "Adaptation"), method="pdp")
        
       if(i==1){
          bestEffs[[i]] <- effs
          best2way[[i]] <- twoway
       }
      }
    
      
      }
  
  ####Climate+longlat RF#####
  mod.rf.1 <- NULL
  RMSEP_rf_1 <- NULL
  Pred_rf_1 <- NULL
  R2_rf_1_int <- NULL
  R2_rf_1 <- NULL
  AIC_rf_1 <- NULL
  for(i in 1:length(crop_species)){
    set.seed(1234)
    #if(same_locations){ #RZA REMOVE
      mod.rf.1[[i]] <- ranger(Effect~., data=Training_1[[i]], num.trees=1000, importance="permutation")
    # } else {
    #   mod.rf.1[[i]] <- ranger(Effect~., data=Training_1[[i]], num.trees=1000)
    # }
    
    predictor.rf.1 <- Predictor$new(mod.rf.1[[i]], data = Training_1[[i]][-1], y = Training_1[[i]]$Effect, predict.fun = pfun)
    Pred_rf_1[[i]]<-predict( mod.rf.1[[i]], data=Testing_1[[i]])$predictions
    RMSEP_rf_1[[i]]=sqrt(mean((Testing_1[[i]]$Effect-Pred_rf_1[[i]])^2))
    R2_rf_1_int[[i]] <- mod.rf.1[[i]]$r.squared
    R2_rf_1[[i]] <- 1 - sum( (Testing_1[[i]]$Effect-Pred_rf_1[[i]])^2 ) / sum( (Testing_1[[i]]$Effect - mean(Testing_1[[i]]$Effect))^2 )
    AIC_rf_1[[i]] <- mean((Testing_1[[i]]$Effect-Pred_rf_1[[i]])^2) + 2*dim(Training_1[[i]][-1])[2]
    
    if(i==2|i==4){
      bestPred[[i]] <- as.data.frame(cbind(Pred_rf_1[[i]], Testing_1[[i]]$Effect, rep(crop_species[i], length(Pred_rf_1[[i]])), row.names(Testing_1[[i]])))
      bestPred[[i]][,1] <- as.numeric(bestPred[[i]][,1])
      bestPred[[i]][,2] <- as.numeric(bestPred[[i]][,2])
      names(bestPred[[i]]) <- c("Predicted", "Observed", "Crop", "Site")
    }
    
    if(do_iml){
      imp <- FeatureImp$new(predictor.rf.1, loss = "mae")
      
      #strength of interactions
      interact <- Interaction$new(predictor.rf.1)
      
      #feature effects
      effs <- FeatureEffects$new(predictor.rf.1, method="pdp")
      
      twoway <- FeatureEffect$new(predictor.rf.1, feature = c("Delta_temp", "Adaptation"), method="pdp")
      
    if(i==2|i==4){
      bestEffs[[i]] <- effs
      best2way[[i]] <- twoway
    }
    }

  }
  
    #####Climate+longlat GBM#####
    mod.gbm.1 <- NULL
    RMSEP_gbm_1 <- NULL
    Pred_gbm_1 <- NULL
    R2_gbm_1_int <- NULL
    R2_gbm_1 <- NULL
    AIC_gbm_1 <- NULL
    for(i in 1:length(crop_species)){
      set.seed(1234)
      mod.gbm.1[[i]] <- do_GBM(Training_1[[i]])
      
      predictor.gbm.1 <- Predictor$new(mod.gbm.1[[i]], data = Training_1[[i]][-1], y = Training_1[[i]]$Effect)
      
      Pred_gbm_1[[i]] <- predict(mod.gbm.1[[i]], newdata=Testing_1[[i]])
      RMSEP_gbm_1[[i]]=sqrt(mean((Testing_1[[i]]$Effect-Pred_gbm_1[[i]])^2))
      R2_gbm_1_int[[i]] <- mod.gbm.1[[i]][4]$results$Rsquared[which.min(mod.gbm.1[[i]][4]$results$RMSE)]
      R2_gbm_1[[i]] <- 1 - sum( (Testing_1[[i]]$Effect-Pred_gbm_1[[i]])^2 ) / sum( (Testing_1[[i]]$Effect - mean(Testing_1[[i]]$Effect))^2 )
      AIC_gbm_1[[i]] <- mean((Testing_1[[i]]$Effect-Pred_gbm_1[[i]])^2) + 2*dim(Training_1[[i]][-1])[2]
    
      if(i==3){  
        bestPred[[i]] <- as.data.frame(cbind(Pred_gbm_1[[i]], Testing_1[[i]]$Effect, rep(crop_species[i], length(Pred_gbm_1[[i]])), row.names(Testing_1[[i]])))
        bestPred[[i]][,1] <- as.numeric(bestPred[[i]][,1])
        bestPred[[i]][,2] <- as.numeric(bestPred[[i]][,2])
        names(bestPred[[i]]) <- c("Predicted", "Observed", "Crop", "Site")
      }
      
      if(do_iml){
        imp <- FeatureImp$new(predictor.gbm.1, loss = "mae")
        
        #strength of interactions
        interact <- Interaction$new(predictor.gbm.1)
        
        #feature effects
        effs <- FeatureEffects$new(predictor.gbm.1, method="pdp")
        
        twoway <- FeatureEffect$new(predictor.gbm.1, feature = c("Delta_temp", "Adaptation"), method="pdp")
        
        if(i==3){  
          bestEffs[[i]] <- effs
          best2way[[i]] <- twoway
        }
      }  
      
    }
    
    if(run_all_mods){
    #####Mixed models#####
    ####Climate#####
    mod_lm <- NULL
    out_lm <- NULL
    RMSEP_lm <- NULL
    R2_lm <- NULL
    Pred_lm <- NULL
    AIC_lm <- NULL
    for(i in 1:length(crop_species)){
      #Linear model climate
      if(adaptation_types){
        if(same_locations){
          if(i==1){
            mod_lm[[i]]<-lmer(Effect~Delta_temp+TempAvg+Precip+Delta_precip+CO2.ppm+Irrigation+Fertiliser+Cultivar+Planting.time+(1|Loca), data=Training_2[[i]])
            out_lm[[i]]<-anova(lme(Effect~Delta_temp+TempAvg+Precip+Delta_precip+CO2.ppm+Irrigation+Fertiliser+Cultivar+Planting.time, random=~1|Loca, data=Training_2[[i]]))
          }
          if(i==2){
            mod_lm[[i]]<-lmer(Effect~Delta_temp+TempAvg+Precip+Delta_precip+CO2.ppm+Fertiliser+Cultivar+Planting.time+Others+(1|Loca), data=Training_2[[i]])
            out_lm[[i]]<-anova(lme(Effect~Delta_temp+TempAvg+Precip+Delta_precip+CO2.ppm+Fertiliser+Cultivar+Planting.time+Others, random=~1|Loca, data=Training_2[[i]]))
          }
          if(i==3){
            mod_lm[[i]]<-lmer(Effect~Delta_temp+TempAvg+Precip+Delta_precip+CO2.ppm+Irrigation+Fertiliser+Cultivar+Planting.time+Others+(1|Loca), data=Training_2[[i]])
            out_lm[[i]]<-anova(lme(Effect~Delta_temp+TempAvg+Precip+Delta_precip+CO2.ppm+Irrigation+Fertiliser+Cultivar+Planting.time+Others, random=~1|Loca, data=Training_2[[i]]))
          }
          if(i==4){
            mod_lm[[i]]<-lmer(Effect~Delta_temp+TempAvg+Precip+Delta_precip+CO2.ppm+Fertiliser+Cultivar+Planting.time+(1|Loca), data=Training_2[[i]])
            out_lm[[i]]<-anova(lme(Effect~Delta_temp+TempAvg+Precip+Delta_precip+CO2.ppm+Fertiliser+Cultivar+Planting.time, random=~1|Loca, data=Training_2[[i]]))
          }
        } else {
          if(i==1){
            mod_lm[[i]]<-lmer(Effect~Delta_temp+TempAvg+Precip+Delta_precip+CO2.ppm+Irrigation+Fertiliser+Cultivar+Planting.time+(1|Ref.No), data=Training_2[[i]])
            out_lm[[i]]<-anova(lme(Effect~Delta_temp+TempAvg+Precip+Delta_precip+CO2.ppm+Irrigation+Fertiliser+Cultivar+Planting.time, random=~1|Ref.No, data=Training_2[[i]]))
          }
          if(i==2){
            mod_lm[[i]]<-lmer(Effect~Delta_temp+TempAvg+Precip+Delta_precip+CO2.ppm+Fertiliser+Cultivar+Planting.time+Others+(1|Ref.No), data=Training_2[[i]])
            out_lm[[i]]<-anova(lme(Effect~Delta_temp+TempAvg+Precip+Delta_precip+CO2.ppm+Fertiliser+Cultivar+Planting.time+Others, random=~1|Ref.No, data=Training_2[[i]]))
          }
          if(i==3){
            mod_lm[[i]]<-lmer(Effect~Delta_temp+TempAvg+Precip+Delta_precip+CO2.ppm+Irrigation+Fertiliser+Cultivar+Planting.time+Others+(1|Ref.No), data=Training_2[[i]])
            out_lm[[i]]<-anova(lme(Effect~Delta_temp+TempAvg+Precip+Delta_precip+CO2.ppm+Irrigation+Fertiliser+Cultivar+Planting.time+Others, random=~1|Ref.No, data=Training_2[[i]]))
          }
          if(i==4){
            mod_lm[[i]]<-lmer(Effect~Delta_temp+TempAvg+Precip+Delta_precip+CO2.ppm+Fertiliser+Cultivar+Planting.time+(1|Ref.No), data=Training_2[[i]])
            out_lm[[i]]<-anova(lme(Effect~Delta_temp+TempAvg+Precip+Delta_precip+CO2.ppm+Fertiliser+Cultivar+Planting.time, random=~1|Ref.No, data=Training_2[[i]]))
          }
        }
      }else{
        if(same_locations){
          mod_lm[[i]]<-lmer(Effect~Delta_temp+TempAvg+Precip+Delta_precip+Adaptation+CO2.ppm+(1|Loca), data=Training_2[[i]])
          out_lm[[i]]<-anova(lme(Effect~Delta_temp+TempAvg+Precip+Delta_precip+Adaptation+CO2.ppm, random=~1|Loca, data=Training_2[[i]]))
        } else {
          mod_lm[[i]]<-lmer(Effect~Delta_temp+TempAvg+Precip+Delta_precip+Adaptation+CO2.ppm+(1|Ref.No), data=Training_2[[i]])
          out_lm[[i]]<-anova(lme(Effect~Delta_temp+TempAvg+Precip+Delta_precip+Adaptation+CO2.ppm, random=~1|Ref.No, data=Training_2[[i]]))
        }
      }
      
      if(same_locations){
        Pred_lm[[i]]<-predict(mod_lm[[i]], newdata=Testing_2[[i]]) #removed random effect on Jul 5, 2021
      } else {
        Pred_lm[[i]]<-predict(mod_lm[[i]], newdata=Testing_2[[i]], re.form=NA) #NA means include no random effects
      }
      
      RMSEP_lm[[i]]=sqrt(mean((Testing_2[[i]]$Effect-Pred_lm[[i]])^2))
      R2_lm[[i]] <- 1 - sum( (Testing_2[[i]]$Effect-Pred_lm[[i]])^2 ) / sum( (Testing_2[[i]]$Effect - mean(Testing_2[[i]]$Effect))^2 )
      AIC_lm[[i]] <- mean((Testing_2[[i]]$Effect-Pred_lm[[i]])^2) + 2*dim(Training_2[[i]][-1])[2]
    }
    
    #####Climate+Interactions#####
    mod_lm_ints <- NULL
    out_lm_ints <- NULL
    RMSEP_lm_ints <- NULL
    R2_lm_ints <- NULL
    Pred_lm_ints <- NULL
    AIC_lm_ints <- NULL
    for(i in 1:length(crop_species)){
      #Linear model climate
      if(adaptation_types){
        if(same_locations){
          if(i==1){
            mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*TempAvg*Precip*Delta_precip*CO2.ppm*(Irrigation+Fertiliser+Cultivar+Planting.time)+(1|Loca), data=Training_2[[i]])
            out_lm_ints[[i]]<-anova(lme(Effect~Delta_temp*TempAvg*Precip*Delta_precip*CO2.ppm*(Irrigation+Fertiliser+Cultivar+Planting.time), random=~1|Loca, data=Training_2[[i]]))
          }
          if(i==2){
            mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*TempAvg*Precip*Delta_precip*CO2.ppm*(Fertiliser+Cultivar+Planting.time+Others)+(1|Loca), data=Training_2[[i]])
            out_lm_ints[[i]]<-anova(lme(Effect~Delta_temp*TempAvg*Precip*Delta_precip*CO2.ppm*(Fertiliser+Cultivar+Planting.time+Others), random=~1|Loca, data=Training_2[[i]]))
          }
          if(i==3){
            mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*TempAvg*Precip*Delta_precip*CO2.ppm*(Irrigation+Fertiliser+Cultivar+Planting.time+Others)+(1|Loca), data=Training_2[[i]])
            out_lm_ints[[i]]<-anova(lme(Effect~Delta_temp*TempAvg*Precip*Delta_precip*CO2.ppm*(Irrigation+Fertiliser+Cultivar+Planting.time+Others), random=~1|Loca, data=Training_2[[i]]))
          }
          if(i==4){
            mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*TempAvg*Precip*Delta_precip*CO2.ppm*(Fertiliser+Cultivar+Planting.time)+(1|Loca), data=Training_2[[i]])
            out_lm_ints[[i]]<-anova(lme(Effect~Delta_temp*TempAvg*Precip*Delta_precip*CO2.ppm*(Fertiliser+Cultivar+Planting.time), random=~1|Loca, data=Training_2[[i]]))
          }
        } else {
          if(i==1){
            mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*TempAvg*Precip*Delta_precip*CO2.ppm*(Irrigation+Fertiliser+Cultivar+Planting.time)+(1|Ref.No), data=Training_2[[i]])
            out_lm_ints[[i]]<-anova(lme(Effect~Delta_temp*TempAvg*Precip*Delta_precip*CO2.ppm*(Irrigation+Fertiliser+Cultivar+Planting.time), random=~1|Ref.No, data=Training_2[[i]]))
          }
          if(i==2){
            mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*TempAvg*Precip*Delta_precip*CO2.ppm*(Fertiliser+Cultivar+Planting.time+Others)+(1|Ref.No), data=Training_2[[i]])
            out_lm_ints[[i]]<-anova(lme(Effect~Delta_temp*TempAvg*Precip*Delta_precip*CO2.ppm*(Fertiliser+Cultivar+Planting.time+Others), random=~1|Ref.No, data=Training_2[[i]]))
          }
          if(i==3){
            mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*TempAvg*Precip*Delta_precip*CO2.ppm*(Irrigation+Fertiliser+Cultivar+Planting.time+Others)+(1|Ref.No), data=Training_2[[i]])
            out_lm_ints[[i]]<-anova(lme(Effect~Delta_temp*TempAvg*Precip*Delta_precip*CO2.ppm*(Irrigation+Fertiliser+Cultivar+Planting.time+Others), random=~1|Ref.No, data=Training_2[[i]]))
          }
          if(i==4){
            mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*TempAvg*Precip*Delta_precip*CO2.ppm*(Fertiliser+Cultivar+Planting.time)+(1|Ref.No), data=Training_2[[i]])
            out_lm_ints[[i]]<-anova(lme(Effect~Delta_temp*TempAvg*Precip*Delta_precip*CO2.ppm*(Fertiliser+Cultivar+Planting.time), random=~1|Ref.No, data=Training_2[[i]]))
          }
        }
      }else{  
        
        if(same_locations){
          mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*Precip*Delta_precip*CO2.ppm*Adaptation*TempAvg+(1|Loca), data=Training_2[[i]])
          out_lm_ints[[i]]<-anova(lme(Effect~Delta_temp*Precip*Delta_precip*CO2.ppm*Adaptation*TempAvg, random=~1|Loca, data=Training_2[[i]]))
        } else {
          mod_lm_ints[[i]]<-lmer(Effect~Delta_temp*Precip*Delta_precip*CO2.ppm*Adaptation*TempAvg+(1|Ref.No), data=Training_2[[i]])
          out_lm_ints[[i]]<-anova(lme(Effect~Delta_temp*Precip*Delta_precip*CO2.ppm*Adaptation*TempAvg, random=~1|Ref.No, data=Training_2[[i]]))
        }
      }
      
      if(same_locations){
        Pred_lm_ints[[i]]<-predict(mod_lm_ints[[i]], newdata=Testing_2[[i]], re.form=NA) #removed random effect on Jul 5, 2021
      } else {
        Pred_lm_ints[[i]]<-predict(mod_lm_ints[[i]], newdata=Testing_2[[i]], re.form=NA)
      }
      
      RMSEP_lm_ints[[i]]=sqrt(mean((Testing_2[[i]]$Effect-Pred_lm_ints[[i]])^2))
      R2_lm_ints[[i]] <- 1 - sum( (Testing_2[[i]]$Effect-Pred_lm_ints[[i]])^2 ) / sum( (Testing_2[[i]]$Effect - mean(Testing_2[[i]]$Effect))^2 )
      AIC_lm_ints[[i]] <- mean((Testing_2[[i]]$Effect-Pred_lm_ints[[i]])^2) + 2*dim(Training_2[[i]][-1])[2]
    }
    
    #####Spatial#####
    mod_spa_0 <- NULL
    RMSEP_spa_0 <- NULL
    R2_spa_0 <- NULL
    Pred_spa_0 <- NULL
    AIC_spa_0 <- NULL
    for(i in 1:length(crop_species)){
      #Spatial linear model
      mod_spa_0[[i]]<-fitme(Effect~1+Matern(1|x+y), data=Training_2[[i]])
      
      Pred_spa_0[[i]]<-predict(mod_spa_0[[i]], newdata=Testing_2[[i]])
      RMSEP_spa_0[[i]]=sqrt(mean((Testing_2[[i]]$Effect-Pred_spa_0[[i]])^2))
      R2_spa_0[[i]] <- 1 - sum( (Testing_2[[i]]$Effect-Pred_spa_0[[i]])^2 ) / sum( (Testing_2[[i]]$Effect - mean(Testing_2[[i]]$Effect))^2 )
      AIC_spa_0[[i]] <- mean((Testing_2[[i]]$Effect-Pred_spa_0[[i]])^2) + 2*dim(Training_2[[i]][-1])[2]
    }
    
    #####Spatial climate#####
    mod_spa_lm <- NULL
    RMSEP_spa_lm <- NULL
    R2_spa_lm <- NULL
    Pred_spa_lm <- NULL
    AIC_spa_lm <- NULL
    for(i in 1:length(crop_species)){
      #Spatial linear model + climate + CO2
      
      if(adaptation_types){
        if(i==1){
          mod_spa_lm[[i]]<-fitme(Effect~Delta_temp+TempAvg+Precip+Delta_precip+CO2.ppm+Irrigation+Fertiliser+Cultivar+Planting.time+Matern(1|x+y), data=Training_2[[i]])
        }
        if(i==2){
          mod_spa_lm[[i]]<-fitme(Effect~Delta_temp+TempAvg+Precip+Delta_precip+CO2.ppm+Fertiliser+Cultivar+Planting.time+Others+Matern(1|x+y), data=Training_2[[i]])
        }
        if(i==3){
          mod_spa_lm[[i]]<-fitme(Effect~Delta_temp+TempAvg+Precip+Delta_precip+CO2.ppm+Irrigation+Fertiliser+Cultivar+Planting.time+Others+Matern(1|x+y), data=Training_2[[i]])
        }
        if(i==4){
          mod_spa_lm[[i]]<-fitme(Effect~Delta_temp+TempAvg+Precip+Delta_precip+CO2.ppm+Fertiliser+Cultivar+Matern(1|x+y), data=Training_2[[i]]) #For some reason planting time doesn't work here so removed
        }
      }else{
        mod_spa_lm[[i]]<-fitme(Effect~Delta_temp+TempAvg+Precip+Delta_precip+Adaptation+CO2.ppm+Matern(1|x+y), data=Training_2[[i]])
      }
      
      Pred_spa_lm[[i]]<-predict(mod_spa_lm[[i]], newdata=Testing_2[[i]])
      RMSEP_spa_lm[[i]]=sqrt(mean((Testing_2[[i]]$Effect-Pred_spa_lm[[i]])^2))
      R2_spa_lm[[i]] <- 1 - sum( (Testing_2[[i]]$Effect-Pred_spa_lm[[i]])^2 ) / sum( (Testing_2[[i]]$Effect - mean(Testing_2[[i]]$Effect))^2 )
      AIC_spa_lm[[i]] <- mean((Testing_2[[i]]$Effect-Pred_spa_lm[[i]])^2) + 2*dim(Training_2[[i]][-1])[2]
    }
    
    #####Spatial+RF outputs#####
    mod_spa_rf <- NULL
    RMSEP_spa_rf <- NULL
    Testing_2_bis <- NULL
    R2_spa_rf <- NULL
    Pred_spa_rf <- NULL
    AIC_spa_rf <- NULL
    for(i in 1:length(crop_species)){
      #Sptial linear model + RF outputs
      mod_spa_rf[[i]]<-fitme(Effect~X0+Matern(1|x+y), data=Training_2_bis[[i]])
      
      Testing_2_bis[[i]]<-cbind(Testing_2[[i]], X0=Pred_rf_0[[i]])
      Pred_spa_rf[[i]]<-predict(mod_spa_rf[[i]], newdata=Testing_2_bis[[i]])
      RMSEP_spa_rf[[i]]=sqrt(mean((Testing_2_bis[[i]]$Effect-Pred_spa_rf[[i]])^2))
      R2_spa_rf[[i]] <- 1 - sum( (Testing_2[[i]]$Effect-Pred_spa_rf[[i]])^2 ) / sum( (Testing_2[[i]]$Effect - mean(Testing_2[[i]]$Effect))^2 )
      AIC_spa_rf[[i]] <- mean((Testing_2[[i]]$Effect-Pred_spa_rf[[i]])^2) + 2*dim(Training_2_bis[[i]][-1])[2]
    }
    
    }
    
  #####End Training and Testing#####
  for(i in 1:length(crop_species)){  
    
    if(run_all_mods){
      RMSEP_rf_xy_vec[j,i]<-RMSEP_rf_xy[[i]]
      RMSEP_rf_0_vec[j,i]<-RMSEP_rf_0[[i]]
      RMSEP_gbm_xy_vec[j,i]<-RMSEP_gbm_xy[[i]]
      RMSEP_lm_vec[j,i]<-RMSEP_lm[[i]]
      RMSEP_lm_ints_vec[j,i]<-RMSEP_lm_ints[[i]]
      RMSEP_spa_0_vec[j,i]<-RMSEP_spa_0[[i]]
      RMSEP_spa_lm_vec[j,i]<-RMSEP_spa_lm[[i]]
      RMSEP_spa_rf_vec[j,i]<-RMSEP_spa_rf[[i]]
    }
    
    RMSEP_rf_1_vec[j,i]<-RMSEP_rf_1[[i]]
    RMSEP_gbm_0_vec[j,i]<-RMSEP_gbm_0[[i]]
    RMSEP_gbm_1_vec[j,i]<-RMSEP_gbm_1[[i]]
    
    if(run_all_mods){
      R2_rf_xy_vec[j,i]<-R2_rf_xy[[i]]
      R2_rf_0_vec[j,i]<-R2_rf_0[[i]]
      R2_gbm_xy_vec[j,i]<-R2_gbm_xy[[i]]
      R2_lm_vec[j,i]<-R2_lm[[i]]
      R2_lm_ints_vec[j,i]<-R2_lm_ints[[i]]
      R2_spa_0_vec[j,i]<-R2_spa_0[[i]]
      R2_spa_lm_vec[j,i]<-R2_spa_lm[[i]]
      R2_spa_rf_vec[j,i]<-R2_spa_rf[[i]]
    }
    
    R2_rf_1_vec[j,i]<-R2_rf_1[[i]]
    R2_gbm_0_vec[j,i]<-R2_gbm_0[[i]]
    R2_gbm_1_vec[j,i]<-R2_gbm_1[[i]]
    
    if(run_all_mods){
      AIC_rf_xy_vec[j,i]<-AIC_rf_xy[[i]]
      AIC_rf_0_vec[j,i]<-AIC_rf_0[[i]]
      AIC_gbm_xy_vec[j,i]<-AIC_gbm_xy[[i]]
      AIC_lm_vec[j,i]<-AIC_lm[[i]]
      AIC_lm_ints_vec[j,i]<-AIC_lm_ints[[i]]
      AIC_spa_0_vec[j,i]<-AIC_spa_0[[i]]
      AIC_spa_lm_vec[j,i]<-AIC_spa_lm[[i]]
      AIC_spa_rf_vec[j,i]<-AIC_spa_rf[[i]]
    }
    
    AIC_rf_1_vec[j,i]<-AIC_rf_1[[i]]
    AIC_gbm_0_vec[j,i]<-AIC_gbm_0[[i]]
    AIC_gbm_1_vec[j,i]<-AIC_gbm_1[[i]]
  }
  
  ObsPred[[j]] <- rbind(bestPred[[1]], bestPred[[2]], bestPred[[3]], bestPred[[4]])
  #print(ObsPred)
  
  if(do_iml){
    PDPeffs[[j]] <- list(bestEffs[[1]], bestEffs[[2]], bestEffs[[3]], bestEffs[[4]])
    PDP2way[[j]] <- list(best2way[[1]], best2way[[2]], best2way[[3]], best2way[[4]])
  }
  save.mod.rf[[j]] <- list(mod.gbm.0[[1]], mod.rf.1[[2]], mod.gbm.1[[3]], mod.rf.1[[4]]) 
  save.Training.rf[[j]] <- list(Training_0[[1]], Training_1[[2]], Training_1[[3]], Training_1[[4]])
  if(run_all_mods){
  save.mod.out.lm[[j]] <- list(out_lm[[1]], out_lm[[2]], out_lm[[3]], out_lm[[4]])
  save.mod.out.lm.ints[[j]] <- list(out_lm_ints[[1]], out_lm_ints[[2]], out_lm_ints[[3]], out_lm_ints[[4]])
  }
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
  NAME<-c("GBM climate","RF climate + long lat","GBM climate + long lat") 
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
    
    AIC_MEAN <- c(
      mean(AIC_rf_xy_vec[,i]),
      mean(AIC_gbm_xy_vec[,i]),
      mean(AIC_rf_0_vec[,i]),
      mean(AIC_gbm_0_vec[,i]),
      mean(AIC_rf_1_vec[,i]),
      mean(AIC_gbm_1_vec[,i]),
      mean(AIC_lm_vec[,i]),
      mean(AIC_lm_ints_vec[,i]),
      mean(AIC_spa_0_vec[,i]),
      mean(AIC_spa_lm_vec[,i]),
      mean(AIC_spa_rf_vec[,i]))
    
    AIC_SE<-c(
      sd(AIC_rf_xy_vec[,i])/sqrt(Nb),
      sd(AIC_gbm_xy_vec[,i])/sqrt(Nb),
      sd(AIC_rf_0_vec[,i])/sqrt(Nb),
      sd(AIC_gbm_0_vec[,i])/sqrt(Nb),
      sd(AIC_rf_1_vec[,i])/sqrt(Nb),
      sd(AIC_gbm_1_vec[,i])/sqrt(Nb),
      sd(AIC_lm_vec[,i])/sqrt(Nb),
      sd(AIC_lm_ints_vec[,i])/sqrt(Nb),
      sd(AIC_spa_0_vec[,i])/sqrt(Nb),
      sd(AIC_spa_lm_vec[,i])/sqrt(Nb),
      sd(AIC_spa_rf_vec[,i])/sqrt(Nb))
  }else{
    RMSEP_MEAN <- c(
      mean(RMSEP_gbm_0_vec[,i]),
      mean(RMSEP_rf_1_vec[,i]),
      mean(RMSEP_gbm_1_vec[,i]))
    
    RMSEP_SE<-c(
      sd(RMSEP_rf_1_vec[,i])/sqrt(Nb),
      sd(RMSEP_gbm_0_vec[,i])/sqrt(Nb),
      sd(RMSEP_gbm_1_vec[,i])/sqrt(Nb))
    
    R2_MEAN <- c(
      mean(R2_rf_1_vec[,i]),
      mean(R2_gbm_0_vec[,i]),
      mean(R2_gbm_1_vec[,i]))
    
    R2_SE<-c(
      sd(R2_rf_1_vec[,i])/sqrt(Nb),
      sd(R2_gbm_0_vec[,i])/sqrt(Nb),
      sd(R2_gbm_1_vec[,i])/sqrt(Nb))
    
    AIC_MEAN <- c(
      mean(AIC_rf_1_vec[,i]),
      mean(AIC_gbm_0_vec[,i]),
      mean(AIC_gbm_1_vec[,i]))
    
    AIC_SE<-c(
      sd(AIC_rf_1_vec[,i])/sqrt(Nb),
      sd(AIC_gbm_0_vec[,i])/sqrt(Nb),
      sd(AIC_gbm_1_vec[,i])/sqrt(Nb))
  }
  
  crop_species_col <- rep(crop_species[[i]], length(R2_MEAN))
  
  ###TEMP
  print(paste("NAME",NAME))
  print(paste("RMSEP_MEAN",RMSEP_MEAN))
  print(paste("crop_species_col",crop_species_col))
  ###TEMP
  
  RESULT[[i]]<-data.frame(NAME,RMSEP_MEAN, RMSEP_SE, R2_MEAN, R2_SE, AIC_MEAN, AIC_SE, crop_species_col)
  print(crop_species[[i]])
  print(RESULT[[i]])
}

print("loop done")

RESULT_ALL <- rbind(RESULT[[1]], RESULT[[2]],RESULT[[3]], RESULT[[4]])

#list(RESULT_ALL, ObsPred, save.mod.rf, save.Training.rf, PDPeffs)

fileprefix <- paste0(outputdir,"sameLocs",same_locations,"_adaptationTypes",adaptation_types,"_doIML",do_iml,"_Nb",Nb,"runAllMods",run_all_mods,"_trainOnAll",train_on_all)

write.csv(file=paste0(fileprefix,"_results.csv"),RESULT_ALL)
save(ObsPred, file=paste0(fileprefix,"_store_bestML_predictions.RData"))
if(do_iml){
  save(PDPeffs, file=paste0(fileprefix,"_store_bestML_PDPs.RData"))
  save(PDP2way, file=paste0(fileprefix,"_store_bestML_PDP2way.RData"))
  #PDPeffs[[Nb 1-10]][[Crop 1-4]]
}
save(save.mod.rf, file=paste0(fileprefix,"_saved_bestMLs.RData"))
save(save.Training.rf,file=paste0(fileprefix,"_saved_trainingData.RData"))

if(run_all_mods){
save(save.mod.out.lm,file=paste0(fileprefix,"_saved_modOutLm.RData"))
save(save.mod.out.lm.ints,file=paste0(fileprefix,"_saved_modOutLmInts.RData"))
}

print("all done")
print(PDPeffs)
}