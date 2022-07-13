readin <- function(){
  datadir <- paste0(rootdir,"/DownloadedFromNature/")
  mdatadir <- paste0(rootdir,"/Data/")
  
  TAB<-read.xlsx(paste0(datadir,"Projected_Impacts_datasheet_11.24.2021.xlsx"), sheet=1)
  
  FTAB <- TAB %>%
    dplyr::rename(Delta_temp = Global.delta.T.from.2005, 
                  Effect = Climate.impacts.relative.to.2005,
                  TempAvg = `Current.Average.Temperature.(dC)_area_weighted`,
                  Latitude = latitude,
                  Longitude = longitude,
                  Precip = `Current.Annual.Precipitation.(mm)._area_weighted`,
                  Delta_precip = `Annual.Precipitation.change.from.2005.(mm)`) %>%
    dplyr::mutate(Loca = paste(Longitude,Latitude, sep="_"),
                  CO2 = replace(CO2, CO2 == "yes", "Yes"),
                  Cultivar = replace(Cultivar, Cultivar == "Yes ", "Yes"),
                  (across(c(CO2, Adaptation, Fertiliser, Irrigation, Cultivar, Soil.organic.matter.management,
                            Planting.time, Tillage, Others),
                                  factor)),
                  CO2.ppm = CO2.ppm - 390)
  return(FTAB)
}

#RCP<-as.factor(TAB$Climate.scenario)