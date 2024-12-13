#application
library(tidyverse)
library(slider)
library(readxl)
library(lubridate)
library(data.table)
#Running this will load all random forest models used to create predictions for the paper
load("Random_forest_models_Glutopeak_farinograph.RData")
load("Random_forest_modelsno_protein_Glutopeak_farinograph.RData")

#Loading GlutoPeak data and calculating features
#This script will load all files in a folder, assumed to be the .xlsx folders output
#by Braebender software
wd <- "C:\\User\\Directory\\"
files <- list.files(wd)

complete_out <- data.frame()

for(i in files){
  #This first part reads in the sheet which contains the raw glutopeak output, converts the time to seconds
  #And adds some sliding window summary statistics for each timepoint
  data <- read_xlsx(paste0(wd,i), sheet = "Measurement")
  data <- data %>% mutate(time = (minute(ymd_hms(data$`Time[mm:ss]`)) * 60 + second(ymd_hms(data$`Time[mm:ss]`))))
  data <- data %>% filter(time < 151)
  
  data <- data %>% mutate(slide_torque_av = slide_mean(`TorqueMean[BU]`, before = 7, after = 7))
  data <- data %>% mutate(slide_forward = slide_mean(`TorqueMean[BU]`, before = 0, after = 7))
  data <- data %>% mutate(dif = `TorqueMean[BU]` -slide_torque_av)
  data <- data %>% mutate(dif_forward = `TorqueMean[BU]` -slide_forward)
  data <- data %>% mutate(slide_torque_max = slide_max(`TorqueMean[BU]`, before = 5, after = 5))
  data <- data %>% mutate(rl = rleid(dif > 0))
  
  data <-data %>% mutate(trap = slide_mean(`Torque[BU]`, after = 1, before = 0))
  
  #This part begins the calcuation of the summary statistics
  summary <-data.frame(0)
  summary$Max_time <- data %>% filter(`TorqueMean[BU]` == max(`TorqueMean[BU]`)) %>% select(time) %>% as.numeric()
  summary$Max_torque <- max(data$`TorqueMean[BU]`)
  
  summary$Max_dif <- data %>% filter(dif == max(dif)) %>% select(dif) %>% as.numeric()
  summary$Max_dif_time <- data %>% filter(dif == max(dif)) %>% select(time) %>% as.numeric()
  summary$First_peak_time <- data %>% filter(slide_torque_max*.95 <= `TorqueMean[BU]`) %>% filter(dif_forward > `TorqueMean[BU]`*.025) %>% filter(time == min(time)) %>% select(time) %>% as.numeric()
  summary$First_peak_torque <- data %>% filter(slide_torque_max*.95 <= `TorqueMean[BU]`) %>% filter(dif_forward > `TorqueMean[BU]`*.025) %>% filter(time == min(time)) %>% select(`TorqueMean[BU]`) %>% as.numeric()
  summary$Max_before60_torque <- data %>% filter(time < 60) %>% summarise(max(`TorqueMean[BU]`)) %>% as.numeric()


  if(summary$First_peak_time + 15 < 151){
  PM <- data %>% filter(time == summary$First_peak_time + 15) %>% select(`TorqueMean[BU]`) %>% as.numeric()
  summary$PMslope <- (summary$First_peak_torque - PM)/15}else{
      PM <- data %>% filter(time == 150) %>% select(`TorqueMean[BU]`) %>% as.numeric()
      summary$PMslope <- (summary$First_peak_torque - PM)/(150 - summary$First_peak_time)}
  
  
  if(summary$First_peak_time - 15 > 1){
    AM <- data %>% filter(time == summary$First_peak_time - 15) %>% select(`TorqueMean[BU]`) %>% as.numeric()
    summary$AMslope <- (summary$First_peak_torque - AM)/15}else{
      AM <- data %>% filter(time == 0) %>% select(`TorqueMean[BU]`) %>% as.numeric()
      summary$AMslope <- (summary$First_peak_torque - AM)/summary$First_peak_time}
  
  
  firstpeak_rel <- data %>% filter(time == summary$First_peak_time) %>% select(rl) 
  maxpeakrl <- data %>% filter(time == summary$Max_time) %>% select(rl) 
  summary$firstpeakarea <- data %>% filter(rl == firstpeak_rel$rl) %>% summarise(sum(trap)) %>% as.numeric()
  summary$maxpeakarea <- data %>% filter(rl == maxpeakrl$rl) %>% summarise(sum(trap)) %>% as.numeric()
  summary$time_near_max <- data %>% filter(`TorqueMean[BU]` > max(`TorqueMean[BU]`) *.80) %>% count() %>% as.numeric()
  
  summary$trough_time <- data %>% filter(time > summary$First_peak_time) %>% filter(`TorqueMean[BU]` == min(`TorqueMean[BU]`)) %>% filter(time==max(time)) %>% select(time) %>% as.numeric()
  summary$trough_torque <- data %>% filter(time > summary$First_peak_time) %>% filter(`TorqueMean[BU]` == min(`TorqueMean[BU]`)) %>% filter(time==max(time)) %>% select(`TorqueMean[BU]`) %>% as.numeric()
  summary$peak_slope <- as.numeric((summary$First_peak_torque-summary$trough_torque)/(summary$First_peak_time - summary$trough_time))
  summary$A12 <- data %>% filter(time >= summary$First_peak_time & time <= summary$trough_time) %>% summarise(sum(trap)) %>% as.numeric()
  summary$A01 <- data %>% filter(time <= summary$First_peak_time) %>% summarise(sum(trap)) %>% as.numeric()
  summary$Torque_at_150 <- data %>% filter(time == max(time)) %>% select(`TorqueMean[BU]`) %>% as.numeric()
  summary$torque_decrease <- summary$First_peak_torque - summary$Torque_at_150
  
  data <- data %>% select(time, dif,`TorqueMean[BU]`) %>% pivot_wider(names_from = time, values_from = c(dif, `TorqueMean[BU]`), names_prefix = "sec")
  data <- cbind(data, summary[-c(1)])
  colnames(data) <-gsub("`","",colnames(data))
  data$sample <- paste0(sub("\\.xlsx$", "", i)) %>% as.character()
  complete_out <- bind_rows(complete_out, data)
  
}

#Output will be a large dataframe, with one row for each input file,
#using the filename (minus the .xlsx suffix) as an ID

#This dataframe contains multiple columns for each timepoint, which is useful for some applications
#but unnecessary for others. These can be removed by running

complete_out <- complete_out %>% select(!starts_with("dif")) %>% select(!starts_with("TorqueMean[BU]"))

#The main models also require grain protein as a predictor 
#If protein is unavailable, predictions can be made using the "Trait_RF_no_protein" model
#Prediction accuracies will be marginally lower, but very close to the main models


#Predict

#the following code can be used to predict values using a given model

complete_out$Absorption_predict <- predict(Absorption_RF_no_protein, complete_out)
