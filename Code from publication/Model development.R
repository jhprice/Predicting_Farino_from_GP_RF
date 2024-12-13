library(tidyverse)
library(caret)
library(randomForest)
library(readxl)
library(lubridate)
library(slider)
library(data.table)

#Loading GlutoPeak data and calculating features
#This script will load all files in a folder, assumed to be the .xlsx folders output
#by Braebender software
wd <- "C:\\User\\Directory\\"
files <- list.files(wd)

#input Farinograph data, one column per trait one row per sample

Farinodata <- read.csv("Farinograph_output.csv")
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

#This dataframe contains multiple columns for each timepoint, which is useful for some applications
#but unnecessary for others. These can be removed by running

complete_out <- complete_out %>% select(!starts_with("dif")) %>% select(!starts_with("TorqueMean[BU]"))


#In this study, multiple GlutoPeak runs, some originally longer than 150 seconds, were included in
# the raw data. One GlutoPeak run was randomly selected for each sample
#The filename prefixes included in the raw data for this experiment can be removed, and 
#inputs downsampled to one per sample ID using the following lines
complete_out$sample <- gsub("_long", "", complete_out$sample)
complete_out$sample <- gsub("_.*", "", complete_out$sample)

complete_out  <- complete_out %>% group_by(sample) %>% sample_n(1)



RF_train_data  <- inner_join(Farinodata, complete_out)


#The following lines add categorical trait vlaues for MTI and TS
RF_train_data  <- RF_train_data  %>% mutate(Tol_factor = case_when(Tolerance_stability < 7.5 ~ 0,
                                                                     Tolerance_stability >= 7.5 & Tolerance_stability < 14 ~1,
                                                                     Tolerance_stability >= 14 ~ 2))



RF_train_data  <- RF_train_data  %>% mutate(MTI_factor = case_when(MTI_BU < 20 ~ 2,
                                                                     MTI_BU >= 20 & MTI_BU < 40 ~1,
                                                                     MTI_BU >= 40 ~ 0))

#Model creation and evalution using the R package 'caret'
#The following first creates an object which contains the training parameters for model testing
ctrl_Farino <- trainControl(method = "cv", number = 5) 

#Next, we can create a single model for a trait, in this case Absorption


Ab_RF_model <- train(
  Absorption ~ .,  # Response variable can be changed
  data = RF_train_data[-c(1,3,4,5,25,26)], #for training data, it is simplest to include the entire
                                            #dataset, minus the "sample" column, and all Farinograph
                                            #parameters other than the one being predicted
  method = "rf",        
  trControl = ctrl_Farino ,
  tuneGrid = data.frame(mtry = c(2,4,6,8,10,12,14))
)


#The created model is sufficient for futher prediciton

#To build a model with the same variables multiple times to better evalaute accucary
#The following framework can be used 
#number of tests
num=30
Ab_result <- as.data.frame(matrix(nrow=num,ncol = 2))
Ab_import <- data.frame()
for(i in 1:num){
  Ab_RF_model_eval <- train(
    Absorption ~ .,  
    data = RF_train_data[-c(1,3,4,5,25,26)],
    method = "rf",        # Random Forest method
    trControl = ctrl_Farino ,
    tuneGrid = data.frame(mtry = c(2,4,6,8,10,12,14))
  )
  result_frame <- as.data.frame(Ab_RF_model_eval[["results"]])
  Ab_result[i,1] <- min(result_frame$RMSE)
  Ab_result[i,2] <-  result_frame %>% filter(RMSE == min(RMSE)) %>% select(Rsquared) %>% as.numeric()
  import <- as.data.frame(Ab_RF_model_eval[["finalModel"]][["importance"]])
  
  import$Features <- row.names(import)
  Ab_import <- rbind(Ab_import,import)}

#summarize results
Ab_import_summary <- Ab_import %>% group_by(Features) %>% summarise(median(IncNodePurity))

#median RMSE
median(Ab_result$V1)
#Median R2
median(Ab_result$V2)
#importance
Ab_import_summary %>% View
