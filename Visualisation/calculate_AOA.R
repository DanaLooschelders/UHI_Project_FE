#####calculation of AOA ####
library(CAST)
#for model 1
#calculate AOA for day
model_1_aoa<-aoa(pred_stack_1, model_1)
#calculate percentage of areas outside AOA
ncell(model_1_aoa$AOA[model_1_aoa$AOA==1])/ncell(model_1_aoa$AOA)*100

#calculate AOA for night
model_1_aoa<-aoa(pred_stack_1, model_1)
#calculate percentage of areas outside AOA
ncell(model_1_aoa$AOA[model_1_aoa$AOA==1])/ncell(model_1_aoa$AOA)*100

#for model 2
#calculate AOA 
model_2_aoa<-aoa(pred_stack_2, model_2)
#calculate percentage of areas outside AOA
ncell(model_2_aoa$AOA[model_2_aoa$AOA==1])/ncell(model_2_aoa$AOA)*100

####plot areas inside AOA ####
#for model 1 day
model_1_day_predict_aoa<-model_1_day_predict[model_1_aoa$AOA == 0] <- NA
test_predict2<-test_predict[test_aoa$AOA == 0] <- NA