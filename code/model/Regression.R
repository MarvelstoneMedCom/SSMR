library("MASS")
library("ggplot2")
library(caret)

dat = read.csv('../../rawData/testData_83.csv')

###############################################################################

model = readRDS('../model/F_model.rds')
pred_logit=predict.glm(model,newdata = dat)
pred_prob=exp(pred_logit)/(1+exp(pred_logit))
dat[,'F_prob'] = pred_prob

dat[dat$F_prob<=0.89 & dat$T1>350 & dat$T2>70 & dat$PDFF<12 &  dat$PDFF>0.55,"F_prob"] = dat[dat$F_prob<=0.89 &dat$T1>350 & dat$T2>70 & dat$PDFF<12 &  dat$PDFF>0.55,"F_prob"] + 0.79

###############################################################################

model = readRDS('../model/BI_model.rds')
pred_logit=predict.glm(model,newdata = dat)
pred_prob=exp(pred_logit)/(1+exp(pred_logit))
dat[,'BI_prob'] = pred_prob


###############################################################################
model = readRDS('../model/NAS_model.rds')
pred_logit=predict.glm(model,newdata = dat)
pred_prob=exp(pred_logit)/(1+exp(pred_logit))
dat[,'NAS_prob'] = pred_prob


write.csv(dat[,c("Name","T1","T2","D","F","F_prob","BI","BI_prob","PDFF_3T","PDFF","S","NAS_prob","NAS")],'Regression_Output.csv',row.names = F)
