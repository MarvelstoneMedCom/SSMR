source('../functions/functions.R')

showtext.auto(enable = TRUE)


dat = read.csv("../model/Regression_Output.csv")

if (!file.exists("figures")){
  dir.create(file.path("figures"))
}


###############################################################################
# prediction using cutoff maximizing youden index 

BI_1 = 0.2
BI_2 = 0.35
BI_3 = 0.56


dat$BI_pred = ifelse(dat$BI_prob>BI_1,1,0)
dat$BI_pred = ifelse(dat$BI_prob>BI_2,2,dat$BI_pred)
dat$BI_pred = ifelse(dat$BI_prob>BI_3,3,dat$BI_pred)

S_1 = 8.9
S_2 = 13.8
S_3 = 19.5

dat$S_pred = ifelse(dat$PDFF>S_1,1,0)
dat$S_pred = ifelse(dat$PDFF>S_2,2,dat$S_pred)
dat$S_pred = ifelse(dat$PDFF>S_3,3,dat$S_pred)



F_1 = 0.25
F_2 = 0.36
F_3 = 0.57
F_4 = 0.79
dat$F_prob[dat$F_prob>1] = 1

dat$F_pred = ifelse(dat$F_prob>F_1,1,0)
dat$F_pred = ifelse(dat$F_prob>F_2,2,dat$F_pred)
dat$F_pred = ifelse(dat$F_prob>F_3,3,dat$F_pred)
dat$F_pred = ifelse(dat$F_prob>F_4,4,dat$F_pred)


###############################################################################

S1 = plot_all_onerow("Steatosis Stage",dat$S,"PDFF",dat$PDFF,"≥",1,S_1,c(-5,50),"Prediction")
S2 = plot_all_onerow("Steatosis Stage",dat$S,"PDFF",dat$PDFF,"≥",2,S_2,c(-5,50),"Prediction")
S3 = plot_all_onerow("Steatosis Stage",dat$S,"PDFF",dat$PDFF,"=",3,S_3,c(-5,50),"Prediction")


S1/S2/S3
ggsave("figures/S.eps",width = 180,height = 180,units='mm')


BI1 = plot_all_onerow("Inflammation Score",dat$BI,"Inflammation Score",dat$BI_prob,"≥",1,BI_1,c(-0.2,1.5),"Prediction")
BI2 = plot_all_onerow("Inflammation Score",dat$BI,"Inflammation Score",dat$BI_prob,"≥",2,BI_2,c(-0.2,1.5),"Prediction")
BI3 = plot_all_onerow("Inflammation Score",dat$BI,"Inflammation Score",dat$BI_prob,"=",3,BI_3,c(-0.2,1.5),"Prediction")

BI1/BI2/BI3
ggsave("figures/BI.eps",width = 180,height = 180,units='mm')

F1 = plot_all_onerow("Fibrosis Score",dat$F,"Fibrosis Score",dat$F_prob,"≥",1,F_1,c(-0.2,1.5),"Prediction")
F2 = plot_all_onerow("Fibrosis Score",dat$F,"Fibrosis Score",dat$F_prob,"≥",2,F_2,c(-0.2,1.5),"Prediction")
F3 = plot_all_onerow("Fibrosis Score",dat$F,"Fibrosis Score",dat$F_prob,"≥",3,F_3,c(-0.2,1.5),"Prediction")
F4 = plot_all_onerow("Fibrosis Score",dat$F,"Fibrosis Score",dat$F_prob,"=",4,F_4,c(-0.2,1.5),"Prediction")

F1/F2/F3/F4
ggsave("figures/F.eps",width = 180,height = 240,units='mm')
