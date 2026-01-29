source('../2.0.functions/functions.R')


dat = read.csv("../2.1.model training/Regression_Output.csv")

if (!file.exists("table")){
  dir.create(file.path("table"))
}


###############################################################################
#cutoff values for:
#max youden, max acc, 0.9 spe, 0.9 sen

F1_thre = c(0.32,0.25,0.54,0.27)
F2_thre = c(0.36,0.34,0.57,0.35)
F3_thre = c(0.57,0.57,0.65,0.36)
F4_thre = c(0.65,0.79,0.85,0.27)

dat[,'F_0_1234'] = ifelse(dat$F>0,1,0)
dat[,'F_01_234'] = ifelse(dat$F>1,1,0)
dat[,'F_012_34'] = ifelse(dat$F>2,1,0)
dat[,'F_0123_4'] = ifelse(dat$F>3,1,0)

tbl1 = write_summary(dat,'F_prob','F_0_1234',F1_thre)
tbl2 = write_summary(dat,'F_prob','F_01_234',F2_thre)
tbl3 = write_summary(dat,'F_prob','F_012_34',F3_thre)
tbl4 = write_summary(dat,'F_prob','F_0123_4',F4_thre)

tbl = cbind(tbl1,tbl2$Val,tbl3$Val,tbl4$Val)
colnames(tbl) = c("","F>=1","F>=2","F>=3","F=4")
write.csv(tbl,"table/F.csv",row.names = F)

###############################################################################
#youden acc spe sen


BI1_thre = c(0.34,0.20,0.38,0.21)
BI2_thre = c(0.35,0.35,0.54,0.34)
BI3_thre = c(0.40,0.56,0.78,0.38)

dat[,'BI_0_123'] = ifelse(dat$BI>0,1,0)
dat[,'BI_01_23'] = ifelse(dat$BI>1,1,0)
dat[,'BI_012_3'] = ifelse(dat$BI>2,1,0)

tbl1 = write_summary(dat,'BI_prob','BI_0_123',BI1_thre)
tbl2 = write_summary(dat,'BI_prob','BI_01_23',BI2_thre)
tbl3 = write_summary(dat,'BI_prob','BI_012_3',BI3_thre)

tbl = cbind(tbl1,tbl2$Val,tbl3$Val)
colnames(tbl) = c("","BI>=1","BI>=2","BI>=3")
write.csv(tbl,"table/BI.csv",row.names = F)


###############################################################################

#youden acc spe sen

S1_thre = c(8.5,8.9,8.8,4.4)
S2_thre = c(8.9,13.8,16.2,11.5)
S3_thre = c(13.8,19.5,18.0,13.8)

dat[,'S_0_123'] = ifelse(dat$S>0,1,0)
dat[,'S_01_23'] = ifelse(dat$S>1,1,0)
dat[,'S_012_3'] = ifelse(dat$S>2,1,0)

tbl1 = write_summary(dat,'PDFF','S_0_123',S1_thre)
tbl2 = write_summary(dat,'PDFF','S_01_23',S2_thre)
tbl3 = write_summary(dat,'PDFF','S_012_3',S3_thre)

tbl = cbind(tbl1,tbl2$Val,tbl3$Val)
colnames(tbl) = c("","S>=1","S>=2","S=3")
write.csv(tbl,"table/S.csv",row.names = F)