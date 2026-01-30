library("MASS")
library("ggplot2")
library("ggsignif")
library("pROC")
library(ggpol)
library(cvms)
library(tibble)
library(tidyr)
library(RColorBrewer)
library(patchwork)
library(jpeg)
library(glmnet)

library(showtext)
font.add('font1','arial.ttf')

fontsize = 8.2
title_size = 6.5
confmatfontsize = 2.5
windowsFonts(font1 = windowsFont("Arial"),font2 = windowsFont("MS Gothic"))

###############################################################################
# model selection & prediction functions

max_accuracy<-function(value,target){
  max_i = 1
  max_accuracy = 0
  
  df = data.frame(value,target)
  df = df[order(df$value),]
  
  value = df$value
  target = df$target
  
  for (i in 1:length(value)){
    
    if (value[i] == min(value) | value[i] == max(value)){
      next
    }
    
    
    pred = rep(1,length(value))
    threshold = value[i]
    pred[value< threshold] = 0
    cur_accuracy = mean(pred == target)
    if(cur_accuracy > max_accuracy){
      max_accuracy = cur_accuracy
      max_i = i
    }
  }
  return(c(max_accuracy,value[max_i]))
}

min_deviance<-function(value,target){
  min_i = 1
  min_deviance = 10
  
  df = data.frame(value,target)
  df = df[order(df$value),]
  
  value = df$value
  target = df$target
  
  for (i in 1:length(value)){
    
    if (value[i] == min(value) | value[i] == max(value)){
      next
    }
    
    
    pred = rep(1,length(value))
    threshold = value[i]
    pred[value< threshold] = 0
    cur_deviance = -2*(sum(log(value[value>=threshold]))+sum(log(1-value[value<threshold])))
    if(cur_deviance < min_deviance){
      min_deviance = cur_deviance
      min_i = i
    }
  }
  return(c(min_deviance,value[min_i]))
}

make_predict<-function(value,threshold){
  pred_label = rep(1,length(value))
  pred_label[value<threshold] = 0
  return(pred_label)
}

draw_metric_equispace<-function(target_name,target_orig,level,prob,thresholds){
  
  target = ifelse(target_orig>=level,1,0)
  
  spe_arr = rep(0,length(thresholds))
  sen_arr = rep(0,length(thresholds))
  acc_arr = rep(0,length(thresholds))
  youden_arr = rep(0,length(thresholds))
  
  for (i in 1:length(thresholds)){
    threshold = thresholds[i]
    pred = ifelse(prob>threshold,1,0)
    spe = sum(pred == 0 & target ==0)/sum(target ==0)
    sen = sum(pred == 1 & target ==1)/sum(target ==1)
    acc = mean(pred == target)
    youden_arr[i] = spe+sen-1
    spe_arr[i] = spe
    sen_arr[i] = sen
    acc_arr[i] = acc
  }
  
  
  df = data.frame(spe_arr =spe_arr,sen_arr = sen_arr,acc_arr = acc_arr, youden_arr = youden_arr, thresholds = thresholds)
  df <- df %>% pivot_longer(cols=c('spe_arr', 'sen_arr', 'acc_arr', 'youden_arr'),
                            names_to='metric',
                            values_to='value')
  ggp = ggplot(df, aes(x=thresholds,y=value,group=metric)) +
    geom_line(aes(color=metric))+
    geom_point(aes(color=metric))+
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20))+
    scale_y_continuous(breaks = scales::pretty_breaks(n = 20))
  ggsave(paste("metric/",target_name,"_metrics.png",sep=""))
  
  outdf = data.frame(thresholds=round(thresholds,2),sen=round(sen_arr,2),spe=round(spe_arr,2),acc=round(acc_arr,2),youden=round(youden_arr,2))
  write.csv(outdf[order(outdf[,"thresholds"]),],paste("metric/",target_name,"_threshold.csv",sep=""),row.names = F)
  
  
}


###############################################################################
# metric plot functions

plot_ROC<-function(value,target,title_text){
  roc_curve=roc(factor(target),value)
  auc_value = auc(roc_curve)
  p=ggroc(roc_curve)+
    ggtitle(paste(title_text,round(auc_value,3),sep=''))+
    xlab('Specificity')+
    ylab('Sensitivity')+
    theme(title = element_text(family = "font1",size=title_size),
          text=element_text(family = "font1",size=fontsize),
          axis.text.x = element_text(family = "font1",size=fontsize),
          axis.text.y = element_text(family = "font1",size=fontsize),
          axis.title.x = element_text(family = "font1",size=fontsize),
          axis.title.y = element_text(family = "font1",size=fontsize),
          aspect.ratio = 1
    )
  return(p)
}

plot_violin<-function(value,target,threshold,tick_1,tick_2,label_x,label_y,ylims){
  violin_data = data.frame(factor(target),value)
  p = ggplot(violin_data, aes(violin_data[,1], y=violin_data[,2] )) + 
    geom_violin(trim=FALSE)+
    geom_jitter(shape=16, position=position_jitter(0.1),size=1)+
    geom_signif(comparisons = list(c("0", "1")),map_signif_level = T)+
    geom_abline(slope=0,intercept = threshold,color = "#5D90BA")+
    scale_x_discrete( labels = c(tick_1,tick_2))+
    ylim(ylims[1],ylims[2])+
    xlab(label_x)+
    ylab(label_y)+
    theme(title = element_text(family = "font1",size=title_size),
          text=element_text(family = "font1",size=fontsize),
          axis.text.x = element_text(family = "font1",size=fontsize),
          axis.text.y = element_text(family = "font1",size=fontsize),
          axis.title.x = element_text(family = "font1",size=fontsize),
          axis.title.y = element_text(family = "font1",size=fontsize),
          aspect.ratio = 1
          )
  return(p)
}

plot_confmat<-function(target,pred,tick_1,tick_2,label_target,label_pred,accuracy){
  
  
  d_binomial=tibble("target"=target,"pred"=pred)
  basic_table=table(d_binomial)
  cfm=as_tibble(basic_table)
  p=plot_confusion_matrix(cfm,target_col = "target",prediction_col = "pred",counts_col = "n",
                          add_row_percentages = FALSE,
                          add_col_percentages = FALSE,
                          font_counts = font(family="font1",size=confmatfontsize,fontface='plain'), 
                          font_normalized = font(family="font1",size=confmatfontsize,fontface='plain'),
                          font_row_percentages = font(family="font1",size = confmatfontsize,fontface='plain'),
                          font_col_percentages = font(family="font1",size = confmatfontsize,fontface='plain'),
                          rotate_y_text = FALSE)+
    scale_x_discrete( labels = c(tick_1,tick_2),)+
    scale_y_discrete(labels = c(tick_1,tick_2))+
    xlab(label_target)+
    ylab(label_pred)+
    ggtitle(paste("Accuracy: ",round(accuracy,2),sep=''))+
    theme(title = element_text(family = "font1",size=title_size),
          text=element_text(family = "font1",size=fontsize),
          axis.text.x = element_text(family = "font1",size=fontsize),
          axis.text.y = element_text(family = "font1",size=fontsize),
          axis.title.x = element_text(family = "font1",size=fontsize),
          axis.title.y = element_text(family = "font1",size=fontsize),
          aspect.ratio = 1
    )
  return(p)
}

plot_all_onerow<-function(target_name,target,score_name,score,relation,level,threshold,ylims,confmat_prediction_name){
  
  target = ifelse(target>=level,1,0)
  pred = ifelse(score>threshold,1,0)
  accuracy = mean(target==pred)
  
  p_ROC = plot_ROC(score,target,title_text = "AUC: ")
  p_violin = plot_violin(score,target,threshold,paste("<",level,sep=""),paste(relation,level,sep=""),paste(target_name,"by Biopsy"),paste(score_name,"by SSMR"),ylims)
  p_confmat = plot_confmat(target,pred,paste("<",level,sep=""),paste(relation,level,sep=""),paste(target_name,"by Biopsy"),paste(confmat_prediction_name,"by SSMR"),accuracy)
  
  return(p = p_violin|p_ROC|p_confmat)
}

###############################################################################
# report output functions

bootCI<-function(pop_dat,pop_prob,target,threshold){
  pred = ifelse(pop_dat[,pop_prob]>threshold,1,0)
  TP = sum(pred == 1 & pop_dat[,target] ==1)
  TN = sum(pred == 0 & pop_dat[,target] ==0)
  FP = sum(pred == 1 & pop_dat[,target] ==0)
  FN = sum(pred == 0 & pop_dat[,target] ==1)
  spe = TN/(TN+FP)
  sen = TP/(TP+FN)
  acc = (TN+TP)/(TN+TP+FN+FP)
  
  boot_acc = rep(0,2000)
  boot_sen = rep(0,2000)
  boot_spe = rep(0,2000)
  
  for (q in 1:2000) {
    bootindex = sample(1:nrow(pop_dat),nrow(pop_dat),replace = T)
    bootsample = pop_dat[bootindex,]
    pred = ifelse(bootsample[,pop_prob]>threshold,1,0)
    boot_acc[q] = mean(pred ==bootsample[,target])
    boot_sen[q] = sum(pred == 1 & bootsample[,target] ==1)/sum(bootsample[,target] ==1)
    boot_spe[q] = sum(pred == 0 & bootsample[,target] ==0)/sum(bootsample[,target] ==0)
  }
  
  sorted_sen = sort(boot_sen)
  sorted_spe = sort(boot_spe)
  sorted_acc = sort(boot_acc)
  
  return(list(TP,TN,FP,FN,sen,spe,acc,sorted_sen,sorted_spe,sorted_acc))
  
}

write_summary_head<-function(pop_dat,pop_prob,target){
  summary_tbl = data.frame()
  boot_auc = rep(0,2000)
  
  for (q in 1:2000) {
    bootindex = sample(1:nrow(dat),nrow(dat),replace = T)
    bootsample = dat[bootindex,]
    
    if(length(levels(factor(bootsample[,target])))>1){
      roc_curve=roc(factor(bootsample[,target]),bootsample[,pop_prob])
      boot_auc[q] = auc(roc_curve)
    }else{
      boot_auc[q] = NaN
    }
    
    
  }
  
  sorted_auc = sort(boot_auc)
  auc_l = sorted_auc[ceiling(0.025*length(sorted_auc))]
  auc_r = sorted_auc[floor(0.975*length(sorted_auc))]
  summary_tbl = rbind(summary_tbl,c("AUCROC(95% CI)",paste(round(mean(sorted_auc),2), ' (', round(auc_l,2),'-',round(auc_r,2),')',sep="")))
  summary_tbl = rbind(summary_tbl,c("Prevalence",paste(round(mean(pop_dat[,target]),2), ' (n = ',sum(pop_dat[,target]),')',sep="")))
  colnames(summary_tbl) = c("Name","Val")
  return(summary_tbl)
}

write_summary_body<-function(pop_dat,pop_prob,target,threshold){
  summary_tbl = data.frame()
  bootCI_vals = bootCI(pop_dat,pop_prob,target,threshold)
  
  TP = bootCI_vals[[1]]
  TN = bootCI_vals[[2]]
  FP = bootCI_vals[[3]]
  FN = bootCI_vals[[4]]
  sen = bootCI_vals[[5]]
  spe = bootCI_vals[[6]]
  acc = bootCI_vals[[7]]
  sorted_sen = bootCI_vals[[8]]
  sorted_spe = bootCI_vals[[9]]
  sorted_acc = bootCI_vals[[10]]
  
  sen_l = sorted_sen[ceiling(0.025*length(sorted_sen))]
  sen_r = sorted_sen[floor(0.975*length(sorted_sen))]
  spe_l = sorted_spe[ceiling(0.025*length(sorted_spe))]
  spe_r = sorted_spe[floor(0.975*length(sorted_spe))]  
  acc_l = sorted_acc[ceiling(0.025*length(sorted_acc))]
  acc_r = sorted_acc[floor(0.975*length(sorted_acc))]  
  
  summary_tbl = rbind(summary_tbl,c("Cutoff",format(round(threshold,2),nsmall=2)))
  summary_tbl = rbind(summary_tbl,c("Se (95% CI)",paste(format(round(sen,2),nsmall=2), ' (',format(round(sen_l,2),nsmall=2),'-',format(round(sen_r,2),nsmall=2),')',sep="")))
  summary_tbl = rbind(summary_tbl,c("TP/(TP+FN)",paste(' (',TP,'/',TP+FN,')',sep="")))
  summary_tbl = rbind(summary_tbl,c("Sp (95% CI)",paste(format(round(spe,2),nsmall=2), ' (',format(round(spe_l,2),nsmall=2),'-',format(round(spe_r,2),nsmall=2),')',sep="")))
  summary_tbl = rbind(summary_tbl,c("TN/(TN+FP)",paste(' (',TN,'/',TN+FP,')',sep="")))
  summary_tbl = rbind(summary_tbl,c("Acc (95% CI)",paste(format(round(acc,2),nsmall=2), ' (',format(round(acc_l,2),nsmall=2),'-',format(round(acc_r,2),nsmall=2),')',sep="")))
  summary_tbl = rbind(summary_tbl,c("TN+TP/(TN+FP+TP+FN)",paste(' (',TN+TP,'/',TN+FP+TP+FN,')',sep="")))
  
  
  colnames(summary_tbl) = c("Name","Val")
  
  return(summary_tbl)
}

write_summary<-function(pop_dat,pop_prob,target,threshold){
  tbl_head = write_summary_head(dat,pop_prob,target)
  tbl_body_you = write_summary_body(dat,pop_prob,target,threshold[1])
  tbl_body_acc = write_summary_body(dat,pop_prob,target,threshold[2])
  tbl_body_spe = write_summary_body(dat,pop_prob,target,threshold[3])
  tbl_body_sen = write_summary_body(dat,pop_prob,target,threshold[4])
  
  tbl_list = rbind(tbl_head,data.frame(Name ="Max Youden",Val =""),tbl_body_you,data.frame(Name = "Se = 0.90",Val = ""),tbl_body_sen,data.frame(Name = "Sp = 0.90",Val= ""),tbl_body_spe,data.frame(Name ="Max Accuracy",Val =""),tbl_body_acc)
  return(tbl_list)
}

