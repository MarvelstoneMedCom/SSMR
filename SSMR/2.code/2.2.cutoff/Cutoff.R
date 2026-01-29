
source('../2.0.functions/functions.R')



dat = read.csv("../2.1.model training/Regression_Output.csv")


if (!file.exists("metric")){
  dir.create(file.path("metric"))
}


thresholds = seq(0,30,by = 0.1)
draw_metric_equispace("S1",dat$S,1,dat$PDFF,thresholds)
draw_metric_equispace("S2",dat$S,2,dat$PDFF,thresholds)
draw_metric_equispace("S3",dat$S,3,dat$PDFF,thresholds)

thresholds = seq(0,1,by = 0.01)
draw_metric_equispace("F1",dat$F,1,dat$F_prob,thresholds)
draw_metric_equispace("F2",dat$F,2,dat$F_prob,thresholds)
draw_metric_equispace("F3",dat$F,3,dat$F_prob,thresholds)
draw_metric_equispace("F4",dat$F,4,dat$F_prob,thresholds)

thresholds = seq(0,1,by = 0.01)
draw_metric_equispace("BI1",dat$BI,1,dat$BI_prob,thresholds)
draw_metric_equispace("BI2",dat$BI,2,dat$BI_prob,thresholds)
draw_metric_equispace("BI3",dat$BI,3,dat$BI_prob,thresholds)
