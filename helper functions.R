library(ROCR)
# https://rocr.bioinf.mpi-sb.mpg.de/

fn_ROC_AUC <- function(l_votes, l_responses) {
  pred <- prediction(l_votes, l_responses)
  perf <- performance(pred, measure = "tpr", x.measure = "fpr")
  auc <- performance(pred, measure = "auc")
  # https://davidrroberts.wordpress.com/2015/09/22/quick-auc-function-in-r-with-rocr-package/
  xline <- c(0,1)
  yline <- xline
  plot(perf, colorize = T)
  text(1,0.15,labels=paste("AUC = ",round(auc@y.values[[1]],digits=3),sep=""),adj=1)
  lines(xline, yline, col = 'grey')
}
