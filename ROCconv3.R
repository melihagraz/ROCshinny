library(cutpointr )
library(pROC)


ROC_fin <- function(data, x, y, method = c("youden_topleft", "cutoff", "maximized"),
                    index = NULL, cutoff = NULL,
                    constrain_metric = c("specificity", "sensitivity"), min_cons = 0.5) {
   library(dplyr)                    
                       # if the method is best you can specify the index = c("youden","closest.topleft")
                       # cutoff = NULL #you can specify the cutoff
                       # data = aSAH
                       # x="s100b"
                       # y="outcome"
                       # index = "youden"
 
  
  # Print data, x, and y
  print(head(data))
  print(x)
  print(y)
  
  x1 <- data[[x]]
  y1 <- data[[y]]
  
  # Print x1 and y1
  print(head(x1))
  print(head(y1))
  
  # Add more print statements
 
 
                       roc.s100b <- roc(y1, x1, ci=TRUE,ci.alpha = ci_value, stratified = FALSE, plot = FALSE) 
                  
                   
                       
                       auc.s100b <- round(roc.s100b$auc[1],4)
                       ci_L<-round(roc.s100b$ci[1],4)
                       ci_U<-round(roc.s100b$ci[3],4)
                       
                       if (method == "youden_topleft") {
                         main_res <- round(coords(roc.s100b, "best", ret=c("threshold",  "sensitivity", "specificity", 
                                                                           "ppv", "npv", "accuracy", "auc"),  
                                                  best.method = index, transpose = FALSE), 4)
                         main_res = cbind(main_res, auc = auc.s100b, ci_low = ci_L, ci_up = ci_U)
                         
                         main_res = list(best_res = main_res, pl = roc.s100b)
                         
                       } else if (method == "cutoff") {
                         main_res <- round(coords(roc.s100b, cutoff, ret=c("threshold",  "sensitivity", "specificity", 
                                                                           "ppv", "npv", "accuracy"),  
                                                  best.method = index, transpose = FALSE), 4)
                         main_res = cbind(main_res, auc = auc.s100b, ci_low = ci_L, ci_up = ci_U)
                         main_res = list(specific_cutoff = main_res, pl = roc.s100b)
                       }else{
                         
                        
                         if (constrain_metric == "specificity") {
                         main_res <- data.frame(cut_off = numeric(),
                                                Sen = numeric(),
                                                Spec = numeric(),
                                                PPV = numeric(),
                                                NPV = numeric(),
                                                Acc = numeric(),
                                                Minimum_Specificity = numeric())
                         
                         
                         #for (ix in 1:length(min_cons)) {
                           # constrain specif
                           df_ns<-data.frame(Dependent= data[,y], X1 = data[,x])
                          
                           df_ns <- df_ns[complete.cases(df_ns), ]
                           
        
                           cp <- cutpointr( x = df_ns[,2], class = df_ns[,1],
                                            method = maximize_metric,
                                            metric = sens_constrain,
                                            constrain_metric = specificity,
                                            min_constrain = min_cons)
                           test<-summary(cp)
                           test
                           cutf<-test$confusion_matrix[[1]][[1]]#cutoff
                           tp<-test$confusion_matrix[[1]][[2]]
                           fn<-test$confusion_matrix[[1]][[3]]
                           fp<-test$confusion_matrix[[1]][[4]]
                           tn<-test$confusion_matrix[[1]][[5]]
                           sens<-(tp)/(tp+fn) 
                           spec<-(tn)/(tn+fp) 
                           PPV<-(tp)/(tp+fp)
                           NPV<-(tn)/(tn+fn)
                           Acc = (tp+tn)/(tp+fp+tn+fn)
                           
                           main_res[1,1] <- cutf
                           main_res[1,2] <- sens
                           main_res[1,3] <- spec
                           main_res[1,4] <- PPV
                           main_res[1,5] <- NPV
                           main_res[1,6] <- Acc
                           main_res[1,7] <- min_cons
                           
                           main_res_max_sens<-main_res
                         }else{
                         
                         main_res <- data.frame(cut_off = numeric(),
                                                Sen = numeric(),
                                                Spec = numeric(),
                                                PPV = numeric(),
                                                NPV = numeric(),
                                                Acc = numeric(),
                                                Minimum_Sensitivity = numeric())
                         
                        #  for (ix in 1:length(min_cons)) {
                           # constrain specif
                           df_ns<-data.frame(Dependent= data[,y], X1 = data[,x])
                           df_ns <- df_ns[complete.cases(df_ns), ]
                           
                           cp <- cutpointr(x = df_ns[,2], class = df_ns[,1],
                                           method = maximize_metric,
                                           metric = spec_constrain,
                                           constrain_metric = sensitivity,
                                           min_constrain = min_cons)
                           test<-summary(cp)
                           cutf<-test$confusion_matrix[[1]][[1]]#cutoff
                           tp<-test$confusion_matrix[[1]][[2]]
                           fn<-test$confusion_matrix[[1]][[3]]
                           fp<-test$confusion_matrix[[1]][[4]]
                           tn<-test$confusion_matrix[[1]][[5]]
                           sens<-(tp)/(tp+fn) 
                           spec<-(tn)/(tn+fp) 
                           PPV<-(tp)/(tp+fp)
                           NPV<-(tn)/(tn+fn)
                           Acc = (tp+tn)/(tp+fp+tn+fn)
                           
                           main_res[1,1] <- cutf
                           main_res[1,2] <- sens
                           main_res[1,3] <- spec
                           main_res[1,4] <- PPV
                           main_res[1,5] <- NPV
                           main_res[1,6] <- Acc
                           main_res[1,7] <- min_cons
                           round(main_res,4)
                           main_res_max_spec<-main_res
                         }
                         main_res <- if (constrain_metric == "specificity") main_res_max_sens else main_res_max_spec
                         main_res = cbind(main_res, auc = auc.s100b, ci_low = ci_L, ci_up = ci_U)
                         main_res = list(specific_cutoff = main_res, pl = roc.s100b)                   
                       }
                       
                       return(main_res)
                       
}
                     