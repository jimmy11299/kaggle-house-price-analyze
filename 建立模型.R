#透過h2o框架建立機器學習模型
library(h2o)
h2o.init(nthreads = -1, max_mem_size = "4G")  

############################################
#h2o-隨機森林(randomForest)
set.seed(123)
rf1 <- sample(2, nrow(test_3), replace = TRUE, prob = c(0.8, 0.2))
g<-subset(test_3,select = -V2)

rf_f2 <- h2o.randomForest(  x = colnames(g),
                            y = colnames(test_3[,75]),
                            training_frame = as.h2o(test_3[rf1 == 1,]),
                            validation_frame = as.h2o(test_3[rf1 == 2,]),
                            model_id = "rf_f2",
                            ntrees = 200,#設置樹的數量為200
                            seed = 123,
                            nfolds = 5) #交叉驗證的次數
#模型效能評估
h2o.performance(rf_f2,as.h2o(test_3[rf1 == 2,]))

#變數重要度排序
h2o.varimp_plot(rf_f2, num_of_features = 10)
as.data.frame(h2o.varimp(rf_f2))

#資料預測
pd2<-h2o.predict(rf_f2, newdata = as.h2o(test_3))
df_pd2<-as.data.frame(pd2)

#建立h2o.dl模型估計表
c1<-c("MSE",  "8670076*", "20984387")
c2<-c("RMSE",  "2944.499*", "4580.872")
c3<-c("MAE",  "1865.096*", "2266.53")
c4<-c("RMSLE",  "0.01613993*", "0.02207941")
c5<-c("Mean Residual Deviance",  "8670076*", "20984387")
c6<-rbind(c1,c2,c3,c4,c5)
colnames(c6)<-c("names","測試資料集","交叉驗證")
c6

#預測圖
ggplot(df_pd2,aes(x = predict))+  
  geom_histogram(bins = 30, aes(y = ..density..), alpha = 0.5)+ 
  geom_density()+ 
  labs(x="房價", y="數量")

############################################
#h2o-Gradient Boosting Machine(gbdt/gbm)
#gbm_f1 <- h2o.gbm(x = colnames(g), 
#y = colnames(test_3[,75]), 
#training_frame =as.h2o(test_3[rf1 == 1,]),
#validation_frame = as.h2o(test_3[rf1 == 2,]),
#nfolds = 5,
#ntrees = 5000,
#stopping_rounds = 10,
#stopping_tolerance = 0,
#model_id = "gbm_f1",
#seed = 123)

## create hyperparameter grid
#hyper_grid <- list(
#max_depth = c(1, 3, 5),
#min_rows = c(1, 5, 10),
#learn_rate = c(0.01, 0.05, 0.1),
#learn_rate_annealing = c(.99, 1),
#sample_rate = c(.5, .75, 1),
#col_sample_rate = c(.8, .9, 1))

## perform grid search(執行全網格搜索)<至少2小時> 
#grid <- h2o.grid(
#algorithm = "gbm",
#grid_id = "gbm_grid1",
#x = colnames(g), 
#y = colnames(test_3[,75]),
#training_frame =as.h2o(test_3[rf1 == 1,]),
#validation_frame = as.h2o(test_3[rf1 == 2,]),
#hyper_params = hyper_grid,
#ntrees = 5000,
#stopping_rounds = 10,
#stopping_tolerance = 0,
#seed = 123)
# 收集結果並對效能進行排序
#grid_perf <- h2o.getGrid(
#grid_id = "gbm_grid1", 
#sort_by = "mse", 
#decreasing = FALSE)
#grid_perf

#查看性能最佳模型的更多詳細信息。
#best_model_id <- grid_perf@model_ids[[1]]
#best_model <- h2o.getModel(best_model_id)

#最佳模型的效能
#h2o.performance(model = best_model, valid = TRUE)

#建立最後模型
h2o.final <- h2o.gbm(
  x = colnames(g), 
  y = colnames(test_3[,75]),
  training_frame = as.h2o(test_3[rf1 == 1,]),
  validation_frame = as.h2o(test_3[rf1 == 2,]),
  nfolds = 5,
  ntrees = 10000,
  learn_rate = 0.1,
  learn_rate_annealing = 1,
  max_depth = 1,
  min_rows = 1,
  sample_rate = 1,
  col_sample_rate = 1,
  stopping_rounds = 10,
  stopping_tolerance = 0,
  seed = 123)

#建立h2o.dl模型估計表
b1<-c("MSE",  "90425.8*", "596247.2")
b2<-c("RMSE",  "300.7088*", "772.1705")
b3<-c("MAE",  "200.4143*", "312.8046")
b4<-c("RMSLE",  "0.001619159*", "0.004197335")
b5<-c("Mean Residual Deviance",  "90425.8", "596247.2*")
b6<-rbind(b1,b2,b3,b4,b5)
colnames(b6)<-c("names","測試資料集","交叉驗證")
b6

#最後模型的效能
h2o.performance(model = h2o.final)

# model stopped after xx trees
h2o.final@parameters$ntrees

# cross validated RMSE
h2o.rmse(h2o.final, xval = TRUE)

#變量重要性
h2o.varimp_plot(h2o.final, num_of_features = 10)
as.data.frame(h2o.varimp(h2o.final))

#使用測試資料計算模型效能
h2o.performance(model = h2o.final, newdata = as.h2o(test_3[rf1 == 2,]))

#預測
gbm_predict<-predict(h2o.final,as.h2o(test_3))
df_gbm_predict<-as.data.frame(gbm_predict)

#房價預測圖
ggplot(df_gbm_predict,aes(x = predict))+  
  geom_histogram(bins = 30, aes(y = ..density..), alpha = 0.5)+ 
  geom_density()+ 
  labs(x="房價", y="數量")

############################################
#h2o-深度學習(DL)
dl <- h2o.deeplearning(x = colnames(g), 
                       y = colnames(test_3[,75]), 
                       training_frame =as.h2o(test_3[rf1 == 1,]),
                       model_id = "dl",
                       validation_frame = as.h2o(test_3[rf1 == 2,]),  #in DL, early stopping is on by default
                       epochs = 20,
                       hidden=c(32,32,32),
                       input_dropout_ratio=0,
                       rate=0.01,
                       rate_annealing=1e-8,
                       score_interval = 1,           #used for early stopping
                       stopping_rounds = 3,          #used for early stopping
                       stopping_metric = "rmse",      #used for early stopping
                       stopping_tolerance = 0.0005,  #used for early stopping
                       seed = 123,
                       nfolds = 5)
dl

#模型效能
dl_p <- h2o.performance(model = dl,
                        newdata = as.h2o(test_3[rf1 == 2,]))
dl_p

#預測
pdl<-predict(dl,as.h2o(test_3))
df_pdl<-as.data.frame(pdl)
df_pdl

#建立h2o.dl模型估計表
d1<-c("MSE","1311013","2505431","6419506" )
d2<-c("RMSE" ,"1144.995","1582.855","2533.674" )
d3<-c("MAE","847.3397","1055.737","1420.995" )
d4<-c("RMSLE","0.006221825","0.008575702","0.01263158" )
d5<-c("Mean Residual Deviance","1311013","2505431","6419506" )
d6<-rbind(d1,d2,d3,d4,d5)
colnames(d6)<-c("names","訓練資料","測試資料","交叉驗證")

