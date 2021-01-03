source('000.global.R', encoding = 'UTF-8')

###############################################################################################################################
# SUMMARY


# 예측결과 SUMMARY 뽑기
# data 불러오기
product <- fread(paste0(data_path,'product.csv'), encoding = 'UTF-8')
search1 <- fread(paste0(data_path,'search1.csv'), encoding = 'UTF-8')
session <- fread(paste0(data_path,'session.csv'), encoding = 'UTF-8')
master <- fread(paste0(data_path,'master.csv'), encoding = 'UTF-8')

session$SESS_DT <- as.character(session$SESS_DT)
# act 데이터 불러오기
set_act <- product %>% 
  inner_join(master, by = c('PD_C')) %>%
  inner_join(session, by = c('CLNT_ID', 'SESS_ID')) %>% 
  inner_join(search1, by = c('CLNT_ID', 'SESS_ID')) %>% 
  inner_join(set_date, by = c('SESS_DT' = 'dt')) %>% 
  group_by(CLAC3_NM, WEEK) %>% 
  summarise(sum_ord = sum(as.integer(PD_BUY_CT))) %>% 
  setDT()

# 예측값 불러오기
set_predict_list <-list.files(predict_result_path)
predict_week <- grep('base', set_predict_list, value= T)
predict_week <- as.numeric(substr(predict_week, 15, 20))

compare_list <- list()
for(i in predict_week) {
  set_predict_file_name <- grep(i, set_predict_list, value=T)

  temp_act <- set_act[WEEK == i]
  
  # 예측 대상 CLAC3_NM 가져오기
  temp <- c()
  
  for(j in set_predict_file_name){
    set_clac3 <- fread(paste0(predict_result_path, j), encoding = 'UTF-8')
    temp <- rbind(temp,set_clac3[,1])
  }
  set_clac3 <- unique(temp)
  set_clac3$WEEK <- as.character(i)
  temp_act <- set_clac3 %>%
    left_join(temp_act, by = c('CLAC3_NM','WEEK')) %>%
    setDT()
  temp_act[is.na(sum_ord), 'sum_ord'] <- 0
  
  set_summary_list <- list()
  for(jj in set_predict_file_name){
    set_predict <- fread(paste0(predict_result_path, jj), encoding = 'UTF-8')
    set_predict[,2] <- round(set_predict[,2])
    
    set_summary <- set_predict %>%
      right_join(temp_act, by = 'CLAC3_NM') %>%
      setDT()
    
    set_summary <- merge(set_clac3, set_summary, by = c('CLAC3_NM','WEEK'), all.x=T)
    set_summary[is.na(WEEK), 'WEEK'] <- as.character(i)
    set_summary[is.na(sum_ord),'sum_ord'] <- 0
    
    pred <- unlist(set_summary[,3])
    act <- unlist(set_summary[,4])
    
    set_summary$MMR <- ifelse(is.na(pred),0,ifelse(pred == 0 & act == 0, 1, pmin(pred,act)/pmax(pred,act)))
    set_summary$abs_error <- ifelse(is.na(pred), NA, abs(pred-act))
    colnames(set_summary)[c(5,6)] <- c(paste0(colnames(set_summary)[3],'_MMR'),
                                       paste0(colnames(set_summary)[3],'_abs_error'))
    set_summary_list[[jj]] <- set_summary
  }
  
  for(jjj in 1:length(set_summary_list)){
    if(jjj == 1){
      temp_all <- merge(set_clac3, set_summary_list[[jjj]], by=c('CLAC3_NM', 'WEEK'))
    } else {
      temp_all <- merge(temp_all, set_summary_list[[jjj]], by=c('CLAC3_NM','WEEK','sum_ord'))
    }
  }
  summary_all <- data.table(temp_all)
  set_mmr <- summary_all[,c('CLAC3_NM','WEEK',grep('MMR', colnames(summary_all),value=T)), with=F]
  set_mmr <- set_mmr[order(CLAC3_NM)]
  set_abs_error <- summary_all[,c('CLAC3_NM','WEEK',grep('abs', colnames(summary_all),value=T)), with=F]
  set_abs_error <- set_abs_error[order(CLAC3_NM)]
  summary_all <- summary_all[order(CLAC3_NM)]
  
  summary_all$BEST_MODEL_MMR <- colnames(set_mmr[,3:ncol(set_mmr)])[apply(set_mmr[,3:ncol(set_mmr)],1,function(x) which.max(x))]
  summary_all$BEST_MODEL_abs_error <- colnames(set_abs_error[,3:ncol(set_abs_error)])[apply(set_abs_error[,3:ncol(set_abs_error)],1,function(x) which.max(x))]
  write.csv(summary_all, paste0(summary_result_path,'ALL_SUUMARY_',i,'.csv'),row.names = F)
  
  compare_set_temp <- summary_all[,c('CLAC3_NM','BEST_MODEL_MMR','BEST_MODEL_abs_error')]
  colnames(compare_set_temp)[2:3] <- paste0(colnames(compare_set_temp)[2:3],'_',i)
  
  compare_list[[paste0(i,'_BEST')]] <- compare_set_temp
}



## pedict 추출 및 Lift 확인
for(nk in 검색){
  summary_all <- fread(paste0('./summary_result/ALL_SUUMARY_',nk,'.csv'))
  summary_result_list <- sort(list.files('./summary_result/'))
  summary_result_list <- summary_result_list[1:(grep(nk,summary_result_list)-1)]

  set_compare<-list()
  for(i in summary_result_list){
    set_compare[[i]] <- fread(paste0('./summary_result/',i))
  }

  set_compare <- do.call(rbind,set_compare)

  set_compare <- set_compare %>%
    group_by(CLAC3_NM) %>%
    summarise(arima_predict_MMR = mean(arima_predict_MMR),            
              markov_predict_MMR = mean(markov_predict_MMR),
              mean_TF_IDF_nv_predict_MMR = mean(mean_TF_IDF_nv_predict_MMR),
              mean_TF_IDF_predict_MMR = mean(mean_TF_IDF_predict_MMR),
              mean_TF_nv_predict_MMR = mean(mean_TF_nv_predict_MMR),
              mean_TF_predict_MMR = mean(mean_TF_predict_MMR),
              sum_TF_IDF_nv_predict_MMR = mean(sum_TF_IDF_nv_predict_MMR),
              sum_TF_IDF_predict_MMR = mean(sum_TF_IDF_predict_MMR),
              sum_TF_nv_predict_MMR = mean(sum_TF_nv_predict_MMR),
              sum_TF_predict_MMR = mean(sum_TF_predict_MMR),
              mean_2week = mean(mean_2week_MMR)) %>%
    setDT()
  
  set_compare$BEST_set_MMR <- colnames(set_compare[,!'CLAC3_NM'])[apply(set_compare[,!'CLAC3_NM'],1, function(x) order(x,decreasing = T)[1])]
  set_compare$BEST_set_predict <- gsub('_MMR','',set_compare$BEST_set_MMR)
  
  set_compare$BEST_set_MMR_s <- colnames(set_compare[,!'CLAC3_NM'])[apply(set_compare[,!c('CLAC3_NM','BEST_set_MMR','BEST_set_predict')],1,function(x) order(x,decreasing = T)[2])]
  set_compare$BEST_set_predict_s <- gsub('_MMR','',set_compare$BEST_set_MMR_s)
  
  summary_all <- merge(summary_all, set_compare[,c('CLAC3_NM','BEST_set_predict','BEST_set_MMR',
                                         'BEST_set_predict_s', 'BEST_set_MMR_s'), with = F], by = 'CLAC3_NM')
  
  predict_result_list <- list()
  for(jjjj in 1:nrow(summary_all)){
    set_best <- summary_all[jjjj, BEST_set_predict]
    our_predict <- summary_all[jjjj, c('CLAC3_NM','WEEK',as.character(set_best),'sum_ord'), with = F]
    
    if(length(colnames(our_predict)[grep('predict',colnames(our_predict))])==0){
      colnames(our_predict)[colnames(our_predict)=='mean_2week'] <- 'predict'
    } else {
      colnames(our_predict)[grep('predict',colnames(our_predict))] <- 'predict'
    }
    
    if(is.na(our_predict$predict)){
      set_best <- summary_all[jjjj, BEST_set_predict_s]
      our_predict <- summary_all[jjjj, c('CLAC3_NM','WEEK',as.character(set_best),'sum_ord'), with = F]
      if(length(colnames(our_predict)[grep('predict',colnames(our_predict))])==0){
        colnames(our_predict)[colnames(our_predict)=='mean_2week'] <- 'predict'
      } else {
        colnames(our_predict)[grep('predict',colnames(our_predict))] <- 'predict'
      }
      our_predict$MODEL <- set_best
    } else {
      our_predict$MODEL <- set_best
    }
    
    if(is.na(our_predict$predict)){
      set_best <- 'mean_2week'
      our_predict <- summary_all[jjjj, c('CLAC3_NM','WEEK',as.character(set_best),'sum_ord'), with = F]
      if(length(colnames(our_predict)[grep('predict',colnames(our_predict))])==0){
        colnames(our_predict)[colnames(our_predict)=='mean_2week'] <- 'predict'
      } else {
        colnames(our_predict)[grep('predict',colnames(our_predict))] <- 'predict'
      }
      our_predict$MODEL <- set_best
    } else {
      our_predict$MODEL <- set_best
    }
    our_predict$mean_2week <- summary_all$mean_2week[jjjj]
    predict_result_list[[jjjj]] <- our_predict
  }
  predict_result <- do.call(rbind, predict_result_list)
  
  predict_result$MMR <- ifelse(is.na(predict_result$predict),0,ifelse(predict_result$predict == 0 & predict_result$sum_ord == 0, 1, pmin(predict_result$predict,predict_result$sum_ord)/pmax(predict_result$predict,predict_result$sum_ord)))
  predict_result$MMR_2week <- ifelse(is.na(predict_result$mean_2week),0,ifelse(predict_result$mean_2week == 0 & predict_result$sum_ord == 0, 1, pmin(predict_result$mean_2week,predict_result$sum_ord)/pmax(predict_result$mean_2week,predict_result$sum_ord)))
  predict_result <- predict_result[,c('CLAC3_NM', 'WEEK', 'MODEL', 'predict', 'mean_2week', 'sum_ord',
                                      'MMR', 'MMR_2week')]
  
  write.csv(predict_result, paste0('./our_result_path/',nk,'.csv'), row.names = F)
}
