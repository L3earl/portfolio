library(skmeans)
library(kohonen)
library(data.table)
library(dplyr)


marcov_confusion <- function(data_set){
  
  temp <- data_set
  matrix_temp <- data.table(pre=integer(0), nex=integer(0))
  
  if(length(unique(temp$WEEK)) >= 2){
    WEEK_temp <- sort(unique(temp$WEEK))
    
    for(i in WEEK_temp[1:(length(WEEK_temp)-1)]){
      start <- i
      
      WEEK_temp_2 <- WEEK_temp[start == WEEK_temp | WEEK_temp == (start+1)]        
      temp_use <- temp[temp$WEEK %in% WEEK_temp_2,]
      temp_use <- temp_use[order(WEEK)]
      
      if(length(unique(temp_use$WEEK)) >= 2){
        start_CLAC3_NM_key_cnt <- nrow(temp_use[temp_use$WEEK==start,])
        
        for(j in 1:start_CLAC3_NM_key_cnt){
          if(start_CLAC3_NM_key_cnt >= 2){
            temp_use_2 <- temp_use[c(j,(start_CLAC3_NM_key_cnt+1):nrow(temp_use)),]
          }else{
            temp_use_2 <- temp_use  
          }
          
          matrix_temp_t <- data.table(pre= temp_use_2$CLAC3_NM_key[1] ,
                                      nex= temp_use_2$CLAC3_NM_key[2:nrow(temp_use_2)])
          matrix_temp <- rbind(matrix_temp,matrix_temp_t, fill=T)
        }
      }
    }
  } 
  return(matrix_temp)
}

###############################################################################################################################
predict_date_original<-201827
clust_date_original<-201826

for(i in 1:7){

file_path_1<-'C:/Users/김태훈/Desktop/빅데이터/LPOINT/Data_for_predict/'

#예측주 날짜
predict_date<-predict_date_original+i

#군집데이터 기간
clust_date<-clust_date_original+i

#모델 이름
model_name<-'marcov_p'

#주 월 매핑 테이블 로드
set_week<-fread(paste0(file_path_1,'mon_week.csv'))
set_week$Date <-gsub('-','',set_week$Date)
set_week$Date <- as.numeric(set_week$Date) 
unique_week<-unique(set_week$WEEK)


train_date<-unique_week[which(unique_week==predict_date)-1]
day_train<-set_week$Date[set_week$WEEK==train_date]
day_train<-day_train[length(day_train)]

day_clust<-set_week$Date[set_week$WEEK==clust_date]
day_clust<-day_clust[length(day_clust)]

#TRAIN 데이터
markov_variable<-list.files(file_path_1)[grep('markov_variable',list.files(file_path_1))]
markov_variable<-markov_variable[grep(day_train,markov_variable)]
all_preprocessed<-fread(paste0(file_path_1,markov_variable))

#군집에 필요한 데이터 로드 
som_table_need<-list.files(file_path_1)[grep('som_class_table',list.files(file_path_1))]
som_table_need<-som_table_need[grep(day_clust,somvariable_need)]

clust_som<-fread(paste0(file_path_1,som_table_need))

if(model_name=='marcov_p'){
  
  #군집분석

  #수요데이터 인터폴레이션
  need_col_interp <- c("CLAC3_NM",'WEEK','PD_BUY_CT')
  pro_interpol <- all_preprocessed[,need_col_interp,with=F]
  pro_interpol_week <- pro_interpol %>% group_by(CLAC3_NM,WEEK) %>% summarise(sum_p=sum(PD_BUY_CT)) %>% as.data.table()
  
  pro_list <- split(pro_interpol_week, pro_interpol_week$CLAC3_NM)
  
  #interpolation
  pro_list_final<-list()
  for(aa in 1:length(pro_list)){
    CLAC3 <- unique(pro_list[[aa]]$CLAC3_NM)
    temp_pro <- merge(pro_list[[aa]],
                      unique(set_week[set_week$WEEK>=201814 & set_week$WEEK<=unique_week[which(unique_week==train_date)], 'WEEK']),
                      by = 'WEEK', all.y = T)
    
    temp_pro[is.na(sum_p), 'sum_p'] <- 0
    temp_pro[is.na(CLAC3_NM), 'CLAC3_NM'] <- CLAC3
    pro_list_final[[aa]]<-temp_pro
  }
  pro_list_final <- do.call(rbind,pro_list_final)
  pro_list_final <- merge(pro_list_final, clust_som, by=c('CLAC3_NM'), all.x = T)
  
  need_col <- c("CLAC3_NM","CLNT_ID",'WEEK')
  marcov_data <- all_preprocessed[ , need_col, with=F]
  marcov_data <- merge(marcov_data, clust_som, by=c('CLAC3_NM'), all.x = T)
 
 
 
    marcov_data <- marcov_data[WEEK <= train_date,]
    
    final_mmr_result<-list()
    for(group in unique(marcov_data$class)){
      
      group_data <- marcov_data[class==group]
      group_data <- group_data[order(group_data$CLNT_ID,group_data$WEEK),]
      
      prod_type <- unique(group_data$CLAC3_NM)
      
      prod_type_table <- data.table(CLAC3_NM_key = c(1:length(prod_type)), CLAC3_NM = prod_type)
      
      group_data <- merge(group_data,prod_type_table, by=c('CLAC3_NM'), all.x = T)
      group_data$CLAC3_NM <- NULL
      
      group_data_use <- group_data
      group_data_use <- group_data_use[!duplicated(group_data_use), ]
      group_data_CLNT <- split(group_data_use,group_data_use$CLNT_ID)
      
      marcov_conf_data <- lapply(group_data_CLNT, marcov_confusion)
      marcov_conf_data <- do.call(rbind,marcov_conf_data)
      marcov_conf_data <- data.frame(marcov_conf_data)
      
      marcov_matrix <- matrix(rep(0,length(prod_type)*length(prod_type)),nrow = length(prod_type) ,ncol = length(prod_type))
      for(k in 1:nrow(marcov_conf_data)){
        
        marcov_matrix[marcov_conf_data[k,'pre'],marcov_conf_data[k,'nex']]<-marcov_matrix[marcov_conf_data[k,'pre'],marcov_conf_data[k,'nex']]+1
        
      }
      
      marcov_matrix_final <- as.data.frame(marcov_matrix)
      marcov_matrix_final <- as.data.frame(t(apply(marcov_matrix_final,1,function(x){
        if(sum(x)!=0){
          return(x/sum(x))
        }else{
          return(x)
        }
      })))
      
      colnames(marcov_matrix_final) <- unlist(prod_type_table[1:nrow(prod_type_table), 'CLAC3_NM'])
      rownames(marcov_matrix_final) <- unlist(prod_type_table[1:nrow(prod_type_table), 'CLAC3_NM'])
      
      train <- pro_list_final %>% filter(WEEK ==train_date  & class==group)
      
      temp_sell <- t(train[,3])
      colnames(temp_sell) <- train[,1]
      temp_sell <- temp_sell[,rownames(marcov_matrix_final)]
      
      predict_marcov <- t(t(matrix(temp_sell)) %*% as.matrix(marcov_matrix_final))
      
      predict_marcov <- as.data.frame(predict_marcov)
      colnames(predict_marcov) <- 'predict'
      predict_marcov$CLAC3_NM <- row.names(predict_marcov)
      predict_marcov$Train_week <- train_date
      predict_marcov$Predict_week <- predict_date
      
      final_mmr_result[[group]] <- predict_marcov
    
    }
    
    final_mmr_result <- do.call(rbind,final_mmr_result)
    
    
    
    write.csv(final_mmr_result,paste0(file_path_1,'marcov_forecast_',predict_date,'.csv'),row.names = F)
    
}
}
