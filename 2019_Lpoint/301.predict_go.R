source('000.global.R', encoding = 'UTF-8')

###############################################################################################################################
#예측주 날짜
predict_date <- set_date[dt == as.numeric(edt) + 1, WEEK]

# Train 데이터 기간
train_date <- set_date[dt == edt, WEEK]

#주 월 매핑 테이블 로드
unique_week <- unique(set_date$WEEK)

if(model_name == 'ARIMA_XREG'){ 
    train <- fread(paste0(kwd_tf_merge_file_path, kwd_tf_merge_file_name), encoding = 'UTF-8')
    
    train <- as.data.frame(train)
    train[,grep('TF', colnames(train), value = T)] <- apply(train[,grep('TF', colnames(train), value = T)], 2, function(x) ifelse(x<0,0,x))
    train <- as.data.table(train)
    xreg_set <- grep('TF', colnames(train), value = T) 
    
    # 분석 조건에 맞게 데이터 테이블 -> 리스트로 변환
    sell_li <- split(train, train$CLAC3_NM)
    
    # ARIMA_STEP Start !
    for(h in 1:length(xreg_set)){

      # CCF Create
      cor_li <- list()
      for(i in 1:length(sell_li)){
        tmp_data <- sell_li[[i]]
        xg <- unlist(tmp_data[, xreg_set[h], with=F])
        y <- tmp_data$sum_ord
      
        cor_table <- data.table(lag = 0, cor = cor(xg,y))
        for(j in 1:length(xg)){
          if(length(xg[1:(length(xg)-j)]) > 3){
            xg_temp <- xg[1:(length(xg)-j)]
            y_temp <- y[(j+1):(length(y))]
            cor_table <- rbind(cor_table,
                               data.table(lag = j, cor = cor(xg_temp, y_temp)))
         }
       }
        cor_table$CLAC3_NM <- sell_li[[i]]$CLAC3_NM[1]
        cor_table_non_zero <- cor_table[lag != 0]
        cor_li[[i]] <- cor_table_non_zero[which.max(cor)]
      }
      cor_table <- do.call(rbind,cor_li)
      cor_table <- cor_table[cor > 0.5]
      
      result_li <- list()
      for( i in cor_table$CLAC3_NM) {
      
        lag <- unlist(cor_table[CLAC3_NM == i, 'lag'])
        CLAC3 <- unlist(cor_table[CLAC3_NM == i, 'CLAC3_NM'])
      
        tmp_data <- sell_li[[CLAC3]]
      
        tmp_x <- ts(tmp_data$sum_ord[(1+lag):length(tmp_data$sum_ord)])
        tmp_rag <- ts(tmp_data[1:(length(tmp_data$sum_ord)-lag),xreg_set[h],with=F])
        tmp_new_reg <- tmp_data[(length(tmp_data$sum_ord)-lag+1),xreg_set[h],with=F]
      
        fit <- auto.arima(tmp_x,
                          xreg = tmp_rag)
        pre <- predict(fit, n.ahead = 1,
                       newxreg = tmp_new_reg)
      
        pre <- pre$pred
        pre <- ifelse(pre < 0 , 0, pre)
        pre <- round(pre)
      
        result <- data.table(CLAC3_NM = CLAC3,
                             predict = as.numeric(pre))
      
        colnames(result)[2] <- paste0(xreg_set[h],'_predict')
        result_li[[i]] <- result
      }
    result <- do.call(rbind, result_li)
    table(result$CLAC3_NM)[table(result$CLAC3_NM) > 1]
    write.csv(result,paste0(predict_result_path,
                            'arima_forecast_',predict_date,'_',xreg_set[h],'.csv'),row.names = F,
              fileEncoding = 'UTF-8')
    }
   
} else if(model_name == 'ARIMA'){
    all_preprocessed <- fread(paste0(markov_variable_path, markov_variable_file_name), encoding = 'UTF-8')
    all_preprocessed$WEEK <- as.character(all_preprocessed$WEEK)
    
    #수요데이터 인터폴레이션
    need_col_interp <- c("CLAC3_NM",'WEEK','PD_BUY_CT')
    pro_interpol <- all_preprocessed[,need_col_interp,with=F]
    pro_interpol_week <- pro_interpol %>% 
      group_by(CLAC3_NM,WEEK) %>% 
      summarise(sum_ord=sum(PD_BUY_CT)) %>% 
      setDT()
    
    pro_list <- split(pro_interpol_week, pro_interpol_week$CLAC3_NM)
    
    #interpolation
    pro_list_final<-list()
    for(aa in 1:length(pro_list)){
      CLAC3 <- unique(pro_list[[aa]]$CLAC3_NM)
      
      temp_pro <- merge(pro_list[[aa]],
                        unique(set_date[set_date$WEEK>=201814 & set_date$WEEK <= unique_week[which(unique_week==train_date)], 'WEEK']),
                        by = 'WEEK', all.y = T)
      
      temp_pro[is.na(sum_ord), 'sum_ord'] <- 0
      temp_pro[is.na(CLAC3_NM), 'CLAC3_NM'] <- CLAC3
      pro_list_final[[aa]]<-temp_pro
    }
    train <- do.call(rbind,pro_list_final)
    
    # 분석 조건에 맞게 데이터 테이블 -> 리스트로 변환
    sell_li <- split(train, train$CLAC3_NM)
    
    result_li <- list()
    for( i in unique(train$CLAC3_NM)) {
      CLAC3 <- unique(train[CLAC3_NM == i, CLAC3_NM])
      
      tmp_data <- sell_li[[CLAC3]]
      
      tmp_x <- ts(tmp_data$sum_ord)
      
      fit <- auto.arima(tmp_x)
      pre <- forecast(fit, 1)
      pre <- pre$mean
      pre <- ifelse(pre < 0 , 0, pre)
      pre <- round(pre)
      
      result <- data.table(CLAC3_NM = CLAC3,
                           predict = as.numeric(pre))
      
      colnames(result)[2] <- paste0('arima_predict')
      result_li[[i]] <- result
    }
    result <- do.call(rbind, result_li)
    
    write.csv(result,paste0(predict_result_path,
                            'arima_forecast_',predict_date,'.csv'),row.names = F,
              fileEncoding = 'UTF-8')
    
} else if(model_name=='MARKOV') {
      
      day_clust <- e_dt
  
      #TRAIN 데이터
      all_preprocessed <- fread(paste0(markov_variable_path, markov_variable_file_name),
                                encoding = 'UTF-8')
      all_preprocessed$WEEK <- as.character(all_preprocessed$WEEK)
      
      #군집 데이터
      clust_som <- fread(paste0(som_class_table_path, som_class_table_name),
                         encoding = 'UTF-8')
      
      #수요데이터 인터폴레이션
      need_col_interp <- c("CLAC3_NM",'WEEK','PD_BUY_CT')
      pro_interpol <- all_preprocessed[,need_col_interp,with=F]
      pro_interpol_week <- pro_interpol %>% 
        group_by(CLAC3_NM,WEEK) %>% 
        summarise(sum_p=sum(PD_BUY_CT)) %>% 
        setDT()
      
      pro_list <- split(pro_interpol_week, pro_interpol_week$CLAC3_NM)
    
      #interpolation
      pro_list_final<-list()
      for(aa in 1:length(pro_list)){
        CLAC3 <- unique(pro_list[[aa]]$CLAC3_NM)
        temp_pro <- merge(pro_list[[aa]],
                          unique(set_date[set_date$WEEK>=201814 & set_date$WEEK <= unique_week[which(unique_week==train_date)], 'WEEK']),
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
        group_data_use$WEEK <- as.numeric(group_data_use$WEEK)
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
      final_mmr_result <- final_mmr_result[,c('CLAC3_NM','predict')]
      colnames(final_mmr_result)[2] <- 'markov_predict'
      write.csv(final_mmr_result,paste0(predict_result_path,'marcov_forecast_',predict_date,'.csv'),
                row.names = F, fileEncoding = 'UTF-8')    
    
      } else if(model_name=='BASE'){
        #TRAIN 데이터
        all_preprocessed <- fread(paste0(markov_variable_path, markov_variable_file_name),
                                  encoding = 'UTF-8')
        all_preprocessed$WEEK <- as.character(all_preprocessed$WEEK)
        
        #수요데이터 인터폴레이션
        need_col_interp <- c("CLAC3_NM",'WEEK','PD_BUY_CT')
        pro_interpol <- all_preprocessed[,need_col_interp,with=F]
        pro_interpol_week <- pro_interpol %>% 
          group_by(CLAC3_NM,WEEK) %>% 
          summarise(sum_ord=sum(PD_BUY_CT)) %>% 
          setDT()
        
        pro_list <- split(pro_interpol_week, pro_interpol_week$CLAC3_NM)
        
        #interpolation
        pro_list_final<-list()
        for(aa in 1:length(pro_list)){
          CLAC3 <- unique(pro_list[[aa]]$CLAC3_NM)
          temp_pro <- merge(pro_list[[aa]],
                            unique(set_date[set_date$WEEK>=201814 & set_date$WEEK <= unique_week[which(unique_week==train_date)], 'WEEK']),
                            by = 'WEEK', all.y = T)
          
          temp_pro[is.na(sum_ord), 'sum_ord'] <- 0
          temp_pro[is.na(CLAC3_NM), 'CLAC3_NM'] <- CLAC3
          pro_list_final[[aa]]<-temp_pro
        }
       train <- do.call(rbind,pro_list_final)
       CLAC3_train<-split(train,train$CLAC3_NM)
     
       final_base_result<-lapply(CLAC3_train,function(x){
                                             temp=x[(nrow(x)-1):nrow(x),]
                                             temp=data.frame(CLAC3_NM=unique(temp$CLAC3_NM),mean_2week=mean(temp$sum_ord))
                                             return(temp)})
       final_base_result<-do.call(rbind,final_base_result)   
       final_base_result$mean_2week<-round(final_base_result$mean_2week,0)
       write.csv(final_base_result, paste0(predict_result_path, 'base_forecast_',predict_date,'.csv'),
                 row.names = F, fileEncoding = 'UTF-8')
      }