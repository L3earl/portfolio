source('000.global.R', encoding = 'UTF-8')

# 데이터 불러오기
search2 <- fread(paste0(data_path,'search2.csv'), encoding = 'UTF-8')

# 네이버 크롤링 데이터 불러오기

if(file.exists(paste0(naver_file_path, naver_file_name))){
  naver_dlab_data <- read.csv(paste0(naver_file_path, naver_file_name), fileEncoding = 'UTF-8') %>% 
      setDT()
} else {
  print("Naver 크롤링 데이터가 없습니다.")
}

# TF_IDF에 사용할 키워드 전체 데이터
tf_idf_kwd <- search2 %>% 
    group_by(SESS_DT,KWD_NM) %>% 
    summarise(SEARCH_CNT = sum(SEARCH_CNT)) %>% 
    setDT()

### TF_IDF 파라미터
# 기간 sdt ~ edt
# 파라미터 tf_phase

# TF_IDF 데이터 만들 날짜 생성
dt_tf_idf <- set_date %>% 
    filter(Date >= s_dt, Date <= e_dt) %>% 
    select(dt) %>% 
    setDT()

set_day <- sort(unique(dt_tf_idf$dt))

## search2 키워드로 TF_IDF 생성
tf_result <- c()

for(mon_w in 1:(length(set_day)-tf_phase+1)){
    print(paste0(round(mon_w/(length(set_day)-tf_phase+1)*100,2) , '%'))
    
    tf_idf_daily_file_name <- paste0('tf_idf_', set_day[(mon_w + tf_phase - 1)], '_',
                                     tf_phase, '.csv')
  
    if(!file.exists(paste0(tf_idf_daily_file_path,tf_idf_daily_file_name))){
    # 학습에 사용할 날짜
    set_day_temp <- set_day[mon_w:(mon_w + tf_phase - 1)]
    
    # 학습에 쓰일 데이터만 추출
    data_temp <- lapply(set_day_temp, function(x) tf_idf_kwd[tf_idf_kwd$SESS_DT == x])
    
    # 마지막 날짜의 데이터
    data_now <- data_temp[[length(data_temp)]]
    
    # 전체 날짜 데이터
    data_temp <- do.call(rbind, data_temp)
    
    # TF (현재 날짜의 검색량) 생성     .,,.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,                                                                         ,ㅡ       ,,  
    data_now$TF<- log(data_now$SEARCH_CNT)
    
    # 키워드별로 n_IDF (phase 동안 검색어가 출현한 빈도) 생성
    data_set_IDF <- lapply(data_now$KWD_NM, function(x) data.table(KWD_NM = x,
                                                                   n_idf = nrow(data_temp[KWD_NM == x])))
    data_set <- do.call(rbind, data_set_IDF)
    
    # 최종 데이터
    data_now <- merge(data_now, data_set, by = 'KWD_NM')
    data_now$IDF <- 1+log(length(set_day_temp)/data_now$n_idf)
    data_now$TF_IDF <- data_now$TF * data_now$IDF
    
    write.csv(data_now, paste0(tf_idf_daily_file_path,tf_idf_daily_file_name), 
              fileEncoding = 'UTF-8', row.names = F)
    
    tf_result <- rbind(tf_result, data_now)
    } else {
      data_now <- fread(paste0(tf_idf_daily_file_path,tf_idf_daily_file_name),
                        encoding = 'UTF-8')
      tf_result <- rbind(tf_result, data_now)
    }
}

## 네이버 크롤링 키워드로 TF_IDF 생성
if(file.exists(paste0(naver_file_path, naver_file_name))){
tf_result_naver <- c()

for(mon_w in 1:(length(set_day)-tf_phase+1)){ 
    print(paste0(round(mon_w/(length(set_day)-tf_phase+1)*100,2) , '%'))
    
    # 학습에 사용할 날짜
    set_day_temp <- set_day[mon_w:(mon_w + tf_phase - 1)]
    
    # 학습에 쓰일 데이터만 추출
    data_temp <- lapply(set_day_temp, function(x) naver_dlab_data[naver_dlab_data$SESS_DT == x])
    
    # 마지막 날짜의 데이터
    data_now <- data_temp[[length(data_temp)]]
    
    # 전체 날짜 데이터
    data_temp <- do.call(rbind, data_temp)
    
    # TF (현재 날짜의 검색량) 생성
    data_now$TF_nv<- log(data_now$SEARCH_CNT_nv)
    
    # 키워드별로 n_IDF (phase 동안 검색어가 출현한 빈도) 생성
    data_set_IDF <- lapply(data_now$KWD_NM, function(x) data.table(KWD_NM = x,
                                                                   n_idf_nv = nrow(data_temp[KWD_NM == x])))
    data_set <- do.call(rbind, data_set_IDF)
    
    # 최종 데이터
    data_now <- merge(data_now, data_set, by = 'KWD_NM')
    data_now$IDF_nv <- 1+log(length(set_day_temp)/data_now$n_idf_nv)
    data_now$TF_IDF_nv <- data_now$TF_nv * data_now$IDF_nv
    
    tf_result_naver <- rbind(tf_result_naver, data_now)
}

  final <- tf_result_naver %>% 
      inner_join(tf_result, by = c('KWD_NM', 'SESS_DT')) %>% 
      setDT()
  
  # 저장
  write.csv(final, paste0(tf_idf_file_path, tf_idf_file_name), 
            fileEncoding = 'UTF-8', row.names = FALSE)
} else {
  
  # 저장
  write.csv(tf_result, paste0(tf_idf_file_path, tf_idf_file_name), 
            fileEncoding = 'UTF-8', row.names = FALSE)
}



