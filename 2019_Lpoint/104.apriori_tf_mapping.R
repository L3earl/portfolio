source('000.global.R', encoding = 'UTF-8')

## data 불러오기
product <- fread(paste0(data_path,'product.csv'), encoding = 'UTF-8')
search1 <- fread(paste0(data_path,'search1.csv'), encoding = 'UTF-8')
session <- fread(paste0(data_path,'session.csv'), encoding = 'UTF-8')
master <- fread(paste0(data_path,'master.csv'), encoding = 'UTF-8')

# 소분류 상품군의 주별 구매량
session$SESS_DT <- as.character(session$SESS_DT)
set_date_temp <- set_date %>%
  filter(Date >= s_dt & Date <= e_dt) %>%
  setDT()

sell_dt <- product %>% 
    inner_join(master, by = c('PD_C')) %>%
    inner_join(session, by = c('CLNT_ID', 'SESS_ID')) %>% 
    inner_join(search1, by = c('CLNT_ID', 'SESS_ID')) %>% 
    inner_join(set_date_temp, by = c('SESS_DT' = 'dt')) %>% 
    group_by(CLAC3_NM, WEEK) %>% 
    summarise(sum_ord = sum(as.integer(PD_BUY_CT))) %>% 
    setDT()

interpolation_week <- unique(set_date_temp[,'WEEK'])

sell_dt <- split(sell_dt, sell_dt$CLAC3_NM)
for(i in names(sell_dt)){
  tmp <- merge(sell_dt[[i]], interpolation_week , by = 'WEEK', all.y=T)
  tmp[is.na(CLAC3_NM), 'CLAC3_NM'] <- tmp[!is.na(CLAC3_NM), 'CLAC3_NM'][1]
  tmp[is.na(sum_ord), 'sum_ord'] <- 0
  sell_dt[[i]] <- tmp
}
sell_dt <- do.call(rbind, sell_dt)
# 키워드 - 소분류 규칙
kwd_mapping_data <- fread(paste0(kwd_mapping_file_path, kwd_mapping_file_name), 
                          encoding = 'UTF-8', stringsAsFactors = F)

# TF_IDF 데이터
tf_idf_data <- fread(paste0(tf_idf_file_path, tf_idf_file_name), 
                     encoding = 'UTF-8', stringsAsFactors = F)
tf_idf_data$SESS_DT <- as.character(tf_idf_data$SESS_DT)

####################################### 이부분 코드 맞나 확인 필요 - 시작 #############3333
# 키워드 - 소분류 규칙에 tf_idf, week, 소분류별 구매량 붙힘
if(is.null(tf_idf_data$TF_nv)){
  final_data <- kwd_mapping_data %>% 
    inner_join(tf_idf_data, by = 'KWD_NM') %>% 
    inner_join(set_date_temp, by = c('SESS_DT' = 'dt')) %>% 
    group_by(CLAC3_NM, WEEK) %>% 
    summarise(sum_TF = sum(TF),
              mean_TF = mean(TF),
              sum_TF_IDF = sum(TF_IDF),
              mean_TF_IDF = mean(TF_IDF)) %>%
    setDT()
} else {
  final_data <- kwd_mapping_data %>% 
    inner_join(tf_idf_data, by = 'KWD_NM') %>% 
    inner_join(set_date_temp, by = c('SESS_DT' = 'dt')) %>% 
    group_by(CLAC3_NM, WEEK) %>% 
    summarise(sum_TF = sum(TF),
              mean_TF = mean(TF),
              sum_TF_IDF = sum(TF_IDF),
              mean_TF_IDF = mean(TF_IDF),
              sum_TF_nv = sum(TF_nv),
              mean_TF_nv = mean(TF_nv),
              sum_TF_IDF_nv = sum(TF_IDF_nv),
              mean_TF_IDF_nv = mean(TF_IDF_nv)) %>% 
    setDT()
}
final_data <- split(final_data,final_data$CLAC3_NM)

for(i in names(final_data)){
  tmp <- final_data[[i]]
  tmp <- merge(tmp, interpolation_week , by = 'WEEK', all.y=T)
  tmp[is.na(CLAC3_NM), 'CLAC3_NM'] <- tmp[!is.na(CLAC3_NM), 'CLAC3_NM'][1]
  tmp <- as.data.frame(tmp)
  tmp[,grep('TF',colnames(tmp))] <- apply(tmp[,grep('TF',colnames(tmp))], 2, function(x) ifelse(is.na(x),0,x))
  tmp <- as.data.table(tmp)
  final_data[[i]] <- tmp
}
final_data <- do.call(rbind,final_data)

# 판매량-검색어 merge
final_data <- final_data %>%
  inner_join(sell_dt, by = c('WEEK', 'CLAC3_NM')) %>%
  setDT()

####################################### 이부분 코드 맞나 확인 필요 - 끝#############3333

# 저장
write.csv(final_data, paste0(kwd_tf_merge_file_path, kwd_tf_merge_file_name), 
          fileEncoding = 'UTF-8', row.names = FALSE)
