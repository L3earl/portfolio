source('000.global.R', encoding = 'UTF-8')

# data 불러오기
product <- fread(paste0(data_path,'product.csv'), encoding = 'UTF-8')
search1 <- fread(paste0(data_path,'search1.csv'), encoding = 'UTF-8')
session <- fread(paste0(data_path,'session.csv'), encoding = 'UTF-8')
master <- fread(paste0(data_path,'master.csv'), encoding = 'UTF-8')


# 구매가 일어난 데이터에 구매한 제품 소분류, 구매날짜, 구매에 쓰인 검색어를 매핑시킴
set_transaction <- product %>% 
    inner_join(master, by = c('PD_C')) %>% 
    inner_join(session, by = c('CLNT_ID', 'SESS_ID')) %>% 
    inner_join(search1, by = c('CLNT_ID', 'SESS_ID')) %>% 
    select(SESS_ID,SESS_DT,CLAC3_NM, KWD_NM) %>% 
    mutate(SESS_ID = as.character(SESS_ID)) %>% 
    setDT()

### 연관규칙분석
# 기간 sdt ~ edt
# 파라미터 ap_supp, ap_con, ap_min, ap_max

# 검색어와 구매한 소분류의 연관규칙을 찾아낼 날짜 세팅
dt_transaction <- set_date %>% 
    filter(Date >= s_dt, Date <= e_dt) %>% 
    select(dt) %>% 
    setDT()

# 결과 테이블 세팅
KWD_mapping_all_table <- c()

# 특정 날짜만큼 실행
for(i in sort(dt_transaction[,dt])) {
  
    print(i)
    kwd_mapping_daily_file_name <- paste0("kwd_map_",i, '_',
                                          ap_supp, '_', ap_con, '_',
                                          ap_min, '_', ap_max, '.csv')
    if(!file.exists(paste0(kwd_mapping_daily_file_path, kwd_mapping_daily_file_name))){
      
    # 날짜별로 검색어-구매 연관규칙을 따로 만들 것이므로, 하루의 데이터만 추출
    tmp_tran <- set_transaction[SESS_DT == i]
    
    # 연관규칙분석에 사용하기 위해 각 ID별로 검색-구매한 데이터를 하나의 데이터로 만듬
    set_transaction_li <- lapply(unique(tmp_tran$SESS_ID), 
                                 function(x) {
                                     data.table(SESS_ID = x,
                                                text = c(unique(tmp_tran[SESS_ID == x]$CLAC3_NM),
                                                         unique(tmp_tran[SESS_ID == x]$KWD_NM)))
                                 })
    set_transaction_li <- do.call(rbind, set_transaction_li)
    rioter.list <- split(set_transaction_li$text,set_transaction_li$SESS_ID)
    
    # 전체 데이터를 트랜젝션으로 변환
    rioter.transaction <- as(rioter.list, "transactions")
    
    # 연관규칙분석 실행
    rules = apriori(rioter.transaction, parameter = list(supp = ap_supp,
                                                         confidence = ap_con,
                                                         minlen = ap_min,
                                                         maxlen = ap_max),
                    appearance = list(rhs = unique(tmp_tran$CLAC3_NM)))
    
    # 연관규칙
    rule.list <- as.data.table(inspect(rules))
    
    # 연관규칙 데이터 테이블로 변환
    KWD_mapping_table <- data.table(KWD_NM = as.character(rule.list$lhs),
                                    CLAC3_NM = as.character(rule.list$rhs))
    
    KWD_mapping_table$KWD_NM <- gsub('[{}]','',KWD_mapping_table$KWD_NM)
    KWD_mapping_table$CLAC3_NM <- gsub('[{}]','',KWD_mapping_table$CLAC3_NM)
    write.csv(KWD_mapping_table, paste0(kwd_mapping_daily_file_path, kwd_mapping_daily_file_name),
              row.names = F,fileEncoding = 'UTF-8')
    
    # 전체 연관규칙 합침
    KWD_mapping_all_table <- rbind(KWD_mapping_all_table, KWD_mapping_table)
    
    } else {
      KWD_mapping_table <- fread(paste0(kwd_mapping_daily_file_path, 
                                        kwd_mapping_daily_file_name), encoding = 'UTF-8')
      KWD_mapping_all_table <- rbind(KWD_mapping_all_table, KWD_mapping_table)
    }
}

# 중복 규칙 삭제
KWD_mapping_all_table <- unique(KWD_mapping_all_table)

# 저장
write.csv(KWD_mapping_all_table, paste0(kwd_mapping_file_path, kwd_mapping_file_name), 
          fileEncoding = 'UTF-8', row.names = F)
