source('000.global.R', encoding = 'UTF-8')

# data 불러오기
product <- fread(paste0(data_path,'product.csv'), encoding = 'UTF-8')
session <- fread(paste0(data_path,'session.csv'), encoding = 'UTF-8')
master <- fread(paste0(data_path,'master.csv'), encoding = 'UTF-8')

#===================================================================
#=================== 결측치 및 이상치 imputation ===================
#===================================================================
    
# imputation 대상 : HITS_SEQ, TOT_PAG_VIEW_CT, TOT_SESS_HR_V
set_all_data <- product %>% 
  inner_join(master, by = c('PD_C')) %>% 
  inner_join(session, by = c('CLNT_ID', 'SESS_ID')) %>% 
  mutate(SESS_DT = as.character(SESS_DT)) %>%
  setDT()
    
# SOM 알고리즘에 활용 할 파생변수 생성을 시행할 데이터 날짜 세팅
set_imputation <- set_all_data %>%
  filter(SESS_DT >= sdt & SESS_DT <= edt) %>%
  select(CLNT_ID, SESS_ID, SESS_DT, CLAC3_NM, HITS_SEQ, TOT_PAG_VIEW_CT, TOT_SESS_HR_V) %>%
  setDT()
    
# 구매 전환 횟수 변수 생성
set_conversions <- set_imputation %>% 
  group_by(CLNT_ID, SESS_ID, SESS_DT) %>%
  tally() %>%
  setDT()
    
set_imputation <- set_imputation %>%
  inner_join(set_conversions, by = c('CLNT_ID', 'SESS_ID', 'SESS_DT')) %>%
  setDT()
    
# TOT_PAG_VIEW_CT Imputation : 구매 전환 횟수 별 TOT_PAG_VIEW_CT의 중앙값으로 imputation
conver <- set_imputation %>%
  filter(is.na(TOT_PAG_VIEW_CT)) %>%
  select(n) %>%
  unique()
    
for(i in unlist(conver)) {
  set_imputation[is.na(TOT_PAG_VIEW_CT) & n == i]$TOT_PAG_VIEW_CT <- round(median(set_imputation[!is.na(TOT_PAG_VIEW_CT) & n == i]$TOT_PAG_VIEW_CT))
}
    
# TOT_SESS_HR_V Imputation : TOT_PAG_VIEW_CT 별 TOT_SESS_HR_V의 중앙값으로 imputation
TOT_SESS_temp <- set_imputation %>%
  filter(is.na(TOT_SESS_HR_V)) %>%
  select(TOT_PAG_VIEW_CT) %>%
  unique()
    
for(i in unlist(TOT_SESS_temp)){
  set_imputation[is.na(TOT_SESS_HR_V) & TOT_PAG_VIEW_CT == i]$TOT_SESS_HR_V <- round(median(set_imputation[TOT_PAG_VIEW_CT == i]$TOT_SESS_HR_V, na.rm = T))
}
    
# 독립 변수 : TOT_PAG_VIEW_CT, TOT_SESS_HR_V
# 종속 변수 : HITS_SEQ
# 회귀분석 모델 예측을 이용한 HITS_SEQ의 이상치 예측
out_vari <- set_imputation[HITS_SEQ == 1]
base_vari <- set_imputation[HITS_SEQ != 1]
    
fit_rf <- lm(HITS_SEQ~TOT_PAG_VIEW_CT + TOT_SESS_HR_V, data=base_vari)
summary(fit_rf)
    
out_vari$HITS_SEQ <- round(predict(fit_rf, out_vari[,c('TOT_PAG_VIEW_CT', 'TOT_SESS_HR_V')]))
set_imputation <- rbind(base_vari, out_vari)
    
#===========================================================================
#==================== 군집분석을 위한 파생 변수 생성 =======================
#===========================================================================
# 파생 변수 생성을 위한 imputation 완료 데이터 준비
ready_data <- set_all_data %>%
  filter(SESS_DT >= sdt & SESS_DT <= edt) %>%
  select(CLNT_ID, SESS_ID, SESS_DT, CLAC3_NM, PD_BUY_AM, PD_BUY_CT, PD_BRA_NM) %>%
  setDT()
    
ready_data <- ready_data %>%
  inner_join(set_imputation, by = c('CLNT_ID', 'SESS_ID', 'SESS_DT','CLAC3_NM')) %>%
  setDT()
    
# 구매 전환 별 평균 클릭수, 페이지뷰수, 세션시간 변수 생성
ready_data$HITS_SEQ_n <- ceiling(ready_data$HITS_SEQ/ready_data$n)
ready_data$TOT_PAG_VIEW_CT_n <- ceiling(ready_data$TOT_PAG_VIEW_CT/ready_data$n)
ready_data$TOT_SESS_HR_V_n <- ceiling(ready_data$TOT_SESS_HR_V/ready_data$n)

# 세션 별 한 페이지 당 머무른 시간 변수 생성
ready_data$Time_Click <- ceiling(ready_data$TOT_SESS_HR_V/ready_data$TOT_PAG_VIEW_CT)

# 소분류 상품들의 구매 전환 대비 TOT_SESS_HR_V의 중앙값 생성
M_TOT_SESS_HR <- ready_data %>% 
  group_by(CLAC3_NM) %>% 
  summarise(TOT_SESS_HR_V_median = median(TOT_SESS_HR_V_n)) %>%
  setDT()

# 소분류 상품들의 구매 전환 대비 TOT_PAG_VIEW_CT의 중앙값 생성
M_TOT_PAG_VIEW_CT <- ready_data %>% 
  group_by(CLAC3_NM) %>% 
  summarise(TOT_PAG_VIEW_CT_medain = median(TOT_PAG_VIEW_CT_n)) %>%
  setDT()

# 소분류 상품들의 구매 전환 대비 HITS_SEQ의 중앙값 생성
M_HIT_SEQ <- ready_data %>% 
  group_by(CLAC3_NM) %>% 
  summarise(HITS_SEQ_medain = median(HITS_SEQ_n)) %>%
  setDT()

# 소분류 상품 별 브랜드 개수 생성
CNT_BRA <- ready_data %>%
  group_by(CLAC3_NM) %>% 
  summarise(BRA_cnt = length(unique(PD_BRA_NM))) %>%
  setDT()
    
# 브랜드 별 평균 상품 금액의 변동계수 생성
M_CV_BRA_BUY_AM <- ready_data %>%
  group_by(CLAC3_NM,PD_BRA_NM) %>% 
  summarise(BRA_mean = mean(PD_BUY_AM)) %>% 
  group_by(CLAC3_NM) %>%
  summarise(BRA_mean_cv = sd(BRA_mean)/mean(BRA_mean)) %>%
  setDT()

# 브랜드가 1개인 소분류상품 0으로 채우기
M_CV_BRA_BUY_AM[is.na(BRA_mean_cv)]$BRA_mean_cv <- 0

# 소분류 상품의 평균 판매 가격 생성
M_BUY_AM <- ready_data %>% 
  group_by(CLAC3_NM) %>% 
  summarise(PD_BUY_AM_mean = mean(PD_BUY_AM)) %>%
  setDT()

#소분류 상품 별 평균 페이지에 머무른시간
M_TIME_CLICK <- ready_data %>% 
  group_by(CLAC3_NM) %>% 
  summarise(TIME_CLICK_mean = mean(Time_Click)) %>%
  setDT()

#소분류의 WEEK별 판매량의 변동계수
ready_data <- merge(ready_data, set_date, by.x = 'SESS_DT', by.y = 'dt')

markov_table <- ready_data %>%
  select(CLAC3_NM,WEEK,PD_BUY_CT,CLNT_ID)

write.csv(markov_table, paste0(markov_variable_path,'markov_variable_',sdt,'_',edt,'_used.csv'), 
          fileEncoding = 'UTF-8', row.names = F)

CV_BUY_CT <- ready_data %>% 
  group_by(CLAC3_NM,WEEK) %>% 
  summarise(PD_BUY_CT_sum = sum(PD_BUY_CT)) %>% 
  group_by(CLAC3_NM) %>% 
  summarise(PD_BUY_CT_cv = sd(PD_BUY_CT_sum) / mean(PD_BUY_CT_sum)) %>%
  setDT()

CV_BUY_CT[is.na(PD_BUY_CT_cv)]$PD_BUY_CT_cv <- 0

# 파생변수 통합 및 Write
SOM_Variable <- M_TOT_SESS_HR %>%
  inner_join(M_TOT_PAG_VIEW_CT, by = 'CLAC3_NM') %>%
  inner_join(M_HIT_SEQ, by = 'CLAC3_NM') %>%
  inner_join(M_CV_BRA_BUY_AM, by = 'CLAC3_NM') %>%
  inner_join(M_BUY_AM, by = 'CLAC3_NM') %>%
  inner_join(M_TIME_CLICK, by = 'CLAC3_NM') %>%
  inner_join(CNT_BRA, by = 'CLAC3_NM') %>%
  inner_join(CV_BUY_CT, by = 'CLAC3_NM') %>%
  setDT()

write.csv(SOM_Variable, paste0(som_variable_path,som_variable_file_name), 
          fileEncoding = 'UTF-8', row.names = F)

som_clust_data <- som(as.matrix(scale(SOM_Variable[,-1])),
                      grid = somgrid(som_cluster, som_cluster, "rectangular"))

clust_som <- data.table(CLAC3_NM = SOM_Variable$CLAC3_NM,
                        class = som_clust_data$unit.classif)

write.csv(clust_som, paste0(som_class_table_path,som_class_table_name), 
          fileEncoding = 'UTF-8', row.names = F)
