source('000.global.R', encoding = 'UTF-8')

### 받은 데이터 전처리

# 데이터 불러오기
product <- fread(paste0(raw_data_path,'제5회 Big Data Competition-분석용데이터-01.Pruduct.csv'), encoding = 'UTF-8')
search1 <- fread(paste0(raw_data_path,'제5회 Big Data Competition-분석용데이터-02.Search1.csv'), encoding = 'UTF-8')
search2 <- fread(paste0(raw_data_path,'제5회 Big Data Competition-분석용데이터-03.Search2.csv'), encoding = 'UTF-8')
custom <- fread(paste0(raw_data_path,'제5회 Big Data Competition-분석용데이터-04.Custom.csv'), encoding = 'UTF-8')
session <- fread(paste0(raw_data_path,'제5회 Big Data Competition-분석용데이터-05.Session.csv'), encoding = 'UTF-8')
master <- fread(paste0(raw_data_path,'제5회 Big Data Competition-분석용데이터-06.Master.csv'), encoding = 'UTF-8')

# 검색어 전처리, 공백제거/ 소문자화
search1 <- search1 %>% 
    mutate(KWD_NM = tolower(gsub(" ", "", KWD_NM))) %>% 
    setDT()

search2 <- search2 %>% 
    mutate(KWD_NM = tolower(gsub(" ", "", KWD_NM))) %>% 
    setDT()

# 검색어 개수 숫자형으로 변경
search2 <- search2 %>% 
    mutate(SEARCH_CNT = as.numeric(gsub(',','',SEARCH_CNT))) %>% 
    setDT()

# 세션당 페이지에 머무른 시간 숫자형으로 변경
session <- session %>%
    mutate(TOT_SESS_HR_V = as.numeric(gsub(',','',TOT_SESS_HR_V))) %>%
    setDT

# 상품 구매 금액 숫자형으로 변경
product <- product %>% 
    mutate(PD_BUY_AM = as.numeric(gsub(',','',PD_BUY_AM))) %>% 
    setDT()

# 상품 구매 개수 숫자형으로 변경
product <- product %>% 
    mutate(PD_BUY_CT = as.numeric(gsub(',','',PD_BUY_CT))) %>% 
    setDT()

# 브랜드 이름 문자열 정제
product$PD_BRA_NM <- gsub('\\[', '', product$PD_BRA_NM)
product$PD_BRA_NM <- gsub('\\]', '', product$PD_BRA_NM)
product$PD_BRA_NM <- gsub(' ', '', product$PD_BRA_NM)
product$PD_BRA_NM <- tolower(product$PD_BRA_NM)
product$PD_BRA_NM <- sapply(product$PD_BRA_NM, function(x) unlist(strsplit(x,split = '\\('))[1])
product$PD_BRA_NM <- gsub('정품','', product$PD_BRA_NM)
product$PD_BRA_NM <- gsub('[[:punct:]]','', product$PD_BRA_NM)

# 데이터 저장
write.csv(product, paste0(data_path, 'product.csv'), row.names = F, fileEncoding = 'UTF-8')
write.csv(search1, paste0(data_path, 'search1.csv'), row.names = F, fileEncoding = 'UTF-8')
write.csv(search2, paste0(data_path, 'search2.csv'), row.names = F, fileEncoding = 'UTF-8')
write.csv(custom, paste0(data_path, 'custom.csv'), row.names = F, fileEncoding = 'UTF-8')
write.csv(session, paste0(data_path, 'session.csv'), row.names = F, fileEncoding = 'UTF-8')
write.csv(master, paste0(data_path, 'master.csv'), row.names = F, fileEncoding = 'UTF-8')