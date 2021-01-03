source('000.global.R', encoding = 'UTF-8')

## 크롤링 필요한 키워드만 남기기
# 크롤링 여부 파일 불러오기, 없으면 생성 (전체 키워드에 크롤링 여부 붙혀서 저장)
if(file.exists(paste0(naver_file_path, naver_all_kwd_yn))){
  all_kwd_yn <- fread(paste0(naver_file_path, naver_all_kwd_yn), encoding = 'UTF-8') %>% 
    setDT()
  
}else{
  search1 <- fread(paste0(data_path,'search1.csv'), encoding = 'UTF-8')
  
  all_kwd_yn <- search1 %>% 
    select(KWD_NM) %>% 
    distinct() %>%
    mutate(yn = 0) %>% 
    setDT()
  
  write.csv(all_kwd_yn, paste0(naver_file_path, naver_all_kwd_yn), 
            fileEncoding = 'UTF-8', row.names = FALSE)
}

# 크롤링 필요한 키워드 필터링
crawing_kwd <- fread(paste0(kwd_mapping_file_path, kwd_mapping_file_name), encoding = 'UTF-8') %>% 
  select(KWD_NM) %>% 
  distinct() %>% 
  left_join(all_kwd_yn, by = 'KWD_NM') %>% 
  filter(is.na(yn) | yn == 0) %>% 
  setDT()

### 네이버 데이터랩 크롤링
# 기간 all_sdt ~ all_edt
# 파라미터 cr_timeunit

# 결과 테이블 frame 생성 (날짜 전체 데이터로 칼럼 생성)
# colname의 시작이 숫자인채로 csv로 저장하면 불러올때 변형되므로 앞부분에 문자를 붙혀줌
result <- set_date %>% 
    filter(dt >= all_sdt, dt <= all_edt) %>% 
    select(dt) %>% 
    mutate(dt = paste0("dt_", dt), srch_kwd = "tmp",  tmp = 0) %>% 
    spread(dt, tmp)

## 네이버 데이터랩 통합검색어 트랜드 크롤링
# 초기 세팅
i <- 1
idnum <- 1 # 사용할 ID 순서
loop.end <- nrow(crawing_kwd) # 반복문 돌아야 할 숫자

# 크롤링 및 데이터 적재 
while(i <= loop.end){
    # 어디돌고 있나 확인
    print(paste0(crawing_kwd$KWD_NM[i], ' // ', round(i/loop.end*100,2) , '%'))
    
    # 검색에 사용할 id,pw 세팅
    use_id <- nid[idnum]
    use_pw <- npw[idnum]
    
    # 검색할 키워드
    srch_kwd <- crawing_kwd$KWD_NM[i]
    
    # 검색 설정
    json_qr <- paste0('{\"startDate\":\"', all_s_dt,
                      '\",\"endDate\":\"', all_e_dt,
                      '\",\"timeUnit\":\"', cr_timeunit,
                      '\",\"keywordGroups\":[{\"groupName\":\"', srch_kwd,
                      '\",\"keywords\":[\"', srch_kwd, '\"]}]}')
    
    # API 호출
    qr <- POST("https://openapi.naver.com/v1/datalab/search", 
               add_headers('X-Naver-Client-Id' = use_id,
                           'X-Naver-Client-Secret' = use_pw,
                           'Content-Type' = 'application/json'),
               body = json_qr
    )
    
    # id의 API 호출 횟수가 한계에 도달할 경우
    if(qr$status_code == 429){
        # 다음 id 사용
        print(paste0('API 호출 횟수 한계 : ', qr$status_code))
        idnum <- idnum + 1
        
        # 사용 가능한 id 없으면 루프 종료
        if(idnum > length(nid)){
            print(paste0('API 호출 가능한 ID 없음'))
            break;
        }
        next;
        
    }else if(qr$status_code == 200){ # 결과가 정상일 경우, 데이터 변환하여 저장
        # 결과값 받아와서 테이터 테이블화
        respond.JSON <- content(qr, "text")
        respond <- jsonlite::fromJSON(respond.JSON)
        respond.body <- respond$results$data[[1]]
        
        # 결과값이 null 인 경우 처리
        if(length(respond.body) == 0){ 
            respond.body <- data.frame(period = c(all_s_dt, all_e_dt),
                                       ratio = c(0,0))
        }
        
        # 결과 테이블 형태에 맞게 변환
        respond.dt <- respond.body %>% 
            mutate(period = paste0("dt_", format(as.Date(period), '%Y%m%d'))) %>% 
            spread(period, ratio) %>% 
            mutate(srch_kwd = srch_kwd) %>% 
            setDT()
        
        result <- rbind(result, respond.dt, fill = TRUE)
        
    }else{ # 그외는 루프 종료
        print(paste0('Error : ', qr$status_code))
        break;
    }
    
    # 시간 break (네이버 API는 1초에 10회이상 호출 제한이 있으므로, 시간차를 둠)
    Sys.sleep(1)
    
    # loop 숫자 증가
    i <- i + 1
}

# 검색기록이 없는 날의 데이터가 NA로 들어오므로, 0으로 변경
result[is.na(result)] <- 0

# 검색 세팅 내용 붙혀주기 & 테이블 생성을 위해 넣은 tmp 키워드 삭제
result <- result %>% 
    filter(srch_kwd != 'tmp') %>% 
    mutate(startDate = all_s_dt,
           endDate = all_e_dt,
           timeunit = cr_timeunit) %>% 
    setDT()

# 예전에 같은 기준으로 크롤링한 데이터가 있다면 불러와서 합침
if(file.exists(paste0(naver_file_path, naver_all_kwd_name))){
  crawing_kwd_before <- fread(paste0(naver_file_path, naver_all_kwd_name), 
                                 encoding = 'UTF-8', stringsAsFactors = F) %>% 
    setDT()
  
  crawing_kwd_result <- crawing_kwd_before %>% 
    rbind(result, fill = T) %>% 
    distinct() %>% 
    setDT()
}
d <- fread('./naver_dlab/naver_dlab_20180401_20180819_date.csv', encoding = 'UTF-8')

# 저장
write.csv(crawing_kwd_result, paste0(naver_file_path, naver_all_kwd_name), 
          fileEncoding = 'UTF-8', row.names = FALSE)

# 크롤링 여부 업데이트
all_kwd_yn <- all_kwd_yn %>% 
  mutate(yn = ifelse(KWD_NM %in% crawing_kwd_result$srch_kwd, 1, 0)) %>% 
  setDT()

write.csv(all_kwd_yn, paste0(naver_file_path, naver_all_kwd_yn), 
          fileEncoding = 'UTF-8', row.names = FALSE)


# 필요한 날짜 데이터만 남김
result_filter <- crawing_kwd_result %>% 
  select(-startDate, -endDate, -timeunit) %>% 
  gather(key = SESS_DT, value = SEARCH_CNT, -srch_kwd) %>% 
  mutate(SESS_DT = gsub("dt_", "", SESS_DT)) %>% 
  rename(KWD_NM = srch_kwd) %>% 
  filter(SESS_DT >= sdt, SESS_DT <= edt) %>% 
  setDT()

# 비율데이터이므로 데이터를 0 ~ 100 사이로 정규화, 0이 안 나오도록 분자에 작은 수 추가
result_norm <- result_filter %>%
  group_by(KWD_NM) %>% 
  mutate(min_score = min(SEARCH_CNT)) %>% 
  mutate(score = ifelse(min_score == 0, 0, SEARCH_CNT/min(SEARCH_CNT))) %>% 
  mutate(SEARCH_CNT_nv = ((score-min(score) + 0.00001)/(max(score)-min(score) + 0.00001)*100)) %>% 
  setDT()

# 저장
write.csv(result_norm, paste0(naver_file_path ,naver_file_name), 
          fileEncoding = 'UTF-8', row.names = FALSE)
