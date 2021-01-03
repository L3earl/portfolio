### 공통 함수


# 현재 load된 라이브러리 모두 unload
detachAllPackages <- function(keep = NULL, keep.basic = TRUE) {
    # function for detaching all attached packages (except basic ones)
    basic.packages <- c("package:stats","package:graphics","package:grDevices",
                        "package:utils","package:datasets","package:methods",
                        "package:base")
    package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1,
                                    TRUE, FALSE)]
    if (!is.null(keep)){
        package.list <- setdiff(package.list, paste("package", keep, sep = ":"))
    }
    if (keep.basic){
        package.list <- setdiff(package.list, basic.packages)
    }
    if (length(package.list) > 0) {
        for (package in package.list) detach(package, character.only = TRUE)
    }
}

# 사용하는 패키지 이름을 입력하면, 설치 안된 것은 설치하고, 업데이트를 원하면 업데이트함
check.package <- function(packageNames, updatePackage = FALSE){
    pn <- packageNames
    num <- length(pn)
    
    # 입력된 패키지를 install 된 것과 안된 것으로 구분
    installed <- pn[(as.character(pn) %in% rownames(installed.packages()))]
    notInstalled <- pn[!(as.character(pn) %in% rownames(installed.packages()))]
    
    # install 안된 애들은 설치
    if(length(notInstalled) != 0){
        for(i in notInstalled){
            install.packages(i)
        }
    }
    
    # install 된 애들은 원하면, 최신 버전으로 업데이트함
    if(updatePackage == TRUE){
        for(i in installed){
            update.packages(i)
        }
    }
    
    for(i in 1:num){
        temp.text <- paste0('library(', pn[i], ')')
        eval(parse(text=temp.text))
    }
}

# 해당 위치에 폴더 없을 시 생성하는 함수
check.dir <- function(dir.path) {
    dp <- dir.path
    num <- length(dp)
    
    for(i in dp){
        if(!dir.exists(i)){
            print(paste0(i, ' 폴더를 생성합니다.'))
            dir.create(i)
        }else{
            next
        }
    }
}

# Markov chain 예측 모델 함수
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

# option
options(scipen = 100)

# 현재 load된 라이브러리 모두 unload
detachAllPackages()

# 사용하는 패키지 전체
packageNames <- c('dplyr', 'data.table', 'arules', 'forecast', 'tidyr', 'httr', 'jsonlite','kohonen','gdata')

# 사용하는 패키지 이름을 입력하면, 설치 안된 것은 설치하고, 업데이트를 원하면 업데이트함
check.package(packageNames, FALSE)


### 공통적으로 쓰이거나, 변경되면 안되는 파일 위치
# 월~일 을 한주로 보는 2018년도 날짜
set_date <- data.table(Date = seq(from = as.Date('2018-01-01'),
                                  to = as.Date('2018-12-31'),
                                  by = 'day')) %>% 
    mutate(dt = gsub("-", "", Date), WEEK = ifelse(ceiling(row_number()/7) < 10,
                                                   paste0('20180', ceiling(row_number()/7)),
                                                   paste0('2018', ceiling(row_number()/7))
                                                   )) %>% 
    setDT()

# 네이버 개발자 계정 정보 세팅 (파일 제출전 삭제할 것)
# 이얼, 이재륜, 김태훈, 황현식, 김민수
nid <- c()
npw <- c()


# 파일 저장되는 상위 폴더들
raw_data_path <- './raw_data/' # L.point 에서 준 파일 
data_path <- './preprocessed_data/' # L.point에서 준 파일을 전처리한 데이터
kwd_mapping_file_path <- './kwd_mapping_table/' # 연관규칙분석으로 만들어지는 키워드 매핑 데이터
kwd_mapping_daily_file_path <- './kwd_mapping_table/kwd_mapping_daily_table/' # 연관규칙분석으로 만들어지는 키워드 매핑 데이터
naver_file_path <- './naver_dlab/' # 네이버 데이터랩 크롤링한 데이터
tf_idf_file_path <- './TF_IDF_Search/' # TF_IDF 로 만들어진 데이터셋 
tf_idf_daily_file_path <- './TF_IDF_Search/TF_IDF_daily_Search/' # TF_IDF 로 만들어진 데이터셋 
kwd_tf_merge_file_path <- './kwd_tf_merge/'# 키워드 - tf 매핑 데이터셋
som_variable_path <- './som_variable/'# Som 알고리즘 적용을 위한 데이터셋
markov_variable_path <- './markov_variable/'# markov 알고리즘 적용을 위한 데이터셋
som_class_table_path <- './som_class_table/'# som 알고리즘 군집 결과 파일
predict_result_path <- './predict_result/'
summary_result_path <- './summary_result/'
our_result_path <- './our_result_path/'

# 위에 필요한 폴더가 없다면 생성
folder_name <- c(raw_data_path, data_path, kwd_mapping_file_path, naver_file_path,
                 tf_idf_file_path, kwd_tf_merge_file_path, som_variable_path, markov_variable_path,
                 kwd_mapping_daily_file_path, tf_idf_daily_file_path, som_class_table_path,predict_result_path,
                 summary_result_path, our_result_path)
check.dir(folder_name)

