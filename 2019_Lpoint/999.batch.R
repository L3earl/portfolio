# 실제 batch 프로그램에 쓰이는 파일은 아님
# 각 모델의 최적화를 위해 파라미터를 바꿔가며 자동적으로 실행하는 파일
source('000.global.R', encoding = 'UTF-8')

### 반복 실행 부분 (아리마)

# 세팅된 파라미터 불러오기
preprocessing_para_dt <- fread("preprocessing_para.csv", encoding = 'UTF-8') %>% 
  arrange(priority) %>% 
  setDT()

# 실행
p1 <- proc.time() # 시간 재는 함수

batch_num <- 1

batch_id <- preprocessing_para_dt %>% 
  filter(priority != 999) %>% # 완료되면 999 넣음
  filter(!is.na(ap_sup)) %>% 
  select(id)

batch_id <- batch_id[,]

batch_end <- length(batch_id)

for(batch_i in 1:length(batch_id)){
    if(batch_end == 0){
        print(paste0('실행할 시나리오가 없습니다. para.csv 파일을 확인하세요.'))
        break;
    }
    
    
    # 실행하려는 파라미터 가져옴
    preprocessing_para <- preprocessing_para_dt[id == batch_id[batch_i],]
    
    print(paste0(batch_i, '번째 배치 실행중입니다. STEP : ', preprocessing_para$step))
    
    # train 데이터 기간의 시작 ~ 끝
    s_dt <- preprocessing_para[,train_st_dt]
    e_dt <- preprocessing_para[,train_end_dt]
    
    sdt <- gsub('-', '', preprocessing_para[,train_st_dt])
    edt <- gsub('-', '', preprocessing_para[,train_end_dt])
    
    all_s_dt <- as.Date('2018-04-01')
    all_e_dt <- as.Date('2018-09-30')
    
    all_sdt <- '20180401'
    all_edt <- '20180930'
    
    # STEP 선택
    if(preprocessing_para$step == 'preprocessing'){
      
    ## apriori, 키워드 - 구매 매핑 데이터 생성
    # 파라미터 세팅 
    ap_supp <- preprocessing_para[,ap_sup]
    ap_con <- preprocessing_para[,ap_con]
    ap_min <- preprocessing_para[,ap_min]
    ap_max <- preprocessing_para[,ap_max]
    
    # 파일명 생성
    kwd_mapping_file_name <- paste0('kwd_map_', sdt, '_', edt, '_',
                                    ap_supp, '_', ap_con, '_',
                                    ap_min, '_', ap_max, '.csv')
    
    # 키워드 매핑 데이터가 있는지 확인하여, 없으면 키워드 매핑 만드는 코드 실행
    if(preprocessing_para$kwd_map == 0){
        if(!file.exists(paste0(kwd_mapping_file_path, kwd_mapping_file_name))){
            print(paste0('101 코드 실행'))
            source('./101.apriori_kwd_buy_mapping.R', encoding = 'UTF-8')
        }
        preprocessing_para$kwd_map <- 1
    }
    
    ## 네이버 키워드 데이터 크롤링
    # 파라미터 세팅
    cr_timeunit <- preprocessing_para[,cr_timeunit]
    
    # 파일명 생성 
    naver_all_kwd_name <- paste0('naver_dlab_', cr_timeunit, '.csv')
    naver_all_kwd_yn <- paste0('naver_dlab_', cr_timeunit, '_yn.csv')
    
    naver_file_name <- paste0('naver_dlab_', sdt, '_', edt, '_',
                              cr_timeunit, '.csv')
    
    # 네이버 크롤링 데이터가 완료 안되었다면, 크롤링 소스 실행
    if(preprocessing_para$naver_dlab == 0){
      print(paste0('102 코드 실행'))
      source('./102.naver_dlab.R', encoding = 'UTF-8')
      preprocessing_para$naver_dlab <- 1
      
      # 네이버 API 호출횟수 제한에 걸려서 크롤링이 안되는 상황이라면 그냥 넘어감
    }
    
    
    ## tf_idf 데이터 생성
    # 파라미터 세팅
    tf_phase <- preprocessing_para[,tf_phase]
    
    # 파일명 생성
    tf_idf_file_name <- paste0('tf_idf_', sdt, '_', edt, '_',
                               tf_phase, '.csv')
    
    # tf 최종파일이 없고, 실행에 필요한 조건을 만족하면 실행
    if(preprocessing_para$tf_idf == 0){
        if(!file.exists(paste0(tf_idf_file_path, tf_idf_file_name))){
            if(preprocessing_para$naver_dlab == 1 && preprocessing_para$kwd_map == 1){
                print(paste0('103 코드 실행'))
                source('./103.tf_idf.R', encoding = 'UTF-8')
                preprocessing_para$tf_idf <- 1
            }
        }else{
            preprocessing_para$tf_idf <- 1
        }
    }
    
    ## 모든 파일이 생성된 후라면 merge 실행
    # 파일명 생성
    kwd_tf_merge_file_name <- paste0('kwd_tf_', sdt, '_', edt, '_',
                                     ap_supp, '_', ap_con, '_',
                                     ap_min, '_', ap_max, '_',
                                     cr_timeunit, '_',  tf_phase, '.csv')
    
    if(preprocessing_para$tf_idf == 1){
        print(paste0('104 코드 실행'))
        source('104.apriori_tf_mapping.R', encoding = 'UTF-8')
        wpreprocessing_para$priority <- 999
    }
    
    
    # 파라미터 세팅
    som_cluster <- preprocessing_para[,som_cluster]

    som_variable_file_name <- paste0('som_variable_', som_cluster,'x',som_cluster, '_',
                                     sdt, '_', edt, '_used.csv')
    som_class_table_name <- paste0('som_class_table_',som_cluster,'x',som_cluster,'_',
                                   sdt,'_',edt,'_used.csv')
    # Som 알고리즘을 활용한 군집 생성
    if(preprocessing_para$som == 0){
      if(!file.exists(paste0(som_variable_path, som_variable_file_name))){
        if(!file.exists(paste0(som_class_table_path, som_class_table_name))){
        print(paste0('201 코드 실행'))
        source('./201.derived_variable_som.R', encoding = 'UTF-8')
        }
      }
      preprocessing_para$som <- 1
    }
    } else {
      
      # 파라미터 세팅 
      ap_supp <- preprocessing_para[,ap_sup]
      ap_con <- preprocessing_para[,ap_con]
      ap_min <- preprocessing_para[,ap_min]
      ap_max <- preprocessing_para[,ap_max]
      cr_timeunit <- preprocessing_para[,cr_timeunit]
      model_name <- preprocessing_para[,model]
      tf_phase <- preprocessing_para[,tf_phase]
      som_cluster <- preprocessing_para[,som_cluster]
      
      # File Path 세팅
      kwd_tf_merge_file_name <- paste0('kwd_tf_', sdt, '_', edt, '_',
                                       ap_supp, '_', ap_con, '_',
                                       ap_min, '_', ap_max, '_',
                                       cr_timeunit, '_',  tf_phase, '.csv')
      markov_variable_file_name <- paste0('markov_variable_',sdt,'_',edt,'_used.csv')
      som_class_table_name <- paste0('som_class_table_',som_cluster,'x',som_cluster,'_',
                                     sdt,'_',edt,'_used.csv')
      
      # 예측 시
      if(preprocessing_para$predict == 0){
        source('301.predict_go.R', encoding = 'UTF-8')
      }
      preprocessing_para$predict <- 1
    }
    
    # 전체 실행 완료되면 최종파일 생성 여부 업데이트 후, batch 숫자 증가
    preprocessing_para_dt[id == preprocessing_para$id,] <- preprocessing_para
    write.csv(preprocessing_para_dt, "preprocessing_para.csv", 
              fileEncoding = 'UTF-8', row.names = FALSE)
    
    batch_num <- batch_num + 1
}



proc.time() - p1 # 시간 재는 함수    

