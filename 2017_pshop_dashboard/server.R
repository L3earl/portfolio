
# global.R 은 여기에 자동으로 로드됨

shinyServer(function(input, output, session) {
        
        # 디버그 fun
        debug.fun <- function(state, step = 4, obj = NULL, msg = NULL, d.mode = debug.mode){
                if(d.mode == "off"){
                }else{
                        if(is.character(d.mode)){
                                if(state == d.mode){
                                        if(!is.null(msg)){
                                                print(paste(obj, msg, state, sep = "/"))
                                        }else{
                                                print(paste(obj, state, sep = "/"))
                                        }
                                        eval(parse(text = paste0("str(", obj, ")")))
                                }
                        }else if(step <= d.mode){
                                if(!is.null(msg)){
                                        print(paste(obj, msg, state, sep = "/"))
                                }else{
                                        print(paste(obj, state, sep = "/"))
                                }
                                eval(parse(text = paste0("str(", obj, ")")))
                        }
                }
        }
        
        
        # 사이드바 UI
        source('make_sidebar.R', local = TRUE, encoding = "UTF-8") # local = F 라면, shared environment 에 로드됨, 문제는 local에 만들어진 애는 shared에서 사용하지 못함
        
        # body UI
        source('make_body.R', local = TRUE, encoding = "UTF-8")
        
        # option UI
        source('make_optionUI.R', local = TRUE, encoding = "UTF-8")
        
        # 회원 그룹 수정 
        observeEvent(input$user.group.googleSheet.browse, {
                
                # 버튼 여러번 못 누르게 disable
                shinyjs::disable("user.group.googleSheet.browse")
                
                # progress bar UI
                progress.value$max <- 5 # 최대 progress
                progress <- Progress$new(max = progress.value$max)
                
                # 고정 유저 그룹 데이터 불러오기
                progress.value$now <- progress.value$now + 1
                progress$set(message = "유저 그룹 데이터 불러오는 중", value = progress.value$now)
                debug.fun(state = "progress", obj = "progress.value$now", step = 4)
                
                # pshop 유저 그룹 관리 파일 목록 가져옴
                user_group_gs_obj <- gs_ls(regex = "^pshopUserGroup")
                
                # 고정 유저 그룹 백업 생성
                progress.value$now <- progress.value$now + 1
                progress$set(message = "백업 생성 중", value = progress.value$now)
                debug.fun(state = "progress", obj = "progress.value$now", step = 4)
                
                # 백업을 만듬
                for(i in 1:nrow(user_group_gs_obj)){
                        sheet_data <- user_group_gs_obj$sheet_title[i] %>% 
                                gs_title() %>% 
                                gs_copy(to = paste0("백업_", user_group_gs_obj$sheet_title[i], "_",(Sys.time() + hours(9))))
                                
                }
                
                # 오래된 고정 유저 그룹 백업 삭제
                progress.value$now <- progress.value$now + 1
                progress$set(message = "오래된 백업 삭제 중", value = progress.value$now)
                debug.fun(state = "progress", obj = "progress.value$now", step = 4)
                
                # 유저 그룹 백업 파일을 불러옴
                user_group_gs_obj_backup <- gs_ls(regex = "^백업_pshopUserGroup_")
                
                if(!is.null(user_group_gs_obj_backup)){
                        # 만들어진지 한달 이상된 백업을 리스트화
                        del_list <- user_group_gs_obj_backup %>% 
                                filter(updated < Sys.Date() - months(1))  
                                
                        # 삭제
                        gs_vecdel(del_list$sheet_title)
                }
                
                # 구글 드라이브 관리 파일 폴더 열기
                progress.value$now <- progress.value$now + 1
                progress$set(message = "마무리 중", value = progress.value$now)
                debug.fun(state = "progress", obj = "progress.value$now", step = 4)
                
                # 분석이 끝난 후 작업
                shinyjs::enable("user.group.googleSheet.browse")
                progress$set(message = "서버 작업 완료", value = progress.value$max) # 분석의 마지막이므로 max value 사용
                debug.fun(state = "progress", obj = "progress.value$max", step = 4)
                
                progress.value$now <- 0 # 변수 초기화
                on.exit(progress$close()) # 여기까지 진행을 했는데, Error가 발생한다면 progress bar를 강제 종료
        })
        
        # 회원 그룹 새로고침
        observeEvent(input$user.group.googleSheet.load, {
                
                # 버튼 여러번 못 누르게 disable
                shinyjs::disable("user.group.googleSheet.load")
                
                # progress bar UI
                progress.value$max <- 3 # 최대 progress
                progress <- Progress$new(max = progress.value$max)
                
                # 고정 유저 그룹 데이터 불러오기
                progress.value$now <- progress.value$now + 1
                progress$set(message = "유저 그룹 데이터 불러오는 중", value = progress.value$now)
                debug.fun(state = "progress", obj = "progress.value$now", step = 4)
                
                # 에러 로그 초기화
                get.user.group.error.log <<- c() 
                
                # 구글 sheet에서 유저 그룹 가져옴
                user_group <- get.user.group()
                
                # 에러 로그 있으면 프린트 함
                if(length(get.user.group.error.log) > 0){
                        for(i in get.user.group.error.log){
                                showNotification(ui = i,
                                                 type = "error",
                                                 duration = 5)
                                
                                print(paste0("구글 sheet 에러 로그 ///", i))
                        }
                }
                
                # 데이터 합치기
                progress.value$now <- progress.value$now + 1
                progress$set(message = "데이터 합치는 중", value = progress.value$now)
                debug.fun(state = "progress", obj = "progress.value$now", step = 4)
                
                member_class <- read.csv(paste0(data.path, "member_class.csv"), fileEncoding = 'UTF-8') %>% 
                        full_join(user_group, by = "CUSTOMER_NUM") %>%setDT()
                member_class[, SITE_JOIN_DATE:= ymd(SITE_JOIN_DATE)]
                
                member_class <<- member_class
                
                # 분석이 끝난 후 작업
                shinyjs::enable("user.group.googleSheet.load")
                progress$set(message = "서버 작업 완료", value = progress.value$max) # 분석의 마지막이므로 max value 사용
                debug.fun(state = "progress", obj = "progress.value$max", step = 4)
                
                progress.value$now <- 0 # 변수 초기화
                on.exit(progress$close()) # 여기까지 진행을 했는데, Error가 발생한다면 progress bar를 강제 종료
        })
        
        # 분석 시작 버튼 누르면 순차적으로 발생
        observeEvent(input$start.button, {
                isolate({
                        
                # 시작 버튼 여러번 못 누르게 disable
                disable("start.button")
                
                # 디버그 fun
                debug.fun <- function(state, step = 4, obj = NULL, msg = NULL, d.mode = debug.mode){
                        if(d.mode == "off"){
                        }else{
                                if(is.character(d.mode)){
                                        if(state == d.mode){
                                                if(!is.null(msg)){
                                                        print(paste(obj, msg, state, sep = "/"))
                                                }else{
                                                        print(paste(obj, state, sep = "/"))
                                                }
                                                eval(parse(text = paste0("str(", obj, ")")))
                                        }
                                }else if(step <= d.mode){
                                        if(!is.null(msg)){
                                                print(paste(obj, msg, state, sep = "/"))
                                        }else{
                                                print(paste(obj, state, sep = "/"))
                                        }
                                        eval(parse(text = paste0("str(", obj, ")")))
                                }
                        }
                }
                
                # 분석 시작
                source('analysis_start.R', local = TRUE, encoding = "UTF-8")
                
                #
                source('UI_test.R', local = TRUE, encoding = "UTF-8")
                
                #
                source('data_filtering.R', local = TRUE, encoding = "UTF-8")
                
                #
                source('date_measure.R', local = TRUE, encoding = "UTF-8")
                
                #
                source('make_table.R', local = TRUE, encoding = "UTF-8")
                
                #
                source('facet.R', local = TRUE, encoding = "UTF-8")
                
                # 그래프 시각화
                source('graph.R', local = TRUE, encoding = "UTF-8")
                
                # 그래프를 이해 할 수 있는 데이터를 table로 보여줌
                source('table.R', local = TRUE, encoding = "UTF-8")
                
                # 분석이 끝난 후 작업
                enable("start.button") # 시작 버튼 살림
                progress$set(message = "서버 작업 완료", value = progress.value$max) # 분석의 마지막이므로 max value 사용
                debug.fun(state = "progress", obj = "progress.value$max", step = 4)
                
                progress.value$now <- 0 # 변수 초기화
                on.exit(progress$close()) # 여기까지 진행을 했는데, Error가 발생한다면 progress bar를 강제 종료
                })
        })
        
})


