tryCatch({
        if(tab$name == "basicProductSales"){
                # 기간 단위화
                progress.value$now <- progress.value$now + 1
                progress$set(message = "기간 단위를 나누는 중", value = progress.value$now)
                debug.fun(state = "progress", obj = "progress.value$now", step = 4)
                
                # 안 쓰는 칼럼 삭제
                if(input$date.unit.input != 7){
                        temp <- con.data %>% 
                                select(-ORD_TIME) %>% 
                                setDT()
                        
                }else{
                        temp <- con.data %>% 
                                select(-ORD_DT) %>% 
                                setDT()
                }
                
                # 선택된 단위에 맞춰서 기간을 단위로 만듬
                switch(input$date.unit.input,
                       "1" = { # 일
                               temp <- temp %>% 
                                       rename(date = ORD_DT) %>% 
                                       setDT()
                       },
                       '2' = { # 주
                               start_wday <- temp %>% 
                                       select(ORD_DT) %>% 
                                       arrange(ORD_DT) %>% 
                                       slice(1)
                               
                               start_wday <- start_wday$ORD_DT[1]
                               
                               temp <- temp %>% 
                                       mutate(ORD_DT = floor_date(ORD_DT, unit = "week", week_start = wday(start_wday) - 1)) %>% 
                                       rename(date = ORD_DT) %>% 
                                       setDT()
                       },
                       '3' = { # 월
                               temp <- temp %>% 
                                       mutate(ORD_DT = floor_date(ORD_DT, unit = "month")) %>% 
                                       rename(date = ORD_DT) %>% 
                                       setDT()
                       },
                       '4' = { # 분기
                               temp <- temp %>% 
                                       mutate(ORD_DT = floor_date(ORD_DT, unit = "quarter")) %>% 
                                       rename(date = ORD_DT) %>% 
                                       setDT()
                       },
                       '5' = { # 년
                               temp <- temp %>% 
                                       mutate(ORD_DT = floor_date(ORD_DT, unit = "year")) %>% 
                                       rename(date = ORD_DT) %>% 
                                       setDT()
                       },
                       '6' = { # 요일
                               temp <- temp %>% 
                                       mutate(ORD_DT = wday(ORD_DT, label = TRUE)) %>% 
                                       rename(date = ORD_DT) %>% 
                                       setDT()
                       },
                       '7' = { # 시간
                               temp <- temp %>% 
                                       mutate(ORD_TIME = hour(strptime(ORD_TIME, format = "%k"))) %>% 
                                       rename(date = ORD_TIME) %>% 
                                       setDT()
                       }
                )
                
                debug.fun(state = "data_measur", obj = "temp", msg = "기간 단위화", step = 2)
                
                # 클래스 부여 (class 부여는 최대한 뒤에서 하는게 계산 양을 줄일 수 있지만..)
                con.data.class <- temp %>% 
                        inner_join(con.goods.class, by = "GOODS_NO") %>% 
                        inner_join(con.member.class, by = "CUSTOMER_NUM") %>% 
                        setDT()
                
                debug.fun(state = "data_measur", obj = "con.data.class", msg = "class 부여", step = 2)
        }
},
error = function(e) print(paste0("data_measur /// ", e)),
#warning = function(w) print(w),
finally = NULL)


