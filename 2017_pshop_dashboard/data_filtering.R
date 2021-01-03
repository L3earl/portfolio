tryCatch({
        if(tab$name == "basicProductSales"){
                #### UI 선택에 의해 변하는 공통 데이터 필터링
                ### 선택된 유저 분류
                progress.value$now <- progress.value$now + 1
                progress$set(message = "유저 데이터 필터링", value = progress.value$now)
                debug.fun(state = "progress", obj = "progress.value$now", step = 4)
                
                if(input$user.type.input == 1){ # 전체
                        con.member.class <- member_class %>% 
                                distinct(CUSTOMER_NUM) %>% 
                                setDT()
                        
                }else if(input$user.type.input == 2){ # 회원
                        con.member.class <- member_class %>% 
                                filter(C.type.class == "회원") %>% 
                                setDT()
                        
                        if(!is.null(input$user.input) | !is.null(input$user.select.input)){ # 분류가 1개 이상 선택되거나, 개별 선택이 된 경우
                                if(1 %in% input$user.input){ # 회원 그룹이 선택됨
                                        con.member.class <- con.member.class %>% 
                                                filter(C.group.class %in% c(input$user.group.box.input)) %>% 
                                                setDT()
                                }
                                
                                if(2 %in% input$user.input){ # 회원 등급이 선택됨
                                        con.member.class <- con.member.class %>% 
                                                filter(C.grade.class %in% c(input$user.grade.box.input)) %>% 
                                                setDT()
                                }
                                
                                if(3 %in% input$user.input){ # 회원 연령이 선택됨
                                        con.member.class <- con.member.class %>% 
                                                filter(C.age.class %in% c(input$user.age.box.input)) %>% 
                                                setDT()
                                }
                                
                                if(!is.null(input$user.select.input)){ # 개별 회원이 선택됨
                                        con.member.class <- con.member.class %>% 
                                                filter(CUSTOMER_NUM %in% c(input$user.select.input)) %>% 
                                                setDT()
                                }
                        }else{ # 분류가 하나도 안 선택되면 class column 다 버림 
                                con.member.class <- con.member.class %>% 
                                        distinct(CUSTOMER_NUM) %>% 
                                        setDT()
                        }
                        
                }else if(input$user.type.input == 3){ # 임직원
                        con.member.class <- member_class %>% 
                                filter(C.type.class == "임직원") %>% 
                                setDT()
                        
                }else if(input$user.type.input == 4){ # 비회원
                        con.member.class <- member_class %>% 
                                filter(C.type.class == "비회원") %>% 
                                setDT()
                        
                }else if(input$user.type.input == 5){ # 비회원 + 회원
                        con.member.class <- member_class %>% 
                                filter(C.type.class %in% c("회원", "비회원")) %>% 
                                setDT()
                }
                
                debug.fun(state = "data_filtering", obj = "con.member.class", msg = "선택된 유저 분류", step = 2)
                
                # 선택된 유저 분류에 따라 멤버를 필터링하여 작게 만듬 
                con.member <- con.member.class %>% 
                        distinct(CUSTOMER_NUM) %>% # 선택된 유저 분류(그룹) 중에 한 유저가 여러 class를 가질 수 있으므로, 중복되는 유저를 없앰
                        inner_join(member_entire, by = "CUSTOMER_NUM") %>% # 선택한 유저 분류만 필터링
                        select(CUSTOMER_NUM) %>% 
                        setDT()
                
                debug.fun(state = "data_filtering", obj = "con.member", msg = "선택된 유저", step = 2)
                
                ### 선택된 상품 분류 and 조건으로 필터링
                progress.value$now <- progress.value$now + 1
                progress$set(message = "상품 데이터 필터링", value = progress.value$now)
                debug.fun(state = "progress", obj = "progress.value$now", step = 4)
                
                #
                con.goods.class <- product_class %>% 
                        setDT()
                
                debug.fun(state = "data_filtering", obj = "product_class", msg = "raw 상품 분류", step = 3)
                
                # 상품 분류, 상품 검색이 선택 안 되었을때, 전체 선택, 상품 분류 column 다 버림
                if(is.null(input$category.input) & is.null(input$goods.select.input)){
                        con.goods.class <- con.goods.class %>% 
                                distinct(GOODS_NO) %>% 
                                setDT()
                }
                
                # 상품 분류가 1개라도 선택되었다면, 선택된 분류로 필터링, and 조건이므로 매 경우를 따로 필터링함
                if(!is.null(input$category.input)){
                        
                        if(2 %in% input$category.input){ # 채널이 선택됨
                                con.goods.class <- con.goods.class %>% 
                                        filter(G.channel.class %in% c(input$channal.box.input)) %>% 
                                        setDT()
                        }
                        
                        if(3 %in% input$category.input){ # 카테고리가 선택됨
                                con.goods.class <- con.goods.class %>% 
                                        filter(G.category.class %in% c(input$category.box.input)) %>% 
                                        setDT()
                        }
                        
                        if(4 %in% input$category.input){ # POV 카테고리가 선택됨 (사용 안함)
                                con.goods.class <- con.goods.class %>% 
                                        filter(G.PovCategory.class %in% c(input$category.pov.box.input)) %>% 
                                        setDT()
                        }
                        
                        if(5 %in% input$category.input){ # 브랜드가 선택됨
                                con.goods.class <- con.goods.class %>% 
                                        filter(G.brand.class %in% c(input$brand.box.input)) %>% 
                                        setDT()
                        }
                }
                
                # 삼품 검색에서 선택된 것이 있다면, 선택된 상품으로 필터링, and 조건이므로 상품 분류로 필터링 된 곳에서 이어서 필터링
                if(!is.null(input$goods.select.input)){
                        temp.goods.select <- goods %>% 
                                filter(GOODS_NM %in% c(input$goods.select.input)) %>% 
                                select(GOODS_NO) %>% 
                                setDT()
                        
                        con.goods.class <- con.goods.class %>% 
                                filter(GOODS_NO %in% c(temp.goods.select$GOODS_NO)) %>% 
                                setDT()
                }
                
                debug.fun(state = "data_filtering", obj = "con.goods.class", msg = "선택된 상품 분류", step = 2)
                
                ### 선택된 상품 분류에 따라 상품을 필터링하여 작게 만듬
                con.goods <- con.goods.class %>% 
                        distinct(GOODS_NO) %>% # 선택된 상품 분류(그룹) 중에 한 상품가 여러 class를 가질 수 있으므로, 중복되는 상품을 없앰
                        inner_join(goods, by = "GOODS_NO") %>% # 선택한 상품 분류만 필터링
                        select(GOODS_NO, GOODS_NM) %>% 
                        setDT()
                
                debug.fun(state = "data_filtering", obj = "con.goods", msg = "선택된 상품", step = 2)
                
                ### order 정보를 선택된 유저(전체/회원/임직원/비회원)에 따라 필터링
                progress.value$now <- progress.value$now + 1
                progress$set(message = "주문 데이터 필터링", value = progress.value$now)
                debug.fun(state = "progress", obj = "progress.value$now", step = 4)
                
                con.order <- order %>% 
                        semi_join(con.member, by = "CUSTOMER_NUM") %>%
                        #select(CUSTOMER_NUM, ORD_NO, ORD_DT, ORD_TIME) %>%  #########################
                        setDT()
                
                debug.fun(state = "data_filtering", obj = "con.order", msg = "raw 주문", step = 3)
                
                ### con.order 정보를 선택된 기간에 따라 필터링 + 나중에 쓸 날짜 클래스 부여
                for(i in 1:input$date.number.input){ 
                        temp.txt <- paste0("v.date.range <- input$date.select.input.0", i)
                        eval(parse(text = temp.txt))
                        
                        temp.txt <- paste0("v.date.range.min <- input$date.range.input.0", i, "[1]")
                        eval(parse(text = temp.txt))
                        
                        temp.txt <- paste0("v.date.range.max <- input$date.range.input.0", i, "[2]")
                        eval(parse(text = temp.txt))
                        
                        temp <- con.order %>% 
                        {if(v.date.range == 1){
                                filter(., ORD_DT %between% c(last.batch.date - weeks(1) + days(1), last.batch.date)) %>% 
                                        mutate(date.class = "1주")
                        }else if(v.date.range == 2){
                                filter(., ORD_DT %between% c(last.batch.date - weeks(2) + days(1), last.batch.date)) %>% 
                                        mutate(date.class = "2주")
                        }else if(v.date.range == 3){
                                filter(., ORD_DT %between% c(last.batch.date - weeks(3) + days(1), last.batch.date)) %>% 
                                        mutate(date.class = "3주")
                        }else if(v.date.range == 4){
                                filter(., ORD_DT %between% c(last.batch.date - months(1), last.batch.date)) %>% 
                                        mutate(date.class = "1달")
                        }else if(v.date.range == 5){
                                filter(., ORD_DT %between% c(last.batch.date - months(3), last.batch.date)) %>% 
                                        mutate(date.class = "3달")
                        }else if(v.date.range == 6){
                                filter(., ORD_DT %between% c(last.batch.date - months(6), last.batch.date)) %>% 
                                        mutate(date.class = "반년")
                        }else if(v.date.range == 7){
                                filter(., ORD_DT %between% c(last.batch.date - years(1), last.batch.date)) %>% 
                                        mutate(date.class = "1년")
                        }else if(v.date.range == 8){
                                filter(., ORD_DT %between% c(last.batch.date - years(2), last.batch.date)) %>% 
                                        mutate(date.class = "2년")
                        }else if(v.date.range == 9){
                                filter(., ORD_DT %between% c(v.date.range.min, v.date.range.max)) %>% 
                                        mutate(date.class = paste0(format(as.Date(v.date.range.min), format="%m-%d"),
                                                                   " ~ ",
                                                                   format(as.Date(v.date.range.max), format="%m-%d")))
                        }} %>% setDT()
                        
                        if(i == 1){
                                temp.2 <- temp
                        }else{
                                temp.2 <- temp.2 %>% 
                                        bind_rows(temp) %>% 
                                        setDT()
                        }
                }
                
                # 위에서 기간 필터링 및 class 부여된 데이터를 con.order로 사용
                con.order <- temp.2
                
                debug.fun(state = "data_filtering", obj = "con.order", msg = "선택된 주문", step = 2)
                
                ### 선택된 상품에 따라 order_detial을 필터링
                progress.value$now <- progress.value$now + 1
                progress$set(message = "주문 상세 데이터 필터링", value = progress.value$now)
                debug.fun(state = "progress", obj = "progress.value$now", step = 4)
                
                con.order.detail <- con.goods %>% 
                        select(GOODS_NO, GOODS_NM) %>% 
                        inner_join(order_detail, by = "GOODS_NO") %>% 
                        #select(GOODS_NO, ORD_NO, SELL_PRICE, ORD_CNT, GOODS_NM) %>% 
                        setDT()
                
                debug.fun(state = "data_filtering", obj = "con.order.detail", msg = "선택된 주문 디테일", step = 2)
                
                ###### 필터링 이후의 분석
                progress.value$now <- progress.value$now + 1
                progress$set(message = "필터링 된 데이터 합치는 중", value = progress.value$now)
                debug.fun(state = "progress", obj = "progress.value$now", step = 4)
                
                ### 필터링이 끝난 order, order_detail 정보를 합쳐서 con.data 로 만듬
                con.data <- con.order.detail %>% 
                        left_join(con.order, by = "ORD_NO") %>% 
                        filter(!is.na(CUSTOMER_NUM)) %>% # 외부몰 주문 삭제
                        setDT()
                
                ### 총 판매금액 생성
                con.data <- con.data %>% 
                        mutate(TOTAL_SELL_PRICE = ifelse(TAX_TYP_CD == "0001" & ORD_HPN_CD %in% c('0003', '0024')
                                                   , round((NORMAL_PRICE*CANCEL_POSS_CNT)/1.1),
                                                   ifelse(TAX_TYP_CD == "0001" & ORD_HPN_CD %out% c('0003', '0024')
                                                          , round((SELL_PRICE*CANCEL_POSS_CNT)/1.1),
                                                          ifelse(TAX_TYP_CD == "0002" & ORD_HPN_CD %in% c('0003', '0024')
                                                                 , NORMAL_PRICE*CANCEL_POSS_CNT,
                                                                 ifelse(TAX_TYP_CD == "0002" & ORD_HPN_CD %out% c('0003', '0024')
                                                                        , SELL_PRICE*CANCEL_POSS_CNT, 0))))
                               ) %>% 
                        filter(!is.na(TOTAL_SELL_PRICE)) %>% 
                        setDT()
                        
                debug.fun(state = "data_filtering", obj = "con.data", msg = "공통 데이터 생성", step = 2)
                
        }
},
error = function(e) print(paste0("data_filtering /// ", e)),
#warning = function(w) print(w),
finally = NULL)




