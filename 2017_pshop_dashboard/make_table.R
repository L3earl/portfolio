tryCatch({
        if(tab$name == "basicProductSales"){
                ### x축에 표현되야 하는 단위로 group_by -> summarize로 집계함
                progress.value$now <- progress.value$now + 1
                progress$set(message = "매출, 주문 건수/수량 계산 중", value = progress.value$now)
                debug.fun(state = "progress", obj = "progress.value$now", step = 4)
                
                # 집계 기준이 될 column
                group.col <- c("date", "date.class")
                
                if(1 %in% input$user.input & 2 %in% update$user.type.input){ # 회원 그룹이 선택됨
                        group.col <- append(group.col, "C.group.class")
                }
                
                if(2 %in% input$user.input & 2 %in% update$user.type.input){ # 회원 등급이 선택됨
                        group.col <- append(group.col, "C.grade.class")
                }
                
                if(3 %in% input$user.input & 2 %in% update$user.type.input){ # 회원 연령이 선택됨
                        group.col <- append(group.col, "C.age.class")
                }
                
                if(2 %in% input$category.input){ # 채널이 선택됨
                        group.col <- append(group.col, "G.channel.class")
                }
                
                if(3 %in% input$category.input){ # 카테고리가 선택됨
                        group.col <- append(group.col, "G.category.class")
                }
                
                if(4 %in% input$category.input){ # POV 카테고리가 선택됨 (사용 안함)
                        group.col <- append(group.col, "G.PovCategory.class")
                }
                
                if(5 %in% input$category.input){ # 브랜드가 선택됨
                        group.col <- append(group.col, "G.brand.class")
                }
                
                if(!is.null(input$goods.select.input)){ # 상품 검색에서 1개 이상 선택됨
                        group.col <- append(group.col, "GOODS_NM")
                }
                
                debug.fun(state = "make_table", obj = "group.col", msg = "집계할 column", step = 3)
                
                temp.txt <- paste0("con.data.class <- con.data.class %>% group_by(", paste(group.col, collapse = ","), ")")
                
                eval(parse(text = temp.txt))
                        
                # 집계
                graph.data <- con.data.class %>% 
                        #group_by_at(vars(one_of(group.col))) %>% 
                        summarise(order.num = n_distinct(ORD_NO), # 주문 건수
                                  line.num = n(), # 라인(order_detail 라인 개수, 주문한 상품 종류) 수
                                  sales = sum(TOTAL_SELL_PRICE), # 매출
                                  goods.num = sum(ORD_CNT), # 주문한 상품 수
                                  customer.num = n_distinct(CUSTOMER_NUM), # 주문한 고객 수
                                  goods.type.num = n_distinct(GOODS_NO) # 주문한 상품 종류 수
                        ) %>% 
                        {
                                if(input$user.type.input == 2 | input$user.type.input == 3){ # 회원, 임직원
                                        mutate(., 
                                               sales.per.goods.type = round(sales/goods.num, digit = 0), # 주문한 상품 종류 1개당 평균 매출
                                               sales.per.order = round(sales/order.num, digit = 0), # 주문 1건당 평균 매출
                                               sales.per.line = round(sales/line.num, digit = 0), # 라인 1건당 평균 매출
                                               sales.per.goods = round(sales/goods.num, digit = 1), # 주문한 상품 개수 1개당 평균 매출 = 상품 평균 가격
                                               order.num.per.goods.type = round(order.num/goods.type.num, digit = 1), # 주문한 상품 종류 1개당 평균 주문 수
                                               line.per.goods.type = round(line.num/goods.type.num, digit = 1), # 주문한 상품 종류 1개당 평균 라인 수
                                               line.per.order = round(line.num/order.num, digit = 1), # 주문 1건당 평균 라인 수
                                               goods.num.per.goods.type = round(goods.num/goods.type.num, digit = 1), # 주문한 상품 종류 1개당 주문한 상품 개수
                                               goods.num.per.order = round(goods.num/order.num, digits = 1), # 주문 1건당 평균 주문한 상품 개수
                                               goods.num.per.line = round(goods.num/line.num, digit = 1), # 라인 1건당 평균 주문한 상품 개수
                                               # goods.type.per.user = round(goods.type.num/customer.num, digit = 1), # 주문한 1인당 평균 주문한 상품 종류 수
                                               sales.per.user = round(sales/customer.num, digits = 0), # 고객 1인당 평균 매출
                                               order.num.per.user = round(order.num/customer.num, digits = 1), # 고객 1인당 평균 주문 수
                                               line.per.user = round(line.num/customer.num, digit = 1), # 고객 1인당 평균 라인 수
                                               goods.num.per.user = round(goods.num/customer.num, digits = 1) # 고객 1인당 주문한 상품 개수
                                        )
                                }else{ # 전체, 비회원
                                        mutate(., 
                                               sales.per.goods.type = round(sales/goods.num, digit = 0), # 주문한 상품 종류 1개당 평균 매출
                                               sales.per.order = round(sales/order.num, digit = 0), # 주문 1건당 평균 매출
                                               sales.per.line = round(sales/line.num, digit = 0), # 라인 1건당 평균 매출
                                               sales.per.goods = round(sales/goods.num, digit = 1), # 주문한 상품 개수 1개당 평균 매출 = 상품 평균 가격
                                               order.num.per.goods.type = round(order.num/goods.type.num, digit = 1), # 주문한 상품 종류 1개당 평균 주문 수
                                               line.per.goods.type = round(line.num/goods.type.num, digit = 1), # 주문한 상품 종류 1개당 평균 라인 수
                                               line.per.order = round(line.num/order.num, digit = 1), # 주문 1건당 평균 라인 수
                                               goods.num.per.goods.type = round(goods.num/goods.type.num, digit = 1), # 주문한 상품 종류 1개당 주문한 상품 개수
                                               goods.num.per.order = round(goods.num/order.num, digits = 1), # 주문 1건당 평균 주문한 상품 개수
                                               goods.num.per.line = round(goods.num/line.num, digit = 1) # 라인 1건당 평균 주문한 상품 개수
                                        )
                                }
                        } %>% setDT()
                
                debug.fun(state = "make_table", obj = "graph.data", msg = "집계", step = 2)
                
                ### 데이터가 있는지 없는지 확인
                if(nrow(graph.data) == 0){
                        showNotification(ui = "선택된 조건에 부합하는 데이터가 없습니다.",
                                         type = "error",
                                         duration = 20)
                }
                
                ### 데이터를 기간(date.class)에 따라 spread함
                # 데이터 col name
                table.dt.names <- colnames(graph.data)
                
                # spread 안할 col
                no.spread.col <- group.col[-which(group.col == "date.class")]
                
                debug.fun(state = "make_table", obj = "no.spread.col", step = 3)
                
                # 선택된 매출/주문 건수/수량 의 총합/ 평균 조건에 따라 테이블에 보여주고, spread 할 데이터가 달라짐
                spread.col <- case.table[[input$data.type.input]][[input$data.measure.input]][1]
                
                debug.fun(state = "make_table", obj = "spread.col", step = 3)
                
                # spread
                temp.txt <- paste0("result.table <- dcast(graph.data, ", paste(no.spread.col, collapse = "+"), " ~ date.class,
                                   value.var = '", spread.col, "')")
                eval(parse(text = temp.txt))
                
                debug.fun(state = "make_table", obj = "result.table", step = 2)
                
                ### column 이름 변경
                new.col.name <- c()
                if("date" %in% no.spread.col){
                        if(input$date.unit.input == 7){ # 시간
                                new.col.name <- append(new.col.name, "시간")
                        }else if(input$date.unit.input == 6){ # 요일
                                new.col.name <- append(new.col.name, "요일")
                        }else{
                                new.col.name <- append(new.col.name, "날짜")
                        }
                }
                if("C.group.class" %in% no.spread.col){
                        new.col.name <- append(new.col.name, "고객 그룹")
                }
                if("C.grade.class" %in% no.spread.col){
                        new.col.name <- append(new.col.name, "고객 등급")
                }
                if("C.age.class" %in% no.spread.col){
                        new.col.name <- append(new.col.name, "고객 연령")
                }
                if("G.channel.class" %in% no.spread.col){
                        new.col.name <- append(new.col.name, "채널")
                }
                if("G.category.class" %in% no.spread.col){
                        new.col.name <- append(new.col.name, "카테고리")
                }
                if("G.PovCategory.class" %in% no.spread.col){
                        new.col.name <- append(new.col.name, "POV 카테고리")
                }
                if("G.brand.class" %in% no.spread.col){
                        new.col.name <- append(new.col.name, "브랜드")
                }
                if("GOODS_NM" %in% no.spread.col){
                        new.col.name <- append(new.col.name, "상품명")
                }
                
                setnames(result.table, old = no.spread.col, new = new.col.name)
                
                debug.fun(state = "make_table", obj = "new.col.name", step = 3)
                
                
                
                
                
                
                ###### date 가 없는 데이터 테이블
                progress.value$now <- progress.value$now + 1
                progress$set(message = "합쳐진 데이터 테이블 만드는 중", value = progress.value$now)
                debug.fun(state = "progress", obj = "progress.value$now", step = 4)
                
                # 구분하는 col로 group_by 하여 summarize
                group.col.sum <- group.col[-which(group.col == "date")]
                
                temp.txt <- paste0("con.data.class <- con.data.class %>% group_by(", paste(group.col.sum, collapse = ","), ")")
                
                eval(parse(text = temp.txt))
                
                result.table.sum <- con.data.class %>% 
                        #group_by_at(vars(one_of(group.col.sum))) %>% 
                        summarise(order.num = n_distinct(ORD_NO), # 주문 건수
                                  line.num = n(), # 라인(order_detail 라인 개수, 주문한 상품 종류) 수
                                  sales = sum(TOTAL_SELL_PRICE), # 매출
                                  goods.num = sum(ORD_CNT), # 주문한 상품 수
                                  customer.num = n_distinct(CUSTOMER_NUM), # 주문한 고객 수
                                  goods.type.num = n_distinct(GOODS_NO) # 주문한 상품 종류 수
                        ) %>% 
                        {
                                if(input$user.type.input == 2 | input$user.type.input == 3){ # 회원, 임직원
                                        mutate(., 
                                               sales.per.goods.type = round(sales/goods.num, digit = 0), # 주문한 상품 종류 1개당 평균 매출
                                               sales.per.order = round(sales/order.num, digit = 0), # 주문 1건당 평균 매출
                                               sales.per.line = round(sales/line.num, digit = 0), # 라인 1건당 평균 매출
                                               sales.per.goods = round(sales/goods.num, digit = 1), # 주문한 상품 개수 1개당 평균 매출 = 상품 평균 가격
                                               order.num.per.goods.type = round(order.num/goods.type.num, digit = 1), # 주문한 상품 종류 1개당 평균 주문 수
                                               line.per.goods.type = round(line.num/goods.type.num, digit = 1), # 주문한 상품 종류 1개당 평균 라인 수
                                               line.per.order = round(line.num/order.num, digit = 1), # 주문 1건당 평균 라인 수
                                               goods.num.per.goods.type = round(goods.num/goods.type.num, digit = 1), # 주문한 상품 종류 1개당 주문한 상품 개수
                                               goods.num.per.order = round(goods.num/order.num, digits = 1), # 주문 1건당 평균 주문한 상품 개수
                                               goods.num.per.line = round(goods.num/line.num, digit = 1), # 라인 1건당 평균 주문한 상품 개수
                                               # goods.type.per.user = round(goods.type.num/customer.num, digit = 1), # 주문한 1인당 평균 주문한 상품 종류 수
                                               sales.per.user = round(sales/customer.num, digits = 0), # 고객 1인당 평균 매출
                                               order.num.per.user = round(order.num/customer.num, digits = 1), # 고객 1인당 평균 주문 수
                                               line.per.user = round(line.num/customer.num, digit = 1), # 고객 1인당 평균 라인 수
                                               goods.num.per.user = round(goods.num/customer.num, digits = 1) # 고객 1인당 주문한 상품 개수
                                        )
                                }else{ # 전체, 비회원
                                        mutate(., 
                                               sales.per.goods.type = round(sales/goods.num, digit = 0), # 주문한 상품 종류 1개당 평균 매출
                                               sales.per.order = round(sales/order.num, digit = 0), # 주문 1건당 평균 매출
                                               sales.per.line = round(sales/line.num, digit = 0), # 라인 1건당 평균 매출
                                               sales.per.goods = round(sales/goods.num, digit = 1), # 주문한 상품 개수 1개당 평균 매출 = 상품 평균 가격
                                               order.num.per.goods.type = round(order.num/goods.type.num, digit = 1), # 주문한 상품 종류 1개당 평균 주문 수
                                               line.per.goods.type = round(line.num/goods.type.num, digit = 1), # 주문한 상품 종류 1개당 평균 라인 수
                                               line.per.order = round(line.num/order.num, digit = 1), # 주문 1건당 평균 라인 수
                                               goods.num.per.goods.type = round(goods.num/goods.type.num, digit = 1), # 주문한 상품 종류 1개당 주문한 상품 개수
                                               goods.num.per.order = round(goods.num/order.num, digits = 1), # 주문 1건당 평균 주문한 상품 개수
                                               goods.num.per.line = round(goods.num/line.num, digit = 1) # 라인 1건당 평균 주문한 상품 개수
                                        )
                                }
                        } %>% setDT()
                
                debug.fun(state = "table.sum", obj = "result.table.sum", step = 3)
                
                no.spread.col.sum <- group.col[-which(group.col %in% c("date", "date.class"))]
                
                if(length(no.spread.col.sum) > 0){
                        
                        # spread
                        temp.txt <- paste0("result.table.sum <- dcast(result.table.sum, ", paste(no.spread.col.sum, collapse = "+"), " ~ date.class,
                                           value.var = '", spread.col, "')")
                        
                        eval(parse(text = temp.txt))
                        
                        debug.fun(state = "table.sum", obj = "result.table.sum", step = 3)
                        
                        # colname 변경
                        if(input$date.unit.input == 7){ # 시간
                                new.col.name <- new.col.name[-which(new.col.name == "시간")]
                        }else if(input$date.unit.input == 6){ # 요일
                                new.col.name <- new.col.name[-which(new.col.name == "요일")]
                        }else{
                                new.col.name <- new.col.name[-which(new.col.name == "날짜")]
                        }
                        
                        setnames(result.table.sum, old = no.spread.col.sum, new = new.col.name)
                        
                }else{
                        myCols <- c("date.class",spread.col)
                        colNums <- match(myCols,names(result.table.sum))
                        result.table.sum <- result.table.sum %>% 
                                select(colNums) %>% 
                                setDT()
                        
                        colnames(result.table.sum) <- c("날짜 그룹", case.table[[input$data.type.input]][[input$data.measure.input]][2])
                }
        }
},
error = function(e) print(paste0("make_table /// ", e)),
#warning = function(w) print(w),
finally = NULL)


