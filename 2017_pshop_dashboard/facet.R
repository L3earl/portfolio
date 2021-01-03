tryCatch({
        if(tab$name == "basicProductSales"){
                ### 그래프 나누기 옵션, 나누는 기준 column은 facet으로 rename하고 그 외의 분류 colum들은 하나의 class로 합침 (분류는 유저 그룹/ 상품 그룹/ 기간)
                progress.value$now <- progress.value$now + 1
                progress$set(message = "그래프 나누는 중", value = progress.value$now)
                debug.fun(state = "progress", obj = "progress.value$now", step = 4)
                
                # 나누는 기준 column으로 facet column을 만듬
                temp.2 <- graph.data %>% 
                {
                        if(input$facet.option.input == 1){ # 선택안함, 없으면 오류남
                                mutate(., sales = sales) 
                                
                        }else if(input$facet.option.input == 2){ # 기간
                                rename(., facet = date.class)
                                
                        }else if(input$facet.option.input == 3){ # 채널
                                rename(., facet = G.channel.class)
                                
                        }else if(input$facet.option.input == 4){ # 카테고리
                                rename(., facet = G.category.class)
                                
                        }else if(input$facet.option.input == 5){ # POV 카테고리
                                rename(., facet = G.PovCategory.class)
                                
                        }else if(input$facet.option.input == 6){ # 브랜드
                                rename(., facet = G.brand.class)
                                
                        }else if(input$facet.option.input == 7){ # 회원 그룹
                                rename(., facet = C.group.class)
                                
                        }else if(input$facet.option.input == 8){ # 회원 등급
                                rename(., facet = C.grade.class)
                                
                        }else if(input$facet.option.input == 9){ # 회원 연령
                                rename(., facet = C.age.class)
                                
                        }else if(input$facet.option.input == 10){ # 상품 검색
                                rename(., facet = GOODS_NM)
                                
                        }else{
                                rename(., date = date)
                        }
                } %>% setDT()
                
                debug.fun(state = "facet", obj = "temp.2", msg = "facet 완료", step = 3)
                
                # 빈 data frame을 만들고, class에 사용할 column만 남기고 삭제
                temp.class.names <- temp.2 %>% 
                        filter(sales == -9999999) %>% # 없는 것이 확실한 데이터로 필터링하여 data를 비움
                        select(-date, # 회원, 비회원 할 것 없이 공통적으로 들어가는 column 삭제
                               -order.num, -line.num, -sales, -goods.num, -customer.num, -goods.type.num,
                               -sales.per.goods.type, -sales.per.order, -sales.per.line, -sales.per.goods, -order.num.per.goods.type, -line.per.goods.type, -line.per.order, -goods.num.per.goods.type, -goods.num.per.order, -goods.num.per.line) %>% 
                               { # 회원, 임직원에만 있는 column 삭제
                                       if(input$user.type.input == 2 | input$user.type.input == 3){ # 회원, 임직원
                                               select(., -sales.per.user, -order.num.per.user, -line.per.user, -goods.num.per.user) # , -goods.type.per.user
                                       }else{
                                               ungroup(.) # 없으면 오류남
                                       }
                               } %>% 
                        setDT()
                
                # 그래프 나누기를 한다면, 거기에 쓴 facet은 class로 들어가지 않음
                if("facet" %in% colnames(temp.2)){
                        temp.class.names <- temp.class.names %>% 
                                select(-facet) %>% 
                                setDT()
                }
                
                # 나머지 column으로 class에 들어갈 내용을 만듬
                temp.class.names <- names(temp.class.names)
                
                # class에 들어갈 내용 순서를 factor로 정의
                temp.class.names <- sort(factor(temp.class.names, levels = c("GOODS_NM", "G.channel.class", "G.category.class", "G.PovCategory.class", "G.brand.class", "C.group.class", "C.grade.class", "C.age.class", "date.class")))
                
                debug.fun(state = "facet", obj = "temp.class.names", msg = "class에 사용할 col", step = 3)
                
                # class 부여
                progress.value$now <- progress.value$now + 1
                progress$set(message = "유저/상품 분류에 따라 데이터 나누는 중", value = progress.value$now)
                debug.fun(state = "progress", obj = "progress.value$now", step = 4)
                
                if(identical(as.character(temp.class.names), character(0))){ # class.col.names가 없을 경우, class에 전체를 부여함... 어떤 케이스에 이렇게 되는지 확인해야 함. todo !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                        result <<- temp.2 %>% # 전체를 분석하는 경우들을 살펴볼것 1. 그래프 분할이 "기간"으로 되는 경우
                                mutate(class = "전체") %>% 
                                setDT()
                        
                        debug.fun(state = "class가 없는 경우", obj = "result", step = 1)
                        
                }else{
                        temp.txt <- paste0("result <<- temp.2 %>% 
                                           mutate(class = paste0(", paste(temp.class.names, collapse = ",'/',"), ")) %>% 
                                           setDT()")
                        eval(parse(text = temp.txt))
                }
                
                debug.fun(state = "facet", obj = "result", msg = "결과", step = 2)
                
                # 그려야 하는 그래프의 개수
                if(input$facet.option.input == 1){ # 나누지 않음
                        value$graph.num <- 1
                }else if(input$facet.option.input == 2){ # 기간
                        value$graph.num <- input$date.number.input
                }else{ # 유저, 상품 분류
                        value$graph.num <- length(unique(result$facet))
                }
                
                debug.fun(state = "facet", obj = "value$graph.num", msg = "분석해야 하는 그래프 개수", step = 3)
                
        }
},
error = function(e) print(paste0("facet /// ", e)),
#warning = function(w) print(w),
finally = NULL)
