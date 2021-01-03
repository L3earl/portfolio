
# 그래프 박스
tryCatch({
        expr = {
                if(input$sidebar.menu == "basicProductSales"){
                        
                        progress.value$now <- progress.value$now + 1
                        progress$set(message = "그래프 데이터 만드는 중", value = progress.value$now)
                        debug.fun(state = "progress", obj = "progress.value$now", step = 4)
                        
                        # 기존 그래프 box 덮어씌워서 초기화함
                        output$graph <- renderUI({
                                box(width = NULL, solidHeader = TRUE, collapsible = TRUE,
                                    tags$div(id = "multiGraph")
                                )
                        })
                        
                        # facet 분류에 따라 그래프 여러개 그리기
                        for(i in 1:value$graph.num){
                                
                                # 그래프 그릴 자리 배치
                                insertUI(
                                        selector = "#multiGraph",
                                        where = "beforeBegin",
                                        ui = div(plotlyOutput(paste0("graph.", i)),
                                                 tags$hr(style="border-color: purple;"))
                                )
                                
                                # facet별 그래프에 들어갈 데이터 subset
                                if("facet" %in% colnames(result)){
                                        facet.data <- as.character(unique(result$facet))[i]
                                        
                                        graph.data <- result %>% 
                                                filter(facet == facet.data) %>% 
                                                setDT()
                                        
                                }else{
                                        facet.data <- "전체"
                                        
                                        graph.data <- result %>% 
                                                mutate(facet = facet.data) %>% 
                                                setDT()
                                }
                                debug.fun(state = "graph", obj = "facet.data", step = 2)
                                debug.fun(state = "graph", obj = "graph.data", step = 2)
                                
                                # 선택된 매출/주문 건수/수량 의 총합/ 평균 조건에 따라 Y축 데이터가 달라짐 (앞에서 전부 만들어 놓음)
                                temp.txt <- paste0("p <- ggplot(graph.data, aes(x = date, y = ", case.table[[input$data.type.input]][[input$data.measure.input]][1], "))")
                                eval(parse(text = temp.txt))
                                
                                # 그래프 공통 조건
                                p <- p + geom_point(aes(color = class, alpha = 0.7), size = 1) +
                                        geom_line(aes(color = class, alpha = 0.7, group = class)) +
                                        labs(x="날짜", title = facet.data, fill = "", color = "", alpha = "") +
                                        theme_bw()
                                
                                if(input$date.unit.input == 6){
                                        p <- p + labs(x = "요일")
                                        
                                }else if(input$date.unit.input == 7){
                                        p <- p + labs(x = "시간")
                                }
                                
                                gp <- ggplotly(p)
                                
                                # 아래의 output$graph.i <- renderPlotly({ ggplotly(p.i) }) 부분이 반복문이 끝난 후에 실행되는 듯하다. p.i를 여러개로 변경하지 않으면 같은 그래프만 여러개 나옴
                                temp.txt <- paste0("p.", i, " <- gp")
                                eval(parse(text = temp.txt))
                                
                                # 그래프 그리기 실행
                                temp.txt <- paste0("output$graph.", i, " <- renderPlotly({ p.", i, " })")
                                eval(parse(text = temp.txt))
                                
                        }
                }
        }
},
error = function(e) print(paste0("graph /// ", e)),
#warning = function(w) print(w),
finally = NULL)


