
# 데이터 테이블 박스
tryCatch({
        expr = {
                if(input$sidebar.menu == "basicProductSales"){
                        progress.value$now <- progress.value$now + 1
                        progress$set(message = "데이터 테이블 만드는 중", value = progress.value$now)
                        debug.fun(state = "progress", obj = "progress.value$now", step = 4)
                        
                        ### 집계된 테이블 excel 다운로드
                        # 오늘 날짜
                        file.name <- paste0(spread.col, "_", gsub(" ", "_", as_datetime(Sys.time(), tz = "Asia/Seoul")), ".csv")
                        
                        debug.fun(state = "table", obj = "file.name", step = 3)
                        
                        #
                        output$download.table.dt <- downloadHandler(
                                filename = function(){file.name}, 
                                content = function(file){
                                        write.csv(result.table, file, fileEncoding = 'EUC-KR')
                                }
                        )
                        
                        ### raw 데이터 테이블
                        output$download.table.raw <- downloadHandler(
                                filename = function(){paste0("raw_", file.name)}, 
                                content = function(file){
                                        write.csv(graph.data, file, fileEncoding = 'EUC-KR') 
                                }
                        )
                        
                        # UI 초기화
                        output$data.table <- renderUI({
                        })
                        
                        ###
                        output$data.table <- renderUI({
                                box(width = NULL, solidHeader = TRUE, collapsible = TRUE,
                                    style = 'overflow-x: scroll', DT::renderDataTable(
                                            DT::datatable(
                                                    data = result.table, 
                                                    class = 'cell-border stripe', 
                                                    rownames = FALSE,
                                                    extensions = c('ColReorder', 
                                                                   'KeyTable', 
                                                                   'Responsive'),
                                                    options = list(colReorder = TRUE, 
                                                                   keys = TRUE) 
                                            )
                                    ),
                                    downloadButton('download.table.dt',"테이블 다운로드"), ### excel 다운로드
                                    downloadButton('download.table.raw',"raw 데이터 다운로드") ### excel 다운로드
                                )
                        })
                        
                        ### date 없이 집계한 테이블 excel 다운로드
                        #
                        output$download.table.dt.sum <- downloadHandler(
                                filename = function(){paste0("sum_", file.name)}, 
                                content = function(file){
                                        write.csv(result.table.sum, file, fileEncoding = 'EUC-KR')
                                }
                        )
                        
                        # UI 초기화
                        output$data.table.sum <- renderUI({
                        })
                        
                        #
                        output$data.table.sum <- renderUI({
                                box(width = NULL, solidHeader = TRUE, collapsible = TRUE,
                                    style = 'overflow-x: scroll', DT::renderDataTable(
                                            DT::datatable(
                                                    data = result.table.sum, 
                                                    class = 'cell-border stripe', 
                                                    rownames = FALSE,
                                                    extensions = c('ColReorder', 
                                                                   'KeyTable', 
                                                                   'Responsive'),
                                                    options = list(colReorder = TRUE, 
                                                                   keys = TRUE) 
                                            )
                                    ),
                                    downloadButton('download.table.dt.sum',"테이블 다운로드") ### excel 다운로드
                                )
                        })
                }
        }
},
error = function(e) print(paste0("table_UI /// ", e)),
#warning = function(w) print(w),
finally = NULL)








