#### UI 초기화

# 
output$data.option <- renderUI({
        if(tab$name == "basicProductSales"){
                box(width = NULL, solidHeader = TRUE, collapsible = TRUE,
                    uiOutput("user.type"),
                    uiOutput("date.unit"),
                    uiOutput("data.type"),
                    uiOutput("data.measure"),
                    uiOutput("facet.option"),
                    actionButton(inputId = "start.button", label = "분석 시작")
                )
        }
})

#### UI 초기화 및 업데이트
# 자주 쓰는 format
observeEvent({
        input$start.button
}, {
        
})

# 유저 종류 선택 UI input
observeEvent({
        input$sidebar.menu
}, {
        debug.fun(state = "make_option_UI", obj = "update$user.type.input", step = 4)
        
        value$user.type.input <- isolate(update$user.type.input)
        
        output$user.type <- renderUI({
                selectizeInput("user.type.input", label = "유저 종류", 
                               choices = list("전체" = 1, "회원" = 2, "임직원" = 3, "비회원" = 4, "회원+비회원" = 5),
                               selected = value$user.type.input)
        })
        
})

# 기간 단위 UI input
observeEvent({
        input$sidebar.menu
}, {
        debug.fun(state = "make_option_UI", obj = "update$date.unit.input", step = 4)
        
        value$date.unit.input <- isolate(update$date.unit.input)
        
        output$date.unit <- renderUI({
                selectizeInput("date.unit.input", label = "기간 단위", 
                               choices = list("일" = 1, "주" = 2, "월" = 3, "분기" = 4, "년" = 5, "요일" = 6, "시간" = 7),
                               selected = value$date.unit.input)
        })
        
})

# 분석 종류 UI input
observeEvent({
        input$sidebar.menu
}, {
        debug.fun(state = "make_option_UI", obj = "update$data.type.input", step = 4)
        
        value$data.type.input <- isolate(update$data.type.input)
        
        output$data.type <- renderUI({
                selectizeInput("data.type.input", label = "분석 종류", 
                               choices = list("매출" = 1, "주문 건수" = 2, "라인 수" = 3, "주문한 상품 수" = 4), #, "기타" = 5
                               selected = value$data.type.input)
        })
        
})

# 분석 내용 UI input
observeEvent({
        input$sidebar.menu
        update$data.type.input
        update$user.type.input
}, {
        debug.fun(state = "make_option_UI", obj = "update$data.measure.input", step = 4)
        
        value$data.measure.input <- isolate(update$data.measure.input)
        
        ## 공통 분석 조건
        data.measure.select.list <- list("전체" = 1, "상품 종류 1개당 평균" = 2)
        
        ## 회원/임직원 공통 분석 조건
        if(update$user.type.input == 2 | update$user.type.input == 3){ # 회원, 임직원
                data.measure.select.list <- append(data.measure.select.list, list("고객 1명당 평균" = 3))
        }
        
        ## 개별 분석 조건
        if(update$data.type.input == 1){ # 매출
                data.measure.select.list <- append(data.measure.select.list, list("주문 1건당 평균" = 4,
                                                                                  "라인 1개당 평균" = 5,
                                                                                  "주문한 상품 1개당 평균" = 6))
        }else if(update$data.type.input == 3){ # 라인 수
                data.measure.select.list <- append(data.measure.select.list, list("주문 1건당 평균" = 4))
                
        }else if(update$data.type.input == 4){ # 주문한 상품 수
                data.measure.select.list <- append(data.measure.select.list, list("주문 1건당 평균" = 4,
                                                                                  "라인 1개당 평균" = 5))
        }else if(update$data.type.input == 5){ # 기타 (사용 안함)
                data.measure.select.list <- list("주문한 1인당 평균 주문한 상품 종류 수" = 3) # 1번 (총합) 일 경우를 사용한 조건문이 table에 이 있음
        }
        
        #
        output$data.measure <- renderUI({
                selectizeInput("data.measure.input", label = "분석 내용", 
                               choices = data.measure.select.list, 
                               selected = value$data.measure.input
                )
        })
        
})

# 화면 분할 옵션 UI input
observeEvent({
        input$sidebar.menu
        update$user.type.input
}, {
        debug.fun(state = "make_option_UI", obj = "update$facet.option.input", step = 4)
        
        value$facet.option.input <- isolate(update$facet.option.input)
        
        output$facet.option <- renderUI({
                selectizeInput("facet.option.input", label = "그래프 나누기", 
                               choices = if(update$user.type.input == 2){ # 회원 선택
                                       list("안 나눔" = 1, "기간" = 2, "채널" = 3, "카테고리" = 4, "브랜드" = 6, "회원 그룹" = 7, "회원 등급" = 8, "회원 연령" = 9, "상품 검색" = 10) # "POV 카테고리" = 5, 
                               }else{
                                       list("안 나눔" = 1, "기간" = 2, "채널" = 3, "카테고리" = 4, "브랜드" = 6, "상품 검색" = 10) # "POV 카테고리" = 5, 
                               }, selected = value$facet.option.input)
        })
})

# 회원 분류 UI input
observeEvent({
        input$sidebar.menu
        update$user.type.input
}, {
        debug.fun(state = "make_option_UI", obj = "update$user.input", step = 4)
        debug.fun(state = "make_option_UI", obj = "update$user.select.input", step = 4)
        
        value$user.input <- isolate(update$user.input)
        value$user.select.input <- isolate(update$user.select.input)
        
        if(2 %in% update$user.type.input){
                output$user <- renderUI({
                        box(width = NULL, solidHeader = TRUE, collapsible = TRUE,
                            checkboxGroupInput("user.input", label = "회원 분류", 
                                               choices = c("그룹" = 1, "등급" = 2, "연령" = 3), 
                                               selected = value$user.input),
                            selectizeInput("user.select.input", label = "회원 검색 (복수 입력 가능)", 
                                           choices = NULL,
                                           selected = value$user.select.input,
                                           multiple = TRUE,
                                           options = list(create = TRUE, createOnBlur = TRUE)),
                            h6("회원 검색 방법: 고객 번호 1개(ex: 20080985) 입력이 완료 된 후에, 다른 고객 번호(ex: 20119016)를 입력해 주세요.")
                        )
                })
        }else{
                output$user <- renderUI({
                })
        }
})

# 회원 그룹 박스
observeEvent({
        input$sidebar.menu
        update$user.input
        update$user.type.input
        input$user.group.googleSheet.load
}, {
        debug.fun(state = "make_option_UI", obj = "update$user.group.box.input", step = 4)
        
        value$user.group.box.input <- isolate(update$user.group.box.input)
        
        if(1 %in% update$user.input & 2 %in% update$user.type.input){
                output$user.group.box <- renderUI({
                        box(width = NULL, solidHeader = TRUE, collapsible = TRUE, 
                            checkboxGroupInput("user.group.box.input", "회원 그룹", inline = TRUE,
                                               choices = unique(filter(member_class, !is.na(C.group.class))$C.group.class),
                                               selected = value$user.group.box.input),
                            actionButton(inputId = "user.group.box.all", label = "전체 선택"),
                            actionButton(inputId = "user.group.box.none", label = "전체 해제"),
                            actionButton(inputId = "user.group.googleSheet.browse", label = "그룹 수정",
                                         onclick ="window.open('https://drive.google.com/drive/folders/1zWzXtmj-0jEmS1GMeboBAONjo3RCSVQK?usp=sharing', '_blank')"), # 풀무원에 설치할 때 변경 필요 (todo)
                            actionButton(inputId = "user.group.googleSheet.load", label = "새로 고침")
                        )
                })
        }else{
                output$user.group.box <- renderUI({
                })
        }
}, ignoreNULL = FALSE)

observeEvent(input$user.group.box.all, {
        updateCheckboxGroupInput(
                session, "user.group.box.input", "회원 그룹", inline = TRUE,
                choices = unique(filter(member_class, !is.na(C.group.class))$C.group.class),
                selected = unique(filter(member_class, !is.na(C.group.class))$C.group.class)
        )
})

observeEvent(input$user.group.box.none, {
        updateCheckboxGroupInput(
                session, "user.group.box.input", "회원 그룹", inline = TRUE,
                choices = unique(filter(member_class, !is.na(C.group.class))$C.group.class),
                selected = NULL
        )
})

# 회원 등급 박스
observeEvent({
        input$sidebar.menu
        update$user.input
        update$user.type.input
}, {
        debug.fun(state = "make_option_UI", obj = "update$user.grade.box.input", step = 4)
        
        value$user.grade.box.input <- isolate(update$user.grade.box.input)
        
        if(2 %in% update$user.input & 2 %in% update$user.type.input){
                output$user.grade.box <- renderUI({
                        box(width = NULL, solidHeader = TRUE, collapsible = TRUE,
                            checkboxGroupInput("user.grade.box.input", "회원 등급", inline = TRUE,
                                               choices = unique(filter(member_class, !is.na(C.grade.class))$C.grade.class),
                                               selected = value$user.grade.box.input),
                            actionButton(inputId = "user.grade.box.all", label = "전체 선택"),
                            actionButton(inputId = "user.grade.box.none", label = "전체 해제")
                        )
                })
        }else{
                output$user.grade.box <- renderUI({
                })
        }
}, ignoreNULL = FALSE)

observeEvent(input$user.grade.box.all, {
        updateCheckboxGroupInput(
                session, "user.grade.box.input", "회원 등급", inline = TRUE,
                choices = unique(filter(member_class, !is.na(C.grade.class))$C.grade.class),
                selected = unique(filter(member_class, !is.na(C.grade.class))$C.grade.class)
        )
})

observeEvent(input$user.grade.box.none, {
        updateCheckboxGroupInput(
                session, "user.grade.box.input", "회원 등급", inline = TRUE,
                choices = unique(filter(member_class, !is.na(C.grade.class))$C.grade.class),
                selected = NULL
        )
})

# 회원 연령 박스
observeEvent({
        input$sidebar.menu
        update$user.input
        update$user.type.input
}, {
        debug.fun(state = "make_option_UI", obj = "update$user.age.box.input", step = 4)
        
        value$user.age.box.input <- isolate(update$user.age.box.input)
        
        if(3 %in% update$user.input & 2 %in% update$user.type.input){
                output$user.age.box <- renderUI({
                        box(width = NULL, solidHeader = TRUE, collapsible = TRUE,
                            checkboxGroupInput("user.age.box.input", "회원 연령", inline = TRUE,
                                               choices = unique(filter(member_class, !is.na(C.age.class))$C.age.class),
                                               selected = value$user.age.box.input),
                            actionButton(inputId = "user.age.box.all", label = "전체 선택"),
                            actionButton(inputId = "user.age.box.none", label = "전체 해제")
                        )
                })
        }else{
                output$user.age.box <- renderUI({
                })
        }
}, ignoreNULL = FALSE)

observeEvent(input$user.age.box.all, {
        updateCheckboxGroupInput(
                session, "user.age.box.input", "회원 연령", inline = TRUE,
                choices = unique(filter(member_class, !is.na(C.age.class))$C.age.class),
                selected = unique(filter(member_class, !is.na(C.age.class))$C.age.class)
        )
})

observeEvent(input$user.age.box.none, {
        updateCheckboxGroupInput(
                session, "user.age.box.input", "회원 연령", inline = TRUE,
                choices = unique(filter(member_class, !is.na(C.age.class))$C.age.class),
                selected = NULL
        )
})

# 상품 그룹 UI input
observeEvent({
        input$sidebar.menu
}, {
        debug.fun(state = "make_option_UI", obj = "update$product.group.input", step = 4)
        
        value$product.group.input <- isolate(update$product.group.input)
        
        output$product.group <- renderUI({
                checkboxInput("product.group.input", label = "상품 그룹", value = value$product.group.input, width = NULL)
        })
})

# 상품 그룹 박스
observeEvent({
        input$sidebar.menu
}, {
        debug.fun(state = "make_option_UI", obj = "update$product.group.box.input", step = 4)
        
        value$product.group.box.input <- isolate(update$product.group.box.input)
        
        output$product.group.box <- renderUI({
                conditionalPanel(
                        condition = "input['product.group.input'] == true",
                        box(width = NULL, solidHeader = TRUE, collapsible = TRUE,
                            checkboxGroupInput("product.group.box.input", "상품 그룹", inline = TRUE,
                                               choices = c(),
                                               selected = value$product.group.box.input)
                        )
                )
        })
})

# 상품 분류/개별 선택 UI input
observeEvent({
        input$sidebar.menu
}, {
        debug.fun(state = "make_option_UI", obj = "update$category.input", step = 4)
        debug.fun(state = "make_option_UI", obj = "update$goods.select.input", step = 4)
        
        value$category.input <- isolate(update$category.input)
        value$goods.select.input <- isolate(update$goods.select.input)
        
        output$category <- renderUI({
                box(width = NULL, solidHeader = TRUE, collapsible = TRUE,
                    checkboxGroupInput("category.input", label = "상품 분류",
                                       choices = c("채널" = 2, "카테고리" = 3, "브랜드" = 5), # "선택안함" = 1, "POV 카테고리" = 4 빠짐
                                       selected = value$category.input),
                    selectizeInput("goods.select.input", label = "상품 검색 (복수 선택 가능)", 
                                   choices = unique(goods$GOODS_NM),
                                   selected = value$goods.select.input,
                                   multiple = TRUE,
                                   options = list(maxOptions = 10, openOnFocus = F, closeAfterSelect = T, hideSelected = T))
                )
        })
})

# 채널 박스
observeEvent({
        input$sidebar.menu
        update$category.input
}, {
        debug.fun(state = "make_option_UI", obj = "update$channal.box.input", step = 4)
        
        value$channal.box.input <- isolate(update$channal.box.input)
        
        if(2 %in% update$category.input){
                output$channal.box <- renderUI({
                        box(width = NULL, solidHeader = TRUE, collapsible = TRUE,
                            checkboxGroupInput("channal.box.input", "채널", inline = TRUE,
                                               choices = unique(filter(product_class, !is.na(G.channel.class))$G.channel.class),
                                               selected = value$channal.box.input),
                            actionButton(inputId = "channal.box.all", label = "전체 선택"),
                            actionButton(inputId = "channal.box.none", label = "전체 해제")
                        )
                })
        }else{
                output$channal.box <- renderUI({
                })
        }
}, ignoreNULL = FALSE)

observeEvent(input$channal.box.all, {
        updateCheckboxGroupInput(
                session, "channal.box.input", "채널", inline = TRUE,
                choices = unique(filter(product_class, !is.na(G.channel.class))$G.channel.class),
                selected = unique(filter(product_class, !is.na(G.channel.class))$G.channel.class)
        )
})

observeEvent(input$channal.box.none, {
        updateCheckboxGroupInput(
                session, "channal.box.input", "채널", inline = TRUE,
                choices = unique(filter(product_class, !is.na(G.channel.class))$G.channel.class),
                selected = NULL
        )
})

# 카테고리 박스
observeEvent({
        input$sidebar.menu
        update$category.input
}, {
        debug.fun(state = "make_option_UI", obj = "update$category.box.input", step = 4)
        
        value$category.box.input <- isolate(update$category.box.input)
        
        if(3 %in% update$category.input){
                output$category.box <- renderUI({
                        box(width = NULL, solidHeader = TRUE, collapsible = TRUE,
                            checkboxGroupInput("category.box.input", "카테고리", inline = TRUE,
                                               choices = unique(filter(product_class, !is.na(G.category.class))$G.category.class),
                                               selected = value$category.box.input),
                            actionButton(inputId = "category.box.all", label = "전체 선택"),
                            actionButton(inputId = "category.box.none", label = "전체 해제")
                        )
                })
        }else{
                output$category.box <- renderUI({
                })
        }
}, ignoreNULL = FALSE)

observeEvent(input$category.box.all, {
        updateCheckboxGroupInput(
                session, "category.box.input", "카테고리", inline = TRUE,
                choices = unique(filter(product_class, !is.na(G.category.class))$G.category.class),
                selected = unique(filter(product_class, !is.na(G.category.class))$G.category.class)
        )
})

observeEvent(input$category.box.none, {
        updateCheckboxGroupInput(
                session, "category.box.input", "카테고리", inline = TRUE,
                choices = unique(filter(product_class, !is.na(G.category.class))$G.category.class),
                selected = NULL
        )
})

# POV 카테고리 박스, 사용 안함
observeEvent({
        input$sidebar.menu
        update$category.input
}, {
        debug.fun(state = "make_option_UI", obj = "update$category.pov.box.input", step = 4)
        
        value$category.pov.box.input <- isolate(update$category.pov.box.input)
        
        if(4 %in% update$category.input){
                output$category.pov.box <- renderUI({
                        box(width = NULL, solidHeader = TRUE, collapsible = TRUE,
                            checkboxGroupInput("category.pov.box.input", "POV 카테고리", inline = TRUE,
                                               choices = unique(filter(product_class, !is.na(G.PovCategory.class))$G.PovCategory.class),
                                               selected = value$category.pov.box.input)
                        )
                })
        }else{
                output$category.pov.box <- renderUI({
                })
        }
}, ignoreNULL = FALSE)

# 브랜드 박스
observeEvent({
        input$sidebar.menu
        update$category.input
}, {
        debug.fun(state = "make_option_UI", obj = "update$brand.box.input", step = 4)
        
        value$brand.box.input <- isolate(update$brand.box.input)
        
        if(5 %in% update$category.input){
                output$brand.box <- renderUI({
                        box(width = NULL, solidHeader = TRUE, collapsible = TRUE,
                            checkboxGroupInput("brand.box.input", "브랜드", inline = TRUE,
                                               choices = unique(filter(product_class, !is.na(G.brand.class))$G.brand.class),
                                               selected = value$brand.box.input),
                            actionButton(inputId = "brand.box.all", label = "전체 선택"),
                            actionButton(inputId = "brand.box.none", label = "전체 해제")
                        )
                })
        }else{
                output$brand.box <- renderUI({
                })
        }
}, ignoreNULL = FALSE)

observeEvent(input$brand.box.all, {
        updateCheckboxGroupInput(
                session, "brand.box.input", "브랜드", inline = TRUE,
                choices = unique(filter(product_class, !is.na(G.brand.class))$G.brand.class),
                selected = unique(filter(product_class, !is.na(G.brand.class))$G.brand.class)
        )
})

observeEvent(input$brand.box.none, {
        updateCheckboxGroupInput(
                session, "brand.box.input", "브랜드", inline = TRUE,
                choices = unique(filter(product_class, !is.na(G.brand.class))$G.brand.class),
                selected = NULL
        )
})

# 기간 선택 UI input
observeEvent({
        input$sidebar.menu
}, {
        debug.fun(state = "make_option_UI", obj = "update$date.number.input", step = 4)
        
        debug.fun(state = "make_option_UI", obj = "update$date.select.input.01", step = 4)
        debug.fun(state = "make_option_UI", obj = "update$date.range.input.01.min", step = 4)
        debug.fun(state = "make_option_UI", obj = "update$date.range.input.01.max", step = 4)
        
        debug.fun(state = "make_option_UI", obj = "update$date.select.input.05", step = 4)
        debug.fun(state = "make_option_UI", obj = "update$date.range.input.05.min", step = 4)
        debug.fun(state = "make_option_UI", obj = "update$date.range.input.05.max", step = 4)
        
        value$date.number.input <- isolate(update$date.number.input)
        value$date.select.input.01 <- isolate(update$date.select.input.01)
        value$date.range.input.01.min <- isolate(update$date.range.input.01.min)
        value$date.range.input.01.max <- isolate(update$date.range.input.01.max)
        value$date.select.input.02 <- isolate(update$date.select.input.02)
        value$date.range.input.02.min <- isolate(update$date.range.input.02.min)
        value$date.range.input.02.max <- isolate(update$date.range.input.02.max)
        value$date.select.input.03 <- isolate(update$date.select.input.03)
        value$date.range.input.03.min <- isolate(update$date.range.input.03.min)
        value$date.range.input.03.max <- isolate(update$date.range.input.03.max)
        value$date.select.input.04 <- isolate(update$date.select.input.04)
        value$date.range.input.04.min <- isolate(update$date.range.input.04.min)
        value$date.range.input.04.max <- isolate(update$date.range.input.04.max)
        value$date.select.input.05 <- isolate(update$date.select.input.05)
        value$date.range.input.05.min <- isolate(update$date.range.input.05.min)
        value$date.range.input.05.max <- isolate(update$date.range.input.05.max)
        
        output$date.select <-  renderUI({
                if(tab$name == "basicProductSales"){
                        box(width = NULL, solidHeader = TRUE, collapsible = TRUE,
                            title = sprintf("%s 기준", last.batch.date),
                            sliderInput("date.number.input", label = "기간 개수",
                                        min = 1, max = 5,
                                        value = value$date.number.input),
                            selectizeInput("date.select.input.01", label = "기간 범위 1", 
                                           choices = list("1-week" = 1, "2-week" = 2, "3-week" = 3, "1-month" = 4, "3-month" = 5, "6-month" = 6, "1-year" = 7, "2-year" = 8, "직접 입력" = 9),
                                           selected = value$date.select.input.01),
                            conditionalPanel(
                                    condition = "input['date.select.input.01'] == 9",
                                    dateRangeInput("date.range.input.01", "직접 입력 1", start = value$date.range.input.01.min, end = value$date.range.input.01.max, min = NULL,
                                                   max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                                   language = "en", separator = " to ", width = NULL)
                            ),
                            conditionalPanel(
                                    condition = "input['date.select.input.01'] != 9",
                                    uiOutput("date.selected.01"),
                                    tags$hr(style="border-color: purple;")
                            ),
                            conditionalPanel(
                                    condition = "input['date.number.input'] >= 2",
                                    selectizeInput("date.select.input.02", label = "기간 범위 2", 
                                                   choices = list("1-week" = 1, "2-week" = 2, "3-week" = 3, "1-month" = 4, "3-month" = 5, "6-month" = 6, "1-year" = 7, "2-year" = 8, "직접 입력" = 9),
                                                   selected = value$date.select.input.02),
                                    conditionalPanel(
                                            condition = "input['date.select.input.02'] == 9",
                                            dateRangeInput("date.range.input.02", "직접 입력2", start = value$date.range.input.02.min, end = value$date.range.input.02.max, min = NULL,
                                                           max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                                           language = "en", separator = " to ", width = NULL)
                                    ),
                                    conditionalPanel(
                                            condition = "input['date.select.input.02'] != 9",
                                            uiOutput("date.selected.02"),
                                            tags$hr(style="border-color: purple;")
                                    )
                            ),
                            conditionalPanel(
                                    condition = "input['date.number.input'] >= 3",
                                    selectizeInput("date.select.input.03", label = "기간 범위 3", 
                                                   choices = list("1-week" = 1, "2-week" = 2, "3-week" = 3, "1-month" = 4, "3-month" = 5, "6-month" = 6, "1-year" = 7, "2-year" = 8, "직접 입력" = 9),
                                                   selected = value$date.select.input.03),
                                    conditionalPanel(
                                            condition = "input['date.select.input.03'] == 9",
                                            dateRangeInput("date.range.input.03", "직접 입력 3", start = value$date.range.input.03.min, end = value$date.range.input.03.max, min = NULL,
                                                           max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                                           language = "en", separator = " to ", width = NULL)
                                    ),
                                    conditionalPanel(
                                            condition = "input['date.select.input.03'] != 9",
                                            uiOutput("date.selected.03"),
                                            tags$hr(style="border-color: purple;")
                                    )
                            ),
                            conditionalPanel(
                                    condition = "input['date.number.input'] >= 4",
                                    selectizeInput("date.select.input.04", label = "기간 범위 4", 
                                                   choices = list("1-week" = 1, "2-week" = 2, "3-week" = 3, "1-month" = 4, "3-month" = 5, "6-month" = 6, "1-year" = 7, "2-year" = 8, "직접 입력" = 9),
                                                   selected = value$date.select.input.04),
                                    conditionalPanel(
                                            condition = "input['date.select.input.04'] == 9",
                                            dateRangeInput("date.range.input.04", "직접 입력 4", start = value$date.range.input.04.min, end = value$date.range.input.04.max, min = NULL,
                                                           max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                                           language = "en", separator = " to ", width = NULL)
                                    ),
                                    conditionalPanel(
                                            condition = "input['date.select.input.04'] != 9",
                                            uiOutput("date.selected.04"),
                                            tags$hr(style="border-color: purple;")
                                    )
                            ),
                            conditionalPanel(
                                    condition = "input['date.number.input'] >= 5",
                                    selectizeInput("date.select.input.05", label = "기간 범위 5", 
                                                   choices = list("1-week" = 1, "2-week" = 2, "3-week" = 3, "1-month" = 4, "3-month" = 5, "6-month" = 6, "1-year" = 7, "2-year" = 8, "직접 입력" = 9),
                                                   selected = value$date.select.input.05),
                                    conditionalPanel(
                                            condition = "input['date.select.input.05'] == 9",
                                            dateRangeInput("date.range.input.05", "직접 입력 5", start = value$date.range.input.05.min, end = value$date.range.input.05.max, min = NULL,
                                                           max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                                           language = "en", separator = " to ", width = NULL)
                                    ),
                                    conditionalPanel(
                                            condition = "input['date.select.input.05'] != 9",
                                            uiOutput("date.selected.05"),
                                            tags$hr(style="border-color: purple;")
                                    )
                            )
                        )
                }
        })
})

observeEvent({
        input$sidebar.menu
        input$date.select.input.01
}, {
        output$date.selected.01 <- renderText({
                date.selected <- isolate(update$date.select.input.01)
                
                if(date.selected == 1){
                        start.date <- last.batch.date - weeks(1) + days(1)
                }else if(date.selected == 2){
                        start.date <- last.batch.date - weeks(2) + days(1)
                }else if(date.selected == 3){
                        start.date <- last.batch.date - weeks(3) + days(1)
                }else if(date.selected == 4){
                        start.date <- last.batch.date - months(1)
                }else if(date.selected == 5){
                        start.date <- last.batch.date - months(3)
                }else if(date.selected == 6){
                        start.date <- last.batch.date - months(6)
                }else if(date.selected == 7){
                        start.date <- last.batch.date - years(1) 
                }else if(date.selected == 8){
                        start.date <- last.batch.date - years(2) 
                }else if(date.selected == 9){ # 초기값이 9여서 UI 생성시에 오류가 나는 경우만
                        start.date <- last.batch.date
                }
                sprintf("선택된 기간 : %s ~ %s", start.date, last.batch.date)
        })
})

observeEvent({
        input$sidebar.menu
        input$date.select.input.02
}, {
        output$date.selected.02 <- renderText({
                date.selected <- isolate(update$date.select.input.02)
                
                if(date.selected == 1){
                        start.date <- last.batch.date - weeks(1) + days(1)
                }else if(date.selected == 2){
                        start.date <- last.batch.date - weeks(2) + days(1)
                }else if(date.selected == 3){
                        start.date <- last.batch.date - weeks(3) + days(1)
                }else if(date.selected == 4){
                        start.date <- last.batch.date - months(1)
                }else if(date.selected == 5){
                        start.date <- last.batch.date - months(3)
                }else if(date.selected == 6){
                        start.date <- last.batch.date - months(6)
                }else if(date.selected == 7){
                        start.date <- last.batch.date - years(1) 
                }else if(date.selected == 8){
                        start.date <- last.batch.date - years(2) 
                }
                sprintf("선택된 기간 : %s ~ %s", start.date, last.batch.date)
        })
})

observeEvent({
        input$sidebar.menu
        input$date.select.input.03
}, {
        output$date.selected.03 <- renderText({
                date.selected <- isolate(update$date.select.input.03)
                
                if(date.selected == 1){
                        start.date <- last.batch.date - weeks(1) + days(1)
                }else if(date.selected == 2){
                        start.date <- last.batch.date - weeks(2) + days(1)
                }else if(date.selected == 3){
                        start.date <- last.batch.date - weeks(3) + days(1)
                }else if(date.selected == 4){
                        start.date <- last.batch.date - months(1)
                }else if(date.selected == 5){
                        start.date <- last.batch.date - months(3)
                }else if(date.selected == 6){
                        start.date <- last.batch.date - months(6)
                }else if(date.selected == 7){
                        start.date <- last.batch.date - years(1) 
                }else if(date.selected == 8){
                        start.date <- last.batch.date - years(2) 
                }
                sprintf("선택된 기간 : %s ~ %s", start.date, last.batch.date)
        })
})

observeEvent({
        input$sidebar.menu
        input$date.select.input.04
}, {
        output$date.selected.04 <- renderText({
                date.selected <- isolate(update$date.select.input.04)
                
                if(date.selected == 1){
                        start.date <- last.batch.date - weeks(1) + days(1)
                }else if(date.selected == 2){
                        start.date <- last.batch.date - weeks(2) + days(1)
                }else if(date.selected == 3){
                        start.date <- last.batch.date - weeks(3) + days(1)
                }else if(date.selected == 4){
                        start.date <- last.batch.date - months(1)
                }else if(date.selected == 5){
                        start.date <- last.batch.date - months(3)
                }else if(date.selected == 6){
                        start.date <- last.batch.date - months(6)
                }else if(date.selected == 7){
                        start.date <- last.batch.date - years(1) 
                }else if(date.selected == 8){
                        start.date <- last.batch.date - years(2) 
                }
                sprintf("선택된 기간 : %s ~ %s", start.date, last.batch.date)
        })
})

observeEvent({
        input$sidebar.menu
        input$date.select.input.05
}, {
        output$date.selected.05 <- renderText({
                date.selected <- isolate(update$date.select.input.05)
                
                if(date.selected == 1){
                        start.date <- last.batch.date - weeks(1) + days(1)
                }else if(date.selected == 2){
                        start.date <- last.batch.date - weeks(2) + days(1)
                }else if(date.selected == 3){
                        start.date <- last.batch.date - weeks(3) + days(1)
                }else if(date.selected == 4){
                        start.date <- last.batch.date - months(1)
                }else if(date.selected == 5){
                        start.date <- last.batch.date - months(3)
                }else if(date.selected == 6){
                        start.date <- last.batch.date - months(6)
                }else if(date.selected == 7){
                        start.date <- last.batch.date - years(1) 
                }else if(date.selected == 8){
                        start.date <- last.batch.date - years(2) 
                }
                sprintf("선택된 기간 : %s ~ %s", start.date, last.batch.date)
        })
})

#### user input 변수로 저장하기
# 자주 쓰는 format
observeEvent({
        input$sidebar.menu
}, {
        
})

#
observeEvent({
        input$user.type.input
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$user.type.input", msg = "varSaving", step = 4)
        
        update$user.type.input <- input$user.type.input
})

#
observeEvent({
        input$date.unit.input
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$date.unit.input", msg = "varSaving", step = 4)
        
        update$date.unit.input <- input$date.unit.input
})

#
observeEvent({
        input$data.type.input
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$data.type.input", msg = "varSaving", step = 4)
        
        update$data.type.input <- input$data.type.input
})

#
observeEvent({
        input$data.measure.input
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$data.measure.input", msg = "varSaving", step = 4)
        
        update$data.measure.input <- input$data.measure.input
})

#
observeEvent({
        input$facet.option.input
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$facet.option.input", msg = "varSaving", step = 4)
        
        update$facet.option.input <- input$facet.option.input
})

#
observeEvent({
        input$date.number.input
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$date.number.input", msg = "varSaving", step = 4)
        
        update$date.number.input <- input$date.number.input
})

#
observeEvent({
        input$date.select.input.01
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$date.select.input.01", msg = "varSaving", step = 4)
        
        update$date.select.input.01 <- input$date.select.input.01
})

#
observeEvent({
        input$date.range.input.01
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$date.range.input.01[1]", msg = "varSaving", step = 4)
        debug.fun(state = "make_option_UI_varSaving", obj = "update$date.range.input.01[2]", msg = "varSaving", step = 4)
        
        update$date.range.input.01.min <- input$date.range.input.01[1]
        update$date.range.input.01.max <- input$date.range.input.01[2]
})

#
observeEvent({
        input$date.select.input.02
}, {
        update$date.select.input.02 <- input$date.select.input.02
})

#
observeEvent({
        input$date.range.input.02
}, {
        update$date.range.input.02.min <- input$date.range.input.02[1]
        update$date.range.input.02.max <- input$date.range.input.02[2]
})

#
observeEvent({
        input$date.select.input.03
}, {
        update$date.select.input.03 <- input$date.select.input.03
})

#
observeEvent({
        input$date.range.input.03
}, {
        update$date.range.input.03.min <- input$date.range.input.03[1]
        update$date.range.input.03.max <- input$date.range.input.03[2]
})

#
observeEvent({
        input$date.select.input.04
}, {
        update$date.select.input.04 <- input$date.select.input.04
})

#
observeEvent({
        input$date.range.input.04
}, {
        update$date.range.input.04.min <- input$date.range.input.04[1]
        update$date.range.input.04.max <- input$date.range.input.04[2]
})

#
observeEvent({
        input$date.select.input.05
}, {
        update$date.select.input.05 <- input$date.select.input.05
})

#
observeEvent({
        input$date.range.input.05
}, {
        update$date.range.input.05.min <- input$date.range.input.05[1]
        update$date.range.input.05.max <- input$date.range.input.05[2]
})

#
observeEvent({
        input$user.input
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$user.input", msg = "varSaving", step = 4)
        
        update$user.input <- input$user.input
}, ignoreNULL = FALSE)

observeEvent({
        input$user.select.input
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$user.select.input", msg = "varSaving", step = 4)
        
        update$user.select.input <- input$user.select.input
        
        debug.fun(state = "make_option_UI_varSaving", obj = "input$user.select.input", msg = "varSaving", step = 4)
}, ignoreNULL = FALSE)

#
observeEvent({
        input$user.group.box.input
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$user.group.box.input", msg = "varSaving", step = 4)
        
        update$user.group.box.input <- input$user.group.box.input
}, ignoreNULL = FALSE)

#
observeEvent({
        input$user.grade.box.input
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$user.grade.box.input", msg = "varSaving", step = 4)
        
        update$user.grade.box.input <- input$user.grade.box.input
}, ignoreNULL = FALSE)

#
observeEvent({
        input$user.age.box.input
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$user.age.box.input", msg = "varSaving", step = 4)
        
        update$user.age.box.input <- input$user.age.box.input
}, ignoreNULL = FALSE)

#
observeEvent({
        input$product.group.input
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$product.group.input", msg = "varSaving", step = 4)
        
        update$product.group.input <- input$product.group.input
}, ignoreNULL = FALSE)

#
observeEvent({
        input$product.group.box.input
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$product.group.box.input", msg = "varSaving", step = 4)
        
        update$product.group.box.input <- input$product.group.box.input
}, ignoreNULL = FALSE)

#
observeEvent({
        input$category.input
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$category.input", msg = "varSaving", step = 4)
        
        update$category.input <- input$category.input
}, ignoreNULL = FALSE)

#
observeEvent({
        input$goods.select.input
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$goods.select.input", msg = "varSaving", step = 4)
        
        update$goods.select.input <- input$goods.select.input
}, ignoreNULL = FALSE)

#
observeEvent({
        input$channal.box.input
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$channal.box.input", msg = "varSaving", step = 4)
        
        update$channal.box.input <- input$channal.box.input
}, ignoreNULL = FALSE)

#
observeEvent({
        input$category.box.input
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$category.box.input", msg = "varSaving", step = 4)
        
        update$category.box.input <- input$category.box.input
}, ignoreNULL = FALSE)

#
observeEvent({
        input$category.pov.box.input
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$category.pov.box.input", msg = "varSaving", step = 4)
        
        update$category.pov.box.input <- input$category.pov.box.input
}, ignoreNULL = FALSE)

#
observeEvent({
        input$brand.box.input
}, {
        debug.fun(state = "make_option_UI_varSaving", obj = "update$brand.box.input", msg = "varSaving", step = 4)
        
        update$brand.box.input <- input$brand.box.input
}, ignoreNULL = FALSE)
