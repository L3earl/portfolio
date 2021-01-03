# 주의! UI가 생성되는 것은 if 문이 아닌 insertUI로 해야 오류가 안 나는 듯함
output$tab.items <- renderUI({
        
        if(tab$name == "basicProductSales"){ # 매출/ 주문 건수/ 주문 수량
                tabItems(
                        tabItem(
                                tabName = paste0(tab$name),
                                class = "active",
                                fluidRow(
                                        column(width = 9,
                                               uiOutput("channal.box"),
                                               uiOutput("category.box"),
                                               #uiOutput("category.pov.box"), # 사용 안함
                                               uiOutput("brand.box"),
                                               #uiOutput("product.group.box"),
                                               uiOutput("user.group.box"),
                                               uiOutput("user.grade.box"),
                                               uiOutput("user.age.box"),
                                               uiOutput("graph"),
                                               uiOutput("data.table"),
                                               uiOutput("data.table.sum")
                                        ),
                                        column(width = 3,
                                               uiOutput("data.option"),
                                               uiOutput("category"),
                                               #uiOutput("product.group"),
                                               uiOutput("user"),
                                               uiOutput("date.select")
                                        )
                                )
                        )
                )
        }else if(tab$name == "memberGroupModify"){ 
                tabItems(
                        tabItem(
                                tabName = paste0(tab$name),
                                class = "active",
                                fluidRow(
                                        column(width = 12,
                                               uiOutput("user.group.addBox"),
                                               fluidRow(
                                                       column(width = 3,
                                                              uiOutput("user.group.add")
                                                       ),
                                                       column(width = 3,
                                                              uiOutput("user.group.add.example")
                                                       ),
                                                       tags$div(id = 'placeholder')
                                               )
                                        )
                                )
                        )
                )
        }
})

observeEvent({
        input$sidebar.menu
}, {
        tab$name <- input$sidebar.menu
})
