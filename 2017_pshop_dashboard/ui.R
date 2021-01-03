

header <- dashboardHeader(title = uploadTime) ## "pshop DB 분석" todo

# 나중에 시간되면 아이콘들 변경할 것 
sidebar <- dashboardSidebar(
        sidebarMenuOutput("sidebar")
)

body <- dashboardBody(
        useShinyjs(),
        uiOutput("tab.items")
)

dashboardPage(header, sidebar, body)

