
output$sidebar <- renderMenu({
        sidebarMenu(id = "sidebar.menu",
                    menuItem("매출 및 주문 (상품)", tabName = "basicProductSales", icon = icon("dashboard"))
        )
})