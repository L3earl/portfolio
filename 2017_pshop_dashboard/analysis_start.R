if(tab$name == "basicProductSales"){
        # tab name 에 따라 달라지는 내용들 초기화
        
        # progress bar UI
        progress.value$max <- 14 # 최대 progress
        progress <- Progress$new(max = progress.value$max)
        
        # UI 선택(# 선택된 매출/주문 건수/수량 의 총합/ 평균 조건) 에 따라 달라지는 데이터들
        case.table <- list('1' = list('1' = c("sales", "매출 총합"),
                                      '2' = c("sales.per.goods.type", "주문한 상품 종류 1개당 평균 매출"),
                                      '3' = c("sales.per.user", "고객 1인당 평균 매출"),
                                      '4' = c("sales.per.order", "주문 1건당 평균 매출"),
                                      '5' = c("sales.per.line", "라인 1건당 평균 매출"),
                                      '6' = c("sales.per.goods", "주문한 상품 개수 1개당 평균 매출 (상품 평균 가격)")),
                           '2' = list('1' = c("order.num", "주문 건수"),
                                      '2' = c("order.num.per.goods.type", "주문한 상품 종류 1개당 평균 주문 건수"),
                                      '3' = c("order.num.per.user", "고객 1인당 평균 주문 건수")),
                           '3' = list('1' = c("line.num", "라인 수"),
                                      '2' = c("line.per.goods.type", "주문한 상품 종류 1개당 평균 라인 수"),
                                      '3' = c("line.per.user", "고객 1인당 평균 라인 수"),
                                      '4' = c("line.per.order", "주문 1건당 평균 라인 수")),
                           '4' = list('1' = c("goods.num", "주문한 상품 개수"),
                                      '2' = c("goods.num.per.goods.type", "주문한 상품 종류 1개당 주문한 상품 개수"),
                                      '3' = c("goods.num.per.user", "고객 1인당 주문한 상품 개수"),
                                      '4' = c("goods.num.per.order", "주문 1건당 평균 주문한 상품 개수"),
                                      '5' = c("goods.num.per.line", "라인 1건당 평균 주문한 상품 개수")),
                           '5' = list('1' = c("goods.type.per.user", "주문한 1인당 평균 주문한 상품 종류 수"))
        )
}