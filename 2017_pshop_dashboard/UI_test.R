tryCatch({
        if(tab$name == "basicProductSales"){
                #### UI 단위 테스트, 분류 선택 했는지 안 했는지 확인
                progress.value$now <- progress.value$now + 1
                progress$set(message = "UI 테스트", value = progress.value$now)
                debug.fun(state = "progress", obj = "progress.value$now", step = 4)
                
                # 유저 분류 UI 테스트
                if(1 %in% input$user.input & is.null(input$user.group.box.input) & 2 %in% update$user.type.input){ # 회원 그룹이 선택됨
                        showNotification(ui = "회원 분류 선택 안됨(그룹)",
                                         type = "error",
                                         duration = 20)
                }
                
                if(2 %in% input$user.input & is.null(input$user.grade.box.input) & 2 %in% update$user.type.input){ # 회원 등급이 선택됨
                        showNotification(ui = "회원 분류 선택 안됨(등급)",
                                         type = "error",
                                         duration = 20)
                }
                
                if(3 %in% input$user.input & is.null(input$user.age.box.input) & 2 %in% update$user.type.input){ # 회원 연령이 선택됨
                        showNotification(ui = "회원 분류 선택 안됨(연령)",
                                         type = "error",
                                         duration = 20)
                }
                
                # 상품 분류 UI 테스트
                if(2 %in% input$category.input & is.null(input$channal.box.input)){ # 채널
                        showNotification(ui = "상품 분류 선택 안됨(채널)",
                                         type = "error",
                                         duration = 20)
                }
                
                if(3 %in% input$category.input & is.null(input$category.box.input)){ # 카테고리
                        showNotification(ui = "상품 분류 선택 안됨(카테고리)",
                                         type = "error",
                                         duration = 20)
                }
                
                if(4 %in% input$category.input & is.null(input$category.pov.box.input)){ # pov(사용안함) 카테고리
                        showNotification(ui = "상품 분류 선택 안됨(POV 카테고리)",
                                         type = "error",
                                         duration = 20)
                }
                
                if(5 %in% input$category.input & is.null(input$brand.box.input)){ # 브랜드
                        showNotification(ui = "상품 분류 선택 안됨(브랜드)",
                                         type = "error",
                                         duration = 20)
                }
                
                # 그래프 나누기 UI 테스트
                if(input$facet.option.input == 2 & input$date.number.input == 1){ # 기간
                        showNotification(ui = "그래프를 나눌 수 없습니다(기간 1개 선택됨)",
                                         type = "error",
                                         duration = 20)
                        
                }else if(input$facet.option.input == 3 & 2 %out% input$category.input){ # 채널
                        showNotification(ui = "그래프를 나눌 수 없습니다(채널 선택 안됨)",
                                         type = "error",
                                         duration = 20)
                        
                }else if(input$facet.option.input == 4 & 3 %out% input$category.input){ # 카테고리
                        showNotification(ui = "그래프를 나눌 수 없습니다(카테고리 선택 안됨)",
                                         type = "error",
                                         duration = 20)
                        
                }else if(input$facet.option.input == 5 & 4 %out% input$category.input){ # pov(사용안함) 카테고리
                        showNotification(ui = "그래프를 나눌 수 없습니다(POV 카테고리 선택 안됨)",
                                         type = "error",
                                         duration = 20)
                        
                }else if(input$facet.option.input == 6 & 5 %out% input$category.input){ # 브랜드
                        showNotification(ui = "그래프를 나눌 수 없습니다(브랜드 선택 안됨)",
                                         type = "error",
                                         duration = 20)
                        
                }else if(input$facet.option.input == 7 & 1 %out% input$user.input){ # 회원 그룹
                        showNotification(ui = "그래프를 나눌 수 없습니다(회원 그룹 선택 안됨)",
                                         type = "error",
                                         duration = 20)
                        
                }else if(input$facet.option.input == 8 & 2 %out% input$user.input){ # 회원 등급
                        showNotification(ui = "그래프를 나눌 수 없습니다(회원 등급 선택 안됨)",
                                         type = "error",
                                         duration = 20)
                        
                }else if(input$facet.option.input == 9 & 3 %out% input$user.input){ # 회원 연령
                        showNotification(ui = "그래프를 나눌 수 없습니다(회원 연령 선택 안됨)",
                                         type = "error",
                                         duration = 20)
                        
                }else if(input$facet.option.input == 10 & is.null(input$goods.select.input)){ # 상품 검색
                        showNotification(ui = "그래프를 나눌 수 없습니다(상품 선택 안됨)",
                                         type = "error",
                                         duration = 20)
                }
        }
},
error = function(e) print(paste0("UI_test /// ", e)),
#warning = function(w) print(w),
finally = NULL)




