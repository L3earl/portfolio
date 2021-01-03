library(shiny)
library(shinydashboard)
library(ggplot2) 
library(plotly) 
library(scales)
library(zoo)
library(data.table)
library(rlang)
library(dplyr)
library(lubridate)
library(tidyr)
library(gtools)
library(shinyjs)
library(DT)
library(googlesheets)
library(feather)

############## function
# %in% 의 반대
`%out%` <- Negate(`%in%`) 

# 구글시트에서 고정 유저 그룹 데이터 가져오기 function
get.user.group <- function(){
        tryCatch({
                sheet_title <- gs_ls(regex = "^pshopUserGroup")$sheet_title
                
                for(i in 1:length(sheet_title)){
                        sheet_data <- sheet_title[i] %>% 
                                gs_title() %>% 
                                gs_read()
                        
                        for(j in 1:ncol(sheet_data)){
                                user_group_each <- sheet_data[j] %>% 
                                        filter(!is.na(.)) %>% 
                                        mutate(C.group.class = colnames(sheet_data[j])) %>% 
                                        setDT()
                                
                                colnames(user_group_each)[1] <- "CUSTOMER_NUM"
                                
                                if(nrow(user_group_each) == 0){
                                        get.user.group.error.log <<- append(get.user.group.error.log, 
                                                                           paste0("(", colnames(sheet_data[j]), ") 그룹에 데이터가 없거나 빈칸(스페이스바)이 입력되어 있습니다.")) 
                                        
                                }else if(class(user_group_each$CUSTOMER_NUM) != "integer"){
                                        get.user.group.error.log <<- append(get.user.group.error.log, 
                                                                           paste0("(", colnames(sheet_data[j]), ") 그룹에 숫자가 아닌 데이터가 있습니다."))
                                                                           
                                }else{
                                        if(j == 1 & i == 1){
                                                user_group <- user_group_each %>% 
                                                        setDT()
                                        }else{
                                                user_group <- user_group %>% 
                                                        bind_rows(user_group_each) %>% 
                                                        setDT()
                                        }
                                }
                        }
                }
        },
        error = function(e){
                get.user.group.error.log <<- append(get.user.group.error.log, 
                                                    "데이터를 가져오지 못하고 있습니다. 다시 시도해 주세요.")
                
                print(paste0("gs_ls (데이터를 불러오고 가공하는 과정에 문제) /// ", e))
        },
        finally = NULL)
        
        return(user_group)
}

############## data load #################################
data.path <- "./data/"  # "./dev_dashboard/data/"

# 배치 프로그램으로 데이터 가져온 시간 (테스트용. 프로젝트 끝날 때, 관련 내용 삭제할 것. UI에서도)
uploadTime <- readRDS(paste0(data.path, "time.RDS")) 

# 구글 sheet 권한 가져오기
tryCatch({
        gs_auth(readRDS(paste0(data.path, "google_sheet_auth.rds"))) # 권한 가져오기
},
error = function(e){
        print(paste0("gs_auth (구글 스프레드시트 권한 요청) /// ", e))
        
        # 권한 가져오기 실패 시, 구글 권한 다시 요청
        gs_auth(token = NULL, new_user = TRUE, verbose = T)
        
        google_sheet_auth <- gs_auth()
        saveRDS(google_sheet_auth, paste0(data.path, "google_sheet_auth.rds"))
},
finally = NULL)

# 구글 sheet에서 유저 그룹 가져옴
get.user.group.error.log <- c() # 에러 로그 초기화
user_group <- get.user.group()

# 에러 로그 있으면 프린트 함
if(length(get.user.group.error.log) > 0){
        for(i in get.user.group.error.log){
                print(paste0("구글 sheet 에러 로그 ///", i))
        }
}

# 유저 클래스 합침
member_class <- read_feather(paste0(data.path, "member_class.feather")) %>% 
        full_join(user_group, by = "CUSTOMER_NUM") %>%setDT()
member_entire <- read_feather(paste0(data.path, "member_entire.feather")) %>% setDT()
order <- read_feather(paste0(data.path, "order.feather")) %>% setDT()
order_detail <- read_feather(paste0(data.path, "order_detail.feather")) %>% setDT()
goods <- read_feather(paste0(data.path, "goods.feather")) %>% setDT()
goods_eval <- read_feather(paste0(data.path, "goods_eval.feather")) %>% setDT()
product_class <- read_feather(paste0(data.path, "product_class.feather")) %>% setDT()

# data 형 변환 
member_entire[, SITE_JOIN_DATE:= ymd(SITE_JOIN_DATE)]
member_class[, SITE_JOIN_DATE:= ymd(SITE_JOIN_DATE)]
order[, ORD_DT:= ymd(ORD_DT)]
goods_eval[, REG_DT:= ymd(REG_DT)]

############## settings #################################
# 디버그 
debug.mode <<- 'make_option_UI' # 4까지 있음. 숫자가 높아질수록 덜 중요한 것. "off" 로 끌 수 있음

# 마지막 배치 날짜 날짜 
last.batch.date <- readRDS(paste0(data.path, "last_batch_date.RDS"))

# tab UI 초기값 및 변수 세팅
tab <- reactiveValues()
tab$name <- "default" # if의 조건값으로 쓰이기 때문에, c()를 넣어놓으면 error가 출력됨

# input UI 초기값 세팅
update <- reactiveValues()

# input UI 변수 세팅 
value <- reactiveValues()

# progress bar 값 세팅
progress.value <- reactiveValues()
progress.value$now <- 0
progress.value$max <- c()

# 초기값 세팅
update$user.type.input <- 2
update$date.unit.input <- 1
update$data.type.input <- 1
update$data.measure.input <- 1
update$facet.option.input <- 1
update$date.number.input <- 1
update$date.select.input.01 <- 8
update$date.range.input.01.min <- last.batch.date - months(1)
update$date.range.input.01.max <- last.batch.date
update$date.select.input.02 <- 9
update$date.range.input.02.min <- last.batch.date - months(2)
update$date.range.input.02.max <- last.batch.date
update$date.select.input.03 <- 9
update$date.range.input.03.min <- last.batch.date - months(3)
update$date.range.input.03.max <- last.batch.date
update$date.select.input.04 <- 9
update$date.range.input.04.min <- last.batch.date - months(4)
update$date.range.input.04.max <- last.batch.date
update$date.select.input.05 <- 9
update$date.range.input.05.min <- last.batch.date - months(5)
update$date.range.input.05.max <- last.batch.date

update$product.input <- FALSE

update$category.input <- c()
update$goods.select.input <- c()

update$user.input <- c()
update$user.group.box.input <- FALSE
update$user.grade.box.input <- FALSE
update$user.age.box.input <- FALSE
update$user.select.input <- c()

update$channal.box.input <- FALSE
update$category.box.input <- FALSE
update$category.pov.box.input <- FALSE
update$brand.box.input <- FALSE

update$product.group.input <- 1
update$product.group.box.input <- FALSE


