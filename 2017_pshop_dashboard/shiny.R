
library(methods)

tryCatch({
        expr = {
                
                options(repos="https://CRAN.R-project.org")
                                
                library(rsconnect)
                library(lubridate)                
                
                appDir = "D:/googledrive/work/4.upside/dashboard/pshop/dashboard"
                
                rsconnect::setAccountInfo(name='smartmiew-test', 
                                          token='38255982BAFDB25E3698377034C61F1C', 
                                          secret='7yDyax8/0uaYP8fR58hPtVaY7MafSvY/oHFaeViT')
                options(encoding = "UTF-8")
                
                rsconnect::deployApp(appDir = appDir,
                                     appFiles = c("server.R", "ui.R", "global.R",
                                                  "analysis_start.R", "data_filtering.R", "date_measure.R",
                                                  "facet.R", "graph.R", "make_body.R", 
                                                  "make_optionUI.R", "make_sidebar.R", "make_table.R",
                                                  "table.R", "UI_test.R",
                                                  "./data/time.RDS", #######
                                                  "./data/goods_eval.feather",
                                                  "./data/goods.feather", 
                                                  "./data/member_class.feather",
                                                  "./data/member_entire.feather", 
                                                  "./data/order_detail.feather",
                                                  "./data/order.feather",
                                                  "./data/product_class.feather",
                                                  "./data/google_sheet_auth.rds",
                                                  "./data/last_batch_date.RDS"),
                                     account = "smartmiew-test", server = "shinyapps.io", appName = "dashboard") 
        }
},
error = function(e) saveRDS(e, "error.RDS"),
#warning = function(w) saveRDS(w, "warning.RDS"),
finally = NULL)


