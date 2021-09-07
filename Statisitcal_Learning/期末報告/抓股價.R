## 抓股價
require(PerformanceAnalytics)  
library(data.table)  
library(dplyr)  
library(tibble)  
library(TTR)  
library(tidyr)  
library(tidyquant)  
library(tsfeatures)  
library(rsample)  
library(purrr)  
library(stringr)  
library(tibbletime) # tsibble clashes with the base R index() function  
library(xgboost)  
library(rvest)  
library(quantmod)
library(tidyverse)

Company = read.csv("C:/Users/Lai/Desktop/Company.csv")
Company = unique(Company)
names(Company) = "ticker"

Ticker = read.csv("C:/Users/Lai/Desktop/統計學習報告/最後用到/sp500_ticker_category.csv")
Ticker = select(Ticker,ticker,category)
# names(Company) = c("Company","Industry")
# 
# 
# Company = gsub(" ","",Company$Company)
# 
# C_S = sapply(1:length(Company), function(a){
#   strsplit(Company,split = "U")[[a]][1]
# })

C_S = Company$ticker

dataEnv <- new.env()  
getSymbols(C_S,
           from = "2008-01-01",
           to = Sys.Date(),
           src = "yahoo",
           env = dataEnv)



df <- eapply(dataEnv, function(x){  
  as.data.frame(x) %>%  
    rename_all(function(n){  
      gsub("^(\\w+)\\.", "", n, perl = TRUE)  #將資料清理存成dataframe
    }  
    ) %>%  
    rownames_to_column("date")   #row名稱
}) %>%  
  rbindlist(idcol = TRUE) %>%      #自動列出.id 
  mutate(date = as.Date(date)) %>%  
  group_by(.id) %>%  
  tq_mutate(   #可以用來算各種值,指標 (periodReturn)
    select = Adjusted,  
    mutate_fun = periodReturn,  
    period = "daily",  
    type = "arithmetic"  
  ) %>% 
  select("date", ".id", "Adjusted", "daily.returns", "Open", "High", "Low", "Close") 

# Company_2 = read.csv("C:/Users/Lai/Desktop/Company_Choose.csv",header = T)
# answer = cbind(C_S,Company_2$V1) %>% as.data.frame()
# names(answer) = c(".id","Industry")
names(Ticker) = c(".id","Industry")

df = merge(df,Ticker)



#write.csv(df,"C:/Users/Lai/Desktop/stock_select.csv")









