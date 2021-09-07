library(openxlsx)
library(tidyverse)
library(purrr)
library(tibble)
library(qs)


SheetList = qread("C:/Users/Lai/Desktop/data")
length(SheetList)

SheetList = lapply(1:length(SheetList), function(x){
  data = SheetList[[x]] %>%
    purrr::map(~as.numeric(.)) %>%  
    as.data.frame()
})

firm = c()

firm_index = sapply(1:length(SheetList), function(a){
  a=1
  x = SheetList[[1]] %>% purrr::map(~as.numeric(.)) %>%
    as.data.frame()%>%  
    map(~sum(is.na(.))/length(.)) %>% 
    as.data.frame()
  
  if (median(as.numeric(x[1,-1])) >0.3){ # い计>0.3:禬筁常0.3
    firm[a] = "Delete" #禬筁3Θ常琌NA
    
  }else if (length(which(x[1,-1] < 0.3))< ncol(x)){ # ぃ琌场常<0.3
    firm[a] = "Specific" # 疭﹚碭︽禬筁琌NA
  }else{
    firm[a] = "Normal" #伐ぶ场だ琌NA  
  } 
})
firm_index %>% table()

filename <-"C:/Users/Lai/Desktop/fuck4.xlsx"
sheets_name <- openxlsx::getSheetNames(filename)
names(SheetList) = sheets_name

Ticker = read.csv("C:/Users/Lai/Desktop/参璸厩策厨/sp500_ticker.csv")

sheets_name_1 = sapply(1:length(SheetList), function(a){
  strsplit(sheets_name,split = " ")[[a]][1]
})

sheets_name_1 %in% Ticker$ticker %>%table()

EPS = qread("C:/Users/Lai/Desktop/data2")

E = EPS[c(2:8,10:285)]
names(E) = sheets_name

for (i in 1:length(SheetList)) {
  SheetList[[i]]$EPS = lag(E[[i]]$Diluted.EPS[-1])
}  














