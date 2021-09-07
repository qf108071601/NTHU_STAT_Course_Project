
## 讀多個xlsx的sheet 

library(openxlsx)
library(tidyverse)
library(purrr)
library(tibble)
library(qs)


# filename <-"C:/Users/Lai/Desktop/統計學習報告/new.xlsx"
# sheets <- openxlsx::getSheetNames(filename)
# SheetList <- lapply(sheets,openxlsx::read.xlsx,xlsxFile=filename)
# names(SheetList) <- sheets
# qsave(SheetList,"C:/Users/Lai/Desktop/統計學習報告/data")


SheetList = qread("C:/Users/Lai/Desktop/統計學習報告/data")

SheetList_new = lapply(1:126, function(x){
  SheetList[[x]] %>% as.data.frame()
})

firm_name <- openxlsx::getSheetNames("C:/Users/Lai/Desktop/統計學習報告/new.xlsx")
names(SheetList_new) = firm_name

x = sapply(1:length(SheetList_new), function(a){
  strsplit(firm_name,split = ' ')[[a]][1]
})

# col = c("NET_REV",
#         "EV_TO_EBITA",
#         "CUR_RATIO",
#         "QUICK_RATIO",
#         "EV_TO_EBITA")



SheetList_new = lapply(1:126, function(x){
  data = SheetList_new[[x]] %>%
    purrr::map(~as.numeric(.)) %>%  
    as.data.frame()
  #data[,which(colnames(data) %in% col==F)]
})

firm = c()
# 每個變數缺多少比例資料
# 除了Date之外的變數NA情況
firm_index = sapply(1:126, function(a){
  
  x = SheetList_new[[a]] %>% purrr::map(~as.numeric(.)) %>%
    as.data.frame()%>%  
    map(~sum(is.na(.))/length(.)) %>% 
    as.data.frame()
  
  if (median(as.numeric(x[1,-1])) >0.2){
    firm[a] = "Delete" #超過5成都是NA
    
  }else if (length(which(x[1,-1] < 0.2))< 16){
    firm[a] = "Specific" # 特定幾行超過是NA
  }else{
    firm[a] = "Normal" #極少部分是NA
  }
})

Sp = which(firm_index=="Normal") %>% as.numeric()

Normal = cbind(SheetList_new[[Sp[1]]],Firm = x[1])

for (i in 2:length(Sp)) {
  y = cbind(SheetList_new[[Sp[i]]],Firm = x[i])
  Normal = rbind(Normal,y)
}


source("C:/Users/Lai/Desktop/統計學習報告/取產業別ticker.R")

y = sapply(1:length(Ticker$Firm), function(a){
  strsplit(Ticker$Firm,split = ' ')[[a]][1]
})

Ticker = select(Ticker,-Firm)
Ticker$Firm = y

x = merge(Normal,Ticker)

table = x %>% group_by(Industry,Firm) %>%
  summarise(table(Firm))
x$Industry = as.factor(x$Industry)

Technology = x[which(x$Industry=="Technology "),]


library(VIM)
aggr_plot <- aggr(t, col=c('navyblue','yellow'), 
                  numbers=TRUE, sortVars=TRUE, labels=names(data), 
                  cex.axis=.7, gap=3, ylab=c("Hist of missing data","Pattern"))



