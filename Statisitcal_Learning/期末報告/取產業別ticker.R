
library(openxlsx)
library(tidyverse)
library(purrr)
library(tibble)
library(qs)

filename <-"C:/Users/Lai/Desktop/統計學習報告/SP100-產業別.xlsx"
sheets <- openxlsx::getSheetNames(filename)
Industry <- lapply(sheets,openxlsx::read.xlsx,xlsxFile=filename)
names(Industry) <- sheets

Industry_new = lapply(2:11, function(x){
  Industry[[x]] %>% as.data.frame()
})

sheets = sapply(2:11, function(a){
  strsplit(sheets[[a]],split = '\\(')[[1]][1]
})

Ticker = data.frame(Firm = Industry_new[[1]][,1][-1],Industry = sheets[1])

for (a in 2:10) {
  x = data.frame(Firm = Industry_new[[a]][,1][-1],Industry = sheets[a])
  Ticker = rbind(Ticker,x)
  }  



