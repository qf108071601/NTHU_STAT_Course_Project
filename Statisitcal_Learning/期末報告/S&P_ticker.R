library(openxlsx)
library(tidyverse)
library(purrr)
library(tibble)
library(qs)


filename <-"C:/Users/Lai/Desktop/統計學習報告/sp500.xlsx"
sheets <- openxlsx::getSheetNames(filename)
SheetList <- lapply(sheets,openxlsx::read.xlsx,xlsxFile=filename)
names(SheetList) <- sheets

d = sapply(1:9, function(a){
  cbind(Firm = SheetList[[a]][,1],Industry = sheets[a])
})

data = d[[1]]
for (i in 2:9) {
  data = rbind(data,d[[i]])
}
# write.csv(data,"C:/Users/Lai/Desktop/sp500_ticker.csv")        






filename <-"C:/Users/Lai/Desktop/統計學習報告/company ticker.xlsx"
sheets <- openxlsx::getSheetNames(filename)
SheetList <- lapply(sheets,openxlsx::read.xlsx,xlsxFile=filename)
names(SheetList) <- sheets

c = sapply(2:12, function(a){
  cbind(Firm = SheetList[[a]][,c(1,2)],Industry = sheets[a])
})


cc = cbind(c[[1]],c[[2]],c[[3]]) %>% as.data.frame()
for (i in 1:10) {
  cc = rbind(cc,cbind(c[[1+3*i]],c[[2+3*i]],c[[3+3*i]]))
}


sp_name = sapply(1:492, function(a){
  strsplit(data[,1]," ")[[a]][1]
})
data = cbind(data,sp_name) %>% as.data.frame()

cc$V1 %in% sp_name %>% table() #有重複縮寫
sp_name %in% cc$V1 %>% table()
# names(cc) = c("sp_name","Category","Industry")
# data_in = data[which(sp_name %in% cc$sp_name & data$Industry %in% cc$Industry),]

#取出重複字的
t = cc$V1 %>% table() %>% as.data.frame()
replicate = t[which(t$Freq==2),1] %>% as.character()

cc = cc[!(cc$V1 %in% replicate),]

cc$V1 %in% sp_name %>% table() #沒重複的了
sp_name %in% cc$V1 %>% table()


t = cc[which(cc$V1 %in% sp_name),c(1,2)]
names(t) = c("sp_name","Category")
data_in = data[which(data$sp_name %in% cc$V1),]
tt = merge(data_in,t)


write.csv(tt,"C:/Users/Lai/Desktop/sp500_ticker_category.csv")        


























