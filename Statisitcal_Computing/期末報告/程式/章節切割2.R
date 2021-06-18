library(pdftools)
library(tidyverse)
x = list()
x[[1]] = pdf_text("D:/Downloads/Technical Analysis of Stock Trends.pdf")[44:49] %>%
  readr::read_lines() #open the PDF inside your project folder
x[[2]] = pdf_text("D:/Downloads/Technical Analysis of Stock Trends.pdf")[50:53] %>%
  readr::read_lines() #open the PDF inside your project folder
x[[3]] = pdf_text("D:/Downloads/Technical Analysis of Stock Trends.pdf")[54:65] %>%
  readr::read_lines() #open the PDF inside your project folder
x[[4]] = pdf_text("D:/Downloads/Technical Analysis of Stock Trends.pdf")[66:85] %>%
  readr::read_lines() #open the PDF inside your project folder
x[[5]] = pdf_text("D:/Downloads/Technical Analysis of Stock Trends.pdf")[86:93] %>%
  readr::read_lines() #open the PDF inside your project folder
x[[6]] = pdf_text("D:/Downloads/Technical Analysis of Stock Trends.pdf")[94:111] %>%
  readr::read_lines() #open the PDF inside your project folder
x[[7]] = pdf_text("D:/Downloads/Technical Analysis of Stock Trends.pdf")[112:135] %>%
  readr::read_lines() #open the PDF inside your project folder
x[[8]] = pdf_text("D:/Downloads/Technical Analysis of Stock Trends.pdf")[136:165] %>%
  readr::read_lines() #open the PDF inside your project folder
x[[9]] = pdf_text("D:/Downloads/Technical Analysis of Stock Trends.pdf")[166:187] %>%
  readr::read_lines() #open the PDF inside your project folder
x[[10]] = pdf_text("D:/Downloads/Technical Analysis of Stock Trends.pdf")[188:225] %>%
  readr::read_lines() #open the PDF inside your project folder
x[[11]] = pdf_text("D:/Downloads/Technical Analysis of Stock Trends.pdf")[226:247] %>%
  readr::read_lines() #open the PDF inside your project folder
x[[12]] = pdf_text("D:/Downloads/Technical Analysis of Stock Trends.pdf")[248:267] %>%
  readr::read_lines() #open the PDF inside your project folder
x[[13]] = pdf_text("D:/Downloads/Technical Analysis of Stock Trends.pdf")[268:289] %>%
  readr::read_lines() #open the PDF inside your project folder
x[[14]] = pdf_text("D:/Downloads/Technical Analysis of Stock Trends.pdf")[290:317] %>%
  readr::read_lines() #open the PDF inside your project folder
x[[15]] = pdf_text("D:/Downloads/Technical Analysis of Stock Trends.pdf")[318:331] %>%
  readr::read_lines() #open the PDF inside your project folder
x[[16]] = pdf_text("D:/Downloads/Technical Analysis of Stock Trends.pdf")[332:337] %>%
  readr::read_lines() #open the PDF inside your project folder
x[[17]] = pdf_text("D:/Downloads/Technical Analysis of Stock Trends.pdf")[338:343] %>%
  readr::read_lines() #open the PDF inside your project folder

n = paste0("CHAP_",1:17)
names(x) = n

library(qs)
qsave(x,"D:/Downloads/Tech")






















