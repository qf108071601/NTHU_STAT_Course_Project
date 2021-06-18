library(pdftools)
library(tidyverse)

CHAP1 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[17:23] %>%
  readr::read_lines() #open the PDF inside your project folder


CHAP2 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[24:35] %>%
  readr::read_lines() #open the PDF inside your project folder


CHAP3 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[36:45] %>%
  readr::read_lines() #open the PDF inside your project folder

CHAP4 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[46:55] %>%
  readr::read_lines() #open the PDF inside your project folder

CHAP5 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[56:74] %>%
  readr::read_lines() #open the PDF inside your project folder

CHAP6 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[75:87] %>%
  readr::read_lines() #open the PDF inside your project folder

CHAP7 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[88:98] %>%
  readr::read_lines() #open the PDF inside your project folder

CHAP8 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[99:106] %>%
  readr::read_lines() #open the PDF inside your project folder

CHAP9 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[107:114] %>%
  readr::read_lines() #open the PDF inside your project folder

CHAP10 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[115:128] %>%
  readr::read_lines() #open the PDF inside your project folder

CHAP11 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[129:140] %>%
  readr::read_lines() #open the PDF inside your project folder

CHAP12 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[141:149] %>%
  readr::read_lines() #open the PDF inside your project folder

CHAP13 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[150:160] %>%
  readr::read_lines() #open the PDF inside your project folder

CHAP14 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[161:169] %>%
  readr::read_lines() #open the PDF inside your project folder

CHAP15 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[170:179] %>%
  readr::read_lines() #open the PDF inside your project folder

CHAP16 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[180:189] %>%
  readr::read_lines() #open the PDF inside your project folder

CHAP17 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[190:203] %>%
  readr::read_lines() #open the PDF inside your project folder

CHAP18 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[204:219] %>%
  readr::read_lines() #open the PDF inside your project folder

CHAP19 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[220:227] %>%
  readr::read_lines() #open the PDF inside your project folder

CHAP20 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[228:233] %>%
  readr::read_lines() #open the PDF inside your project folder

CHAP21 <- pdf_text(
  "D:/Downloads/The_Most_Important_Thing_Illuminated_Uncommon_Sense_for_the_Thoughtful_Investor.pdf")[234:242] %>%
  readr::read_lines() #open the PDF inside your project folder

n = paste0("CHAP_",1:21)

x = list(CHAP1,CHAP2,CHAP3,CHAP4,CHAP5,
         CHAP6,CHAP7,CHAP8,CHAP9,CHAP10,
         CHAP11,CHAP12,CHAP13,CHAP14,CHAP15,
         CHAP16,CHAP17,CHAP18,CHAP19,CHAP20,CHAP21)
names(x) = n

library(qs)
qsave(x,"D:/Downloads/Book")


