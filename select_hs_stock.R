
rm(list=ls())

# Read from csv
# install.packages("readxl")

# 从这里开始读入数据！
library(readxl)
hs_stock <- read.csv("D:/0_0 Careers/2020/2004_Value_Stocks/value_temp_190602-20210508.csv")

names(hs_stock)
dim(hs_stock)
head(hs_stock)
tail(hs_stock)
anyNA(hs_stock)
str(hs_stock)

#install.packages("tidyverse")
library(dplyr)
hs_stock_2 <- hs_stock %>%
  filter( PB != "#DIV/0!") 

hs_stock_2 <- hs_stock_2 %>%
  mutate( List_year= lubridate::year(as.Date(List_date)),
          Mkt_Cap  = as.numeric(Mkt_value) / 100000000
  )

hs_stock_2$PB      <- as.numeric(hs_stock_2$PB)
hs_stock_2$Growth  <- as.numeric(hs_stock_2$Growth)
hs_stock_2$PE18_20 <- as.numeric(hs_stock_2$PE18_20)


# select(All_stock,-List_date)
# filter(All_stock,Name=="广汽集团")

# 筛选大盘股
Value_Stocks <-  hs_stock_2 %>%
  filter(
    Mkt_Cap > 500 & List_year < 2014 & 
      Profit == "1" & Div=="是"        & 
      Growth >0.33  & 
      PB<1.5        & PE18_20 < 15 )

# 筛选中盘股
Value_Stocks2 <-  hs_stock_2 %>%
  filter(
    Mkt_Cap>200 & Mkt_Cap < 500 &
      List_year < 2014 & 
      Profit=="1" & 
      Div=="是" & 
      Growth > 0.33 & 
      PB<1.5 & 
      PE18_20 < 15 )

# 筛选小盘股
Value_Stocks3 <-  hs_stock_2 %>%
  filter(
    Mkt_Cap>100 & Mkt_Cap < 200 &
      List_year < 2014 & 
      Profit=="1" & 
      Div=="是" & 
      Growth > 0.33 & 
      PB<1.5 & 
      PE18_20 < 15 )

options(digits = 4)  

large_cap<- Value_Stocks %>%
  select(Code, Name, Close, Industry,List_date,Mkt_Cap,PE18_20,PB)

Output<- Value_Stocks %>%
  select(Name,PE18_20,PB)

mid_cap<- Value_Stocks2 %>%
  select(Name, Close, Industry,List_date,Mkt_Cap, Growth, PE18_20,PB)

small_cap<- Value_Stocks3 %>%
  select(Name, Close, Industry,List_date,Mkt_Cap, Growth, PE18_20,PB)