
# A股价值投资选股
# 2023/6/17


# Read from csv
#install.packages("readxl")

# Clean all objects from the current workspace (R memory) 
rm(list=ls())
Sys.setlocale(category = "LC_ALL", locale = "Chinese") # 将本地语言默认为中文
# https://www.jianshu.com/p/486c6b98f7cb

# import data
#install.packages("readxl")

library(readxl)
hs_stock <- read.csv(
  "D:/0_0 Careers/2020/2004_Value_Stocks/Chinese_Stock_Data/value_temp_190602-2023_1104.csv")

# names(hs_stock)
# dim(hs_stock)
# head(hs_stock)
# tail(hs_stock)
# anyNA(hs_stock)
# str(hs_stock)

#install.packages("guf")
library(dplyr)
hs_stock_2 <- hs_stock %>%
  filter( Mkt_value != "Code Error",
          Mkt_value != "#DIV/0!",
          Mkt_value != "#VALUE!",
          PB        != "#DIV/0!",
          Close     != "0",          #剔除停牌股票
          PB        != "#VALUE!") 

hs_stock_2 <- hs_stock_2 %>%
  mutate(Mkt_value2 = as.numeric(Mkt_value))

#summary(hs_stock_2$Mkt_value)

library(stringr)
hs_stock_2 <- hs_stock_2 %>%
  mutate( 
          List_year = lubridate::year(as.Date(List_date)),
          #List_year = as.numeric(str_sub(List_date,-4)),
          Mkt_Cap  = as.numeric(Mkt_value2) / 100000000
          )

#lubridate::year(as.Date(1/20/1990))
#Mkt_Cap  =  as.numeric(Mkt_value),

hs_stock_2$PB      <- as.numeric(hs_stock_2$PB)
hs_stock_2$Growth  <- as.numeric(hs_stock_2$Growth)
hs_stock_2$PE20_22 <- as.numeric(hs_stock_2$PE20_22)

# Large Cap
Value_Stocks <-  hs_stock_2 %>%
  filter(
      Mkt_Cap   >  500  &
      List_year <  2014 & 
      Profit    == "1"  & 
      # Div     == "???" & 
      Growth    >  0.33 & 
      PB        <  1.5  & 
      PE20_22   <  15 ) 

# Mid Cap
Value_Stocks2 <-  hs_stock_2 %>%
  filter(
    Mkt_Cap   >  200  & Mkt_Cap < 500 &
    List_year <  2014 & 
    Profit    == "1"  & 
    # Div=="???" & 
    Growth    > 0.33  & 
    PB        < 1.5   & 
    PE20_22   < 15 ) 

# Small Cap
Value_Stocks3 <-  hs_stock_2 %>%
  filter(
    Mkt_Cap>100 & Mkt_Cap < 200 &
      List_year < 2014 & 
      Profit=="1" & 
      # Div=="???" & 
      Growth > 0.33 & 
      PB<1.5 & 
      PE20_22 < 15 ) 

options(digits = 4)  

large_cap<- Value_Stocks %>%
  select(Code, Name, Close, Industry,List_date,Mkt_Cap,PE20_22,PB, YTDchg)

Output<- Value_Stocks %>%
  select(Name,PE20_22,PB)

mid_cap<- Value_Stocks2 %>%
  select(Name, Close, Industry,List_date,Mkt_Cap, PE20_22,PB, YTDchg)%>% 
  rename(PE19_21 = PE20_22)

small_cap<- Value_Stocks3 %>%
  select(Name, Close, Industry,List_date,Mkt_Cap, PE20_22,PB, YTDchg)%>% 
  rename(PE19_21 = PE20_22)

large_cap2 <- large_cap %>%
  arrange(Mkt_Cap)%>% 
  rename(PE19_21 = PE20_22)

# Save to Excel file -----------------------------------------------------
#install.packages('writexl')

library("writexl")
write_xlsx(large_cap2,"D:\\0_0 Careers\\2020\\2004_Value_Stocks\\Chinese_Stock_Data\\231104_large_cap.xlsx")
write_xlsx(mid_cap   ,"D:\\0_0 Careers\\2020\\2004_Value_Stocks\\Chinese_Stock_Data\\231104_mid_cap.xlsx")
write_xlsx(small_cap ,"D:\\0_0 Careers\\2020\\2004_Value_Stocks\\Chinese_Stock_Data\\231104_small_cap.xlsx")

write.csv(large_cap2,"D:\\0_0 Careers\\2020\\2004_Value_Stocks\\Chinese_Stock_Data\\231104_large_cap.csv")
write.csv(mid_cap   ,"D:\\0_0 Careers\\2020\\2004_Value_Stocks\\Chinese_Stock_Data\\231104_mid_cap.csv")
write.csv(small_cap ,"D:\\0_0 Careers\\2020\\2004_Value_Stocks\\Chinese_Stock_Data\\231104_small_cap.csv")






# Read data from Github or bit.ly

# Clean all objects from the current workspace (R memory) 
rm(list=ls())

# Method 1
library(httr)
df<-read.csv("http://bit.ly/389zjAb")
head(df)

# Method 2
library (tidyverse)
#urlfile="http://bit.ly/389zjAb"
#urlfile="https://raw.githubusercontent.com/wx2123/R/master/mytest.csv"
urlfile= "https://raw.githubusercontent.com/wx2123/Chinese_Stock_Data/master/value_temp_190602-20210828.csv"

mydata<-read_csv(url(urlfile), encoding = "UTF-8")
head(mydata)

# Method 3
#install.packages("RCurl")
library(RCurl)
x <- getURL("https://raw.githubusercontent.com/wx2123/R/master/mytest.csv")
y <- read.csv(text = x)


library(httr)
All_stock22<-read.csv("https://raw.githubusercontent.com/wx2123/R/master/value_temp_190602-20200331v2.csv", as.is=T)
head(All_stock)



library(ggplot2)
ggplot(Output, aes(y = PE20_22, x = Name), color="steelblue") + geom_point()

ggplot(Output, aes(y = PE20_22, x = Industry)) s
      + geom_boxplot(fill="yellow",color="red")


library(arsenal)
library(rmarkdown)
# Need to specify the folder where you want to save and the name to be saved under 
# - here I called it 'Table1.doc'

write2word(Output,"~/output.doc")
write2html(Output, "~/Output.html")
write2pdf(Output, "~/Output.pdf")

write2html(Output2, "~/Output2.html")


data(mockstudy)
# tableby example
tab1 <- tableby(arm ~ sex + age, data=mockstudy)
write2html(tab1, "~/trash.html")

# freqlist example
tab.ex <- table(mockstudy[, c("arm", "sex", "mdquality.s")], useNA = "ifany")
noby <- freqlist(tab.ex, na.options = "include")
write2pdf(noby, "~/trash2.pdf")


# A more complicated example
write2word(tab1, "~/trash.doc",
           keep.md = TRUE,
           reference_docx = mystyles.docx, # passed to rmarkdown::word_document
           quiet = TRUE, # passed to rmarkdown::render
           title = "My cool new title") # passed to summary.tableby

## End(Not run)