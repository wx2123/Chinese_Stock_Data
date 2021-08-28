# Read data from Github or bit.ly

# Method 1
library(httr)
df<-read.csv("http://bit.ly/389zjAb")
head(df)

# Method 2
library (readr)

urlfile="http://bit.ly/389zjAb"

urlfile="https://raw.githubusercontent.com/wx2123/R/master/mytest.csv"

mydata<-read_csv(url(urlfile))
head(mydata)

# Method 3
#install.packages("RCurl")
library(RCurl)
x <- getURL("https://raw.githubusercontent.com/wx2123/R/master/mytest.csv")
y <- read.csv(text = x)


library(httr)
All_stock22<-read.csv("https://raw.githubusercontent.com/wx2123/R/master/value_temp_190602-20200331v2.csv", as.is=T)
head(All_stock)










# Read from csv
#install.packages("readxl")

# ?????￪ʼ???????ݣ?
library(readxl)
hs_stock <- read.csv("D:/0_0 Careers/2020/2004_Value_Stocks/value_temp_190602-20210802.csv")

names(hs_stock)
dim(hs_stock)
head(hs_stock)
tail(hs_stock)
anyNA(hs_stock)
str(hs_stock)

#install.packages("tidyverse")
library(dplyr)
hs_stock_2 <- hs_stock %>%
  filter( Mkt_value !="Code Error",
          PB != "#DIV/0!",
          PB != "#VALUE!") 

hs_stock_2 <- hs_stock_2 %>%
  mutate( List_year= lubridate::year(as.Date(List_date)),
          #Mkt_Cap  =  as.numeric(Mkt_value),
          Mkt_Cap  = as.numeric(Mkt_value) / 100000000
          )

hs_stock_2$PB      <- as.numeric(hs_stock_2$PB)
hs_stock_2$Growth  <- as.numeric(hs_stock_2$Growth)
hs_stock_2$PE18_20 <- as.numeric(hs_stock_2$PE18_20)



# ɸѡ???̹?
Value_Stocks <-  hs_stock_2 %>%
  filter(
      Mkt_Cap   >  500  &
      List_year <  2014 & 
      Profit    == "1"  & 
      Div       == "是" & 
      Growth    >  0.33 & 
      PB        <  1.5  & 
      PE18_20   <  15 )

# ɸѡ???̹?
Value_Stocks2 <-  hs_stock_2 %>%
  filter(
    Mkt_Cap>200 & Mkt_Cap < 500 &
    List_year < 2014 & 
    Profit=="1" & 
    Div=="是" & 
    Growth > 0.33 & 
    PB<1.5 & 
    PE18_20 < 15 )

# ɸѡС?̹?
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

large_cap2 <- large_cap %>%
  arrange(Mkt_Cap)

#install.packages('writexl')

library("writexl")
write_xlsx(large_cap2,"D:\\0_0 Careers\\2020\\2004_Value_Stocks\\large_cap.xlsx")
write_xlsx(mid_cap,"D:\\0_0 Careers\\2020\\2004_Value_Stocks\\mid_cap.xlsx")
write_xlsx(small_cap,"D:\\0_0 Careers\\2020\\2004_Value_Stocks\\small_cap.xlsx")






library(ggplot2)
ggplot(Output, aes(y = PE18_20, x = Name), color="steelblue") + geom_point()

ggplot(Output, aes(y = PE18_20, x = Industry)) 
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