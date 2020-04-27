# 1. Merge the datasets


data1 <- read.csv("C:\\Users\\Ritwik Sinha\\Desktop\\Ritwik\\Projects\\Transactions (1).csv")
data2 <- read.csv("C:\\Users\\Ritwik Sinha\\Desktop\\Ritwik\\Projects\\Customer.csv")
data3 <- read.csv("C:\\Users\\Ritwik Sinha\\Desktop\\Ritwik\\Projects\\prod_cat_info.csv")


merge1 <- merge(x=data1, y=data2, by.x='cust_id', by.y='customer_Id',all.x=T)
View(merge1)

Customer_Final <- merge(x=merge1, y=data3, by.x=c("prod_subcat_code", "prod_cat_code"), by.y=c("prod_sub_cat_code", "prod_cat_code"))
View(Customer_Final)

sapply(Customer_Final, function(x) sum(is.na(x)))

## Changing data type of tran date to date time
Customer_Final$tran_date <- strptime(x = as.character(Customer_Final$tran_date ),
                                     format = "%d/%m/%Y")

table(prod_cat_code)
table(prod_subcat_code)
table(Qty)
table(city_code)
table(prod_sub_cat_code)

Customer_Final$prod_cat_code <- as.factor(Customer_Final$prod_cat_code)
Customer_Final$prod_subcat_code <- as.factor(Customer_Final$prod_subcat_code)
Customer_Final$Qty <- as.factor(Customer_Final$Qty)
Customer_Final$city_code <- as.factor(Customer_Final$city_code)


# 2. Prepare a summary report for the merged data set.

# 2.(a) column names and data types
names(Customer_Final)
sapply(Customer_Final,class)

# 2. (b) Top/Bottom 10 observations
head(Customer_Final, 10)
tail(Customer_Final, 10)

# 2. (c) "Five-number summary" for continuous variables (min, Q1, median, Q3 and max)
summary(Customer_Final)

# 2. (d)	Frequency tables for all the categorical variables
# Categorical variables are:- prod_cat, Store_type, prod_subcat, Gender, prod_cat_code, prod_subcat_code,
# Qty, city_code, prod_sub_cat_code

library(plyr)
count(Customer_Final, 'prod_cat')
count(Customer_Final, 'Store_type')
count(Customer_Final, 'prod_subcat')
count(Customer_Final, 'Gender')


# 3. Generate histograms for all continuous variables and frequency bars for categorical variables

attach(Customer_Final)
hist(Rate,xlab = "Rate",col = "yellow",border = "blue")
hist(Tax,xlab = "Tax",col = "yellow",border = "blue")
hist(total_amt,xlab = "total_amt",col = "yellow",border = "blue")

library(ggplot2)
install.packages('ggpubr')
library(ggpubr)
theme_set(theme_pubr())

ggplot(Customer_Final, aes(prod_cat_code)) + geom_bar(fill = "#0073C2FF") + theme_pubclean()
ggplot(Customer_Final, aes(prod_subcat_code)) + geom_bar(fill = "#0073C2FF") + theme_pubclean()
ggplot(Customer_Final, aes(Qty)) + geom_bar(fill = "#0073C2FF") + theme_pubclean()
ggplot(Customer_Final, aes(city_code)) + geom_bar(fill = "#0073C2FF") + theme_pubclean()
ggplot(Customer_Final, aes(prod_sub_cat_code)) + geom_bar(fill = "#0073C2FF") + theme_pubclean()
ggplot(Customer_Final, aes(prod_cat)) + geom_bar(fill = "#0073C2FF") + theme_pubclean()
ggplot(Customer_Final, aes(Store_type)) + geom_bar(fill = "#0073C2FF") + theme_pubclean()
ggplot(Customer_Final, aes(prod_subcat)) + geom_bar(fill = "#0073C2FF") + theme_pubclean()
ggplot(Customer_Final, aes(Gender)) + geom_bar(fill = "#0073C2FF") + theme_pubclean()


# 4. Calculate the following information using the merged dataset :

# 4 (a)	Time period of the available transaction data
colSums(is.na(Customer_Final))
Customer_Final$tran_date = as.Date(Customer_Final$tran_date, format='%d-%m-%Y')
Customer_Final$tran_date

#timeperiod
range(Customer_Final$tran_date, na.rm=T)
first_tran_date <- min(Customer_Final$tran_date,na.rm = T)
last_tran_date <- max(Customer_Final$tran_date,na.rm = T)
transaction_time_period <- last_tran_date - first_tran_date
transaction_time_period

# 4 (b)	Count of transactions where the total amount of transaction was negative
range(Customer_Final$total_amt)
library(dplyr)
Customer_Final %>% filter(total_amt < 0)  %>% dplyr::summarise(n=n())


# 5. Analyze which product categories are more popular among females vs male customers.

levels(Customer_Final$prod_cat)

## Different product categories are Bags, Books, Clothing, Electronics, Footwear, Home and Kitchen

Customer_Final %>%  filter(prod_cat == "Bags" & Gender == "M") %>% dplyr::summarise(n=n())
Customer_Final %>%  filter(prod_cat == "Bags" & Gender == "F") %>% dplyr::summarise(n=n())
## Number of males for Bags as product category is 1004 and of females is 994

Customer_Final %>%  filter(prod_cat == "Books" & Gender == "M") %>% dplyr::summarise(n=n())
Customer_Final %>%  filter(prod_cat == "Books" & Gender == "F") %>% dplyr::summarise(n=n())
## Number of males for Books as product category is 3116 and of females is 2949

Customer_Final %>%  filter(prod_cat == "Clothing" & Gender == "M") %>% dplyr::summarise(n=n())
Customer_Final %>%  filter(prod_cat == "Clothing" & Gender == "F") %>% dplyr::summarise(n=n())
## Number of males for clothing as product category is 1518 and of females is 1439

Customer_Final %>%  filter(prod_cat == "Electronics" & Gender == "M") %>% dplyr::summarise(n=n())
Customer_Final %>%  filter(prod_cat == "Electronics" & Gender == "F") %>% dplyr::summarise(n=n())
## Number of males for Electronics as product category is 2570 and of females is 2328

Customer_Final %>%  filter(prod_cat == "Footwear" & Gender == "M") %>% dplyr::summarise(n=n())
Customer_Final %>%  filter(prod_cat == "Footwear" & Gender == "F") %>% dplyr::summarise(n=n())
## Number of males for Footwear as product category is 1469 and of females is 1529

Customer_Final %>%  filter(prod_cat == "Home and kitchen" & Gender == "M") %>% dplyr::summarise(n=n())
Customer_Final %>%  filter(prod_cat == "Home and kitchen" & Gender == "F") %>% dplyr::summarise(n=n())
## Number of males for Home and kitchen as product category is 2134 and of females is 1994

## From all the above analysis, the conclusion is that both among males and females Books as the
## product category is popular



# 6. Which City code has the maximum customers and what was the percentage of customers from that city

## Customer can be either Male or Female so finding total number of males and females per city
levels(Customer_Final$city_code)

Customer_Final %>% group_by(Gender) %>% filter(city_code == "1") %>% dplyr::summarise(n=n())
## No. of male customer = 1072 and No.of female customer = 1186. Thus, total customer for city code 1 = 2258

Customer_Final %>% group_by(Gender) %>% filter(city_code == "2") %>% dplyr::summarise(n=n())
## No. of male customer = 1204 and No.of female customer = 1061. Thus, total customer for city code 2 = 2265

Customer_Final %>% group_by(Gender) %>% filter(city_code == "3") %>% dplyr::summarise(n=n())
## No. of male customer = 1329 and No.of female customer = 1082. Thus, total customer for city code 3 = 2411

Customer_Final %>% group_by(Gender) %>% filter(city_code == "4") %>% dplyr::summarise(n=n())
## No. of male customer = 1280 and No.of female customer = 1142. Thus, total customer for city code 4 = 2422

Customer_Final %>% group_by(Gender) %>% filter(city_code == "5") %>% dplyr::summarise(n=n())
## No. of male customer = 1204 and No.of female customer = 1156. Thus, total customer for city code 5 = 2360

Customer_Final %>% group_by(Gender) %>% filter(city_code == "6") %>% dplyr::summarise(n=n())
## No. of male customer = 1073 and No.of female customer = 1050. Thus, total customer for city code 6 = 2123

Customer_Final %>% group_by(Gender) %>% filter(city_code == "7") %>% dplyr::summarise(n=n())
## No. of male customer = 1104 and No.of female customer = 1252. Thus, total customer for city code 7 = 2356

Customer_Final %>% group_by(Gender) %>% filter(city_code == "8") %>% dplyr::summarise(n=n())
## No. of male customer = 1390 and No.of female customer = 940. Thus, total customer for city code 8 = 2330

Customer_Final %>% group_by(Gender) %>% filter(city_code == "9") %>% dplyr::summarise(n=n())
## No. of male customer = 1047 and No.of female customer = 1131. Thus, total customer for city code 9 = 2178

Customer_Final %>% group_by(Gender) %>% filter(city_code == "10") %>% dplyr::summarise(n=n())
## No. of male customer = 1102 and No.of female customer = 1231. Thus, total customer for city code 10 = 2333

## Hence, city code 4 has maximum number of customers. And it's percentage is (2422/23053)*100 = 10.506%


# 7. Which store type sells the maximum products by value and by quantity

# 8. What was the total amount earned from the "Electronics" and "Clothing" categories from Flagship Stores

amt=0
for (row in 1:nrow(Customer_Final)){
  if (Customer_Final[Store_type,row] == "Flagship Stores" & (Customer_Final[prod_cat,row] == "Electronics")) {
    amt = amt + Customer_Final[total_amt, row]
  }}