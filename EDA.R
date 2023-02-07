#the dataset contains 4 table relative to the sales of each store
#among them, 'holiday' and 'oil price' are factors that will maybe affect the sales
#'store' are info abt each store, 'transaction' are statistics table count total nbr of transactions for each store on each day
#Additional info:(1)it is an oil-dependent country and it's economical health is highly vulnerable to shocks in oil prices.


library(ggplot2)
library(dplyr)
library(lubridate)
setwd('/Users/xjl290021/Desktop/portfolio_projects/R_working directory/store-sales-time-series-forecasting')
sales = read.csv('train1.csv', header = T)
holiday = read.csv('holidays_events.csv',header = T)
oil = read.csv('oil.csv',header = T)
transaction = read.csv('transactions.csv',header = T)
head(sales,10)
summary(sales)
transaction = arrange(transaction, store_nbr, date) #reorder

#check monthly transaction numbers
#There is a stable pattern in Transaction. All months are similar except December from 2013 to 2017 by boxplot. Store sales had always increased at the end of the year.
p1 = ggplot(transaction, mapping = aes(x=as.Date(transaction$date), y = transactions, group = interaction(year(transaction$date),month(transaction$date)), color = as.factor(month(transaction$date))))+ geom_boxplot()
p1+scale_x_date(date_breaks = "1 month", date_labels = "%m")+ scale_fill_brewer(palette = "Set1")+xlab("month") + ylab("monthly transactons total number")

#check avg transaction numbers on each day of a week
#The days of week shows us stores make more transactions at weekends. Saturday is the most important day for shopping.
weekly_avg_tran = transaction%>% group_by(year(transaction$date),wday(transaction$date)) %>% summarise(
  weekly_avg_tran = mean(transactions)
)
weekly_avg_tran["year"] = weekly_avg_tran$`year(transaction$date)`
weekly_avg_tran["dayofweek"] = weekly_avg_tran$`wday(transaction$date)`

p2 = ggplot(weekly_avg_tran, aes(dayofweek , weekly_avg_tran))+ geom_line(aes(group = interaction(year),color = as.factor(year)))
p2


#check the relationships between oil price and sales
sales$date = as.Date(sales$date)
oil$date = as.Date(oil$date)
daily_avg_sales = sales %>% group_by(date) %>% summarise(
  daily_total_sales = mean(sales, na.rm = F)
)
plot(daily_avg_sales$daily_total_sales,type = "l", ylab = "total sales per day", xlab = "days")
tbl = left_join(oil,daily_avg_sales,by = "date")
tbl = filter(tbl, tbl$dcoilwtico != "na"&tbl$daily_total_sales != "na")
cor(tbl$daily_total_sales,tbl$dcoilwtico,method = "pearson")
picture = ggplot(tbl, aes(x = tbl$date))+
                   geom_line(aes(y=tbl$dcoilwtico),  color="blue") +
                   geom_line(aes(y=tbl$daily_total_sales), color="black")
picture+ xlab("year")+ ylab("oil price & sales")

plot(tbl$date,tbl$daily_total_sales,type = "l",col = "RosyBrown",ylim=range( c(tbl$dcoilwtico, tbl$daily_total_sales) ),xlab = "year", ylab = "oil price & sales")
lines(tbl$date,tbl$dcoilwtico,type = "l", col = "blue")
grid()
legend("topleft",c("daily_avg_sales","oil_price"),col=c("RosyBrown","blue"),text.col=c("RosyBrown","blue"),pch=c(15,16),lty=c(1,1))

# corr = -0.7050015 很强的负相关性


##holiday
##transferred holidays
holiday$date = as.Date(holiday$date)
tran1 = filter(holiday, holiday$type == "Holiday" & holiday$transferred == "True")
tran1 = select(tran1, -transferred)
tran2 = filter(holiday, holiday$type == "Transfer")
tran2 = select(tran2, -transferred)
tr = rbind(tran1,tran2)
tr = tr[, c(5,1,2,3,4)] # 选出了该放假的所有日子和转移到的所有日子

holiday_copy = holiday
holiday_copy = filter(holiday_copy, holiday_copy$type != "Transfer" & holiday_copy$transferred == "False")#在该放假的时候放假了
holiday_copy = select(holiday_copy, -transferred)
holiday_copy = holiday_copy[, c(5,1,2,3,4)]
holiday_copy = rbind(holiday_copy, tr)

#additional holidays are also holidays, rename it
holiday_copy$type = as.character(holiday_copy$type)
holiday_copy$type = ifelse(holiday_copy$type == "Additional", "Holiday", holiday_copy$type) 

##rename bridge holiday too
holiday_copy$type = ifelse(holiday_copy$type == "Bridge", "Holiday", holiday_copy$type) # then all changes to holiday or event or workday

#
workday = filter(holiday_copy, holiday_copy$type == "Work Day")#Work Day Holidays, that is meant to payback the Bridge.
holidays = filter(holiday_copy, holiday_copy$type != "Work Day")

#split
# events are all national
library(stringr)
events = filter(holiday_copy, holiday_copy$type == "Event") #56
events$description = gsub("\\+","",events$description) #去掉后缀
events$description = gsub("\\d","",events$description) #去掉后缀
events$description = gsub(":.*","",events$description) #去掉比赛场次
events = rename(select(events, -type,-locale,-locale_name), "events" = "description")

#seperate reginal, local, national holidays
holidays = filter(holiday_copy, holiday_copy$type != "Event")
holidays$description = gsub("\\-","",holidays$description)
holidays$description = gsub("\\+","",holidays$description)
holidays$description = gsub("\\d","",holidays$description) #all clear
regional = filter(holidays, holidays$locale == "Regional")
regional = select(rename(regional, "holiday_regional" = "description", "state" = "locale_name"),-locale)
regional = distinct(regional)#24

national = filter(holidays, holidays$locale == "National")
national = select(rename(national, "holiday_national" = "description"),-locale,-locale_name) #118
national = distinct(national)

local = filter(holidays, holidays$locale == "Local")
local = select(rename(local, "holiday_local" = "description", "city" = "locale_name"),-locale) #152
local = distinct(local)

store_info = read.csv("stores.csv", header = T)
d = left_join(sales, store_info)
head(d)
d$date = as.Date(d$date)
d = left_join(d,events) 
d = left_join(d,national, by = "date") 
d = left_join(d,regional, by = c("date" = "date","state" = "state"))
d = left_join(d,local, by = c("date" = "date","city" = "city"))

#binary feature
d["holiday_national_binary"] = ifelse(!is.na(d$holiday_national),1,0) #针对于各种大类节日的dummy
d["holiday_regionall_binary"] = ifelse(!is.na(d$holiday_regional),1,0)
d["holiday_local_binary"] = ifelse(!is.na(d$holiday_local),1,0)

#d["national_independence"] = ifelse(d$holiday_national %in% c('Batalla de Pichincha',  'Independencia de Cuenca', 
#'Independencia de Guayaquil', 'Independencia de Guayaquil', 'Primer Grito de Independencia'),1,0)
library(stringr)
#d["local_cantonizacio"] = ifelse(str_detect(d$holiday_local, "Cantonizacio"),1,0)
#d["local_fundacion"] = ifelse(str_detect(d$holiday_local, "Fundacion"),1,0)
#d["local_independencia"] = ifelse(str_detect(d$holiday_local, "Independencia"),1,0)

library(fastDummies)
dummies = function(df){
  cn = c(colnames(df))
  dfdummies = dummy_cols(df,select_columns = cn, ignore_na = T)
  return(dfdummies)
}

event_dummy = dummies(d$events)
holidays_dummy = dummies(d[,c("holiday_national","holiday_regional","holiday_local")])

d = select(d, -holiday_national,-holiday_regional,-holiday_local)
d = cbind(d,event_dummy)
d = cbind(d,holidays_dummy)
d = select(d, -events, -holiday_national,-holiday_regional,-holiday_local)
d[is.na(d)] = 0
d_sorted = arrange(d, family,store_nbr,date)
d_sorted = d_sorted[,-c(9:13)]
d_sorted = d_sorted[,-c(12)]

write.csv(d_sorted,"d_sorted.csv")



#Hypothesis test Under this festival, whether there is a significant difference in the sales of each falimy, obey the normal distribution, and the variance is known
groupA = filter(d_sorted, d_sorted$`holiday_national_Primer dia del ano`==1)[,c("date","store_nbr","family","sales")]
groupB = filter(d_sorted, d_sorted$`holiday_national_Primer dia del ano`==0)[,c("date","store_nbr","family","sales")]

groupA1 = groupA %>% group_by(family) %>% summarise(
  total_sales_byfamily = mean(sales, na.rm = T),
  sd = var(sales)/54
)
groupB1 = groupB %>% group_by(family) %>% summarise(
  total_sales_byfamily = mean(sales, na.rm = T),
  sd = var(sales)/54
)
u_1_alpha_2 = 1.96
u = (groupA1$total_sales_byfamily-groupB1$total_sales_byfamily)/sqrt(groupA1$sd+groupB1$total_sales_byfamily)
res = ifelse(u<(-1)*1.96, "they will sale less on holiday of Primer dia del ano","sales has no difference" )

result = data.frame(
  family = groupA1$family,
  u_statistic = u,
  u_1_alpha_2 = -1.96,
  result = res
  
)
result = filter(result, result !="na" )
write.csv(result,"result.csv")












   