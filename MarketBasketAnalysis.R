install.packages("tidyverse")
install.packages("readxl")
install.packages("knitr")
install.packages("lubridate")
install.packages("arules")
install.packages("arulesViz")
yesinstall.packages("plyr")
install.packages("magrittr")
install.packages("dplyr")
ins?all.packages("devtools")


library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)
library(magrittr)
library(dplyr)

library(devtools)
install_github("mhahsler/arulesViz")
retai?<-read_excel('C:/Users/rosha/Downloads/Online Retail.xlsx')
retail<-retail[complete.cases(retail),]
retail<-retail %>% mutate(Description =as.factor(Description))
retail<-retail %>% mutate(Country=as.factor(Country))
retail$Date<-as.Date(retail$InvoiceDate?
retail$Time<-format(retail$InvoiceDate,"%H:%M:%S")
retail$InvoiceNo<-as.numeric(as.character(retail$InvoiceNo))

glimpse(retail)

#What time do people often purchase online?

retail$Time<-as.factor(retail$Time)
a<-hms(as.character(retail$Time))
retail$Tim?=hour(a)

retail%>%
  ggplot(aes(x=Time))+ geom_bar(stat="count", fill="indianred")

#How many items each customers buy?
detach("package:plyr",unload=TRUE)

retail%>%
  group_by(InvoiceNo) %>%
  summarize(n_items= mean(Quantity)) %>%
  ggplot(aes(x=n_items?)+ geom_histogram(fill="indianred", bins=100000)+ geom_rug()+coord_cartesian(xlim = c(0,80))

#Top 10 best sellers
tmp <- retail %>% 
  group_by(StockCode, Description) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
tmp <- head(tmp, n=10)
tmp
tm? %>% 
  ggplot(aes(x=reorder(Description,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()

#Association Rule for Online retailers
retail_sorted <- retail[order(retail$CustomerID),]
library(plyr)
itemList <- ddply(retail,c("C?stomerID","Date"), 
                  function(df1)paste(df1$Description, 
                                     collapse = ","))

#We only need item transactions, so remove customerID and Date columns

itemList$CustomerID <- NULL
itemList$Date <- NULL
coln?mes(itemList) <- c("items")

#Write the data fram to a csv file and check whether our transaction format is correct

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)

#Let's have a closer look at how many transactions we have and wh?t they are.
tr <- read.transactions('market_basket.csv', format = 'basket', sep=',', rm.duplicates=TRUE)
summary(tr)

#Let's have a look at the item frequency plot
itemFrequencyPlot(tr, topN=20, type='absolute')

#Using the Apriori Algorithm in Arules libr?ry
#We pass a sup=0.001 and conf=0.8 so that support=0.1% and confidence=80%
rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

#We have 89,697 rules. I don't want to print ?hem all, so let's inspect the top 10.

inspect(rules[1:10])

#The interpretation is pretty straight forward:
#100% customers who bought "WOBBLY CHICKEN" also bought "DECORATION".
#100% customers who bought "BLACK TEA" also bought "SUGAR JAR".
#And plot the?e top 10 rules.

topRules <- rules[1:10]
plot(topRules)

plot(topRules, method="graph")
plot(topRules, method = "grouped")
