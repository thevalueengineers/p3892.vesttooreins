#load libraries
library(haven)
library(dplyr)
library(tidyverse)
library(psych)
library(GPArotation)

cb <- function(arg1){
  write.table(arg1,"clipboard",sep="\t",row.names=FALSE)
}

coal <- function(arg1){
  return(coalesce(arg1,0))
}

pcts <- function(arg1,arg2,arg3){
  arg1 %>% filter(!is.na({{arg2}})) %>% group_by({{arg2}},{{arg3}}) %>% summarise(n=n()) %>% mutate(pct=100*(n/sum(n)))}

#load data
setwd("C:/Work/p3892.vesttooreins")
df <- read_sav("reins_final.sav")
df <- rename_with(df,tolower)
df <- df %>% mutate(broker=if_else(s4==1,1,0),engagedfintech=if_else(a7r3>=4,1,0),engagedmarket=if_else(a7r5>=4,1,0))
View(df)

s5count <- function(arg1){
  x <- if_else(arg1>0,1,0)
  n <- NROW(df)
  return(100*(sum(x)/n))
}
#c1-c20 - see meaning from s5
df %>% select(contains('s5')) %>% summarise(across(everything(),s5count))
pcts(df,c1_lr1r1)
pcts(df,c1_lr1r2)
pcts(df,c1_lr1r6)
pcts(df,c1_lr1r10)
pcts(df,c1_lr1r11)
pcts(df,c1_lr1r12)
pcts(df,a7r1)
pcts(df,a7r2)
pcts(df,a7r3)
pcts(df,a7r4)
pcts(df,a7r5)
pcts(df,a7r6)
pcts(df,a7r7)

df %>% select(contains('a7')) %>% summarise(across(everything(),mean))
df %>% select(broker,contains('a7')) %>% group_by(broker) %>% summarise(across(everything(),mean))

pcts(df,b1_1)
#drop b1_2 - 74% say they follow latest tech trends closely
pcts(df,b1_2<=4)
df %>% select(broker,contains('b1'),contains('b2'),-contains('0r')) %>% group_by(broker) %>% summarise(across(everything(),mean))
#drop b1_3 - heavy concentration around being middle-of-the-road (>50% within one of the midpoint) when it comes to personal risk taking
pcts(df,b1_3) 
#drop b1_4 - 70% say they take long term view
pcts(df,b1_4<=4)
#drop b1_5 - 72% say they're fundamentally optimistic
pcts(df,b1_5<=4)
#drop b1_6 - heavy skew to competitive
pcts(df,b1_6>=6)
#*maybe* drop b2_1 - heavy concentration around being middle-of-the-road (>50% within one of the midpoint) when it comes to beliefs about AI. only ~20% have top2/bot2 box opinions
pcts(df,b2_1)
pcts(df,b2_2)
#drop b2_3 - only 18% pessimistic about future of reins market
pcts(df,b2_3 >= 6)
pcts(df,b2_4)
pcts(df,b2_5)
#drop b2_6 - only 14% think you don't need to be well connected
pcts(df,b2_6>=6)
pcts(df,b2_7)
pcts(df,b2_8)

pcts(df,c3r1)
pcts(df,c3r2)
pcts(df,c3r3)
pcts(df,c3r4)
pcts(df,c3r5)
pcts(df,c3r6)
pcts(df,c3r7)
pcts(df,c3r8)
pcts(df,c3r9)
pcts(df,c3r10)
pcts(df,c3r11)
pcts(df,c3r12)
df %>% select(contains('b1'),contains('b2'),contains('c3'),-contains('0r'))

brokerprofile <- df %>% select(broker, contains('b1'),contains('b2'),contains('c3'),-contains('0r'),-contains('98'),-contains('99')) %>% group_by(broker) %>% summarise(across(everything(),mean))
View(brokerprofile)
write.csv(brokerprofile,'brokerstats.csv')

clustv1 <- c('b1_1','b1_2','b2_2','b2_4','b2_5','b2_7','b2_8','engagedfintech','engagedmarket')
clust1 <- df %>% select(clustv1)
k1<-kmeans(clust1, centers=5, iter.max = 1000, nstart=5)
k1
df$cluster1 <- k1$cluster
clust_stats_1<-df %>% select(cluster1,clustv1,broker,s6r1,s2ar1,s2ar2,s2ar3) %>% select(-contains('0r')) %>% group_by(cluster1) %>% summarise(across(everything(),mean))
cpcts <- pcts(df,cluster1)
lr <- df %>% group_by(cluster1) %>% summarise(lr=mean(a5,na.rm=TRUE),usebroker=mean(a6,na.rm=TRUE),sizeover1b=mean(if_else(a3==10,1,0)))
clust_stats_1<-bind_cols(clust_stats_1,cpcts,lr)
write_csv(clust_stats_1,'c1.csv')
