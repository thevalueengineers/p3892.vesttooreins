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
pcts(df,b1_1)
pcts(df,b1_2)
pcts(df,b1_3)
pcts(df,b1_4)
pcts(df,b1_5)
pcts(df,b1_6)
pcts(df,b2_1)
pcts(df,b2_2)
pcts(df,b2_3)
pcts(df,b2_4)
pcts(df,b2_5)
pcts(df,b2_6)
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

clustv1 <- c('b1_1','b1_2','b1_3','b1_4','b1_5','b1_6','b2_1','b2_2','b2_3','b2_4','b2_5','b2_6','b2_7','b2_8')
clust1 <- df %>% select(clustv1)
k1<-kmeans(clust1, centers=5, iter.max = 1000, nstart=5)
k1
df$cluster1 <- k1$cluster
clust_stats_1<-df %>% select(cluster1,contains('b1'),contains('b2')) %>% select(-contains('0r')) %>% group_by(cluster1) %>% summarise(across(everything(),mean))
write_csv(clust_stats_1,'c1.csv')
