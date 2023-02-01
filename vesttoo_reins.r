#load libraries
library(haven)
library(dplyr)
library(tidyverse)
library(psych)
library(GPArotation)
library(openxlsx)

cb <- function(arg1){
  write.table(arg1,"clipboard",sep="\t",row.names=FALSE)
}

coal <- function(arg1){
  return(coalesce(arg1,0))
}

pcts <- function(arg1,arg2,arg3){
  arg1 %>% filter(!is.na({{arg2}})) %>% group_by({{arg2}},{{arg3}}) %>% summarise(n=n()) %>% mutate(pct=100*(n/sum(n)))}

meanrm<- function(arg1){
  return(mean(arg1,na.rm=TRUE))}

binarize <- function(arg1){
  return(if_else(arg1>0,1,0))
}

#load data
setwd("C:/Work/p3892.vesttooreins")
df <- read_sav("reins_final.sav")
df <- rename_with(df,tolower)
df <- df %>% mutate(timeandadmin=c3r1+c3r2,workscat=if_else(s5r21>0,1,0),broker=if_else(s4==1,1,0),engagedcapmark=if_else(a7r2>=4,1,0),engagedfintech=if_else(a7r3>=4,1,0),engagedmarket=if_else(a7r5>=4,1,0),reinsprimary=if_else(s2b==1,1,0),retroprimary=if_else(s2b==2,1,0),pctfac=if_else(b4a==1,1,0),pcttreaty=if_else(b4a==2,1,0),pctboth=if_else(b4a==3,1,0),pctasl=if_else(b4b==1,1,0),pctqs=if_else(b4b==2,1,0),pctxol=if_else(b4b==3,1,0),numlines=if_else(s5r1>=1,1,0)+if_else(s5r2>=1,1,0)+if_else(s5r3>=1,1,0)+if_else(s5r4>=1,1,0)+if_else(s5r5>=1,1,0)+if_else(s5r6>=1,1,0)+if_else(s5r7>=1,1,0)+if_else(s5r8>=1,1,0)+if_else(s5r9>=1,1,0)+if_else(s5r10>=1,1,0)+if_else(s5r11>=1,1,0)+if_else(s5r12>=1,1,0)+if_else(s5r13>=1,1,0)+if_else(s5r14>=1,1,0)+if_else(s5r15>=1,1,0)+if_else(s5r16>=1,1,0)+if_else(s5r17>=1,1,0)+if_else(s5r18>=1,1,0)+if_else(s5r19>=1,1,0)+if_else(s5r20>=1,1,0),numlinesbracketed=ceiling(numlines/3),numlinesbracketed=if_else(numlines>=10,4,numlinesbracketed))
df <- df %>% mutate(
  c_1_price = rowMeans(select(df,c(c1_lr1r1,c1_lr2r1,c1_lr3r1,c1_lr4r1,c1_lr5r1,c1_lr6r1,c1_lr7r1,c1_lr8r1,c1_lr9r1,c1_lr10r1,c1_lr11r1,c1_lr12r1,c1_lr13r1,c1_lr14r1,c1_lr15r1,c1_lr16r1,c1_lr17r1,c1_lr18r1,c1_lr19r1,c1_lr20r1)), na.rm = TRUE),
  c_1_contractflex = rowMeans(select(df,c(c1_lr1r2,c1_lr2r2,c1_lr3r2,c1_lr4r2,c1_lr5r2,c1_lr6r2,c1_lr7r2,c1_lr8r2,c1_lr9r2,c1_lr10r2,c1_lr11r2,c1_lr12r2,c1_lr13r2,c1_lr14r2,c1_lr15r2,c1_lr16r2,c1_lr17r2,c1_lr18r2,c1_lr19r2,c1_lr20r2)), na.rm = TRUE),
  c_1_reassurancehistory = rowMeans(select(df,c(c1_lr1r3,c1_lr2r3,c1_lr3r3,c1_lr4r3,c1_lr5r3,c1_lr6r3,c1_lr7r3,c1_lr8r3,c1_lr9r3,c1_lr10r3,c1_lr11r3,c1_lr12r3,c1_lr13r3,c1_lr14r3,c1_lr15r3,c1_lr16r3,c1_lr17r3,c1_lr18r3,c1_lr19r3,c1_lr20r3)), na.rm = TRUE),
  c_1_regulatoryexp = rowMeans(select(df,c(c1_lr1r4,c1_lr2r4,c1_lr3r4,c1_lr4r4,c1_lr5r4,c1_lr6r4,c1_lr7r4,c1_lr8r4,c1_lr9r4,c1_lr10r4,c1_lr11r4,c1_lr12r4,c1_lr13r4,c1_lr14r4,c1_lr15r4,c1_lr16r4,c1_lr17r4,c1_lr18r4,c1_lr19r4,c1_lr20r4)), na.rm = TRUE),
  c_1_relationship = rowMeans(select(df,c(c1_lr1r5,c1_lr2r5,c1_lr3r5,c1_lr4r5,c1_lr5r5,c1_lr6r5,c1_lr7r5,c1_lr8r5,c1_lr9r5,c1_lr10r5,c1_lr11r5,c1_lr12r5,c1_lr13r5,c1_lr14r5,c1_lr15r5,c1_lr16r5,c1_lr17r5,c1_lr18r5,c1_lr19r5,c1_lr20r5)), na.rm = TRUE),
  c_1_speedplace = rowMeans(select(df,c(c1_lr1r6,c1_lr2r6,c1_lr3r6,c1_lr4r6,c1_lr5r6,c1_lr6r6,c1_lr7r6,c1_lr8r6,c1_lr9r6,c1_lr10r6,c1_lr11r6,c1_lr12r6,c1_lr13r6,c1_lr14r6,c1_lr15r6,c1_lr16r6,c1_lr17r6,c1_lr18r6,c1_lr19r6,c1_lr20r6)), na.rm = TRUE),
  c_1_histdata = rowMeans(select(df,c(c1_lr1r7,c1_lr2r7,c1_lr3r7,c1_lr4r7,c1_lr5r7,c1_lr6r7,c1_lr7r7,c1_lr8r7,c1_lr9r7,c1_lr10r7,c1_lr11r7,c1_lr12r7,c1_lr13r7,c1_lr14r7,c1_lr15r7,c1_lr16r7,c1_lr17r7,c1_lr18r7,c1_lr19r7,c1_lr20r7)), na.rm = TRUE),
  c_1_speedclaims = rowMeans(select(df,c(c1_lr1r8,c1_lr2r8,c1_lr3r8,c1_lr4r8,c1_lr5r8,c1_lr6r8,c1_lr7r8,c1_lr8r8,c1_lr9r8,c1_lr10r8,c1_lr11r8,c1_lr12r8,c1_lr13r8,c1_lr14r8,c1_lr15r8,c1_lr16r8,c1_lr17r8,c1_lr18r8,c1_lr19r8,c1_lr20r8)), na.rm = TRUE),
  c_1_location = rowMeans(select(df,c(c1_lr1r9,c1_lr2r9,c1_lr3r9,c1_lr4r9,c1_lr5r9,c1_lr6r9,c1_lr7r9,c1_lr8r9,c1_lr9r9,c1_lr10r9,c1_lr11r9,c1_lr12r9,c1_lr13r9,c1_lr14r9,c1_lr15r9,c1_lr16r9,c1_lr17r9,c1_lr18r9,c1_lr19r9,c1_lr20r9)), na.rm = TRUE),
  c_1_flexcollateral = rowMeans(select(df,c(c1_lr1r10,c1_lr2r10,c1_lr3r10,c1_lr4r10,c1_lr5r10,c1_lr6r10,c1_lr7r10,c1_lr8r10,c1_lr9r10,c1_lr10r10,c1_lr11r10,c1_lr12r10,c1_lr13r10,c1_lr14r10,c1_lr15r10,c1_lr16r10,c1_lr17r10,c1_lr18r10,c1_lr19r10,c1_lr20r10)), na.rm = TRUE),
  c_1_multiline = rowMeans(select(df,c(c1_lr1r11,c1_lr2r11,c1_lr3r11,c1_lr4r11,c1_lr5r11,c1_lr6r11,c1_lr7r11,c1_lr8r11,c1_lr9r11,c1_lr10r11,c1_lr11r11,c1_lr12r11,c1_lr13r11,c1_lr14r11,c1_lr15r11,c1_lr16r11,c1_lr17r11,c1_lr18r11,c1_lr19r11,c1_lr20r11)), na.rm = TRUE),
  c_1_marketplace = rowMeans(select(df,c(c1_lr1r12,c1_lr2r12,c1_lr3r12,c1_lr4r12,c1_lr5r12,c1_lr6r12,c1_lr7r12,c1_lr8r12,c1_lr9r12,c1_lr10r12,c1_lr11r12,c1_lr12r12,c1_lr13r12,c1_lr14r12,c1_lr15r12,c1_lr16r12,c1_lr17r12,c1_lr18r12,c1_lr19r12,c1_lr20r12)), na.rm = TRUE)
)
df <- df %>% mutate(across(contains('s5'),binarize))
df <- df %>% mutate(across(contains('c5'),binarize))

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
View(df %>% select(contains('b1'),contains('b2'),contains('c3'),contains('c_1'),-contains('0r')) %>% summarise(across(everything(),meanrm)))


### how to handle b3a 
#how to handle media section d?#

brokerprofile <- df %>% select(broker, contains('b1'),contains('b2'),contains('c3'),-contains('0r'),-contains('98'),-contains('99')) %>% group_by(broker) %>% summarise(across(everything(),mean))
View(brokerprofile)
write.csv(brokerprofile,'brokerstats.csv')

#C1 - openness
#'b2_7','b1_2','b2_4',,'c_1_regulatoryexp'
clustv1 <- c('b1_1','b2_2','b2_5','b2_8','engagedfintech','engagedmarket')
clust1 <- df %>% select(clustv1)
k1<-kmeans(clust1, centers=5, iter.max = 1000, nstart=5)
k1
df$cluster1 <- k1$cluster
clust_stats_1<-df %>% select(cluster1,clustv1,broker,s6r1,s2ar1,s2ar2,s2ar3,reinsprimary,retroprimary,contains('b1'),contains('b2'),contains('c3'),contains('s5r'),contains('c4r'),contains('c5'),pctfac,pcttreaty,pctboth,pctasl,pctqs,pctxol,contains('a7'),contains('c_1'),numlines) %>% select(-contains('0r'),-contains('98'),-contains('99')) %>% group_by(cluster1) %>% summarise(across(everything(),meanrm))
cpcts <- pcts(df,cluster1)
lr <- df %>% group_by(cluster1) %>% summarise(lr=mean(a5,na.rm=TRUE),usebroker=mean(a6,na.rm=TRUE),sizeover1b=mean(if_else(a3==10,1,0)))
clust_stats_1<-bind_cols(clust_stats_1,cpcts,lr)
#write.xlsx(clust_stats_1,'c1.xlsx')

#c2 - needs - removed 'c_1_price','c_1_location','c_1_flexcollateral','c_1_contractflex',,'c_1_marketplace',,'c_1_reassurancehistory','c_1_histdata',
clustv2 <- c('numlinesbracketed','c_1_regulatoryexp','c_1_relationship','c_1_speedplace','c_1_speedclaims','c_1_multiline')
dfn <- df %>% filter(broker==0)
clust2 <- dfn %>% select(clustv2)
k2<-kmeans(clust2, centers=4, iter.max = 1000, nstart=5)
k2
dfn$cluster2 <- k2$cluster
dfq <- dfn %>% select(record,cluster2) %>% right_join(df,by = 'record',copy = FALSE)
df <- dfq %>% mutate(cluster2 = coalesce(cluster2,5))
clust_stats_2<-df %>% select(cluster2,clustv2,broker,s6r1,s2ar1,s2ar2,s2ar3,reinsprimary,retroprimary,contains('b1'),contains('b2'),contains('c3'),contains('s5r'),contains('c4r'),contains('c5'),pctfac,pcttreaty,pctboth,pctasl,pctqs,pctxol,contains('a7'),contains('c_1'),numlines) %>% select(-contains('0r'),-contains('98'),-contains('99')) %>% group_by(cluster2) %>% summarise(across(everything(),meanrm))
cpcts <- pcts(df,cluster2)
lr <- df %>% group_by(cluster2) %>% summarise(lr=mean(a5,na.rm=TRUE),usebroker=mean(a6,na.rm=TRUE),sizeover1b=mean(if_else(a3==10,1,0)))
clust_stats_2<-bind_cols(clust_stats_2,cpcts,lr)
#write.xlsx(clust_stats_2,'c2.xlsx')

#c3
#clustv3 <- c('numlinesbracketed','c_1_regulatoryexp','c_1_relationship','c_1_speedplace','c_1_speedclaims','c_1_multiline')
#clust3 <- df %>% filter(b4a!=4) %>% select(clustv3)
#clust3 <- scale(clust3)
#k3<-kmeans(clust3, centers=5, iter.max = 1000, nstart=5)
#k3
#df$cluster3 <- k3$cluster
#clust_stats_3<-df %>% select(cluster3,clustv3,broker,s6r1,s2ar1,s2ar2,s2ar3,reinsprimary,retroprimary,contains('b1'),contains('b2'),contains('c3'),contains('s5r'),contains('c4r'),contains('c5'),pctfac,pcttreaty,pctboth,pctasl,pctqs,pctxol,contains('a7'),contains('c_1'),numlines) %>% select(-contains('0r'),-contains('98'),-contains('99')) %>% group_by(cluster1) %>% summarise(across(everything(),meanrm))
#cpcts <- pcts(df,cluster3)
#lr <- df %>% group_by(cluster3) %>% summarise(lr=mean(a5,na.rm=TRUE),usebroker=mean(a6,na.rm=TRUE),sizeover1b=mean(if_else(a3==10,1,0)))
#clust_stats_3<-bind_cols(clust_stats_3,cpcts,lr)

pcts(df,cluster1,b1_1<=4)
pcts(df,cluster1,b2_2<=4)
pcts(df,cluster1,b2_5<=4)
pcts(df,cluster1,b2_8<=4)
pcts(df,cluster1,engagedfintech)
pcts(df,cluster1,engagedmarket)
pcts(df,cluster1)
pcts(df,cluster1,between(a5,60,75))
pcts(df,cluster1,a3==10)
pcts(df,cluster1,a1==6)
pcts(df,cluster1,s5r21)
pcts(df,cluster1,s5r18)
pcts(df,cluster1,c5r11)
pcts(df,cluster1,numlinesbracketed)
pcts(df,cluster1,c4r3)
View(df %>% select(cluster1,contains('s5')) %>% group_by(cluster1) %>% summarise(across(everything(),meanrm)))
View(df %>% select(cluster1,contains('c5')) %>% group_by(cluster1) %>% summarise(across(everything(),meanrm)))
View(df %>% select(cluster1,contains('d1')) %>% select(-contains('c2')) %>% group_by(cluster1) %>% summarise(across(everything(),meanrm)))
print(pcts(df,cluster1,s3),n=100)
print(pcts(df,cluster1,s4),n=100)
pcts(df,cluster1,s6r1>=5)

pcts(df,cluster2,c_1_regulatoryexp>=2.5)
pcts(df,cluster2,c_1_relationship>=2.5)
pcts(df,cluster2,c_1_speedplace>=2.5)
pcts(df,cluster2,c_1_speedclaims>=2.5)
pcts(df,cluster2,c_1_multiline>=2.5)
pcts(df,cluster2,c_1_price>=2.5)
pcts(df,cluster2,c_1_contractflex>=2.5)
pcts(df,cluster2,c_1_histdata>=2.5)
pcts(df,cluster2)
pcts(df,cluster2,between(a5,60,75))
pcts(df,cluster2,a3==10)
pcts(df,cluster2,a1==6)
pcts(df,cluster2,s5r21)
pcts(df,cluster2,s5r18)
pcts(df,cluster2,c5r11)
pcts(df,cluster2,numlinesbracketed)
cb(df %>% select(cluster2,contains('s5')) %>% group_by(cluster2) %>% summarise(across(everything(),meanrm)))
cb(df %>% select(cluster2,contains('c5')) %>% group_by(cluster2) %>% summarise(across(everything(),meanrm)))
cb(df %>% select(cluster2,contains('d1')) %>% select(-contains('c2')) %>% group_by(cluster2) %>% summarise(across(everything(),meanrm)))
pcts(df,cluster2,a4>=4)
print(pcts(df,cluster2,s3),n=100)
print(pcts(df,cluster2,s4),n=100)
pcts(df,cluster2,s6r1>=5)

#write_sav(df,'clust_assign.sav')
