library(dplyr)
library(tidyr)
getwd()
setwd("C://Users//Admin//Documents//R lang//R_Projects//Datasets")
store_train=read.csv("store_train.csv",stringsAsFactors = F)
store_test= read.csv("store_test.csv",stringsAsFactors = F)

store_test$store=NA

store_train$data='train'
store_test$data='test'

store_all=rbind(store_train,store_test)
glimpse(store_all)
lapply(store_all,function(x) length(unique(x)))
lapply(store_all,function(x) sum(is.na(x)))

# drop storecode

store_all$storecode=NULL

glimpse(store_all)

store_all=store_all%>%mutate(population=ifelse(is.na(population),mean(population,na.rm=T),population),country=ifelse(is.na(country),6,country))

store_all=store_all%>%mutate(country=as.character(country))%>%select(-State)

#Create Dummies
CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}


glimpse(store_all)
names(store_all)[sapply(store_all,function(x) is.character(x))]

cat_cols=c("state_alpha","store_Type")


for(cat in cat_cols){
  store_all=CreateDummies(store_all,cat,70)
}

## separate train and test


store_train=store_all %>% filter(data=='train') %>% select(-data)
store_test=store_all %>% filter(data=='test') %>% select(-data,-store)


#VIF

set.seed(2)
store_samp=sample(1:nrow(store_train),0.7*nrow(store_train))
store_train1=store_train[store_samp,]
store_train2=store_train[-store_samp,]

library(car)
# we'll take vif cutoff as 10

for_vif=lm(store~.-Id,data=store_train1)
sort(vif(for_vif),decreasing = T)[1:3]

summary(for_vif)

log_fit=glm(store~.-Id-sales0-sales2-sales3,data=store_train1,family = "binomial")

log_fit=step(log_fit)
formula(log_fit)

#AIC=3823.5
summary(log_fit)
log_fit=glm(store ~  sales4 + CouSub + population + country_19 + 
              country_11 + state_alpha_PR + 
              state_alpha_IN + state_alpha_TN +  state_alpha_CT + 
              state_alpha_VT + state_alpha_NH + state_alpha_MA + state_alpha_ME
             ,data=store_train1,family = "binomial")


summary(log_fit)

###
library(pROC)
val.score=predict(log_fit,newdata=store_train2,type ='response')
auc(roc(store_train2$store,val.score))
#AUC =0.7618

errors=store_train2$store-val.score

#RMSE =  0.4375893
errors**2 %>% mean() %>% sqrt()

### model for predcition on the entire data
for_vif=lm(store~.-Id-sales0-sales2-sales3,data=store_train)
sort(vif(for_vif),decreasing = T)[1:3]
log_fit1=glm(store~.-Id-sales0-sales2-sales3,data=store_train,family = "binomial")

log_fit1=step(log_fit1)
formula(log_fit)

fit.final=glm(store ~ sales4 + CouSub + population + country_19 + country_11 + 
                state_alpha_PR + state_alpha_IN + state_alpha_TN + 
                state_alpha_VT + state_alpha_NH + state_alpha_MA + state_alpha_ME,data=store_train,family = 'binomial')

summary(fit.final)

test.pred=predict(fit.final,newdata=store_test,type = 'response')
test.pred

write.csv(test.pred,"Himabindu_Rachamallu_P2_part2.csv",row.names = F)

#Find cutoff from tarin data
train.score=predict(fit.final,newdata = store_train,type='response')

real=store_train$store
cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

for(cutoff in cutoffs){
  
  predicted=as.numeric(train.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,
                    c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]
cutoff_data

#### visualise how these measures move across cutoffs
library(ggplot2)
ggplot(cutoff_data,aes(x=cutoff,y=M))+geom_line()

library(tidyr)

cutoff_long=cutoff_data %>% 
  gather(Measure,Value,Sn:M)
?gather


ggplot(cutoff_long,aes(x=cutoff,y=Value,color=Measure))+geom_line()


my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff
#0.403

#Perform on test data
test.prob.score= predict(fit.final,newdata = store_test,type='response')


test.predicted=as.numeric(test.prob.score>my_cutoff)
length(test.predicted)
test.predicted
store_test$store=test.predicted
auc(roc(store_test$store,test.prob.score))




