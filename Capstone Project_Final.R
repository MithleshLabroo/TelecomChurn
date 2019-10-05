library(dplyr)
library(ggplot2)
library(irr)
library(caret)
library(gains)
data<-read.csv("E:/R-Studio/RWorkspace/Capstone Project/sampletelecomfinal.csv", na.strings = c("", NA))
summary(data)

colnames(data)
data<-data[,-c(12, 47, 48, 49, 52, 53, 55, 61, 62, 63, 64, 66, 72)]
colnames(data) 
data$churn<-as.factor(data$churn)
data<-data[,-c(30, 55)]
colnames(data) 

########Handling Missiing Values###############
unique(data$mou_Mean)
summary(data)
data$mou_Mean[is.na(data$mou_Mean)]<-mean(data$mou_Mean, na.rm = TRUE)
data$totmrc_Mean[is.na(data$totmrc_Mean)]<-mean(data$totmrc_Mean, na.rm = TRUE)
data$rev_Range[is.na(data$rev_Range)]<-mean(data$rev_Range, na.rm = TRUE)
data$mou_Range[is.na(data$mou_Range)]<-mean(data$mou_Range, na.rm = TRUE)
data$change_mou[is.na(data$change_mou)]<-mean(data$change_mou, na.rm = TRUE)
data$ovrrev_Mean[is.na(data$ovrrev_Mean)]<-mean(data$ovrrev_Mean, na.rm = TRUE)
data$rev_Mean[is.na(data$rev_Mean)]<-mean(data$rev_Mean, na.rm = TRUE)
data$ovrmou_Mean[is.na(data$ovrmou_Mean)]<-mean(data$ovrmou_Mean, na.rm = TRUE)
data$avg6mou[is.na(data$avg6mou)]<-mean(data$avg6mou, na.rm = TRUE)
data$avg6qty[is.na(data$avg6qty)]<-mean(data$avg6qty, na.rm = TRUE)
data$age1[is.na(data$age1)]<-mean(data$age1, na.rm = TRUE)
data$age2[is.na(data$age2)]<-mean(data$age2, na.rm = TRUE)
data$hnd_price[is.na(data$hnd_price)]<-mean(data$hnd_price, na.rm = TRUE)
data$roam_Mean[is.na(data$roam_Mean)]<-mean(data$roam_Mean, na.rm = TRUE)
data$da_Mean[is.na(data$da_Mean)]<-mean(data$da_Mean, na.rm = TRUE)
data$da_Range[is.na(data$da_Range)]<-mean(data$da_Range, na.rm = TRUE)
data$datovr_Mean[is.na(data$datovr_Mean)]<-mean(data$datovr_Mean, na.rm = TRUE)
data$datovr_Range[is.na(data$datovr_Range)]<-mean(data$datovr_Range, na.rm = TRUE)

unique(data$prizm_social_one)

data$prizm_social_one[is.na(data$prizm_social_one)]<- "S"
data$area[is.na(data$area)]<- "NEW YORK CITY AREA"
data$hnd_webcap[is.na(data$hnd_webcap)]<- "WCMB"
data$marital[is.na(data$marital)]<- "U"
data$ethnic[is.na(data$ethnic)]<- "N"
data$dwlltype[is.na(data$dwlltype)]<- "S"
data$car_buy[is.na(data$car_buy)]<- "UNKNOWN"
#########data$csa[is.na(data$csa)]<- "NYCBRO917"

boxplot(data$mou_Mean)

#####################
data%>%count(churn,levels=mtrcycle)%>%filter(churn==1)->datC1
datC1$N<-unclass(data%>%filter(mtrcycle%in%datC1$levels)%>%count(mtrcycle))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N
datC1$Var.Name<-rep("mtrcycle",nrow(datC1))
data$mtrcycle[is.na(data$mtrcycle)]<- 0

data%>%count(churn,levels=truck)%>%filter(churn==1)->datC2
datC2$N<-unclass(data%>%filter(truck%in%datC2$levels)%>%count(truck))[[2]]
datC2$ChurnPerc<-datC2$n/datC2$N
datC2$Var.Name<-rep("truck",nrow(datC2))
data$truck[is.na(data$truck)]<- 1

data%>%count(churn,levels=forgntvl)%>%filter(churn==1)->datC3
datC3$N<-unclass(data%>%filter(forgntvl%in%datC3$levels)%>%count(forgntvl))[[2]]
datC3$ChurnPerc<-datC3$n/datC3$N
datC3$Var.Name<-rep("forgntvl",nrow(datC3))
data$forgntvl[is.na(data$forgntvl)]<- 0

unique(data$area)
data%>%count(churn,levels=area)%>%filter(churn==1)->datC4
datC4$N<-unclass(data%>%filter(area%in%datC4$levels)%>%count(area))[[2]]
datC4$ChurnPerc<-datC4$n/datC4$N                                                               
datC4$Var.Name<-rep("area",nrow(datC4))
datC4$ChurnPerc<-sort(datC4$ChurnPerc, decreasing = FALSE)
data$area<-ifelse(data$area == "ATLANTIC SOUTH AREA"|data$area=="CALIFORNIA NORTH AREA"|data$area=="CENTRAL/SOUTH TEXAS AREA",0,
                  ifelse(data$area=="CHICAGO AREA"|data$area=="DALLAS AREA"|data$area=="DC/MARYLAND/VIRGINIA AREA"|data$area=="GREAT LAKES AREA"|data$area=="HOUSTON AREA"|data$area=="LOS ANGELES AREA"|data$area=="MIDWEST AREA"|data$area=="NEW ENGLAND AREA"|data$area=="NEW YORK CITY AREA"|data$area=="NORTH FLORIDA AREA"|data$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,2))
unique(data$area)
head(data$area,30)

unique(data$ethnic)
data%>%count(churn,levels=ethnic)%>%filter(churn==1)->datC5
datC5$N<-unclass(data%>%filter(ethnic%in%datC5$levels)%>%count(ethnic))[[2]]
datC5$ChurnPerc<-datC5$n/datC5$N                                                               
datC5$Var.Name<-rep("ethnic",nrow(datC5))
datC5$ChurnPerc<-sort(datC5$ChurnPerc, decreasing = FALSE)

data$ethnic<-ifelse(data$ethnic == "B"|data$ethnic=="C"|data$ethnic=="D"|data$ethnic=="F"|data$ethnic=="G",0,
                    ifelse(data$ethnic=="H"|data$ethnic=="I"|data$ethnic=="J"|data$ethnic=="M"|data$ethnic=="N"|data$ethnic=="O"|data$ethnic=="P"|data$ethnic=="R",1,2))
unique(data$ethnic)
head(data$ethnic,20)

unique(data$models)
data%>%count(churn,levels=models)%>%filter(churn==1)->datC6
datC6$N<-unclass(data%>%filter(models%in%datC6$levels)%>%count(models))[[2]]
datC6$ChurnPerc<-datC6$n/datC6$N                                                               
datC6$Var.Name<-rep("models",nrow(datC6))
datC6$ChurnPerc<-sort(datC6$ChurnPerc, decreasing = FALSE)
data$models<-ifelse(data$models=="1"|data$models=="2"|data$models=="3",0,1)
unique(data$models)
head(data$models,20)

unique(data$actvsubs)
data%>%count(churn,levels=actvsubs)%>%filter(churn==1)->datC7
datC7$N<-unclass(data%>%filter(actvsubs%in%datC7$levels)%>%count(actvsubs))[[2]]
datC7$ChurnPerc<-datC7$n/datC7$N                                                               
datC7$Var.Name<-rep("actvsubs",nrow(datC7))
datC7$ChurnPerc<-sort(datC7$ChurnPerc, decreasing = FALSE)
data$actvsubs<-ifelse(data$actvsubs=="0"|data$actvsubs=="1"|data$actvsubs=="2",0,1)
unique(data$actvsubs)
head(data$actvsubs,20)

unique(data$uniqsubs)
data%>%count(churn,levels=uniqsubs)%>%filter(churn==1)->datC8
datC8$N<-unclass(data%>%filter(uniqsubs%in%datC8$levels)%>%count(uniqsubs))[[2]]
datC8$ChurnPerc<-datC8$n/datC8$N                                                               
datC8$Var.Name<-rep("uniqsubs",nrow(datC8))
datC8$ChurnPerc<-sort(datC8$ChurnPerc, decreasing = FALSE)
data$uniqsubs<-ifelse(data$uniqsubs=="1"|data$uniqsubs=="2",0,1)
unique(data$uniqsubs)
head(data$uniqsubs,20)

#######Test and Train dataset###########
set.seed(200)
index<-sample(nrow(data), 0.70*nrow(data), replace = F)
train<-data[index,]
test<-data[-index,]
colnames(data)

###########Model Building##############
mod<-glm(churn~., data = train[,-64], family = "binomial")
summary(mod)

mod1<-glm(churn~totmrc_Mean+mou_Range+drop_blk_Mean+owylis_vce_Range+months+eqpdays+
            ovrrev_Mean+asl_flag+prizm_social_one+refurb_new+age1+uniqsubs+
            blck_dat_Mean+datovr_Mean+datovr_Range+adjmou+adjrev, data = train, family = "binomial")
summary(mod1)

unique(train$prizm_social_one)
sum(is.na(train))

train$prizm_social_one_S<-ifelse(train$prizm_social_one=="S", 1,0)
train$prizm_social_one_C<-ifelse(train$prizm_social_one=="C", 1,0)
train$refurb_new_N<-ifelse(train$refurb_new=="N", 1,0)
train$refurb_new_R<-ifelse(train$refurb_new=="R", 1,0)
train$asl_flag_N<-ifelse(train$asl_flag=="N", 1,0)
train$asl_flag_Y<-ifelse(train$asl_flag=="Y", 1,0)

test$prizm_social_one_S<-ifelse(test$prizm_social_one=="S", 1,0)
test$prizm_social_one_C<-ifelse(test$prizm_social_one=="C", 1,0)
test$refurb_new_N<-ifelse(test$refurb_new=="N", 1,0)
test$refurb_new_R<-ifelse(test$refurb_new=="R", 1,0)
test$asl_flag_N<-ifelse(test$asl_flag=="N", 1,0)
test$asl_flag_Y<-ifelse(test$asl_flag=="Y", 1,0)

mod2<-glm(churn~mou_Mean+totmrc_Mean+months+eqpdays+mou_Range+ovrrev_Mean+avgmou+mou_pead_Mean+
            asl_flag_Y+prizm_social_one_S+refurb_new_R+age1+uniqsubs+
            datovr_Mean+datovr_Range+adjmou, data = train, family = "binomial")
summary(mod2)

library(car)
vif(mod2)

predicted<-mod2$fitted.values
head(predicted)
table(data$churn)/nrow(data)

predbkt<-ifelse(predicted>=0.2438,1,0)
table(predbkt,train$churn)

library(ROCR)
pred<-prediction(predicted, train$churn)
perf<-performance(pred, "tpr", "fpr")
plot(perf)
abline(0,1)

auc<-performance(pred, "auc")
auc


#######FINAL Model#########
mod3<-glm(churn~mou_Mean+totmrc_Mean+months+eqpdays+mou_Range+ovrrev_Mean+avgmou+mou_pead_Mean+
            asl_flag_Y+prizm_social_one_S+refurb_new_R+age1+
            uniqsubs+adjmou, data = train, family = "binomial")
summary(mod3)

library(car)
vif(mod3)

predicted<-mod3$fitted.values
head(predicted)
table(data$churn)/nrow(data)

predbkt<-ifelse(predicted>=0.245,1,0)
table(predbkt,train$churn)

library(ROCR)
pred<-prediction(predicted, train$churn)
perf<-performance(pred, "tpr", "fpr")
plot(perf)
abline(0,1)

auc<-performance(pred, "auc")
auc

coeff<-sort(mod3$coefficients, decreasing = TRUE)
coeff


test$pred1<-predict(mod3, type = "response", newdata = test)
head(test$pred1)
t1<- test%>%select(churn,pred1, totrev, Customer_ID)%>%filter(churn == 1 & pred1>=0.245)
summary(t1$totrev)
nrow(data[data$churn==0,])

t4<-data%>%select(totrev,churn)%>%filter(churn == 1)%>%arrange(desc(totrev))%>%
  mutate(quantile=ntile(totrev,5))%>%group_by(quantile)%>%summarize(N=n())
t3<-data%>%select(totrev,churn)%>%filter(churn == 1)%>%arrange(desc(totrev))
t5<-head(t3, 624)
max(t5$totrev)
min(t5$totrev)

summary(t3)
