#-------------------(핸드폰_센서_데이터_소개)---------------------
setwd("D:/Omega/A_DeviceMotion_data")
d=getwd()
fls=dir(d,recursive = T) #recursive -> 디렉토리 언더 모든 데이터 가져오기
fls
length(fls)
save.image("data.RData")

#recursive 구현
library(stringr)
for (f in fls) {
  a=file.path(str_c(d,"/",f))
  temp=read.csv(a)
  assign(f,temp)
}
assign() #->값을 이름으로 바꿔주는 형태


mag=function(df,column) {
  df[,str_c("mag",column)]=with(df,sqrt(get(str_c(column,".x"))^2+get(str_c(column,".y"))^2+get(str_c(column,".z"))^2))
  return(df)
}

mag(`wlk_8/sub_8.csv`,"userAcceleration")
`wlk_8/sub_8.csv`=mag(`wlk_8/sub_8.csv`,"userAcceleration")
mag(`wlk_8/sub_8.csv`,"rotationRate")


#-------------------(핸드폰_데이터로_행동분류)-------------------------
library(dplyr)
fls
user1=fls[str_detect(fls,"sub_1\\.")]
length(user1)
user_walking=user1[str_detect(user1,"wlk")]
user_walking


#sample_data_eject
user1_walking_total=data.frame()

for (f in user_walking) {
  temp=get(f)
  user1_walking_total<-rbind(user1_walking_total,temp %>% mutate(exp_no=unlist(regmatches(f,gregexpr("[[:digit:]]+",f)[1]))[1],id=unlist(regmatches(f,gregexpr("[[:digit:]]+",f)[1]))[2]))
}
user1_walking_total=user1_walking_total[2:length(user1_walking_total)]
user1_walking_total

#making_magnitude_data_column
user1_walking_total=mag(user1_walking_total,"userAcceleration")
user1_walking_total

#sample_data_visualization
user1_walking_total=user1_walking_total %>% group_by(exp_no) %>% mutate(time=row_number()) %>% ungroup()

library(ggplot2)
windows()
ggplot(user1_walking_total,aes(x=time,y=maguserAcceleration))+geom_line()+facet_wrap(.~exp_no,nrow=3)

#total_data_eject
HAR_total=data.frame()

for (f in fls) {
  temp=get(f)
  HAR_total<-rbind(HAR_total,(temp %>% mutate(exp_no=unlist(regmatches(f,gregexpr("[[:digit:]]+",f)[1]))[1],id=unlist(regmatches(f,gregexpr("[[:digit:]]+",f)[1]))[2],activity=unlist(str_split(f,"\\_"))[1])))
}

HAR_total %>% group_by(activity) %>% summarise(n=n())
summary(as.factor(HAR_total$activity))

#magnitude of V",cycle 
HAR_total=mag(HAR_total,"userAcceleration")
HAR_total=mag(HAR_total,"rotationRate")
head(HAR_total,n=100)
HAR_total #total 1403228 rows

save.image("HAR_total.RData")
load("HAR_total.RData")

#install.packages("moments")
library(moments);library(tidyverse) #moments for skewness()
HAR_summary=HAR_total %>% group_by(id, exp_no, activity) %>% summarise_at(.vars=c("maguserAcceleration","magrotationRate"),.funs = c(mean,min,max,sd,skewness))
HAR_summary


#jog_data_visualization
library(dplyr)
library(stringr)
fls
user1=fls[str_detect(fls,"sub_1\\.")]
length(user1)
user_jogging=user1[str_detect(user1,"jog")]
user_jogging


##sample_data_eject
user1_jogging_total=data.frame()

for (f in user_walking) {
  temp=get(f)
  user1_jogging_total<-rbind(user1_jogging_total,temp %>% mutate(exp_no=unlist(regmatches(f,gregexpr("[[:digit:]]+",f)[1]))[1],id=unlist(regmatches(f,gregexpr("[[:digit:]]+",f)[1]))[2]))
}
user1_jogging_total=user1_jogging_total[2:length(user1_jogging_total)]
user1_jogging_total

#making_magnitude_data_column
user1_jogging_total=mag(user1_jogging_total,"userAcceleration")
user1_jogging_total

#sample_data_visualization
user1_jogging_total=user1_jogging_total %>% group_by(exp_no) %>% mutate(time=row_number()) %>% ungroup()

library(ggplot2)
windows()
ggplot(user1_jogging_total,aes(x=time,y=maguserAcceleration))+geom_line()+facet_wrap(.~exp_no,nrow=3)

#-------------------(행동분류모델)-----------------------------
#install.packages("RWeka") #you need jdk_ver>=8.0 (I use 11.0.9)
library(RWeka) #https://cran.r-project.org/web/packages/RWeka/index.html
RF=make_Weka_classifier("weka/classifiers/trees/RandomForest") #this is using for calling function
RF
Bayes_net=make_Weka_classifier("weka/classifiers/bayes/BayesNet")
Bayes_net
J48=make_Weka_classifier("weka/classifiers/trees/RandomForest")
J48
save.image("HAR_03.Rdata")
load("HAR_03.Rdata")

library(dplyr);library(stringr)
HAR_summary$activity=as.factor(HAR_summary$activity)
activity=HAR_summary %>% ungroup() %>% select(c(colnames(HAR_summary)[str_detect(colnames(HAR_summary),"mag")], "activity"))

str_detect(colnames(HAR_summary),'mag')

c(colnames(HAR_summary)[FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE])

m=J48(activity~.,data=activity)
e=evaluate_Weka_classifier(m,numFolds=10,complexity = T,class=T)
e

r=RF(activity~.,data=activity)
e=evaluate_Weka_classifier(m,numFolds=10,complexity = T,class=T)
e

setwd("C:\Users\sherm\Desktop\unstructed_data")
save.image("HAR_04.Rdata")
load("HAR_04.Rdata")

#-------------------(기술통계량)----------------------------
alpha=sample(1:10,10)
m=cummax(alpha)
m

#install.packages("seewave")
library(seewave)
t=seq(0,1,0.01)
x=cos(2*pi*t)
plot(t,x,"l")
y=rms(x) #rms is Root mean square 제곱평균제곱근 ref) https://m.blog.naver.com/pkw00/220226903866


#나일강의 유량 파악
nile_river=Nile
windows()
plot(nile_river)
hist(nile_river)

##대푯값 추출
n=length(nile_river)
mean(nile_river)
median(nile_river)
which.max(nile_river) #Find mode(최빈값)
prod(nile_river)^(1/n) #geometric mean
1/mean(1/nile_river) #harmonic mean -> It is same F1 score


#왜도 활용 
#install.packages("fBasics")
library(fBasics);library(ggplot2)
str(diamonds)
skewness(diamonds$price)

ggplot(diamonds,aes(x=price))+geom_histogram()+facet_grid(color~.) #It is two way to show many graph


##분포 형태와 대칭정도
with(diamonds,tapply(price,color,skewness))
with(diamonds,tapply(price,color,kurtosis))

#install.packages("pracma")
#install.packages("signal")
#install.packages("e1071")
library(pracma);library(e1071);library(signal);library(signal);library(dplyr);library(RWeka)

rss=function(x) rms(x)*(length(x))^0.5
HAR_summary_extend= HAR_total %>% group_by(id, exp_no, activity) %>% summarize_at(.vars = c("maguserAcceleration","magrotationRate"),.funs = c(mean, min, max, sd, skewness, rms, rss ,IQR, e1071::kurtosis))
sapply(HAR_summary_extend,class)
length(HAR_summary_extend)

HAR_summary_extend2=HAR_summary_extend %>% ungroup() %>% select(-c("id","exp_no"))
HAR_summary_extend2

library(RWeka)
m2=J48(as.factor(activity)~.,data=HAR_summary_extend2)
m2
e2=evaluate_Weka_classifier(m2,numFolds = 10,complexity = T,class = TRUE)
e2

setwd("C:/Users/sherm/Desktop/unstructed_data")
save.image("HAR_05.Rdata")
load("HAR_05.Rdata")

#PCA
mtcars.pca=prcomp(HAR_summary_extend2 %>% ungroup %>% select(-activity),center = TRUE, scale. = TRUE)
mtcars.pca

plot(mtcars.pca$sdev)+title("Screeplot")

m3=J48(as.factor(activity)~.,data=HAR_summary_extend2 %>% select(1,2,11,12,14,15))
e3=evaluate_Weka_classifier(m3,numFolds = 10,complexity = TRUE,class = TRUE)
e3

#-------------------(peak)------------------
#install.package("pracma")
library(pracma)
x=seq(0,1,len=1024)
pos=c(0.1,0.13,0.15,0.23,0.25,0.4,0.44,0.65,0.76,0.78,0.81)
hgt=c(4,5,3,4,5,4.2,2.1,4.3,2.1,3.1,5.1,4.2)
wdt=c(0.005,0.005,0.006,0.011,0.01,0.03,0.01,0.01,0.005,0.008,0.005)
pSignal=numeric(length(x))
for (i in seq(along=pos)) {
  pSignal=pSignal+hgt[i]/(1+abs((x-pos[i])/wdt[i]))^4
}
plot(pSignal,type="l",col='navy')

x=findpeaks(pSignal,npeaks = 3,threshold = 4, sortstr = T)
points(x[,2],x[,1],pch=20,col="maroon")

save.image("HAR_06.Rdata")
load("HAR_06.Rdata")