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

for (f in user_walking) {
  temp=get(f)
  HAR_total<-rbind(HAR_total,temp %>% mutate(exp_no=unlist(regmatches(f,gregexpr("[[:digit:]]+",f)[1]))[1],id=unlist(regmatches(f,gregexpr("[[:digit:]]+",f)[1]))[2],activity=unlist(str_split(f,"\\_"))[1]))
}

head(HAR_total,n=100)

#magnitude of V",cycle 
HAR_total=mag(HAR_total,"userAcceleration")
HAR_total=mag(HAR_total,"rotationRate")
head(HAR_total,n=100)

save.image("HAR_total.Rdata")
