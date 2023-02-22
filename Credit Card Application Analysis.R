library(dplyr)
library(lattice)
library(plyr)
library(readr)

credit_application <- read_csv("credit application.csv")
df=credit_application
head(df,5)
colnames(df) = c("id","gender","owncar","ownland","income","incometype","edu","status","house","age","exp","occupation","familycount")

#filling empty string
#converting age days to years
#converting exp days to years
df$age=df$age/-365            
df$exp=df$exp/-365            
df$exp <- round(df$exp)
df$age <- round(df$age)
df=df %>% mutate(agerange = case_when(age>=20 & age<=30 ~ "20 to 30",
                                      age>=30 & age<=40 ~ "30 to 40",
                                      age>=40 & age<=50 ~ '40 to 50',
                                      age>=50 ~ "above 50"))

#age wise experience
ggplot(data=df, aes(x=age, y=exp)) +
  geom_bar(stat="identity")

#removing outliers in exp
boxplot(exp~ occupation, data =df)
Q1 <- quantile(df$exp, .25)
Q3 <- quantile(df$exp, .75)
IQR <- IQR(df$exp)
df <- subset(df, df$exp > (Q1 - 1.5*IQR) & df$exp < (Q3 + 1.5*IQR))
View(df)

#count of each applicant'job
df %>% ggplot(aes(occupation))+
  geom_bar(stat="Count",fill="#A87809")+theme(axis.text.x = element_text(angle = 90))
hist=subset(df,occupation=="Laborers")
histogram(~income|occupation,data=hist)

#distribution of income by gender
histogram(~income|gender,data=df,breaks=50)
dm=subset(df,gender=='M')
dfe=subset(df,gender=='F')

#distribution of income by gender who having own land
histogram(~income|ownland,data=dm,breaks=50)
histogram(~income|ownland,data=dfe,breaks=50)

#subseting ownland by applicant gender
d3=subset(dm,ownland=='Y')
d4=subset(dfe,ownland=='Y')

#distribbution of income by age range
histogram(~income|agerange,data=dm,breaks=50)
histogram(~income|agerange,data=dfe,breaks=50)

df5 = rbind(d3,d4)
dim(df5)

#subseting age range because of regular income

df5 %>% ggplot(aes(agerange))+
  geom_bar(stat="Count",fill="#09a853")+theme(axis.text.x = element_text(angle = 90))

d6=subset(df5,agerange=='30 to 40')
d7=subset(df5,agerange=='40 to 50')

d8 = rbind(d6,d7)
summary(d8)

#subsetting married people coz of both members income
count(d8,'status')
d8 %>% ggplot(aes(status))+
  geom_bar(stat="Count",fill="#2084ba")+theme(axis.text.x = element_text(angle = 90))
d9=subset(d8,status=='Married')

#subsetting married people who have own house
count(d9,'house')
d9 %>% ggplot(aes(house))+
  geom_bar(stat="Count",fill="#b50954")+theme(axis.text.x = element_text(angle = 90))

d10=subset(d9,house=="House / apartment")
head(d10,5)

summary(d10)
count(d10,"occupation")
