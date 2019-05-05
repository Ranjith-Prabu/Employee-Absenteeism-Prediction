rm(list=ls())

getwd()

library(readxl)
library(stringr) 
library(stringi) 
library(MASS) 
library(DMwR)
library(plyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(corrgram)
library(caret)
library(tidyverse)
library(rpart)
library(car)
library(rpart)
library(dummies)
library(e1071)


#PROJECT EMPLOYEE ABSENTEEISM:

emp=read_xls("C:/Users/Ranjith P/Desktop/Employees/Absenteeism_at_work_Project.xls")

str(emp)

summary(emp)

dim(emp)
#_________________________________________________________________________________________________________#


#Checking Missing Values:
sum(is.na(emp))
mv=data.frame(apply(emp, 2, function(x){sum(is.na(x))}))
mv
mv$Var=rownames(mv)
rownames(mv)=NULL
mv=mv[,c(2,1)]
names(mv)[2] <- "Missing_Value"


#Calculate Missing Value Percentage
mv$missingpercent=((mv$Missing_Value/nrow(emp))* 100)
mv$missingpercent<- round(mv$missingpercent,2)

#Plot:
ggplot(data = mv, aes(x=reorder(Var,missingpercent),y = missingpercent,col = "blue"))+
  geom_bar(stat = "identity",fill = "pink")+xlab("Variables")+
  ggtitle("Missing data value") + theme_bw() +theme(axis.text.x = element_text(angle = 90))

#Omitting the observations which has NA in target column:
emp1=emp[!is.na(emp$`Absenteeism time in hours`),]
summary(emp1)

#Imputing NA's in predictors:
sum(is.na(emp1))
mv1=data.frame(apply(emp1, 2, function(x){sum(is.na(x))}))
names(mv1)[1]="missingvalues"
mv1$missingpercentage=round((mv1$missingvalues/nrow(emp))*100,2)
mv1

emp2=data.frame(emp1)

emp3=knnImputation(emp2,k=3)
sum(is.na(emp3))

#_________________________________________________________________________________________________________#


#Exploratory Data Analysis & Data Pre-processing:
colnames(emp3)
str(emp3)

#Rounding off the decimals in numerical variables:
#num=dplyr::select_if(emp3, is.numeric)

emp3=trunc(emp3,0)
#unique(emp3$Reason.for.absence) #to verify 

#Changing the structure of some variables to factor.
fact <- c(1:5,12:17)

emp3[fact] <- lapply(emp3[fact], factor)
str(emp3)

#_________________________________________________________________________________________________________#


#Recoding Factor variables:
#Reason.for.absence
summary(emp3$Reason.for.absence)

levels(emp3$Reason.for.absence)


emp3 <- emp3 %>%mutate(Reason.for.absence = fct_recode(Reason.for.absence,'unknown'='0','infectious,parasitic diseases'='1',
                                                   'Neoplasms'='2','Diseases of the blood'='3','Endocrine and metabolic diseases'='4','Mental and behavioural disorders'='5', 
                                                   'Diseases of the nervous system'='6','Diseases of the eye and adnexa'='7','Diseases of the ear and mastoid process'='8',
                                                   'Diseases of the circulatory system'='9','Diseases of the respiratory system'='10','Diseases of the digestive system'='11', 
                                                   'Diseases of the skin and subcutaneous tissue'='12','Diseases of the musculoskeletal system and connective tissue'='13', 
                                                   'Diseases of the genitourinary system'='14','Pregnancy, childbirth and the puerperium'='15','Certain conditions originating in the perinatal'='16', 
                                                   'Congenital malformations, deformations and chromosomal abnormalities'= '17','Symptoms, signs and abnormal clinical  findings'='18',
                                                   'Injury, poisoning and certain other consequences of external causes'= '19','causes of morbidity and mortality'='20',
                                                   'Factors influencing health status and contact with health services'='21','patient follow-up'='22','medical consultation'='23','blood donation'='24',
                                                   'laboratory examination'='25','unjustified absence'='26','physiotherapy'='27','dental consultation'='28'))

summary(emp3$Reason.for.absence)
colnames(emp3)


#Month.of.absence
summary(emp3$Month.of.absence)
emp3 <- emp3 %>%mutate(Month.of.absence= fct_recode(Month.of.absence,'unknown'='0','Jan'='1','Feb'='2','Mar'='3','Apr'='4','May'='5',
                                                'Jun'='6','Jul'='7','Aug'='8','Sep'='9','Oct'='10','Nov'='11','Dec'='12'))


#Day.of.the.week
summary(emp3$Day.of.the.week)
emp3=emp3 %>%mutate(Day.of.the.week=fct_recode(Day.of.the.week,'mon'='2','tues'='3','wed'='4','thurs'='5','fri'='6'))


#Seasons
summary(emp3$Seasons)
emp3=emp3 %>%mutate(Seasons= fct_recode(Seasons,'summer'='1','autumn'='2','winter'='3','spring'='4'))


#Education
summary(emp3$Education)
emp3=emp3 %>%mutate(Education = fct_recode(Education,'highschool'='1','graduate'='2',
                                           'postgraduate'='3','master& doctrate'='4'))


#Thus the levels of some factor variables have been recoded.
#_________________________________________________________________________________________________________#


#Visualisation:
absent <- as.data.frame( emp3 %>% select(everything()) %>% filter(Absenteeism.time.in.hours > 0))

#Reason.for.absence
ggplot(emp3,aes(emp3$Reason.for.absence))+geom_bar(aes(fill = emp3$Reason.for.absence),position = "dodge")+
  labs(title = "Reason.for.absence")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_dodge(0.5),vjust=-0.2)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Absent in Reason wise
emp3$Reason.for.absence <- fct_explicit_na(emp3$Reason.for.absence)
Reason <-  as.data.frame(absent %>% group_by(Reason.for.absence) %>% summarise(count= n(), percent = round(count*100/nrow(absent),1))%>% arrange(desc(count)))
ggplot(Reason,aes(x = reorder(Reason.for.absence,percent), y= percent, fill= Reason.for.absence)) + geom_bar(stat = 'identity') + coord_flip() + theme(legend.position='none') +  
  geom_text(aes(label = percent), vjust = 0.5, hjust = 1.1) + xlab('Reason for absence')


#Month.of.absence
ggplot(emp3,aes(emp3$Month.of.absence))+geom_bar(aes(fill = emp3$Month.of.absence),position = "dodge")+
  labs(title = "Month.of.absence")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),stat='count',position=position_dodge(0.5),vjust=-0.2)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Absenteeism in Month wise
month <- as.data.frame(absent %>% group_by(Month.of.absence) %>% summarise(count= n(),
                                                                           percent = round(count*100/nrow(absent),1))%>% arrange(desc(count)))

ggplot(month,aes(x= reorder(Month.of.absence,percent), y= percent, fill = month$Month.of.absence)) + geom_bar(stat='identity') + coord_flip() +
  geom_text(aes(label = percent), vjust = 1.1, hjust = 1.2) + xlab('Month')


#Day.of.the.week
ggplot(emp3,aes(emp3$Day.of.the.week))+geom_bar(aes(fill = emp3$Day.of.the.week),position = "dodge")+
  labs(title = "Day.of.the.week")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_dodge(0.5),vjust=-0.2)

#Absenteeism in day wise:
day <- as.data.frame(absent %>% group_by(Day.of.the.week) %>% summarise(count= n(),
                                                                        percent = round(count*100/nrow(absent),1))%>% arrange(desc(count)))

ggplot(day,aes(x= reorder(Day.of.the.week,percent), y= percent, fill = Day.of.the.week)) + geom_bar(stat='identity') + coord_flip() +
  geom_text(aes(label = percent), vjust = 1.1, hjust = 1.2) + xlab('Day')


#Seasons
ggplot(emp3,aes(emp3$Seasons))+geom_bar(aes(fill = emp3$Seasons),position = "dodge")+
  labs(title = "Seasons")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_dodge(0.5),vjust=-0.2)

#Abseentism in season wise
season1 <- as.data.frame(absent %>% group_by(Seasons) %>% summarise(count= n(),
                                                                    percent = round(count*100/nrow(absent),1))%>% arrange(desc(count)))

ggplot(season1,aes(x= reorder(Seasons,percent), y= percent, fill = Seasons)) + geom_bar(stat='identity') + coord_flip() +
  geom_text(aes(label = percent), vjust = 1.1, hjust = 1.2) + xlab('Seasons')


#Education
ggplot(emp3,aes(emp3$Education))+geom_bar(aes(fill = emp3$Education),position = "dodge")+
  labs(title = "Education")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_dodge(0.5),vjust=-0.2)

#Absenteeism in education wise:
edu <- as.data.frame(absent %>% group_by(Education) %>% summarise(count= n(),
                                                                  percent = round(count*100/nrow(absent),1))%>% arrange(desc(count)))

ggplot(edu,aes(x= reorder(Education,percent), y= percent, fill = Education)) + geom_bar(stat='identity') + coord_flip() +
  geom_text(aes(label = percent), vjust = 1.1, hjust = 1.2) + xlab('Day')


#Son
ggplot(emp3,aes(emp3$Son))+geom_bar(aes(fill = emp3$Son),position = "dodge")+
  labs(title = "No.of Children")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_dodge(0.5),vjust=-0.2)


#Social.drinker
ggplot(emp3,aes(emp3$Social.drinker))+geom_bar(aes(fill = emp3$Social.drinker),position = "dodge")+
  labs(title = "Social.drinker")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_dodge(0.5),vjust=-0.2)


#Social.smoker
ggplot(emp3,aes(emp3$Social.smoker))+geom_bar(aes(fill = emp3$Social.smoker),position = "dodge")+
  labs(title = "Social.smoker")+
  geom_text(aes(label=scales::percent(..count../sum(..count..))),
            stat='count',position=position_dodge(0.5),vjust=-0.2)


#_________________________________________________________________________________________________________#


#Numerical Variables:
numeric_names=names(emp3)[sapply(emp3, is.numeric)]
numerics=dplyr::select_if(emp3, is.numeric)

boxplot(numerics)

boxplots=for (i in 1:length(numeric_names))
{
  assign(paste0("cv",i), ggplot(aes_string(y = (numeric_names[i]), x = "Absenteeism.time.in.hours"), data = subset(emp3))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=2, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=numeric_names[i],x="Absenteeism.time.in.hours")+
           ggtitle(paste("Box plot of absenteeism for",numeric_names[i])))
}

gridExtra::grid.arrange(cv1,cv2,cv3,ncol = 3)
gridExtra::grid.arrange(cv4,cv5,cv6,ncol =3)
gridExtra::grid.arrange(cv7,cv8,cv9,ncol = 3)

#Only few outliers exist in the numerical variables. 
# Let's check the outlier in the variables 

#Transportation.expense
quantile(emp3$Transportation.expense,seq(0,1,0.01))
boxplot(emp3$Transportation.expense)
summary(emp3$Transportation.expense)
bench1=260+1.5*IQR(emp3$Transportation.expense)
emp3$Transportation.expense[emp3$Transportation.expense>bench1]=bench1
summary(emp3$Transportation.expense)


#Service.time
quantile(emp3$Service.time,seq(0,1,0.01))
boxplot(emp3$Service.time)
emp3$Service.time[emp3$Service.time>18]=18


#Age
quantile(emp3$Age,seq(0,1,0.01))
summary(emp3$Age)
bench2=40+1.5*IQR(emp3$Age)
bench2
emp3$Age[emp3$Age>bench2]=bench2
boxplot(emp3$Age)


#Work.load.Average.day
quantile(emp3$Work.load.Average.day,seq(0,1,0.01))
summary(emp3$Work.load.Average.day)
bench3=293631+1.5*IQR(emp3$Work.load.Average.day)
bench3
emp3$Work.load.Average.day[emp3$Work.load.Average.day>bench3]=bench3
boxplot(emp3$Work.load.Average.day)


#Hit.Target
quantile(emp3$Hit.target,seq(0,1,0.01))
summary(emp3$Hit.target)
bench4=93-1.5*IQR(emp3$Hit.target)
bench4
emp3$Hit.target[emp3$Hit.target<bench4]=bench4
boxplot(emp3$Hit.target)


#Height
quantile(emp3$Height,seq(0,1,0.01))
summary(emp3$Height)
bench5=172+1.5*IQR(emp3$Height)
bench5
emp3$Height[emp3$Height>bench5]=bench5
bench6=169-1.5*IQR(emp3$Height)
bench6
emp3$Height[emp3$Height<bench6]=bench6
boxplot(emp3$Height)

#Thus treated the outliers in numerical variables using winsorizing technique.
emp3 %>%keep(is.numeric) %>% gather() %>%ggplot(aes(value)) +facet_wrap(~ key, scales = "free") +geom_histogram()

boxplot(emp3$Absenteeism.time.in.hours)
#_________________________________________________________________________________________________________#


#Feature Selection:
#Looking at the presence of multicollinearity in data:
#Correlation:
corrgram(emp3[,],order=FALSE,upper.panel=panel.pie,text.panel=panel.txt,main="correlation plot")
corr_matrix=cor(numerics)
corrplot.mixed(corr_matrix, lower.col = "black", number.cex = .7)


#_________________________________________________________________________________________________________#


#Feature Scaling:
emp3 %>%keep(is.numeric) %>% gather() %>%ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +geom_histogram()+
  labs(title = "Distribution of Test Data")

#Doing Normalization
skew_var <- c("Transportation.expense","Distance.from.Residence.to.Work","Service.time",
                    "Age","Hit.target","Height","Body.mass.index","Absenteeism.time.in.hours","Weight")
for(i in skew_var)
{
  print(i)
  emp3[,i] = (emp3[,i] - min(emp3[,i]))/(max(emp3[,i])-min(emp3[,i]))
}

#_________________________________________________________________________________________________________#

#Dummy Variable for categorical variables:
library(fastDummies)
emp4=dummy_cols(emp3, select_columns = c("Reason.for.absence" ,"Month.of.absence" ,"Day.of.the.week","Seasons","Son","Education","Pet"),
                 remove_first_dummy = TRUE)

colnames(emp4)
emp4=emp4[, -c(1:5)]
emp4=emp4[,-c(8:9)]


emp4$Social.drinker=as.numeric(as.character(emp4$Social.drinker))
emp4$Social.smoker=as.numeric(as.character(emp4$Social.smoker))
emp4$Disciplinary.failure=as.numeric(as.character(emp4$Disciplinary.failure))

#Final Data:
final=emp4
colnames(final)
str(final)

#_________________________________________________________________________________________________________#


#Model Building:

model1=lm(Absenteeism.time.in.hours~.,data=final)
summary(model1)

#Iterations:
#stepAIC(model1)

model2=lm(Absenteeism.time.in.hours ~ Transportation.expense + 
            Distance.from.Residence.to.Work + Service.time + Hit.target + 
            Body.mass.index + Reason.for.absence_unknown + `Reason.for.absence_medical consultation` + 
            `Reason.for.absence_Injury, poisoning and certain other consequences of external causes` + 
            `Reason.for.absence_Diseases of the musculoskeletal system and connective tissue` + 
            `Reason.for.absence_dental consultation` + `Reason.for.absence_laboratory examination` + 
            `Reason.for.absence_Diseases of the nervous system` + `Reason.for.absence_Diseases of the skin and subcutaneous tissue` + 
            `Reason.for.absence_Diseases of the circulatory system` + 
            Reason.for.absence_Neoplasms + Month.of.absence_Jan + Month.of.absence_Feb + 
            Month.of.absence_May + Day.of.the.week_thurs + Day.of.the.week_fri + 
            Son_1 + Son_0 + Education_postgraduate + Education_graduate, 
          data = final)
summary(model2)

vif(model2)

#Removing Son_1
model3=lm(Absenteeism.time.in.hours ~ Transportation.expense + 
            Distance.from.Residence.to.Work + Service.time + Hit.target + 
            Body.mass.index + Reason.for.absence_unknown + `Reason.for.absence_medical consultation` + 
            `Reason.for.absence_Injury, poisoning and certain other consequences of external causes` + 
            `Reason.for.absence_Diseases of the musculoskeletal system and connective tissue` + 
            `Reason.for.absence_dental consultation` + `Reason.for.absence_laboratory examination` + 
            `Reason.for.absence_Diseases of the nervous system` + `Reason.for.absence_Diseases of the skin and subcutaneous tissue` + 
            `Reason.for.absence_Diseases of the circulatory system` + 
            Reason.for.absence_Neoplasms + Month.of.absence_Jan + Month.of.absence_Feb + 
            Month.of.absence_May + Day.of.the.week_thurs + Day.of.the.week_fri + 
            Son_0 + Education_postgraduate + Education_graduate, 
          data = final)
summary(model3)

vif(model3)

#Removing Son_0
model4=lm(Absenteeism.time.in.hours ~ Transportation.expense + 
            Distance.from.Residence.to.Work + Service.time + Hit.target + 
            Body.mass.index + Reason.for.absence_unknown + `Reason.for.absence_medical consultation` + 
            `Reason.for.absence_Injury, poisoning and certain other consequences of external causes` + 
            `Reason.for.absence_Diseases of the musculoskeletal system and connective tissue` + 
            `Reason.for.absence_dental consultation` + `Reason.for.absence_laboratory examination` + 
            `Reason.for.absence_Diseases of the nervous system` + `Reason.for.absence_Diseases of the skin and subcutaneous tissue` + 
            `Reason.for.absence_Diseases of the circulatory system` + 
            Reason.for.absence_Neoplasms + Month.of.absence_Jan + Month.of.absence_Feb + 
            Month.of.absence_May + Day.of.the.week_thurs + Day.of.the.week_fri + 
            Education_postgraduate + Education_graduate, 
          data = final)
summary(model4)

vif(model4)

#Removing graduate:
model5=lm(Absenteeism.time.in.hours ~ Transportation.expense + 
            Distance.from.Residence.to.Work + Service.time + Hit.target + 
            Body.mass.index + Reason.for.absence_unknown + `Reason.for.absence_medical consultation` + 
            `Reason.for.absence_Injury, poisoning and certain other consequences of external causes` + 
            `Reason.for.absence_Diseases of the musculoskeletal system and connective tissue` + 
            `Reason.for.absence_dental consultation` + `Reason.for.absence_laboratory examination` + 
            `Reason.for.absence_Diseases of the nervous system` + `Reason.for.absence_Diseases of the skin and subcutaneous tissue` + 
            `Reason.for.absence_Diseases of the circulatory system` + 
            Reason.for.absence_Neoplasms + Month.of.absence_Jan + Month.of.absence_Feb + 
            Month.of.absence_May + Day.of.the.week_thurs + Day.of.the.week_fri + 
            Education_postgraduate, 
          data = final)
summary(model5)

vif(model5)

#Removing week_fri
model6=lm(Absenteeism.time.in.hours ~ Transportation.expense + 
            Distance.from.Residence.to.Work + Service.time + Hit.target + 
            Body.mass.index + Reason.for.absence_unknown + `Reason.for.absence_medical consultation` + 
            `Reason.for.absence_Injury, poisoning and certain other consequences of external causes` + 
            `Reason.for.absence_Diseases of the musculoskeletal system and connective tissue` + 
            `Reason.for.absence_dental consultation` + `Reason.for.absence_laboratory examination` + 
            `Reason.for.absence_Diseases of the nervous system` + `Reason.for.absence_Diseases of the skin and subcutaneous tissue` + 
            `Reason.for.absence_Diseases of the circulatory system` + 
            Reason.for.absence_Neoplasms + Month.of.absence_Jan + Month.of.absence_Feb + 
            Month.of.absence_May + Day.of.the.week_thurs + Education_postgraduate, 
          data = final)
summary(model6)


#Removing Neoplasms
model7=lm(Absenteeism.time.in.hours ~ Transportation.expense + 
            Distance.from.Residence.to.Work + Service.time + Hit.target + 
            Body.mass.index + Reason.for.absence_unknown + `Reason.for.absence_medical consultation` + 
            `Reason.for.absence_Injury, poisoning and certain other consequences of external causes` + 
            `Reason.for.absence_Diseases of the musculoskeletal system and connective tissue` + 
            `Reason.for.absence_dental consultation` + `Reason.for.absence_laboratory examination` + 
            `Reason.for.absence_Diseases of the nervous system` + `Reason.for.absence_Diseases of the skin and subcutaneous tissue` + 
            `Reason.for.absence_Diseases of the circulatory system` + 
             Month.of.absence_Jan + Month.of.absence_Feb + 
            Month.of.absence_May + Day.of.the.week_thurs + Education_postgraduate, 
          data = final)
summary(model7)

vif(model7)

#Removing Hit.target
model8=lm(Absenteeism.time.in.hours ~ Transportation.expense + 
            Distance.from.Residence.to.Work + Service.time + 
            Body.mass.index + Reason.for.absence_unknown + `Reason.for.absence_medical consultation` + 
            `Reason.for.absence_Injury, poisoning and certain other consequences of external causes` + 
            `Reason.for.absence_Diseases of the musculoskeletal system and connective tissue` + 
            `Reason.for.absence_dental consultation` + `Reason.for.absence_laboratory examination` + 
            `Reason.for.absence_Diseases of the nervous system` + `Reason.for.absence_Diseases of the skin and subcutaneous tissue` + 
            `Reason.for.absence_Diseases of the circulatory system` + 
            Month.of.absence_Jan + Month.of.absence_Feb + 
            Month.of.absence_May + Day.of.the.week_thurs + Education_postgraduate, 
          data = final)
summary(model8)

#Removing May
model9=lm(Absenteeism.time.in.hours ~ Transportation.expense + 
            Distance.from.Residence.to.Work + Service.time + 
            Body.mass.index + Reason.for.absence_unknown + `Reason.for.absence_medical consultation` + 
            `Reason.for.absence_Injury, poisoning and certain other consequences of external causes` + 
            `Reason.for.absence_Diseases of the musculoskeletal system and connective tissue` + 
            `Reason.for.absence_dental consultation` + `Reason.for.absence_laboratory examination` + 
            `Reason.for.absence_Diseases of the nervous system` + `Reason.for.absence_Diseases of the skin and subcutaneous tissue` + 
            `Reason.for.absence_Diseases of the circulatory system` + 
            Month.of.absence_Jan + Month.of.absence_Feb + 
            Day.of.the.week_thurs + Education_postgraduate, 
          data = final)
summary(model9)

#Removing Feb
model10=lm(Absenteeism.time.in.hours ~ Transportation.expense + 
            Distance.from.Residence.to.Work + Service.time + 
            Body.mass.index + Reason.for.absence_unknown + `Reason.for.absence_medical consultation` + 
            `Reason.for.absence_Injury, poisoning and certain other consequences of external causes` + 
            `Reason.for.absence_Diseases of the musculoskeletal system and connective tissue` + 
            `Reason.for.absence_dental consultation` + `Reason.for.absence_laboratory examination` + 
            `Reason.for.absence_Diseases of the nervous system` + `Reason.for.absence_Diseases of the skin and subcutaneous tissue` + 
            `Reason.for.absence_Diseases of the circulatory system` + 
            Month.of.absence_Jan +  Day.of.the.week_thurs + Education_postgraduate, 
          data = final)
summary(model10)

vif(model10)

#Removing laboratory examination
model11=lm(Absenteeism.time.in.hours ~ Transportation.expense + 
             Distance.from.Residence.to.Work + Service.time + 
             Body.mass.index + Reason.for.absence_unknown + `Reason.for.absence_medical consultation` + 
             `Reason.for.absence_Injury, poisoning and certain other consequences of external causes` + 
             `Reason.for.absence_Diseases of the musculoskeletal system and connective tissue` + 
             `Reason.for.absence_dental consultation` +  
             `Reason.for.absence_Diseases of the nervous system` + `Reason.for.absence_Diseases of the skin and subcutaneous tissue` + 
             `Reason.for.absence_Diseases of the circulatory system` + 
             Month.of.absence_Jan +  Day.of.the.week_thurs + Education_postgraduate, 
           data = final)
summary(model11)

#Removing Jan
model12=lm(Absenteeism.time.in.hours ~ Transportation.expense + 
             Distance.from.Residence.to.Work + Service.time + 
             Body.mass.index + Reason.for.absence_unknown + `Reason.for.absence_medical consultation` + 
             `Reason.for.absence_Injury, poisoning and certain other consequences of external causes` + 
             `Reason.for.absence_Diseases of the musculoskeletal system and connective tissue` + 
             `Reason.for.absence_dental consultation` +  
             `Reason.for.absence_Diseases of the nervous system` + `Reason.for.absence_Diseases of the skin and subcutaneous tissue` + 
             `Reason.for.absence_Diseases of the circulatory system` + 
              Day.of.the.week_thurs + Education_postgraduate, 
           data = final)
summary(model12)

#Thus the variables in model12 are highly significant or some what significant. So, finalising model12.
#_________________________________________________________________________________________________________#


#Extracting the most important features from the model result to serve the business needs.
model12$coefficients


important_features=data.frame(varImp(model12))
important_features$features=row.names(important_features)
row.names(important_features)=NULL

important_features=important_features[order(-important_features$Overall),]

#_________________________________________________________________________________________________________#
#_________________________________________________________________________________________________________#

#Time Series model to predict for next year:
#install.packages("tseries")
library(tseries)
#TIME SERIES MODEL:
#Converting Month.of.absence to factor
str(emp1)
emp1$`Month of absence` = as.factor(emp1$`Month of absence`)
#Making a timeseries aggregating Absenteeism.time.in.hours by Month.of.absence
monthly_absence = aggregate(emp1$`Absenteeism time in hours`,by=list(Category=emp1$`Month of absence`),FUN=sum)
monthly_absence = monthly_absence[2:13,]
monthly_absence

#Calculating absenteeism_dataeeism time as percent of total time in column absenteeism_datahours
monthly_absence$absenteeism_datahours = monthly_absence$x/3
row.names(monthly_absence) = monthly_absence$Category
monthly_absence

# Modelling time series using arima
tsdata = ts(monthly_absence$absenteeism_datahours)
class(tsdata)
tsdata

#Visualizing timeseries
plot(tsdata)

#Checking stationarity - Augmented Dickey-Fuller Test
library(tseries)
adf.test(tsdata, alternative="stationary", k=0)

tsdata2 = tsdata - stats::lag((tsdata),1)
plot(tsdata2)

#Doing Augmented Dickey-Fuller Test again
adf.test(tsdata2, alternative="stationary", k=0)

#ACF plot
acf(tsdata2)
#value of p should be 0.
#PACF plot
pacf(tsdata2)
#value of q should be 0.
library(forecast)
model = arima(tsdata2,c(4,0,9))
fit1 = fitted(model)
residuals1 = tsdata2 - fit1
sum(residuals1**2)

#Arima with order=(4,0,9) gives us lowest Residual sum of squares. so we will finalise p,d,q =(4,0,9)

plot(tsdata2)
lines(fit1)
absenteeism_data2011 = predict(model,n.ahead = 12)

#Scaling absenteeism_data2011 back to original
absent_2011 = cumsum(absenteeism_data2011$pred)
absent_2011 = absent_2011 + rep(tsdata[4],12)
as.data.frame(absent_2011)
ts_2011 = ts(absent_2011)
df1 = as.data.frame(absent_2011)
row.names(df1) = c(13:24)
ts_2011 = ts(df1$absent_2011,start=13)

#Plotting original timeseries & forecast values

plot(tsdata,xlim=c(1,24))
lines(ts_2011)

ts_2011
#_________________________________________________________________________________________________________#
#_________________________________________________________________________________________________________#