## Part 1 250 pantients lab records
## we have collected the 250 patients lab record contained in the file 'longegfr1.csv' and 'longegfr2.csv'.
## For each subject, the eGFR(estimated glomerular filtration rate, a measure of kindey function ) 
##was collected at irregularly spaced time points: variable "fu.years" contains the follow-up time 
## that is, the distance from baseline to the date when each eGFR measurement was taken, expressed in years).
## id means the patient id
## sex 0 means female and 1 means male

## As we have the information from the two data tables therefore as the first step 
## we will need to read the data file and merge the data table together


library(data.table)
library(dplyr)
longegfr1.dt<-fread('C:/Users/44772/Documents/Rproject/project/data/longegfr1.csv',stringsAsFactors=TRUE)
longegfr2.dt<-fread('C:/Users/44772/Documents/Rproject/project/data/longegfr2.csv',stringsAsFactors=TRUE)
## we firstly load two datesets and save them as the data tables then we will start merging two datatables together. 
## As we can see from the two data tables, both of them contains ID and fu.years features.
## The join method I used here is the full join and the condition we applied here is ID and fu.years.plus we will need to 
## rename the one of the id in the data table which could help us to use that condition
names(longegfr2.dt)[1]<-"id"
longegfrnew.dt <-full_join(longegfr1.dt, longegfr2.dt, by=c("id"="id","fu.years"="fu.years"))
## As we can see there are onlu 3819 rows in the longegfr.dt as a condequence 
## we will need to omit the n/a rows after the joining function
longegfrnew_complete.dt<-na.omit(longegfrnew.dt)
## As we collect the data of eGFR in the corresponding follow-up time in order to check the average eGFR status for each partients during 
## the recording period ie. the max time collected for each person is the total time of the monitoring period
length_follow_up.dt<-aggregate(x=longegfrnew.dt $fu.years,by=list(longegfrnew.dt $id),FUN=max)
length_follow_up.dt<-as.data.table(length_follow_up.dt)
names(length_follow_up.dt)<-c("id","measurements")
## we could calculate the average value of the eGFR and allocated with the corresponding patirnt ID and allocate them in the
## the several intervals in 0,15,30,60,90 to max


mean_eGFR.dt<-aggregate(x=longegfrnew.dt$egfr,by=list(longegfrnew.dt$id),FUN=mean,na.rm=TRUE)
mean_eGFR.dt<-as.data.table(mean_eGFR.dt)
names(mean_eGFR.dt)<-c("id","mean_eg")
eGFR_groupby<-table(cut(mean_eGFR.dt$mean ,breaks=c(0,15,30,60,90,max(longegfrnew.dt$egfr,na.rm = TRUE))))
eGFR_groupby
## when eGFR is greater than 90 indicates that the patients kidney function will be normal and there is 66 patients 
## being recovered and showed that the eGFR for the majority of the patients are located at the 30-90 which means they are still neeed to be monitored and do the further
## because when the eGFR is below 60 we need to be cautious it may be caused by kidney disease.

mean_eGFR.dt$mean %>% 
  is.na() %>% 
  table()
## there are three missing values in our table/
## we have collected the patient data whose eGFR is greater 90 and merge them with the information table.
mean_eGFR90.dt<-mean_eGFR.dt[which(mean_eGFR.dt$mean_eg>90)]
information_eGFR90.dt<-merge(length_follow_up.dt,mean_eGFR90.dt,by="id")
names(information_eGFR90.dt)<-c("id","fu.years","average_eGFR")
freq_measurement.dt<-count(longegfrnew.dt,"id")
freq_measurement.dt<-as.data.table(freq_measurement.dt)
names(freq_measurement.dt)[1]="id"
information_eGFR90.dt<-merge(x=information_eGFR90.dt,y=freq_measurement.dt,by="id")
information_eGFR90.dt<-merge(x=information_eGFR90.dt,y=longegfrnew.dt ,by=c("id","fu.years"))
names(information_eGFR90.dt)[2]<-"last_measurment_time"
names(information_eGFR90.dt)[4]<-"measurements"
information_eGFR90.dt<-information_eGFR90.dt[,-7]
information_eGFR90.dt<-information_eGFR90.dt[,c(1,5,6,3,2,4)]
##
patient3.dt<-longegfrnew.dt[which(longegfrnew.dt$id==3)] 
patient3.dt<-patient3.dt[,-c(1:3)]
plot(patient3.dt,xlab="time",ylab="eGFR_measurement",main="patient3")
patient3_x<-patient3.dt$fu.years
patient3_y<-patient3.dt$egfr
regr_patient3 <- lm(patient3_y~patient3_x)
regr_patient3$coefficients
abline(regr_patient3 ,col="red")
confint(regr_patient3)
patient3_new_x.dt<-patient3.dt[which(patient3.dt$egfr>min(patient3.dt$egfr)&patient3.dt$egfr<max(patient3.dt$egfr))]
patient3_new_x<-patient3_new_x.dt$fu.years
patient3_new_y<-patient3_new_x.dt$egfr
regr_patient3_new<-lm(patient3_new_y~patient3_new_x)
abline(regr_patient3_new,col="black")
regr_patient3_new$coefficients
class
##
patient3.dt<-longegfrnew.dt[which(longegfrnew.dt$id==37)] 
patient3.dt<-patient3.dt[,-c(1:3)]
plot(patient3.dt,xlab="time",ylab="eGFR_measurement",main="patient3")
patient3_x<-patient3.dt$fu.years
patient3_y<-patient3.dt$egfr
regr_patient3 <- lm(patient3_y~patient3_x)
regr_patient3$coefficients
abline(regr_patient3 ,col="red")
confint(regr_patient3)
patient3_new_x.dt<-patient3.dt[which(patient3.dt$egfr>min(patient3.dt$egfr)&patient3.dt$egfr<max(patient3.dt$egfr))]
patient3_new_x<-patient3_new_x.dt$fu.years
patient3_new_y<-patient3_new_x.dt$egfr
regr_patient3_new<-lm(patient3_new_y~patient3_new_x)
abline(regr_patient3_new,col="black")
regr_patient3_new$coefficients
class
##
patient37.dt<-longegfrnew.dt[which(longegfrnew.dt$id==162)]
patient37.dt<-patient37.dt[,-c(1:3)]
plot(patient37.dt,xlab="time",ylab="eGFR_measurement",main="patient37")
patient37_x<-patient37.dt$fu.years
patient37_y<-patient37.dt$egfr
regr_patient37 <- lm(patient37_y~patient37_x)
regr_patient37$coefficients
abline(regr_patient37 ,col="red")
confint(regr_patient37)
patient37_new_x.dt<-patient37.dt[which(patient37.dt$egfr>min(patient37.dt$egfr)&patient37.dt$egfr<max(patient37.dt$egfr))]
patient37_new_x<-patient37_new_x.dt$fu.years
patient37_new_y<-patient37_new_x.dt$egfr
regr_patient37_new<-lm(patient37_new_y~patient37_new_x)
abline(regr_patient37_new,col="black")
regr_patient37_new$coefficients
class(patient37.dt)
##
patient223.dt<-longegfrnew.dt[which(longegfrnew.dt$id==223)]
patient223.dt<-patient223.dt[,-c(1:3)]
patient223.dt<-na.omit(patient223.dt)
plot(patient223.dt,xlab="time",ylab="eGFR_measurement",main="patient223")
patient223_x<-patient223.dt$fu.years
patient223_y<-patient223.dt$egfr
regr_patient223 <- lm(patient223_y~patient223_x)
regr_patient223$coefficients
abline(regr_patient223 ,col="red")
confint(regr_patient223)
patient223_new_x.dt<-patient223.dt[which(patient223.dt$egfr>min(patient223.dt$egfr)&patient223.dt$egfr<max(patient223.dt$egfr))]
patient223_new_x<-patient223_new_x.dt$fu.years
patient223_new_y<-patient223_new_x.dt$egfr
regr_patient223_new<-lm(patient223_new_y~patient223_new_x)
abline(regr_patient223_new,col="black")
regr_patient223_new$coefficients
class(patient223.dt)
