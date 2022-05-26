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
## As we collect the data of eGFR in the corresponding follow-up time in order to check the average eGFR staus during 
the 
length_follow_up.dt<-aggregate(x=longegfrnew.dt $fu.years,by=list(longegfrnew.dt $id),FUN=max)
length_follow_up.dt<-as.data.table(length_follow_up.dt)
names(length_follow_up.dt)<-c("id","measurements")

