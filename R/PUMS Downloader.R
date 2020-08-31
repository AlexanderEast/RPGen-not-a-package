# PUMS Download

rm(list=ls())
getwd()


source("./tests/contents.R")
source("./tests/report card.R")
source("./tests/pool test.R")
library(data.table)
library(tidyverse)
library(plyr)
library(dplyr)






#pumshousing  = "https://www2.census.gov/programs-surveys/acs/data/pums/2018/5-Year/csv_hus.zip"

#PUMSNAME<-"PUMS_Houses_RPGen.zip"
#pums_housing_variables<-toupper(c("adjinc", "hincp","np","region","serialno","puma","st","bld",
#                                  "wgtp","veh","type"))


#download(pumshousing, dest = PUMSNAME, mode = "wb")
#unzip(PUMSNAME, files = c("psam_husa.csv","psam_husb.csv","psam_husc.csv","psam_husd.csv"))


#pumsa<-fread("psam_husa.csv", encoding = "UTF-8", header=TRUE, select = pums_housing_variables)
#pumsb<-fread("psam_husb.csv", encoding = "UTF-8", header=TRUE, select = pums_housing_variables)
#pumsc<-fread("psam_husc.csv", encoding = "UTF-8", header=TRUE, select = pums_housing_variables)
#pumsd<-fread("psam_husd.csv", encoding = "UTF-8", header=TRUE, select = pums_housing_variables)
#pumsh<-rbind(pumsa,pumsb,pumsc,pumsd)

#rm(pumsa,pumsb,pumsc,pumsd)
#file.remove(c("psam_husa.csv","psam_husb.csv","psam_husc.csv","psam_husd.csv","PUMS_Houses_RPGen.zip"))
#colnames(pumsh)<-tolower(colnames(pumsh))



pumsh<-fread("pumshousing.csv")
pumsh<-pumsh[pumsh$type == 1 & pumsh$np > 0 & pumsh$region < 5]
pumsh<-pumsh[,!("type"), with = FALSE]




#pumspeople ="https://www2.census.gov/programs-surveys/acs/data/pums/2018/5-Year/csv_pus.zip"




#PUMSPNAME<-"PUMS_People_RPGen.zip"
#pums_people_variables<-toupper(c("agep","sex","serialno","pwgtp","rac1p","hisp","jwmnp"))
#download(pumspeople,dest =PUMSPNAME, mode = "wb")
#unzip(PUMSPNAME, files = c("psam_pusa.csv","psam_pusb.csv","psam_pusc.csv","psam_pusd.csv"))
#pumspa<-fread("psam_pusa.csv", encoding = "UTF-8", header=TRUE, select = pums_people_variables)
#pumspb<-fread("psam_pusb.csv", encoding = "UTF-8", header=TRUE, select = pums_people_variables)
#pumspc<-fread("psam_pusc.csv", encoding = "UTF-8", header=TRUE, select = pums_people_variables)
#pumspd<-fread("psam_pusd.csv", encoding = "UTF-8", header=TRUE, select = pums_people_variables)
#pumsp<-rbind(pumspa,pumspb,pumspc,pumspd)
#rm(pumspa,pumspb,pumspc,pumspd)
#colnames(pumsp)<-tolower(colnames(pumsp))
#file.remove(c("psam_pusa.csv","psam_pusb.csv","psam_pusc.csv","psam_pusd.csv, PUMS_Houses_RPGen.zip"))


#pumsh<-fread("pumshousing.csv")
#pums<-merge(pumsh,pumsp,by = "serialno")
#rm(pumsh,pumsp)
#pums<- pums[order(pums$serialno)]

#fwrite(pums, "pums.csv")


rm(list=ls())

pums<-fread("pums.csv")[1:5000,]