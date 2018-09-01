{\rtf1\ansi\ansicpg1252\cocoartf1561\cocoasubrtf400
{\fonttbl\f0\fswiss\fcharset0 ArialMT;\f1\froman\fcharset0 Times-Roman;}
{\colortbl;\red255\green255\blue255;\red190\green0\blue4;\red0\green0\blue0;\red16\green60\blue192;
\red251\green0\blue7;\red45\green101\blue22;}
{\*\expandedcolortbl;;\cssrgb\c80000\c0\c0;\cssrgb\c0\c0\c0;\cssrgb\c6667\c33333\c80000;
\cssrgb\c100000\c0\c0;\cssrgb\c21961\c46275\c11373;}
\paperw11900\paperh16840\margl1440\margr1440\vieww10800\viewh8400\viewkind0
\deftab720
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \expnd0\expndtw0\kerning0
\outl0\strokewidth0 \strokec2 library(stringi)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 library(stringr)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 library(dplyr)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 library(tidyr)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 library(e1071)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata <- Crime_Data_2010_2017
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Crime Code 1` <- NULL
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 View(crimedata)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Crime Code 2` <- NULL
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Crime Code 3` <- NULL
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Crime Code 4` <- NULL
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Cross Street` <- NULL
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$Address <- NULL
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Reporting District` <- NULL
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`DR Number` <- NULL
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 LACrime_latlong = gsub('\\\\(','',crimedata$Location)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 LACrime_latlong = gsub('\\\\)','',LACrime_latlong)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 LACrime_latlong = strsplit(LACrime_latlong,",")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 LACrime_latlong = do.call(rbind.data.frame, LACrime_latlong)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 colnames(LACrime_latlong) = c("latitude","longitude")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$latitude \'a0= as.numeric(as.character(LACrime_latlong$latitude))
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$longitude = as.numeric(as.character(LACrime_latlong$longitude))
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 rm(LACrime_latlong)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$Location = NULL
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 PremiseCode<- unique(crimedata[c("Premise Code", "Premise Description")])
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 PremiseCode[is.na(PremiseCode$`Premise Description`),"Premise Description"] <-"UNK"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 write.csv(PremiseCode, "Premise_Desc.csv")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 CrimeCodeDesc <- unique(crimedata[c("Crime Code", "Crime Code Description")])
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 CrimeCodeDesc[is.na(CrimeCodeDesc$`Crime Code Description`),"Crime Code Description"] <- "UNK"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 write.csv(CrimeCodeDesc, "Crime_Code_Desc.csv")
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Crime Code Description` = NULL
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Premise Description` = NULL
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 AreaName <- unique(crimedata[c("Area ID", "Area Name")])
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 AreaName[is.na(AreaName$`Area Name`),"Area Name"] <- "UNK"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 write.csv(AreaName, "Area_Name.csv")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 WeaponDescription <- unique(crimedata[c("Weapon Used Code", "Weapon Description")])
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 WeaponDescription[is.na(WeaponDescription$`Weapon Description`),"Weapon Description"] <- "UNK"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 write.csv(WeaponDescription, "Weapon_Description.csv")
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Weapon Description` = NULL
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Area Name` = NULL
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`MO Codes` = NULL
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 StatusDescription <- unique(crimedata[c("Status Code", "Status Description")])
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 StatusDescription[is.na(StatusDescription$`Status Description`),"Status Description"] <- "UNK"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 write.csv(StatusDescription, "Status_Description.csv")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Status Description` = NULL
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata <- crimedata[!is.na(crimedata$`latitude`),]
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata <- crimedata[!is.na(crimedata$`Status Code`),]
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 weapondata <- crimedata[!is.na(crimedata$`Weapon Used Code`),]
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 write.csv(weapondata, "weapondata.csv")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Weapon Used Code` = NULL
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 table(is.na(crimedata$`Victim Sex`))
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 table(is.na(crimedata$`Victim Age`))
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata<-separate(crimedata,"Date Reported",c("DRYear","DRMonth","DRDay"),sep="-");
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata<-separate(crimedata,"Date Occurred",c("DOMonth","DODay","DOYear"),sep="/");
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 #Clustering attributes values
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Time Occurred` < 2400),"Time Period"] <- "4"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Time Occurred`< 1800),"Time Period"] <- "3"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Time Occurred`< 1200),"Time Period"] <- "2"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Time Occurred` < 600),"Time Period"] <- "1"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Time Occurred` <- NULL
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Victim Descent`== "A"|crimedata$`Victim Descent`== "C"|crimedata$`Victim Descent`== "D"|crimedata$`Victim Descent`== "F"|crimedata$`Victim Descent`== "J"|crimedata$`Victim Descent`== "K"|crimedata$`Victim Descent`== "L"|crimedata$`Victim Descent`== "V"|crimedata$`Victim Descent`== "Z"),"Victim Descent"] <- "1"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Victim Descent`== "B"),"Victim Descent"] <- "2"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Victim Descent`== "O"|crimedata$`Victim Descent`== "X"),"Victim Descent"] <- "7"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Victim Descent`== "H"),"Victim Descent"] <- "4"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Victim Descent`== "W"),"Victim Descent"] <- "5"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Victim Descent`== "I"),"Victim Descent"] <- "6"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Victim Descent`== "G"|crimedata$`Victim Descent`== "P"|crimedata$`Victim Descent`== "S"|crimedata$`Victim Descent`== "U"),"Victim Descent"] <- "3"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Victim Descent` <- as.integer(crimedata$`Victim Descent`)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Victim Sex`== "M"),"Victim Sex"] <- "1"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Victim Sex`== "F"),"Victim Sex"] <- "2"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Victim Sex`== "H"),"Victim Sex"] <- "3"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Victim Sex`== "X"),"Victim Sex"] <- "4"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Victim Sex` <- as.integer(crimedata$`Victim Sex`)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Victim Age` < 100),"Victim Age1"] <- "4"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Victim Age` < 60),"Victim Age1"] <- "3"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Victim Age`< 36),"Victim Age1"] <- "2"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Victim Age` < 19),"Victim Age1"] <- "1"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Victim Age` <- NULL
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 colnames(crimedata)[16] <- "Victim Age"\cf4 \strokec4  # Renaming column name
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Victim Age` <- as.integer(crimedata$`Victim Age`)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata <- crimedata[!(crimedata$`Status Code` == "13"),]
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata <- crimedata[!(crimedata$`Status Code` == "19"),]
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata <- crimedata[!(crimedata$`Status Code` == "TH"),]
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Status Code`== "AA"),"Status Code"] <- "1"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Status Code`== "AO"),"Status Code"] <- "2"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Status Code`== "CC"),"Status Code"] <- "3"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Status Code`== "IC"),"Status Code"] <- "4"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Status Code`== "JA"),"Status Code"] <- "5"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Status Code`== "JO"),"Status Code"] <- "6"
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 # Clustering crime dataset
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Crime Code`== "760"| crimedata$`Crime Code` == "814"|crimedata$`Crime Code` == "822"|crimedata$`Crime Code` == "921"),"Crime Code"] <- "0"
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Crime Code`== "110"| crimedata$`Crime Code`== "113"| crimedata$`Crime Code`== "230"| crimedata$`Crime Code`== "231"| crimedata$`Crime Code`== "250"| crimedata$`Crime Code`== "251"| crimedata$`Crime Code`== "622"| crimedata$`Crime Code`== "623"| crimedata$`Crime Code`== "624"| crimedata$`Crime Code`== "625"| crimedata$`Crime Code`== "626"| crimedata$`Crime Code`== "753"| crimedata$`Crime Code`== "910"| crimedata$`Crime Code`== "920"),"Crime Code"] <- "1"
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Crime Code`== "121"|crimedata$`Crime Code`== "122"|crimedata$`Crime Code`== "236"|crimedata$`Crime Code`== "815"|crimedata$`Crime Code`== "820"|crimedata$`Crime Code`== "821"|crimedata$`Crime Code`== "840"|crimedata$`Crime Code`== "860"|crimedata$`Crime Code`== "762"|crimedata$`Crime Code`== "805"|crimedata$`Crime Code`== "806"|crimedata$`Crime Code`== "810"|crimedata$`Crime Code`== "830"|crimedata$`Crime Code`== "850"|crimedata$`Crime Code`== "956"),"Crime Code"] <- "2"
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Crime Code`== "210"|crimedata$`Crime Code`== "220"|crimedata$`Crime Code`== "310"|crimedata$`Crime Code`== "320"|crimedata$`Crime Code`== "330"|crimedata$`Crime Code`== "331"|crimedata$`Crime Code`== "341"|crimedata$`Crime Code`== "343"|crimedata$`Crime Code`== "350"|crimedata$`Crime Code`== "351"|crimedata$`Crime Code`== "352"|crimedata$`Crime Code`== "353"|crimedata$`Crime Code`== "410"|crimedata$`Crime Code`== "420"|crimedata$`Crime Code`== "421"|crimedata$`Crime Code`== "432"|crimedata$`Crime Code`== "433"|crimedata$`Crime Code`== "440"|crimedata$`Crime Code`== "441"|crimedata$`Crime Code`== "442"|crimedata$`Crime Code`== "443"|crimedata$`Crime Code`== "444"|crimedata$`Crime Code`== "445"|crimedata$`Crime Code`== "446"|crimedata$`Crime Code`== "450"|crimedata$`Crime Code`== "451"|crimedata$`Crime Code`== "452"|crimedata$`Crime Code`== "473"|crimedata$`Crime Code`== "474"|crimedata$`Crime Code`== "475"|crimedata$`Crime Code`== "470"|crimedata$`Crime Code`== "471"|crimedata$`Crime Code`== "472"|crimedata$`Crime Code`== "662"|crimedata$`Crime Code`== "664"|crimedata$`Crime Code`== "666"|crimedata$`Crime Code`== "668"|crimedata$`Crime Code`== "670"|crimedata$`Crime Code`== "345"|crimedata$`Crime Code`== "347"|crimedata$`Crime Code`== "349"|crimedata$`Crime Code`== "354"|crimedata$`Crime Code`== "480"|crimedata$`Crime Code`== "485"|crimedata$`Crime Code`== "487"|crimedata$`Crime Code`== "510"|crimedata$`Crime Code`== "520"|crimedata$`Crime Code`== "950"|crimedata$`Crime Code`== "951"),"Crime Code" ] <- "3"
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Crime Code`== "235"|crimedata$`Crime Code`== "237"|crimedata$`Crime Code`== "627"|crimedata$`Crime Code`== "812"|crimedata$`Crime Code`== "813"|crimedata$`Crime Code`== "865"|crimedata$`Crime Code`== "870"|crimedata$`Crime Code`== "922"),"Crime Code"] <- "4"
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Crime Code`== "435"|crimedata$`Crime Code`== "436"|crimedata$`Crime Code`== "648"|crimedata$`Crime Code`== "740"|crimedata$`Crime Code`== "745"|crimedata$`Crime Code`== "880"|crimedata$`Crime Code`== "882"|crimedata$`Crime Code`== "884"|crimedata$`Crime Code`== "886"|crimedata$`Crime Code`== "926"|crimedata$`Crime Code`== "755"|crimedata$`Crime Code`== "756"),"Crime Code"] <- "5"
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Crime Code`== "434"|crimedata$`Crime Code`== "437"|crimedata$`Crime Code`== "438"|crimedata$`Crime Code`== "439"|crimedata$`Crime Code`== "453"|crimedata$`Crime Code`== "888"|crimedata$`Crime Code`== "890"|crimedata$`Crime Code`== "900"|crimedata$`Crime Code`== "901"|crimedata$`Crime Code`== "902"|crimedata$`Crime Code`== "903"|crimedata$`Crime Code`== "924"|crimedata$`Crime Code`== "931"|crimedata$`Crime Code`== "932"|crimedata$`Crime Code`== "942"|crimedata$`Crime Code`== "943"|crimedata$`Crime Code`== "944"|crimedata$`Crime Code`== "946"|crimedata$`Crime Code`== "948"|crimedata$`Crime Code`== "949"|crimedata$`Crime Code`== "952"|crimedata$`Crime Code`== "954"),"Crime Code"] <- "6"
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Crime Code`== "649"|crimedata$`Crime Code`== "651"|crimedata$`Crime Code`== "652"|crimedata$`Crime Code`== "653"|crimedata$`Crime Code`== "654"|crimedata$`Crime Code`== "660"|crimedata$`Crime Code`== "661"),"Crime Code"] <- "7"
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Crime Code`== "647"|crimedata$`Crime Code`== "761"|crimedata$`Crime Code`== "763"|crimedata$`Crime Code`== "928"|crimedata$`Crime Code`== "930"|crimedata$`Crime Code`== "940"|crimedata$`Crime Code`== "933"),"Crime Code"] <- "8"
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 #Converting numeric, string, etc. to factor in R
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 sapply(crimedata, class)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$DRYear<- as.factor(crimedata$DRYear)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$DRMonth<- as.factor(crimedata$DRMonth)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$DRDay<- as.factor(crimedata$DRDay)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$DOYear<- as.factor(crimedata$DOYear)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$DOMonth<- as.factor(crimedata$DOMonth)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$DODay<- as.factor(crimedata$DODay)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Area ID`<- as.factor(crimedata$`Area ID`)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Crime Code`<- as.factor(crimedata$`Crime Code`)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Victim Sex`<- as.factor(crimedata$`Victim Sex`)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Victim Descent`<- as.factor(crimedata$`Victim Descent`)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Premise Code`<- as.factor(crimedata$`Premise Code`)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Status Code`<- as.factor(crimedata$`Status Code`)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$latitude<- as.factor(crimedata$latitude)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$longitude<- as.factor(crimedata$longitude)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Time Period`<- as.factor(crimedata$`Time Period`)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata$`Victim Age`<- as.factor(crimedata$`Victim Age`)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 #Preparing Test And Train data for predicting missing attributes & Filling Missing # Values Using Naive Bayes
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimSextrain <- crimedata[!(is.na(crimedata$`Victim Sex`)),]
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimSextrain <- VictimSextrain[!(VictimSextrain$`Victim Sex` == "-"),]
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimSextest <- crimedata[is.na(crimedata$`Victim Sex`),]
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 #predicting Victim Sex with 62% accuracy
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 model <- naiveBayes(VictimSextrain$`Victim Sex` ~ ., data = VictimSextrain)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 preds <- predict(model, newdata = VictimSextest)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimSextest$`Victim Sex` <- preds
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 total <- rbind(VictimSextrain, VictimSextest)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimAgetrain <- total[!(is.na(total$`Victim Age`)),]
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimAgetest <- total[is.na(total$`Victim Age`),]
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 #predicting Victim Age with 52% accuracy
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 model <- naiveBayes(VictimAgetrain$`Victim Age` ~ ., data = VictimAgetrain)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 preds <- predict(model, newdata = VictimAgetest)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimAgetest$`Victim Age` <- preds
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 total <- rbind(VictimAgetrain, VictimAgetest)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimDescenttrain <- total[!(is.na(total$`Victim Descent`)),]
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimDescenttrain <- VictimDescenttrain[!(VictimDescenttrain$`Victim Descent` == "-"),]
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimDescenttest <- total[is.na(crimedata$`Victim Descent`),]
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 #predicting Victim Descent with 51.4% accuracy
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 model <- naiveBayes(VictimDescenttrain$`Victim Descent` ~ ., data = VictimDescenttrain)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 preds <- predict(model, newdata = VictimDescenttest)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimDescenttest$`Victim Descent` <- preds
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata <- rbind(VictimDescenttrain, VictimDescenttest)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 Premisetrain <- crimedata[!(is.na(crimedata$`Premise Code`)),]
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 Premisetest <- crimedata[is.na(crimedata$`Premise Code`),]
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 #predicting Premise Code with 52% accuracy
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 model <- naiveBayes(Premisetrain$`Premise Code` ~ ., data = Premisetrain)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 preds <- predict(model, newdata = Premisetest)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 Premisetest$`Premise Code` <- preds
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata <- rbind(Premisetrain, Premisetest)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 # Writing train and test data to csv
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 write.csv(crimedata, "crimedatanew.csv")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 # write.csv(VictimSextrain, "VictimSextrain.csv")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 # write.csv(VictimSextest, "VictimSextest.csv")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 # write.csv(VictimAgetrain, "VictimAgetrain.csv")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 # write.csv(VictimAgetest, "VictimAgetest.csv")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 # write.csv(VictimDescenttrain, "VictimDescentTrain.csv")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 # write.csv(VictimDescenttest, "VictimDescentTest.csv")
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 # ONE HOT ENCODING ON OBTAINED CRIME DATASET
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 # ON VICTIM SEX
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Victim Sex`== 1),"Victim Sex"] <- "M"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Victim Sex`== 2),"Victim Sex"] <- "F"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Victim Sex`== 3),"Victim Sex"] <- "H"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata[which(crimedata$`Victim Sex`== 4),"Victim Sex"] <- "X"
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimSex <- crimedata$`Victim Sex`
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimSex <- as.data.frame(VictimSex)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 library(dummies)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimSex<- dummy.data.frame(VictimSex, names =
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0	c("VictimSex"),sep="=")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 write.csv(VictimSex, "victimsex.csv")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata <- data.frame(crimedata, VictimSex)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 # ON STATUS CODE
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 StatusCode <- crimedata$Status.Code
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 StatusCode <- as.data.frame(StatusCode)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 library(dummies)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 StatusCode<- dummy.data.frame(StatusCode, names =
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0	c("StatusCode"),sep="=")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 write.csv(StatusCode, "statuscode.csv")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata <- data.frame(crimedata, StatusCode)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 # ON VICTIM DESCENT
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimDescent <- crimedata$Victim.Descent
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimDescent <- as.data.frame(VictimDescent)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 library(dummies)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimDescent<- dummy.data.frame(VictimDescent, names =
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0	c("VictimDescent"),sep="=")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata <- data.frame(crimedata, VictimDescent)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 # ON TIME PERIOD
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 TimePeriod <- crimedata$Time.Period
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 TimePeriod <- as.data.frame(TimePeriod)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 library(dummies)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 TimePeriod<- dummy.data.frame(TimePeriod, names =
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0	c("TimePeriod"),sep="=")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata <- data.frame(crimedata, TimePeriod)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 # ON CRIME CODE
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 CrimeCode <- crimedata$Crime.Code
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 CrimeCode <- as.data.frame(CrimeCode)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 library(dummies)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 CrimeCode<- dummy.data.frame(CrimeCode, names =
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0	c("CrimeCode"),sep="=")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata <- data.frame(crimedata, CrimeCode)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 # ON VICTIM AGE
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimAge <- crimedata$Victim.Age
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimAge <- as.data.frame(VictimAge)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 library(dummies)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimAge<- dummy.data.frame(VictimAge, names =
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0	c("VictimAge"),sep="=")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 crimedata <- data.frame(crimedata, VictimAge)
\f1\b0\fs24 \cf3 \strokec3 \
\
# Analysis \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 # Day based
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 install.packages("magrittr")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 install.packages("lubridate")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 install.packages("dplyr")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 install.packages("tidyr")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 install.packages("ggplot2")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 library(lubridate)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 library(magrittr)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 library("dplyr")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 library("tidyr")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 library(base)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 library(ggplot2)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 LACrime$DayOfCrime = \'a0wday(mdy(LACrime$DateOccurred),label = TRUE)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 LACrime %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0group_by(DayOfCrime) %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0summarise(CountIncidents = n()) %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0mutate(DayOfCrime = reorder(DayOfCrime,CountIncidents)) %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0ggplot(aes(x = DayOfCrime,y = CountIncidents)) +
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0geom_bar(stat='identity',colour="white", fill ="steelblue") +
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0geom_text(aes(x = DayOfCrime, y = 1, label = paste0("(",CountIncidents,")",sep="")),
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0hjust=0, vjust=.5, size = 4, colour = 'black',
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0fontface = 'bold') +
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0labs(x = 'Day Of Crime', y = 'Count of Incidents', 
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0title = 'Count of Incidents') +
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0coord_flip() + 
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0theme_bw()
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\
\
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 # Year based
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 LACrime$YearOfCrime = \'a0year(mdy(LACrime$DateOccurred))\uc0\u8232 \u8232 LACrime %>%\u8232  \'a0group_by(YearOfCrime) %>%\u8232  \'a0summarise(CountIncidents = n()) %>%\u8232  \'a0mutate(YearOfCrime = reorder(YearOfCrime,CountIncidents)) %>%\u8232  \'a0\u8232  \'a0ggplot(aes(x = YearOfCrime,y = CountIncidents)) +\u8232  \'a0geom_bar(stat='identity',colour="white", fill ="steelblue") +\u8232  \'a0geom_text(aes(x = YearOfCrime, y = 1, label = paste0("(",CountIncidents,")",sep="")),\u8232  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0hjust=0.5, vjust=-0.5, size = 4, colour = 'black',\u8232  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0fontface = 'bold') +\u8232  \'a0labs(x = 'Year Of Crime', y = 'Count of Incidents', \u8232  \'a0\'a0\'a0\'a0\'a0\'a0title = 'Count of Incidents')
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 #descent based
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimDescentAbbr = c("A","B","C","D","F","G","H","I","J","K","L","O","P","S","U","V","W","X","Z")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimDescentDescription = c("Other Asian","Black",
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0"Chinese","Cambodian","Filipino",
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0"Guamanian","Hispanic/Latin/Mexican",
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0"American Indian/Alaskan Native",
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0"Japanese","Korean","Laotian ",
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0"Other","Pacific Islander",
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0"Samoan","Hawaiian","Vietnamese",
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0"White","Unknown","AsianIndian")
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 VictimDescentFull = data.frame(VictimDescent = as.character(VictimDescentAbbr),
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0VictimDescentDescription = as.character(VictimDescentDescription))
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 LACrime$VictimDescent = as.character(LACrime$VictimDescent)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 LACrime %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0filter(!is.na(VictimDescent)) %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0group_by(VictimDescent) %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0tally() %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0ungroup() %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0arrange(desc(n)) %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0inner_join(VictimDescentFull) %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0head(10) %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0mutate(VictimDescentDescription = reorder(VictimDescentDescription,n)) %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0ggplot(aes(x = VictimDescentDescription,y = n)) +
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0geom_bar(stat='identity',colour="white", fill ="yellow") +
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0geom_text(aes(x = VictimDescentDescription, y = 1, label = paste0("(",n,")",sep="")),
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0hjust=0, vjust=.5, size = 4, colour = 'black',
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0fontface = 'bold') +
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0labs(x = 'VictimDescent', y = 'Count of Incidents', 
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0title = 'Count of Incidents and VictimDescent') +
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0coord_flip() + 
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0theme_bw()
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 #count of incidents and crime code
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 LACrime %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0group_by(CrimeCodeDescription) %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0tally() %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0arrange(desc(n)) %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0head(20) %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0mutate(CrimeCodeDescription = reorder(CrimeCodeDescription,n)) %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0ggplot(aes(x = CrimeCodeDescription,y = n)) +
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0geom_bar(stat='identity',colour="white", fill ="yellow") +
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0geom_text(aes(x = CrimeCodeDescription, y = 1, label = paste0("(",n,")",sep="")),
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0hjust=0, vjust=.5, size = 4, colour = 'black',
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0fontface = 'bold') +
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0labs(x = 'CrimeCodeDescription', y = 'Count of Incidents', 
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0title = 'Count of Incidents and CrimeCodeDescription') +
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0coord_flip() + 
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0theme_bw()
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 #trend of crime
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 LACrimeGroup = LACrime %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0mutate(year_month = make_date(year=year(mdy(DateOccurred)),month=month(mdy(DateOccurred))) ) %>% 
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0filter(!is.na(year_month)) %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0group_by(year_month) %>% summarize(CountCrimes = n()) 
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 LACrimeGroup %>% filter(year_month != "2017-09-01") %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0ggplot(aes(x=year_month,y=CountCrimes)) + 
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0geom_line(size=.5, color="red")+geom_point(size=2, color="red")+theme_bw()
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\
\
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 Code 19\cf4 \strokec4  reporting districts and crime
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 library(rgdal)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 library(dplyr)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 library(RColorBrewer)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 library(leaflet)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 RDGeoLocationFull = Crime_Data_2010_2017 %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0group_by(`Reporting District`) %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0summarise(RDlat = median(Latitude,na.rm = TRUE)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0,RDlon = median(longitude,na.rm = TRUE)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0,CountIncidents = n())
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 RD = readOGR(dsn = "LAPD_Reporting_Districts.shp")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 RD@data$REPDIST = as.numeric(RD@data$REPDIST )
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 RDGeoLocationFull$`Reporting District` = as.numeric(RDGeoLocationFull$`Reporting District`)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 RD@data = left_join(RD@data, RDGeoLocationFull, by = c("REPDIST" = "Reporting District"))
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 bins = c(0,1000,2000, 4000, 5000, 6000, 7000,8000, 9000)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 pal = colorBin("YlOrRd", domain = RD@data$CountIncidents, bins = bins)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 labels = sprintf(
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0"<strong>%s</strong><br/>%g",
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0RD@data$REPDIST, RD@data$CountIncidents
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 ) %>% lapply(htmltools::HTML)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 center_lon = median(RD@data$RDlon,na.rm = TRUE)
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 center_lat = median(RD@data$RDlat,na.rm = TRUE)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 leaflet(data = RD) %>% \'a0setView(lng=center_lon, lat=center_lat, zoom=12) %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0addPolygons(
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0fillColor = ~pal(CountIncidents),
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0weight = 2,
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0opacity = 1,
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0color = "white",
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0dashArray = "3",
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0fillOpacity = 0.7,
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0highlight = highlightOptions(
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0weight = 5,
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0color = "#666",
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0dashArray = "",
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0fillOpacity = 0.7,
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0bringToFront = TRUE),
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0label = labels,
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0labelOptions = labelOptions(
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0style = list("font-weight" = "normal", padding = "3px 8px"),
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0textsize = "15px",
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0direction = "auto")) %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0addLegend(pal = pal, values = ~CountIncidents, opacity = 0.7, title = "Reporting Districts and Crimes",
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0position = "bottomleft")
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\
\
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf4 \strokec4 ### FORECAST
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 LACrimeGroupBatterySA = LACrime %>% 
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0filter(CrimeCode ==\cf3 \strokec3  \cf5 \strokec5 624\cf2 \strokec2 ) %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0mutate(year_month = make_date(year=year(mdy(DateOccurred)),month=month(mdy(DateOccurred))) ) %>% 
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0filter(!is.na(year_month)) %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0filter(year_month != "2017-09-01") %>%
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0group_by(year_month) %>% 
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2  \'a0summarize(CountCrimes = n()) \'a0
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\
\
\
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf6 \strokec6 install.packages("forecast",
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf6 \strokec6  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0repos = c("http://rstudio.org/_packages",
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf6 \strokec6  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0"http://cran.rstudio.com"))
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf6 \strokec6 install.packages("xts", repos="http://cloud.r-project.org")
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 CountOfCrimes = ts(LACrimeGroupBatterySA$CountCrimes)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 fit = auto.arima(CountOfCrimes,stepwise=FALSE, approximation=FALSE)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 predictions = fit %>% forecast(h=4)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 predictions %>% autoplot(include=80) +theme_bw()
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 preds=data.frame(predictions)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 dataSetCrimes = data.frame(Month = as.character(),Sex = as.character(), CrimeCount = as.numeric())
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 dataSetMonths = 
\fs28 data.frame(c("September 2017",\uc0\u8232  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0"October 2017",\u8232  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0"November 2017",\u8232  \'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0\'a0"December 2017"))
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 #preds<-round(as.numeric(unlist(preds)))
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 dataSetCrimes = cbind(dataSetMonths,preds)
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 colnames(dataSetCrimes) = c("Months","Predictions")
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf6 \strokec6 # install.packages("vignette")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf6 \strokec6 # install.packages("DT")
\f1\b0\fs24 \cf3 \strokec3 \

\f0\b\fs29\fsmilli14667 \cf6 \strokec6 # install.packages("yaml", repos="http://cloud.r-project.org")
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\pard\pardeftab720\sl400\partightenfactor0

\f0\b\fs29\fsmilli14667 \cf2 \strokec2 datatable(dataSetCrimes, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
\f1\b0\fs24 \cf3 \strokec3 \
\pard\pardeftab720\sl280\partightenfactor0
\cf3 \
\
}