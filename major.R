library(stringi)
library(stringr)
library(dplyr)
library(tidyr)
library(e1071)

crimedata <- Crime_Data_2010_2017
crimedata$`Crime Code 1` <- NULL
View(crimedata)
crimedata$`Crime Code 2` <- NULL
crimedata$`Crime Code 3` <- NULL
crimedata$`Crime Code 4` <- NULL
crimedata$`Cross Street` <- NULL
crimedata$Address <- NULL
crimedata$`Reporting District` <- NULL
crimedata$`DR Number` <- NULL

LACrime_latlong = gsub('\\(','',crimedata$Location)
LACrime_latlong = gsub('\\)','',LACrime_latlong)
LACrime_latlong = strsplit(LACrime_latlong,",")
LACrime_latlong = do.call(rbind.data.frame, LACrime_latlong)
colnames(LACrime_latlong) = c("latitude","longitude")
crimedata$latitude  = as.numeric(as.character(LACrime_latlong$latitude))
crimedata$longitude = as.numeric(as.character(LACrime_latlong$longitude))
rm(LACrime_latlong)
crimedata$Location = NULL

PremiseCode<- unique(crimedata[c("Premise Code", "Premise Description")])
PremiseCode[is.na(PremiseCode$`Premise Description`),"Premise Description"] <-"UNK"
write.csv(PremiseCode, "Premise_Desc.csv")
CrimeCodeDesc <- unique(crimedata[c("Crime Code", "Crime Code Description")])
CrimeCodeDesc[is.na(CrimeCodeDesc$`Crime Code Description`),"Crime Code Description"] <- "UNK"
write.csv(CrimeCodeDesc, "Crime_Code_Desc.csv")

crimedata$`Crime Code Description` = NULL
crimedata$`Premise Description` = NULL
AreaName <- unique(crimedata[c("Area ID", "Area Name")])
AreaName[is.na(AreaName$`Area Name`),"Area Name"] <- "UNK"
write.csv(AreaName, "Area_Name.csv")
WeaponDescription <- unique(crimedata[c("Weapon Used Code", "Weapon Description")])
WeaponDescription[is.na(WeaponDescription$`Weapon Description`),"Weapon Description"] <- "UNK"
write.csv(WeaponDescription, "Weapon_Description.csv")

crimedata$`Weapon Description` = NULL
crimedata$`Area Name` = NULL
crimedata$`MO Codes` = NULL
StatusDescription <- unique(crimedata[c("Status Code", "Status Description")])
StatusDescription[is.na(StatusDescription$`Status Description`),"Status Description"] <- "UNK"
write.csv(StatusDescription, "Status_Description.csv")
crimedata$`Status Description` = NULL

crimedata <- crimedata[!is.na(crimedata$`latitude`),]
crimedata <- crimedata[!is.na(crimedata$`Status Code`),]

weapondata <- crimedata[!is.na(crimedata$`Weapon Used Code`),]
write.csv(weapondata, "weapondata.csv")
crimedata$`Weapon Used Code` = NULL

table(is.na(crimedata$`Victim Sex`))
table(is.na(crimedata$`Victim Age`))

crimedata<-separate(crimedata,"Date Reported",c("DRYear","DRMonth","DRDay"),sep="-");
crimedata<-separate(crimedata,"Date Occurred",c("DOMonth","DODay","DOYear"),sep="/");

#Clustering attributes values
crimedata[which(crimedata$`Time Occurred` < 2400),"Time Period"] <- "4"
crimedata[which(crimedata$`Time Occurred`< 1800),"Time Period"] <- "3"
crimedata[which(crimedata$`Time Occurred`< 1200),"Time Period"] <- "2"
crimedata[which(crimedata$`Time Occurred` < 600),"Time Period"] <- "1"
crimedata$`Time Occurred` <- NULL

crimedata[which(crimedata$`Victim Descent`== "A"|crimedata$`Victim Descent`== "C"|crimedata$`Victim Descent`== "D"|crimedata$`Victim Descent`== "F"|crimedata$`Victim Descent`== "J"|crimedata$`Victim Descent`== "K"|crimedata$`Victim Descent`== "L"|crimedata$`Victim Descent`== "V"|crimedata$`Victim Descent`== "Z"),"Victim Descent"] <- "1"
crimedata[which(crimedata$`Victim Descent`== "B"),"Victim Descent"] <- "2"
crimedata[which(crimedata$`Victim Descent`== "O"|crimedata$`Victim Descent`== "X"),"Victim Descent"] <- "7"
crimedata[which(crimedata$`Victim Descent`== "H"),"Victim Descent"] <- "4"
crimedata[which(crimedata$`Victim Descent`== "W"),"Victim Descent"] <- "5"
crimedata[which(crimedata$`Victim Descent`== "I"),"Victim Descent"] <- "6"
crimedata[which(crimedata$`Victim Descent`== "G"|crimedata$`Victim Descent`== "P"|crimedata$`Victim Descent`== "S"|crimedata$`Victim Descent`== "U"),"Victim Descent"] <- "3"
crimedata$`Victim Descent` <- as.integer(crimedata$`Victim Descent`)

crimedata[which(crimedata$`Victim Sex`== "M"),"Victim Sex"] <- "1"
crimedata[which(crimedata$`Victim Sex`== "F"),"Victim Sex"] <- "2"
crimedata[which(crimedata$`Victim Sex`== "H"),"Victim Sex"] <- "3"
crimedata[which(crimedata$`Victim Sex`== "X"),"Victim Sex"] <- "4"
crimedata$`Victim Sex` <- as.integer(crimedata$`Victim Sex`)

crimedata[which(crimedata$`Victim Age` < 100),"Victim Age1"] <- "4"
crimedata[which(crimedata$`Victim Age` < 60),"Victim Age1"] <- "3"
crimedata[which(crimedata$`Victim Age`< 36),"Victim Age1"] <- "2"
crimedata[which(crimedata$`Victim Age` < 19),"Victim Age1"] <- "1"
crimedata$`Victim Age` <- NULL
colnames(crimedata)[16] <- "Victim Age" # Renaming column name
crimedata$`Victim Age` <- as.integer(crimedata$`Victim Age`)


crimedata <- crimedata[!(crimedata$`Status Code` == "13"),]
crimedata <- crimedata[!(crimedata$`Status Code` == "19"),]
crimedata <- crimedata[!(crimedata$`Status Code` == "TH"),]
crimedata[which(crimedata$`Status Code`== "AA"),"Status Code"] <- "1"
crimedata[which(crimedata$`Status Code`== "AO"),"Status Code"] <- "2"
crimedata[which(crimedata$`Status Code`== "CC"),"Status Code"] <- "3"
crimedata[which(crimedata$`Status Code`== "IC"),"Status Code"] <- "4"
crimedata[which(crimedata$`Status Code`== "JA"),"Status Code"] <- "5"
crimedata[which(crimedata$`Status Code`== "JO"),"Status Code"] <- "6"

# Clustering crime dataset
crimedata[which(crimedata$`Crime Code`== "760"| crimedata$`Crime Code` == "814"|crimedata$`Crime Code` == "822"|crimedata$`Crime Code` == "921"),"Crime Code"] <- "0"

crimedata[which(crimedata$`Crime Code`== "110"| crimedata$`Crime Code`== "113"| crimedata$`Crime Code`== "230"| crimedata$`Crime Code`== "231"| crimedata$`Crime Code`== "250"| crimedata$`Crime Code`== "251"| crimedata$`Crime Code`== "622"| crimedata$`Crime Code`== "623"| crimedata$`Crime Code`== "624"| crimedata$`Crime Code`== "625"| crimedata$`Crime Code`== "626"| crimedata$`Crime Code`== "753"| crimedata$`Crime Code`== "910"| crimedata$`Crime Code`== "920"),"Crime Code"] <- "1"


crimedata[which(crimedata$`Crime Code`== "121"|crimedata$`Crime Code`== "122"|crimedata$`Crime Code`== "236"|crimedata$`Crime Code`== "815"|crimedata$`Crime Code`== "820"|crimedata$`Crime Code`== "821"|crimedata$`Crime Code`== "840"|crimedata$`Crime Code`== "860"|crimedata$`Crime Code`== "762"|crimedata$`Crime Code`== "805"|crimedata$`Crime Code`== "806"|crimedata$`Crime Code`== "810"|crimedata$`Crime Code`== "830"|crimedata$`Crime Code`== "850"|crimedata$`Crime Code`== "956"),"Crime Code"] <- "2"


crimedata[which(crimedata$`Crime Code`== "210"|crimedata$`Crime Code`== "220"|crimedata$`Crime Code`== "310"|crimedata$`Crime Code`== "320"|crimedata$`Crime Code`== "330"|crimedata$`Crime Code`== "331"|crimedata$`Crime Code`== "341"|crimedata$`Crime Code`== "343"|crimedata$`Crime Code`== "350"|crimedata$`Crime Code`== "351"|crimedata$`Crime Code`== "352"|crimedata$`Crime Code`== "353"|crimedata$`Crime Code`== "410"|crimedata$`Crime Code`== "420"|crimedata$`Crime Code`== "421"|crimedata$`Crime Code`== "432"|crimedata$`Crime Code`== "433"|crimedata$`Crime Code`== "440"|crimedata$`Crime Code`== "441"|crimedata$`Crime Code`== "442"|crimedata$`Crime Code`== "443"|crimedata$`Crime Code`== "444"|crimedata$`Crime Code`== "445"|crimedata$`Crime Code`== "446"|crimedata$`Crime Code`== "450"|crimedata$`Crime Code`== "451"|crimedata$`Crime Code`== "452"|crimedata$`Crime Code`== "473"|crimedata$`Crime Code`== "474"|crimedata$`Crime Code`== "475"|crimedata$`Crime Code`== "470"|crimedata$`Crime Code`== "471"|crimedata$`Crime Code`== "472"|crimedata$`Crime Code`== "662"|crimedata$`Crime Code`== "664"|crimedata$`Crime Code`== "666"|crimedata$`Crime Code`== "668"|crimedata$`Crime Code`== "670"|crimedata$`Crime Code`== "345"|crimedata$`Crime Code`== "347"|crimedata$`Crime Code`== "349"|crimedata$`Crime Code`== "354"|crimedata$`Crime Code`== "480"|crimedata$`Crime Code`== "485"|crimedata$`Crime Code`== "487"|crimedata$`Crime Code`== "510"|crimedata$`Crime Code`== "520"|crimedata$`Crime Code`== "950"|crimedata$`Crime Code`== "951"),"Crime Code" ] <- "3"

crimedata[which(crimedata$`Crime Code`== "235"|crimedata$`Crime Code`== "237"|crimedata$`Crime Code`== "627"|crimedata$`Crime Code`== "812"|crimedata$`Crime Code`== "813"|crimedata$`Crime Code`== "865"|crimedata$`Crime Code`== "870"|crimedata$`Crime Code`== "922"),"Crime Code"] <- "4"

crimedata[which(crimedata$`Crime Code`== "435"|crimedata$`Crime Code`== "436"|crimedata$`Crime Code`== "648"|crimedata$`Crime Code`== "740"|crimedata$`Crime Code`== "745"|crimedata$`Crime Code`== "880"|crimedata$`Crime Code`== "882"|crimedata$`Crime Code`== "884"|crimedata$`Crime Code`== "886"|crimedata$`Crime Code`== "926"|crimedata$`Crime Code`== "755"|crimedata$`Crime Code`== "756"),"Crime Code"] <- "5"

crimedata[which(crimedata$`Crime Code`== "434"|crimedata$`Crime Code`== "437"|crimedata$`Crime Code`== "438"|crimedata$`Crime Code`== "439"|crimedata$`Crime Code`== "453"|crimedata$`Crime Code`== "888"|crimedata$`Crime Code`== "890"|crimedata$`Crime Code`== "900"|crimedata$`Crime Code`== "901"|crimedata$`Crime Code`== "902"|crimedata$`Crime Code`== "903"|crimedata$`Crime Code`== "924"|crimedata$`Crime Code`== "931"|crimedata$`Crime Code`== "932"|crimedata$`Crime Code`== "942"|crimedata$`Crime Code`== "943"|crimedata$`Crime Code`== "944"|crimedata$`Crime Code`== "946"|crimedata$`Crime Code`== "948"|crimedata$`Crime Code`== "949"|crimedata$`Crime Code`== "952"|crimedata$`Crime Code`== "954"),"Crime Code"] <- "6"

crimedata[which(crimedata$`Crime Code`== "649"|crimedata$`Crime Code`== "651"|crimedata$`Crime Code`== "652"|crimedata$`Crime Code`== "653"|crimedata$`Crime Code`== "654"|crimedata$`Crime Code`== "660"|crimedata$`Crime Code`== "661"),"Crime Code"] <- "7"

crimedata[which(crimedata$`Crime Code`== "647"|crimedata$`Crime Code`== "761"|crimedata$`Crime Code`== "763"|crimedata$`Crime Code`== "928"|crimedata$`Crime Code`== "930"|crimedata$`Crime Code`== "940"|crimedata$`Crime Code`== "933"),"Crime Code"] <- "8"


#Converting numeric, string, etc. to factor in R
sapply(crimedata, class)
crimedata$DRYear<- as.factor(crimedata$DRYear)
crimedata$DRMonth<- as.factor(crimedata$DRMonth)
crimedata$DRDay<- as.factor(crimedata$DRDay)
crimedata$DOYear<- as.factor(crimedata$DOYear)
crimedata$DOMonth<- as.factor(crimedata$DOMonth)
crimedata$DODay<- as.factor(crimedata$DODay)
crimedata$`Area ID`<- as.factor(crimedata$`Area ID`)
crimedata$`Crime Code`<- as.factor(crimedata$`Crime Code`)
crimedata$`Victim Sex`<- as.factor(crimedata$`Victim Sex`)
crimedata$`Victim Descent`<- as.factor(crimedata$`Victim Descent`)
crimedata$`Premise Code`<- as.factor(crimedata$`Premise Code`)
crimedata$`Status Code`<- as.factor(crimedata$`Status Code`)
crimedata$latitude<- as.factor(crimedata$latitude)
crimedata$longitude<- as.factor(crimedata$longitude)
crimedata$`Time Period`<- as.factor(crimedata$`Time Period`)
crimedata$`Victim Age`<- as.factor(crimedata$`Victim Age`)

#Preparing Test And Train data for predicting missing attributes & Filling Missing # Values Using Naive Bayes

VictimSextrain <- crimedata[!(is.na(crimedata$`Victim Sex`)),]
VictimSextrain <- VictimSextrain[!(VictimSextrain$`Victim Sex` == "-"),]
VictimSextest <- crimedata[is.na(crimedata$`Victim Sex`),]
#predicting Victim Sex with 62% accuracy
model <- naiveBayes(VictimSextrain$`Victim Sex` ~ ., data = VictimSextrain)
preds <- predict(model, newdata = VictimSextest)
VictimSextest$`Victim Sex` <- preds
total <- rbind(VictimSextrain, VictimSextest)


VictimAgetrain <- total[!(is.na(total$`Victim Age`)),]
VictimAgetest <- total[is.na(total$`Victim Age`),]
#predicting Victim Age with 52% accuracy
model <- naiveBayes(VictimAgetrain$`Victim Age` ~ ., data = VictimAgetrain)
preds <- predict(model, newdata = VictimAgetest)
VictimAgetest$`Victim Age` <- preds
total <- rbind(VictimAgetrain, VictimAgetest)

VictimDescenttrain <- total[!(is.na(total$`Victim Descent`)),]
VictimDescenttrain <- VictimDescenttrain[!(VictimDescenttrain$`Victim Descent` == "-"),]
VictimDescenttest <- total[is.na(crimedata$`Victim Descent`),]
#predicting Victim Descent with 51.4% accuracy
model <- naiveBayes(VictimDescenttrain$`Victim Descent` ~ ., data = VictimDescenttrain)
preds <- predict(model, newdata = VictimDescenttest)
VictimDescenttest$`Victim Descent` <- preds
crimedata <- rbind(VictimDescenttrain, VictimDescenttest)


Premisetrain <- crimedata[!(is.na(crimedata$`Premise Code`)),]
Premisetest <- crimedata[is.na(crimedata$`Premise Code`),]
#predicting Premise Code with 52% accuracy
model <- naiveBayes(Premisetrain$`Premise Code` ~ ., data = Premisetrain)
preds <- predict(model, newdata = Premisetest)
Premisetest$`Premise Code` <- preds
crimedata <- rbind(Premisetrain, Premisetest)

# Writing train and test data to csv
write.csv(crimedata, "crimedatanew.csv")
# write.csv(VictimSextrain, "VictimSextrain.csv")
# write.csv(VictimSextest, "VictimSextest.csv")
# write.csv(VictimAgetrain, "VictimAgetrain.csv")
# write.csv(VictimAgetest, "VictimAgetest.csv")
# write.csv(VictimDescenttrain, "VictimDescentTrain.csv")
# write.csv(VictimDescenttest, "VictimDescentTest.csv")

# ONE HOT ENCODING ON OBTAINED CRIME DATASET
# ON VICTIM SEX
crimedata[which(crimedata$`Victim Sex`== 1),"Victim Sex"] <- "M"
crimedata[which(crimedata$`Victim Sex`== 2),"Victim Sex"] <- "F"
crimedata[which(crimedata$`Victim Sex`== 3),"Victim Sex"] <- "H"
crimedata[which(crimedata$`Victim Sex`== 4),"Victim Sex"] <- "X"
VictimSex <- crimedata$`Victim Sex`
VictimSex <- as.data.frame(VictimSex)
library(dummies)
VictimSex<- dummy.data.frame(VictimSex, names =
                                 	c("VictimSex"),sep="=")
write.csv(VictimSex, "victimsex.csv")
crimedata <- data.frame(crimedata, VictimSex)

# ON STATUS CODE
StatusCode <- crimedata$Status.Code
StatusCode <- as.data.frame(StatusCode)
library(dummies)
StatusCode<- dummy.data.frame(StatusCode, names =
                           	c("StatusCode"),sep="=")
write.csv(StatusCode, "statuscode.csv")
crimedata <- data.frame(crimedata, StatusCode)

# ON VICTIM DESCENT
VictimDescent <- crimedata$Victim.Descent
VictimDescent <- as.data.frame(VictimDescent)
library(dummies)
VictimDescent<- dummy.data.frame(VictimDescent, names =
                            	c("VictimDescent"),sep="=")
crimedata <- data.frame(crimedata, VictimDescent)

# ON TIME PERIOD
TimePeriod <- crimedata$Time.Period
TimePeriod <- as.data.frame(TimePeriod)
library(dummies)
TimePeriod<- dummy.data.frame(TimePeriod, names =
                               	c("TimePeriod"),sep="=")
crimedata <- data.frame(crimedata, TimePeriod)

# ON CRIME CODE
CrimeCode <- crimedata$Crime.Code
CrimeCode <- as.data.frame(CrimeCode)
library(dummies)
CrimeCode<- dummy.data.frame(CrimeCode, names =
                            	c("CrimeCode"),sep="=")
crimedata <- data.frame(crimedata, CrimeCode)

# ON VICTIM AGE
VictimAge <- crimedata$Victim.Age
VictimAge <- as.data.frame(VictimAge)
library(dummies)
VictimAge<- dummy.data.frame(VictimAge, names =
                           	c("VictimAge"),sep="=")
crimedata <- data.frame(crimedata, VictimAge)

# Analysis 

# Day based

install.packages("magrittr")
install.packages("lubridate")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
library(lubridate)
library(magrittr)
library("dplyr")
library("tidyr")
library(base)
library(ggplot2)

LACrime$DayOfCrime =  wday(mdy(LACrime$DateOccurred),label = TRUE)

LACrime %>%
  group_by(DayOfCrime) %>%
  summarise(CountIncidents = n()) %>%
  mutate(DayOfCrime = reorder(DayOfCrime,CountIncidents)) %>%
  
  ggplot(aes(x = DayOfCrime,y = CountIncidents)) +
  geom_bar(stat='identity',colour="white", fill ="steelblue") +
  geom_text(aes(x = DayOfCrime, y = 1, label = paste0("(",CountIncidents,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Day Of Crime', y = 'Count of Incidents', 
       title = 'Count of Incidents') +
  coord_flip() + 
  theme_bw()



# Year based

LACrime$YearOfCrime =  year(mdy(LACrime$DateOccurred))  LACrime %>%   group_by(YearOfCrime) %>%   summarise(CountIncidents = n()) %>%   mutate(YearOfCrime = reorder(YearOfCrime,CountIncidents)) %>%      ggplot(aes(x = YearOfCrime,y = CountIncidents)) +   geom_bar(stat='identity',colour="white", fill ="steelblue") +   geom_text(aes(x = YearOfCrime, y = 1, label = paste0("(",CountIncidents,")",sep="")),             hjust=0.5, vjust=-0.5, size = 4, colour = 'black',             fontface = 'bold') +   labs(x = 'Year Of Crime', y = 'Count of Incidents',         title = 'Count of Incidents')


#descent based

VictimDescentAbbr = c("A","B","C","D","F","G","H","I","J","K","L","O","P","S","U","V","W","X","Z")
VictimDescentDescription = c("Other Asian","Black",
                             "Chinese","Cambodian","Filipino",
                             "Guamanian","Hispanic/Latin/Mexican",
                             "American Indian/Alaskan Native",
                             "Japanese","Korean","Laotian ",
                             "Other","Pacific Islander",
                             "Samoan","Hawaiian","Vietnamese",
                             "White","Unknown","AsianIndian")

VictimDescentFull = data.frame(VictimDescent = as.character(VictimDescentAbbr),
                               VictimDescentDescription = as.character(VictimDescentDescription))

LACrime$VictimDescent = as.character(LACrime$VictimDescent)

LACrime %>%
  filter(!is.na(VictimDescent)) %>%
  group_by(VictimDescent) %>%
  tally() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  inner_join(VictimDescentFull) %>%
  head(10) %>%
  mutate(VictimDescentDescription = reorder(VictimDescentDescription,n)) %>%
  
  ggplot(aes(x = VictimDescentDescription,y = n)) +
  geom_bar(stat='identity',colour="white", fill ="yellow") +
  geom_text(aes(x = VictimDescentDescription, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'VictimDescent', y = 'Count of Incidents', 
       title = 'Count of Incidents and VictimDescent') +
  coord_flip() + 
  theme_bw()


#count of incidents and crime code

LACrime %>%
  group_by(CrimeCodeDescription) %>%
  tally() %>%
  arrange(desc(n)) %>%
  head(20) %>%
  mutate(CrimeCodeDescription = reorder(CrimeCodeDescription,n)) %>%
  
  ggplot(aes(x = CrimeCodeDescription,y = n)) +
  geom_bar(stat='identity',colour="white", fill ="yellow") +
  geom_text(aes(x = CrimeCodeDescription, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'CrimeCodeDescription', y = 'Count of Incidents', 
       title = 'Count of Incidents and CrimeCodeDescription') +
  coord_flip() + 
  theme_bw()

#trend of crime

LACrimeGroup = LACrime %>%
  mutate(year_month = make_date(year=year(mdy(DateOccurred)),month=month(mdy(DateOccurred))) ) %>% 
  filter(!is.na(year_month)) %>%
  group_by(year_month) %>% summarize(CountCrimes = n()) 

LACrimeGroup %>% filter(year_month != "2017-09-01") %>%
  ggplot(aes(x=year_month,y=CountCrimes)) + 
  geom_line(size=.5, color="red")+geom_point(size=2, color="red")+theme_bw()



Code 19 reporting districts and crime

library(rgdal)
library(dplyr)
library(RColorBrewer)
library(leaflet)
RDGeoLocationFull = Crime_Data_2010_2017 %>%
  
  group_by(`Reporting District`) %>%
  summarise(RDlat = median(Latitude,na.rm = TRUE)
            ,RDlon = median(longitude,na.rm = TRUE)
            ,CountIncidents = n())

RD = readOGR(dsn = "LAPD_Reporting_Districts.shp")
RD@data$REPDIST = as.numeric(RD@data$REPDIST )
RDGeoLocationFull$`Reporting District` = as.numeric(RDGeoLocationFull$`Reporting District`)

RD@data = left_join(RD@data, RDGeoLocationFull, by = c("REPDIST" = "Reporting District"))

bins = c(0,1000,2000, 4000, 5000, 6000, 7000,8000, 9000)

pal = colorBin("YlOrRd", domain = RD@data$CountIncidents, bins = bins)

labels = sprintf(
  "<strong>%s</strong><br/>%g",
  RD@data$REPDIST, RD@data$CountIncidents
) %>% lapply(htmltools::HTML)

center_lon = median(RD@data$RDlon,na.rm = TRUE)
center_lat = median(RD@data$RDlat,na.rm = TRUE)

leaflet(data = RD) %>%  setView(lng=center_lon, lat=center_lat, zoom=12) %>%
  addPolygons(
    fillColor = ~pal(CountIncidents),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~CountIncidents, opacity = 0.7, title = "Reporting Districts and Crimes",
            position = "bottomleft")



### FORECAST

LACrimeGroupBatterySA = LACrime %>% 
  filter(CrimeCode == 624) %>%
  mutate(year_month = make_date(year=year(mdy(DateOccurred)),month=month(mdy(DateOccurred))) ) %>% 
  
  filter(!is.na(year_month)) %>%
  filter(year_month != "2017-09-01") %>%
  
  group_by(year_month) %>% 
  summarize(CountCrimes = n())  




install.packages("forecast",
                 repos = c("http://rstudio.org/_packages",
                           "http://cran.rstudio.com"))
install.packages("xts", repos="http://cloud.r-project.org")

CountOfCrimes = ts(LACrimeGroupBatterySA$CountCrimes)

fit = auto.arima(CountOfCrimes,stepwise=FALSE, approximation=FALSE)

predictions = fit %>% forecast(h=4)

predictions %>% autoplot(include=80) +theme_bw()
preds=data.frame(predictions)

dataSetCrimes = data.frame(Month = as.character(),Sex = as.character(), CrimeCount = as.numeric())

dataSetMonths = data.frame(c("September 2017",                              "October 2017",                              "November 2017",                              "December 2017"))

#preds<-round(as.numeric(unlist(preds)))
dataSetCrimes = cbind(dataSetMonths,preds)

colnames(dataSetCrimes) = c("Months","Predictions")


# install.packages("vignette")
# install.packages("DT")
# install.packages("yaml", repos="http://cloud.r-project.org")

datatable(dataSetCrimes, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))


