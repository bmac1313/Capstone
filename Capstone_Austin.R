#load dplyr, tidyr and ggplot packages

library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
#set workspace and verify
setwd("Desktop/Austin_Realty")
getwd()


#ZILLOW DATA

#DATA IMPORT AND CLEANING

#import Zillow time series rent data for all homes plus multi-family
ZillowRent<-read.csv(file="Zri_All_wMH_RENT.csv", header=TRUE,sep=",")

#Rename ZipCode, County, Years in ZillowRent
names(ZillowRent)<-c("RegionID","ZipCode","City","State","Metro","County","SizeRank","Rent2011","Rent2012","Rent2013","Rent2014","Rent2015")
head(ZillowRent,n=5)

#import Zillow time series home value data for all SF plus Condo homes
ZillowOwn<-read.csv(file="Zhvi_AllHomes_Values.csv", header=TRUE,sep=",")

#Remove "X" from time series (year) column header
names(ZillowOwn)<-c("RegionID","ZipCode","City","State","Metro","County","SizeRank","Own2011","Own2012","Own2013","Own2014","Own2015")
head(ZillowOwn,n=5)




#AMERICAN COMMUNITY SURVEY (CENSUS) DATA

#DATA IMPORT AND CLEANING
ACS2011<-read.csv(file="ACS_11_5YR_S1903.csv", header=TRUE,sep=",")
ACS2012<-read.csv(file="ACS_12_5YR_S1903.csv", header=TRUE,sep=",")
ACS2013<-read.csv(file="ACS_13_5YR_S1903.csv", header=TRUE,sep=",")
ACS2014<-read.csv(file="ACS_14_5YR_S1903.csv", header=TRUE,sep=",")
head(ACS2014, n=3)

#Change Headers for all ACS .csv Files
names(ACS2011) <- c("Geoid1", "ZipCode", "TotalEstHH2011", "MedIncomeHH2011")
  head(ACS2011,n=5)

names(ACS2012) <- c("Geoid1", "ZipCode", "TotalEstHH2012", "MedIncomeHH2012")
  head(ACS2012,n=5)

names(ACS2013) <- c("Geoid1", "ZipCode", "TotalEstHH2013", "MedIncomeHH2013")
  head(ACS2013,n=5)

names(ACS2014) <- c("Geoid1", "ZipCode", "TotalEstHH2014", "MedIncomeHH2014")
  head(ACS2014,n=5)

#Join ACS time series dataframes into a single dataframe called ACStotal

ACStotal_1 <- full_join(ACS2013, ACS2014, by="ZipCode")

ACStotal_2 <- full_join(ACS2011, ACS2012, by="ZipCode")

ACStotal <- full_join(ACStotal_2, ACStotal_1, by = "ZipCode")

head(ACStotal,n=5)
ACStotal$Geoid1.x.x <- NULL
ACStotal$Geoid1.y.x <- NULL
ACStotal$Geoid1.x.y <- NULL
ACStotal$Geoid1.y.y <- NULL
head(ACStotal, n=3)



#DATA ANALYSIS
#Add City, Metro and County data to ACS data, from Zillow Data 
ACStotal_geo <- full_join(ACStotal, ZillowOwn, by = "ZipCode")
head(ACStotal_geo, n=5)

#Remove unneccesary columns
ACStotal_geo$SizeRank <-ACStotal_geo$ Own2011  <- ACStotal_geo$ Own2012  <- ACStotal_geo$ Own2013  <- ACStotal_geo$ Own2014 <- ACStotal_geo$ Own2015 <- NULL

#Combine all data into one dataframe for easier analysis

#Master list of ACS, Rent, Own
Master_b <- full_join(ACStotal, ZillowOwn, by = "ZipCode")
Master <- full_join(Master_b, ZillowRent, by = "ZipCode")
head(Master, n=5)

#Remove unneccesary columns
Master$RegionID.x <-Master$City.y <- Master$RegionID.y <-Master$SizeRank.x <- Master$RegionID.y <- Master$City.y <- Master$State.y <-  Master$Metro.y<- Master$County.y<- Master$SizeRank.y<-NULL
head(Master, n=5)

# in Master DF - Renam City, State, Metro, County
Master <- rename(Master, City = City.x, State = State.x, Metro = Metro.x, County = County.x )
head(Master, n=5)

#Price to Rent Ratio per zip code
#Add new column with Rent-to-own in Master df
#$Homevalue ÷ (12 x Rent)
Master <- mutate(Master, RentOwn2011 = Own2011 / (12 * Rent2011))
Master <- mutate(Master, RentOwn2012 = Own2012 / (12 * Rent2012))
Master <- mutate(Master, RentOwn2013 = Own2013 / (12 * Rent2013))
Master <- mutate(Master, RentOwn2014 = Own2014 / (12 * Rent2014))
Master <- mutate(Master, RentOwn2015 = Own2015 / (12 * Rent2015))

head(Master,n=5)


#Summarize per city for ZillowOwn using dyplr
CityOwn<-Master %>%
  group_by(City) %>%
  summarise(
    Own2011 = mean(Own2011),
    Own2012 = mean(Own2012),
    Own2013 = mean(Own2013),
    Own2014 = mean(Own2014),
    Own2015 = mean(Own2015)
  )
head(CityOwn,n=5)

#Summarize per city for ZillowRent using dyplr
CityRent<-Master %>%
  group_by(City) %>%
  summarise(
    Rent2011 = mean(Rent2011),
    Rent2012 = mean(Rent2012),
    Rent2013 = mean(Rent2013),
    Rent2014 = mean(Rent2014),
    Rent2015 = mean(Rent2015)
  )
head(CityRent,n=5)

#Summarize per city for Medium Income using dyplr
CityMedIncome<-Master %>%
  group_by(City) %>%
  summarise(
    MedIncomeHH2011 = mean(MedIncomeHH2011),
    MedIncomeHH2012 = mean(MedIncomeHH2012),
    MedIncomeHH2013 = mean(MedIncomeHH2013),
    MedIncomeHH2014 = mean(MedIncomeHH2014)
  )
head(CityMedIncome,n=18)
head(CityOwn, n=5)
head(CityRent, n=5)
#Price to Rent Ratio per City
#Full Join CityMedIncome and CityRent tables
CityRent2Own <- full_join(CityOwn, CityRent, by = "City")
head(CityRent2Own,n=5)

#Add new column with Rent-to-own in Master df
#$Homevalue ÷ (12 x Rent)
CityRent2Own <- mutate(CityRent2Own, RentOwn2011 = (Own2011 / (12 * Rent2011)))
CityRent2Own <- mutate(CityRent2Own, RentOwn2012 = (Own2012 / (12 * Rent2012)))
CityRent2Own <- mutate(CityRent2Own, RentOwn2013 = (Own2013 / (12 * Rent2013)))
CityRent2Own <- mutate(CityRent2Own, RentOwn2014 = (Own2014 / (12 * Rent2014)))
CityRent2Own <- mutate(CityRent2Own, RentOwn2015 = (Own2015 / (12 * Rent2015)))
head(CityRent2Own,n=5)


#ggplot2 - Graph findings by city

#Graph 1 - Gather CityOwn df then plot City Ownership (CityOwnGather df)
#gather CityOwn df
head(CityOwn,n=18)
CityOwnGather <- CityOwn %>%
  gather(Type, Value, Own2011:Own2015)
head(CityOwnGather,n=5)

#split Own#### column into Type, Year Columns
CityOwnGather<-CityOwnGather %>%
  separate(Type, c("Type", "Year"), "Own")
CityOwnGather$Type<-"Own"
head(CityOwnGather,n=5)

#rename headers for CityOwnGather to City, Type Year, HomeValue
names(CityOwnGather) <- c("City", "Type", "Year", "HomeValue")
head(CityOwnGather,n=5)

#Create ggplot for City Ownership (CityOwnGather df)
#find Min and Max for y axis of graph
min(CityOwnGather$HomeValue)
max(CityOwnGather$HomeValue)

#Create ggplot for City Ownership (CityOwnGather df)
ggplot(data=CityOwnGather,
       aes(x=Year, y=HomeValue, color=City, group=City))+
        geom_line()+
        geom_point()+
        scale_y_continuous(limits = c(100000, 500000))+
        ggtitle("Austin Metro Home Values (Zillow 2011-2015)")+
        labs(x="Year",y="Home Value ($)")

  
#Graph 2 - Create ggplot for Zillow Rent
#Gather CityRent df then plot City Rent (CityRentGather df)
head(CityRent,n=18)
CityRentGather <- CityRent %>%
  gather(Type, Value, Rent2011:Rent2015)
head(CityRentGather,n=5)

#split Rent#### column into Type, Year Columns
CityRentGather<-CityRentGather %>%
  separate(Type, c("Type", "Year"), "Rent")
CityRentGather$Type<-"Rent"
head(CityRentGather,n=18)

#rename headers for CityRentGather to City, Type, Year, RentValue
names(CityRentGather) <- c("City", "Type", "Year", "RentValue")
head(CityRentGather,n=5)

#Create ggplot for City Rent (CityRentGather df)
#find Min and Max for y axis of graph
min(CityRentGather$RentValue)
max(CityRentGather$RentValue)

#Create ggplot for City Rent (CityRentGather df)
ggplot(data=CityRentGather,
       aes(x=Year, y=RentValue, color=City, group=City))+
  geom_line()+
  geom_point()+
  scale_y_continuous(limits = c(900, 3200))+
  ggtitle("Austin Metro Rent Values (Zillow 2011-2015)")+
  labs(x="Year",y="Rent Value ($)")

#Graph 3 - Create ggplot for ACS Data (CityMedIncome)
#Gather ACS df then plot ACS Income  (CityMedIncomeGather df)
head(CityMedIncome,n=18)
CityMedIncomeGather <- CityMedIncome %>%
  gather(Type, Value, MedIncomeHH2011:MedIncomeHH2014)
head(CityMedIncomeGather,n=5)

#split Income#### column into Type, Year Columns
CityMedIncomeGather<-CityMedIncomeGather %>%
  separate(Type, c("Type", "Year"), "HH")
CityMedIncomeGather$Type<-"Med HH Income"
head(CityMedIncomeGather,n=22)

#rename headers for ACSGather to City, Type, Year, AvgMedIncome
names(CityMedIncomeGather) <- c("City", "Type", "Year", "Income")
head(CityMedIncomeGather,n=5)

#Create ggplot for ACS Income (ACSGather df)
#find Min and Max for y axis of graph
min(CityMedIncomeGather$Income)
max(CityMedIncomeGather$Income)

#Create ggplot for ACS Income Values (ACSGather df)
ggplot(data=CityMedIncomeGather,
       aes(x=Year, y=Income, color=City, group=City))+
  geom_line()+
  geom_point()+
  scale_y_continuous(limits = c(40000, 130000))+
  ggtitle("Austin Metro Household Income (ACS 2011-2014)")+
  labs(x="Year",y="Household Income($)")

#Graph 4 - Create ggplot of Rent to Rent2Own Data
#Gather CityRent2Own df then plot City Rent2Own (CityRent2Own df)
head(CityRent2Own,n=5)
CityRent2OwnGather <- CityRent2Own %>%
  gather(Type, Rent2Own, RentOwn2011:RentOwn2015)
head(CityRent2OwnGather,n=5)

#split Rent-to-Own-Ratio#### column into Type, Year Columns
CityRent2OwnGather<-CityRent2OwnGather %>%
  separate(Type, c("Type", "Year"), "Own")
CityRent2OwnGather$Type<-"Rent to Own Ratio"
head(CityRent2OwnGather,n=18)

#Create ggplot for City Rent-to-Own (CityRent2Own df)
#find Min and Max for y axis of graph
min(CityRent2OwnGather$Rent2Own)
max(CityRent2OwnGather$Rent2Own)

#Create ggplot for City Rent-to-Own (CityRent2Own df)
ggplot(data=CityRent2OwnGather,
       aes(x=Year, y=Rent2Own, color=City, group=City))+
  geom_line()+
  geom_point()+
  scale_y_continuous(limits = c(8, 15))+
  ggtitle("Austin Metro Rent-to-Own Ratio (Zillow 2011-2015)")+
  labs(x="Year",y="Rent to Own Ratio")

#Graph 5 - Create Rent vs Income

#Graph 6 - Create Own vs Income

#Graph 7 - Create Graph of Rent to Own vs Income

#Regression model

