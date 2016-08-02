#Load relevant libraries.
library(data.table)
library(ggplot2)
library(dplyr)

#Set working directory to the west nile folder.
setwd("~/GitHub-kszela24/west-nile")

#Load in competition spreadsheets as data tables.
train <- fread("./Data/train.csv", stringsAsFactors = F)
test <- fread("./Data/test.csv", stringsAsFactors = F)
weather <- fread("./Data/weather.csv", stringsAsFactors = F)
spray <- fread("./Data/spray.csv", stringsAsFactors = F)

#Plotting species vs target variable
train$Species <- as.factor(train$Species)
species_plot <- ggplot(train, aes(x = train$Species, y = train$WnvPresent))
species_plot + geom_bar(stat = "identity")

#It appears that the West Nile Virus is not present in species Salinarius, Tarsalis, 
#Territans, and Erraticus in this dataset.  Strangely, from this website:
#https://www.cdc.gov/westnile/resources/pdfs/mosquitospecies1999-2012.pdf we should see at 
#least some of these species with the virus present.  Maybe they have a much lower rate at 
#which they contract the virus?
#Maybe try a boolean feature to help separate these two groups, or a relevel to help tree
#based algorithms to easily split between the different species on the target variable.


#Plotting Block vs target variable.
block_plot <- ggplot(train, aes(x = train$Block, y = train$WnvPresent))
block_plot + geom_bar(stat = "identity")

#It looks like some blocks may be more prone to mosquitoes having the virus.  Could be 
#due to presence of still water, since I believe that's required for mosquito reproduction.


#Plotting lat and long vs target variable.  Although the best way to do this would be to 
#take the curvature of the earth into account, since the distances should be relatively
#short (IE all in the Chicagoland area) this approximation should be fine.  Not as 
#interested in the other location data, since I would probably end up converting addresses,
#streets, and traps to lat/long anyways.
lat_long_plot <- ggplot(train, aes(x = train$Longitude, y = train$Latitude))
lat_long_plot + geom_point(aes(color = train$WnvPresent))

#May be a pattern here, but need to go more in depth.  The way this is being graphed
#might be on displaying the topmost point.  Since the traps would be at the same location,
#overlaying them might not be the best choice.  Need to possibly try a 3 dimensional 
#density chart for further analysis.


#Plotting addressaccuracy vs target variable.
add_acc_plot <- ggplot(train, aes(x = train$AddressAccuracy, y = train$WnvPresent))
add_acc_plot + geom_bar(stat = "identity")

#Don't really know what to make of this yet.  The AddressAccuracy is the accuracy 
#returned from the GeoCoder (from the Kaggle dataset definitions).


#Plotting date vs count of observations where wnv was present.
train$Date <- as.Date(train$Date)
ggplot(train, aes(Date, WnvPresent)) + geom_bar(stat = "identity")

#Also looks like there may be a trend here.  From looking at train, the dataset doesn't
#appear to have observations for the winter months, but I can see some tapering off of 
#the number of obeervations where WNV was present as the Date moves from winter to spring,
#and from summer to fall.  Could be some potential features here:
#1.  Due to the way the train and test sets are arranged chronologically, I want to make
  #the predictions irrelvant to the time series data.  One idea for this is to get rid of
  #the year portion of the date, and separate the day of month from the month.  This way
  #the month data from previous years may have some predictive power for subsequent years.
#2.  Also may try distance to each of the testing centers.


