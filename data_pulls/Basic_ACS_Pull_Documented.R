# This code is for pulling data from the American Community Survey (ACS) through 
# the Latinum server. The ACS has both population-level and household-level 
# variables, each of which is stored in different data file. In order to 
# determine what variables to pull and what file to pull them from, please refer
# to the ACS data dictionary for the relevant year. The dictionary is labeled 
# "[Year] Data Dictionary" and can be found in the Z drive: 
# Z:\Public_Data\ACS\Documentation. 

# Set your working directory to the folder where files will be saved/pulled from
setwd("C:/Users/Cristina/Documents")

# Install the necessary packages and load libraries you will need. You only need
# to install packages the first time you use RStudio, but libraries need to be loaded 
# every time.
install.packages("RPostgreSQL")
install.packages("plyr")
install.packages("data.table")
install.packages("reshape2")

library(RPostgreSQL)
library(plyr)
library(data.table)
library(reshape2)


# In this step you establish a connection with the R server, which is hosted locally
# in the Latinum office. Please note the server cannot be accessed remotely, so 
# if you ever need to work from home be sure to load data prior to leaving the office.
# Connect using your own username and password provided by Latinum administrators.
drv <- dbDriver("PostgreSQL") #Sets up driver
con <- dbConnect(drv, host = "192.168.3.69",user = "username",password = "password",
                 dbname = "DECI_ODS") #Connects to data, in this case staging

# Define and pull the necessary variables, including year, from the household 
# and/or population data tables. You can look up the variable names in the ACS
# data dictionary for the relevant year. The data is stored in DECI, which you 
# can access from PGAdminIII.
hh <- dbGetQuery(con, "SELECT  serialno, puma, noc, wgtp, year from 
                 public_data.acs_household where year = 2013")

pop <- dbGetQuery(con, "select serialno, puma, sex, agep, rac1p, hisp, lanp, eng,
                  rel08_x, pwgtp from public_data.acs_population where year = 2013")

# IMPORTANT: Make sure to disconnect from the server/database once your data
# is pulled. This is extremely important for the processing speed of the server.
dbDisconnect(con)


# Data originally is stored as a data frame. Convert to data table for 
# easier manipulation. 
pop<-as.data.table(pop)
hh<-as.data.table(hh)


# To look at the data you pulled, the default head(pop) will show you the first
# six lines. You can adjust the # of lines by modifying head(pop, [# of lines]).
head(pop)
head(hh)


# You can define any variable as a new column in the data table, using the
# existing variables pulled from the ACS. Below are common definitions that you
# may want to use, but modify as needed for your particular analysis. 

# Race
pop[, Hispanic:= ifelse(hisp != 1, 1, 0)]
pop[, White:=ifelse((hisp == 1) & (rac1p == 1), 1, 0)]
pop[, AfAm:=ifelse((hisp == 1) & (rac1p == 2), 1, 0)]
pop[, Asian:=ifelse((hisp == 1) & (rac1p %in% 6:7), 1, 0)]
pop[, Other_Race:=ifelse((hisp == 1) & (rac1p %in% c(3,4,5,8,9)), 1, 0)]

# Factors allow you to look at a variable in a single column, rather than having
# a column per variable. This allows you to play with the data in a "long"
# rather than "wide" format.
pop[, Race := factor(White*1 + Hispanic*2 + AfAm*3 + Asian*4 + Other_Race*5, 
                    levels = 1:5, labels = c("White", "Hispanic", "AfAm", 
                                             "Asian", "Other"))]

# Define age buckets
pop[, Age_Bucket := ifelse(agep < 18,"0-17", ifelse(agep < 35, "18-34", 
                    ifelse(agep < 50, "35-49", ifelse(agep < 65, "50-64", 
                    ifelse(agep >= 65, "65+",0)))))]


# Define acculturation
pop[, Acculturated:= ifelse((is.na(lanp)) & (agep > 4) & (Hispanic == 1), 1, 0)]
pop[, Bicultural:=  ifelse((((!is.na(lanp)) & (lanp == '625')) & (eng == '1') & 
                             (agep > 4) & (Hispanic == 1)), 1, 0)]
pop[, Unacculturated:=  ifelse(((!is.na(lanp)) & (lanp == '625') & (eng %in% 
                        c('2','3','4')) & (agep > 4) & (Hispanic == 1)), 1, 0)]
pop[,Other := ifelse((Hispanic == 1 & Acculturated == 0 & Bicultural == 0 & 
                      Unacculturated == 0), 1, 0)]
# Create factor for acculturation.
pop[,Acculturation := factor(Acculturated*1 + Bicultural*2 + Unacculturated*3 +
                               Other*4, levels = 1:4, labels = c("Acculturated",
                                "Bicultural", "Unacculturated", "Other"))]


# Hispanic origin
pop[,Hisp_Orig:= ifelse(hisp == 2, "Mexican", ifelse(hisp %in% 3:5, "Caribbean", 
                                            ifelse(hisp %in% 6:12, "C_American",
                                             ifelse(hisp %in% 13:22,"S_American",
                                                    "Other"))))]

# To make sure your data is pulled correctly, test a variable that you are 
# familiar with. For example, we know the Hispanic population should be ~54K for
# 2013. The result will show up on the console below. 
pop[Race == "Hispanic", sum(pwgtp)]


# Now on to the household file. Similar to the pop file, define the variables
# you'd like to create using any of the ACS variables you pulled from the server. 

# Ex: Define Income
hh[, inc:= hincp * (adjinc / 1000000.0)]
hh[,Income_Bucket:= ifelse(inc < 35000, "<$35K", ifelse(inc < 75000, "$35K-$75K",
                                              ifelse(inc >= 75000, ">$75K", 0)))]


# In order to look at household variables by race, origin, or any other population
# -specific variable, you must combine the household and population files into one.
combine <- join(pop, hh, by = "serialno")
combine <- as.data.table(combine)


# At this point, you can work with your data however you'd like. You can perform
# multiple data cuts, look at single variables at a time, or export all variables. 

# Example: Subsetting data to look at population numbers by race, age, origin,
# and acculturation. 
pop.subset<-subset(pop, select=c("Race", "Age_Bucket", "Hisp_Orig", 
                                 "Acculturation", "pwgtp"))

# Before you export, use the lapply function to aggregate your data so that you are 
# looking at the sum of all records rather than individual population records. 
pop.subset<-pop.subset[, lapply(.SD, sum), by=list(Race, Age_Bucket, Hisp_Orig,
                                                   Acculturation), .SDcols="pwgtp"]


# Export data to a csv. Note that the file will save onto the working directory.
write.csv(pop.subset, "Population_2013.csv")

