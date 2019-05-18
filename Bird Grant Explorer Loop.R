#
#
#
# Script to load grants data to create dataset of grants with
# bird in award description.
#
# 5/17/2019
#

rm(list = ls())
options(scipen = 9999, digits = 6)

library(dplyr)
library(stringr)
library(data.table)


# Create Working Directory Short Cuts
local_dir <- "PUT DIRECTORY INFO HERE"
setwd(local_dir)

# Raising RAM ceiling
memLim = 56000
memory.limit(size = memLim)


#####
# Importing/Loading grants daa
# This section is necessary if you don't have RData files already
# You can modify it to do the same thing as below to save time.
# I don't feel like doing that and then testing it.
# Cheers!
#####

years = c(2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009, 2008)
grants <- data.frame() #creating empty dataframe for use below
for (i in years){
  num_files = length(list.files(path = paste("Grants\\", i, sep = "")))
  for (j in 1:num_files){
    if (j == 1){
      address = paste("Grants\\",i,"\\all_assistance_prime_transactions", "_", 1 ,".csv", sep = "")
      grantsA <- fread(address, na.strings = c("NA", "Null", "NULL", ""))
      grantsA$fiscal_year <- paste(i)
      print(paste("File and Year Completed: ", j, i))
    } else{
      address = paste("Grants\\",i,"\\all_assistance_prime_transactions", "_", j ,".csv", sep = "")
      grantsB <- fread(address, na.strings = c("NA", "Null", "NULL", ""))
      grantsB$fiscal_year <- paste(i)
      grantsA <- rbind(grantsA, grantsB)
      print(paste("File and Year Completed: ", j, i))
    }
  }
  address = paste("Grants\\raw-grants", i, ".RData", sep = "")
  saveRDS(grantsA, file = address)
  grants <- rbind(grants, grantsA)
  rm(grantsA, grantsB, grantsAB)
  print(paste("Year Completed: ", i))
}


#
#####
# This loop loads in all grants that may be of interest
# ***NOTE: Use script below only if grant RData files have already been created!***
#####
#
#

# DATA ARE FILTERED TO LOOK ONLY AT GRANTS. LOANS, DIRECT PAYMENTS, & INSURANCE EXCLUDED
wordsToLookFor <- c("bird")
years = c(2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009)
grants <- data.frame() #creating empty dataframe for use below
for (i in years){
  address = paste("Grants\\RData\\raw-grants", i, ".RData", sep = "")
  grantsA <-readRDS(file = address)
  grantsA$fiscal_year <- paste(i)
  grantsA <- data.frame(lapply(grantsA, as.character), stringsAsFactors = FALSE)
  grantsA <- filter(grantsA, assistance_type_code != "6", assistance_type_code != "7", assistance_type_code != "8",
                    assistance_type_code != "9", assistance_type_code != "10", assistance_type_code != "11")
  grantsA <- select(grantsA, award_id_fain, award_id_uri, award_description, cfda_title, action_date, 
                    awarding_agency_name, awarding_sub_agency_name, recipient_name,
                    recipient_country_name, recipient_state_name, recipient_city_name, 
                    federal_action_obligation, fiscal_year) # selecting only columns of interest
  grantsA <- filter(grantsA, str_detect(str_to_lower(award_description), paste(wordsToLookFor, collapse = "|")))
  grants <- rbind(grants, grantsA)
  rm(grantsA)
  print(nrow(grants))
  print(paste("Year Completed: ", i))
}

saveRDS(grants, "birbGrants.RData")

