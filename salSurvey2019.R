######
#
#
# Analyzing 2019 Salary Survey from Ask A Manager
#
# Source Data: https://docs.google.com/spreadsheets/d/1rGCKXIKt-7l5gX06NAwO3pjqEHh-oPXtB8ihkp0vGWo/edit#gid=382484678
#####



#####
# Loading Packages and Setting Directories
#####
library(tidyverse)
library(data.table)


# Create Working Directory Short Cuts
local_dir <- "/Users/samhunley/Desktop"
setwd(local_dir)


#####
# Loading Salaray data
#####
sals2019 <- fread("salSurvey2019.csv")

# Finding job titles with "data" in them
dataSals2019 <- sals2019 %>%
  filter(str_detect(str_to_lower(`Job title`), "data"))

# what sort of mess are we dealing with....
test <- unique(dataSals2019$`Job title`)
test # 245 unique job titles. That's better than I was expecting.
# still need to pull out the managers, analysts, and scientists...

analyst2019 <- dataSals2019 %>%
  filter(str_detect(str_to_lower(`Job title`), "analyst")) %>%
  filter(!str_detect(str_to_lower(`Job title`), "manager")) %>%
  filter(!str_detect(str_to_lower(`Job title`), "senior")) %>%
  filter(!str_detect(str_to_lower(`Job title`), "lead"))#175 of these

analyst2019$`What is your annual salary?` <- as.numeric(str_replace_all(analyst2019$`What is your annual salary?`, "\\$|[:alpha:]|,|[:space:]", ""))

df$payment_2 = as.numeric(gsub("\\$", "", df$payment))
unique(analyst2019$`What is your annual salary?`)

scientist2019 <- dataSals2019 %>%
  filter(str_detect(str_to_lower(`Job title`), "scientist")) %>%
  filter(!str_detect(str_to_lower(`Job title`), "manager")) %>%
  filter(!str_detect(str_to_lower(`Job title`), "senior")) %>%
  filter(!str_detect(str_to_lower(`Job title`), "lead")) # 92 of these

scientist2019$`What is your annual salary?` <- as.numeric(str_replace_all(scientist2019$`What is your annual salary?`, "\\$|[:alpha:]|,|[:space:]", ""))


# I wanted to look at senior & lead folks as well as managers, but based on how these sliced,
# doesn't look like there's enough data to really do that


#####
# Descriptive Stats
#####
analyst2019 %>%
  group_by(`How many years of post-college professional work experience do you have?`) %>%
  summarise(avgSal = mean(as.numeric(`What is your annual salary?`)),
            medSal = median(as.numeric(`What is your annual salary?`)),
            sdSal = sd(as.numeric(`What is your annual salary?`)),
            maxSal = max(as.numeric(`What is your annual salary?`)),
            minSal = min(as.numeric(`What is your annual salary?`)))


scientist2019 %>%
  group_by(`How many years of post-college professional work experience do you have?`) %>%
  summarise(avgSal = mean(as.numeric(`What is your annual salary?`)),
            medSal = median(as.numeric(`What is your annual salary?`)),
            sdSal = sd(as.numeric(`What is your annual salary?`)),
            maxSal = max(as.numeric(`What is your annual salary?`)),
            minSal = min(as.numeric(`What is your annual salary?`)))

# The above has a lot that I don't care about
# let's look at early career folks

earlyAnalyst2019 <- analyst2019 %>%
  filter(`How many years of post-college professional work experience do you have?` == "1 year or less" |
         `How many years of post-college professional work experience do you have?` == "2 - 4 years")

earlyScientist2019 <- scientist2019 %>%
  filter(`How many years of post-college professional work experience do you have?` == "1 year or less" |
           `How many years of post-college professional work experience do you have?` == "2 - 4 years")


# trying again

earlyAnalyst2019 %>%
  summarise(avgSal = mean(as.numeric(`What is your annual salary?`)),
            medSal = median(as.numeric(`What is your annual salary?`)),
            sdSal = sd(as.numeric(`What is your annual salary?`)),
            maxSal = max(as.numeric(`What is your annual salary?`)),
            minSal = min(as.numeric(`What is your annual salary?`)))


earlyScientist2019 %>%
  summarise(avgSal = mean(as.numeric(`What is your annual salary?`)),
            medSal = median(as.numeric(`What is your annual salary?`)),
            sdSal = sd(as.numeric(`What is your annual salary?`)),
            maxSal = max(as.numeric(`What is your annual salary?`)),
            minSal = min(as.numeric(`What is your annual salary?`)))

earlyAnalyst2019$analystOrScientist <- "Analyst"
earlyScientist2019$analystOrScientist <- "Scientist"

earlyDataFolks2019 <- bind_rows(earlyAnalyst2019, earlyScientist2019)

write.csv(earlyDataFolks2019, "earlyDataFolks2019.csv", row.names = FALSE)









