
rm(list = ls())
options(scipen = 9999, digits = 6)


library(tidyverse)
library(data.table)
library(RColorBrewer)
library(scales)
library(gganimate)
library(gifski)

# Create Working Directory Short Cuts
local_dir <- "DIRECTORY INFO GOES HERE"
setwd(local_dir)

# loading data
birbs <- readRDS("birbGrants.RData")


# descriptive fun
birbs$federal_action_obligation <- as.numeric(birbs$federal_action_obligation)

#FY sum
birbs %>% 
  group_by(fiscal_year) %>%
  summarise(TotalObligations = sum(federal_action_obligation))

#Agency Sum
birbs %>% 
  group_by(awarding_agency_name) %>%
  summarise(TotalObligations = sum(federal_action_obligation)) %>%
  arrange(desc(TotalObligations))


# Graphing stuff

birbs2 <- birbs

lessThan5 <- c("DEPARTMENT OF COMMERCE (DOC)", "NATIONAL ENDOWMENT FOR THE ARTS (NEA)",
                  "INSTITUTE OF MUSEUM AND LIBRARY SERVICES (IMLS)", "DEPARTMENT OF STATE (DOS)",
                  "AFRICAN DEVELOPMENT FOUNDATION (EOP)", "INTER-AMERICAN FOUNDATION (EOP)", 
                  "CORPORATION FOR NATIONAL AND COMMUNITY SERVICE (CNCS)")

birbs2$awarding_agency_name <- ifelse(birbs2$awarding_agency_name %in% lessThan5, "Smaller Agency Obligations",
                                      birbs2$awarding_agency_name)

birbsFlock <- birbs2 %>% 
  group_by(awarding_agency_name, fiscal_year) %>% 
  summarise(TotalObligations = sum(federal_action_obligation))

#####
# One problem with this dataset is that not all agencies obligate funds each year
# And some obligations are in the negative
# This leads to an area graph with holes in it
# This overly convoluted step addresses that issue by adding 0s for each of the above cases
#####

#creating a dataset that has all FYs
agencies <- birbs2 %>%  
  select(awarding_agency_name) %>% 
  distinct()
agencies$fiscal_year <- "2018"
years <-  c("2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009")
agenciesA <- tibble()
for (i in years) {
  agenciesA <- distinct(select(agencies, -fiscal_year))
  agenciesA$fiscal_year <- i
  agencies <- bind_rows(agencies, agenciesA)
}

birbsFlock <- left_join(agencies, birbsFlock, by = c("awarding_agency_name", "fiscal_year") )

birbsFlock$TotalObligations[is.na(birbsFlock$TotalObligations)] <- 0
birbsFlock$TotalObligations[birbsFlock$TotalObligations < 0] <- 0

# And there you have an overly complicated way to deal with the fact not all agencies have funds for all years
# Also for the fact that there are some negative values

#####
# Cleaning the data to make it prettier
#####
# This line changes all awarding agency names to sentence capitalization and removes acronyms)
birbsFlock$awarding_agency_name <- str_to_title(gsub("\\s*\\([^\\)]+\\)","", 
                                        as.character(birbsFlock$awarding_agency_name)),
                                        locale = "en")
birbsFlock$TotalObligationsMillions <- birbsFlock$TotalObligations/1000000
birbsFlock$fiscal_year <- as.character(birbsFlock$fiscal_year)
p <-  ggplot(birbsFlock, aes(x =fiscal_year, y = TotalObligationsMillions, group = awarding_agency_name)) + 
  geom_area(aes(fill = awarding_agency_name), position = 'stack') +
 scale_fill_brewer(palette = "Set3") +
  labs(title = "Federal Funding is for the Birds",
       subtitle = "US Federal Grants for 'Birds', FY2009 - FY2018",
       caption = "Created by @Shunley42; Source: USAspending.gov",
       fill = "Awarding Agency",
       x = "Fiscal Year",
       y = "Total Obligations (Millions of Dollars)") +
  scale_y_continuous(labels=dollar_format(prefix="$"), limits = c(0, 70),
                     expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  theme(text = element_text(family = "Times", 
                            size = 18,
                            color = "Black"),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

p

t <- p + 
  transition_reveal(as.numeric(fiscal_year)) 

gif <- animate(t, end_pause = 25, width = 800, height = 400, fps = 8)

anim_save("birbFunds.gif", gif)
