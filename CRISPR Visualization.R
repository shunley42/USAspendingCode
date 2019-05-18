
rm(list = ls())
options(scipen = 9999, digits = 6)


library(tidyverse)
library(data.table)
library(RColorBrewer)
library(scales)
library(gganimate)
library(gifski)

# Create Working Directory Short Cuts
local_dir <- "/Users/samhunley/Desktop/Analyses and Fun/CRISPR"
setwd(local_dir)

# loading data
crispr <- readRDS("crisprGrants.RData")


# descriptive fun
crispr$federal_action_obligation <- as.numeric(crispr$federal_action_obligation)

#FY sum
crispr %>% 
  group_by(fiscal_year) %>%
  summarise(TotalObligations = sum(federal_action_obligation))

#Agency Sum
crispr %>% 
  group_by(awarding_agency_name) %>%
  summarise(TotalObligations = sum(federal_action_obligation)) %>%
  arrange(desc(TotalObligations))


# Graphing stuff

crispr2 <- crispr

crispr <- crispr2 %>% 
  group_by(awarding_agency_name, fiscal_year) %>% 
  summarise(TotalObligations = sum(federal_action_obligation))

#####
# One problem with this dataset is that not all agencies obligate funds each year
# And some obligations are in the negative
# This leads to an area graph with holes in it
# This overly convoluted step addresses that issue by adding 0s for each of the above cases
#####

#creating a dataset that has all FYs
agencies <- crispr2 %>%  
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

crispr <- left_join(agencies, crispr, by = c("awarding_agency_name", "fiscal_year") )

crispr$TotalObligations[is.na(crispr$TotalObligations)] <- 0
crispr$TotalObligations[crispr$TotalObligations < 0] <- 0

# And there you have an overly complicated way to deal with the fact not all agencies have funds for all years
# Also for the fact that there are some negative values

#####
# Cleaning the data to make it prettier
#####
# This line changes all awarding agency names to sentence capitalization and removes acronyms)
crispr$awarding_agency_name <- str_to_title(gsub("\\s*\\([^\\)]+\\)","", 
                                        as.character(crispr$awarding_agency_name)),
                                        locale = "en")
crispr$TotalObligationsMillions <- crispr$TotalObligations/1000000
crispr$fiscal_year <- as.character(crispr$fiscal_year)


p <-  ggplot(crispr, aes(x =fiscal_year, y = TotalObligations, group = awarding_agency_name)) 

p <- p +   geom_bar(aes(fill = awarding_agency_name), position = 'stack', stat = "identity") +
 scale_fill_brewer(palette = "Set3") +
  labs(title = "Federal Grants including CRISPR",
       subtitle = "US Federal Grants for with CRISPR in the Description, FY2009 - FY2018",
       caption = "Created by @Shunley42; Source: USAspending.gov",
       fill = "Awarding Agency",
       x = "Fiscal Year",
       y = "Total Obligations (Dollars)") +
  scale_y_continuous(labels=dollar_format(prefix="$"), expand = c(0, 0)) +
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

ggsave("crisprFunding.png", plot = p, device = png(), dpi = 400, width = 400, height = 200, units = c("mm"))

t <- p + 
  transition_reveal(as.numeric(fiscal_year)) 

gif <- animate(t, end_pause = 40, width = 800, height = 400, fps = 7)

anim_save("crisprFunds.gif", gif)
