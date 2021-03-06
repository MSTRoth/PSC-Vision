library(lubridate)
library(tidyverse)


##Cleaning Agency Data for Top PSC charts - Vision####

#Location for saving charts
setwd("~/Vision/2019/All top PSC Graphs")
####load all data (From BGOV) for an agency for the most current full year; if larger data set (not BGOV limitations) load for full year)
data <- read_csv("X:/1 Marielle Folder/Data Sets/By Agency/HHS/HHS FY18.csv") ###location
dpap <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")

##Filter if Sub-Agency; if larger dataset, divide into lists by agency and bureau, run lapply for quicker result
top_psc <- data %>% 
  rename(PSC = 'Product Service Code (PSC) / Federal Supply Code (FSC)',
         transaction_value = 'Transaction Value',
         fiscal_year = "Fiscal Year") %>% 
#filter(`Funding Bureau` == "Centers for Medicare and Medicaid Services (CMS)") %>% 
#  filter(`Funding Bureau` == "National Institutes of Health (NIH)") %>%   
  filter(`Funding Bureau` == "Food and Drug Administration (FDA)") %>% 
#  filter(`Funding Bureau`== "Bureau of Alcohol, Tobacco, Firearms and Explosives (ATF)") %>% 
#  filter(`Funding Bureau`== "Drug Enforcement Administration (DEA)") %>% 
#  filter(`Funding Bureau`== "Federal Bureau of Investigation (FBI)") %>% 
#  filter(`Funding Bureau`== "Justice Management Division (JMD)") %>% 
#  filter(`Funding Bureau`== "United States Marshals Service (USMS)") %>% 
#  filter(`Funding Bureau`== "Food and Drug Administration (FDA)") %>%
  #filter(fiscal_year == 2018) %>% 
  select(PSC, transaction_value, fiscal_year) %>% 
  left_join(select(dpap, c('PSC Code', 'PSC Description', 'P.S')), by = c(PSC = 'PSC Code')) %>% 
  filter(P.S == "Service") %>% 
  rename(psc_desc = 'PSC Description') %>%
  group_by(psc_desc, PSC, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC , PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)) %>% 
  arrange(-sum) %>% 
  head(10)

top_psc$fiscal_year <- as.character(top_psc$fiscal_year)


write.csv(top_psc, "FDA FY18 top psc.csv")              ###name new file



