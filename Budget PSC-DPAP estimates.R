#####Budget PSC/DPAP estimates

##Checking BGOV####
setwd("C:/Users/Roth/Documents/Vision/2019/Civilian Services/")



#FY19 - civ####
BGov_estimates_PSC_19 <- read_csv("C:/Users/Roth/Documents/Vision/2019/Civilian Services/19 budget BGov estimate PSC justcivmaybe.csv")
dpap <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")

BGov_estimates_PSC_19$fiscal_year <- 2019

dpap_estimates_19 <- BGov_estimates_PSC_19 %>% 
  left_join(select(dpap, c("PSC Code", DPAP, "P.S")), by = c(PSC = "PSC Code")) %>% 
  mutate(DPAP_category = ifelse(P.S == "Products", "Products", DPAP)) %>% 
  #filter(P.S != "Products") %>% 
  #filter(fiscal_year != 2019) %>% 
  filter(PSC != "UNKN") %>% 
  filter(!is.na(Estimate)) %>% 
  group_by(DPAP_category, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(Estimate))





#FY19####
BGov_estimates_PSC_19 <- read_csv("C:/Users/Roth/Documents/Vision/2019/Civilian Services/19 budget BGov estimate PSC.csv")
dpap <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")

BGov_estimates_PSC_19$fiscal_year <- 2019

dpap_estimates_19 <- BGov_estimates_PSC_19 %>% 
  left_join(select(dpap, c("PSC Code", DPAP, "P.S")), by = c(PSC = "PSC Code")) %>% 
  mutate(DPAP_category = ifelse(P.S == "Products", "Products", DPAP)) %>% 
  #filter(P.S != "Products") %>% 
  #filter(fiscal_year != 2019) %>% 
  filter(PSC != "UNKN") %>% 
  filter(!is.na(Estimate)) %>% 
  group_by(DPAP_category, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(Estimate))

write.csv(dpap_estimates_19, "BGOV FY19 DPAP estimates.csv")

#FY18####
BGov_estimates_PSC_18 <- read_csv("C:/Users/Roth/Documents/Vision/2019/Civilian Services/18 budget BGov estimate PSC.csv")

BGov_estimates_PSC_18$fiscal_year <- 2018

dpap_estimates_18 <- BGov_estimates_PSC_18 %>% 
  left_join(select(dpap, c("PSC Code", DPAP, "P.S")), by = c(PSC = "PSC Code")) %>% 
  mutate(DPAP_category = ifelse(P.S == "Products", "Products", DPAP)) %>% 
  #filter(P.S != "Products") %>% 
  #filter(fiscal_year != 2019) %>% 
  filter(PSC != "UNKN") %>% 
  filter(!is.na(Estimate)) %>% 
  group_by(DPAP_category, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(Estimate))

write.csv(dpap_estimates_18, "BGOV FY18 DPAP estimates.csv")

#FY17####

BGov_estimates_PSC_17 <- read_csv("C:/Users/Roth/Documents/Vision/2019/Civilian Services/17 budget BGov estimate PSC.csv")

BGov_estimates_PSC_17$fiscal_year <- 2017

dpap_estimates_17 <- BGov_estimates_PSC_17 %>% 
  left_join(select(dpap, c("PSC Code", DPAP, "P.S")), by = c(PSC = "PSC Code")) %>% 
  mutate(DPAP_category = ifelse(P.S == "Products", "Products", DPAP)) %>% 
  #filter(P.S != "Products") %>% 
  #filter(fiscal_year != 2019) %>% 
  filter(PSC != "UNKN") %>% 
  filter(!is.na(Estimate)) %>% 
  group_by(DPAP_category, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(Estimate))

write.csv(dpap_estimates_17, "BGOV FY17 DPAP estimates.csv")
