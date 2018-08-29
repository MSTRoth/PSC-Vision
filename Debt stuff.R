setwd("~/Other Requests/Debt/FRED")
data<- read_csv("Total Debt and breakdown by quarter.csv")

install.packages("lubridate")
library(lubridate)
library(tidyverse)
options(scipen = 999)

breakdown <- data %>% 
  select(Year, `Total Debt`, `Intergovernmental Holdings`, `foreign and international  investors`,
         `Debt Held by the Public (minus held by foreign and international  investors)`) %>% 
  dplyr::rename("Public Holdings (minus Foreign Holdings)" = `Debt Held by the Public (minus held by foreign and international  investors)`) %>% 
  gather(key = "Debt_Held_By", value = "amount",
         `Total Debt`:`Public Holdings (minus Foreign Holdings)`) %>% 
  mutate(amount_t = amount/1000000000000) %>% 
  select(-amount) 

breakdown$Year <- mdy(breakdown$Year) 

breakdown_line<- breakdown %>% 
  filter(month(Year) == 7)
  

breakdown$Debt_Held_By <- factor(breakdown$Debt_Held_By, levels = c("Total Debt", "foreign and international  investors", 
                                                                    "Public Holdings (minus Foreign Holdings)",  "Intergovernmental Holdings"), 
                                 ordered = is.ordered(breakdown$Debt_Held_By))

breakdown_bar<- breakdown %>% 
  filter(month(Year) == 7) %>% 
  mutate(Year_y = as.character(year(Year))) %>% 
  filter(Debt_Held_By != "Total Debt") %>% 
  select(-Year) %>% 
  arrange(desc(Debt_Held_By)) %>% 
  group_by(Year_y) %>%
  mutate(label_y = cumsum(amount_t),
         prop = 100*amount_t/sum(amount_t))

levels(breakdown_bar$Debt_Held_By)[2] <- "Foreign and International Investors"


# ggplot(breakdown_line, aes(x=Year, y = amount_t, color = Debt_Held_By, group = Debt_Held_By))+
#   geom_line()+
#   scale_color_brewer(palette = "Set1")+
#   labs(x = "Year", y= "Total Securities Held (in Trillions)", title = "Total Foreign Holdings of U.S. Securities")


ggplot(breakdown_bar, aes(x = Year_y, y = amount_t, fill = Debt_Held_By)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(amount_t, digits = 1), y= label_y, vjust = 1.5), size = 3, fontface = "bold")+
  geom_text(data = breakdown_bar, aes(label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 2.75, fontface = "bold")+
  scale_color_manual(name = "Debt Held By:", values = "Spectral")+
  labs(x = "Year", y= "Total Securities Held (in Trillions)", 
       title = "Total US Debt") + 
  stat_summary(fun.y = sum, aes(label = round(..y.., 1), group = Year_y),
               geom = "text", vjust = -.5, size = 4, fontface = "bold")+   
  scale_x_discrete(breaks  = c(2002:2017))

  
  
  
  scale_fill_manual("Fiscal Year", values = c("2014" = "steelblue1", "2015" = "orangered", "2016" = "grey70", "2017" = "orange", "2018" = "olivedrab3")) +
  facet_grid(noquote(paste("~",facet_var, sep = "")), labeller = label_wrap_gen(20))+
  labs(x="Fiscal Year", y = paste("Contract Obligations (in) ", scale_text, sep = ""), 
       title = paste(company_name, " Contract Obligations by Agency ", FY_range, sep = ""))+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), 
        axis.text.x = element_blank(), axis.ticks.x = element_blank())