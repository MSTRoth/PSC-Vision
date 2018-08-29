
setwd("~/Analyzed Datasets/Outside Requests/Vision/2018/USAID")

library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(ggrepel)

USAID_data1418 <- read_csv("USAID_bgov.csv")


fil_data <- USAID_data1418[c(8, 9, 23, 24, 25, 27:30, 52, 53, 63)]


write.csv(fil_data, "USAID pie chart data.csv")

##Top PSC
colnames(fil_data)

USAID_funders <- fil_data %>% 
  dplyr::rename(fiscal_year = "Fiscal Year",
                funder = "Funding Office Level 3",
                transaction_value = "Transaction Value") %>%
  filter(fiscal_year == 2017)

# ifelse(is.na(USAID_funders$funder), 
#        USAID_funders$funders <- USAID_funders$`Funding Bureau`,0)
# 
# ifelse(is.na(USAID_funders$`Funding Office Level 4`)== TRUE, 
#        USAID_funders$funders <- USAID_funders$funder,
#        USAID_funders$funders <- USAID_funders$`Funding Office Level 4`)


USAID_funders$funders <- ifelse(is.na(USAID_funders$funder), 
            USAID_funders$funders <- USAID_funders$`Funding Bureau`, 
       ifelse(is.na(USAID_funders$`Funding Office Level 4`), 
            USAID_funders$funders <- USAID_funders$funder,
            USAID_funders$funders <- USAID_funders$`Funding Office Level 4`))


usaid_fund$funders
top_n_f3 <- USAID_funders %>%
  filter(fiscal_year == 2017) %>% 
  group_by(funders) %>%
  dplyr::summarize(sum = sum(transaction_value)) %>%
  arrange(-sum) %>%
  top_n(10)



# top_n_psc_order <- top_n_psc
# 
# top_n_f3 <- top_n_f3$funders

# EPA_PSC$`Product Service Code (PSC) / Federal Supply Code (FSC)`[
#   !(EPA_PSC$`Product Service Code (PSC) / Federal Supply Code (FSC)` %in% 
#       top_n_psc)] <- "Other"

# unique(EPA_PSC$`Product Service Code (PSC) / Federal Supply Code (FSC)`)



###Process Data to get total transaction value by year

usaid_fund2<- USAID_funders %>%
  filter(fiscal_year == 2017) %>%
  filter(funders %in% top_n_f3$funders) %>% 
  separate(funders, c("funders", "delete"), sep = '\\(') %>% 
  separate(funders, c("funders", "delete2"), sep = '\\/') %>%
  select(`Funding Bureau`,funder, funders, transaction_value)

usaid_fund <- usaid_fund2 %>% 
  group_by(funders) %>%
  dplyr::summarize(sum = sum(transaction_value)) %>% 
  arrange(-sum) %>% 
  mutate(pos = (cumsum(sum) - sum/2)) %>% 
  arrange(pos) %>% 
  # mutate(bureau = ifelse(funders == "Afghanistan Kabul", "Bureau for Middle East",
  #                        ifelse(funders == "Ethiopia USAID-Addis Ababa"|
  #                                 funders == "Tanzania USAID-Dar es Salaam"|
  #                                 funders == "Kenya USAID-Nairobi"| funders == "Pakistan USAID-Islamabad",
  #                               "USAID Office of Procurement", 
  #                               ifelse(funders == "USAID Food for Peace ",
  #                                      "Bureau for Democracy Conflict & Humanitarian Assistance",
  #                                      ifelse(funders == "USAID Europe & Eurasia", "Bureau for Europe & Eurasia",
  #                                             NA)))))
  mutate(bureau = ifelse(funders == "Afghanistan Kabul", "Bureau for Middle East",
                       ifelse(funders == "Ethiopia USAID-Addis Ababa"|
                                funders == "Tanzania USAID-Dar es Salaam"|
                                funders == "Kenya USAID-Nairobi"| funders == "Pakistan USAID-Islamabad",
                              "Bureau for Management",
                              ifelse(funders == "USAID Food for Peace ",
                                     "Bureau for Democracy Conflict & Humanitarian Assistance",
                                     ifelse(funders == "USAID Europe & Eurasia", "Bureau for Europe & Eurasia",
                                            NA)))))


# unique(usaid_fund$funders) 

usaid_fund$funderf <- 
  factor(usaid_fund$funders, 
         levels = usaid_fund$funders)
usaid_fund$sumf <-
  factor(usaid_fund$sum,
         levels = usaid_fund$sum)
usaid_fund$posf <-
  factor(usaid_fund$pos,
         levels = usaid_fund$pos)

# usaid_fund$pos
# usaid_fund$posf
# usaid_fund$sum
# usaid_fund$funderf
#usaid_fund$pos <- (cumsum(c(0,usaid_fund$sum)) + c(usaid_fund$sum/2, .01))[1:nrow(usaid_fund)]

# epa_toppsc$sum

cust<-ggplot(usaid_fund, aes(x=0, y = sum, fill = funderf))+
 geom_col(color = "black", position = position_stack(reverse = T), width = .25) +
  #geom_bar(stat='identity', color='black', position = position_stack()) +
  coord_polar(theta = "y") +
  scale_fill_manual(name = NULL, 
                    values = brewer.pal(11, "Paired")[c(10,8,6,4,2,9,7,5,3,1)], guide = FALSE) +
  theme_minimal()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_text(color = "black"),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 30, face = "bold"))+
  labs(title = "Top 10 USAID Customers by Spending FY17")+
  geom_label_repel(aes(x = .125, y = (pos), label = ifelse(is.na(bureau), (paste(funders, "\n","$", round(sum, digits = 0), sep = "")),
                                                       ifelse(bureau == "Bureau for Democracy Conflict & Humanitarian Assistance",
                                                              paste(funders, "\n","Bureau for Democracy Conflict &","\n",
                                                                    "Humanitarian Assistance","\n", "$", round(sum, digits = 0), sep = ""),
                                                       paste(funders, "\n",usaid_fund$bureau, "\n","$", round(sum, digits = 0), sep = "")))),
                    , colour = ifelse(usaid_fund$funders=="Bureau of Global Health", "white","black"),
                     size = 8, fontface = "bold",show.legend = FALSE, nudge_y = 1.4, nudge_x = .05)
  #scale_y_continuous(breaks = usaid_fund$pos, labels=round(usaid_fund$sum/1000000, digits = 1))

ggsave(paste("USAID top cutomers by spending-all bureaus.jpg"), cust,
       width = 15, height = 15, units = "in")


library(ggrepel)       
       
is.na(usaid_fund$bureau)==TRUE
###Vendor####

top_n_v <- USAID_funders %>%
  filter(fiscal_year == 2017) %>% 
  group_by(`Parent Vendor`) %>%
  dplyr::summarize(sum = sum(transaction_value)) %>%
  arrange(-sum) %>%
  top_n(10)

usaid_vendor<- USAID_funders %>%
  filter(fiscal_year == 2017) %>%
  filter(`Parent Vendor` %in% top_n_v$`Parent Vendor`) %>% 
  select(`Parent Vendor`, transaction_value) %>% 
  group_by(`Parent Vendor`) %>%
  dplyr::summarize(sum = sum(transaction_value)) %>% 
  arrange(-sum) %>% 
  mutate(pos = (cumsum(sum) - sum/2)) %>% 
  arrange(pos) 


usaid_vendor$vendorf <- 
  factor(usaid_vendor$`Parent Vendor`, 
         levels = usaid_vendor$`Parent Vendor`)
usaid_vendor$sumf <-
  factor(usaid_vendor$sum,
         levels = usaid_vendor$sum)
usaid_vendor$posf <-
  factor(usaid_vendor$pos,
         levels = usaid_vendor$pos)

usaid_vendor$pos
usaid_vendor$posf
usaid_vendor$sum
usaid_vendor$vendorf
#usaid_fund$pos <- (cumsum(c(0,usaid_fund$sum)) + c(usaid_fund$sum/2, .01))[1:nrow(usaid_fund)]

# epa_toppsc$sum

vend <- ggplot(usaid_vendor, aes(x=0, y = sum, fill = vendorf))+
  geom_col(width = .5, color = "black", position = position_stack(reverse = T)) +
  #geom_bar(stat='identity', color='black', position = position_stack()) +
  coord_polar(theta = "y") +
  scale_fill_manual(name = "Parent Vendor", 
                    values = brewer.pal(11, "Paired")[c(10,8,6,4,2,9,7,5,3,1)]) +
  theme_minimal()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_text(color = "black"),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 14, face = "bold"))+
  labs(title = "USAID: Top 10 Vendors by Spending FY17")+
  geom_label_repel(aes(x = .4, y = pos, label = paste(vendorf, "\n","$", round(sum, digits = 0), sep = "")),
                   size = 4, fontface = "bold", show.legend = FALSE, nudge_x = 0.01, nudge_y = 1)


ggsave(paste("USAID top vendors by spending.jpg"), vend,
       width = 17, height = 11, units = "in")


