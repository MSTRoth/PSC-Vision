###Vision: Civilian Services
##Civ Agencies by DPAP
install.packages("ggthemes")
install.packages("reshape2")
install.packages("scales")
library(scales)
library(reshape2)
library(tidyverse)
library(RColorBrewer)
library(ggthemes)
library(grid)

data <- read.csv("C:/Users/Roth/Documents/Analyzed Datasets/Outside Requests/Vision/2018/Civilian Services/Civilian agencies DPAP Categories.csv")
colnames(data)
setwd("~/Analyzed Datasets/Outside Requests/Vision/2018/Civilian Services")

data.DPAP.agencies <- data %>%
  dplyr::rename("DPAP_Category" = DPAP.Category)%>% 
  gather("fiscal_year","amount",4:13) %>% 
 # filter(Agency == "Department of Homeland Security (DHS)") %>%
  #filter(Agency == "Department of Commerce (DOC)") %>%
  # filter(Agency == "Department of Energy (DOE)") %>%
   #filter(Agency == "Department of Justice (DOJ)") %>%
   #filter(Agency == "Department of State (DOS)") %>%
   #filter(Agency == "Department of Transportation (DOT)") %>%
   #filter(Agency == "Department of Health and Human Services (HHS)") %>%
   #filter(Agency == "National Aeronautics and Space Administration (NASA)") %>%
   #filter(Agency == "Department of Treasury (TREAS)") %>%
   #filter(Agency == "Agency for International Development (USAID)") %>%
   #filter(Agency == "Department of Agriculture (USDA)") %>%              
   filter(Agency == "Department of Veterans Affairs (VA)") %>%
  dplyr::mutate(total_transaction_value = amount/1000000000) 
  # filter(fiscal_year == "FY14" |fiscal_year == "FY15"|fiscal_year == "FY16"|fiscal_year == "FY17") %>% 

data.DPAP.agencies <- droplevels(data.DPAP.agencies)
plot <- ggplot(data.DPAP.agencies, aes(fill = fiscal_year,
                                x = fiscal_year,
                                y = total_transaction_value))+
  geom_bar(stat = "identity", position = position_dodge()) +
  #geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
  scale_fill_manual("Fiscal Year", values = c("FY09" = "darkred",
                                              "FY10" = "gold",
                                              "FY11" = "royalblue1",
                                              "FY12" = "darkgreen",
                                              "FY13" = "gray38",
                                              "FY14" = "steelblue1", 
                                              "FY15" = "orangered", 
                                              "FY16" = "grey70", 
                                              "FY17" = "orange", 
                                              "FY18" = "olivedrab3")) +
  labs(y = "Contract Obligations (in Billions)", title = unique(droplevels(data.DPAP.agencies$Agency)), x = NULL)+
  facet_grid(~DPAP_Category, labeller = label_wrap_gen(10), scales = "free")+
  theme(plot.title = element_text(hjust = 0.5, size = 36, face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(colour = NA, fill = NA),
       # panel.border = element_rect(colour = NA, fill = NA),
        axis.title = element_text(face = "bold",size = rel(1)),
        axis.title.y = element_text(angle=90,vjust =2),
         axis.line = element_line(colour="#bcbcbc"),
         axis.ticks.y = element_line(),
         panel.grid.major = element_line(colour="#f0f0f0"),
         panel.grid.minor = element_line(colour="#f0f0f0"),
        # legend.key = element_rect(colour = NA),
        # legend.position = "bottom",
        # legend.direction = "horizontal",
        # legend.key.size= unit(0.2, "cm"),
        # legend.margin = unit(0, "cm"),
        legend.title = element_text(face="bold"),
         #plot.margin=unit(c(10,5,5,5),"lines"),
         strip.background=element_rect(colour="#bcbcbc",fill="#f0f0f0"),
         strip.text = element_text(face="bold"),
        panel.border = element_rect(color = "#bcbcbc", fill = NA, size = 1)
  )+
  guides(fill = guide_legend(reverse = TRUE))

ggsave(paste(unique(droplevels(data.DPAP.agencies$Agency))," DPAP FY09-FY18.jpg", sep = ""), plot,
         width = 15, height = 8, units = "in") 

# ################all FY one color####
# data.DPAP.agencies <- data %>%
#   dplyr::rename("DPAP_Category" = DPAP.Category, 
#                 "FY18" = FY18..as.of.8.27.)%>% 
#   gather("fiscal_year","amount",4:13) %>% 
#   filter(Agency == "Department of Homeland Security (DHS)") %>%
#   #filter(Agency == "Department of Commerce (DOC)") %>%
#   # filter(Agency == "Department of Energy (DOE)") %>%
#   # filter(Agency == "Department of Justice (DOJ)") %>%
#   #filter(Agency == "Department of State (DOS)") %>%
#   #filter(Agency == "Department of Transportation (DOT)") %>%
#   #filter(Agency == "Department of Health and Human Services (HHS)") %>%
#   # filter(Agency == "National Aeronautics and Space Administration (NASA)") %>%
#   #filter(Agency == "Department of Treasury (TREAS)") %>%
#   #filter(Agency == "Agency for International Development (USAID)") %>%
#   #filter(Agency == "Department of Agriculture (USDA)") %>%              
#   #filter(Agency == "Department of Veterans Affairs (VA)") %>%
# dplyr::mutate(total_transaction_value = amount/1000000000) 
# # filter(fiscal_year == "FY14" |fiscal_year == "FY15"|fiscal_year == "FY16"|fiscal_year == "FY17") %>% 
# 
# 
# plot <- ggplot(data.DPAP.agencies, aes(fill = fiscal_year,
#                                        x = fiscal_year,
#                                        y = total_transaction_value))+
#   geom_bar(stat = "identity", position = position_dodge()) +
#   #geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
#   scale_fill_manual("Fiscal Year", values = c("FY09" = "steelblue1",
#                                               "FY10" = "steelblue1",
#                                               "FY11" = "steelblue1",
#                                               "FY12" = "steelblue1",
#                                               "FY13" = "steelblue1",
#                                               "FY14" = "steelblue1", 
#                                               "FY15" = "steelblue1", 
#                                               "FY16" = "steelblue1", 
#                                               "FY17" = "steelblue1", 
#                                               "FY18" = "steelblue1")) +
#   labs(y = "Contract Obligations (in Billions)", title = unique(droplevels(data.DPAP.agencies$Agency)), x = NULL)+
#   facet_grid(~DPAP_Category, labeller = label_wrap_gen(10), scales = "free")+
#   theme(plot.title = element_text(hjust = 0.5, size = 36, face = "bold"),
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         panel.background = element_blank(),
#         plot.background = element_rect(colour = NA, fill = NA),
#         # panel.border = element_rect(colour = NA, fill = NA),
#         axis.title = element_text(face = "bold",size = rel(1)),
#         axis.title.y = element_text(angle=90,vjust =2),
#         axis.line = element_line(colour="#bcbcbc"),
#         axis.ticks.y = element_line(),
#         panel.grid.major = element_line(colour="#f0f0f0"),
#         panel.grid.minor = element_line(colour="#f0f0f0"),
#         # legend.key = element_rect(colour = NA),
#         # legend.position = "bottom",
#         # legend.direction = "horizontal",
#         # legend.key.size= unit(0.2, "cm"),
#         # legend.margin = unit(0, "cm"),
#         legend.title = element_text(face="bold"),
#         #plot.margin=unit(c(10,5,5,5),"lines"),
#         strip.background=element_rect(colour="#bcbcbc",fill="#f0f0f0"),
#         strip.text = element_text(face="bold"),
#         panel.border = element_rect(color = "#bcbcbc", fill = NA, size = 1))+
#   guides(fill = FALSE)
# 
# ggsave(paste(unique(droplevels(data.DPAP.agencies$Agency))," DPAP FY09-FY18 - all blue.jpg", sep = ""), plot,
#        width = 15, height = 8, units = "in") 

#####color spectrum FY###########

data <- read_csv("C:/Users/Roth/Documents/Analyzed Datasets/Outside Requests/Vision/2018/Civilian Services/Civilian agencies DPAP Categories.csv")
colnames(data)
setwd("~/Market Briefings/Data/Government-Wide data/FY14-FY18 Agency Charts")
setwd("~/Analyzed Datasets/Outside Requests/Vision/2018/Civilian Services")

data$`DPAP Category` <- factor(data$`DPAP Category`, levels=c("Construction Services", 
                                                              "Electronic & Communication Services", 
                                                              "Equipment Related Services",
                                                              "Facility Related Services",
                                                              "Knowledge Based Services",
                                                              "Logistics Management Services",
                                                              "Medical Services",
                                                              "Research & Development",
                                                              "Transportation Services",
                                                              "Products"))

data.DPAP.agencies <- data %>%
  dplyr::rename("DPAP_Category" = `DPAP Category`)%>% 
  gather("fiscal_year","amount",4:13) %>%
  filter(DPAP_Category != "Total") %>% 
  # filter(Agency == "Department of Homeland Security (DHS)") %>%
  # filter(Agency == "Department of Commerce (DOC)") %>%
  # filter(Agency == "Department of Energy (DOE)") %>%
  # filter(Agency == "Department of Justice (DOJ)") %>%
  # filter(Agency == "Department of State (DOS)") %>%
  # filter(Agency == "Department of Transportation (DOT)") %>%
  # filter(Agency == "Environmental Protection Agency (EPA)") %>%
  # filter(Agency == "Department of Health and Human Services (HHS)") %>%
  # filter(Agency == "National Aeronautics and Space Administration (NASA)") %>%
  # filter(Agency == "Department of Treasury (TREAS)") %>%
  # filter(Agency == "Agency for International Development (USAID)") %>%
  # filter(Agency == "Department of Agriculture (USDA)") %>%
  filter(Agency == "Department of Veterans Affairs (VA)") %>%
dplyr::mutate(total_transaction_value = amount/1000000000) %>% 
filter(fiscal_year == "FY14" |fiscal_year == "FY15"|fiscal_year == "FY16"|fiscal_year == "FY17"|fiscal_year == "FY18")

cc <- scales::seq_gradient_pal("azure3", "steelblue3", "Lab")(seq(0,1,length.out=5))

#data.DPAP.agencies$DPAP_Category <- droplevels(data.DPAP.agencies$DPAP_Category)

#data.DPAP.agencies <- melt(data.DPAP.agencies,id.vars=c("Abbrev","Agency","DPAP_Category","fiscal_year","amount","total_transaction_value"))



plot <- ggplot(data.DPAP.agencies, aes(fill = fiscal_year, x = fiscal_year,
                                       y = total_transaction_value))+
  geom_bar(stat = "identity", position = position_dodge()) +
  #geom_text(aes(label = round(total_transaction_value, digits = 1), vjust = 1.5), size = 3)+
  scale_fill_manual(values = cc) +
  labs(y = "Contract Obligations (in Billions)", title = unique(data.DPAP.agencies$Agency), 
       subtitle = "FY14-FY18", x = NULL)+
  facet_grid(~DPAP_Category, labeller = label_wrap_gen(10), scales = "free")+
  theme(plot.title = element_text(hjust = 0.5, size = 36, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 28, face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(colour = NA, fill = NA),
        # panel.border = element_rect(colour = NA, fill = NA),
        axis.title = element_text(face = "bold",size = 14),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.line = element_line(colour="#bcbcbc"),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_line(colour="#f0f0f0"),
        # legend.key = element_rect(colour = NA),
        # legend.position = "bottom",
        # legend.direction = "horizontal",
        # legend.key.size= unit(0.2, "cm"),
        # legend.margin = unit(0, "cm"),
        legend.title = element_text(face="bold"),
        #plot.margin=unit(c(10,5,5,5),"lines"),
        strip.background=element_rect(colour="#bcbcbc",fill="#f0f0f0"),
        strip.text.x = element_text(size = 13, face="bold"),
        panel.border = element_rect(color = "#bcbcbc", fill = NA, size = 1))+
  guides(fill = FALSE)


ggsave(paste(unique(data.DPAP.agencies$Abbrev)," DPAP FY14-FY18 - blue spectrum.jpg", sep = ""), plot,
       width = 15, height = 7, units = "in") 

#-----------------------------------------------------------------------------

####tree map for total civilian contracts####
#data from USASpending
install.packages("grDevices")
library(grDevices)
install.packages("treemap")
library(treemap)

data <- read.csv("C:/Users/Roth/Documents/Market Briefings/Data/Government-Wide data/DPAP (services and total) Data - Civilian.csv")

colnames(data)
data17 <- data %>%
  rename(fiscal_year = "Fiscal.Year", in_billions = "X..billions") %>% 
  filter(fiscal_year == "FY18") %>% 
  filter(DPAP.Category != "Products") %>% 
  mutate(roundbill = paste("$",(round(in_billions, digits = 1)), "B", sep = ""))


jpeg(filename="treemap_civ_FY18v3.jpeg",width=1500, height=1000)

# ggplot(data17, aes(area = Spend, fill = in_billions, 
#                    label = round(in_billions, digits = 1), subgroup = DPAP.Category)) +
#   geom_treemap()+
#   geom_treemap_text(colour = "white", place = "top",
#                     grow = F)+
#   geom_treemap_subgroup_text(colour = "white", place = "bottom", grow = F, min.size = 0)

palette <- c("#1455B2", 
             "#5887CD", 
             "#8CA9D5", 
             "#5EA909", 
             "#86E377",
             "#88D822",
             "#EDDC11",
             "#EDC511", 
             "#C4A82B")
palette <- c("#86E377", 
             "#88D822",
             "#1455B2",
             "#5887CD", 
             "#EDDC11",
             "#8CA9D5",  
             "#EDC511",       
             "#5EA909",
             "#C4A82B")
data17$DPAP.Category = factor(data17$DPAP.Category, levels = c("Facility Related Services", 
                  "Knowledge Based Services", "Electronic & Communication Services",
                  "Research and Development", "Medical Services", "Construction Services",
                  "Transportation Services", "Equipment Related Services", 
                  "Logistics Management Services"))

treemap(data17, index = c("roundbill", "DPAP.Category"), vSize = "Spend", 
        vColor = "roundbill", type = "index", title = "Civilian Services Contracts in FY18",
        fontsize.title = 24, fontcolor.labels = c("black","black"), fontsize.labels = c(60,40), 
        fontface.labels = c("bold","bold"), bg.labels = 0, palette = rev(palette), 
        # mapping = c("Facility Related Services", 
        #                           "Knowledge Based Services", "Electronic & Communication Services",
        #                               "Research and Development", "Medical Services", "Construction Services",
        #                                          "Transportation Services", "Equipment Related Services", 
        #                                    "Logistics Management Services"),
        align.labels = list(c("center","top"),c("center", "bottom")), lowerbound.cex.labels = .6,
        force.print.labels = F, position.legend = "none", aspRatio = 1.9)
dev.off()        
  