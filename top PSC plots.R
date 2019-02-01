library(lubridate)
library(RColorBrewer)
library(cowplot)
library(ggrepel)
library(tidyverse)

setwd("~/Analyzed Datasets/Outside Requests/Vision/2018/DOJ")

top_psc <- read_csv("PSC_DOJ_2018.csv")


plot<- ggplot(top_psc, aes(x=0, y = TV, fill = reorder(top_psc$PSC, -top_psc$TV)))+
  #geom_col(color = "black", position = position_stack(reverse =F)) +
  geom_bar(width = .5, stat='identity', color='black', position = position_stack(vjust = .5, reverse = T)) +
  coord_polar(theta = "y") +
  scale_fill_manual(name = "PSC/FSC", 
                    values = brewer.pal(11, "Paired")[c(10,8,6,4,2,9,7,5,3,1)],
                    breaks = reorder(top_psc$PSC, -top_psc$TV),
                    labels = str_wrap(reorder(top_psc$Full_PSC, -top_psc$TV), width = 50)) +
  theme_minimal()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_text(color = "black"),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom", 
        legend.title = element_blank(), legend.text = element_text(size = 18))+
  guides(fill=guide_legend(nrow=10,byrow=TRUE))+
  labs(title = "DOE: Top 10 PSC/FSC by Spending FY17")+
  geom_label_repel(aes(x = .25, y = cumsum(TV) - TV/2, label = paste(PSC, "\n","$", TV, sep = "")),
             size = 7, fontface = "bold", show.legend = FALSE, nudge_x = 0.01, nudge_y = .5)

legend <- get_legend(plot)

plot<- plot + theme(legend.position = "none")

ggdraw(legend)

plot


ggsave(paste("DOJ top PSC by spending legend.jpg"), legend,
       width = 10, height = 5, units = "in")

ggsave(paste("DOJ top PSC by spending plot.jpg"), plot,
       width = 9.5, height = 10, units = "in")

######################################
##by component####

setwd("~/Analyzed Datasets/Outside Requests/Vision/2018/DOJ")

top_psc <- read_csv("DOJ_Comp_PSC_2018.csv")

top_psc <- top_psc %>% 
  filter(component == "FBI")


plot<- ggplot(top_psc, aes(x=0, y = TV, fill = reorder(top_psc$PSC, -top_psc$TV)))+
  #geom_col(color = "black", position = position_stack(reverse =F)) +
  geom_bar(width = .5, stat='identity', color='black', position = position_stack(vjust = .5, reverse = T)) +
  coord_polar(theta = "y") +
  scale_fill_manual(name = "PSC/FSC", 
                    values = brewer.pal(11, "Paired")[c(10,8,6,4,2,9,7,5,3,1)],
                    breaks = reorder(top_psc$PSC, -top_psc$TV),
                    labels = str_wrap(reorder(top_psc$Full_PSC, -top_psc$TV), width = 50)) +
  theme_minimal()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_text(color = "black"),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom", 
        legend.title = element_blank(), legend.text = element_text(size = 18))+
  guides(fill=guide_legend(nrow=10,byrow=TRUE))+
  labs(title = "FBI: Top 10 PSC/FSC by Spending FY17")+
  geom_label_repel(aes(x = .25, y = cumsum(TV) - TV/2, label = paste(PSC, "\n","$", TV, sep = "")),
                   size = 7, fontface = "bold", show.legend = FALSE, nudge_x = 0.01, nudge_y = .5)

legend <- get_legend(plot)

plot<- plot + theme(legend.position = "none")

ggdraw(legend)

plot


ggsave(paste("FBI top PSC by spending legend.jpg"), legend,
       width = 10, height = 5, units = "in")

ggsave(paste("FBI top PSC by spending plot.jpg"), plot,
       width = 9.5, height = 10, units = "in")












##Top PSC
colnames(fil_data)

EPA_PSC <- fil_data[c(1, 4, 6, 22)]

top_n_psc <- EPA_PSC %>%
  dplyr::rename(fiscal_year = "Fiscal Year",
                psc = "Product Service Code (PSC) / Federal Supply Code (FSC)",
                transaction_value = "Transaction Value") %>%
  filter(fiscal_year == 2017) %>%
  group_by(psc) %>%
  dplyr::summarize(grand_total_transaction_value = sum(transaction_value)) %>%
  arrange(desc(grand_total_transaction_value)) %>%
  top_n(10)
top_n_psc_order <- top_n_psc

top_n_psc <- top_n_psc$psc

EPA_PSC$`Product Service Code (PSC) / Federal Supply Code (FSC)`[
  !(EPA_PSC$`Product Service Code (PSC) / Federal Supply Code (FSC)` %in% 
      top_n_psc)] <- "Other"

unique(EPA_PSC$`Product Service Code (PSC) / Federal Supply Code (FSC)`)



###Process Data to get total transaction value by year

epa_toppsc<- EPA_PSC %>%
  rename(fiscal_year = "Fiscal Year", psc = "Product Service Code (PSC) / Federal Supply Code (FSC)",
         transaction_value = "Transaction Value") %>%
  filter(fiscal_year == 2017) %>%
  filter(psc != "Other") %>% 
  group_by(psc) %>%
  dplyr::summarize(sum = sum(transaction_value)) %>% 
  mutate(pos = cumsum(sum) - sum/2) %>% 
  arrange(-sum)

epa_toppsc$pos

epa_toppsc$pscf <- 
  factor(epa_toppsc$psc, 
         levels = c("R499", "F108","F999","C211","R425", "C214", 
                    "R408","C219", "7030","D321"))
epa_toppsc$sumr <-c(214899085,146322070, 130911452, 91842576,
                    78230417,   57622092,  57568088, 55816263,
                    53494448,   46620960)

epa_toppsc$sumf <-
  factor(epa_toppsc$sum,
         levels = epa_toppsc$sum)

# epa_toppsc$sum


psc<- ggplot(epa_toppsc, aes(x=0, y = sum, fill = pscf))+
  #geom_col(color = "black", position = position_stack(reverse =F)) +
  geom_bar(width = .5, stat='identity', color='black', position = position_stack(vjust = .5, reverse = T)) +
  coord_polar(theta = "y") +
  scale_fill_manual(name = "PSC/FSC", 
                    values = brewer.pal(11, "Paired")[c(10,8,6,4,2,9,7,5,3,1)],
                    breaks = c("R499", "F108","F999","C211","R425", "C214", 
                               "R408","C219", "7030","D321"),
                    labels = str_wrap(c("R499: Other Professional Services",
                                        "F108: Hazardous Substance Removal, Cleanup, and Disposal Services and Operational Support",
                                        "F999: Other Environmental Services, Studies, and Analytical Support",
                                        "C211: Architect - Engineer Services (Including Landscaping, Interior Layout, and Designing)",
                                        "R425: Engineering and Technical Services",
                                        "C214: A&E Management Engineering Services",
                                        "R408: Program Management/Support Services",
                                        "C219: Other Architect and Engineering Services",
                                        "7030: Adp Software",
                                        "D321: IT and Telecom - Help Desk "), width = 50)) +
  # labels = c("Other Professional Services",
  #            "Hazardous Substance Removal, Cleanup, and Disposal Services and Operational Support", 
  #            "Other Environmental Services, Studies, and Analytical Support", 
  #            "Architect - Engineer Services (Including Landscaping, Interior Layout, and Designing)", 
  #            "Engineering and Technical Services", 
  #            "A&E Management Engineering Services", 
  #            "Program Management/Support Services", 
  #            "Other Architect and Engineering Services", 
  #            "Adp Software", 
  #            "IT and Telecom - Help Desk ")) +
  theme_minimal()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        #axis.title.y = element_text(color = "black"),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        legend.position = "bottom", 
        legend.title = element_blank(), legend.text = element_text(size = 18))+
  guides(fill=guide_legend(nrow=10,byrow=TRUE))+
  labs(title = "EPA: Top 10 PSC/FSC by Spending FY17")+
  geom_label(aes(x = .25, y = cumsum(sum) - sum/2, label = paste(pscf, "\n","$", sumr, sep = "")),
             size = 5, fontface = "bold", show.legend = FALSE, nudge_x = 0.01, nudge_y = .5)

ggsave(paste("EPA top PSC by spendingv2.jpg"), psc,
       width = 9, height = 13, units = "in")

# ggplot(epa_toppsc, aes(x=factor(1), y = sum, fill = psc))+
#   # geom_bar(stat="identity", color = "black") +
#   geom_bar(stat='identity', color='black') +
#   coord_polar(theta = "y") +
#   scale_fill_manual(name = "PSC/FSC", 
#                     values = brewer.pal(11, "Paired")[c(10,8,6,4,2,9,7,5,3,1)]) +
#   theme_minimal()+
#   theme(axis.title.x = element_blank(),
#         axis.text.x=element_text(colour='black'),
#         axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         panel.border = element_blank(),
#         panel.grid=element_blank(),
#         axis.ticks = element_blank(),
#         plot.title = element_text(size = 14, face = "bold"))+
#   labs(title = "EPA: Top 10 PSC/FSC") 
# 
#   
#   scale_y_discrete(breaks = epa_toppsc$pos, labels = epa_toppsc$sum)
#   
# ggpie <- function (dat, by, totals, sumf) {
#   ggplot(dat, aes_string(x=0, y=totals, fill=by)) +
#     geom_bar(stat='identity', color='black', width = 1) +
#     guides(fill=guide_legend(override.aes=list(colour=NA))) + # removes black borders from legend
#     coord_polar(theta='y') +
#     scale_fill_manual(name = "PSC/FSC", 
#                       values = brewer.pal(11, "Paired")[c(10,8,6,4,2,9,7,5,3,1)]) +
#     theme_minimal()+
#     theme(axis.ticks=element_blank(),
#           axis.text.y=element_blank(),
#           axis.text.x=element_text(colour='black'),
#           axis.title=element_blank(),
#           panel.border = element_blank(),
#           panel.grid=element_blank()) +
#     scale_y_continuous(breaks = cumsum(dat[[totals]]) - dat[[totals]] / 2, labels=paste(dat[[by]], "\n", round(as.numeric(levels(dat[[sumf]])[dat[[sumf]]]), digits = 0), sep = ""))
# 
#   }
# # x = epa_toppsc$psc
# # y = epa_toppsc$sum
# ggpie(epa_toppsc, by = "psc", totals = "sum", sumf = "sumf") 
#   
#    gggeom_text(aes(label = round(total_obligations, digits = 1), y = label_y), size = 3, vjust = 1.5, fontface = "bold")+
#   geom_text(data = subset(data.civdef, Year != 2018), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3, fontface = "bold")+
#   stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
#                geom = "text", vjust = -.5, size = 4, fontface = "bold")+   ####Adds total to top
#   #geom_text(aes(color = Quarter == "Q1", label = round(total_obligations, digits = 1), y = label_y), size = 3, vjust = 1.5) +## white on dark
#   # geom_text(data = subset(data.civdef, Year != 2018), aes(color = Quarter == "Q1",
#   #                   label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3)+ ## white on dark
#   #scale_color_manual(guide = FALSE, values = c("black", "white")) +   ## White on dark
#   # scale_fill_manual(name = NULL, values = c("Q4" = "lightcyan", "Q3" = "lightblue2",
#   #     "Q2" = "skyblue3", "Q1" = "skyblue4")) +
#   #scale_fill_brewer(name = "Quarter", palette = "YlOrRd")+
#   scale_fill_manual(name = "Quarter", values = brewer.pal(9, "YlOrRd")[c(1,3,5,7)])+
#   facet_grid(~FYYear, labeller = label_wrap_gen(20))+
#   labs(x="Fiscal Year", y = "Contract Obligations (in) Billions", title = "PSC Analysis Shows Civilian Agency Contract Obligations \n Underruns Available Funding",
#        subtitle = NULL) +
#   theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
#         axis.ticks.x = element_blank(),
#         strip.text = element_text(face = "bold"), axis.title.x = element_blank())
# 
# contract_type <- fil_data %>% 
#   group_by()


##############################################################################
##Top Contract vehicles####

##2017

EPA_2017 <- read_csv("FY2017 EPA Bgov Data.csv")


EPA_2017_Top <- EPA_2017 %>% 
  select(`Contract Number`, `Transaction Signed Date`, `Transaction Value`, `Contract Type`, `Contracting Title`, `Funding Agency`, `Sum of Contract's Transactions`, `Fiscal Year`) %>% 
  group_by(`Contracting Title`) %>% 
  summarise(sum = sum(`Transaction Value`)/1000000) %>% 
  top_n(10) %>% 
  arrange(desc(sum))

order <- EPA_2017_Top$`Contracting Title`



EPA_2017_Top$"Contracting Order" <- 
  factor(EPA_2017_Top$`Contracting Title`, levels = (order))

EPA_2017_Top$color <- "blue"

ggplot(EPA_2017_Top, aes(x = `Contracting Order`, 
                         y = `sum`, fill = color))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(y = "Total Transaction Value (in millions)", title = "Top EPA Contract Vehicles FY17")+
  theme(axis.text.x = element_text(size=12, angle = 45), axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12, face = "bold"),
        legend.position = "none", title = element_text(size = 28))+
  scale_x_discrete(labels = str_wrap(rev(EPA_2017_Top$`Contracting Order`), width = 40),
                   limits = rev(levels(EPA_2017_Top$`Contracting Order`)))+
  coord_flip()

options(scipen = 999)




##2018

EPA_2017 <- read_csv("FY2018 EPA Bgov Data.csv")


EPA_2017_Top <- EPA_2017 %>% 
  select(`Contract Number`, `Transaction Signed Date`, `Transaction Value`, `Contract Type`, `Contracting Title`, `Funding Agency`, `Sum of Contract's Transactions`, `Fiscal Year`) %>% 
  group_by(`Contracting Title`) %>% 
  summarise(sum = sum(`Transaction Value`)/1000000) %>% 
  top_n(10) %>% 
  arrange(desc(sum))

order <- EPA_2017_Top$`Contracting Title`



EPA_2017_Top$"Contracting Order" <- 
  factor(EPA_2017_Top$`Contracting Title`, levels = (order))

EPA_2017_Top$color <- "blue"

ggplot(EPA_2017_Top, aes(x = `Contracting Order`, 
                         y = `sum`, fill = color))+
  geom_bar(stat = "identity", position = "dodge")+
  labs(y = "Total Transaction Value (in millions)", title = "Top EPA Contract Vehicles FY18")+
  theme(axis.text.x = element_text(size=12, angle = 45), axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(size = 12, face = "bold"),
        legend.position = "none", title = element_text(size = 28))+
  scale_x_discrete(labels = str_wrap(rev(EPA_2017_Top$`Contracting Order`), width = 40),
                   limits = rev(levels(EPA_2017_Top$`Contracting Order`)))+
  coord_flip()