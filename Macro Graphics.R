library(colorspace)
library(readr)
data <- read_csv("~/Market Briefings/Data/Government-Wide data/Civilian and Defense Data by quarter.csv")

#data<-rename(data, civ_def = "Civ/Def", total_obligations = "Contract Obligations (in Billions)")

data$Year = as.character(data$Year)

# data.civdef <- data %>%
#   rename(civ_def = "Civ/Def",
#          total_obligations = "Contract Obligations (in Billions)") %>%
#   #filter(Year!=2019) %>%
#   filter(Year == 2014|Year ==2015| Year == 2016 | Year == 2017 | Year == 2018) %>%
#   group_by(Year, civ_def) %>%
#   mutate(label_y = cumsum(total_obligations))
#
# data.civdef$Year = as.character(data.civdef$Year)


data.civdef_total <- data %>%
  rename(civ_def = "Civ/Def",
         total_obligations = "Contract Obligations (in Billions)") %>%
  filter(civ_def == "Civilian") %>% 
  filter(Year>=2015 & Year<=2019) %>%
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(total_obligations))

data.civdef_total$Year = as.character(data.civdef_total$Year)

data.civdef <- data %>%
  rename(civ_def = "Civ/Def",
         total_obligations = "Contract Obligations (in Billions)") %>%
  #filter(Year!=2019) %>%
  filter(civ_def == "Defense") %>% 
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(total_obligations),
         prop = 100*total_obligations/sum(total_obligations)) %>%
  filter(Year == 2016 | Year == 2017 | Year == 2018) %>%
  mutate(FYYear = paste("FY",Year, sep = ""))

display.brewer.all()



plotyr <- ggplot(data.civdef, aes(x = FYYear, y = total_obligations, fill = factor(Quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(total_obligations, digits = 1), y = label_y), size = 3, vjust = 1.5, fontface = "bold")+
  geom_text(data = subset(data.civdef, Year != 2018), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3, fontface = "bold")+
  stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
               geom = "text", vjust = -.5, size = 4, fontface = "bold")+   ####Adds total to top
  scale_fill_manual(name = "Quarter", values = brewer.pal(9, "YlOrRd")[c(1,3,5,7)])+
  # facet_grid(~FYYear, labeller = label_wrap_gen(20))+
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions", title = "DoD Contract Obligations by Quarter",
       subtitle = NULL) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold"))

install.packages("png")
library(png)

img <- readPNG("C:/Users/Roth/Desktop/logo.png")


ggsave("Defense Contract Obligations by Quarter- FY16-FY18.jpg", plotyr,
       width = 8, height = 6, units = "in")

ggsave("Contract Obligations by Quarter- FY16-FY18.pdf", plotyr,
       width = 13, height = 7, units = "in")

library(svglite)