library(tidyverse)
library(stringr)

CMS_CV <- read_csv("~/Analyzed Datasets/Outside Requests/Vision/2018/HHS/data CMS contract vehicle Usage.csv")

CMS_CV_usage <- CMS_CV %>% 
  gather("Fiscal Year", "Spending to Date ($K)", 3:5)

CMS_CV_usage$`Fiscal Year` <- factor(CMS_CV_usage$`Fiscal Year`, levels = c("2016", "2017", "2018"), ordered = TRUE)

df <- data.frame(CMS_CV_usage, V2 = nchar(CMS_CV_usage$Full_Name))

CMS_CV_usage$Name_reorder <- reorder(CMS_CV_usage$Full_Name, -CMS_CV_usage$`Spending to Date ($K)`)



# ggplot(df, aes(x = Full_Name, y = `Spending to Date ($K)`)) + 
#   geom_bar(stat = "identity", position = "dodge", aes(fill = `Fiscal Year`))
# + xlab(NULL) + aes(stringr::str_wrap(Full_Name, 15), V2)

cms<-ggplot(CMS_CV_usage, aes(x = reorder(Full_Name, -`Total`), y = `Spending to Date ($K)`))+
  geom_bar(stat = "identity", position = "dodge", aes(fill = `Fiscal Year`))+
  geom_text(aes(x = reorder(Full_Name, -`Total`), y = `Spending to Date ($K)`, 
            label = ifelse(`Fiscal Year`==2017, `Contract Vehicle`, "")), vjust = -.5, fontface = "bold")+
  scale_fill_manual(values = c("2016" = "midnightblue", "2017" = "gray77", "2018" = "steelblue3"))+
  scale_x_discrete(labels = str_wrap(CMS_CV_usage$Full_Name, width = 15)) +
  labs(title = "CMS Contract Vehicle Usage FY16-FY18", x="")+
  theme_bw()


ggsave(paste("CMS Contract Vehicle Usage FY16-FY18.jpg"), cms,
       width = 11, height = 6, units = "in")


####################################

CMS_CV <- read_csv("~/Analyzed Datasets/Outside Requests/Vision/2018/HHS/data FDA contract vehicle Usage.csv")

CMS_CV_usage <- CMS_CV %>% 
  gather("Fiscal Year", "Spending to Date ($K)", 3:5)

CMS_CV_usage$`Fiscal Year` <- factor(CMS_CV_usage$`Fiscal Year`, levels = c("2016", "2017", "2018"), ordered = TRUE)

df <- data.frame(CMS_CV_usage, V2 = nchar(CMS_CV_usage$Full_Name))

CMS_CV_usage$Name_reorder <- reorder(CMS_CV_usage$Full_Name, -CMS_CV_usage$`Spending to Date ($K)`)



# ggplot(df, aes(x = Full_Name, y = `Spending to Date ($K)`)) + 
#   geom_bar(stat = "identity", position = "dodge", aes(fill = `Fiscal Year`))
# + xlab(NULL) + aes(stringr::str_wrap(Full_Name, 15), V2)

cms<-ggplot(CMS_CV_usage, aes(x = reorder(Full_Name, -`Total`), y = `Spending to Date ($K)`))+
  geom_bar(stat = "identity", position = "dodge", aes(fill = `Fiscal Year`))+
  geom_text(aes(x = reorder(Full_Name, -`Total`), y = `Spending to Date ($K)`, 
                label = ifelse(`Fiscal Year`==2017, `Contract Vehicle`, "")), vjust = -.5, fontface = "bold")+
  scale_fill_manual(values = c("2016" = "midnightblue", "2017" = "gray77", "2018" = "steelblue3"))+
  scale_x_discrete(labels = str_wrap(CMS_CV_usage$Full_Name, width = 15)) +
  labs(title = "CMS Contract Vehicle Usage FY16-FY18", x="")+
  theme_bw()


ggsave(paste("CMS Contract Vehicle Usage FY16-FY18.jpg"), cms,
       width = 11, height = 6, units = "in")

q <- p +  + xlab(NULL) +
  ylab(yaxis_label)

