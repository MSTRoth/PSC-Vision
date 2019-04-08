library(cowplot)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(ggrepel)


##Top PSC
Top_PSC <- read_csv("HHS PSC.csv") 

plot<- ggplot(Top_PSC, aes(x=0, y = TV, fill = reorder(Top_PSC$PSC, -Top_PSC$TV)))+
  #geom_col(color = "black", position = position_stack(reverse =F)) +
  geom_bar(width = .5, stat='identity', color='black', 
           position = position_stack(vjust = .5, reverse = T)) +
  coord_polar(theta = "y") +
  scale_fill_manual(name = "PSC/FSC", 
                    values = brewer.pal(11, "Paired")[c(10,8,6,4,2,9,7,5,3,1)],
                    breaks = reorder(Top_PSC$PSC, -Top_PSC$TV),
                    labels = str_wrap(Top_PSC$Full_PSC, width = 40)) +
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
  labs(title = "HHS: Top 10 PSC/FSC by Spending FY17")+
  geom_label_repel(aes(x = .25, y = cumsum(TV) - TV/2, label = paste(PSC, "\n","$", TV, sep = "")),
             size = 6, fontface = "bold", show.legend = FALSE, nudge_x = .2, 
             nudge_y = 0,label.padding = .5)

legend<-get_legend(plot)

plot <- plot + theme(legend.position='none')

plot

ggdraw(legend)


ggplot2::ggsave(paste("EPA top PSC by spending FY17 plot.jpg"), plot,
       width = 9, height = 13, units = "in")



ggplot2::ggsave(paste("EPA top PSC by spending FY17 legend.jpg"), legend,
                width = 9, height = 13, units = "in")
