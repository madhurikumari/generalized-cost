setwd("/Users/mkumari/Downloads")
library(ggplot2)
library(gridExtra)
library(reshape2)
library(scales)
library(zoom)
library(cowplot)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(forcats)
# Generalized Cost
df <- read.csv("MK_FromMarshall - GeneralizedCost-3.csv", header = TRUE)

stacked_bar <- function(df){
  df_1clean <- select(df, -Category)
  df_1Melted <- melt(df_1clean, id.var = c("Year", "year1"))
  colourCount = length(unique(df_1Melted$variable))
  p <- ggplot(df_1Melted, aes(x = Year, y = value, fill = forcats::fct_rev(variable))) + 
    ylim(0,800)+
    geom_bar(stat = "identity") + facet_grid(~year1)+
    scale_fill_manual(labels =c("Incentive", "Green PR", "Model Availability", "Refueling Inconvenience",
                                "Uncertainty", "Maintenance Cost", "Fuel Cost", "Capital Cost"),
                      values = colorRampPalette(brewer.pal(9, "Set1"))(colourCount))+
    labs( y="Generalized Cost (thousand $)") + 
    theme_bw()+ 
    theme( plot.title = element_text(hjust = 0.5, size=16),
           axis.title.x=element_blank(),
           axis.text.x = element_text(angle = 45, hjust = 1, size=13), 
           axis.title.y = element_text(size=15),
           axis.text.y = element_text(size=13),
           legend.title=element_blank(),
           legend.text = element_text( size=13))
  return(p)
}

#Long Haul
df_1 <- rename(filter(df, Category == "Long Haul"))
p1 <- stacked_bar(df_1) + labs(title = "Long Haul Generalized Cost: BAU")

#Short Haul
df_2 <- rename(filter(df, Category == "Short Haul"))
p2 <- stacked_bar(df_2) + labs(title = "Short Haul Generalized Cost: BAU") +
  theme(  axis.text.x = element_text(angle = 45, hjust = 1, size=13), 
          axis.text.y = element_blank(),
          axis.title.y=element_blank())

#Arrange Plots
ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="right")

