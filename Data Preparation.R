
install.packages("tidyverse")
install.packages("readxl")
install.packages("naniar")

#Obtain and read grain data
library(readxl)
grain <- read_excel("C:\\Users\\rache\\Downloads\\GRAIN---Land-grab-deals---Jan-2012-2.xlsx", sheet = 1)
grain <- read_excel("C:\\Users\\rache\\Downloads\\GRAIN---Land-grab-deals---Jan-2012-2.xlsx", sheet = 2)
grain

head(grain)

#Identified and visualized missing values
library(naniar)
vis_miss(grain)
gg_miss_upset(grain)
#Variable "projected investment" has a very high no. of missing values.  

#Cleaned data
#renamed variables data
summary(grain)
grain <- grain %>%
         rename("projected_investment" = "Projected investment",
                "Status_ofdeal" = "Status of deal")

#Dropped projected_investment.Has too many missing values that cannot be omitted.
grain <- grain %>% select(-projected_investment)

#Dropped missing na row in sector
library(dplyr)
grain = filter(grain, Sector != "NA",Base != "NA",Year != "NA")

#Visualize the status of deal vs year
library(ggplot2)
ggplot(grain, 
       aes(x = Year, 
           fill = Status_ofdeal)) + 
  geom_bar(position = "stack")
  
#replace year values based summary variable report
grain$Year[grain$Year==41000] <- 2009
grain$Year[grain$Year==42675] <- 2011

#Correct spelling in status of deal
grain$Status_ofdeal[grain$Status_ofdeal=="Don"] <- "Done"

#plot of landgrabbed count with show of sector
library(ggplot2)
grain$Sector[grain$Sector=="Agribus"] <- "Agribusiness"

ggplot(grain, 
       aes(x = Landgrabbed,fill = Sector)) + 
      geom_bar(position = "stack") +
  scale_x_discrete(guide = guide_axis(n.dodge = 10))





#Histogram for Hectares
qplot(grain$Hectares,
      geom="histogram",
      binwidth = 5,  
      main = "Histogram for Hectares", 
      xlab = "Hectares",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2))
      







