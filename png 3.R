## Of the four types of sources indicated by the type 
## (point, non-point, on-road, non-road) variable, which of these four sources
## have seen decreases in emissions from 1999–2008 for Baltimore City?
## Which have seen increases in emissions from 1999–2008?

## Load the RDS File
NEI <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")

library(lubridate) ## Need year function to get the year
library(dplyr)
library(ggplot2)

## Convert the char year to Date with beginning of year
NEI$year <- as.Date(ISOdate(NEI$year, 1, 1))

## Create a subset of data for Baltimore City
sub_24510 <- subset(NEI, NEI$fips == "24510")

## Convert the column "type" into factor
sub_24510$type <- as.factor(sub_24510$type)


## get total emission for four type of sources for every year
sub_t_24510 <- sub_24510 %>%
                group_by(type,year) %>%
                summarise(total_emission = round(sum(Emissions),2))

## convert the sub_t_24510 to data frame
sub_t_24510 <- as.data.frame(sub_t_24510)

## convert the year column into factor type
sub_t_24510$year <- as.factor(sub_t_24510$year)

## To plot graph using ggplot
g <- ggplot(sub_t_24510, aes(x = year,y = total_emission, fill = year))
g+
  geom_bar(stat = "identity",position=position_dodge())+
  facet_grid(.~type)+
  labs(title = "Total Emission by different Source Type for Baltimore City")+ 
  labs(x = "YEAR",y = "TOTAL PM2.5 EMISSION")+
  geom_text(aes(label=total_emission), vjust=1.0, color="black",
            position = position_dodge(0.5), size=4.5)+
  scale_x_discrete(labels=c("1999","2002","2005","2008"))+
  theme(axis.line = element_line(colour = "black", 
      size = 1, linetype = "solid"),legend.position="top",
      plot.title = element_text(size = 20,face = "bold.italic"),
      legend.title = element_text(size = 15,face = "bold"))+
  scale_fill_discrete(name = "Year: ", labels = c("1999", "2002", "2005","2008"))
  
# To copy the output plot to png file.
dev.copy(png,file = "plot 3.png", width = 1000 , height = 800)
dev.off()
