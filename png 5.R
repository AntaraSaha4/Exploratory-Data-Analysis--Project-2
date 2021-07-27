## How have emissions from motor vehicle sources changed from 
## 1999–2008 in Baltimore City?

## Load the RDS File
NEI <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")

## Need different libraries for clean up
library(lubridate) 
library(dplyr)
library(ggplot2)

## Convert the char year to Date with beginning of year
NEI$year <- as.Date(ISOdate(NEI$year, 1, 1))

## Create a subset of data for Baltimore City and Motor Vehicle
sub_24510_MV <- subset(NEI, NEI$fips == "24510" & NEI$type == "ON-ROAD")

## Calculate total emission per year and assign it to pm2.5_year
sub_24510_MV_Year <- with(sub_24510_MV, tapply(Emissions,year,sum, na.rm = TRUE))

## Convert it to data frame
sub_24510_MV_Year <- as.data.frame(sub_24510_MV_Year)

## Create a new column Date with the values in Rownames
sub_24510_MV_Year$Date <- rownames(sub_24510_MV_Year)

## Assign rownames
rownames(sub_24510_MV_Year) <- c(1,2,3,4)

## Change the column name 
names(sub_24510_MV_Year)[1] <- "Total_Emission" 

## Convert the date from Character to year
sub_24510_MV_Year$Date <-as.factor(as.character(sub_24510_MV_Year$Date))

## To plot the graph using ggplot
g2 <- ggplot(sub_24510_MV_Year, aes(Date,Total_Emission,fill = Date))

g2+geom_bar(stat = "identity")+
  labs(title = "Total Emission from Motor Vehicle in Baltimore City")+ 
  labs(x = "YEAR",y = "TOTAL PM2.5 EMISSION")+
  geom_text(aes(label=round(Total_Emission,2)), vjust=1.0, color="black",
            position = position_dodge(0.5), size=4.5)+
    scale_x_discrete(labels=c("1999","2002","2005","2008"))+
  theme(axis.line = element_line(colour = "black", 
        size = 1, linetype = "solid"),legend.position="top",
        plot.title = element_text(size = 20,face = "bold.italic"),
        legend.title = element_text(size = 15,face = "bold"))+
  scale_fill_discrete(name = "Year: ", labels = c("1999", "2002", "2005","2008"))

# To copy the output plot to png file.
dev.copy(png,file = "plot 5.png", width = 600 , height = 600)
dev.off()

