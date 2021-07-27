## Across the United States, how have emissions 
## from coal combustion-related sources changed from 1999–2008?

## Load the RDS File
NEI <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")

## Need different libraries for clean up
library(lubridate) 
library(dplyr)
library(ggplot2)

## Convert the char year to Date with beginning of year
NEI$year <- as.Date(ISOdate(NEI$year, 1, 1))

## Get all SCC code with Fuel Combustion as Coal
SCC_Coal_Comb <- SCC[grep("Fuel Comb.*Coal",SCC$EI.Sector),"SCC"]

## Create a subset of data for Coal Combustion Emission only
sub_Coal_Comb <- subset(NEI, NEI$SCC %in% SCC_Coal_Comb)

## Calculate total emission per year and assign it to pm2.5_year
sub_Coal_Comb_Year <- with(sub_Coal_Comb, tapply(Emissions,year,sum, na.rm = TRUE))

## Convert it to data frame
sub_Coal_Comb_Year <- as.data.frame(sub_Coal_Comb_Year)

## Create a new column Date with the values in Rownames
sub_Coal_Comb_Year$Date <- rownames(sub_Coal_Comb_Year)

## Assign rownames
rownames(sub_Coal_Comb_Year) <- c(1,2,3,4)

## Change the column name 
names(sub_Coal_Comb_Year)[1] <- "Total_Emission" 

## Convert the date from Character to year
sub_Coal_Comb_Year$Date <-as.Date(as.character(sub_Coal_Comb_Year$Date),"%Y-%m-%d")

## To plot the graph, set the margin and background 
par(mar = c(8,8,7,3) + 0.1)
options(scipen = 999) ## changes to not use scientific notation

## Use barplot to plot the graph
b3 <- barplot(sub_Coal_Comb_Year$Total_Emission ~ year(sub_Coal_Comb_Year$Date),
        xlab = "",ylab = "",
        col = year(sub_Coal_Comb_Year$Date),
        main = "Total Emission Per Year from\
Coal Combustion Sources",las = 1)

## To add Y-axis and X-Axis title with some space
title(xlab = "Year",cex.lab = 1.1,line = 2.5)
title(ylab = "Total pm2.5 emission in tons",cex.lab = 1.1,
      line = 4.5)
## To add values at the top of the bar
text(b3,sub_Coal_Comb_Year$Total_Emission,
     labels = round(sub_Coal_Comb_Year$Total_Emission),
     pos = 1, cex = 1, col = "BLACK")

# To copy the output plot to png file.
dev.copy(png,file = "plot 4.png", width = 600 , height = 800)
dev.off()



















