## Have total emissions from PM2.5 decreased in the Baltimore City?

## Load the RDS File
NEI <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")

## Need year function to get the year
library(lubridate) 

## Convert the char year to Date with beginning of year
NEI$year <- as.Date(ISOdate(NEI$year, 1, 1))

## Create a subset of data for Baltimore City
sub_24510 <- subset(NEI, NEI$fips == "24510")

## Calculate total emission per year and assign it to pm2.5_24510_year
pm2.5_24510_year <- with(sub_24510, tapply(Emissions,year,sum, na.rm = TRUE))

## Convert to data frame
pm2.5_24510_year <- as.data.frame(pm2.5_24510_year)

## Create a new column Date with the values in Rownames
pm2.5_24510_year$Date <- rownames(pm2.5_24510_year)

## Assign rownames
rownames(pm2.5_24510_year) <- c(1,2,3,4)

## Change the column name 
names(pm2.5_24510_year)[1] <- "Total_Emission" 

## Convert the date from Character to year
pm2.5_24510_year$Date <-as.Date(as.character(pm2.5_24510_year$Date),"%Y-%m-%d")

## Set the margin
par(mar = c(8,8,7,3) + 0.1)
options(scipen = 999) 

# To create a barplot
b2 <- barplot(pm2.5_24510_year$Total_Emission~year(pm2.5_24510_year$Date),
        name.arg = NULL,xlab = "", ylab = "",
        main = "Emission for Baltimore City",ylim = c(0,4000),
        col = year(pm2.5_24510_year$Date),las = 1)

text(b2,pm2.5_24510_year$Total_Emission+1,
     labels = round(pm2.5_24510_year$Total_Emission,2),
     pos = 1,cex = 1, col = "BLACK")

title(xlab = "Year",cex.lab = 1.1,line = 2.5)
title(ylab = "Total pm2.5 Emission",cex.lab = 1.1,line = 4.5)

# To copy the output plot to png file.
dev.copy(png,file = "plot 2.png", width = 500 , height = 500)
dev.off()
