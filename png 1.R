## A plot showing the total PM2.5 emission from all 
## sources for each of the years 1999, 2002, 2005, and 2008.

## Load the RDS File
NEI <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")

## Load the library lubridate for date function
library(lubridate)

## Convert the char year to Date with beginning of year
NEI$year <- as.Date(ISOdate(NEI$year, 1, 1))

## Calculate total emission per year and assign it to pm2.5_year
pm2.5_year <- with(NEI, tapply(Emissions,year,sum, na.rm = TRUE))

## Convert it to data frame
pm2.5_year <- as.data.frame(pm2.5_year)

## Create a new column Date with the values in Rownames
pm2.5_year$Date <- rownames(pm2.5_year)

## Assign rownames
rownames(pm2.5_year) <- c(1,2,3,4)

## Change the column name 
names(pm2.5_year)[1] <- "Total_Emission" 

## Convert the date from Character to year
pm2.5_year$Date <-as.Date(as.character(pm2.5_year$Date),"%Y-%m-%d")

## To plot the graph, set the margin and 
par(mar = c(8,8,7,3) + 0.1)
options(scipen = 999) ## changes to not use scientific notation

## To plot barplot
b1 <- barplot(pm2.5_year$Total_Emission~year(pm2.5_year$Date),
        name.arg = NULL,xlab = "",ylab = "",
        col = year(pm2.5_year$Date),#yaxt = "n",
        main = "Total Emission Per Year",las = 1)

text(b1,(pm2.5_year$Total_Emission),
     labels = round(pm2.5_year$Total_Emission),
          pos = 1, cex = 1, col = "BLACK")
## To add Y-axis and X-Axis title with some space
title(xlab = "Year",cex.lab = 1.1,line = 2.5)
title(ylab = "Total pm2.5 Emission in tons",cex.lab = 1.1,
      line = 4.8)

# To copy the output plot to png file.
dev.copy(png,file = "plot 1.png", width = 500 , height = 500)
dev.off()
