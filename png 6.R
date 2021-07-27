## Compare emissions from motor vehicle sources in Baltimore City with 
## emissions from motor vehicle sources in Los Angeles County, California.
## Which city has seen greater changes over time in motor vehicle emissions?

## Load the RDS File
NEI <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")

## Need different libraries for clean up
library(lubridate) 
library(dplyr)
library(ggplot2)

## Convert the char year to Date with beginning of year
NEI$year <- as.Date(ISOdate(NEI$year, 1, 1))

## Create a subset of data for Baltimore City
sub_24510 <- subset(NEI, NEI$fips == "24510")

## Create a subset of data for Baltimore City
sub_06037 <- subset(NEI, NEI$fips == "06037")

## Calculate total emission per year for Baltimore
## and assign it to sub_24510_Year
sub_24510_Year <- with(sub_24510, tapply(Emissions,year,sum, na.rm = TRUE))

## Calculate total emission per year for Los Angeles County,California
## and assign it to sub_06037_Year
sub_06037_Year <- with(sub_06037, tapply(Emissions,year,sum, na.rm = TRUE))

## Convert it to data frame
sub_24510_Year <- as.data.frame(sub_24510_Year)
sub_06037_Year <- as.data.frame(sub_06037_Year)

## Create a new column Date with the values in Rownames
sub_24510_Year$Year <- rownames(sub_24510_Year)
sub_06037_Year$Year <- rownames(sub_06037_Year)


## Assign rownames
rownames(sub_24510_Year) <- c(1,2,3,4)
rownames(sub_06037_Year) <- c(1,2,3,4)

## Change the column name 
names(sub_24510_Year)[1] <- "Total_Emission"
names(sub_06037_Year)[1] <- "Total_Emission"

## Calculate the percent change in emission for every year for Baltimore city
len <- nrow(sub_24510_Year)
in_value <- c()
v_24510 <- c()

for (i in 1:len) 
{
  if (is.null(in_value)) 
  {
    in_value <- sub_24510_Year[i,"Total_Emission"]
    v_24510 <- append(v_24510,0)
  }
  else
  {
    v2 <- sub_24510_Year[i,"Total_Emission"]
    diff <- ((v2-in_value)/in_value)*100
    v_24510 <- append(v_24510,diff)
    in_value <- sub_24510_Year[i,"Total_Emission"]
  }
}

## Calculate the percent change in emission for every year for Los Angeles
len <- nrow(sub_06037_Year)
in_value <- c()
v_06037 <- c()

for (i in 1:len) 
{
  if (is.null(in_value)) 
  {
    in_value <- sub_06037_Year[i,"Total_Emission"]
    v_06037 <- append(v_06037,0)
  }
  else
  {
    v2 <- sub_06037_Year[i,"Total_Emission"]
    diff <- ((v2-in_value)/in_value)*100
    v_06037 <- append(v_06037,diff)
    in_value <- sub_06037_Year[i,"Total_Emission"]
  }
}

## Create new column for percent change
sub_24510_Year$change <- v_24510
sub_06037_Year$change <- v_06037

## To create the graph
par(mfcol = c(2,2)) # To create plot with 4 graphs
par(mar = c(4,6,4,2) + 0.1)

## For Baltimore city
with(sub_24510_Year, plot(Year,change,ylim = c(-50,50),
                          col = "steelblue",pch = 19,
                          main = "% change in Emission from Motor Vehicle for Baltimore City",
                          xlab = "", xaxt = "n",
                          ylab = "",las = 1))
pts <- c(1999,2002,2005,2008)
axis(1,at = pts,labels = pts,las = 1)

lines(sub_24510_Year$Year,sub_24510_Year$change,type = "l",col = "steelblue"
      ,lwd = 2)

text(change~Year,
     labels = paste(round(sub_24510_Year$change,2),"%",sep = "") ,
     data = sub_24510_Year, pos = 1, cex = 1.2, col = "red" )

title(xlab = "Year",cex.lab = 1.3,line = 2.0)
title(ylab = "%Change",cex.lab = 1.3,line = 3.0)

b_1 <-barplot(sub_24510_Year$Total_Emission~sub_24510_Year$Year,col = "steelblue",
              xlab = "", ylab = "",
              main = "Total Emission from Motor Vehicle for Baltimore City",las = 1)

title(xlab = "Year",cex.lab = 1.3,line = 2.0)
title(ylab = "Total pm2.5 Emission",cex.lab = 1.3,line = 3.5)

text(b_1,sub_24510_Year$Total_Emission+1,
     labels = round(sub_24510_Year$Total_Emission,2),
     pos = 1,cex = 1.2, col = "BLACK")

# For Los Angeles
with(sub_06037_Year, plot(Year,change,ylim = c(-50,50),
                          col = "#69b3a2",pch = 19,xlab = "",
                          ylab = "",xaxt = "n",
                          main = "% change in Emission from Motor Vehicle for Los Angeles",
                          las = 1))

pts <- c(1999,2002,2005,2008)
axis(1,at = pts,labels = pts,las = 1)

lines(sub_06037_Year$Year,sub_06037_Year$change,type = "l",col = "#69b3a2"
      ,lwd = 2)

text(change~Year,
     labels = paste(round(sub_06037_Year$change,2),"%",sep = "") ,
     data = sub_06037_Year,pos = 1, cex = 1.2, col = "red" )

title(xlab = "Year",cex.lab = 1.3,line = 2.0)
title(ylab = "%Change",cex.lab = 1.3,line = 3.0)

b_2 <- barplot(sub_06037_Year$Total_Emission~sub_06037_Year$Year,col = "#69b3a2",
               xlab = "", ylab = "",
               main = "Total Emission from Motor Vehicle for Los Angeles",
               las = 1)

title(xlab = "Year",cex.lab = 1.3,line = 2.0)
title(ylab = "Total pm2.5 Emission",cex.lab = 1.3,line = 3.8)

text(b_2,sub_06037_Year$Total_Emission+1,
     labels = round(sub_06037_Year$Total_Emission,2),
     pos = 1,cex = 1.2, col = "BLACK")

# To copy the output plot to png file.
dev.copy(png,file = "plot 6.png", width = 1000 , height = 900)
dev.off()





