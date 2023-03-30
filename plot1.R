# Have total emissions from PM2.5 decreased in the United States from 1999 to
# 2008? Using the base plotting system, make a plot showing the total PM2.5
# emission from all sources for each of the years 1999, 2002, 2005, and 2008.

plot1 <- function() {
  NEI <- readRDS("summarySCC_PM25.rds")

  total <- tapply(NEI$Emissions, NEI$year, sum)
  df <- data.frame(year=as.integer(names(total)), emission=total)
  
  png("plot1.png")
  
  plot(df, pch=20, cex=2, 
       main="Total Emission of PM2.5 per Year",
       xlab="Year",
       ylab="Emission (tons of PM2.5)")
  
  dev.off()
}
