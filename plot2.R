# Have total emissions from PM2.5 decreased in the Baltimore City,
# Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.

plot2 <- function () {
  NEI <- readRDS("summarySCC_PM25.rds")

  baltimore <- subset(NEI, fips == "24510")
  total <- tapply(baltimore$Emissions, baltimore$year, sum)
  
  png("plot2.png")
  
  plot(names(total), total,
     main="Total Emission of PM2.5 for Baltimore",
     ylab="Emission (tons of PM2.5)",
     xlab="Year",
     ylim=range(total))
  
  abline(lm(total ~ as.numeric(names(total))))
  dev.off()
}

