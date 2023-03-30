# Across the United States, how have emissions from coal combustion-related 
# sources changed from 1999–2008?


library(dplyr)
library(tidyr)
library(ggplot2)

plot4 <- function () {
  NEI <- readRDS("summarySCC_PM25.rds")
  SCC <- readRDS("Source_Classification_Code.rds")
  
  scc_values <- SCC %>% 
    filter(grepl("[cC]oal", Short.Name), grepl("[cC]ombustion", Short.Name)) %>%
    select(SCC) %>% 
    pull
  
  df <- subset(NEI, SCC %in% scc_values) %>%
    filter(year %in% c(1999, 2008)) %>% 
    group_by(year) %>% 
    summarise(emission=sum(Emissions))
  
  segment_df <- pivot_wider(df, names_from = year, values_from = emission)
  
  p <- ggplot(df, aes(x=year, y=emission)) +
    geom_point() +
    geom_segment(aes(x=1999, y=`1999`, xend=2008, yend=`2008`), data=segment_df) +
    xlab("Year") + ylab("Emission (tons of PM2.5)") + ggtitle("Change of PM2.5 Emission from coal combustion-related sources")
  
  ggsave("plot4.png", p)
}

