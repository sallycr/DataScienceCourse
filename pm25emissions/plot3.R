# 
# R script to explore the total emissions changes per type 
# in the Baltimore City, Maryland (𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999 to 2008 
#
library(dplyr)
library(ggplot2)

plotTotalPM25ByTypeInBaltimore <- function() {
    NEI <- readRDS("summarySCC_PM25.rds")
    # total emission of each year of Baltimore City, MaryLand
    emission <- NEI %>% 
        filter(fips == "24510") %>%
        select(year, type, Emissions) %>% 
        mutate(type=tolower(type)) %>%
        group_by(year, type) %>% 
        summarise_each(funs(sum))
    
    # plotting the total emission usnig barplot 
    png('plot3.png')
    qplot(year, Emissions, data=emission, group=type, geom="line") + 
        aes(color=type) + geom_point(aes(color = type)) + 
        labs(title="Total PM 2.5 emission per type in Baltimore City, Maryland") + 
        labs(x="year", y="Total PM2.5 emission")
    dev.off()
}

# plotTotalPM25ByTypeInBaltimore()