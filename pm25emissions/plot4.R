# 
# coal combustion-related sources changes from 1999–20089 to 2008 
# across the United States
#
library(dplyr)
library(ggplot2)

plotCombustionRelatedChanges <- function() {
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    
    # get coal related SCC codes 
    coalScc <- SCC %>% 
        filter(grepl('coal', Short.Name, ignore.case=TRUE)) %>%
        select(SCC, Short.Name)
    
    # count sources of coal related per year 
    sources <- NEI %>% 
        filter(SCC %in% coalScc[,1]) %>%
        select(year, Emissions) %>% 
        group_by(year) %>% 
        summarise_each(funs(sum))
    
    # plotting the coal related total emission changes 
    png('plot4.png')
    barplot(sources$Emissions, names.arg=sources$year, 
            xlab='years', ylab='Total emission', 
            main='Total emissions from coal combustion-related sources') 
    dev.off()
}

# plotCombustionRelatedChanges()