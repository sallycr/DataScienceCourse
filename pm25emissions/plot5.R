# 
# motor vehicle sources changes from 1999–20089 to 2008 
# across the United States
#
library(dplyr)
library(ggplot2)

plotMotorVehicleSources <- function() {
    NEI <- readRDS("summarySCC_PM25.rds")
    SCC <- readRDS("Source_Classification_Code.rds")
    
    # get motor vehicle sources SCC codes 
    mvScc <- SCC %>% 
        filter(grepl('motor vehicle|highway veh', Short.Name, ignore.case=TRUE)) %>%
        select(SCC, Short.Name)
    
    # count sources of motor vehicle related in fip 24510
    sources <- NEI %>% 
        filter(SCC %in% mvScc[,1] & fips == "24510") %>%
        select(year, Emissions) %>% 
        group_by(year) %>% 
        summarise_each(funs(sum))
    
    # plotting the coal related total emission changes 
    png('plot5.png')
    barplot(sources$Emissions, names.arg=sources$year, 
            xlab='years', ylab='Total emission', 
            main='Total emissions from motor vehicle in Baltimore City') 
    dev.off()
}

# plotMotorVehicleSources()