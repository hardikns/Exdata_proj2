require(ggplot2)
require(dplyr)

# This function fetches the dataset zip file, extracts it in the wd 

download_files <- function() {
    if (!file.exists("./dataset.zip")) {
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                      dest="dataset.zip", method="curl")
    }
    unzip("./dataset.zip")   
}

read_data <- function() {
    if (!file.exists("./summarySCC_PM25.rds")) download_files()
    NEI <- tbl_df(readRDS("summarySCC_PM25.rds"))
    SCC <- tbl_df(readRDS("Source_Classification_Code.rds"))
    return(list(NEI=NEI, SCC=SCC))
}

plot4 <- function (pm25_data) {
    set1 <- pm25_data$SCC %>% filter(grepl("Coal", EI.Sector))  %>% filter(grepl("Comb", Short.Name)) 
    set2 <- pm25_data$SCC %>% filter(grepl("Coal", Short.Name)) %>% filter(grepl("Comb", Short.Name)) 
    coal_scc <- unique(c(as.character(set1$SCC), as.character(set2$SCC)))
    NEI_coal <- pm25_data$NEI %>% filter(SCC %in% coal_scc) %>%
        select(Emissions, year) %>%
        group_by(year) %>% 
        summarise(TotalEmissions=sum(Emissions))
    
    g <- ggplot(NEI_coal, aes(x=year, y=TotalEmissions)) +
        geom_point() + geom_smooth(method="loess") +
        xlab("Years") + ylab("Yearly Emissions (tons)") +
        ggtitle("US - PM2.5 Emissions from Coal")
    
    print(g)
}

if (!"pm25_data" %in% ls())
    pm25_data <- read_data()

png("plot4.png", width=480, height=480, units="px")
plot4(pm25_data)
dev.off()