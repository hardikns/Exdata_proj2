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

plot3 <- function (pm25_data) {
    NEI_yearly <- pm25_data$NEI %>% 
                    filter(fips=="24510") %>% 
                    select(Emissions, year, type) %>% 
                    group_by(year, type) %>% 
                    summarise(TotalEmissions=sum(Emissions))
    
    g <- ggplot(NEI_yearly, aes(x=year, y=TotalEmissions)) +
            geom_point() +
            facet_grid(. ~ type) + geom_smooth(method="loess") +
            xlab("Years") + ylab("Yearly Emissions (tons)") +
            ggtitle("PM2.5 Emissions by Type for Baltimore City, Maryland")
    print(g)
}

if (!"pm25_data" %in% ls())
    pm25_data <- read_data()

png("plot3.png", width=800, height=480, units="px")
plot3(pm25_data)
dev.off()