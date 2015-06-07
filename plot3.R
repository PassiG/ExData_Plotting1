#this functions just creates a structure into the directory for this porpose
#it expects to have a CourseraDataScience folder in the working directory
initEnv <- function(Projectname = "Exploratory Data Analysis: Peer Assessments : Course Project 1")
{
        setwd("~/CourseraDataScience")
        Projectwd <- paste(getwd(),Projectname,sep = "/")
        if(getwd()!= Projectwd)
        {
                if(!file.exists(paste("./",Projectname,sep = "")))
                {
                        dir.create(paste("./",Projectname,sep = ""))
                }
                setwd(Projectwd)
                if(!file.exists("./data"))
                {
                        dir.create("./data")
                }
        }
}

#this function just checks the the data for the plots are avaialable. 
#In case not it will download it and make available.
initData <- function()
{
        if(!file.exists("./data/household_power_consumption.txt"))
        {
                print("No data file")
                if(!file.exists("./data/household_power_consumption.zip"))
                {
                        print("No data zip file")
                        print("Download data zip File")
                        download.file ("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", 
                                       destfile="./data/household_power_consumption.zip",
                                       method = "curl")
                }
                print("Unzip data file")
                unzip("./data/household_power_consumption.zip", exdir ="./data")
        }
        print("Data file is available")
}

# reads the data into a dataframe and returns the dataframe
readData <- function()
{
        dataCalc <- read.table("./data/household_power_consumption.txt", 
                               sep=";",
                               header = TRUE,
                               nrows = 10)
        
        classes <- sapply(dataCalc,
                          class)
        
        data <- read.table("./data/household_power_consumption.txt", 
                           sep=";", 
                           header = TRUE, 
                           colClasses = classes, 
                           na.strings = "?")
        
        print(paste("The read into R is", format(object.size(data), 
                                                 units = "auto")))
        data
}

# cleans the given dataframe for the porpose of this project and converts some data
cleanData <- function(data)
{
        library(lubridate)
        data$DateTime <- dmy_hms(paste(data$Date,data$Time))
        sapply(data, class)
        # Subsetting the full data to obtain the data related to two days: 
        data<- subset(data, (data$Date == "1/2/2007" | data$Date== "2/2/2007"))
        print(paste("The read into R is", format(object.size(data), 
                                                 units = "auto")))
        data
}

#creates the respective plot 
createPlot <- function(data)
{
        png("plot3.png", width = 480, height = 480)
        
        plot(data$DateTime, data$Sub_metering_1, type="l", ylab= "Energy sub metering", xlab="")
        
        lines(data$DateTime, data$Sub_metering_2, type="l", col="red")
        
        lines(data$DateTime, data$Sub_metering_3, type="l", col="blue")
        
        legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, col=c("black", "red", "blue"))
        
        dev.off()
        print("plot3.png created")
        
}

## call of the functions in order to get the plot created by the system. Order is important!
initEnv()
initData()
data <- readData()
data <- cleanData(data)
createPlot(data)
