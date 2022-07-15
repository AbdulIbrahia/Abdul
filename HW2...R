df1=data.frame(Name=c('James','Paul','Richards','Marico','Samantha','Ravi','Raghu',
                      'Richards','George','Ema','Samantha','Catherine'),
               State=c('Alaska','California','Texas','North Carolina','California','Texas',
                       'Alaska','Texas','North Carolina','Alaska','California','Texas'),
               Sales=c(14,24,31,12,13,7,9,31,18,16,18,14))

aggregate(df1$Sales, by=list(df1$State), FUN=sum)

library(dplyr)
df1 %>% group_by(State) %>% summarise(sum_sales = sum(Sales)) %>% show()

# The above code does the following: It group the sales by  unique state and does create table with two columns
# The columns includes the names of each state and the su m of thier sales
# The " dplyr" does the same in a sligtly different way. it uses chain method by grouping States and sums thier sales valus.


getwd()
wmData <- read.csv("WorldCupMatches.csv", na.strings = c("", "NA"))

# Question 2a.
dataSize <- dim(wmData) 
print(paste0( "The number of rows in the dataset is ",dataSize[1], ", Number of Cols is ", dataSize[2]))

# Q2. B
print(summary(wmData))
# Q2.C
uniqueLocations <- unique(na.omit(wmData$City))
print(paste("The count of unique locations is: ", length(uniqueLocations)))

#Q2. d
print(paste("The Average Attendence for the matches is: ",mean(wmData$Attendance, na.rm = TRUE)))

# Q2. e
wmData %>% group_by(Home.Team.Name) %>% summarise(Home.Team.Goals = sum(Home.Team.Goals)) %>% show()

#Q2. f
wmData %>% group_by(Year) %>% summarise(Avg.Attendance = mean(Attendance, na.rm = TRUE)) %>% show()
print(median(wmData$Attendance, na.rm = TRUE))


# Question3 a:
mataboliteData <-read.csv("metabolite.csv", na.strings = c("", "NA"))
mataboliteData
AlzheimersCount <-nrow(mataboliteData[mataboliteData$DiseaseName == "Alzheimers", ])
print(paste("Count of patients with Alzheimers is: ", AlzheimersCount))

# Q3.b
print(colSums(is.na(mataboliteData)))

# Q3. c
mataboliteData1 <-mataboliteData[!is.na(mataboliteData$Dopamine), ]
print(mataboliteData1)

# Q3.d

mataboliteData %>%mutate(c4.OH.Pro = ifelse(is.na(c4.OH.Pro), median(c4.OH.Pro, na.rm = T), c4.OH.Pro)) %>% show()

# Q3.e
mataboliteData <-mataboliteData[, which((!colMeans(is.na(mataboliteData))) * 100 >= 25)]
print(mataboliteData)





