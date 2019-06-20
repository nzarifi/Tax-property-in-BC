#https://data.vancouver.ca/datacatalogue/propertyTax.htm

setwd("C:\\Users\\Niloofar-Z\\Desktop\\MetroCo\\R\\Project")
getwd()


data <- read.csv("property_tax_report_2018.csv",header = TRUE, sep = ",",na.strings="")

head(data)
data[c('PID')]
names(data)
dim(data)   #209649   28
sum(!complete.cases(data))  #206989 rows have at least one missing value
sum(complete.cases(data))  #2660
data[!complete.cases(data),] #omitted #206954 rows prints out #35 rows
sum(is.na(data$BLOCK)) #check single col #122159 missing!
sum(is.na(data$PREVIOUS_IMPROVEMENT_VALUE))  #7099 missing!
##double checked sum of missing values with jupyter,same result
sum(is.na(data)) #680100 data points missing
is.na.data.frame(data) #converts to binary dataset
## all 0 and 1 
sapply(data$BLOCK, function(x) sum(length(which(is.na(x))))) 
sapply(data$PREVIOUS_IMPROVEMENT_VALUE, function(x) sum(length(which(is.na(x))))) 
###-----------------a vector of all missing values-------------
na_count <- sapply(data, function(x) sum(length(which(is.na(x)))))
na_count #all NA count
####---------------------------------------------------

cleaned_data=na.omit(data)
head(cleaned_data)
dim(cleaned_data)  #as we know we will so much information (only 2660 rows) with cleaned file
#save cleaned and all missing data to csv files 
NA_data=data[!complete.cases(data),]
write.csv(cleaned_data,"output_clean.csv", row.names = FALSE)
write.csv(NA_data,"output_nan.csv",row.names=FALSE)

sum(is.na(data$PREVIOUS_IMPROVEMENT_VALUE))

#introduction plot
vector1 <- rep("NAN",99)
vector2 <- c("Ideal")
vector <-c(vector1,vector2)
dd=table(vector)
barplot(dd,border=TRUE,cex.lab=1.5,ylim=c(0,100) )

