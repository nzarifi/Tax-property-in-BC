setwd("C:\\Users\\Niloofar-Z\\Desktop\\MetroCo\\R\\Project")
getwd()

##Read data and attribute names
data <- read.csv("property_tax_report_2018.csv",header = TRUE, sep = ",",na.strings="")
data
names(data)
#[1] "PID"                        "LEGAL_TYPE"                 "FOLIO"                     
#[4] "LAND_COORDINATE"            "ZONE_NAME"                  "ZONE_CATEGORY"             
#[7] "LOT"                        "BLOCK"                      "PLAN"                      
#[10] "DISTRICT_LOT"               "FROM_CIVIC_NUMBER"          "TO_CIVIC_NUMBER"           
#[13] "STREET_NAME"                "PROPERTY_POSTAL_CODE"       "NARRATIVE_LEGAL_LINE1"     
#[16] "NARRATIVE_LEGAL_LINE2"      "NARRATIVE_LEGAL_LINE3"      "NARRATIVE_LEGAL_LINE4"     
#[19] "NARRATIVE_LEGAL_LINE5"      "CURRENT_LAND_VALUE"         "CURRENT_IMPROVEMENT_VALUE" 
#[22] "TAX_ASSESSMENT_YEAR"        "PREVIOUS_LAND_VALUE"        "PREVIOUS_IMPROVEMENT_VALUE"
#[25] "YEAR_BUILT"                 "BIG_IMPROVEMENT_YEAR"       "TAX_LEVY"                  
#[28] "NEIGHBOURHOOD_CODE"        

##------------------------------------------
#Number of Houses built between 1800 and 2018

sum(is.na(data$PROPERTY_POSTAL_CODE))  #5087 NA is data$PROPERTY_POSTAL_CODE
postal=data[complete.cases(data$PROPERTY_POSTAL_CODE),]
dim(postal)
library(plyr)
table(postal$YEAR_BUILT) #starts from 1800 and 2018 with #41 data points is not complete
data.frame(postal$YEAR_BUILT) # YEAR_BUILT has some missing
ddd=aggregate(cbind(count = PID) ~ YEAR_BUILT, 
          data = postal, 
          FUN = function(x){NROW(x)})

opar <- par() #keep the original par default
x=ddd$YEAR_BUILT
y=ddd$count
jpeg("year.jpg",width=950,height=650)
par(mar=c(5,5,5,5))
plot(x=x,y=y,pch=1,col="blue",cex=3,lwd=3,cex.axis=1.5,
     xlab="Year",ylab="Number of Properties Built",cex.lab=2)

title(main="Number of Houses built between 1800 and 2018", sub=NULL,cex.main=1.5)

dev.off()
#------------------------------------------------------------------
#let's replot between 1900 to 2018
postal=data[complete.cases(data$PROPERTY_POSTAL_CODE),]
attach(postal)
postal <- subset(postal, 1900<=YEAR_BUILT & YEAR_BUILT< 2019)
dd=aggregate(cbind(count = PID) ~ YEAR_BUILT, 
             data = postal, 
             FUN = function(x){NROW(x)})

x=dd$YEAR_BUILT
y=dd$count
jpeg("year.jpg",width=950,height=650)
par(mar=c(5,5,5,5))
plot(x=x,y=y,pch="*",col="blue",cex=3,lwd=3,cex.axis=1.5,
     xlab="Year",ylab="Number of Properties Built",cex.lab=2)

title(main="Number of Houses built between 1900 and 2018", sub=NULL,cex.main=1.5)

dev.off()
detach(postal)
###____________________________
#same plot with bar chart
counts <- table(postal$YEAR_BUILT)
par(mar=c(6,6,4,4))
jpeg("bar_year.jpg",width=950,height=650)

barplot(counts,border=TRUE,main="Number of Houses built between 1900 and 2018",xlab="Year",
        ylab="Number of Properties Built",cex.lab=1.5 )
dev.off()
#--------------------------------------------
####zone categories
#Top zone categories over the total number of properties
zone=data[complete.cases(data$ZONE_CATEGORY),]
dim(zone)
dd=aggregate(cbind(count = PID) ~ ZONE_CATEGORY, 
             data = zone, 
             FUN = function(x){NROW(x)})
total_properties=sum(dd$count) #205471 
#percentage
(dd$count)/total_properties
# 0.087905349 0.332660083 0.012099031 0.007752919 0.016060661 0.000803033 0.134310925
# 0.333886534 0.074521465
#some values from 3to6 (3,4,5,6) like
#0.012099031 0.007752919 0.016060661 0.000803033, are smaller than 2%, add them up to 'Others'
sum_values=2486+1593+3300+165 #count values


lab = as.vector(dd$ZONE_CATEGORY[c(1,2,7,8,9)]) #take out (3,4,5,6) and add 'Others'
lab=c(lab,'Others')
length(lab)

slices <- as.vector(dd$count[c(1,2,7,8,9)])
slices=c(slices,sum_values)


pct <- round(slices/sum(slices)*100,digit=2)
lab <- paste(lab, pct) # add percents to labels
lab <- paste(lab,"%",sep="") # ad % to labels
pie(slices,labels = lab,radius = 1, col=rainbow(length(lab)),
    main="Top zone categories with percentage\nshare in the total number of properties")

dev.off()
#--------------------------------------------------------------
#comparing "current land or improvment value"
#??????????????doesnt work for multiple cols

landcurrent=data[complete.cases(data['CURRENT_LAND_VALUE','CURRENT_IMPROVEMENT_VALUE']),]

max(data$CURRENT_LAND_VALUE)


#in order to compare "current land or improvment value" we need four conditions
#remove NA of two cols and remove values smaller than 1!!
cc=data[!is.na(data$CURRENT_LAND_VALUE) &  !is.na(data$CURRENT_IMPROVEMENT_VALUE)&
          data$CURRENT_IMPROVEMENT_VALUE>2 & data$CURRENT_LAND_VALUE >2 ,]
dim(cc)

#layout(matrix(c(1,2), nrow = 1), widths = c(0.6, 0.4))
par(mar = c(5, 5, 5, 5))
#scale is 1 million dollars 
CURRENT_LAND=round((cc$CURRENT_LAND_VALUE)/1000000,digit=2)
max(CURRENT_LAND)
length(CURRENT_LAND)
x=c(1:length(CURRENT_LAND))
plot(x=x,y=(CURRENT_LAND),type="b",col="blue",xlab="Land Number",
     ylab="(CURRENT_LAND_VALUE)x $1000000", main="land value in 2018")
abline(h=1,col="red",lwd=2) #abline stands in $1 million 

#------------IMPROVMENT_LAND_VALUE
IMPROVEMENT_LAND=round((cc$CURRENT_IMPROVEMENT_VALUE)/1000000,digit=2)
max(IMPROVEMENT_LAND)
x=c(1:length(IMPROVEMENT_LAND))
plot(x=x,y=(IMPROVEMENT_LAND),type="b",col="red",xlab="Land Number",
     ylab="(IMPROVEMENT_LAND_VALUE)x$1000000", main="improvment value in 2018")
#-----------------------------------------------------------------------------------
##### Rate of growth for Current and previous year LAND VALUE 
## let's remove land value below $1000
summary(data$CURRENT_LAND_VALUE)
summary(data$PREVIOUS_LAND_VALUE)
sort(data$CURRENT_LAND_VALUE)
sort(data$PREVIOUS_LAND_VALUE)

com=data[!is.na(data$CURRENT_LAND_VALUE) &  !is.na(data$PREVIOUS_LAND_VALUE)&
          data$PREVIOUS_LAND_VALUE>1000 & data$CURRENT_LAND_VALUE >1000 ,]
com <- com[c('CURRENT_LAND_VALUE','PREVIOUS_LAND_VALUE')]
dim(com)
summary(com$CURRENT_LAND_VALUE)
sort(com$PREVIOUS_LAND_VALUE)  
rate_of_growth=round(100*(com$CURRENT_LAND_VALUE-com$PREVIOUS_LAND_VALUE)
                     /com$PREVIOUS_LAND_VALUE,digit=2)

#check max and min
sort(rate_of_growth)
which(rate_of_growth==-99.67) #63351
com[63351,]  #(22000-6619000)/6619000 it has almost lost all its value 
sort(rate_of_growth,decreasing=TRUE)
#31272.44  2634.55  1141.26  1068.40  1050.84 top list of profit% !!
# I assume they are outliers and will remove profit more than 1000%!

rate_of_growth <- rate_of_growth[rate_of_growth <1000]



windows()
x=c(1:length(rate_of_growth))
y=rate_of_growth
plot(x,y,pch="*",col="blue",cex=1.5,lwd=1.5,cex.axis=1.5,
     xlab="Number of Land",ylab="Rate of growth (%)",cex.lab=1.5)

title(main="Rate of growth between 2017 and 2018", sub=NULL,cex.main=1.5)
abline(h=-100,col="red",lwd=1)
dev.off()

##--------------------------------------------------------
##median and mean of current land value

com=data[!is.na(data$CURRENT_LAND_VALUE) &  !is.na(data$PREVIOUS_LAND_VALUE)&
           data$PREVIOUS_LAND_VALUE>1000 & data$CURRENT_LAND_VALUE >1000 &
           !is.na(data$YEAR_BUILT),]
com <- com[c('YEAR_BUILT','CURRENT_LAND_VALUE','PREVIOUS_LAND_VALUE')]
dim(com)  #200889      3

#from previous question (rate of growth) we learnt there are some outliers 
#let's get rid of it first
#1. run rate of growth again 2. find the location of outliers in data.frame 3. remove them
rate_of_growth=round(100*(com$CURRENT_LAND_VALUE-com$PREVIOUS_LAND_VALUE)
                     /com$PREVIOUS_LAND_VALUE,digit=2)

sort(rate_of_growth, decreasing=TRUE)

match(c(31272.44,2634.55 ,1141.26,  1068.40,  1050.84),rate_of_growth)
#now remove the following rows from com '99965  10205 155039 137464  52105'
com <- com[-c(99965,  10205, 155039, 137464,  52105),]
dim(com) # 5 rows were removed


by(com$CURRENT_LAND_VALUE,list(com$YEAR_BUILT),summary)
# find out the average or median of the land value over 100 years

mean_2018=by(com$CURRENT_LAND_VALUE,list(com$YEAR_BUILT),mean)
median_2018=by(com$CURRENT_LAND_VALUE,list(com$YEAR_BUILT),median)
class(median_2018) #class is "by" !
dim(median_2018)
median_2018[1] #show two values year and median
#nice try but how to plot this?


windows()
plot(x=names(median_2018),y=(median_2018/1000000),
     xlab="Year",ylab="Land value,($1 million)",cex=3,lwd=3,cex.axis=1.5,cex.lab=1.5)



points(x=names(mean_2018),y=(mean_2018/1000000),pch="*", col="red",cex=3,lwd=3,cex.axis=1.5)
legend(x=1955,y=20, c("Median","Mean"),
       col=c("black","red"),lty=c(1,1))
#more investigation on outliers 
which.max(median_2018)
sort(median_2018,decreasing=TRUE) #check 1899     1886     1893
check <- com[(com$YEAR_BUILT==1899 | com$YEAR_BUILT==1886 | com$YEAR_BUILT==1893),]
#these years only have One data point, no surprise if they are outliers 

total_count=aggregate(cbind(count = CURRENT_LAND_VALUE) ~ YEAR_BUILT, 
             data = com, 
             FUN = function(x){NROW(x)})

total_count #  counting all data points for each year


#--------------------------------------------------------------------
#check the neighbourhood with current value and median 
#more units smaller median :)
length(data$NEIGHBOURHOOD_CODE) # does not have NA value
com=data[!is.na(data$CURRENT_LAND_VALUE) &  !is.na(data$NEIGHBOURHOOD_CODE)&
         data$CURRENT_LAND_VALUE >1000 & !is.na(data$YEAR_BUILT),]
com <- com[c('YEAR_BUILT','CURRENT_LAND_VALUE','NEIGHBOURHOOD_CODE')]
dim(com) #203361      3

table(com$NEIGHBOURHOOD_CODE)


counts <- table(com$NEIGHBOURHOOD_CODE)
windows()
barplot(counts,border=TRUE,main="Neighbourhood code for all years",
        xlab="#NEIGHBOURHOOD_CODE",ylab="#count",cex.lab=1.5,ylim=c(0,20000) )
#axis(side = 1, at = seq(0, 30, by = 1), labels = TRUE, tcl = -1) #this is external, not good here
#barplot document, nothing for unit   
     
median_neighbour=by(com$CURRENT_LAND_VALUE,list(com$NEIGHBOURHOOD_CODE),median)

windows()
barplot(median_neighbour/1000000,border=TRUE,main="Median of land value for each neighbourhood",xlab="#NEIGHBOURHOOD_CODE",
        ylab="Median of land value ($1 million)",cex.lab=1.5,col="red",ylim=c(0,6) )

#---------------------------------------------------------------
##let's focus on year >=2010 
##scatter plot with,'CURRENT_LAND_VALUE','NEIGHBOURHOOD_CODE','ZONE_CATEGORY
com=data[!is.na(data$CURRENT_LAND_VALUE) &  !is.na(data$NEIGHBOURHOOD_CODE)&
           data$CURRENT_LAND_VALUE >1000 & !is.na(data$YEAR_BUILT) & data$YEAR_BUILT >=2010 &
           !is.na(data$ZONE_CATEGORY),]
dim(com) #26490    28
##sounds like still have some missig data
com <- com[c('YEAR_BUILT','CURRENT_LAND_VALUE','NEIGHBOURHOOD_CODE','ZONE_CATEGORY')]
com=na.omit(com) #or from the beginning, add !is.na(data$ZONE_CATEGORY)
#which zone which #neighbour_code has the most zone code
qplot(com$NEIGHBOURHOOD_CODE, com$CURRENT_LAND_VALUE, data = com, color = factor(com$ZONE_CATEGORY),
      geom=c("point"),xlab = "NEIGHBOURHOOD_CODE", ylab = "CURRENT_LAND_VALUE")
dev.off()

#------------------------------------
#Normal boxplot
qplot(ZONE_CATEGORY, CURRENT_LAND_VALUE/1000000, data = com, 
      geom=c("boxplot"), 
      stackdir = "center", binaxis = "y",)

p<-ggplot(com,aes(ZONE_CATEGORY,CURRENT_LAND_VALUE/1000000))
p + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#--------------------------------------------------
#try linear regression
x=ddd$YEAR_BUILT
y=ddd$count
cor(x, y)

regressor <- lm(y~x, data = ddd)
summary(regressor)
# Show attributes of linModel
attributes(regressor) 
regressor$coefficients
regressor$model
# Visualizing the training set results
ggplot() +
  geom_point(aes(x, y),
             colour = 'red') +
  geom_line(aes(x, y = predict(regressor, newdata = ddd)),
            colour = 'blue') +
  ggtitle('Linear Regression') +
  xlab('Year') +
  ylab('Number of Properties Built')



######or


predict(regressor, data.frame(x = 3))

plot(y ~ x, data = dd,
     xlab = "Year",
     ylab = "Number of Properties Built",
     main = "Scatter Plot"
)

abline(regressor, col = "red", lwd = 3)
#__________________________#smooth plot
qplot(YEAR_BUILT, count, data = ddd, geom = c("point", "smooth"))

##------------------------------------------------
##try polynomial

library(polynom)
library(ggplot2)


x=ddd$YEAR_BUILT
y=ddd$count
my.formula <- y ~ poly(x, 2, raw = TRUE)
p <- ggplot(ddd, aes(x, y)) 
p <- p + geom_point(alpha=2/10, shape=21, fill="blue", colour="black", size=5)
p <- p + geom_smooth(method = "lm", se = FALSE, 
                     formula = my.formula, 
                     colour = "red")

m <- lm(my.formula, ddd)
my.eq <- as.character(signif(as.polynomial(coef(m)), 3))
label.text <- paste(gsub("x", "~italic(x)", my.eq, fixed = TRUE),
                    paste("italic(R)^2",format(summary(m)$r.squared, digits = 2), 
                          sep = "~`=`~"),
                    sep = "~~~~")

p + annotate(geom = "text", x = 1900, y = 8000, label = label.text, 
             family = "serif", hjust = 0, parse = TRUE, size = 4) 

#-------------------------------------------------------------------------------------
#how about smaller classification of ZONE_CATEGORY?

com=data[!is.na(data$CURRENT_LAND_VALUE) &  !is.na(data$NEIGHBOURHOOD_CODE)&
           data$CURRENT_LAND_VALUE >1000 & !is.na(data$YEAR_BUILT) & data$YEAR_BUILT >=2010 &
           !is.na(data$ZONE_CATEGORY),]
com <- com[c('YEAR_BUILT','CURRENT_LAND_VALUE','NEIGHBOURHOOD_CODE','ZONE_CATEGORY')]
dim(com)
str(com)
head(com)
is.data.frame(com)
#to replace value I have factor issue

levels(com$ZONE_CATEGORY)
str(com)  #ZONE_CATEGORY is factor

com$ZONE_CATEGORY <- as.character(com$ZONE_CATEGORY)
levels(com$ZONE_CATEGORY)
str(com)

##the problem was family group that had factor error. 
#Commercial and Industerial were just fine
#converted ZONE_CODE to charactor
com[com=="One Family Dwelling"] <- "Family"
com[com=="Two Family Dwelling"] <- "Family"
com[com=="Multiple Family Dwelling"]<- "Family"

com[com=="Comprehensive Development"]="Commercial"
com[com=="Light Industrial"]="Industrial"



groups=table(com$ZONE_CATEGORY,com$NEIGHBOURHOOD_CODE) # create new groups
xtabs(~ com$ZONE_CATEGORY + com$NEIGHBOURHOOD_CODE, data=com)

barplot(groups,border=TRUE,main="Number of Lands between 2010 and 2018",xlab="NEIGHBOURHOOD_CODE",
        ylab="#count",cex.lab=1.5,col=rainbow(5),ylim=c(0,6000) )
legend(x=18,y=5000, c("Commercial","Family","Historic Area","Industrial","Limited Agricultural"),
       col=rainbow(5),lty=c(1,1),lwd=3)

##use rainbow(5) instead of col=colors()[c(4,89,12,6,2)]
##----------------------------------------
##boxplot for rate of growth and zone category

com=data[!is.na(data$CURRENT_LAND_VALUE) &  !is.na(data$PREVIOUS_LAND_VALUE)&
           data$PREVIOUS_LAND_VALUE>1000 & data$CURRENT_LAND_VALUE >1000 &
           !is.na(data$ZONE_CATEGORY),]
com <- com[c('CURRENT_LAND_VALUE','PREVIOUS_LAND_VALUE','ZONE_CATEGORY')]
dim(com) #201070      3


#same as before remove those rate_of_growth that are more than 1000%  
rate_of_growth=round(100*(com$CURRENT_LAND_VALUE-com$PREVIOUS_LAND_VALUE)
                     /com$PREVIOUS_LAND_VALUE,digit=2)
is.vector(rate_of_growth) #add to data.frame
com$rate_of_growth <- rate_of_growth
colnames(com)
dim(com) #201070      4
com <- com[com$rate_of_growth <1000,]
dim(com) #201065      4   five outliers were removed

p <- qplot(com$ZONE_CATEGORY,com$rate_of_growth , data = com, fill = ZONE_CATEGORY,
      geom=c("boxplot"),xlab = "ZONE_CATEGORY" , ylab = "Rate_of_growth(%)")
p + theme(axis.text.x = element_text(angle = 90,hjust = 1)) +
  geom_hline(yintercept=50, linetype="solid", color = "red", size=1)

#let's try new zone_category
com$ZONE_CATEGORY <- as.character(com$ZONE_CATEGORY)
levels(com$ZONE_CATEGORY)
str(com)

#converted ZONE_CODE to charactor
com[com=="One Family Dwelling"] <- "Family"
com[com=="Two Family Dwelling"] <- "Family"
com[com=="Multiple Family Dwelling"]<- "Family"

com[com=="Comprehensive Development"]="Commercial"
com[com=="Light Industrial"]="Industrial"

p <- qplot(com$ZONE_CATEGORY,com$rate_of_growth , data = com, fill = ZONE_CATEGORY,
           geom=c("boxplot"),xlab = "ZONE_CATEGORY" , ylab = "Rate_of_growth(%)")
p + theme(axis.text.x = element_text(angle = 90,hjust = 1)) +  geom_hline(yintercept=50, linetype="solid", color = "red", size=1)
  
  
  
  
dev.off()
