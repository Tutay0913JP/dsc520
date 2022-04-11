# Assignment: ASSIGNMENT 4
# Name: Par, Janine
# Date: 2021-04-10

## Load the libraries Required
library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)

options(scipen=5)

theme_set(theme_minimal())

## Set the working directory to the root of your DSC 520 directory
setwd("C:/Users/janin/OneDrive/Documents/R_repo/dsc520/")

## Load the `data/r4ds/housing.xlsx'
housing_df<- read_excel("data/week-6-housing.xlsx")

#Use the apply function on a variable in your dataset
salesprice_mean <- apply(housing_df[,2],MARGIN=2,FUN=median, na.rm=TRUE)

#Create at least 2 new variables
split_year_month <- function(x)
{
  x_year <- format(x,format = "%Y")
  x_month<- format(x,format = "%m")
  df <- data.frame(x_year,x_month)
}


sale_year_month <- split_year_month(housing_df$`Sale Date`)

housing_df <- cbind(housing_df,sale_year_month)


# Use the aggregate function on a variable in your dataset

#aggregate price by sale year
aggregate(`Sale Price` ~ x_year,housing_df,sum)

#mean price by sale year and sale month

aggregate(`Sale Price` ~ x_year + x_month ,housing_df,median)

#aggregate min price by sale year and zip code
aggregate(`Sale Price` ~ x_year + zip5, housing_df,min )

# trying other ways to pass aggregate argument: aggregate min price by sale year and zip code
agg_min  <-  aggregate(housing_df$`Sale Price`,by=list(housing_df$`x_year`,housing_df$zip5),FUN=min, na.rm=TRUE)
agg_min

#Use the plyr function on a variable in your dataset - more specifically, I want to see you split some data, perform a modification to the data, and then bring it back together

  #use ddply to get mean salesprice, sum salesprice, count house sales per year and put in dataframe
housing_year_mean_sum_count <- ddply(housing_df, .variable=c("x_year"), function(x)
{
  mean_salesprice <- mean(x$`Sale Price`)
  sum_salesprice <- sum(x$`Sale Price`)
  count_year <- length(x$`Sale Price`)
  data.frame(mean_salesprice, sum_salesprice, count_year)
})

#merge result to the housing df
housing_df <- merge(housing_df,housing_year_mean_sum_count, by='x_year')

#Check distributions of the data
ggplot(housing_df, aes(`Sale Price`)) + geom_histogram(bins=10) + ggtitle("Housing Census")+ xlab("Sales Price") +  ylab("Frequency")

#Identify if there are any outliers
  #---> Yes, I observe the outliers sits at the right most side of histogram chart. These are data points with sales price within and greater than 3 million data point range of the sales price. 

#Create at least 2 new variables
 #SEE ABOVE LINE 24-35
