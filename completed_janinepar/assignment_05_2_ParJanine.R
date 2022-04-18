# Assignment: ASSIGNMENT 5
# Name: Par, Janine
# Date: 2021-04-16

## Load the libraries Required
library(readxl)
library(plyr)
library(dplyr)
library(magrittr)
library(purrr)
library(stringr)

options(scipen=5)

theme_set(theme_minimal())

## Set the working directory to the root of your DSC 520 directory
setwd("C:/Users/janin/OneDrive/Documents/R_repo/dsc520/")

## Load the `data/r4ds/housing.xlsx'
housing_df<- read_excel("data/week-6-housing.xlsx")


#Using the dplyr package, use the 6 different operations to analyze/transform the data - GroupBy, Summarize, Mutate, Filter, Select, and Arrange - Remember this isn't just modifying data, you are learning about your data also - so play around and start to understand your dataset in more detail

#Mutate
housing_df %<>% mutate(sale_year=format(housing_df$`Sale Date`,format = "%Y"))

housing_df %<>% mutate(sale_month=format(housing_df$`Sale Date`,format = "%m"))

housing_df %<>% mutate(m = mean(`Sale Price`),     # calculates the mean price
               sd = sd(`Sale Price`),      # calculates standard deviation
               med = median(`Sale Price`))


#Select 
housing_df %>% select(sale_year, `Sale Price`) %>%  mutate(m = mean(`Sale Price`),     # calculates the mean price
                                                   sd = sd(`Sale Price`),      # calculates standard deviation
                                                   med = median(`Sale Price`))


housing_df %>% select(starts_with("sale")) 

#Filter 

    #get distinct year
sale_distict_year <- unique(housing_df[c("sale_year")])

    #Convert into vector
sale_distict_year <- sale_distict_year$sale_year

    #Create separate dataframe per year

housing_list_by_year <- list()

for (year in sale_distict_year)
{
  df_name <- paste("housing_sale",year, sep="_")  #name of each dataframe
  temp <-  filter(housing_df, sale_year==year) #filter for the distinct year
  housing_list_by_year[[year]]  <- temp #List created with dataframe per year
  assign(x=df_name, value=temp) #data frame created for housing sale per year
}


#Arrange  #summarize # group by 

for (i in seq_along(housing_list_by_year))
{
  df_name <- paste("housing_list_sum",names(housing_list_by_year[i]),sep="_")
  temp <- housing_list_by_year[[i]]  %>% group_by(zip5) %>% summarise(m = mean(`Sale Price`),     # calculates the mean price
                                                         sd = sd(`Sale Price`),      # calculates standard deviation
                                                         med = median(`Sale Price`))  %>% arrange(zip5)  # arrange by zipcode
  assign(x=df_name, value=temp) #create dataframe with summarize fields
}


#Using the purrr package - perform 2 functions on your dataset.  You could use zip_n, keep, discard, compact, etc.

keep(housing_sale_2006$`Sale Price`, function(x) x > 100000)

compact(housing_df$postalctyn)

keep(housing_df$`Sale Price`, function(x) x > 100000)

#Split a string, then concatenate the results back together and 
#Use the cbind and rbind function on your dataset


sale_date_char  <- as.character(housing_df$`Sale Date`) #convert to character string

sale_date_char <- str_split(sale_date_char,pattern="-") #split date 

sale_date_matrix <- data.frame(Reduce(rbind,sale_date_char)) #rbind

sale_date_year_month <-  str_c(sale_date_matrix$X1,'-', sale_date_matrix$X2) #concatenate year and month 

housing_df <- cbind(housing_df,sale_date_year_month ) #cbind year and month to dataframe

