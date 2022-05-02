# Assignment: ASSIGNMENT 5
# Name: Par, Janine
# Date: 2022-04-30


## Set the working directory to the root of your DSC 520 directory
setwd("C:/Users/janin/OneDrive/Documents/R_repo/dsc520/")

## Load the `data/r4ds/heights.csv` to
heights_df <- read.csv("data/r4ds/heights.csv")
str(heights_df)

## Using `cor()` compute correclation coefficients for
## height vs. earn
cor(heights_df$height,heights_df$earn)
### age vs. earn
cor(heights_df$age,heights_df$earn)
### ed vs. earn
cor(heights_df$ed,heights_df$earn)

## Spurious correlation
## The following is data on US spending on science, space, and technology in millions of today's dollars
## and Suicides by hanging strangulation and suffocation for the years 1999 to 2009
## Compute the correlation between these variables
tech_spending <- c(18079, 18594, 19753, 20734, 20831, 23029, 23597, 23584, 25525, 27731, 29449)
suicides <- c(5427, 5688, 6198, 6462, 6635, 7336, 7248, 7491, 8161, 8578, 9000)

cor(x=tech_spending,y=suicides,use="all.obs", method = "pearson")

cor(x=tech_spending,y=suicides,use="all.obs", method = "spearman")

cor(x=tech_spending,y=suicides,use="all.obs", method = "kendall")
