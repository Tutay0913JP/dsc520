# Assignment: ASSIGNMENT 3
# Name: Par, Janine
# Date: 2021-03-29

## Load the libraries Required
library(ggplot2)
library(qqplotr)
library(pastecs)

theme_set(theme_minimal())

## Set the working directory to the root of your DSC 520 directory
setwd("C:/Users/janin/OneDrive/Documents/R_repo/dsc520/")

## Load the `data/r4ds/heights.csv` to

acse_df <- read.csv("data/acs-14-1yr-s0201_download.csv")

#Please provide the output from the following functions: str(); nrow(); ncol()
str(acse_df)
nrow(acse_df)
ncol(acse_df)

#Create a Histogram of the HSDegree variable using the ggplot2 package.
    #a.	Set a bin size for the Histogram.
    #b.	Include a Title and appropriate X/Y axis labels on your Histogram Plot.

ggplot(acse_df, aes(HSDegree)) + geom_histogram(bins = 10, aes(y = ..density..)) + ggtitle("2014 American Community Survey.")+ xlab("High School Degree") +  ylab("Frequency")


# h.	Include a normal curve to the Histogram that you plotted. 

ggplot(acse_df, aes(HSDegree)) + geom_histogram(bins = 10, aes(y = ..density..)) + stat_function(fun = dnorm,
                args = list(mean = mean(acse_df$HSDegree),
                            sd = sd(acse_df$HSDegree)),
                col = "black",
                size = 1) + ggtitle("2014 Census")+ xlab("High School Degree") +  ylab("Density")


# V.	Create a Probability Plot of the HSDegree variable.
ggplot(acse_df,mapping = aes(sample = HSDegree)) + stat_qq_point(size = 2,color = "red") + stat_qq_line(color="green") 

qplot(sample=acse_df$HSDegree, stat="qq",line=45)


# Now that you have looked at this data visually for normality, you will now quantify normality with numbers using the stat.desc() function. Include a screen capture of the results produced.
options(scipen=100)
options(digits=2)

stat.desc(acse_df$HSDegree)

# Calculate z-score
z_scores <- (acse_df$HSDegree-mean(acse_df$HSDegree,))/sd(acse_df$HSDegree)
