# Assignment: ASSIGNMENT 4
# Name: Par, Janine
# Date: 2021-04-10

## Load the libraries Required
library(ggplot2)

theme_set(theme_minimal())

## Set the working directory to the root of your DSC 520 directory

setwd("C:/Users/janin/OneDrive/Documents/R_repo/dsc520/")

## Import Scores dataset

student_scores<- read.csv("data/scores.csv")

# i.	Use the appropriate R functions to answer the following questions:
#   1.	What are the observational units in this study? 
#   Answer:-----> The observational unit in the scores data is the 'Section' where both variables: Scores and Count are being collected.


# 2.	Identify the variables mentioned in the narrative paragraph and determine which are categorical and quantitative?
#   Answer:----->    
#   These are the variables in the Scores dataset:
#   a.	Scores variable is assumed to be the course grades gathered for a section. This is Categorical Variable
#   b.	Count variable is assumed to be the total number of students that earned the score in a section. This is quantitative variable


#3.	Create one variable to hold a subset of your data set that contains only the Regular Section and one variable for the Sports Section.
student_regular <- subset(student_scores, Section =="Regular")

student_sport <- subset (student_scores, Section =="Sports")


#4.	Use the Plot function to plot each Sections scores and the number of students achieving that score. 
#Use additional Plot Arguments to label the graph and give each axis an appropriate label. Once you have produced your Plots answer the following questions:

  #need to convert student scores section as factor to use the plot function
  student_scores$Section = as.factor(student_scores$Section)

  #Using GG PLOT
  
  #Separate using GG Plot
  ggplot(student_regular, aes(x=Score  , y=Count   )) +  geom_point(aes(color=Section))
  
  ggplot(student_sport, aes(x=Score  , y=Count   )) +  geom_point(aes(color=Section))


  #Combined using Plot function
  plot(student_scores$Score, student_scores$Count, xlab="Scores", ylab = "Count", main = "Student Scores",  ,
       col = student_scores$Section,  pch = 24
  )

  #Combined using GG PLOT
  ggplot(student_scores, aes(x=Score, y=Count))  + 
    geom_point(aes(col=student_scores$Section, size=student_scores$Section))  +
    labs(subtitle="Score Vs Count", 
         y="No of Students Achieving the Score", 
         x="Score of Students in Sports Section", 
         title="Scatterplot", 
         caption = "Graph of Student Scores in Regular Section")
  
  
  
#a.	Comparing and contrasting the point distributions between the two section, 
#looking at both tendency and consistency: Can you say that one section tended to score more points than the other? Justify and explain your answer.
     #Answer:---->  Students in both section have a varying scores however Regular students appear to have more number of student to achieved a higher score compare to sports student  


#b.	Did every student in one section score more points than every student in the other section? 
#If not, explain what a statistical tendency means in this context.
  
  regular_mean <- mean(student_regular$Score)
  regular_median <- median(student_regular$Score)
  
  sport_mean <- mean(student_sport$Score)
  sport_median <- median(student_sport$Score)
  
  #Answer:---->  No, not every student in one section score more points than the other. However, the median(middle score) and mean (average score) shows higher for Regular section
  
#c.	What could be one additional variable that was not mentioned in the narrative that could be influencing the point distributions between the two sections?
   #Answer:---->   I think categorical variables that define the specific application or further break down the data of the Regular section can help with the analysis.
