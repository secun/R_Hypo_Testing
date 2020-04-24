   #DESCRIPTION
#ANOVA one-way - One factor, four levels, being each of them a continuous variable

###Working directory
library(tidyverse) #contains readr

setwd("C:/DATA/R model/00_Learning/Hypothesis_Testing")

d_train <- read_csv('Data_hypothesis_testing.csv')

df <- d_train %>% 
  mutate(message = factor(X1, ordered = TRUE)) %>%
  select(-starts_with("X1"))
#reorder
df<- select(df, message,C1,C2,C3,C4,C5)
glimpse(df)

#Reformat data
a<-gather(df,key="group",value="impacts",C1:C5)
#visualize
glimpse(a)
levels(a$message)

#Data loaded: Time, type of poison and type treatment
#Test the following assumptions
#H0: There is NO difference in marketing messages between groups,all means are equal
#H1: There is  difference in marketing messages between groups
#Compute the mean and standard deviation for each level
a %>%
  group_by(message) %>%
  summarise(
    count_message = n(),
    mean_time = mean(impacts, na.rm = TRUE),
    sd_time = sd(impacts, na.rm = TRUE)
  )
#Visual inspection, graphical check,there is a difference between the distribution
ggplot(a, aes(x = message, y = impacts)) +
  geom_boxplot(color = "steelblue") +
  geom_point()+
  theme_classic()

#Run the one-way ANOVA test with the command aov
anova_one_way <- aov(impacts~message, data = a)
summary(anova_one_way)
#P Value is less than 0,05, so we can say that H0 can be discarded, 
#meaning there's a statistical difference between the impacts 
#for each type of marketing message
#A great degree of the variation on the outcomes variable (195/12.7)
#comes from the predictor, havign a very good correlcation coefficient

#Tukey Test (is a pairwise comparison) will inform which groups have 
#a  significant different mean
TukeyHSD(anova_one_way)
#A-D,D-B,D-C are the groups with a different mean, so from here we could
#run a T-Test

pairwise.t.test( x = a$impacts, # outcome variable
                 g = a$message, # grouping variable
                 p.adjust.method = "none" # which correction to use?
                )

p.adjust.method = "bonferroni"
p.adjust.method = "holm"
p.adjust.method = "none"

#We found that marketing message D produced a significantly larger impact
# than A (p=4.1*10-5),B (p=1.5*10-5),C(p=87*10-5)
#We found no evidence that B (p=0.601) ,C (p=0.15) performed better than A, 
#or B better than C (p=0.058).
#Holm and Bonferoni shows similar results