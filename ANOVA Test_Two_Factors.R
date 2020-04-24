  #DESCRIPTION: 
#ANOVA one-way - One factor, three levels, being a categorical variable
#ANOVA two-ways - Two factors, being each of them a categorical variable

#Original source:https://www.guru99.com/r-anova-tutorial.html

library(dplyr)
library(ggplot2)
PATH <- "https://raw.githubusercontent.com/thomaspernet/data_csv_r/master/data/poisons.csv"

##################ANOVA TEST ONE WAY(ONE FACTOR)##################
#Time is a function of poison
#Step 1
df <- read.csv(PATH) %>%  
    select(-X) %>%  
  #Step 3
  mutate(poison = factor(poison, ordered = TRUE))


glimpse(df)
#Data loaded: Time, type of poison and type treatment

#Test the followign assumptions
#H0: There is no difference in survival time average between group
#H3: The survival time average is different for at least one group.

levels(df$poison)

#Compute the mean and standard deviation for each level
df %>%
  group_by(poison) %>%
  summarise(
    count_poison = n(),
    mean_time = mean(time, na.rm = TRUE),
    sd_time = sd(time, na.rm = TRUE)
  )

#Visual inspection, graphical check,there is a difference between the distribution
ggplot(df, aes(x = poison, y = time, fill = poison)) +
  geom_boxplot(color = "steelblue") +
  geom_point()+
  theme_classic()

#Run the one-way ANOVA test with the command aov
anova_one_way <- aov(time~poison, data = df)
summary(anova_one_way)

#P Value is less than 0.05, so we can say that H0 can be discarded, 
#meaning there's a statistical difference between the time survival 
#for each type of poison

#Tukey Test (is a pairwise comparison) will inform which group has a  different mean
TukeyHSD(anova_one_way)

#CONCLUSION:Major difference comes from  variables 3 and 1 and 3 and 2, as 
#the graph shows

##################ANOVA TEST TWO WAY (2 FACTORS)##################
#Time is a function of poison and treat
#H0: The means are equal for both variables (i.e., factor variable)
#H1: The means are different for both variables
#Final Step, main effects and interaction effect (poison  treat) analyzed 
anova_two_way <- aov(time~poison + treat + poison:treat, data = df)
summary(anova_two_way)
#Visual inspection, graphical check,there is a difference between the distribution
#Shows that the interaction effect is not significant,as are both main effects
ggplot(df, aes(x = treat, y = time, fill = poison)) +
  geom_boxplot(color = "steelblue") +
  theme_classic()

#You can reject the NULL hypothesis and confirm that changing the treatment 
#or the poison impacts the time of survival
coefficients(anova_two_way)
effects(anova_two_way)