#***MAKING SOUND DECISION***

library(ggplot2)
rm(list=ls())
######1-Proportion Hypothesis Testing #########
#######One sample proportion ########
#DESCRIPTION
#Company says that its supplier operates at 97% SLA. 
#A sample of 200 invoices is reviewed and 197 (98.5%) are found to be defect free
#Does the supplier operate at 97% or higher rate?
#
## H0: The null hypothesis is that the supplier operates at less than 97%. 
## A:  The alternative is that this proportion is greater than 97%
## Data: 200 trials, 197 sucesses, probability of sucess
##  Parameter: It's two-sided, either greater or less
##  Cof.level is the confidence level
res<-prop.test(x = 197, n = 200, p = 0.97, alternative = "greater", conf.level=0.95)

res$p.value
res$conf.int  #Confidence interval for the true proportion

#The p-value of the test is 0.15, which is more than the significance level 
#alpha= 0.05 (thus test is not significant, so we cannot discard the NULL hypothesis)
#We fail to discard the H0, so migh tbe valid that supplier operates at less than 97%. 

#######Two sample proportion ########
#DESCRIPTION
#Two call centers (East, West) have been measured. Do they have equal performance?
#Is performance related to or independent of location of call center?
#Centro A: 1435 llamadas ok sobre 1536
#Centro B: 1472 llamadas ok sobre 1636
## H0: The null hypothesis is that performance is the same. 
## A:  The alternative is that this proportion are different
##  Data: sucessfull calls, total calls
##  Parameter: It's two-sided, either igual or different
##  Cof.level is the confidence level

cc.ok <- c(1435,1472)
cc.total<- c(1536,1636)
res<- prop.test(x=cc.ok,n=cc.total, correct=FALSE, alternative = "two.sided" )
res

#Due to the fact that p<0.05, if H_0 was true, it would be very unusual (0%) 
#to see a difference in two samples far from 0.0344 due to sampling variation. 
#Thus, we have sufficient evidence to conclude that H0 can be discarded 
#and therefore the performance of the two call centers are different.


#######Multiple sample proportion(one way) ########
#DESCRIPTION
#Data presentred in the form proportions + observations
#When launching a new product into one market, four main sources of 
#acquisition of new customers were identified (expected column, as 
#proportions). After several improvements were done, we wanted to know 
#whether something had changed (observed, raw numbers):
## H0: The observed and expected is the same
## HA: There's some difference between observed and expected
# Data:The expected performance is 0.72/0.07/0.12/0.09 through 
#       the different acquisition channels Paid/Referral/Organic/Upgrade  
#       The observed performance is the following 205/26/25/29
# We wanted to reduce dependency on Paid after the improvement
observ <- c(180, 26, 25,29)

observ[1] /sum(observ) #This data may suggest that Paid has been reduced(69%).

#However
res <- chisq.test(observ, p = c(0.72,0.07,0.12,0.09))
res
#We can conclude that the observed proportions are not significantly 
#different from the expected proportions.
#We cannot discard H0 because p>0,05, so we donât see a significant
#difference in the observed NCA compared to the expected values
#(before improvement).

#How should the observations be to match the expected?
res$expected


#######Multiple sample proportion(two way) ########
#DESCRIPTION
#Data presentred in the form of not defective and defective
#Six techs (A, B, C,â¦) have been measured for the same period of time. 
#Do they have equal performance regarding claims by customer?
## H0: The observed performance(claims,jobs done ok) is independent of technician
## HA: Performances are different, so depend on technician
# Data: Number of claims by customer and jobs done have been recorded

claims <- c(35, 16, 21,20,25,27)
jobs <- c(427,406,328,348,460,405)
tech <- c("A","B","C","D","E","F")
data <- as.data.frame(rbind(cbind(tech,jobs,v3=c("ok")), 
                      cbind(tech,claims,v3=c("claim"))),
                      stringsAsFactors=FALSE)
data$tech<-as.factor(data$tech)
data$jobs <-as.integer(data$jobs)
names(data)[3] <- "performance"
data$performance <- as.factor(data$performance)

#Seeing this % grap, we may conclude that performance is not the same
ggplot(data=data, aes(x=tech, y=jobs, fill=performance ))+
  geom_col(position="fill" ,color="black")+
  ggtitle("Stacked bar chart comparing proportions in each ownership class") +
  xlab("Tech")+ylab("Jobs done")+
  scale_fill_grey()+
  scale_y_continuous(labels = scales::percent)

#Let's calculate the contingency table, to provide number to the grap below
table.aux <- xtabs(jobs ~ tech + performance , data=data)
table.aux
prop.table(table.aux,margin=1)

perf.test<-chisq.test(table.aux)
perf.test
perf.test$observed
perf.test$expected
perf.test$residuals

#The p-value is 0.63 which is very large. Hence we conclude that there is 
#no evidence of a difference in the proportion of performance amongst the 
#technicians A-F, or that there is no evidence against performance
#being independent of technician.

assocplot(table.aux)

#######2-Mean Hypothesis Testing ########
#######One sample mean t-test ########
#DESCRIPTION(Ï not known): 
#Complains that O&M costs are increasing are around. 
#During 2016, the overall repair cost per repaired machine
#(CENTAC C1000) was 13,456â¬. During 2017 the following 12 
#data-points for same machines were provided for all 
#the machines repaired by OEM. 
#Can we confirm whether the O&M cost was higher for 2017?
## H0: mu = 13,456
## HA: mu > 13,456
# Data: Number of O&M costs recorded in current year
#
costs <- c(15294,14919,16309,10553,14998,13929,14852,
           21088,16151,12927,17896,12638) 

boxplot(costs,main="Costs in current year for cENTAC",
        xlab="CENTAC product", ylab="O%M costs")
#Low volume sample, we check normality
#Option 1 - visual inspection
library("ggpubr")
ggqqplot(costs, ylab = "Costs",
         ggtheme = theme_minimal())
#Option 2 - Shapiro Test
shapiro.test(costs) #Ho is assumption of normality
###All good

t.test(x=costs, mu = 13456, 
       alternative = "greater", conf.level=0.95)        

#With P=0,027 < 0,05 we reject Ho, so the cost of repair has 
#significantly increased from one year to the next one. 
#However, taking into account the inflation effect, we would need to add a 
#3% to the 2016 cost, raising to 14,128, 
#therefore concluding that there is no evidence with the supplied data 
#that it O&M cost has not been increased.



#######Two sample mean t-test (unequal variances) ########
#DESCRIPTION
#Comparing two means, which are continuous variables
#Quality of an outsourced activity has decreased, so before in-sourcing an additional 25% overcost compared 
#to the 3rd party, we would want to know whether there's a difference higher than 445Ã¢ÂÂ¬
#in performance of CG collection. H0= m1-m2 = 445 
#Provider:53 transactions have been collected for CG, with a mean of 989 and StDev 167
#Internal:45 transactions have been collected for Zurich, with a mean of 1,501 and StDev 287
#Initial analysis suggests that there's a difference of 320, meaning that we could 
#in-source the activity
CG <- c(1451,1200,1500,1300,1300,1600,1100,1200,1400,1345,1389,1200,1100,
        1600,1501,1700,1678,1896,1200,1200,1200,1200,1400,1500,1700,1800,
        1900,1200,1100,1900,1345,1234,1287,1267,1967,1954,1964,1867,1834,
        1923,1976,1834,1451,1451,1451)

zurich<- c(985,1100,900,800,800,1200,1190,1178,1287,1176,854,867,897,854,843,
           831,845,901,923,956,976,1000,1190,1176,1123,1156,1176,1189,1198,
           1234,1267,1289,1298,1209,856,878,865,854,834,834,789,798,765,
           801,804,805,967,999,976,954,954,934,902)

length(CG)
mean(CG)
sd(CG)
length(zurich)
mean(zurich)
sd(zurich)

#Run test saying that std are different, and running the test with a difference of 445
## H0: The null hypothesis is that the difference is less than 445
## A:  The alternative is that the difference is greater than 445
## Data: two different data sets, unbalanced
##  Parameter: It's two-sided, either greater or less
##  var.equal: We treated the variances as being unequal, so FALSE, therefore 
# the unpooled t-test(Welch t-test) in being done
dif <- 445
res<-t.test(x = CG, y=zurich, mu=dif, var.equal = FALSE, alternative = "greater", conf.level=0.95)
res
res$p.value
res$conf.int

#The p-value of the test is 0.086, which is more than the significance level 
#alpha= 0.05 (test is not significant, so the NULL hipothesis is retained - difference might be less than 445)
#We can conclude that there is NOT significant data that proves that there's a difference in costs(445) 
#So, we shouldn't in-source the activity with the data we have.
#Note: Welch Test does not assume homogeneity of variance (they are not equal)

#######Paired observations t-test ########
#Data is often correlated, so they are paired and are dependent measurements.

#######3-Multiple sample mean ANOVA ########
###########One factor, 2 or more levels#########
#Here we measure the variance in-between vs variance within . F close to 1 means
#both variances are similar, F larger means both variances are different. 
#Equal variances is assumed
#Four different marketing messages were tested across several 
#randomized customers clusters (C1,â¦,C5) and the result measured 
#as number of customers acquiring the service is as follows:
#
#Marketing message	C1	C2	C3	C4	C5
#     A             11	17	16	14	15
#     B             12	10	15	19	11
#     C             23	20	18	17	NA
#     D             27	33	22	26	28

#We have one factor (content of marketing message) and 4 levels
#(different contents). The result is the number of new acquired 
#customers. The customers clusters could be used as another factor, 
#but here itâs intended to be used as randomized but similar groups.
#We want to know whether thereâs any difference on the marketing message, 
#having a more positive impact on the customer base.

#H0	Âµ_A = Âµ_B=   Âµ_C= Âµ_D
#HA	Average scores varies by country/team

NCA.a <- c(11,17,16,14,15)
NCA.b <- c(12,10,15,19,11)
NCA.c <- c(23,20,18,17,NA)
NCA.d <- c(27,33,22,26,28)
data <- as.data.frame(rbind(cbind(NCA.a,v3=c("A")), 
                            cbind(NCA.b,v3=c("B")),
                            cbind(NCA.c,v3=c("C")),
                            cbind(NCA.d,v3=c("D"))
                            ),stringsAsFactors=FALSE)
names(data)[1] <- "NCA"
names(data)[2] <- "mkt.msg"
data$mkt.msg<-as.factor(data$mkt.msg)
data$NCA <-as.integer(data$NCA)


ggboxplot(data, x = "mkt.msg", y = "NCA", 
          color = "mkt.msg", 
          xlab = "Marketing message", ylab = "Customer acquired")

res.aov <- aov(NCA ~ mkt.msg, data = data)
summary(res.aov)
#conclude that the null hypothesis is null, with significant 
#variations between the means across the different marketing messages.




###########Two factors,and  or more levels#########
#2-way ANOVA
#Equal variances is assumed
