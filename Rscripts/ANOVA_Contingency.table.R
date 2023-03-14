### Running ANOVA and Contingency table tests ###
# Save this as a reference script to run your future ANOVA tests!


library(reshape2)

#######  ANOVA  #######

## Check the assumptions first

# Mussel count data from different locations 
ex1<-read.csv("Data/Mussels.csv")

#View the data
View(ex1)

#reshape the data
Mussel<-melt(ex1)

#remove NA from the table
Mussel<-Mussel[!is.na(Mussel$value ),]

#Assign column names
colnames(Mussel)<-c("Location","Length")


## Check assumption
# 1. "Shapiro-Wilk normality test".
# **Apply appropriate data transformation if necessary

#shapiro.test(x) #x must be a vector 
shapiro.test(Mussel$Length)


# Visualize with Q-Q (qunatile) plot 
qqnorm(Mussel$Length)

# 2. Levene's Test (testing homogeneity of variance)
#install.packages("Rcmdr")
library(Rcmdr)
#leveneTest(formula, data)
leveneTest(Length ~ Location, data=Mussel)

#Alternatively, you can perform Bartlett test (you don't need to run both)  
#bartlett.test(formula, data)
bartlett.test(Length ~ Location, data=Mussel)


### One-way ANOVA ###
#aov(formula, data)
aov.ex1 = aov(Length ~ Location, data=Mussel) 

# Look at the ANOVA results
summary(aov.ex1)     

# Print the means for each location
print(model.tables(aov.ex1,"means"),digits=3)      

##You can visualize the data quickly (though not necessarily the most beautiful plot)
boxplot(Length~Location,data=Mussel)        

#visualize the residuals
op<-par(mfrow=c(2,2)); plot(aov.ex1); par(op)

## Another way to look at the residuals (same as above but go through each plot separately)
plot(aov.ex1)


# Run TukeyHSD (a post-hoc test) to see which pairs are significantly different
#TukeyHSD(x, which, conf.level = 0.95, ...)
TukeyHSD(x=aov.ex1,'Location', conf.level=0.95)



### Two-Way ANOVA ###

## Example 1: Mussel preadation data (from last week)

ex2=read.csv("Data/Predation_exp.csv")

predation<-melt(ex2, id.vars="Location")
# rename of columns 
colnames(predation)[2:3]<-c("Treatment","Mussel_count")


# "Shapiro-Wilk normality test" 
shapiro.test(predation$Mussel_count)
qqnorm(predation$Mussel_count)


#Levene's Test
leveneTest(Mussel_count~Location*Treatment, data=predation)


#Run 2-way ANOVA
aov.ex2 = aov(Mussel_count~Location+Treatment,data=predation)
summary(aov.ex2)                                    

# Run 2-way ANOVA with the interaction term
aov.ex22 = aov(Mussel_count~Location*Treatment,data=predation)
summary(aov.ex22)                                    


# What is your conclusion?


#You can visualize the data quickly (though not necessarily the most beautiful plot)
boxplot(Mussel_count~Location*Treatment,data=predation) 


# Running TukeyHSD (post-hoc test) to see which pairs are significantly different
TukeyHSD(x=aov.ex22,'Location:Treatment', conf.level=0.95)



## Example 2: Coral Tissue Thickness
# This is my data from looking at coral tissue thickness from a reciprocal transplant experiment.
# In a reciprocal transplant experiment, we have factor1 ="Origin" of the corals, and 
# factor2 = Transplant Site ('Site') of the corals.

#Read the data
ex3=read.csv("Data/tissue_thickness.csv")

#Look at the data to see if you need to 'melt' the data 

# Test the assumptions
# "Shapiro-Wilk normality test" 
shapiro.test(ex3$Thickness)
# visualize
qqnorm(ex3$Thickness)

# "Shapiro-Wilk normality test" with log transformation
shapiro.test(log(ex3$Thickness)) 

# visualize 
qqnorm(log(ex3$Thickness))

#Levene's Test
leveneTest(log(Thickness)~Site*Origin, data=ex3)

#Run 2-way ANOVA
aov.ex3 = aov(log(Thickness)~Site*Origin,data=ex3)

#look at the ANOVA results
summary(aov.ex3)                                    


# Means for each treatment:
print(model.tables(aov.ex3,"means"),digits=3)   


#Visualize the data
boxplot(log(Thickness)~Site*Origin,data=ex3) 


# Which pairs are significant?
TukeyHSD(x=aov.ex3,'Site:Origin', conf.level=0.95)



# ! But wait, if you look at the data, the individuals are actually measured at multiple times
# (I did this because tissue thickness varies a little even within an individual)
# In this case, it would be best to use nested 2-way ANOVA

### Nested Two-Way ANOVA ###
# Nested ANOVA means there were replicates of samples.
# To run nested one-way ANOVA, it's simler. You can formulate like below to lookat the effects of origin of corals
aov.nest1<-aov(log(ex3$Thickness) ~ ex3$Origin/factor(ex3$Indiv))
summary(aov.nest1)


# To run Nested 2-Way ANOVA in R, you first need to model the results, 
# incorporating the repeat measure within individuals as an error term.

# You can do this in multiple ways (using different packages).
# Today, we will use 'nlme' package 
install.packages('nlme')
library(nlme)
model1<-lme(log(Thickness) ~   Origin+Site, random=~1|Indiv, data=ex3)

#Then run ANOVA on the model
anova.lme(model1)


######### Non-parametric Tests #########

### So far, we've done parametric testing, assuming normal distribution

# What if your data do not meet the assumption of parametric testing? 

# Then we can run 'non-parametric' tests. These tests can be apply to almost any data
# but it may not be as robust as'parametric' tests.

## 1. Alternative to t-test or paired comparison
# "Wilcoxon Rnak Sum Test"

# Use mussel counts from the predataion data at the low tidal height 
#suebset the data
low<-predation[predation$Location=="Low",]

wilcox.test(low$Mussel_count[low$Treatment=="Caged"], low$Mussel_count[low$Treatment=="Uncaged"], 
            alternative = "two.sided")

# Compare with t-test results
t.test(low$Mussel_count[low$Treatment=="Caged"], low$Mussel_count[low$Treatment=="Uncaged"])

# What did you notice?


## 2. Alternative to 1-way ANOVA: Kruskal-Wallis Rank Sum Test 

# We can use the same Mussel data
kruskal.test(Length ~ Location, data=Mussel)

# Compare the results from the 1-way ANOVA results

#### Correlation. ####
cor.test()

#### Regression ####
lm()


#### G-test / Chi-sqaure Test / Fisher's Exact Test
## They are used for testing for 'Independence' (compare proportions)  or 
#'Goodness of fit (fit to a theoretical expectation)' of categorical values (observed counts)

# *key: Categorical variables / Proportion comparisons

# Ex. 1)  Test for Independence 
# Birds abundance in remnant habitat vs. restored habitat

#This dataset contains the number of observed birds in each habitat
birds<-read.csv("Data/bird_habitat.csv", row.names=1)

#First run Chi-square test (input should be integers)
chisq.test(birds)

# !! If using a proportion data, you could simply multiply by 10 or 100 !!
# for example, the same data can be presented as a proportion (which is likely to be more common)

bird_prop<-read.csv("Data/bird_habitat_proportion.csv", row.names = 1)

# in this case, you can multiple by 1000 (since the decimal point is 3)
bird_prop<-bird_prop*1000

chisq.test(bird_prop)


# Next, run G-test and compare the results. Should be almost the same.
# G-test does not come with the basic R stats package, so you need to intall a package
install.packages('RVAideMemoire')
library(RVAideMemoire)

#Run G.test. The input for G.test needs to be a matrix (not a data frame)
G.test(birds)

# Are the results comparable between the 2 tests?


# You can then run Post-hoc Test to see which pairs are significant. 
pairwise.G.test(as.matrix(birds), p.method = "holm")

# for more about p-value adjustment for multiple comparison, 
?p.adjust


#Fisher's exact tests are use with a 2x2 contingency table with a small sample size.



# Your Assignment 3 is to run a statistical test if algal cover differs among low, medium and high intertidal zone 


