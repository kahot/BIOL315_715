### Running ANOVA and Contingency table tests Part 2###

# We will go over complicated ANOVA analyses and contingenct tests

# Load all necessary libraries
library(ggplot2)
library(reshape2)
library(Rcmdr)
library(nlme)


# Load the datasets

# Mussel count data from different locations 
ex1<-read.csv("Data/Mussels.csv")
#reshape the data
Mussel<-melt(ex1)
#remove NA from the table
Mussel<-Mussel[!is.na(Mussel$value ),]
#Assign column names
colnames(Mussel)<-c("Location","Length")


## Check assumption ()
# 1. "Shapiro-Wilk normality test".
shapiro.test(Mussel$Length)

# 2. Levene's Test (testing homogeneity of variance)
leveneTest(Length ~ Location, data=Mussel)


### One-way ANOVA ###
#aov(formula, data)
aov.ex1 = aov(Length ~ Location, data=Mussel) 

# Look at the ANOVA results
summary(aov.ex1)     

# Run TukeyHSD (a post-hoc test) to see which pairs are significantly different
TukeyHSD(x=aov.ex1,'Location', conf.level=0.95)



### Two-Way ANOVA ###

## Example 1: Mussel predation data (from last week)
ex2=read.csv("Data/Predation_exp.csv")
predation<-melt(ex2, id.vars="Location")
# rename of columns 
colnames(predation)[2:3]<-c("Treatment","Mussel_count")


# "Shapiro-Wilk normality test" 
shapiro.test(predation$Mussel_count)
qqnorm(predation$Mussel_count)

#Levene's Test
leveneTest(Mussel_count~Location*Treatment, data=predation)


# ** We will assume the data are 'good enough' to use parametric tests (ANOVA) ** 
#Run 2-way ANOVA
aov.ex2 = aov(Mussel_count~Location+Treatment,data=predation)
summary(aov.ex2)                                    

# Run 2-way ANOVA with the interaction term
aov.ex22 = aov(Mussel_count~Location*Treatment,data=predation)
summary(aov.ex22)                                    


# What is your conclusion?


# Running TukeyHSD (post-hoc test) to see which pairs are significantly different
TukeyHSD(x=aov.ex22,'Location:Treatment', conf.level=0.95)



## Example 2: Coral Tissue Thickness
# This is my data from looking at coral tissue thickness from a reciprocal transplant experiment.
# In a reciprocal transplant experiment, we have factor1 ="Origin" of the corals, and 
# factor2 = Transplant Site ('Site') of the corals.

#Read the data
ex3=read.csv("Data/tissue_thickness.csv")

# Test the assumptions
shapiro.test(ex3$Thickness)

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



# ! But wait, if you look at the data, the individuals are actually measured at multiple times (i.e. replicates)
# (I did this because tissue thickness varies a little even within an individual.)
# In this case, it would be best to use nested 2-way ANOVA

### Nested Two-Way ANOVA ###
# Nested ANOVA means there were replicates of samples.
# To run nested one-way ANOVA, it's simpler. You can formulate like below to look at the effects of origin:
aov.nest1<-aov(log(ex3$Thickness) ~ ex3$Origin/factor(ex3$Indiv))
summary(aov.nest1)

# The effects of transplant site can be looked at;
aov.nest2<-aov(log(ex3$Thickness) ~ ex3$Site/factor(ex3$Indiv))
summary(aov.nest2)



# To run Nested 2-Way ANOVA in R, you first need to model the results, 
# incorporating the repeat measure within individuals as an error term.

# We will use 'nlme' package to conduct this analysis: 
model1<-lme(log(Thickness) ~   Origin+Site, random=~1|Indiv, data=ex3)

#Then run ANOVA on the model
anova.lme(model1)

# What is your conclusion?


######### Non-parametric Tests #########

### So far, we've done parametric testing, assuming normal distribution

# What if your data do not meet the assumption of parametric testing? 

# Then we can run 'non-parametric' tests. These tests can be apply to almost any data
# but it may not be as robust as'parametric' tests.

## 1. Alternative to t-test or paired comparison
# "Wilcoxon Rank Sum Test"

# Use mussel counts from the predtaion data at the low tidal height 
#subset the data
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


#### Correlation ####
cor.test()

#### Regression ####
lm()


#### G-test / Chi-square Test / Fisher's Exact Test
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
# G-test does not come with the basic R stats package, so you need to install a package
#install.packages('RVAideMemoire')
library(RVAideMemoire)

#Run G.test. The input for G.test needs to be a matrix (not a data frame)
G.test(as.matrix(birds))

# Are the results comparable between the 2 tests?


# You can then run Post-hoc Test to see which pairs are significant. 
pairwise.G.test(as.matrix(birds), p.method = "holm")


# for more about p-value adjustment for multiple comparison, 
?p.adjust


#Fisher's exact tests are mostly used with a 2x2 contingency table (but doesn't have to be) with a small sample size.
#Fisher's exact test is more accurate than the chi-square test or G–test when the expected numbers are small. 

# Example: Penguin mortality in different nesting areas

penguin<-data.frame(alive=c(43,44,49), dead=c(7,6,1), 
                    row.names=c("Lower nesting area","Middle nesting area","Upper nesting area"))

# Run Fisher's Exact Test
fisher.test(as.matrix(penguin))

#Run G-test
G.test(as.matrix(penguin))
#G = 6.0621, df = 2, p-value = 0.04827


# Are the test results same? 

# Practice Example1: Spotted moray eel &. Purplemouth moray eel abundance in different habitats. 
# Which test would you use with this dataset? What are the results? Do these two species use different habitats? 

eel<-data.frame(spotted=c(127,99,264), purplemouth=c(116,67,161), 
                row.names=c("Grass","Sand","Border"))

G.test(as.matrix(eel))


# Practice Example2: Great blue herons and Great egrets in different resting sites. Do these two species land on different types of substrate? 
birdlake<-data.frame(Heron=c(15,20,14,6), Egret=c(8,5,7,1), 
                     row.names=c("Vegetation","Shoreline","Water","Structures"))

fisher.test(as.matrix(birdlake))

# Your Assignment 3 is to run a statistical test if algal cover differs among low, medium and high intertidal zones 


