### Feb. 14, 2023
###  Data Manipulation


#Reshape your data into the 'tidy data" format

# 1. using 'reshape2' package
# If you haven't installed the package yet;
install.packages('reshape2') 
library(reshape2)


# read the data set from your computer
predation_raw<-read.csv('Data/Predation_exp.csv')


#Use melt to tidy the data
predation<-melt(predation_raw, id.vars="Location")

# rename of columns 
colnames(predation)[2:3]<-c("Treatment","Mussel_count")

# or you can do this all in one step
# predation<-melt(predation_raw, id.vars="Location", value.name = "Mussel_count", variable.name = "Treatment")

# You can save the reshaped data 
write.csv(predation, "Data/Predation_exp_reshaped.csv")


# 2. Using tidyr to do the same thing
install.packages('tidyr')
library(tidyr)

#predation
?pivot_longer
pred<-pivot_longer(predation_raw, -Location, values_to="Mussel_count",names_to="Treatment")


################
## Exercise 1 ##
################

#Try reshaping table4a (TB occurrence data) that comes with tidyr
table4a
#country     `1999` `2000`
#
#1 Afghanistan    745   2666
#2 Brazil       37737  80488
#3 China       212258 213766

table4a<-table4a
#copy and paste your reshaped data table here
new_table4a<-melt(table4a, id.vars="country")

new_table4a
colnames(new_table4a)[2:3]<-c("year","tb")
colnames(new_table4a)[c(2,3)]<-c("year","tb")
colnames(new_table4a)<-c("country","year","tb")

new_table4a
#      country year     tb
#1 Afghanistan 1999    745
#2      Brazil 1999  37737
#3       China 1999 212258
#4 Afghanistan 2000   2666
#5      Brazil 2000  80488
#6       China 2000 213766




### Statistical analysis on predation data ###

#1.t-test (compare 2 results from the low tidal height only)

?t.test # to get help information

#Separate low inter-tidal data
colnames(predation)[2:3]<-c("Treatment","Mussel_count")
low<-predation[predation$Location=="Low",]

#['row','column']
low$Mussel_count
t.test(low$Mussel_count[low$Treatment=="Caged"],low$Mussel_count[low$Treatment=="Uncaged"])

#low[low$Treatment=="Caged","Mussel_count"]

#or you can do exactly the same thing in a slightly different way:
with(low, t.test(Mussel_count[Treatment=="Caged"], Mussel_count[Treatment=="Uncaged"]))

##########################################
# Copy and paste t-test results here 
##########################################

#2. Use ANOVA

aov1 = aov(Mussel_count ~ Treatment,data=low) 
summary(aov1)     

##########################################
# Copy and paste the ANOVA results here 
##########################################


# --- if we have some time ---
## Plot the results
boxplot(Mussel_count ~ Treatment,data=low)
