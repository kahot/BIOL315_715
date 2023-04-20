#Tidy Data: Week 3 Assignment

# Enter all codes into this file
# Copy and paste the output below each assignment
library(stringr)
library(reshape2)
library(tidyr)
library(ggplot2)


#Tidy up the messy dataset  

####################################################################
# 1.Column headers are values, not variable names: simple example
####################################################################
# Using the heterozygosity data (heterozygosity.csv) to practice tidying up the data 
# (you can use either reshape2 or tidyr)

# this data has the "expected heterozygosity" and "observed heterozygosity" values from different Pacific herring populations  
data1<-read.csv("Data/heterozygosity.csv")

het<-melt(data1, id.vars="Population")
het2<-pivot_longer(data1, -Population, values_to="Heterozygosity",names_to="Type")


# Copy your output (table) here #
#            Population variable       value
#1                   CA Expected 0.018299443
#2                   BC Expected 0.017096626
#3                   WA Expected 0.014694855
#4          Sitka_Sound Expected 0.016070640
#5  Price_William_Sound Expected 0.015472095
#6           Togiak_Bay Expected 0.014759337
#7                   CA Observed 0.006648856
#8                   BC Observed 0.005857453
#9                   WA Observed 0.004006923
#10         Sitka_Sound Observed 0.005253815
#11 Price_William_Sound Observed 0.004800803
#12          Togiak_Bay Observed 0.004106661


#correlation test
cor.test(data1$Expected, data1$Observed)

shapiro.test(log(sqrt(het2$Heterozygosity)))

wilcox.test(data1$Expected,data1$Observed)

####################################################################
# 2.Column headers are values, not variable names: iris example
####################################################################
#R has many built-in datasets. One popular one is called iris.
iris

# Convert the iris data into a 'tidy' data format
# first assigning iris to your global environment 
iris<-iris


# work though the steps

iris_reshaped<-melt(iris, id.vars="Species")

# How many columns and rows the final tidied data table has?

###  3 columns & 600 rows ###

# Can you copy and paste your tidy data (just paste first several rows) ?
iris_reshaped[1:5,]

#. Species     variable value
#1  setosa Sepal.Length   5.1
#2  setosa Sepal.Length   4.9
#3  setosa Sepal.Length   4.7
#4  setosa Sepal.Length   4.6
#5  setosa Sepal.Length   5.0


ggplot(iris_reshaped, aes(x = Species, y = value))+
    geom_boxplot()+facet_wrap(~variable)


# run PCA
library(vegan)

pca_iris <- rda(iris[,1:4], scale = FALSE)
plot(pca_iris, display = "sites", type = "points")
plot(pca_iris)


nmds_iris <- metaMDS(iris[,1:4], k = 2, trymax = 100, trace = F)
nmds_iris
plot(nmds_iris)


result1 = as.data.frame(scores(nmds_iris)$sites)

result1$Species<-iris$Species

ggplot(result1, aes(x=NMDS1,y=NMDS2, color=Species)) + 
    geom_point() + theme_bw()
    

anosim(iris[,1:4], iris$Species)




####################################################################
## 2 Multiple variables stored in one column: TB dataset
####################################################################

# TB occurrence example from the Tidy Data exercise
# I will walk though this example as it includes string manipulation (you can try it 
# by yourself if you want to challenge yourself!) 

# Download and load the data 
tb<-read.csv("Data/tb.csv", stringsAsFactors = F)

#Look at the row data and think what needs to be done
View(tb)

#The third column "new_sp" is a total for each row, so remove the column
tb<-tb[,-3]
# if you don't know exactly where the column is, you can do something like;
# tb<-tb[,-which(colnames(tb)=="new_sp")]

#remove the name "new_sp_" from column names as they are not necessary
#here we use 'gsub' to replace "new_sp_" with ''(blank), check 
?gsub # for more information
colnames(tb)<-gsub("new_sp_",'',colnames(tb))

#now you can 'melt' the data
tbm<-melt(tb, id.vars=c("iso2","year"))

#look at the melted data
View(tbm)

#Now you need to separate the sex and age into two columns
# First extract the first letter (m or f) using 'substr'
tbm$sex<-substr(tbm$variable,1,1)

#Extract the age group info using substr again
tbm$age<-substr(tbm$variable,2,nchar(as.character(tbm$variable)))

# Now replace the age group label to more meaningful names
# 04 stands for age 0-4, so create the labels and replace them
# This specifies what to be replaced for each string
ages <- c("04" = "0-4", "514" = "5-14", "014" = "0-14", "1524" = "15-24", "2534" = "25-34", "3544" = "35-44", "4554" = "45-54", "5564" = "55-64", "65"= "65+", "u" = "Unknown")
#now using factor, replace all of them  
tbm$age<-factor(ages[tbm$age], levels = ages)

#remove rows with NA 
tbm<-tbm[!is.na(tbm$value),]

# now we don't need 'variable' column
tbm<-tbm[,-which(colnames(tbm)=="variable")]

#change the 1st and 3rd column names
colnames(tbm)[c(1,3)]<-c("country","count")

# How many rows and columns do you have in your final data table?

#### 5 columns, 35750 rows

