#Tidy Data: Week 3 Assignment

# Enter all codes into this file
# Copy and paste the output below each assignment
library(stringr)
library(reshape2)
library(tidyr)

#Tidy up the messy dataset  

####################################################################
# 1.Column headers are values, not variable names: simple example
####################################################################
# Using the heterozygosity data (heterozygosity.csv) to practice tidying up the data 
# (you can use either reshape2 or tidyr)

# this data has the "expected heterozygosity" and "observed heterozygosity" values from different Pacific herring populations  
data1<-read.csv("Data/heterozygosity.csv")


# Copy your output (table) here #



####################################################################
# 2.Column headers are values, not variable names: iris example
####################################################################
#R has many built-in datasets. One popular one is called iris.
iris

# Convert the iris data into a 'tidy' data format
# first assigng iris to your global environment 
iris<-iris


#work though the steps
# How many columns and rows the final tidied data table has?


# Can you copy and paste your tidy data (just paste first several rows) ?

####################################################################
## 2 Multiple variables stored in one column: TB dataset
####################################################################

# TB occurrence example from the Tidy Data exercise
# I will walk though this example as it includes string manipulation (you can try it 
# by yourself if you want to challenge yourself!) 

# Download and load the data 
tb<-read.csv("Data/tb.csv", stringsAsFactors = F)

#Look at the row data and think what needs to be done
view(tb)

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
view(tbm)

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


