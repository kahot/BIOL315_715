# Community Data Analysis: Diversity: 4/18/23

# Using the Dutch dune meadow data from vegan, we will look at the community composition and diversity

# Load vegan and other libraries
#install.packages("vegan")
library(vegan)
library(reshape2)
library(ggplot2)

# Load the 'dune' data set (comes with vegan) -Vegetation and Environment in Dutch Dune Meadows
# dune = Vegetation Dutch Dune Meadows, 20 sites with 30 species
data(dune)

# Look at the dataset
View(dune)



### 1. Diversity ###

## Calculate diversity indices

# 1.1. Simpson's diversity index
diversity(dune,index = "simpson") 

d1<-diversity(dune,index = "simpson") 

###@@ Q1. What are the minimum and maximum Simpson's diversity indices?  ###


###@@ Q2. Which site has the highest Simpson's diversity?



## 1.2. Shannon's diversity index 

diversity(dune, index = "shannon")

###@@ Q3. What are the minimum and maximum Shannon's diversity indices?  ###


###@@ Q4. Which site has the highest Simpson's diversity?



# if we have time... 
# check if the 2 indices show similar diversity levels

d1<-diversity(dune,index = "simpson") 
d2<-diversity(dune, index = "shannon")

diversity<-data.frame(site=1:20, Simpson=d1, Shannon=d2)

diversity2<-melt(diversity, id.vars="site")
colnames(diversity2)[2:3]<-c("Index","diversity")

ggplot(diversity2, aes(x=site, y=diversity, fill=Index))+
    geom_bar(stat="identity")+
    theme_minimal()+
    scale_fill_manual(values=c("pink","lightblue"))

# you can add "position=position_dodge()" 
ggplot(diversity2, aes(x=site, y=diversity, fill=Index))+
    geom_bar(stat="identity", position=position_dodge())+
    theme_minimal()+
    scale_fill_manual(values=c("pink","lightblue"))


# If you are really interested in the difference between the two indicies,
# you can standardize or normalize the values and compare them:

## a) standardize the values using scale function

scale(diversity$Simpson)

diversity$Simp_scaled<-as.vector(scale(diversity$Simpson))
diversity$Shan_scaled<-as.vector(scale(diversity$Shannon))

# melt the select variables
diversity3<-melt(diversity[,c(1,4:5)], id.vars="site")

# assign column names
colnames(diversity3)[2:3]<-c("Index","diversity")

#plot the results
ggplot(diversity3, aes(x=site, y=diversity, fill=Index))+
    geom_bar(stat="identity", position=position_dodge())+
    theme_minimal()+
    scale_fill_manual(values=c("pink","lightblue"))


## b) normalize the values 
#define normalization function (BBmisc pacakge has 'normalize' function)
min_max_norm <- function(x) {
    (x - min(x)) / (max(x) - min(x))
}

#apply the function
diversity$Simp_norm<-as.vector(min_max_norm(diversity$Simpson))
diversity$Shan_norm<-as.vector(min_max_norm(diversity$Shannon))

# melt the select variables
diversity4<-melt(diversity[,c(1,6:7)], id.vars="site")

# assign column names
colnames(diversity4)[2:3]<-c("Index","diversity")

#plot the results
ggplot(diversity4, aes(x=site, y=diversity, fill=Index))+
    geom_bar(stat="identity", position=position_dodge())+
    theme_minimal()+
    scale_fill_manual(values=c("pink","lightblue"))


### 1.3. beta diversity ###
#Use 'betadiver' function to calculate beta diversity

#Calculate the Whittaker's beta diversity ('w')
w1 <- betadiver(dune, method="w")
mean(w1)



### 2. Rarefaction  ###

## Was sampling enough to detect each species? ##

# We can use 'rarefy' function to calculate "expected" # of species IF only the minimum number of individuals were present in each site

# Calculate the total number of individuals in each site: 
rowSums(dune)

# and find the smallest number of observation
mincount<-min(rowSums(dune))

#Use 'rarefy' function
rarefy(dune, mincount)


#Use 'rarecurve' function to plot the curve for each site

rarecurve(dune, col = "blue")

# Add lines from rarefy values
rarecurve(dune, sample=mincount, col = "blue")


## Plot observed vs. expected 
# - Number of observed species
obs<-specnumber(dune)

# - Number of expected species
exp<-rarefy(dune, mincount)

# Using the base 'plot' function, quickly visualize;
plot(obs, exp, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species", pch=16, col="blue")
abline(0, 1, lty=2, col="gray50")



# We can also look at the aggregate curve to see if the data captured the overall diversity 

# 'specaccum' finds species accumulation curves for the number of samples/individuals 
sp<-specaccum(dune, method="rarefaction")

# Species accumulation curve for # of individuals
plot(sp$individuals, sp$richness, xlab="# of Individuals", ylab="Species richness", type="l")

# Species accumulation curve for # of samples/sites
plot(sp$sites, sp$richness, xlab="# of Sites", ylab="Species richness", type="l")



###@@ Q5. Conduct rarefaction analysis on 'BCI' data (BCI comes with vegan and contains 'Barro Colorado Island Tree Counts' data)
###@@ what do you think of the sampling effort? Was it enough? How does it compare to the 'dune' dataset? Please include at least one plot you created 
data(BCI)





