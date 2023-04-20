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

# Species accumulation curve for # of individulas
plot(sp$individuals, sp$richness, xlab="# of Individuals", ylab="Species richness", type="l")

# Species accumulation curve for # of samples/sites
plot(sp$sites, sp$richness, xlab="# of Sites", ylab="Species richness", type="l")



###@@ Q5. Conduct rarefaction analysis on 'BCI' data (BCI comes with vegan and contains 'Barro Colorado Island Tree Counts' data)
###@@ what do you think of the sampling effort? Was it enough? How does it compare to the 'dune' datset?
data(BCI)






### 3. PCA / NMDS  ###

# For species abundance data, it is recommended to apply Hellinger transformation.
# This converts the data from absolute to relative values and square roots them.

#Use 'decostnad' function
duneh<-decostand(dune,method="hellinger")

# Run PCA (NMDS is preferable)
pca1 <- rda(duneh, scale = FALSE)

# Plot a bar plot the percentage variance explained by each axis
barplot(as.vector(pca1$CA$eig)/sum(pca1$CA$eig)) 


# Calculate the percent of variance explained by the first two axes
## Below shows the eigenvalues
pca1$CA$eig # Eigenvalues = the total amount of variance that can be explained by a given principal component. 

# assign these values to a vector
eigen<-pca1$CA$eig 

# % explained by PC1 is obtained by
eigen[1]/sum(eigen)


# % explained by PC1 and PC2:
sum(eigen[1:2])/sum(eigen)


###%%% Q6. Calculate the percent of variance explained by the first three axes. ###%%%



# Plot the results
plot(pca1)
#This shows both samples and species information 
# Each number = a site, species in red

#Use biplot to create a more meaningful plot
# Plot PCA1 and PCA2
biplot(pca1, choices = c(1,2), type = c("text", "points")) 


#Another way to visualize the results
ordiplot(pca1,display="species",type="text",col="red")

ordiplot(pca1,display="sites")



# You can extract the species and site scores for further analyses:
sitePCA <- pca1$CA$u # Site scores
speciesPCA <- pca1$CA$v # Species scores





### NMDS ###

nmds1<-metaMDS(duneh, k=3,trymax = 100,  autotransform = FALSE, distance="bray")
ordiplot(nmds1, type='text')

# sample (sites) only
ordiplot(nmds1, display="sites")

#save the NMDS axis scores
result1 = as.data.frame(scores(nmds1)$sites)


# 3D plot
library(vegan3d)
ordiplot3d(nmds1, display="sites")

#different 3D plot#
ordirgl(nmds1, display="sites", type='text', col="blue",scaling=3)
orglpoints(nmds1, display = "sites", choices = 1:3, col = "black")

library(scatterplot3d)
library(rgl)
ordiplot3d(nmds1, display="sites")

#Interactive 3D plot#
ordirgl(nmds1, display="sites", type='text', col="blue",scaling=3)

## use different color to separate the types of sites

# The corresponding environmental data set for dune is dune.env
data(dune.env)

View(dune.env)
#A1 = thickness of soil

pasture<-which(dune.env$Use=='Pasture')
hay<-which(dune.env$Use=='Hayfield')
haypas<-which(dune.env$Use=='Haypastu')
use1=nmds1$points[,]  
pel31.3=f31.mds2$points[-sed31,]

ordirgl(nmds1, display="sites", type='n')

points3d(nmds1$points[pasture], col="royalblue",pch=19)
points3d(nmds1$points[hay], col="tan",pch=19)
points3d(nmds1$points[haypas], col="darkgreen",pch=19)
points3d

###  Fitting environmental variables ###
# The corresponding environmental data set for dune is dune.env
data(dune.env)

View(dune.env)
#A1 = thickness of soil


# Run the envfit function to the NMDS results with the metadata 
evfit <- envfit(nmds1 ~ A1+Moisture+Manure, dune.env, permu = 999)

# See the results, which will show you the correlation coefficient of each factor and associated p-value 
envfit


# Create a plot with environmental vectors 
plot(nmds1, type = "n", display = "sites")
ordihull(evfit, Management, col="blue")
plot(envfit)
ordisurf(nmds1, A1, add=TRUE)
# Plot only the vectors of the significant correlations
plot(nmds1, type = "t", display = "sites")
plot(envfit, p.max = 0.05)



# Extract the NMDS1 and NMDS2 score from the results
result1 = as.data.frame(scores(NMDS2)$sites)

#Extract envfit values
ev.scrs<-as.data.frame(scores(ef, display = "vectors"))
ev.scrs$factor <-rownames(ev.scrs)


ggplot() + 
    geom_point(data=result1, aes(x=NMDS1,y=NMDS2), size=2, color="steelblue") + theme_bw()+
    coord_fixed()+
    geom_segment(data=ev.scrs, aes(x=0, xend=NMDS1, y=0, yend=NMDS2), 
                 arrow=arrow(length=unit(0.25, "cm")), color="gray")+
    #theme(panel.grid = element_blank())+
    geom_text(data = ev.scrs, aes(x = NMDS1, y = NMDS2, label = factor),
              size = 3)+
    ylim(-1, 1)+xlim(-1,1.5)

# Note: if you want to only plot the factors with p<0.05, 
ev.scrs2<-ev.scrs
ev.scrs2$pval<-ef$vectors[['pvals']]
ev.scrs2$NMDS1[ev.scrs2$pval>0.05]<-NA
ev.scrs2$NMDS2[ev.scrs2$pval>0.05]<-NA
ggplot() + 
    geom_point(data=result1, aes(x=NMDS1,y=NMDS2), size=2, color="steelblue") + theme_bw()+
    coord_fixed()+
    geom_segment(data=ev.scrs2, aes(x=0, xend=NMDS1, y=0, yend=NMDS2), 
                 arrow=arrow(length=unit(0.25, "cm")), color="gray")+
    theme(panel.grid = element_blank())+
    geom_text(data = ev.scrs2, aes(x = NMDS1, y = NMDS2, label = factor),
              size = 3)+
    ylim(-1, 1)+xlim(-1,1.5)


# Compare with environmental data
data(dune.env)

View(dune.env)
#A1 = thickness of soil






## What if these samples are taken from two different regions and want to plot with different colors? 
# Here is the hypothetical regions data I created
vareloc<-read.csv("Data/vareloc.csv")

#This will add the region info to score data (make sure the sample order is the same! It is safer to use 'merge' if you are not sure)
result1$Region<-vareloc$region

# Now plot with different colors 
ggplot() + 
    geom_point(data=result1, aes(x=NMDS1,y=NMDS2, color=Region), size=2) + theme_bw()+
    coord_fixed()+
    geom_segment(data=ev.scrs, aes(x=0, xend=NMDS1, y=0, yend=NMDS2), 
                 arrow=arrow(length=unit(0.25, "cm")), color="gray")+
    theme(panel.grid = element_blank())+
    geom_text(data = ev.scrs, aes(x = NMDS1, y = NMDS2, label = factor),
              size = 3)+
    ylim(-1, 1)+xlim(-1,1.5)

# This plot offers more biologically meaningful insight. 


# If you want to test if 2 regions are significantly different, you can run ANOSIM function 

ano<-anosim(varespec, vareloc$region,permutations=1000)
ano 


