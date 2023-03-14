# Ordinatio with vegan 3/14/23

# Install and load the following packages
install.packages("vegan")

library(vegan)


# Load the 'varespec' community data (comes with vegan)
# 'varspec' contains estimated plant cover values of 44 species from 24 samples (locations)
data(varespec)

# Look at the dataset
View(varespec)

### 1. PCA  ###

# Run PCA using rda function
pca1 <- rda(varespec, scale = FALSE)

# Plot a bar plot the percentage variance explained by each axis
barplot(as.vector(pca1$CA$eig)/sum(pca1$CA$eig)) 

# Calculate the percent of variance explained by first two axes
sum((as.vector(pca1$CA$eig)/sum(pca1$CA$eig))[1:2])

# Plot the results
plot(pca1)
#This shows both samples and species information (the numbers = rownames)

#If you want to see how different samples group/cluster:
# Here 'sites' represents 'samples/observations'
plot(pca1, display = "sites", type = "points")

# 'species' represents each species or OTU (column)
plot(pca1, display = "species", type = "text")


## Create a biplot:
# Here, 'species' scores are drawn as arrows that point in the direction of increasing values for that variable

# Plot PCA1 and PCA2
biplot(pca1, choices = c(1,2), type = c("text", "points")) 

# Plot PCA1 and PCA3
biplot(pca1, choices = c(1,3), type = c("text","points")) 

# These plots offer an insight into which species abundance is driving the observed pattern


# You can extract the species and site scores for further analyses:
sitePCA <- pca1$CA$u # Site scores
speciesPCA <- pca1$CA$v # Species scores



### 2. NMDS ###

# We will run NMDS on the same dataset

# 1. create a distance matrix. We will use bray-curtis distance, which is recommended for abundance data
dist <- vegdist(varespec,  method = "bray")

# Before you run NMDS, ideally you want to know which k (dimension) to choose: 

# Here is a function NMDS.scree(), which will run NMDS 10 times and plot stress vs. k (1-10) 
NMDS.scree <- function(x) {
    plot(rep(1, 10), replicate(10, metaMDS(x, autotransform = F, k = 1)$stress), xlim = c(1, 10),ylim = c(0, 0.30), xlab = "# of Dimensions", ylab = "Stress", main = "NMDS stress plot")
    for (i in 1:10) {
        points(rep(i + 1,10),replicate(10, metaMDS(x, autotransform = F, k = i + 1)$stress))
    }
}

# Use the function that we just defined to choose the optimal # of dimension
NMDS.scree(dist)




# Run the NMDS analysis with k=2 (2 dimensions) and check the result
NMDS1 <- metaMDS(dist, k = 2, trymax = 100, trace = F)
# trymax -> maximum numbers of random starts in search of stable solution.

# Let's check the results
NMDS1

#Plot the results
plot(NMDS1, type = "t")

#You can visualize the stress based on observed dissimilarity
stressplot(NMDS1)


# To add the species component (like PCA), you need to run a little differently:
# Here, we enter the original data as they are (without calculating a distance matrix) and specify "bray" as your distance of chice
# This will retain the species information
NMDS2 <- metaMDS(varespec, k = 2, trymax = 100, trace = F, autotransform = FALSE, distance="bray")

#Plot the results
plot(NMDS2, type = "t")


###  Fitting environmental variables ###
# The corresponding environmental data set for varespec is varechem 
data(varechem)

View(varechem)

# Run the envfit function to the NMDS results with the metadata 
ef <- envfit(NMDS2, varechem, permu = 999)

# See the results, which will show you the correlation coefficient of each factor and associated p-value 
ef

#The results are helpful to identify which factors are signficantly shaping the observed pattern


# Create a plot with environmental vectors 
plot(NMDS2, type = "t", display = "sites")
plot(ef)

# Plot only the vectors of the significant correlations
plot(NMDS2, type = "t", display = "sites")
plot(ef, p.max = 0.05)



## This plot can be made prettier with ggplot! ##
library(ggplot2)

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

