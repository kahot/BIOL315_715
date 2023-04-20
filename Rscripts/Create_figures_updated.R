## Create plots using ggplot ##

#if you don't have ggplot2 yet, first install the package. 
install.packages('ggplot2')

#load necessary packages
library(ggplot2)
library(reshape2)
install.packages('colorspace')
install.packages('RColorBrewer')

library(colorspace)
library(RColorBrewer)



# Using the examples from last week 

# 1) Mussel size data
ex1<-read.csv("Data/Mussels.csv")

#reshape the data
Mussel<-melt(ex1)

#remove NA from the table
Mussel<-Mussel[!is.na(Mussel$value ),]

#Assign column names
colnames(Mussel)<-c("Location","Length")



##You can visualize the data quickly (though not necessarily the most beautiful plot)
boxplot(Length~Location,data=Mussel)        
# For more about customizing the base plot, http://www.sthda.com/english/wiki/r-base-graphs

# Let's create the same plot using ggplot

ggplot(data=Mussel, aes(x=Location, y=Length))+
    geom_boxplot()

# You can use different theme to customize the look of the plot

ggplot(data=Mussel, aes(x=Location, y=Length))+
    geom_boxplot()+
    theme_classic()

# Change the x and y axis lab, add a title 
ggplot(data=Mussel, aes(x=Location, y=Length))+
    geom_boxplot()+
    theme_classic()+
    xlab('')+ylab("Length (mm)")+
    ggtitle('The length of the anterior adductor of mussels')

# Lastly to save a plot you just created to your local computer:
ggsave("Output/Mussel_length.png", width=6, height=4, dpi=300)
# You need to specify the size and resolution

#you can save in a pdf file
ggsave("Output/Mussel_length.pdf", width=6, height=4)

# 'ggasave' will save the last plot you created
# If you want to save a specific plot, you need to make the plot as an object by assigning a name;

plot1<-ggplot(data=Mussel, aes(x=Location, y=Length))+
    geom_boxplot()+
    theme_bw()+
    xlab('')+ylab("Length (mm)")+
    ggtitle('The length of the anterior adductor of mussels')

#then save the specific plot
ggsave(file="Output/Mussel_length2.pdf",plot1, width=6, height=4)


## Now let's plot the means and SDs of the same data:
# You can do this in several different ways:

# 1: Create a new table with means and sds
# use 'aggregate' to create a summary table of mean
means<-aggregate(Mussel$Length, by=list(Mussel$Location), mean)
colnames(means)<-c("Location", "mean")
# use 'aggregate' to create a summary table of SD
sds<-aggregate(Mussel$Length, by=list(Mussel$Location), sd)
colnames(sds)<-c("Location", "sd")

#You can merge the two tables
means<-merge(means, sds, by="Location")

# 2. Alternatively, the same processes can be done in 1 step by using 'dplyr' package:
install.packages('dplyr')
library(dplyr)
means2<-Mussel %>% group_by(Location) %>% 
            summarise(mean=mean(Length), sd=sd(Length)) %>% 
        as.data.frame()

## Here, 1 and 2 create the same summary table. You can use whichever method you like. 
# I personally haven't use dplyr as much as I should...

# just to visually compare...
ggplot(data=means, aes(x=Location, y=mean))+
    geom_point()+
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd))+
    theme_classic()+
    xlab('')+ylab("Length (mm)")
    
ggplot(data=means2, aes(x=Location, y=mean))+
    geom_point()+
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd))+
    theme_classic()+
    xlab('')+ylab("Length (mm)")


## Another way is to use ggplot to calculate mean and SD: 
ggplot(data=Mussel, aes(x=Location, y=Length))+
    geom_point(stat="summary", fun="mean")+
    geom_errorbar(stat="summary",
                  fun.min=function(x) {mean(x)-sd(x)}, 
                  fun.max=function(x) {mean(x)+sd(x)})+
    theme_classic()+
    xlab('')+ylab("Length (mm)")


# Now let's make the plot nicer:
# Change the width of error bars and the point size

ggplot(data=means, aes(x=Location, y=mean))+
    geom_point(size=3)+
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width=0.2)+
    theme_classic()+
    xlab('')+ylab("Length (mm)")+
    ylim(0,0.15)






## 2) More complicated plots: Mussel preparation data (from before)

ex2=read.csv("Data/Predation_exp.csv")
predation<-melt(ex2, id.vars="Location")
colnames(predation)[2:3]<-c("Treatment","Mussel_count")


#this is the base plot
boxplot(Mussel_count~Location*Treatment,data=predation) 


#Using ggplot, create the same figure
ggplot(predation, aes(x=Treatment, y=Mussel_count, color=Location))+
    geom_boxplot(position=position_dodge(width=1))


#The data can be also presented as follows:
ggplot(predation, aes(x=Location, y=Mussel_count, color=Treatment))+
    geom_boxplot(position=position_dodge(width=1))


# which is better? 


#Assign colors
# install.packages('colorspace')
library(colorspace)

#Create a color vector
# * show palette colors from 'colorspace'
hcl_palettes(plot = TRUE)
hcl_palettes("qualitative", plot = TRUE)

# my color vector
colors<-qualitative_hcl(6, palette="Dark3")
# See the colors
hcl_palettes("qualitative", n=6, plot = TRUE)

ggplot(predation, aes(x=Location, y=Mussel_count, color=Treatment))+
    geom_boxplot(position=position_dodge(width=1))+
    scale_color_manual(values=c("blue","green"))



## using RColorBrewer
display.brewer.all()
# pick qualitative palette
brewer.pal(n = 8, name = "Set2")
# this will set the palette function to pick "Set2"
palette(brewer.pal(n = 8, name = "Set2"))

# To use RColorBrewer palette, you can use scale_color_brewer function 
ggplot(predation, aes(x=Location, y=Mussel_count, color=Treatment))+
    geom_boxplot(position=position_dodge(width=1))+
    scale_color_brewer(palette = "Dark2")

# (or you can simply create a color vector like the above example:)
brewcolors<-brewer.pal(n = 8, name = "Dark2")

ggplot(predation, aes(x=Location, y=Mussel_count, color=Treatment))+
    geom_boxplot(position=position_dodge(width=1))+
    scale_color_manual(values = brewcolors)



# make it nicer by adding 'fill'

ggplot(predation, aes(x=Location, y=Mussel_count, color=Treatment, fill=Treatment))+
    geom_boxplot(position=position_dodge(width=1))+
    scale_color_manual(values = colors)+
    scale_fill_manual(values = paste0(colors, "99"))


    
    

# 3) Scatter plot
# I know some of you are working on creating a PCA plot. We will have a class on ordination later but 
# we can create some scatter plots today with colors.

# Ex1) Correlation plot
# Here, the example is to see how the average read depth and pi (nucleotide diversity) are correlated from one of my data that I'm working on.

# The following data contain information for different populations of Pacific herring. 
reads<-read.csv("Data/read.depth_pi.csv")

#run correlation analysis to obtain the correlation coefficient 
cor.test(reads$depth, reads$mean_pi, method = "spearman")


# Here I want to order the population based on locations 
reads$pop<-factor(reads$pop, levels=c("TB","PWS","SS", "BC","WA","CA"))


# Plot the data, with different colors for different populations 
ggplot(reads, aes(x=depth, y=mean_pi, color=pop))+
    geom_point(size=3)+
    xlab("Mean read depth")+
    ylab(expression(paste("Mean ", pi)))+
    theme_bw()

# Add the 'rho' value (correlation coefficient)
ggplot(reads, aes(x=depth, y=mean_pi, color=pop))+
    geom_point(size=3)+
    xlab("Mean read depth")+
    ylab(expression(paste("Mean ", pi)))+
    theme_bw()+
    annotate('text', x=1.28, y=0.00325, label="rho = 0.75**", size=3)+
    scale_color_manual(values=colors)+ theme(legend.title = element_blank())

# If you want to use Greek letter for rho:
rho <- expression("Spearman's"~rho == 0.75~"**")

ggplot(reads, aes(x=depth, y=mean_pi, color=pop))+
    geom_point(size=3)+
    xlab("Mean read depth")+
    ylab(expression(paste("Mean ", pi)))+
    theme_bw()+
    annotate('text', x=1.22, y=0.00325, parse=T, label=as.character(rho), size=3)+
    scale_color_manual(values=colors)+ theme(legend.title = element_blank())


# PCA plot
# You have to calculate eigenvectors first.
# This data table contains eigenvectors for different Pacific herring populations

pca<-read.csv("Data/pca_herring.csv")
#again, I am ordering the populations based on locations (latitude)
pca$pop<-factor(pca$pop, levels=c("TB","PWS","SS", "BC","WA","CA"))


# 1) color by population
ggplot()+
    geom_point(data = pca, aes(x = PC1, y = PC2, fill = pop, color = pop), shape=21, size = 3)+
    scale_fill_manual(values=paste0(colors,"66"), guide="none")+
    scale_color_manual(values=colors, name="Population")+
    theme_bw()


# 2) color by population and shape by year

ggplot()+
    geom_point(data = pca, aes(x = PC1, y = PC2, fill = pop, color = pop, shape = factor(year)), size = 3)+
    scale_shape_manual(values=c(23,25,3,3,21), name="Year")+
    scale_fill_manual(values=paste0(colors,"99"), guide="none")+
    scale_color_manual(values=colors, name="Population")+
    theme_bw()
    
