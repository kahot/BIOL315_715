### Assignment 4 ###

# Create box plots using SIV diversity data
# In this study, monkeys were infected with SIV and Mtb (tuberculosis ) at

#load necessary libraries
library(ggplot2)
library(colorspace)


#Read the data file
summary<-read.csv("Data/Average_Diversity_R21.csv", stringsAsFactors = F)

#In this study, we tracked SIV diversity in 3 different cohorts (groups) of monkeys
# co-infected with SIV and Mtb (bacteria that cause tuberculosis). The 3 cohorts were  SIV only, 
# Latent NR (non-reactivator = Mtb dormant), & Latent R (reactivator=Mtb came back).


## 1. Let's create a box plot of average diversity by cohort 
# (the 'mean' column = the average SIV diversity within each monkey )

ggplot()+
    geom_boxplot(data=summary, aes(x=Cohort, y=mean*100, fill=Cohort))


#Create a color vector

# my color vector (please change the colors as you wish!)
colors<-qualitative_hcl(6, palette="Dark3")




## Improve the figure by using a 'theme' and custom colors
# There are many themes already available: Example https://www.datanovia.com/en/blog/ggplot-themes-gallery/

ggplot()+
    geom_boxplot(data=summary, aes(x=Cohort, y=mean*100, fill=Cohort), 
                 color="gray20", outlier.alpha = 0.6)+
    scale_fill_manual(values=paste0(colors[c(5,3,1)],"66"))+
    xlab('')+ylab('% Average diversity')+
    theme_classic()
    
ggplot()+
    geom_boxplot(data=summary, aes(x=Cohort, y=mean*100, fill=Cohort), 
                 color="gray20", outlier.alpha = 0.6)+
    scale_fill_manual(values=paste0(colors[c(5,3,1)],"66"))+
    xlab('')+ylab('% Average diversity')+
    theme_bw()+
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

# Q1. Try different themes and colors, and save the plot!
install.packages("RColorBrewer")

library(RColorBrewer)
display.brewer.all(type="seq")
display.brewer.all(type="div")
display.brewer.all(type="qual")

colors2<-brewer.pal(n=4, "Accent")
colors2<-c("#7FC97F", "#BEAED4" ,"#FDC086","#FFFF99")

colors3<-c("pink",'red',"royalblue","steelblue", )

### Explore the themes
#https://ggplot2-book.org/polishing.html
# theme_gray()
# theme_calssic()
# theme_minimal()
# theme_void()
ggplot()+
    geom_boxplot(data=summary, aes(x=Cohort, y=mean*100, fill=Cohort), 
                 color="gray20", outlier.alpha = 0.6)+
    scale_fill_manual(values=paste0(colors[c(5,3,1)],"66"))+
    xlab('')+ylab('% Average diversity')+
    theme_light()

# https://r-charts.com/ggplot2/themes/
install.packages('ggthemes')
library(ggthemes)

ggplot()+
    geom_boxplot(data=summary, aes(x=Cohort, y=mean*100, fill=Cohort), 
                 color="gray20", outlier.alpha = 0.6)+
    scale_fill_manual(values=paste0(colors[c(5,3,1)],"66"))+
    xlab('')+ylab('% Average diversity')+
    theme_tufte()


#Violin plot
ggplot()+
    geom_violin(data=summary, aes(x=Cohort, y=mean*100, fill=Cohort), 
                 color="gray20")+
    scale_fill_manual(values=paste0(colors[c(5,3,1)],"66"))+
    xlab('')+ylab('% Average diversity')+
    theme_classic()

## Improve the figure by ordering independent variables (X-axis). This is done by using 'factor'.
# For these data, we want to order cohorts in the following order (severity of co-infection). 
# Otherwise R will sort by alphabetical order.
cohorts<-c("SIV only","Mtb NR","Mtb R")

# Make Cohort as 'factor'
summary$Cohort<-factor(summary$Cohort, levels=c("SIV only","Mtb NR","Mtb R"))

#Plot and compare how it changed:
ggplot()+
    geom_boxplot(data=summary, aes(x=Cohort, y=mean*100, fill=Cohort),
                 color="gray20", outlier.alpha = 0.6)+
    scale_fill_manual(values=paste0(colors[c(5,3,1)],"66"), guide='none')+
    xlab('')+ylab('% Average diversity')+
    theme_bw()


# Now I'd like you to add the mean of each cohort on top of the box plot 
# (the center line in the box represents median, not mean).

# create a new data frame with means for each cohort
cohort<-aggregate(summary["mean"], by=list(summary$Cohort), mean)
colnames(cohort)[1]<-"Cohort"


ggplot()+
    geom_boxplot(data=summary, aes(x=Cohort, y=mean*100, fill=Cohort),
                 color="gray20", outlier.alpha = 0.6)+
    geom_point(data=cohort, aes(x=Cohort, y=mean*100), color="black",size=2.5, shape=4)+
    scale_fill_manual(values=paste0(colors[c(5,3,1)],"66"), guide='none')+
    xlab('')+ylab('% Average diversity')+
    theme_bw()

## Q1. Can you add the means by using stat="summary" function?

ggplot(data=summary, aes(x=Cohort, y=mean*100, fill=Cohort))+
    geom_boxplot(color="gray20", outlier.alpha = 0.6)+
    geom_point(stat='summary', fun='mean', color="black",size=2.5, shape=4)+
    scale_fill_manual(values=paste0(colors[c(5,3,1)],"66"), guide='none')+
    xlab('')+ylab('% Average diversity')+
    theme_bw()





### Q2. Can you overlay all data points to the box plot above?
# You can do so by adding +geom_point(data=summary,aes(x=Cohort, y=mean*100)) together 
# with geom_boxplot. Change the color of points (to a lighter color) to make it look better.
    
plot1<-ggplot()+
    geom_boxplot(data=summary, aes(x=Cohort, y=mean*100, fill=Cohort),
                 color="gray20", outlier.alpha = 0.6)+
    geom_point(data=cohort, aes(x=Cohort, y=mean*100), color="black",size=2.5, shape=4)+
    geom_point(data=summary,aes(x=Cohort, y=mean*100), color="gray70", alpha=.6, position=position_jitter(width=0.1))+
    scale_fill_manual(values=paste0(colors[c(5,3,1)],"66"), guide='none')+
    xlab('')+ylab('% Average diversity')+
    theme_bw()


### Save the plot to your computer!

ggsave(filename = "Output/assignment4_boxplot_scatter.pdf",plot = plot1, width= 5, height= 4)
ggsave(filename = "Output/assignment4_boxplot_scatter.png", width= 5, height= 4, dpi=300)



####################################################################
## 2. Plot the diversity by Tissue type by Cohort

# use position_dodge()

ggplot()+
    geom_boxplot(data=summary, aes(x=Tissue, y=mean*100, fill=Cohort),
               position=position_dodge(width = 0.8),color="gray20", outlier.alpha = 0.6)+
    scale_fill_manual(values=paste0(colors[c(5,3,1)],"66"))+
    xlab('')+ylab('% Average diversity')+
    theme_bw()


## How can you improve the plot to see grouping more clearly? 
# Use geom_vline to add lines to divide each tissue

ggplot()+
    geom_boxplot(data=summary, aes(x=Tissue, y=mean*100, fill=Cohort),
                 position=position_dodge(width = 0.8),color="gray20", outlier.alpha = 0.6)+
    scale_fill_manual(values=paste0(colors[c(5,3,1)],"66"))+
    xlab('')+ylab('% Average diversity')+
    theme_bw()+
    theme(panel.grid.major.x = element_blank())+
    geom_vline(xintercept=1:4+0.5, color="gray50")


## Q3. Can you add the means to the above plot?

# 1) Calculate the averages for each tissues within each cohort
Tis.mean<-

# 2) Order the tissues for both tables
summary$Tissue<-factor(summary$Tissue, levels = c("Plasma","Plasma nex","Peripheral LN","Thoracic LN","Lung"))
Tis.mean$Tissue<-factor(Tis.mean$Tissue, levels = c("Plasma","Plasma nex","Peripheral LN","Thoracic LN","Lung"))


# Plot the box plot and means together grouped by tissue by cohort.
ggplot()+
    

# Q4. Can you overlay all data points to the above plot? 

ggplot()
 


    
    




    
    
### Heterozygosity data:
library(reshape2)
df<-read.csv("Data/heterozygosity.csv")

#correlation analysis


