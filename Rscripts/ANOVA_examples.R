library(reshape2)

data.ex1<-read.csv("Data/Mussels.csv")

Mussel<-melt(data.ex1)
Mussel<-Mussel[!is.na(Mussel$value ),]
colnames(Mussel)<-c("Location","Length")

## Check assumption
# 1. "Shapiro-Wilk normality test".
# **Apply appropriate data transformation if necessary
shapiro.test(Mussel$Length) 

# Visualize with Q-Q plot 
qqnorm(Mussel$Length)

# 2. Levene's Test (testing homogeneity of variance)
install.packages("Rcmdr")
library(Rcmdr)
leveneTest(Length~ Location, data=Mussel)


### One-way ANOVA ###
aov.ex1 = aov(Length~Location,data=Mussel) 
summary(aov.ex1)     
print(model.tables(aov.ex1,"means"),digits=3)      
boxplot(Length~Location,data=Mussel)        

#visualize the residuals
op<-par(mfrow=c(2,2)); plot(aov.ex1); par(op)

# Tukey-HSD
TukeyHSD(x=aov.ex1,'Location', conf.level=0.95)



### Two-Way ANOVA ###

data.ex2=read.csv("Data/tissue_thickness.csv")

# "Shapiro-Wilk normality test" with log transformation
shapiro.test(log(data.ex2$Thickness)) 

#Levene's Test
leveneTest(log(Thickness)~Site*Origin, data=data.ex2)

#Run 2-way ANOVA
aov.ex2 = aov(log(Thickness)~Site*Origin,data=data.ex2)
summary(aov.ex2)                                    
print(model.tables(aov.ex2,"means"),digits=3)      
boxplot(log(Thickness)~Site*Origin,data=data.ex2) 

TukeyHSD(x=aov.ex2,'Site:Origin', conf.level=0.95)
