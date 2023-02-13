library(ggplot2)
library(colorspace)
### 

pred<-data.frame(treatment=c("Non-caged", "Caged"),mean=c(9.67, 96.67), SD=c(4.53, 13.3))

ggplot(pred,aes(x=treatment,y=mean, ymin=mean-SD, ymax=mean+SD))+
    geom_bar(stat="identity",width=0.6, color='blue', fill='lightblue')+
    geom_errorbar(width=.2, color="gray30")+
    xlab("")+ylab("Mussel count")+
    theme_bw()+
    theme(axis.text.x = element_text(size=12, color=1))
ggsave("Output/Predation_result_example1.png", width = 3.3, height = 3, dpi=300)




pred2<-data.frame(treatment=c("Non-caged", "Caged"),mean=c(41.5, 55.5), SD=c(12, 17.3))

ggplot(pred2,aes(x=treatment,y=mean, ymin=mean-SD, ymax=mean+SD))+
    geom_bar(stat="identity",width=0.6, color='blue', fill='slategray1')+
    geom_errorbar(width=.2, color="gray30")+
    xlab("")+ylab("Mussel count")+
    theme_bw()+
    theme(axis.text.x = element_text(size=12, color=1))
ggsave("Output/Predation_result_example2.png", width = 3.3, height = 3,dpi=300)
