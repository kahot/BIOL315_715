library(ggplot2)

x = sample(1:10, 10, replace=TRUE)
y = sample(1:10, 10, replace=TRUE)



#x = sample.int(10, replace=TRUE)
#y = sample.int(10, replace=TRUE)


plot(x,y, pch=22, cex=5)+
    axis(side=c(1,2), at=1:10, labels=1:10)+
    abline(h=1:9, col = "gray",  cex=0.6)+
    abline(v=1:9, col = "gray", cex=0.6)


quadrats<-data.frame(x=x, y=y)

ggplot(quadrats, aes(x=x, y=y))+
    geom_point(shape=22, size=12)+
    scale_x_continuous(breaks=0:10, limits=c(0,10))+
    scale_y_continuous(breaks=0:10, limits=c(0,10))+
    theme_bw()+
    theme(panel.grid.minor=element_blank())
ggsave("randam_quadrat.png", width = 5, height = 5, dpi=300)

quad2<-quadrats
quad2[!is.na(quad2)]<-NA
ggplot(quad2, aes(x=x, y=y))+
    geom_point(shape=22, size=12)+
    scale_x_continuous(breaks=0:10, limits=c(0,10))+
    scale_y_continuous(breaks=0:10, limits=c(0,10))+
    theme_bw()+
    theme(panel.grid.minor=element_blank())
ggsave("randam_quadrat_empty.png", width = 5, height = 5, dpi=300)

x<-sample(1:100, 10, replace = FALSE)

quadrats2<-data.frame(x=x2, y=0)

library(ggh4x)
ggplot(quadrats2, aes(x=x, y=y))+
    geom_point(shape=22, size=12)+
    scale_x_continuous(breaks=seq(0,100,10),minor_breaks = seq(0,100, by=1), limits=c(0,100))+
    scale_y_continuous(breaks=0)+
    theme_minimal()
theme(panel.grid.minor)
ggsave("randam_transect.png", width = 7, height = 1, dpi=300)

linet<-quadrats2
linet$x<-200
ggplot(linet, aes(x=x, y=y))+
    geom_point(shape=22, size=12)+
    scale_x_continuous(breaks=seq(0,100,10),minor_breaks = seq(0,100, by=1), limits=c(0,100))+
    scale_y_continuous(breaks=0)+
    theme_minimal()
ggsave("randam_transect_empty.png", width = 7, height = 1, dpi=300)
