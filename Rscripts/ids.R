
ids1<-read.table("Data/survey_submitted.txt")
ids<-read.table("Data/survey_submitted2.txt", header = F)

students<-read.csv("Data/participants_new.csv")


colnames(ids1)<-"Username"
colnames(ids)<-"Username"


submitted<-merge(students, ids, by="Username")
submitted

submitted[order(submitted$Last.name),]

sub2<-submitted[!(submitted$Username %in% ids1$Username),]

# Assginmetn 