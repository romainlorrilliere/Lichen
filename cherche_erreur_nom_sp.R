setwd("/Users/laureturcati/git/LichenGo/data")


d <- read.csv ("data_lichen_rhone_grandest.csv",stringsAsFactors=FALSE)
head(d)

#certains noms d'espèces présentent un espace à la fin -> il doit être supprimé
d$sp <- trimws(d$sp, which = c("both")) 

checknom<-as.data.frame(table(d$sp))
dim(checknom)
head(checknom)

write.csv(checknom, "nom-sp-brute.csv" , row.names=F)