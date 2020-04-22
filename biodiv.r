
library(data.table)
library(dplyr)
d <- fread("data/data_spclean_lichen_rhone_grandest.csv")
d <- subset(d,region == "Rhone")
d$face <- gsub("_","",d$face)
d$presence <- 1
d$face_id <-  paste0(d$arbre_id,"_",d$face)
d_face <- unique(subset(d,select=c("site_id","arbre_id","face_id","face")))
d_sp_face.w <- dcast(d, face_id ~sp,value.var = "presence",fun.aggregate=sum)
d_sp_face.w[is.na(d_sp_face.w)] <- 0
d_sp_face <-  melt(d_sp_face.w, id.vars=c("face_id"))
colnames(d_sp_face)[2:3] <- c("sp","abondance")

d_face <- inner_join(d_face,d_sp_face)

d_arbre <- aggregate(abondance~site_id+arbre_id+face+sp,d_face,sum)
d_site <- aggregate(abondance~site_id+face+sp,d_face,sum)






