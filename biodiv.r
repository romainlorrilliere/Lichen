
library(data.table)
library(dplyr)
library(ggplot2)


prepa <- function() {

    d <- fread("data/data_spclean_lichen_rhone_grandest.csv")
    d <- subset(d,region == "Rhone")
    d$face <- gsub("_","",d$face)
    d$face_id <-  paste0(d$arbre_id,"_",d$face)
    d$sample_id <- paste0(d$face_id,"_",d$sample)
    d_sample <- unique(subset(d,select=c("site_id","arbre_id","arbre_num","face_id","face","sample_id","sample","lon_l93","lat_l93")))

#    d_sp_face <- aggregate(presence~face_id + sp,d, sum)
    d_sp_sample.w <- dcast(d, sample_id ~sp,value.var = "ab")
    d_sp_sample.w[is.na(d_sp_sample.w)] <- 0
    d_sp_sample <-  melt(d_sp_sample.w, id.vars=c("sample_id"))
    colnames(d_sp_sample)[2:3] <- c("sp","recouvr")

    d_sample <- inner_join(d_sample,d_sp_sample)

    d_sample$presence <- ifelse(d_sample$recouvr>0,1,0)
    return(d_sample)
}




get_Q <- function(d,plot=TRUE) {

    d <- data.table(d)
    d <- d[presence > 0]
    d <- unique(d[,.(site_id,sp)])

    d <- d[,q := .N,by=site_id]
    d <- d[,q := q-1]
    dq <- aggregate(q~sp,data=d,mean)
    dq <- dq[order(dq$q,decreasing=TRUE),]
    print(head(dq))

    if(plot) {
        dq$sp <-  factor(dq$sp, levels=rev(dq$sp))
        gg <- ggplot(data=dq,aes(x=sp,y=q)) + geom_point() + coord_flip()
        gg <- gg + labs(x="",y="Q")
        print(gg)
        ggsave("output/sp_Q.png",gg,width=6,height=10)
    }

    write.csv(dq,"output/sp_Q.csv",row.names=FALSE)
    return(d_q)
}



get_VDL <- function(d,plot=TRUE) {

    d_maille <- aggregate(recouvr~site_id + arbre_id + face_id + face + sp , d, mean)
    d_freq <- aggregate(presence~site_id + arbre_id + face_id + face + sp ,d,sum)
    dd <- inner_join(d_maille,d_freq)

    dd$prod <- dd$recouvr * dd$presence

    d_site_face <- aggregate(prod~site_id+face,dd,mean)
    d_vdl <- aggregate(prod~site_id,d_site_face,sum)
    colnames(d_vdl)[2] <- "vdl"

    ## LDV
    ## each arbre sum freq toute espèces par face
    ## mean par face pour la station
    ## additionne les moyenne  -> LDV

    print(summary(d_vdl))

    if(plot) {
        gg <- ggplot(data=d_vdl,aes(x="",y=vdl)) + geom_violin() + geom_boxplot(notch=TRUE,width=.2, outlier.colour=NA)+ geom_jitter(width = 0.05,alpha=0.3)
        gg <- gg + labs(x="",y="VDL",title="Distribution du VDL (IBLE) sur les sites",subtitle="Les 3 quantiles 0.25, 0.5, 0.75")

        print(gg)
        ggsave("output/site_vdl.png",gg)
    }

    write.csv(d_vdl,"output/site_vdl.csv",row.names=FALSE)
    return(d_vdl)
}



get_ipa <- function(d,dq) {
    ## IPA
    ## F transforme pour valeur entre 1..5
    ## somme des de somme par site de Q(sp)*F(sp)/10 -> IPA

    d_site <- aggregate(presence~site_id+face+sp,d,sum)
    d$freq1_5<- (d$abondance-1)/(5-1)




}











