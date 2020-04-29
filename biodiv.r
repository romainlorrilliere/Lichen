
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
    return(dq)
}



get_IBLE <- function(d,plot=TRUE) {

    d_maille <- aggregate(recouvr~site_id + arbre_id + face_id + face + sp , d, mean)
    d_freq <- aggregate(presence~site_id + arbre_id + face_id + face + sp ,d,sum)
    dd <- inner_join(d_maille,d_freq)

    dd$prod <- dd$recouvr * dd$presence

    d_site_face <- aggregate(prod~site_id+face,dd,mean)
    d_ible <- aggregate(prod~site_id,d_site_face,sum)
    colnames(d_ible)[2] <- "ible"



    print(summary(d_ible))

    if(plot) {
        gg <- ggplot(data=d_ible,aes(x="",y=ible)) + geom_violin(draw_quantles=c(.25,.5,.75)) + geom_boxplot(notch=TRUE,width=.2, outlier.colour=NA)+ geom_jitter(width = 0.05,alpha=0.3)
        gg <- gg + labs(x="",y="IBLE",title="Distribution du IBLE sur les sites",subtitle="Les 3 quantiles 0.25, 0.5, 0.75")

        print(gg)
        ggsave("output/site_ible.png",gg)
    }

    write.csv(d_ible,"output/site_ible.csv",row.names=FALSE)
    return(d_ible)
}



get_VDL <- function(d,plot=TRUE) {
### Louis-rose, S., 2012. Air quality - Biomonitoring with lichens - Assessing epiphytic lichen diversity.
### LDV
### each arbre sum freq toute espèces par face
### mean par face pour la station
### additionne les moyenne  -> LDV

    d_freq_tree <- aggregate(presence~site_id + arbre_id ,d,sum)
    d_freq_tree$freq <- d_freq_tree$presence
    d_vdl <- aggregate(freq~site_id,d_freq_tree,mean)
    colnames(d_vdl)[2] <- "vdl"

    ## LDV
    ## each arbre sum freq toute espèces par face
    ## mean par face pour la station
    ## additionne les moyenne  -> LDV

    print(summary(d_vdl))

    if(plot) {
        gg <- ggplot(data=d_vdl,aes(x="",y=vdl)) + geom_violin(draw_quantiles=c(.25,.5,.75)) + geom_boxplot(notch=TRUE,width=.2, outlier.colour=NA)+ geom_jitter(width = 0.05,alpha=0.3)
        gg <- gg + labs(x="",y="VDL",title="Distribution du VDL sur les sites",subtitle="Les 3 quantiles 0.25, 0.5, 0.75")

        print(gg)
        ggsave("output/site_vdl.png",gg)
    }

    write.csv(d_vdl,"output/site_vdl.csv",row.names=FALSE)
    return(d_vdl)
}




get_ipa <- function(d,dq,plot=TRUE) {
    ## IPA
    ## F transforme pour valeur entre 1..5
    ## somme des de somme par site de Q(sp)*F(sp)/10 -> IPA

    require(scales)
    require(dplyr)

    d_site <- aggregate(presence~site_id+sp,d,sum)
    d_site$freq1_5 <-  scales::rescale(d_site$presence, to = c(1,5),from=c(0,100))

    d_site$sp <- as.character(d_site$sp)
    dq$sp <- as.character(dq$sp)

    d_site <- inner_join(d_site,dq)

    d_site$iap_sp <- d_site$freq1_5 * d_site$q /10

    d_ipa <- aggregate(iap_sp~site_id,d_site, sum)
    colnames(d_ipa)[2] <- "ipa"

   print(summary(d_ipa))

    if(plot) {
        gg <- ggplot(data=d_ipa,aes(x="",y=ipa)) + geom_violin(draw_quantiles=c(.25,.5,.75)) + geom_boxplot(notch=TRUE,width=.2, outlier.colour=NA)+ geom_jitter(width = 0.05,alpha=0.3)
        gg <- gg + labs(x="",y="IPA",title="Distribution du IPA sur les sites",subtitle="Les 3 quantiles 0.25, 0.5, 0.75")
        print(gg)
        ggsave("output/site_ipa.png",gg)
    }

    write.csv(d_ipa,"output/site_ipa.csv",row.names=FALSE)
    return(d_ipa)


}



get_lichen_indic <- function() {


    d <- prepa()

    d_q <- get_Q(d)
    d_ible <- get_IBLE(d)
    d_vdl <- get_VDL(d)
    d_ipa <- get_ipa(d,d_q)


    d_lichen <- inner_join(inner_join(d_ible,d_vdl),d_ipa)


    library(psych)

    file <- "output/indic_lichen_cor.png"

    png(file)

    pairs.panels(d_lichen[,-1],


                 method = "pearson", # correlation method
                 hist.col = "#00AFBB",
                 density = TRUE,  # show density plots
                 ellipses = TRUE # show correlation ellipses
                 )
    dev.off()

   write.csv(d_lichen,"output/indic_lichen.csv")

    return(d_lichen)


}










