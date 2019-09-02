
require(RODBC)
require(gdata)
require(data.table)
require(reshape2)


library(readxl)

filename <- "data/IBLE_vallee_du_Rhone_1.xlsx"


onglets <- excel_sheets(path = filename)

dd <- NULL

for(o in  1:length(onglets)) {

    ## o <- 1
    site <- onglets[o]

    cat("\n",o," ",site,"::",sep="")
    d <- read.xlsx(filename, sheet = o,colNames =FALSE,skipEmptyRows=FALSE)
    veca <- grep("Arbre",d[,1])
    for(a in 1:length(veci)) {
        ## a <- 1
        cat(" -",a,":",sep="" )
        starta <- veca[a]
        if(a == length(veca)) enda <- veca[a] + 30  else enda <- veca[a+1]-1
        da <- d[starta:enda,]
        da <- da[!(is.na(da[,1])),]
        rownames(da) <- da[,1]
        da <- da[,-1]

        vecf <- which(!is.na(da[1,]))
        for(f in 1:4){
            ##  f <- 1
            face <- da[1,vecf[f]]
            cat(substr(face,1,2))
            daf <- da[,f:(f+4)]
            colnames(daf) <- daf[2,]
            daf <- daf[3:nrow(daf),]
            daf$sp <- row.names(daf)

            daf_l <- melt(daf,id.vars="sp")
            colnames(daf_l) <- c("sp","sample","ab")
            daf_l <- data.frame(site=site,arbre=a,face=face,daf_l)
            dd <- rbind(dd,daf_l)
        }

     }


}
cat("\n")
dd[is.na(dd)] <- 0
dd$ab <- as.numeric(dd$ab)

cat("  --> data/data_lyon.csv")
write.csv(dd,"data/data_lyon.csv",row.names=FALSE)
cat("   DONE !\n")




require(sf)
points_sf <- read_sf('data/Point_observation.shp')
points <- points_sf
st_geometry(points) <- NULL

colnames(points)[1] <- "site"
require(dplyr)
dd2 <- full_join(dd,points)

dd2 <- dd2[,c("site","arbre","face","sample","sp","ab","NAME","CODE_INSEE","Dept","X","Y")]

cat("  --> data/data_lyon_coord.csv")
write.csv(dd2,"data/data_lyon_coord.csv",row.names=FALSE)
cat("   DONE !\n")
