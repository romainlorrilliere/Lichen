
vecPackage <- c("data.table","dplyr","ggplot2","kableExtra","knitr","reshape2","sf","readxl","openxlsx")

ip <- installed.packages()[,1]

for(p in vecPackage){
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "http://cran.univ-paris1.fr/",dependencies=TRUE)
    library(p,character.only=TRUE)
}




main_preparation_data_lichen <- function(skip_until_rhone="sp_rhone",skip_until_grandest="",do.return=FALSE,lesColonnes =  c("date_str","annee_str","annee_min","annee_max","region","site_id","site_code","commune","insee","dept","lon_l93","lat_l93","arbre_id","arbre_num","arbre_nbtot","arbre_sp_fr","arbre_sp_sc","arbre_circ","face","sample","sp","ab")) {


    cat("\n-- RHONE --\n\n")
    if(skip_until_rhone == "all") {
        file_name <- "data/data_lyon_coord_sp_clean_eng.csv"
        cat(" <--",file_name,"\n")
        d.rhone <- fread(file_name)
    } else {

       d.rhone <- main_preparation_data_lichen_lyon(skip_until_rhone,do.return=TRUE)
    }

    cat("\n-- GRAND EST --\n\n")
    if(skip_until_grandest == "all") {
    file_name <- "data/data_lyon_coord_sp_clean_eng.csv"
        cat(" <--",file_name,"\n")
        d.ge <- fread(file_name)

    } else {

       d.ge <- main_preparation_data_lichen_grandest(skip_until_grandest,do.return=TRUE)
    }

    cat("\n-- ASSEMBLAGE --\n\n")
    d <- rbind(d.rhone,d.ge)


    file_name <- "data/data_eng_spclean_lichen_rhone_grandest.csv"
    cat(" ==>",file_name,"\n")
    write.csv(d,file_name,row.names=FALSE)


    if(do.return) return(d)
}




main_preparation_data_lichen_lyon <- function(skip_until="",do.return=FALSE,
                                              lesColonnes = c("date_str","annee_str","annee_min","annee_max","region","site_id","site_code","commune","insee","dept","lon_l93","lat_l93","arbre_id","arbre_num","arbre_nbtot","arbre_sp_fr","arbre_sp_sc","arbre_circ","face","sample","sp","ab")) {



    cat("Skip until:",skip_until,"\n\n")





    todo <- FALSE
    dd <- NULL

    if(skip_until  %in% c("xls_rhone","") | todo  | is.null(skip_until)) {
        cat(" - Importation XLS\n")
        dd <- import_xls_rhone()
        todo <- TRUE
    }
    if(skip_until == "loc_rhone" | todo ) {
        cat(" - Add Loc\n")
        if(is.null(dd)) {
            file_name <- "data/data_lyon.csv"
            cat(" <--",file_name,"\n")
            dd <- fread(file_name)
        }

        dd <- add_loc_rhone(dd)
        todo <- TRUE
    }

    if(skip_until == "traitement_rhone" | todo ) {
        cat(" - Traitement\n")
        if(is.null(dd)){
            file_name <- "data/data_lyon_coord.csv"
            cat(" <--",file_name,"\n")
            dd <- fread(file_name)
        }
        dd <- traitement_rhone(dd,lesColonnes)


        file_name <- "data/data_lyon_coord_clean.csv"
        cat(" -->",file_name,"\n")
        write.csv(dd,file_name,row.names=FALSE)


        todo <- TRUE
    }

    if(skip_until == "sp_rhone" | todo ) {
        cat(" - Correction sp\n")
        if(is.null(dd)) {
            file_name <- "data/data_lyon_coord_clean.csv"
            cat(" <--",file_name,"\n")
            dd <- fread(file_name)
        }
        dd <- correction_sp(dd)

        file_name <-"data/data_lyon_coord_sp_clean.csv"
        cat(" -->",file_name,"\n")
        write.csv(dd,file_name,row.names=FALSE)

        todo <- TRUE
    }

    if(skip_until == "eng_rhone" | todo ) {
        cat(" - eng\n")
        if(is.null(dd)) {
            file_name <- "data/data_lyon_coord_sp_clean.csv"
            cat(" <--",file_name,"\n")
            dd <- fread(file_name)
        }
        dd <- to_eng(dd)
        todo <- TRUE
    }



    file_name <- "data/data_lyon_coord_sp_clean_eng.csv"
    cat(" -->",file_name,"\n")
    write.csv(dd,file_name,row.names=FALSE)




    if(do.return) return(dd)

}






main_preparation_data_lichen_grandest <- function(skip_until="",do.return=FALSE,
                                              lesColonnes = c("date_str","annee_str","annee_min","annee_max","region","site_id","site_code","commune","insee","dept","lon_l93","lat_l93","arbre_id","arbre_num","arbre_nbtot","arbre_sp_fr","arbre_sp_sc","arbre_circ","face","sample","sp","ab")) {



    cat("Skip until:",skip_until,"\n\n")


    todo <- FALSE
    dd <- NULL

    if(skip_until %in% c("","traitement_grandest") | todo | is.null(skip_until) ) {
        cat(" - Traitement\n")
        dd <- traitement_grandest(lesColonnes)
        file_name <- "data/data_grandest_coord_clean.csv"
        cat(" -->",file_name,"\n")
        write.csv(dd,file_name,row.names=FALSE)
        todo <- TRUE
    }

    if(skip_until == "sp_grandest" | todo ) {
        cat(" - Correction sp\n")
        if(is.null(dd)) {
            file_name <- "data/data_grandest_coord_clean.csv"
            cat(" <--",file_name,"\n")
            dd <- fread(file_name)
        }
        dd <- correction_sp(dd)

        file_name <- "data/data_grandest_coord_sp_clean.csv"
        cat(" -->",file_name,"\n")
        write.csv(dd,file_name,row.names=FALSE)

        todo <- TRUE
    }

    if(skip_until == "eng_grandest" | todo ) {
        cat(" - eng\n")
        if(is.null(dd)) {
            file_name <- "data/data_grandest_coord_sp_clean.csv"
            cat(" <--",file_name,"\n")
            dd <- fread(file_name)
        }
        dd <- to_eng(dd)
        todo <- TRUE
    }

    file_name <- "data/data_grandest_coord_sp_clean_eng.csv"
    cat(" -->",file_name,"\n")
    write.csv(dd,file_name,row.names=FALSE)



    if(do.return) return(dd)
}






import_xls_rhone <- function() {


    filename <- "data/IBLE_vallee_du_Rhone_1.xlsx"


    onglets <- excel_sheets(path = filename,row.names=FALSE)

    dd <- NULL

    for(o in  1:length(onglets)) {

        site <- onglets[o]

        cat("\n",o," ",site,"::",sep="")
        dxlsx <- read.xlsx(filename, sheet = o,colNames =FALSE,skipEmptyRows=FALSE)

        veca <- grep("Arbre",dxlsx[,1])
        for(a in 1:length(veca)) {
            ## a <- 1
            cat(" -",a,":",sep="" )
            starta <- veca[a]
            if(a == length(veca)) enda <- veca[a] + 30  else enda <- veca[a+1]-1
            da <- dxlsx[starta:enda,]
            da <- da[!(is.na(da[,1])),]
            rownames(da) <- da[,1]
            da <- da[,-1]

            vecf <- which(!is.na(da[1,]))
            for(f in 1:4){
                cat("",f,"")
                face <- da[1,vecf[f]]
                ##   cat(substr(face,1,2))
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

   file_name <- "data/data_lyon.csv"
    cat(" -->",file_name,"\n")
    write.csv(dd,file_name,row.names=FALSE)


    return(dd)

}







add_loc_rhone <- function(dd) {

    points_sf <- read_sf('data/Point_observation.shp')
    points <- points_sf
    st_geometry(points) <- NULL

    colnames(points)[1] <- "site"

    dd2 <- full_join(dd,points)

    dd2 <- dd2[,c("site","arbre","face","sample","sp","ab","NAME","CODE_INSEE","Dept","X","Y")]

    dd2$face <- gsub("_","",dd2$face)

  file_name <- "data/data_lyon_coord.csv"
    cat(" -->",file_name,"\n")
    write.csv(dd,file_name,row.names=FALSE)



    return(dd)

}

traitement_rhone <- function(d,
                             lesColonnes = c("date_str","annee_str","annee_min","annee_max","region","site_id","site_code","commune","insee","dept","lon_l93","lat_l93","arbre_id","arbre_num","arbre_nbtot","arbre_sp_fr","arbre_sp_sc","arbre_circ","face","sample","sp","ab")) {
    d.rhone <- fread("data/data_lyon_coord.csv")

    d.rhone$date_str <- "avril 2009 - avril 2012"
    d.rhone$annee_str <- "2009-2012"
    d.rhone$annee_min <- "2009"
    d.rhone$annee_max <- "2012"
    d.rhone$arbre_sp_fr <- NA
    d.rhone$arbre_sp_sc <- NA
    d.rhone$arbre_circ <- NA
    d.rhone$region <- "Rhone"
    colnames(d.rhone)[colnames(d.rhone)=="NAME"] <- "commune"
    colnames(d.rhone)[colnames(d.rhone)=="site"] <- "site_code"
    d.rhone$site_id <- paste("R",d.rhone$site_code,sep="_")
    colnames(d.rhone)[colnames(d.rhone)=="CODE_INSEE"] <- "insee"
    colnames(d.rhone)[colnames(d.rhone)=="Dept"] <- "dept"
    colnames(d.rhone)[colnames(d.rhone)=="X"] <- "lon_l93"
    colnames(d.rhone)[colnames(d.rhone)=="Y"] <- "lat_l93"
    colnames(d.rhone)[colnames(d.rhone)=="arbre"] <- "arbre_num"
    d.rhone$arbre_id <- paste(d.rhone$site_id,d.rhone$arbre_num,sep="_")
    d.rhone$sample <- as.numeric(gsub("R","",d.rhone$sample))

d.max <- aggregate(arbre_num ~ site_id,d.rhone,max)
colnames(d.max)[2] <- "arbre_nbtot"

    d.rhone <- data.table(inner_join(d.rhone,d.max))
    d.rhone <- d.rhone[,lesColonnes,with=FALSE]
    d.max <- aggregate(arbre_num ~ site_id,d.rhone,max)
    colnames(d.max)[2] <- "arbre_nbtot"

    return(d.rhone)

}





traitement_grandest <- function(lesColonnes = c("date_str","annee_str","annee_min","annee_max","region","site_id","site_code","commune","insee","dept","lon_l93","lat_l93","arbre_id","arbre_num","arbre_nbtot","arbre_sp_fr","arbre_sp_sc","arbre_circ","face","sample","sp","ab")) {

    d.ge <- fread("data/data_grandest.csv")

 premiere_col <- c("site_code","lon_l93","lat_l93","arbre_sp_fr","arbre_sp_sc","arbre_circ","sp")
colnames(d.ge)[1:length(premiere_col)] <- premiere_col

d.ge$region <- "Grand-Est"
    d.ge$site_id <- paste("GE",d.ge$site_code,sep="_")

nbrow.tot <- nrow(d.ge)
i_keeped <-which(!is.na(d.ge$arbre_circ)& d.ge$arbre_sp_sc != "" &!is.na(d.ge$arbre_sp_sc))
d.ge<- d.ge[i_keeped]
    nbrow.new <- nrow(d.ge)

d.ge$arbre <- paste0(d.ge$arbre_sp_sc,d.ge$arbre_circ)

d.unique <- data.table(unique(d.ge[,c("site_id","arbre")]))
d.unique <- d.unique[,arbre_num := 1:.N,by=site_id]

d.max <- aggregate(arbre_num ~ site_id, d.unique,max)
colnames(d.max)[2] <- "arbre_nbtot"

d.unique <- inner_join(d.unique,d.max)
    d.ge <- inner_join(d.ge,d.unique)



premiere_col <- c(premiere_col,"arbre","arbre_num","arbre_nbtot","region","site_id")

d.ge <- data.table(melt(d.ge, id.vars = premiere_col))

d.ge$face <- substr(d.ge$variable,1,1)
vecFace <- c("N"="Nord","E"="Est","S"="Sud","O"="Ouest")

d.ge$face<- vecFace[d.ge$face]
d.ge$sample <- as.numeric(substr(d.ge$variable,2,nchar(as.character(d.ge$variable))))
    colnames(d.ge)[colnames(d.ge)=="value"] <- "ab"

d.ge$date_str <- "dans les annees 2000s"
d.ge$annee_str <- "2000~2009"
d.ge$annee_min <- "2000"
d.ge$annee_max <- "2009"
d.ge$commune <- NA
d.ge$insee <- NA
d.ge$dept <- NA
##head(d.ge)

d.ge$arbre_id <- paste(d.ge$site_id,d.ge$arbre_num,sep="_")




return(d.ge)












}




correction_sp <- function(d) {
    dsp <- data.table(table(d$sp))
    colnames(dsp) <- c("sp_brut","nb_rep")

    dsp$sp <- trimws(dsp$sp, which = c("both"))
    dsp$sp <- gsub("[sS][pP]\\.","sp",dsp$sp,perl=TRUE)
    dsp$sp <- gsub(" ?\\([A-Za-z]+.?\\)[A-Za-z0-9 \\.]*","",dsp$sp,perl=TRUE)


    dsp.agg <- aggregate(sp_brut ~ sp , dsp, paste,collapse= "' | '")
    dsp.agg.nb <- aggregate(nb_rep ~ sp , dsp, sum)

    dsp.agg <- inner_join(dsp.agg,dsp.agg.nb)
    dsp.agg <- dsp.agg[,c("sp","nb_rep","sp_brut")]

    dsp.agg$sp_brut <- paste0("'",dsp.agg$sp_brut,"'")
    dsp.agg$sp <- paste0("'",dsp.agg$sp,"'")

    dsp.agg <- data.table(dsp.agg[order(dsp.agg$sp),])
    nbsp <- nrow(dsp.agg)

    colnames(d)[colnames(d)=="sp"] <- "sp_brut"

    dsp <- dsp[,c("sp_brut","sp")]
    d <- data.table(inner_join(d,dsp))

    lesColonnes <- c("date_str","annee_str","annee_min","annee_max","region","site_id","site_code","commune","insee","dept","lon_l93","lat_l93","arbre_id","arbre_num","arbre_nbtot","arbre_sp_fr","arbre_sp_sc","arbre_circ","face","sample","sp_brut","sp","ab")

    d <- d[,lesColonnes,with=FALSE]


    d_cor_sp <- fread("library/correction_nom_sp.csv")
    colnames(d_cor_sp)[colnames(d_cor_sp)=="sp_brut"] <- "sp"
    d <- data.table(left_join(d,d_cor_sp))

    d$sp <- ifelse(is.na(d$sp_clean),d$sp,d$sp_clean)



    dsp2 <- data.table(table(d$sp))
    colnames(dsp2) <- c("sp","nb_rep")

    dsp.agg <- aggregate(sp_brut ~ sp , unique(d[,c("sp","sp_brut")]), paste,collapse= "' | '")

    dsp.agg <- inner_join(dsp2,dsp.agg)
    dsp.agg <- dsp.agg[,c("sp","nb_rep","sp_brut")]

    dsp.agg$sp_brut <- paste0("'",dsp.agg$sp_brut,"'")
    dsp.agg$sp <- paste0("'",dsp.agg$sp,"'")

    dsp.agg <- data.table(dsp.agg[order(dsp.agg$nb_rep,decreasing=TRUE),])
    nbsp <- nrow(dsp.agg)

    print(dsp.agg)

    return(d)

}



to_eng <- function(d,
                   lesColonnes = c("date_str","annee_str","annee_min","annee_max","region","site_id","site_code","commune","insee","dept","lon_l93","lat_l93","arbre_id","arbre_num","arbre_nbtot","arbre_sp_fr","arbre_sp_sc","arbre_circ","face","sample","sp_brut","sp","ab"),
                   lesColonnes_eng = c("date_str","year_str","year_min","year_max","region","site_id","site_code","city","insee","dept","lon_l93","lat_l93","tree_id","tree_num","tree_nbtot","tree_sp_fr","tree_sp_sc","tree_circ","face","sample","sp_brut","sp","coverage")) {

    d <- d[,lesColonnes,with=FALSE]
    d <- setnames(d, lesColonnes,lesColonnes_eng)

    return(d)
}
