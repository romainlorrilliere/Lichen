

## rmarkdown::render("exploration.Rmd",output_file="exploration")

vecPackage <- c("data.table","dplyr","ggplot2","kableExtra")

ip <- installed.packages()[,1]

for(p in vecPackage)
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "http://cran.univ-paris1.fr/",dependencies=TRUE)


my_kable_print <- function(d,caption="",bootstrap_options = "hover",position="center" , font_size=11,full_width = FALSE,fixed_thead = TRUE,scroll=TRUE,scroll_height = "300px", scroll_width = "100%") {

    k <- kable_styling(kable(d,caption = caption),bootstrap_options = ,bootstrap_options, full_width = full_width,fixed_thead = fixed_thead, position=position,font_size=font_size)
    if(scroll)
        k <- scroll_box(k,width = scroll_width, height = scroll_height)

    return(k)
}

