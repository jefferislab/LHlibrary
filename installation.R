########################
# Package Installation #
########################

# From CRAN
library(devtools)
cran.packages = c("shiny","shinyBS", "shinyURL","shinythemes","slickR","plotly","colourpicker","formattable","packrat","data.table",
                  "ggplot2","DT","shinydashboard","gdtools","svglite")
for(cp in cran.packages){
  if (!require(cp,character.only = TRUE)){
    utils::install.packages(cp, repos='http://cran.us.r-project.org',lib = "/home/shiny/R/x86_64-pc-linux-gnu-library/3.4/")
  } 
}

# Not from CRAN
library(devtools)
github.packages = c("karthik/wesanderson","jefferislab/elmr","jefferislab/lhns","jefferislab/lhlite","alexanderbates/catnat","jefferis/flycircuit","jefferis/vfbr",'hadley/ggplot2')
for(gp in github.packages){
  if (!require(gp,character.only = TRUE)){
    devtools::install_github(gp, dependencies=TRUE, lib = "/home/shiny/R/x86_64-pc-linux-gnu-library/3.4/")
  } 
}
