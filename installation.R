########################
# Package Installation #
########################

# From CRAN
cran.packages = c("shiny","shinyBS", "shinyURL","shinythemes","slickR","plotly","colourpicker","formattable","packrat","data.table",
                  "ggplot2","DT","shinydashboard")
for(cp in cran.packages){
  if (!require(cp,character.only = TRUE)){
    install.packages(cp)
  } 
}

# Not from CRAN
library(devtools)
github.packages = c("karthik/wesanderson","jefferislab/elmr","jefferislab/lhns","alexanderbates/catnat","jefferis/flycircuit","jefferis/vfbr")
for(gp in github.packages){
  if (!require(gp,character.only = TRUE)){
    devtools::install_github(gp, dependencies=TRUE)
  } 
}
