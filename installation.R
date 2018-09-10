########################
# Package Installation #
########################

# From CRAN
library(devtools)
cran.packages = c("shinyWidgets","devtools","shiny","shinyBS", "shinyURL","shinythemes","slickR","plotly","colourpicker","heatmaply","data.table",
                  "ggplot2","DT","shinydashboard")
for(cp in cran.packages){
  if (!require(cp,character.only = TRUE)){
    utils::install.packages(cp, repos='http://cran.us.r-project.org', dependencies=TRUE)
  } 
}

# Not from CRAN
library(devtools)
github.packages = c("karthik/wesanderson","jefferislab/elmr","jefferislab/lhns","jefferislab/lhlite","alexanderbates/catnat","jefferis/flycircuit","jefferis/vfbr",'hadley/ggplot2')
for(gp in github.packages){
    devtools::install_github(gp, dependencies=TRUE)
}

q()
