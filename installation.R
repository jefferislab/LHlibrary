########################
# Package Installation #
########################

# From CRAN
options(Ncpus = 4)
cran.packages = c("devtools","rgl","shinyWidgets","devtools","shiny","shinyBS", "shinyURL","shinythemes","slickR","plotly","colourpicker","heatmaply","data.table",
                  "ggplot2","DT","shinydashboard")
for(cp in cran.packages){
  if (!require(cp,character.only = TRUE)){
    utils::install.packages(cp, repos='http://cran.us.r-project.org', dependencies=TRUE, lib = "/home/shiny/R/x86_64-pc-linux-gnu-library/3.5/")
  } 
}

# Not from CRAN
library(devtools)
github.packages = c("jefferislab/nat.templatebrains","jefferislab/lhlite","karthik/wesanderson","jefferislab/elmr","jefferis/nat.utils","jefferislab/lhns","jefferislab/lhlite","alexanderbates/catnat","jefferis/flycircuit","jefferis/vfbr",'hadley/ggplot2',"aoles/shinyURL")
for(gp in github.packages){
    devtools::install_github(gp, dependencies=TRUE, lib = "/home/shiny/R/x86_64-pc-linux-gnu-library/3.5/")
}

q()
