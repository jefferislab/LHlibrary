###############
# Update Data #
###############


# Not from CRAN
library(devtools)
github.packages = c("jefferislab/lhlite")
for(gp in github.packages){
  if (!require(gp,character.only = TRUE)){
    devtools::install_github(gp, dependencies=TRUE, lib = "/home/shiny/R/x86_64-pc-linux-gnu-library/3.4/")
  } 
}
