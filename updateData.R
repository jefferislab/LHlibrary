###############
# Update Data #
###############


# Not from CRAN
options(Ncpus = 4)
library(devtools)
Sys.setenv(RGL_USE_NULL=TRUE)
devtools::install_github(repo="jefferislab/lhlite", dependencies=TRUE, host = "https://api.github.com", lib = "/home/shiny/R/x86_64-pc-linux-gnu-library/3.5/", auth_token = "bb13ad495c8a15b2f77ebc7362e1fa7b742d749c")
devtools::install_github(repo="jefferislab/LHlibrary", dependencies=TRUE, host = "https://api.github.com", lib = "/home/shiny/R/x86_64-pc-linux-gnu-library/3.5/", auth_token = "bb13ad495c8a15b2f77ebc7362e1fa7b742d749c")
q()
