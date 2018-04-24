### Some processing and data collection/generating we need to go for this Shiny app

### Not intended to be run on the server hosting the finished app

# Load the packages we need for this App
source("packages.R")
source("functions.R")


# Get the appropriate EM neurons


# All by all LH NBLAST
lh.nblast.allbyall = nblast_allbyall(all.neurons.dps, normalisation = "normalised", UseAlpha = TRUE)
saveRDS(lh.nblast.allbyall,"data/lhnblastallbyall.rds")

