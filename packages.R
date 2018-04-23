# Also set some options
options(rgl.useNULL=TRUE)
options(vfbr.server = 'http://www.virtualflybrain.org')

# R packages we need for this lateralhorn app
library(shiny)
library(rglwidget)
library(nat)
library(nat.nblast)
library(nat.flybrains)
library(catnat)
library(elmr)
library(flycircuit)
library(plotly)
library(downloader)
library(vfbr)
library(shinyURL)
library(shinythemes)
library(shinyBS)
library(colourpicker)
library(formattable)
library(packrat)
library(lhns)
library(wesanderson)
library(data.table)

# Load some functions we might need
source("helper.R")

# Set colours
united.orange = "#E64A1E"
darjeeling = colorRampPalette(wes_palette("Darjeeling1"))
newFCWBNP.surf = nat.flybrains::FCWBNP.surf
newFCWBNP.surf$RegionColourList[grep("LH_R",FCWBNP.surf$RegionList)] = united.orange

# Set up an all.neurons object that will contain all the neurons destined for plotting
most.lhns.clean = subset(lhns::most.lhns,good.trace==TRUE) # Remove neurons that look like mistakes
lh.splits.clean = lhns::lh.splits
lh.splits.clean[,"skeleton.type"] = "ConfocalStack"
all.neurons = c(most.lhns.clean,lh.splits.clean)
df = all.neurons[,]
df = df[,c("id","pnt", "anatomy.group", "cell.type", "coreLH","type", "neurotransmitter","skeleton.type")]
colnames(df) = c("id","pnt", "anatomy.group", "cell.type", "coreLH","type", "transmitter","skeleton.type")
df[is.na(df)] = " "
attr(all.neurons,"df") = df
all.neurons[,"colour"] = sample(darjeeling(length(all.neurons)))

# get dye filled skeletons that will have E-Phys data
dye.fills = all.neurons[grepl("^1",names(most.lhns))]
cells = paste0("nm20",names(dye.fills))
cells = cells[cells%in%names(physplitdata::smSpikes)]
dye.fills = dye.fills[cells%in%names(physplitdata::smSpikes)] # Final list of neurons for which there is e-phy data

# Get the main dotprops object
most.lhns.dps.clean = subset(lhns::most.lhns.dps,good.trace==TRUE) # Remove neurons that look like mistakes
all.neurons.dps = c(most.lhns.dps.clean, lh.splits.clean)
df = all.neurons.dps[,]
df = df[,c("id","pnt", "anatomy.group", "cell.type", "coreLH","type", "neurotransmitter","skeleton.type")]
colnames(df) = c("id","pnt", "anatomy.group", "cell.type", "coreLH","type", "transmitter","skeleton.type")
df[is.na(df)] = " "
attr(all.neurons.dps,"df") = df
all.neurons.dps[,"colour"] = sample(darjeeling(length(all.neurons.dps)))

# Get the E-Phys data
ephys.lhns = physplit.analysis::create_raw_summary_array(x = physplitdata::smSpikes, cells = cells)
odours = colnames(ephys.lhns)
ephys.lhns.m = physplit.analysis::createSummarySpikesMat(ephys.lhns,NALimit=3,numSamplePoints=7)
choose.just.max1.cols = as.logical(rep(c(0,0,1,0,0,0,0),ncol(ephys.lhns.m)/7))
ephys.lhns.m = ephys.lhns.m[,choose.just.max1.cols]
colnames(ephys.lhns.m) = odours[1:ncol(ephys.lhns.m)]
row.cells = dye.fills[gsub("nm20","",rownames(ephys.lhns.m))]
rownames(ephys.lhns.m) = row.cells[,"cell.type"]
ephys.lhns.m = ephys.lhns.m[order(rownames(ephys.lhns.m)),order(colnames(ephys.lhns.m))]

# Select certain odours to show
ephys.lhns.m = ephys.lhns.m[,!colnames(ephys.lhns.m)%in%c("ClrBL","ClrB2","FlyFM")]
colnames(ephys.lhns.m) = c( "Mineral Oil", "trans-2-Hexenal", "Geranyl acetate", "Propyl acetate", "Isoamyl acetate", "Ethyl 3-hydroxybutyrate", "Nonanal", 
   "11-cis vaccenyl acetate- CVA", "Methyl salicylate", "Hexyl acetate", "Phenethyl alcohol", "Acetoin acetate", "Ethyl hexanoate", "2-Phenethyl acetate", "5 odour mix", "Benzaldehyde", "b-citronellol", "1-Hexanol", "Farnesol", "Water blank", "Cadaverine", 
   "Spermine", "Acetoin", "Methyl acetate", "Acetic acid", "Propionic Acid", "Butyric acid", "Ammonia", 
   "Pyridine", "Phenylacetaldehyde", "Hydrochloric acid", "Phenylacetic acid", "Vinegar mimic", "Geosmin",  "Vineger and Geosmin mix",
   "Phenylethylamine")

# Selected columns to display
selected_columns = c("id","pnt", "anatomy.group", "cell.type", "coreLH","type", "transmitter","skeleton.type","colour")
all.lh.pnts = c("ad1", "ad2", "ad3", "ad4", "ad5", "av1", "av2", "av3", 
"av4", "av5", "av6", "av7", "pd1", "pd2", "pd3", 
"pd4", "pd5", "pd6", "pd7", "pv1", "pv10", "pv11", "pv2", "pv3", 
"pv4", "pv5", "pv6", "pv7", "pv8", "pv9")

# Data management
pnt_all = sort(unique(all.neurons[,"pnt"]))
pnt_choices = list(`Anterior dorsal`=pnt_all[grepl("^ad",pnt_all)],`Anterior ventral`=pnt_all[grepl("^av",pnt_all)],`Posterior dorsal`=pnt_all[grepl("^pd",pnt_all)],`Posterior dorsal`=pnt_all[grepl("^pv",pnt_all)]) 

