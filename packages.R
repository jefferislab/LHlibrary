# Also set some options
options(rgl.useNULL=TRUE)

# R packages we need for this lateralhorn app
library(shiny)
library(shinyBS)
library(shinyURL)
library(shinythemes)
library(shinydashboard)
library(slickR)
library(elmr)
library(catnat)
library(flycircuit)
library(plotly)
library(colourpicker)
library(formattable)
library(packrat)
library(lhns)
library(wesanderson)
library(data.table)
library(ggplot2)
library(DT)

# Load some functions we might need
source("helper.R")

# Set colours
united.orange = "#E64A1E"
united.light.orange = "#FFD8B2"
darjeeling = colorRampPalette(wesanderson::wes_palette("Darjeeling1"))
zissou = colorRampPalette(rev(wesanderson::wes_palette("Zissou1")))
newFCWBNP.surf = nat.flybrains::FCWBNP.surf
newFCWBNP.surf$RegionColourList[grep("LH_R",FCWBNP.surf$RegionList)] = united.orange

# Set up an all.neurons object that will contain all the neurons destined for plotting
most.lhns = subset(lhns::most.lhns,cell.type!="notLHproper"&good.trace==TRUE)
most.lhins = subset(lhns::most.lhins,cell.type!="notLHproper"&good.trace==TRUE)
most.lhns.dps = subset(lhns::most.lhns.dps,cell.type!="notLHproper"&good.trace==TRUE)
most.lhins.dps = subset(lhns::most.lhins.dps,cell.type!="notLHproper"&good.trace==TRUE)
most.lhins[,"id"] = most.lhins.dps[,"id"] = names(most.lhins)
most.lhins[,"cell.type"] = most.lhins.dps[,"cell.type"] = paste0(most.lhins[,"anatomy.group"],ifelse(is.na(most.lhins[,"glomerulus"]),"",paste0("_",most.lhins[,"glomerulus"])))
mbons.light.dps[,"cell.type"] = mbons.light.dps[,"fname"] 
mbons.light.dps[,"anatomy.group"] = mbons.light.dps[,"cluster"] 
mbons.light.dps[,"neurotransmitter"] = mbons.light.dps[,"transmitter"] 
pn.axons.light[,"cell.type"] = pn.axons.light[,"GlomType"]
pn.axons.light.dps[,"cell.type"] = pn.axons.light.dps[,"GlomType"]
pn.axons.light[,"anatomy.group"] = paste0("AL_",pn.axons.light[,"tract"])
pn.axons.light.dps[,"anatomy.group"] = paste0("AL_",pn.axons.light.dps[,"tract"])
jfw.lhns = subset(lhns::jfw.lhns,!is.na(cell.type)&cell.type!="notLHproper")
jfw.lhns.dps = subset(lhns::jfw.lhns.dps,!is.na(cell.type)&cell.type!="notLHproper")

# Construct all neurons object
all.neurons = c(most.lhns,most.lhins,lh.splits.dps,mbons.light.dps,emlhns,pn.axons.light,jfw.lhns)
all.neurons[,"id"] = names(all.neurons)
all.neurons.dps = c(most.lhns.dps,most.lhins.dps,lh.splits.dps,mbons.light.dps,emlhns.dps,pn.axons.light.dps,jfw.lhns.dps)
all.neurons.dps[,"id"] = names(all.neurons.dps)

# Sort out meta data and remove NAs to be blanks
df = all.neurons[,]
df = df[,c("id","pnt", "anatomy.group", "cell.type", "coreLH","type", "neurotransmitter","skeleton.type")]
colnames(df) = c("id","pnt", "anatomy.group", "cell.type", "coreLH","type", "transmitter","skeleton.type")
df[is.na(df)] = " "
attr(all.neurons,"df") = df

# Set starting colours
all.neurons[,"colour"] = sample(darjeeling(length(all.neurons)))
all.neurons.dps[,"colour"] = sample(darjeeling(length(all.neurons.dps)))

# Selected columns to display
selected_columns = c("id","pnt", "anatomy.group", "cell.type", "coreLH","type", "transmitter","skeleton.type","colour")

# Data management
pnt_all = sort(unique(all.neurons[,"pnt"]))
pnt_all = pnt_all[pnt_all!=""&pnt_all!=" "]
pnt_choices = list(`Anterior dorsal`=pnt_all[grepl("^ad",pnt_all)],`Anterior ventral`=pnt_all[grepl("^av",pnt_all)],`Posterior dorsal`=pnt_all[grepl("^pd",pnt_all)],`Posterior dorsal`=pnt_all[grepl("^pv",pnt_all)]) 
pnt_choices = pnt_choices[pnt_choices!=""&pnt_choices!=" "]
pnt_lhns = sort(names(lhns::primary.neurite.tracts))
ag_lhns = sort(unique(all.neurons[,"anatomy.group"]))
mod_pns = sort(unique(most.lhins[,"modality"]))
mod_pns[mod_pns=="Olfactory+Gustatory"] = "Olfactory\\+Gustatory"
  
# PNT image paths
PNT_images = paste0("RGLsnapshots/PNT/",list.files("www/RGLsnapshots/PNT"))
AG_images =  paste0("www/RGLsnapshots/AG/",list.files("www/RGLsnapshots/AG"))
CT_images = paste0("RGLsnapshots/CT/",list.files("www/RGLsnapshots/CT"))
MOD_images = paste0("RGLsnapshots/PNmodality/",list.files("www/RGLsnapshots/PNmodality"))
TRACT_images = paste0("RGLsnapshots/PNtract/",list.files("www/RGLsnapshots/PNtract"))
PNAG_images = paste0("RGLsnapshots/PNAG/",list.files("www/RGLsnapshots/PNAG"))
PN_images = c(MOD_images,TRACT_images,PNAG_images)
PN_images = unlist(sapply(mod_pns,function(x) PN_images[grepl(paste0(x,"_"),PN_images)]))
split_brain_images = paste0("maxprojections/Brain/",list.files("www/maxprojections/Brain/"))
split_brain_images = split_brain_images[!grepl("ntitled",split_brain_images)]
split_vnc_images = paste0("maxprojections/VNC/",list.files("www/maxprojections/VNC/"))
split_vnc_images = split_vnc_images[!grepl("ntitled|creenshot",split_vnc_images)]
lines = sapply(split_brain_images, function(x )gsub(".jpg","",tail(unlist(strsplit(x,'/')),n=1)))
lines = intersect(lh_line_info$linecode,lines) # Some lines are lost...


############
# Old Code #
############

#
# options(vfbr.server = 'http://www.virtualflybrain.org')
#
# # Load VFB ID lookup table
# vfb_ids=readRDS('data/vfb_ids.rds')
# 
# # Load VFB annotation ID lookup table
# vfb_annotations <- read.table("data/annotation_map.tsv", header=TRUE, sep="\t", quote = "")
# 
# # Get the GMR line URLs
# gmr_stack_urls=readRDS('data/gmr_stack_urls.rds')


