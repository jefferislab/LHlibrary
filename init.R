# Also set some options
#rm(list = ls())
options(rgl.useNULL=TRUE)
#options(warn=-1) # Turns warnings off

# R packages we need for this lateralhorn app
library(shiny)
library(shinyBS)
library(shinyURL)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(slickR)
library(elmr)
library(plotly)
library(colourpicker)
library(lhlite)
library(heatmaply)
library(wesanderson)
library(data.table)
library(DT)

# Set colours
united.orange = "#E64A1E"
united.light.orange = "#FFD8B2"
darjeeling = colorRampPalette(wesanderson::wes_palette("Darjeeling1"))
zissou = colorRampPalette(rev(wesanderson::wes_palette("Zissou1")))

# Selected columns to display
selected_columns = c("id","pnt", "anatomy.group", "cell.type", "coreLH","type", "transmitter","skeleton.type","colour")

# Data management
pnt_all = sort(unique(all.lh.neurons[,"pnt"]))
pnt_all = pnt_all[pnt_all!=""&pnt_all!=" "]
pnt_choices = list(`Anterior dorsal`=pnt_all[grepl("^ad",pnt_all)],`Anterior ventral`=pnt_all[grepl("^av",pnt_all)],`Posterior dorsal`=pnt_all[grepl("^pd",pnt_all)],`Posterior dorsal`=pnt_all[grepl("^pv",pnt_all)]) 
pnt_choices = pnt_choices[pnt_choices!=""&pnt_choices!=" "]
pnt_lhns = c("AD1", "AD2", "AD3", "AD4", "AD5", "AV1", "AV2", "AV3", "AV4", 
             "AV5", "AV6", "AV7", "PD1", "PD2", "PD3", "PD4", "PD5", "PD6", 
             "PD7", "PV1", "PV2", "PV3", "PV4", "PV5", "PV6", "PV7", "PV8", "PV9", "PV10", "PV11", "PV12")
#sort(names(lhlite::primary.neurite.tracts))
ag_lhns = sort(unique(all.lh.neurons[,"anatomy.group"]))
mod_pns = c("Centrifugal", "Gustatory", "Mechanosensation", "Memory", "Neuromodulatory", 
            "Olfactory", "Olfactory+Gustatory", "Thermosensory", "Unknown")
mod_pns[mod_pns=="Olfactory+Gustatory"] = "Olfactory\\+Gustatory"
  
# PNT image paths
PNT_images = paste0("RGLsnapshots/PNT/",list.files("www/RGLsnapshots/PNT"))
AG_images =  paste0("images/RGLsnapshots/AG/",list.files("images/RGLsnapshots/AG"))
CT_images = paste0("RGLsnapshots/CT/",list.files("www/RGLsnapshots/CT"))
MOD_images = paste0("RGLsnapshots/PNmodality/",list.files("www/RGLsnapshots/PNmodality"))
TRACT_images = paste0("RGLsnapshots/PNtract/",list.files("www/RGLsnapshots/PNtract"))
PNAG_images = paste0("RGLsnapshots/PNAG/",list.files("www/RGLsnapshots/PNAG"))
PN_images = c(MOD_images,TRACT_images,PNAG_images)
PN_images = unlist(sapply(mod_pns,function(x) PN_images[grepl(paste0(x,"_"),PN_images)]))
split_brain_images = paste0("images/maxprojections/Brain/",list.files("images/maxprojections/Brain/"))
split_brain_images = split_brain_images[!grepl("ntitled",split_brain_images)]
split_vnc_images = paste0("images/maxprojections/VNC/",list.files("images/maxprojections/VNC/"))
split_vnc_images = split_vnc_images[!grepl("ntitled|creenshot",split_vnc_images)]

# Low res images
split_brain_images_low = paste0("maxprojections_lowres/Brain/",list.files("www/maxprojections_lowres/Brain/"))
split_brain_images_low = split_brain_images_low[!grepl("ntitled",split_brain_images_low)]
split_vnc_images_low = paste0("maxprojections_lowres/VNC/",list.files("www/maxprojections_lowres/VNC/"))
split_vnc_images_low = split_vnc_images_low[!grepl("ntitled|creenshot",split_vnc_images_low)]


# linecodes
lines = sapply(split_brain_images, function(x )gsub(".jpg","",tail(unlist(strsplit(x,'/')),n=1)))
lines = intersect(lhlite::lh_line_info$linecode,lines) # Some lines are lost...


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


