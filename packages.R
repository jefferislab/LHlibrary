# Also set some options
options(rgl.useNULL=TRUE)
options(vfbr.server = 'http://www.virtualflybrain.org')

# R packages we need for this lateralhorn app
library(shiny)
library(rglwidget)
library(nat)
library(nat.nblast)
library(nat.flybrains)
library(flycircuit)
library(plotly)
library(downloader)
library(vfbr)
library(shinyURL)
library(shinythemes)
library(shinydashboard)
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

# Functions
label.help <- function(label,id){
  HTML(paste0(label,actionLink(id,label=NULL,icon=icon('question-circle'))))
}

get_neurons<-function(input, db){
  if(grepl("xample",input$Type)){
    cts = "pd2a1"
  }else{
    if(sum(grepl("all",input$CT))>0){
      if(sum(grepl("all",input$AG))>0){
        AG = subset(db[,],pnt%in%input$PNT)[,"anatomy.group"]
      }else{
        AG = input$AG
      }
      CT = subset(db[,],anatomy.group%in%AG)[,"cell.type"]
    }else{
      CT = input$CT
    }
    if(sum(grepl("all",input$lhns))>0){
      lhn.cts = sort(unique(db[,"cell.type"]))
    }else{
      lhn.cts = input$lhns
    }
    cts = unique(c(lhn.cts,CT))
    cts = cts[cts%in%db[,"cell.type"]]
  }
  neurons = subset(db,skeleton.type%in%input$SkeletonType&cell.type%in%cts)
  neurons[,"colour"] = darjeeling(length(neurons))
  neurons
}

update_neurons <- function(input,db){
  if(!is.null(input$SelectionTable_rows_selected)){ # Don't show neurons highlighted in selection table
    db = db[-input$SelectionTable_rows_selected]
  }
  db
}

# Define a function for a frontal view of the brain
frontalView<-function(zoom=0.6){
  um=structure(c(1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1), .Dim = c(4L, 4L))
  rgl.viewpoint(userMatrix=um,zoom=zoom)
}

# Plot a selection of pnts
plot_pnt <- function (pnts = "pd2") {
  plot.pnts = lhns::primary.neurite.tracts[pnts]
  rgl::plot3d(plot.pnts, soma = T, lwd = 5, col = "darkgrey",skipRedraw = TRUE)
  pxyz = t(sapply(plot.pnts, function(x) nat::xyzmatrix(x)[nat::rootpoints(x),]))
  rownames(pxyz) = gsub(pattern = "LH|lh", "", names(plot.pnts))
  shift <- matrix(c(-3, 3, 0), nrow(pxyz), 3, byrow = TRUE)
  rgl::text3d(pxyz + shift, texts = rownames(pxyz))
}

# Download skeletons and associated metadata
downloadskeletons <- function (nl, dir, format = NULL, subdir = NULL, INDICES = names(nl), 
          files = NULL, Force = FALSE, ...) 
{
  if (grepl("\\.zip", dir)) {
    zip_file = dir
    if (file.exists(zip_file)) {
      if (!Force) 
        stop("Zip file: ", zip_file, "already exists")
      unlink(zip_file)
    }
    zip_dir = tools::file_path_as_absolute(dirname(zip_file))
    zip_file = file.path(zip_dir, basename(zip_file))
    dir <- file.path(tempfile("user_neurons"))
  }
  else {
    zip_file = NULL
  }
  if (!file.exists(dir)) 
    dir.create(dir)
  df = attr(nl, "df")
  ee = substitute(subdir)
  subdirs = NULL
  if (!is.null(ee) && !is.character(ee)) {
    if (!is.null(df)) 
      df = df[INDICES, ]
    subdirs = file.path(dir, eval(ee, df, parent.frame()))
    names(subdirs) = INDICES
  }
  ff = substitute(files)
  if (!is.null(ff)) {
    if (!is.character(ff)) 
      files = eval(ff, df, parent.frame())
    if (is.null(names(files))) 
      names(files) = INDICES
  }
  written = structure(rep("", length(INDICES)+1), .Names = c(INDICES,"metadata"))
  for (nn in INDICES) {
    n = nl[[nn]]
    thisdir = dir
    if (is.null(subdirs)) {
      if (!is.null(subdir)) {
        propval = n[[subdir]]
        if (!is.null(propval)) 
          thisdir = file.path(dir, propval)
      }
    }
    else {
      thisdir = subdirs[nn]
    }
    if (!file.exists(thisdir)) 
      dir.create(thisdir, recursive = TRUE)
    written[nn] = write.neuron(n, dir = thisdir, file = files[nn], 
                               format = format, Force = Force)
  }
  # Save metadata
  write.csv(df,file = paste0(dir,"/neurons_metadata.csv"),row.names = FALSE)
  written["metadata"] = paste0(dir,"_metadata.csv")
  if (!is.null(zip_file)) {
    owd = setwd(dir)
    on.exit(setwd(owd))
    zip(zip_file, files = dir(dir, recursive = TRUE))
    unlink(dir, recursive = TRUE)
    written <- zip_file
  }
  invisible(written)
}


