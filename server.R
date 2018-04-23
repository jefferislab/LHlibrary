# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

# Load the packages we need for this App
source("packages.R")
source("functions.R")


shinyServer(function(input, output, session) {
  
  ###################
  # Reactive Objects #
  ###################
  
  # Get neurons for plotting and selection table
  vals <- reactiveValues()
  vals$zoom = 0.4 # Zoom onto brain
  vals$um = structure(c(1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1), .Dim = c(4L, 4L)) # Frame of view
  vals$neurons <- subset(all.neurons,cell.type=="pd2a1")
  vals$neuronsDF <- data.table::data.table(subset(all.neurons,cell.type=="pd2a1")[,selected_columns])
  vals$CATMAID = list(CATMAID_server = "https://neuropil.janelia.org/tracing/fafb/v14/", CATMAID_authname= NULL,CATMAID_authpassword = NULL, CATMAID_token = NULL, CATMAID_warning = FALSE)

  # Dynamically create neurons and neuronsDF object
  observeEvent(input$plotneurons, {
    original = vals$neurons
    # Add neurons
    selected = get_neurons(input=input,db=all.neurons)
    if("neuronlist"%in%class(selected)&length(selected)>0){ # Prevent neurons of the same name being added together
      new = selected[!names(selected)%in%names(original)] 
    }
    # Neurons are deleted from the selection via an observe app below
    vals$neurons <- c(original,new)
    vals$neuronsDF <- data.table::data.table(vals$neurons[,])
  })
  
  #####################
  # Choose Library LHNs #
  #####################
  
  # Dynamically update LHN selection, depending on which data type is picked 
  output$LHNselection <- renderUI({
    if(input$Type%in%c("ON","LN","LHN")){
      LHN_choices =  sort(unique(subset(all.neurons[,],skeleton.type%in%input$SkeletonType)[,"cell.type"]))
      selectInput("lhns", label = paste0("Cell types in dataset (",length(LHN_choices),") :"), choices = LHN_choices,selected = "pd2a1", multiple=TRUE, selectize=TRUE)
    }
  })
  
  # Dynamically update PNT selection, depending on which data type is picked 
  output$PNTselection <- renderUI({
    if(input$Type%in%c("ON","LN","LHN")){
      PNT_all =  sort(unique(subset(all.neurons[,],skeleton.type%in%input$SkeletonType)[,"pnt"]))
      PNT_choices = list(`Anterior dorsal`=PNT_all[grepl("^ad",PNT_all)],`Anterior ventral`=PNT_all[grepl("^av",PNT_all)],`Posterior dorsal`=PNT_all[grepl("^pd",PNT_all)],`Posterior dorsal`=PNT_all[grepl("^pv",PNT_all)])
      PNT_choices = c(PNT_choices, list(`Other`= c(PNT_all[!PNT_all%in%unlist(PNT_choices)])))
      selectInput("PNT", label = paste0("Primary neurite tracts in dataset (",length(PNT_choices),") :"), choices = PNT_choices,selected = NULL, multiple=TRUE, selectize=TRUE)
    }
  })
  
  # Dynamically update AG selection, depending on which PNTs are picked 
  output$AGselection <- renderUI({
    if(input$Type%in%c("ON","LN","LHN")){
      AG_choices =  sort(unique(subset(all.neurons[,],pnt%in%input$PNT&skeleton.type%in%input$SkeletonType)[,"anatomy.group"]))
      selectInput("AG", label = paste0("Anatomy groups (",length(AG_choices),") :"), choices = c("all in selected primary neurite tracts",AG_choices),selected = "all in selected primary neurite tracts", multiple=TRUE, selectize=TRUE)
    }
  })
  
  # Dynamically update CT selection, depending on which PNTs and AGs are picked 
  output$CTselection <- renderUI({
    if(input$Type%in%c("ON","LN","LHN")){
      if("all in selected primary neurite tracts"%in%input$AG){
        CT_choices = sort(unique(subset(all.neurons[,],pnt%in%input$PNT&skeleton.type%in%input$SkeletonType)[,"cell.type"]))
      }else{
        CT_choices = sort(unique(subset(all.neurons[,],anatomy.group%in%input$AG&skeleton.type%in%input$SkeletonType)[,"cell.type"]))
      }
      selectInput("CT", label = paste0("Cell types (",length(CT_choices),") :"), choices = c("all in selected anatomy groups",CT_choices), selected = "all in selected anatomy groups", multiple=TRUE, selectize=TRUE)
    }
  })
  
  ############
  # 3D Viewer #
  ############
  
  # The 3D RGL window
  output$plot3D <- renderRglwidget({
    # Clear the space
    rgl::clear3d()
    rgl::open3d(mouseMode = c("trackball", "user", "zoom"), FOV = 0) # Hmm, how can we get this thing to pan?
    nat:::pan3d(2)
    # Plot brain
    if(input$BrainMesh){
      rgl::plot3d(FCWB, alpha = 0.1,skipRedraw = TRUE)
    }
    if(length(input$neuropils)>0){
      if("all neuropils"%in%input$neuropils) {
        rgl::plot3d(subset(newFCWBNP.surf, NULL), alpha = 0.2,skipRedraw = TRUE)
      }else{
        rgl::plot3d(subset(newFCWBNP.surf, input$neuropils), alpha = 0.2,skipRedraw = TRUE)
      }
    }
    # Generate and plot neuron selection
    neurons = vals$neurons
    if(length(neurons)>0){
      neurons = update_neurons(input=input,db=neurons)
      rgl::plot3d(neurons,soma=T,lwd=3, col = neurons[,"colour"],skipRedraw = TRUE, WithConnectors = TRUE)
    }
    # Plot PNTs if selected
    pnts_to_plot = c(input$PNT1,input$PNT2,input$PNT3,input$PNT4,input$PNT5,input$PNT6,input$PNT7,input$PNT8,input$PNT9,input$PNT10,
                     input$PNT11,input$PNT12,input$PNT13,input$PNT14,input$PNT15,input$PNT16,input$PNT17,input$PNT18,input$PNT19,input$PNT20,
                     input$PNT21,input$PNT22,input$PNT23,input$PNT24,input$PNT25,input$PNT26,input$PNT27,input$PNT28,input$PNT29,input$PNT30)
    if(sum(pnts_to_plot)>0){
      plot_pnt(pnts_to_plot) 
    }
    # Set 3D view
    rgl::rgl.viewpoint(userMatrix=vals$um,zoom=vals$zoom)
    #frontalView()
    rgl::rglwidget()
  })
  
  ########################
  # Neuron Selection Table #
  ########################
  
  # Show neuron selection table with some table-wide buttons above it
  output$MainTable<-renderUI({
    fluidPage(
        box(width=12,
            column(6,offset = 0,
                   HTML('<div class="btn-group" role="group" aria-label="Basic example">'),
                   actionButton(inputId = "Del_row_head",label = "delete selected"),
                   actionButton(inputId = "Col_row_head",label = "recolour selected"),
                   actionButton(inputId = "Compare_row_head",label = "compare selected"),
                   actionButton(inputId = "Download",label = "download data"),
                   HTML('</div>')
            ),
            column(width = 12, 
                   DT::dataTableOutput("SelectionTable") 
                   ),
              tags$script(HTML('$(document).on("click", "input", function () {
                               var checkboxes = document.getElementsByName("row_selected");
                               var checkboxesChecked = [];
                               for (var i=0; i<checkboxes.length; i++) {
                                    if (checkboxes[i].checked) {
                                        checkboxesChecked.push(checkboxes[i].value);
                                    }
                               } 
                             Shiny.onInputChange("checked_rows",checkboxesChecked);})'
              )),# This HTML code assigns input$checked_rows, so we know which rows in the table are checked
              tags$script("$(document).on('click', '#SelectionTable button', function () {
                    Shiny.onInputChange('lastClickId',this.id);
                    Shiny.onInputChange('lastClick', Math.random())});"
              ) # When a button in output$SelectionTable is clicked, input$lastClickId gets assigned the id of this button and input$lastClick gets assigned a random value
            # The last click is used to detect the click (for instance when a button is clicked twice, the id won’t change and hence cannot be observed)
        )
      )
  })
  
  # Render neuron selection table
  output$SelectionTable <- DT::renderDataTable({
    DT=vals$neuronsDF
    # If there's stuff in the table...
    if(length(DT)>0){
      # Add check boxes to the table
      DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(vals$neuronsDF),'"><br>')
      # Add two action buttons to the table
      DT[["Actions"]]<-paste0('
               <div class="btn-group" role="group" aria-label="Basic example">
               <button type="button" class="btn btn-secondary modify" id=modify_',1:nrow(vals$neuronsDF),'>recolour</button> 
               <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(vals$neuronsDF),'><i class="fa fa-trash"></i></button>
               </div>
               ')
      DT::datatable(DT,  escape=F, options = list(paging=FALSE),extensions = c('Responsive'), editable = FALSE, rownames= FALSE) %>%
        DT::formatStyle('cell.type', target = "row", backgroundColor = united.orange,color = 'black') %>%
        DT::formatStyle('colour', backgroundColor = DT::styleEqual(DT$colour,DT$colour))
      # Using escape = F means that using it, datatable will render the buttons according to their HTML codes instead of strings
    }else{
      NULL
    }
  })
  
  # Delete rows that have been checked
  observeEvent(input$Del_row_head,{
    row_to_del=as.numeric(gsub("Row","",input$checked_rows))
    vals$neuronsDF = vals$neuronsDF[-row_to_del] # Update dynamic object
    vals$neurons = vals$neurons[-row_to_del] # Update dynamic object
  })
  
  ###################################
  # Compare Selected Neurons in Table #
  ###################################
  
  # Select neurons from table to compare 
  observeEvent(input$Compare_row_head,{
    row_to_del=as.numeric(gsub("Row","",input$checked_rows)) # Here 'row_to_del' is actually the selected rows
    vals$Comparison = vals$neuronsDF[row_to_del] # Use this in compare_neurons_modal
    showModal(compare_neurons_modal)
  }
  )
  
  # Compare neurons selected in table
  compare_neurons_modal<-modalDialog(
    h3(strong("selected neurons"),align="center"),
    fluidPage(
      mainPanel(
      rgl::rglwidgetOutput("plot3D", width="1000px", height="1000px") # Doesn't work...
      )
    )
  )
  
  ######################
  # Modify Colours Table #
  ######################
  
  # Delete rows that have been checked
  observeEvent(input$Col_row_head,{
    showModal(modal_recolor_multiple)
  })
  
  # Observe input$lastClick and then act to delete or modify a row
  observeEvent(input$lastClick,
               {
                 if (input$lastClickId%like%"delete") # Was is the delete button?
                 {
                   row_to_del=as.numeric(gsub("delete_","",input$lastClickId))
                   vals$neuronsDF = vals$neuronsDF[-row_to_del] # Remove from data table
                   vals$neurons = vals$neurons[-row_to_del] # Remove from selected neurons
                 }
                 else if (input$lastClickId%like%"modify") # Or was it the modify button?
                 {
                   showModal(modal_modify)
                 }
               }
  )
  
  # Modify colour
  modal_modify<-modalDialog(
    fluidPage(
      h3(strong("select a colour"),align="center"),
      uiOutput("OldColour"),
      actionButton("ChangeColour","change")
    )
  )
  
  # Modify colours
  modal_recolor_multiple <-modalDialog(
    fluidPage(
      h3(strong("select a colour"),align="center"),
      uiOutput("RandomStartColour"),
      actionButton("ChangeColourMultiple","change")
    )
  )
  
  # Render old neuron colour and select new one in modal window
  output$OldColour <- renderUI({
    selected_row=as.numeric(gsub("modify_","",input$lastClickId))
    old_row=vals$neuronsDF[selected_row,"colour"][[1]]
    if(length(old_row)>1){old_row=united.orange}
    colourInput(inputId = "NewColour",label= NULL,value = old_row)
  })
  
  # Render old neuron colour and select new one in modal window
  output$RandomStartColour <- renderUI({
    colourInput(inputId = "NewColourMultiple",label= NULL,value = sample(darjeeling(100),1))
  })
  
  # Update colour on the data frame and in neurons
  observeEvent(input$ChangeColour,{
    selected_row = as.numeric(gsub("modify_","",input$lastClickId))
    vals$neurons[selected_row,"colour"] <- input$NewColour
    vals$neuronsDF[selected_row,"colour"] <- input$NewColour
  })
  
  # Update colour on the data frame and in neurons
  observeEvent(input$ChangeColourMultiple,{
    selected_row = as.numeric(gsub("Row","",input$checked_rows))
    vals$neurons[selected_row,"colour"] <- input$NewColourMultiple
    vals$neuronsDF[selected_row,"colour"] <- input$NewColourMultiple
  })
  
  #################
  # Download Data #
  #################
  
  # Download skeletons in selection table
  observeEvent(input$Download,{
    showModal(modal_download)
  })
  
  # Download data
  modal_download <-modalDialog(
    fluidPage(
      h3(strong("download data"),align="center"),
      selectInput(inputId='DownloadType', label=NULL, choices = c("neurons in table","e-phys data for relevant neurons in table"), selected = "neurons in table", multiple=FALSE, selectize=TRUE),
      downloadButton("downloadData", "download")
    )
  )
  
  # Download skeletons
  output$downloadData <- downloadHandler(
    filename = function() {
      if(input$DownloadType=="neurons in table"){
        "LH_library_sample.zip"
      }else{
        "LH_library_sample_ephys.csv"
      }
    },
    content = function(file) {
      if(input$DownloadType=="neurons in table"){
        downloadskeletons(vals$neurons,dir = file,subdir = pnt,format="swc",file = paste0(cell.type,"_",id),Force = TRUE)
      }else{
        cts = unique(vals$neurons[,"cell.type"][vals$neurons[,"cell.type"]%in%dye.fills])
        csv = ephys.lhns.m[rownames(ephys.lhns.m)%in%cts,]
        write.csv(csv,file = file, row.names = TRUE)
      }
    },
    contentType = "application/zip"
  )
  
  #####################
  # Plotly E-Phys Data #
  #####################
  
  # Choose cell types for E-Phys plot 
  output$ChooseCTs <- renderUI({
    ct_choices = sort(unique(dye.fills[,"cell.type"]))
    ct_choices = ct_choices[!ct_choices%in%c("notLHproper")] # Get rid of uncertain LHNs
    if(length(input$SelectionTable_rows_selected)>1){
      cts = sort(unique(vals$neurons[,"cell.type"][-input$SelectionTable_rows_selected]))
    }else{
      cts = sort(unique(vals$neurons[,"cell.type"]))
    }
    cts_in_data = cts[cts%in%ct_choices]
    selectInput("EphysSelection", label = paste0("Cell types with electrophysiological response data (",length(ct_choices),") :"), choices = ct_choices, selected = cts_in_data, multiple=TRUE, selectize=TRUE)
  })
  
  # Choose odours for which to plot cell type responses
  output$ChooseOdours <- renderUI({
    Odour_choices = colnames(ephys.lhns.m)
    selectInput("OdourSelection", label = paste0("choose odour (",length(Odour_choices),") :"), choices = Odour_choices, selected = "Vinegar mimic", multiple=TRUE, selectize=TRUE)
  })
  
  # A plotly graph for the E-Phys data as tuning curves
  output$Ephys <- plotly::renderPlotly({
    if(is.null(input$EphysSelection)){
      data = rep(0,ncol(ephys.lhns.m))
      names(data) = colnames(ephys.lhns.m)
    }else{
      data = ephys.lhns.m[rownames(ephys.lhns.m)%in%input$EphysSelection,]
      data = data[order(rownames(data)),] 
    }
    if(input$CTmean){ # Take cell type mean
      if(!is.null(nrow(data))){
        data = apply(data, 2, function(x) tapply(x, rownames(data), mean))
      }
    }else{ # Otherwise differently name neurons of the same cell type
      tbl = table(rownames(data))
      r = rownames(data)
      for(t in 1:length(tbl)){
        n = names(tbl)[t]
        r[r%in%n] = paste0(n,"#",1:tbl[t])
      }
      rownames(data) = r 
    }
    # Get the ploting order for odours, in order to make a tuning curve
    if(!is.null(nrow(data))){
      df = reshape2::melt(data)
      ordered = sort(tapply(df$value, df$Var2, FUN=max),decreasing = TRUE)
      smells = colnames(data)
    }else{
      ordered = sort(data,decreasing=TRUE)
      smells = names(data)
    }
    alternating = c()
    counter = 0
    for(i in 1:length(ordered)){ # Order tuning curve data
      position = ifelse((as.integer(i) %% 2)==1,counter,-counter)
      a = ceiling(length(ordered)/2)+position
      names(a) = names(ordered[i])
      alternating = c(alternating,a)
      counter = ifelse((as.integer(i) %% 2)==1,counter+1,counter)
    }
    plot.order = alternating[match(smells,names(alternating))]
    # Choose colours for cell.types
    if(length(unique(input$EphysSelection))<2){
      plotting.colours = united.orange # Nice orange colour, for single cell type plots
    }else{
      plotting.colours = darjeeling(length(unique(as.character(input$EphysSelection))))
    }
    names(plotting.colours) = unique(as.character(input$EphysSelection)) 
    # Create traces to plot
    traces = list()
    p <- plotly::plot_ly()
    if(is.null(nrow(data))){ # Plot one trace
      idata = data[match(1:length(data),plot.order)]
      ct = unique(as.character(input$EphysSelection))
      colour = united.orange # Plot level orange line if there is no true data
      trace = list(
        spikes = idata,
        odours = names(idata),
        name = ct,
        line = list(
          color = paste0("rgba(",paste(grDevices::col2rgb(colour,alpha=TRUE),collapse=", "),")"),
          shape = "spline",
          smoothing = 1.3,
          width = 3
        ),
        mode = "lines",
        type = "scatter"
      )
      trace$odours <- factor(trace$odours, levels = trace[["odours"]])
      p <- plotly::add_trace(p,x=trace$odours,y=trace$spikes,connectgaps=trace$connectgaps, line=trace$line, mode=trace$mode, name=trace$name, type=trace$type)
    }else{ # Plot multiple
      for(i in 1:nrow(data)){
        idata = data[i,match(1:ncol(data),plot.order)]
        ct = rownames(data)[i]
        colour = plotting.colours[gsub("#.","",ct)] # make sure neurons of the same cell type are the same colour
        if(grepl("#",ct)){
          alpha = 1.5/as.numeric(gsub(".*#","",ct)) # Let alpha indicate the individual in the cell type  
        }else{
          alpha = 1 # Else a fully opaque line
        }
        trace = list(
          spikes = idata,
          odours = names(idata),
          name = ct,
          line = list(
            color = paste0("rgba(",paste(grDevices::col2rgb(colour,alpha=FALSE),collapse=", "),", ",alpha,")"), 
            shape = "spline",
            smoothing = 1.3,
            width = 3
          ),
          mode = "lines",
          type = "scatter"
        )
        trace$odours <- factor(trace$odours, levels = trace[["odours"]])
        p <- plotly::add_trace(p,x=trace$odours,y=trace$spikes,connectgaps=trace$connectgaps, line=trace$line, mode=trace$mode, name=trace$name, type=trace$type)
      }
    }
    p <- layout(p,margin = list(b = 200),yaxis = list (title = "spike number")) # margin arument prevents x-axis cut off
    p
  })
  
  # A plotly graph for the E-Phys data as tuning curves
  output$OdoursResponses <- plotly::renderPlotly({
    if(is.null(input$OdourSelection)){ # If no odours are selected
      data = t(as.matrix(rep(0,nrow(ephys.lhns.m))))
      colnames(data) = rownames(ephys.lhns.m)
    }else{ # A set of multiple odours
      data = t(ephys.lhns.m[,colnames(ephys.lhns.m)%in%input$OdourSelection])
    }
    if(input$OdourMean){ # Take odours' mean
      if(!is.null(nrow(data))){
        data = colSums(data)
        ns = names(data)
        data = t(as.matrix(data))
        colnames(data) = ns
      }
    }
    data = data[,order(colnames(data))]
    if(!"matrix"%in%class(data)){
      ns = names(data)
      data = t(as.matrix(data))
      colnames(data) = ns
    }
    if(input$OdourCTMean){ # Take cell type mean
      data = t(apply(data, 1, function(x) tapply(x, colnames(data), mean)))
    }else{
      tbl = table(colnames(data))
      r = colnames(data)
      for(t in 1:length(tbl)){
        n = names(tbl)[t]
        r[r%in%n] = paste0(n,"#",1:tbl[t])
      }
      colnames(data) = r 
    }
    df = reshape2::melt(data)
    ordered = sort(tapply(df$value, df$Var2, FUN=max),decreasing = TRUE)
    cts = colnames(data)
    # Get the ploting order for odours, in order to make a tuning curve
    alternating = c()
    counter = 0
    for(i in 1:length(ordered)){ # Order tuning curve data
      position = ifelse((as.integer(i) %% 2)==1,counter,-counter)
      a = ceiling(length(ordered)/2)+position
      names(a) = names(ordered[i])
      alternating = c(alternating,a)
      counter = ifelse((as.integer(i) %% 2)==1,counter+1,counter)
    }
    plot.order = alternating[match(cts,names(alternating))]
    # Choose colours for odours
    if(length(unique(input$OdourSelection))<2){
      plotting.colours = united.orange # Nice orange colour, for single cell type plots
    }else{
      plotting.colours = darjeeling(length(unique(as.character(input$OdourSelection))))
    }
    names(plotting.colours) = unique(as.character(input$OdourSelection)) 
    # Create traces to plot
    traces = list()
    p <- plotly::plot_ly()
    if(is.null(nrow(data))){ # Plot one trace
      idata = data[match(1:length(data),plot.order)]
      odour = unique(as.character(input$OdourSelection))
      colour = united.orange # Plot level orange line if there is no true data
      trace = list(
        spikes = idata,
        cell.types = names(idata),
        name = odour,
        line = list(
          color = paste0("rgba(",paste(grDevices::col2rgb(colour,alpha=TRUE),collapse=", "),")"),
          shape = "spline",
          smoothing = 1.3,
          width = 3
        ),
        mode = "lines",
        type = "scatter"
      )
      trace$cell.types <- factor(trace$cell.types, levels = trace[["cell.types"]])
      p <- plotly::add_trace(p,x=trace$cell.types,y=trace$spikes,connectgaps=trace$connectgaps, line=trace$line, mode=trace$mode, name=trace$name, type=trace$type)
    }else{ # Plot multiple traces
      for(i in 1:nrow(data)){
        idata = data[i,match(1:ncol(data),plot.order)]
        odour = rownames(data)[i]
        if(is.null(odour)){
          colour = united.orange
        }else{
          colour = plotting.colours[odour]
        }
        trace = list(
          spikes = idata,
          cell.types = names(idata),
          name = odour,
          line = list(
            color = paste0("rgba(",paste(grDevices::col2rgb(colour,alpha=TRUE),collapse=", "),")"), 
            shape = "spline",
            smoothing = 1.3,
            width = 3
          ),
          mode = "lines",
          type = "scatter"
        )
        trace$cell.types <- factor(trace$cell.types, levels = trace[["cell.types"]])
        p <- plotly::add_trace(p,x=trace$cell.types,y=trace$spikes,connectgaps=trace$connectgaps, line=trace$line, mode=trace$mode, name=trace$name, type=trace$type)
      }
    }
    p <- layout(p,margin = list(b = 160),yaxis = list (title = "spike number"))
    p
  })
  
  
  ####################
  # Upload a tracing #
  ####################
  
  # Upload a neuron tracing
  observeEvent(input$Upload,{
    showModal(modal_upload)
  })
  
  # Upload data
  modal_upload <-modalDialog(
    fluidPage(
      h3(strong("upload tracing"),align="center"),
      selectInput(inputId = 'TracingType', label = 'upload type', choices = list(`local file`="UserUpload",`CATMAID API`="CATMAID"), selected = list(`local file`="UserUpload")),
      conditionalPanel(condition = "input.TracingType == 'UserUpload'",
        HTML("Upload a tracing (e.g. .swc or .rds file) or skeletonised neuron (e.g. .nrrd). 
               You can then compare your neuron against the LH-associated neurons in our library visually and via <a href='https://www.ncbi.nlm.nih.gov/pubmed/27373836'>NBLAST</a>.
               The query neuron will at first be plotted in <b><span style='color: black;'>black</span></b> or <b><span style='color: grey;'>grey</span></b> in the 3D viewer. 
               The tracing to be uploaded must be registered into a standard fly brainspace.
               Supported neuron file formats include: swc (.swc), vtk (.vtk), fijitraces (.traces, .xml), amiramesh (.am, .amiramesh), hxlines (.am), hxskels (.am), neuroml (.xml, .nml), nrrd (.nrrd, .nhdr), rds (.rds), swcng (.swc), and vaa3draw (.v3d, .v3draw).
               You can learn more about registering your neuron and bridging it between templates <a href='http://dx.doi.org/10.1016/j.neuron.2016.06.012'>here</a>.
               See our about section for more information."),             
        hr(), 
        fileInput('TracingFile', label = NULL, multiple = TRUE, placeholder = "No file selected"),
        selectInput('TracingBrain', 'template brain',
                    choices = list(`FlyCircuit (FCWB)`= "FCWB",`Janelia FlyLight/VFB (JFRC2010/JFRC2)`= "JFRC2",`Janelia FlyLight (JFRC2013)`= "JFRC2013" ,`Cambridge (IS2)`= "IS2", `Vienna (T1)`= "T1",`Janelia EM (FAFB14)`= "FAFB14"),
                    selected =  list(`FlyCircuit (FCWB)`= "FCWB"),
                    multiple = FALSE,
                    selectize = TRUE)
      ),
      conditionalPanel(condition = "input.TracingType == 'CATMAID'",
                       HTML("Upload a synaptic-resolution tracing using the CATMAID API, based on electron microscopy data.
                            You can then compare your neuron against the LH-associated neurons in our library visually and via <a href='https://www.ncbi.nlm.nih.gov/pubmed/27373836'>NBLAST</a>.
                            The query neuron will at first be plotted in <b><span style='color: black;'>black</span></b> or <b><span style='color: grey;'>grey</span></b> in the 3D viewer.
                            To do this, you will need login details to a CATMAID server for fly brain data.
                            You will need a URL for a CATMAID server and a CATMAID <a href='http://catmaid.readthedocs.io/en/stable/api.html'>token</a>.
                            You may also need the http author username/author password that optionally secures a CATMAID website.
                            These are not the same as your CATMAID login details."),             
                       hr(),
                       uiOutput("CATMAID_detail_server"),
                       uiOutput("CATMAID_detail_authname"),
                       uiOutput("CATMAID_detail_authpassword"),
                       uiOutput("CATMAID_detail_token"),
                       selectInput('CATMAID_searchtype', 'search type',
                                   choices = list(`neuron annotation (regex accepted)`= "annotation",`neuron name (regex accepted)`= "name",`skeleton ID (multiples separated by a space)`= "skid"),
                                   selected =  list(`neuron name (regex accepted)`= "name"),
                                   multiple = FALSE,
                                   selectize = TRUE),
                       textInput(inputId = "CATMAID_search", label = "search neurons:", value = "", width = NULL, placeholder = "Enter CATMAID search term")
      ),
      checkboxInput("TracingMirror", "flip brain-side", value=FALSE),
      actionButton(inputId = "TracingUploaded",label = "upload",icon = icon("upload")),
      conditionalPanel(condition = "output.CATMAID_warning == true",
                       HTML("error")
      )
    )
  )
  
  # Show error panel when there's an issue pulling neurons from CATMAID
  output$CATMAID_warning <- reactive({
    if(vals$CATMAID$CATMAID_warning==TRUE){
     return(TRUE) 
    }else{
     return(FALSE)
    }
  })
  
  # Select CATMAID login details, and remember them by using a reactive object!
  output$CATMAID_detail_server <- renderUI({
    textInput(inputId = "CATMAID_server", label = "server:", value = vals$CATMAID$CATMAID_server, width = NULL, placeholder = "Enter CATMAID server")
  })
  output$CATMAID_detail_authname <- renderUI({
    textInput(inputId = "CATMAID_authname", label = "CATMAID author:", value = vals$CATMAID$CATMAID_authname, width = NULL, placeholder = "Enter CATMAID author username")
  })
  output$CATMAID_detail_authpassword <- renderUI({
    textInput(inputId = "CATMAID_authpassword", label = "CATMAID password:", value = vals$CATMAID$CATMAID_authpassword, width = NULL, placeholder = "Enter CATMAID author password")
  })
  output$CATMAID_detail_token <- renderUI({
    textInput(inputId = "CATMAID_token", label = "CATMAID token:", value = vals$CATMAID$CATMAID_token, width = NULL, placeholder = "Enter CATMAID token")
  })

  # Add uploaded neurons to vals$neurons and vals$df
  observeEvent(input$TracingUploaded, {
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    progress$set(message = "uploading neurons", value = 0)
    # update CATMAID login values
    vals$CATMAID$CATMAID_server = input$CATMAID_server
    vals$CATMAID$CATMAID_authname = input$CATMAID_authname
    vals$CATMAID$CATMAID_authpassword = input$CATMAID_authpassword
    vals$CATMAID$CATMAID_token = input$CATMAID_token
    vals$CATMAID$CATMAID_warning = FALSE
    isolate({
      tracing_neurons <- nat::neuronlist()
      if(input$TracingType=="CATMAID"){ # Access the catmaid API
        CATMAID.conn = tryCatch(shiny_catmaid_login(server = input$CATMAID_server,authname = input$CATMAID_authname,authpassword = input$CATMAID_authpassword, token = input$CATMAID_token), error = function(e) NULL)
        if(input$CATMAID_searchtype!="skid"){
          skids = paste0(input$CATMAID_searchtype,":",input$CATMAID_search)
        }else{
          skids = strsplit(input$CATMAID_search," ") # Separated by a space
        }
        progress$inc(1/3, detail = "pulling neurons from CATMAID")
        tracing_neurons <- tryCatch(elmr::fetchn_fafb(skids = skids, mirror = input$TracingMirror, reference = nat.flybrains::FCWB), error = function(e) NULL)
        if(is.null(tracing_neurons)){
          warning("CATMAID pull failed. Login details may be incorrect, search terms do not exist in database.")
          vals$CATMAID$CATMAID_warning = TRUE
        }else{
          progress$inc(2/3, detail = "transforming neurons")
          # tracing_neurons = nat::nlapply(tracing_neurons,catnat:::resample.catmaidneuron,stepsize = 1) # TODO resample with connectors
          tdf = as.data.frame(matrix("unknown",nrow = length(tracing_neurons),ncol = length(selected_columns), dimnames = list(names(tracing_neurons),selected_columns)))
          tdf[,"id"] = tracing_neurons[,"name"]
          tdf[,"skeleton.type"] = input$TracingType
          tdf[,"colour"] = grDevices::grey.colors(n=length(tracing_neurons),start=0,end=0.5) # A range of shades of grey
          attr(tracing_neurons,"df") = tdf # Attach meta data 
        }
      }else{ # Add user uploaded file
        template_brain <- input$TracingBrain
        for(tracingfile in 1:length(input$TracingFile$name)){ # If multiple selected
          query_neuron <- input$TracingFile
          progress$inc(1/3, detail = "reading neurons")
          if(is.null(query_neuron)) return(NULL)
          if(grepl("\\.nrrd", query_neuron$name[tracingfile])) { # TODO come up with a heuristic to choose the number of neighbours (k) based on the voxel dimensions
            tracing_neurons <-  c(tracing_neurons,dotprops_from_nrrd(query_neuron$datapath[tracingfile], k=10))
          } else {
            if (grepl("\\.swc|\\.xml|\\.nml\\.vtk\\.traces", query_neuron$name[tracingfile])){
              tracing_neurons <- c(tracing_neurons,nat:::read.neurons(query_neuron$datapath[tracingfile]))
            } else if (grepl("\\.rds", query_neuron$name[tracingfile])){
              tracing_neurons <- base::readRDS(query_neuron$datapath[tracingfile])
            }else{
              tracing_neurons <-  c(tracing_neurons,nat:::read.neurons(query_neuron$datapath[tracingfile]))
              if("neuronlist"%in%class(tracing_neurons)|"neuron"%in%class(tracing_neurons)) {
                tracing_neurons <-  c(tracing_neurons,nat::dotprops(query_neuron$datapath[tracingfile],k=5))
              }
            } # TODO Or some sort of error?  
          }
        }
        progress$inc(2/3, detail = "transforming neurons")
        tracing_neurons = nat::as.neuronlist(tracing_neurons) # Make sure we are working with 'neuronlist' not 'neuron' objects
        # Set up metadata
        tdf = as.data.frame(matrix("unknown",nrow = length(tracing_neurons),ncol = length(selected_columns), dimnames = list(names(tracing_neurons),selected_columns)))
        if(length(input$TracingFile$name)==length(tracing_neurons)){
          tdf[,"id"] = rownames(tdf) = input$TracingFile$name
        }else{
          tdf[,"id"] = rownames(tdf) = names(tracing_neurons)  
        }
        tdf[,"skeleton.type"] = input$TracingType
        tdf[,"colour"] = grDevices::grey.colors(n=length(tracing_neurons),start=0,end=0.5) # A range of shades of grey
        attr(tracing_neurons,"df") = tdf # Attach meta data
        # Comoute transforms
        if(input$TracingBrain!="FCWB"){ # Transform the brain if it needs to be transformed
          tracing_neurons = nat.templatebrains::xform_brain(tracing_neurons,sample = input$TracingBrain,reference = FCWB)
        }
        if(!"dotprops"%in%class(tracing_neurons)[[1]]){ # Resample to one micron spacing 
          tracing_neurons = nat::nlapply(tracing_neurons,nat::resample,stepsize = 1)
        }
        if(input$TracingMirror){ # Mirror neuron, if that's asked for
          tracing_neurons = nat.templatebrains::mirror_brain(tracing_neurons,brain = FCWB, mirrorAxis = "X")
        }
      }
    })
    if(!is.null(tracing_neurons)){
      progress$inc(1, detail = "plotting neurons")
      # Now add these to the already selected library neurons
      original = vals$neurons
      # Add neurons
      new = tracing_neurons[!names(tracing_neurons)%in%names(original)] # Prevent neurons of the same name being added together
      # Neurons are deleted from the selection via an observe app
      vals$neurons <- c(original,new)
      vals$neuronsDF <- data.table::data.table(vals$neurons[,])
    }
  })
  
  

  
  
  output$Test = renderPrint({
    str(vals$CATMAID)
    #s =  length(vals$neurons)
    # if(length(s)>0){
    #   s = update_neurons(input=input,db=s)
    # }
    #if (length(s)) {
    #cat(s, sep = ', ')
    #}
  })


})
