# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

# Load the packages we need for this App
source("packages.R")


shinyServer(function(input, output, session) {
  
  # Get neurons for plotting and selection table
  vals <- reactiveValues()
  vals$zoom = 0.4 # Zoom onto brain
  vals$um = structure(c(1, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 0, 1), .Dim = c(4L, 4L)) # Frame of view
  vals$neurons <- subset(all.neurons,cell.type=="pd2a1")
  vals$neuronsDF <- data.table::data.table(subset(all.neurons,cell.type=="pd2a1")[,selected_columns])
  
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
      rgl::plot3d(neurons,soma=T,lwd=3, col = neurons[,"colour"],skipRedraw = TRUE)
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
  
  # Delete rows that have been checked
  observeEvent(input$Col_row_head,{
    showModal(modal_recolor_multiple)
  })
  
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
          alpha = 1/as.numeric(gsub(".*#","",ct)) # Let alpha indicate the individual in the cell type  
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
  
  output$Test = renderPrint({
    s =  input$cts_in_data
    # if(length(s)>0){
    #   s = update_neurons(input=input,db=s)
    # }
    #if (length(s)) {
    cat(s, sep = ', ')
    #}
  })


})
