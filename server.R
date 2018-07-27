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
  vals$neurons <- subset(all.neurons,cell.type=="PD2a1"&skeleton.type=="FlyCircuit")
  vals$neuronsDF <- data.table::data.table(subset(all.neurons,cell.type=="PD2a1")[,selected_columns])
  vals$CATMAID = list(CATMAID_server = "https://neuropil.janelia.org/tracing/fafb/v14/", CATMAID_authname= NULL,CATMAID_authpassword = NULL, CATMAID_token = NULL)
  vals$NBLAST = list(tracings = NULL, result = NULL, matches = NULL)
  vals$split_brain_images_chosen = split_brain_images
  
  
  ############
  # Carousel #
  ############
  
  lapply(1:length(pnt_lhns), function(i) {
    output[[paste0("Carousel",pnt_lhns[i])]] <- renderSlickR({
      images = AG_images[grepl(paste0(pnt_lhns[i],"[a-z]"),AG_images)]
      slickR(obj = images,height = '100%',width='100%')
    })
  })
  
  lapply(1:length(PN_images), function(i) {
    observeEvent(input[[PN_images[i]]], {
      showModal(modalDialog(
        img(src = PN_images[i],height = "470px",width="879px",align = "center")
      ))
    })
  })
    
  
  ###################
  # Splitline Modal #
  ###################
  
  lapply(1:length(lines), function(i) {
    observeEvent(input[[lines[i]]], {
      showModal(modalDialog(
        title = lines[i],
        tabsetPanel(type = "tabs",
                    tabPanel("brain", img(src = split_brain_images[grepl(paste0(lines[i],".jpg"),split_brain_images)],height = "470px",width="879px",align = "center")),
                    tabPanel("VNC", img(src = split_vnc_images[grepl(paste0(lines[i],".jpg"),split_vnc_images)],height = "470px",width="879px",align = "center"))
        ),
        br(),
        fluidRow(
          column(3,
                 p(strong("cell.type: "),lhns::lh_line_info[lines[i],c("cell.type")]),
                 p(strong("type: "),lhns::lh_line_info[lines[i],c("type")])
                 ),
          column(3,
                 p(strong("neurotransmitter: "),lhns::lh_line_info[lines[i],c("neurotransmitter")]),
                 p(strong("MCFO available: "),lhns::lh_line_info[lines[i],c("MCFO")])
          ),
          column(3,
                 p(strong("AD: "),lhns::lh_line_info[lines[i],c("AD")]),
                 p(strong("DBD: "),lhns::lh_line_info[lines[i],c("DBD")])
          ),
          column(3,
                 p(strong("stablestock available: "),lhns::lh_line_info[lines[i],c("stablestock")]),
                 p(strong("VNC expression: "),lhns::lh_line_info[lines[i],c("VNC")])
          )
        ),
        easyClose = TRUE
      ))
    })
  })
  
  observeEvent({input$splittype
               input$splitNT},{
               vals$split_brain_images_chosen = split_brain_images
               if(input$splittype!="all"){
                 lines_chosen = unique(as.character(subset(lhns::lh_line_info[lines,],grepl(input$splittype,type))$linecode))
                 vals$split_brain_images_chosen = vals$split_brain_images_chosen[as.logical(rowSums(sapply(lines_chosen,function(x) grepl(paste0(x,".jpg"),vals$split_brain_images_chosen))))]
               }
               if(input$splitNT!="all"){
                 lines_chosen = unique(as.character(subset(lhns::lh_line_info[lines,],grepl(input$splitNT,neurotransmitter))$linecode))
                 vals$split_brain_images_chosen = vals$split_brain_images_chosen[as.logical(rowSums(sapply(lines_chosen,function(x) grepl(paste0(x,".jpg"),vals$split_brain_images_chosen))))]
               }
  })
  
  output$imageGrid <- renderUI({
    fluidRow(
         lapply(1:length(vals$split_brain_images_chosen), function(i) {
           column(3,
             shiny::strong(sapply(vals$split_brain_images_chosen, function(x )gsub(".jpg","",tail(unlist(strsplit(x,'/')),n=1)))[i]),
             tags$button(
             id = sapply(vals$split_brain_images_chosen, function(x )gsub(".jpg","",tail(unlist(strsplit(x,'/')),n=1)))[i],
             class = "btn action-button",
             tags$img(src = vals$split_brain_images_chosen[i],height = "313px",width="586px")
           ))
         })
    )
  })
  

  ###############
  # Get Neurons #
  ###############
  
  # Dynamically create neurons and neuronsDF object
  observeEvent(input$Append, {
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
    if(input$Type%in%c("ON","LN","IN","MBON","LHN")){
      LHN_choices =  sort(unique(subset(all.neurons,skeleton.type%in%input$SkeletonType&type%in%input$Type)[,"cell.type"]))
      selectInput("lhns", label = paste0("Cell types in dataset (",length(LHN_choices),") :"), choices = LHN_choices,selected = NULL, multiple=TRUE, selectize=TRUE)
    }
  })
  
  # Dynamically update PNT selection, depending on which data type is picked 
  output$PNTselection <- renderUI({
    if(is_lhn_type(input$Type)){
      PNT_all =  sort(unique(subset(all.neurons,skeleton.type%in%input$SkeletonType&type%in%input$Type)[,"pnt"]))
      PNT_choices = list(`Anterior dorsal`=PNT_all[grepl("^ad",PNT_all)],`Anterior ventral`=PNT_all[grepl("^av",PNT_all)],`Posterior dorsal`=PNT_all[grepl("^pd",PNT_all)],`Posterior dorsal`=PNT_all[grepl("^pv",PNT_all)])
      PNT_choices = c(PNT_choices, list(`Other`= c(PNT_all[!PNT_all%in%unlist(PNT_choices)])))
      selectInput("PNT", label = paste0("Primary neurite tracts in dataset (",length(unlist(PNT_choices)),") :"), choices = PNT_choices,selected = NULL, multiple=TRUE, selectize=TRUE)
    }
  })
  
  # Dynamically update AG selection, depending on which PNTs are picked 
  output$AGselection <- renderUI({
    if(is_lhn_type(input$Type)){
      AG_choices =  sort(unique(subset(all.neurons[,],pnt%in%input$PNT&skeleton.type%in%input$SkeletonType&type%in%input$Type)[,"anatomy.group"]))
      selectInput("AG", label = paste0("Anatomy groups (",length(AG_choices),") :"), choices = c("all in selected primary neurite tracts",AG_choices),selected = "all in selected primary neurite tracts", multiple=TRUE, selectize=TRUE)
    }
  })
  
  # Dynamically update CT selection, depending on which PNTs and AGs are picked 
  output$CTselection <- renderUI({
    if(is_lhn_type(input$Type)){
      if("all in selected primary neurite tracts"%in%input$AG){
        CT_choices = sort(unique(subset(all.neurons[,],pnt%in%input$PNT&skeleton.type%in%input$SkeletonType&type%in%input$Type)[,"cell.type"]))
      }else{
        CT_choices = sort(unique(subset(all.neurons[,],anatomy.group%in%input$AG&skeleton.type%in%input$SkeletonType)[,"cell.type"]))
      }
      selectInput("CT", label = paste0("Cell types (",length(CT_choices),") :"), choices = c("all in selected anatomy groups",CT_choices), selected = "all in selected anatomy groups", multiple=TRUE, selectize=TRUE)
    }
  })
  
  # MBON selection
  output$MBONselection <- renderUI({
    if(!is_lhn_type(input$Type)){
      if(grepl("MBON",input$Type)){
        MBON_choices = sort(unique(subset(all.neurons[,],skeleton.type%in%input$SkeletonType&grepl("MBON",type))[,"cell.type"]))
        selectInput("MBON", label = paste0("MBON types (",length(MBON_choices),") :"), choices = MBON_choices, selected = MBON_choices[1], multiple=TRUE, selectize=TRUE)
      }
    }
  })
  
  
  # PN types selection
  output$PNtype <- renderUI({
    if(!is_lhn_type(input$Type)){
      if(grepl("IN",input$Type)){
        input_all = sort(as.character(sort(unique(subset(all.neurons[,],skeleton.type%in%input$SkeletonType&grepl("IN",type))[,"anatomy.group"]))))
        input_choices = list(`medial antennal lobe tract (mALT)`= input_all[grepl("AL-mALT",input_all )], 
                             `mediolateral antennal lobe tract (mlALT)`= input_all[grepl("AL-mlALT",input_all )],
                             `lateral antennal lobe tract (lALT)`= input_all[grepl("AL-lALT",input_all )],
                             `transverse antennal lobe tract`= input_all[grepl("AL-t",input_all )],
                             `Putative gustatory projections`= input_all[grepl("GNG|mAL-PN|SOG",input_all )],
                             `Putative mechanosensory projections`= input_all[grepl("WED",input_all)],
                             `Centrifgual projections`= input_all[grepl("Centrifugal",input_all )],
                             `Visual projections`= input_all[grepl("LO",input_all )],
                             `Other`= input_all[grepl("notLHproper|Expansive",input_all )])
        input_choices = input_choices[sapply(input_choices,length)>1] # get rid of empty fields
        selectInput("PNtype", label = paste0("Projection neuron types (",length(unlist(input_choices)),") :"), choices = input_choices, selected = NULL, multiple=TRUE, selectize=TRUE)
      }
    }
  })
  
  # PN selection
  output$PNselection <- renderUI({
    if(!is_lhn_type(input$Type)){
      if(grepl("IN",input$Type)){
        input_all = sort(as.character(sort(unique(subset(all.neurons[,],skeleton.type%in%input$SkeletonType&grepl("IN",type)&anatomy.group%in%input$PNtype)[,"cell.type"]))))
        input_choices = list(`medial antennal lobe tract (mALT)`= input_all[grepl("AL-mALT",input_all )], 
                             `mediolateral antennal lobe tract (mlALT)`= input_all[grepl("AL-mlALT",input_all )],
                             `lateral antennal lobe tract (lALT)`= input_all[grepl("AL-lALT",input_all )],
                             `transverse antennal lobe tract`= input_all[grepl("AL-t",input_all )],
                             `Putative gustatory projections`= input_all[grepl("GNG|mAL-PN|SOG",input_all )],
                             `Putative mechanosensory projections`= input_all[grepl("WED",input_all)],
                             `Centrifgual projections`= input_all[grepl("Centrifugal",input_all )],
                             `Visual projections`= input_all[grepl("LO",input_all )],
                             `Other`= input_all[grepl("notLHproper|Expansive",input_all )])
        input_choices = input_choices[sapply(input_choices,length)>1] # get rid of empty fields
        selectInput("PN", label = paste0("Projection neuron types (",length(unlist(input_choices)),") :"), choices = input_choices, selected = NULL, multiple=TRUE, selectize=TRUE)
      }
    }
  })
  
  ############
  # 3D Viewer #
  ############
  
  # The 3D RGL window
  output$plot3D <- renderRglwidget({
    # Clear the space
    rgl::clear3d()
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
    shiny::fluidPage(
      box(width=12,
            column(6,offset = 0,
                   shiny::HTML('<div class="btn-group" role="group" aria-label="Basic example">'),
                   actionButton(inputId = "Del_row_head",label = "delete selected"),
                   actionButton(inputId = "Col_row_head",label = "recolour selected"),
                   actionButton(inputId = "Compare_row_head",label = "compare selected"),
                   actionButton(inputId = "Download",label = "download data"),
                   shiny::HTML('</div>')
            ),
            shiny::column(width = 12, 
                   DT::dataTableOutput("SelectionTable") 
                   ),
              tags$script(shiny::HTML('$(document).on("click", "input", function () {
                               var checkboxes = document.getElementsByName("row_selected");
                               var checkboxesChecked = [];
                               for (var i=0; i<checkboxes.length; i++) {
                                    if (checkboxes[i].checked) {
                                        checkboxesChecked.push(checkboxes[i].value);
                                    }
                               } 
                             Shiny.onInputChange("checked_rows",checkboxesChecked);})'
              )),# This shiny::HTML code assigns input$checked_rows, so we know which rows in the table are checked
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
    h3(shiny::strong("selected neurons"),align="center"),
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
      h3(shiny::strong("select a colour"),align="center"),
      uiOutput("OldColour"),
      actionButton("ChangeColour","change")
    )
  )
  
  # Modify colours
  modal_recolor_multiple <-modalDialog(
    fluidPage(
      h3(shiny::strong("select a colour"),align="center"),
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
      h3(shiny::strong("download data"),align="center"),
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
      data = rep(0,ncol(ephys.lhns.m)) # ephys.lhns.m is a matrix
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
    p$elementId <- NULL
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
    p$elementId <- NULL # Avoid R Shiny warning abotu unusd unique plotly IDs
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
      h3(shiny::strong("upload tracing"),align="center"),
      selectInput(inputId = 'TracingType', label = 'upload type', choices = list(`local file`="UserUpload",`CATMAID API`="CATMAID"), selected = list(`local file`="UserUpload")),
      conditionalPanel(condition = "input.TracingType == 'UserUpload'",
        shiny::HTML("Upload a tracing (e.g. .swc or .rds file) or skeletonised neuron (e.g. .nrrd). 
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
                       shiny::HTML("Upload a synaptic-resolution tracing using the CATMAID API, based on electron microscopy data.
                            You can then compare your neuron against the LH-associated neurons in our library visually and via <a href='https://www.ncbi.nlm.nih.gov/pubmed/27373836'>NBLAST</a>.
                            The query neuron will at first be plotted in <b><span style='color: black;'>black</span></b> or <b><span style='color: grey;'>grey</span></b> in the 3D viewer.
                            To do this, you will need login details to a CATMAID server for fly brain data.
                            You will need a URL for a CATMAID server and a CATMAID <a href='http://catmaid.readthedocs.io/en/stable/api.html'>token</a>.
                            You may also need the http basic authorisation username/password that optionally secure a CATMAID website.
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
      actionButton(inputId = "TracingUploaded",label = "upload",icon = icon("upload"))
    )
  )
  
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
        }else{
          progress$inc(2/3, detail = "transforming neurons")
          # tracing_neurons = nat::nlapply(tracing_neurons,catnat:::resample.catmaidneuron,stepsize = 1) # TODO resample with connectors
          tdf = as.data.frame(matrix("unknown",nrow = length(tracing_neurons),ncol = length(selected_columns), dimnames = list(names(tracing_neurons),selected_columns)))
          tdf[,"id"] = tracing_neurons[,"name"]
          tdf[,"skeleton.type"] = input$TracingType
          tdf[,"colour"] = grDevices::grey.colors(n=length(tracing_neurons),start=0,end=0.5) # A range of shades of grey
          attr(tracing_neurons,"df") = tdf # Attach meta data 
          names(tracing_neurons) = tdf[,"id"] # Make sure names are same as ID
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
        names(tracing_neurons) = tdf[,"id"]
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
  
  #########
  # NBLAST #
  #########
  
  # Dynamically update Uploaded Neurons selection
  output$ChooseUploadedSkeletons <- renderUI({
    if(input$QueryType=="UserUpload"){
        uploaded.neurons = subset(vals$neurons,skeleton.type%in%c("UserUpload","CATMAID"))
        Upload_choices = names(uploaded.neurons)
        if(length(Upload_choices)==0){
          Upload_choices = "no user uploaded neurons"
        }
        selectInput("UploadedSkeletons", label = paste0("uploaded neurons (",length(uploaded.neurons),") :"), choices = Upload_choices,selected = Upload_choices[1], multiple=TRUE, selectize=TRUE)
    }
  })
  
  # If choosing from library, select what data type first
  output$NBLAST_SkeletonType <- renderUI({
    if(input$QueryType=="Library"){
      selectInput(inputId='NBLAST_SkeletonType', label='dataset:', choices = sort(unique(all.neurons[,"skeleton.type"])), selected = "FlyCircuit", multiple=FALSE, selectize=TRUE)
    }
  })
  
  output$NBLAST_ChooseFromLibrary <- renderUI({
    if(input$QueryType=="Library"){
      possible = subset(vals$neurons,skeleton.type%in%input$NBLAST_SkeletonType)
      library_choices = unique(sort(possible[,"cell.type"]))
      selectInput(inputId='NBLAST_ChooseFromLibrary', label=paste0('selected cell types (',length(library_choices),') :'), choices = library_choices, selected = NULL, multiple=FALSE, selectize=TRUE)
    }
  })
  
  output$NBLAST_ChooseID <- renderUI({
    if(input$QueryType=="Library"){
      possible = subset(vals$neurons,skeleton.type%in%input$NBLAST_SkeletonType&cell.type%in%input$NBLAST_ChooseFromLibrary)
      skel_choices = subset(vals$neurons,cell.type%in%input$NBLAST_ChooseFromLibrary)[,"id"]
      selectInput(inputId='NBLAST_ChooseID', label=paste0('individual neurons (',length(skel_choices),') :'), choices = skel_choices, selected = skel_choices[1], multiple=TRUE, selectize=TRUE)
    }
  })

  # Get tracing and perform NBLAST
  observeEvent(input$NBLASTGO, {
    if(input$QueryType=="UserUpload"){
      if(input$UploadedSkeletons!="no user uploaded neurons"){
        vals$NBLAST$tracings =  nat::dotprops(vals$neurons[input$UploadedSkeletons])
      }else{
        vals$NBLAST$tracings = NULL
      }  
      # Get the NBLAST tracing scores for the uploaded object
      isolate({ 
        query_neurons.dps <- vals$NBLAST$tracings
        if(is.null(query_neurons.dps)) {return(NULL)}
        scores <- list()
        shiny::withProgress(min=1, max=10, message="NBLAST in progress", expr={ # NBLAST progress bar
        for(i in 1:10) {
              chunk <- split(1:length(all.neurons.dps), cut(1:length(all.neurons.dps), 10))[[i]]
              if(input$UseMean) {
                if(length(query_neurons.dps)>1){
                  mean.score <- (nat.nblast::nblast(query_neurons.dps, all.neurons.dps[chunk], normalised=TRUE, UseAlpha = TRUE) + t(nat.nblast::nblast(all.neurons.dps[chunk], query_neurons.dps, normalised=TRUE,UseAlpha = TRUE))) / 2
                  mean.score <- matrix(rowMeans(mean.score),ncol=1,dimnames = list(rownames(mean.score),0)) # Average over multiple query neurons
                  scores[[i]] <- mean.score
                }else{
                  scores[[i]] <- (nat.nblast::nblast(query_neurons.dps, all.neurons.dps[chunk], normalised=TRUE, UseAlpha = TRUE) + nat.nblast::nblast(all.neurons.dps[chunk], query_neurons.dps, normalised=TRUE)) / 2
                }
              } else {
                if(length(query_neurons.dps)>1){
                  mean.score <- nat.nblast::nblast(query_neurons.dps, all.neurons.dps[chunk],normalised=TRUE, UseAlpha = TRUE)
                  mean.score <- matrix(rowMeans(mean.score),ncol=1,dimnames = list(rownames(mean.score),0)) # Average over multiple query neurons
                  scores[[i]] <- mean.score
                }else{
                  scores[[i]] <- nat.nblast::nblast(query_neurons.dps, all.neurons.dps[chunk],normalised=TRUE, UseAlpha = TRUE)
                }
              }
              shiny::setProgress(value=i)
          }
        })
      })
      scores = base::unlist(scores,use.names = TRUE) # Unlist, preserve names
      names(scores) = names(all.neurons.dps) # Hmm, make sure names are preserved
    }else if (input$QueryType=="Library") { # Search through pre-calculated scores
      vals$NBLAST$tracings = vals$neurons[input$NBLAST_ChooseID]
      if(length(input$NBLAST_ChooseID)>1){
        scores = colSums(lh_nblast[input$NBLAST_ChooseID,])
        scores.reverse = colSums(lh_nblast[input$NBLAST_ChooseID,])
      }else{
        scores = lh_nblast[input$NBLAST_ChooseID,]
        scores.reverse = lh_nblast[input$NBLAST_ChooseID,]
      }
      if(input$UseMean){
        scores = (scores + scores.reverse) / 2
      }
      names(scores) = colnames(lh_nblast)
      scores = scores[!names(scores)%in%input$NBLAST_ChooseID] # Remove self-match
    }
    scores <- sort(scores, decreasing=TRUE)
    vals$NBLAST$result = scores
    # Get, order and colour the NBLAST matches
    matches = all.neurons[names(vals$NBLAST$result)[1:as.numeric(input$NumHits)]]
    matches[,"colour"] = zissou(length(matches))
    vals$NBLAST$matches = matches
    # Create data frame for selection table
    matchesDF = matches[,]
    matchesDF[,"nblast.score"] = signif(vals$NBLAST$result[1:as.numeric(input$NumHits)],digits = 3)
    matchesDF = matchesDF[,c("id","nblast.score","cell.type","transmitter","skeleton.type","colour")]
    vals$NBLAST$matchesDF = matchesDF
  })

  # Plot the tracing scores result
  output$NBLAST_results_plot <- renderPlot({
    scores <- vals$NBLAST$result # Get the reactive tracing_nblast_scores object
    if(is.null(scores)) {return(NULL)}
    nblast_results <- base::data.frame(scores=scores)
    p <- ggplot2::ggplot(nblast_results, aes(x=scores)) + ggplot2::geom_histogram(binwidth=diff(range(nblast_results$scores))/100) + xlab("NBLAST score") + ylab("Frequency density") + geom_vline(xintercept=0, colour='red')
    p
  })

  # Download the NBLAST results
  output$NBLAST_results_download <- downloadHandler(
    filename = function() {  paste0(input$.tracing_file$name, '_nblast_results_', Sys.Date(), '.csv') },
    content = function(file) {
      scores <- vals$NBLAST$result # the norm isn't correct here, sicne we normalised earlier?
      score_table <- data.frame(neuron=names(scores), raw=scores, norm=scores/nblast(vals$NBLAST$tracings, vals$NBLAST$tracings), type=sapply(names(scores), function(x) paste0(type_for_neuron(x), collapse=", ")))
      colnames(score_table) <- c("Neuron", "Raw NBLAST score", "Normalised NBLAST score", "Type")
      write.csv(score_table, file, row.names=FALSE)
    }
  )

  # Announce when the NBLAST is complete
  output$tracing_nblast_complete <- reactive({
    scores <- vals$NBLAST$result
    return(ifelse(is.null(scores), FALSE, TRUE))
  })
  outputOptions(output, 'tracing_nblast_complete', suspendWhenHidden=FALSE)

  # Plot NBLAST results
  output$NBLAST_View3D <- renderRglwidget({
    rgl::clear3d()
    rgl::plot3d(FCWB)
    query_neurons <- vals$NBLAST$tracings
    query_neurons[,"colour"] <- grDevices::grey.colors(n=length(query_neurons),start=0,end=0.5)
    if(!is.null(query_neurons)) {
      plot3d(query_neurons, col=query_neurons[,"colour"], lwd=3, soma=TRUE)
      scores <- vals$NBLAST$result
      scores <- sort(scores, decreasing=TRUE)
      top.matches <- vals$NBLAST$matches
      if(!is.null(input$NBLAST_SelectionTable_rows_selected)){ # Don't show neurons highlighted in selection table
        top.matches = top.matches[-input$NBLAST_SelectionTable_rows_selected]
      }
      plot3d(top.matches,col = top.matches[,"colour"],lwd = 2, soma=TRUE)
    }
    frontalView()
    rglwidget()
  })

  
  ########################
  # NBLAST Selection Table #
  ########################
  
  # Show neuron selection table with some table-wide buttons above it
  output$NBLAST_MainTable<-renderUI({
    shiny::fluidPage(
      box(width=12,
          column(6,offset = 0,
                 shiny::HTML('<div class="btn-group" role="group" aria-label="Basic example">'),
                 actionButton(inputId = "NBLAST_Del_row_head",label = "delete selected"),
                 actionButton(inputId = "NBLAST_Col_row_head",label = "recolour selected"),
                 shiny::HTML('</div>')
          ),
          shiny::column(width = 12, 
                        DT::dataTableOutput("NBLAST_SelectionTable") 
          ),
          tags$script(shiny::HTML('$(document).on("click", "input", function () {
                           var checkboxes = document.getElementsByName("row_selected");
                           var checkboxesChecked = [];
                           for (var i=0; i<checkboxes.length; i++) {
                           if (checkboxes[i].checked) {
                           checkboxesChecked.push(checkboxes[i].value);
                           }
                           } 
                           Shiny.onInputChange("NBLAST_checked_rows",checkboxesChecked);})'
          )),# This HTML code assigns input$NBLAST_checked_rows, so we know which rows in the table are checked
          tags$script("$(document).on('click', '#NBLAST_SelectionTable button', function () {
                      Shiny.onInputChange('NBLAST_lastClickId',this.id);
                      Shiny.onInputChange('NBLAST_lastClick', Math.random())});"
          ) # When a button in output$NBLAST_SelectionTable is clicked, input$NBLAST_lastClickId gets assigned the id of this button and input$NBLAST_lastClick gets assigned a random value
          # The last click is used to detect the click (for instance when a button is clicked twice, the id won’t change and hence cannot be observed)
      )
      )
    })
  
  # Render neuron selection table
  output$NBLAST_SelectionTable <- DT::renderDataTable({
    DT=vals$NBLAST$matchesDF
    # If there's stuff in the table...
    if(length(DT)>0){
      # Add check boxes to the table
      DT[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(vals$NBLAST$matchesDF),'"><br>')
      # Add two action buttons to the table
      DT[["Actions"]]<-paste0('
                              <div class="btn-group" role="group" aria-label="Basic example">
                              <button type="button" class="btn btn-secondary modify" id=modify_',1:nrow(vals$NBLAST$matchesDF),'>recolour</button> 
                              <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(vals$NBLAST$matchesDF),'><i class="fa fa-trash"></i></button>
                              </div>
                              ')
      DT::datatable(DT,  escape=F, options = list(paging=FALSE),extensions = c('Responsive'), editable = FALSE, rownames= FALSE) %>%
        DT::formatStyle('cell.type', target = "row", backgroundColor = "lightgrey",color = 'black') %>%
        DT::formatStyle('colour', backgroundColor = DT::styleEqual(DT$colour,DT$colour))
      # Using escape = F means that using it, datatable will render the buttons according to their HTML codes instead of strings
    }else{
      NULL
    }
  })
  
  # Delete rows that have been checked
  observeEvent(input$NBLAST_Del_row_head,{
    row_to_del=as.numeric(gsub("Row","",input$NBLAST_checked_rows))
    vals$NBLAST$matchesDF = vals$NBLAST$matchesDF[-row_to_del] # Update dynamic object
    vals$NBLAST$matches = vals$NBLAST$matches[-row_to_del] # Update dynamic object
  })
  
  ##############################
  # Modify NBLAST Colours Table #
  #############################
  
  # Delete rows that have been checked
  observeEvent(input$NBLAST_Col_row_head,{
    showModal(NBLAST_modal_recolor_multiple)
  })
  
  # Observe input$NBLAST_lastClick and then act to delete or modify a row
  observeEvent(input$NBLAST_lastClick,
               {
                 if (input$NBLAST_lastClickId%like%"delete") # Was is the delete button?
                 {
                   row_to_del=as.numeric(gsub("delete_","",input$NBLAST_lastClickId))
                   vals$NBLAST$matchesDF = vals$NBLAST$matchesDF[-row_to_del] # Remove from data table
                   vals$NBLAST$matches = vals$NBLAST$matches[-row_to_del] # Remove from selected neurons
                 }
                 else if (input$NBLAST_lastClickId%like%"modify") # Or was it the modify button?
                 {
                   showModal(NBLAST_modal_modify)
                 }
               }
  )
  
  # Modify colour
  NBLAST_modal_modify<-modalDialog(
    fluidPage(
      h3(shiny::strong("select a colour"),align="center"),
      uiOutput("NBLAST_OldColour"),
      actionButton("NBLAST_ChangeColour","change")
    )
  )
  
  # Modify colours
  NBLAST_modal_recolor_multiple <-modalDialog(
    fluidPage(
      h3(shiny::strong("select a colour"),align="center"),
      uiOutput("NBLAST_RandomStartColour"),
      actionButton("NBLAST_ChangeColourMultiple","change")
    )
  )
  
  # Render old neuron colour and select new one in modal window
  output$NBLAST_OldColour <- renderUI({
    selected_row=as.numeric(gsub("modify_","",input$NBLAST_lastClickId))
    old_row=vals$NBLAST$matchesDF[selected_row,"colour"][[1]]
    if(length(old_row)>1){old_row=united.orange}
    colourInput(inputId = "NBLAST_NewColour",label= NULL,value = old_row)
  })
  
  # Render old neuron colour and select new one in modal window
  output$NBLAST_RandomStartColour <- renderUI({
    colourInput(inputId = "NBLAST_NewColourMultiple",label= NULL,value = sample(darjeeling(100),1))
  })
  
  # Update colour on the data frame and in neurons
  observeEvent(input$NBLAST_ChangeColour,{
    selected_row = as.numeric(gsub("modify_","",input$NBLAST_lastClickId))
    vals$NBLAST$matches[selected_row,"colour"] <- input$NBLAST_NewColour
    vals$NBLAST$matchesDF[selected_row,"colour"] <- input$NBLAST_NewColour
  })
  
  # Update colour on the data frame and in neurons
  observeEvent(input$NBLAST_ChangeColourMultiple,{
    selected_row = as.numeric(gsub("Row","",input$NBLAST_checked_rows))
    vals$NBLAST$matches[selected_row,"colour"] <- input$NBLAST_NewColourMultiple
    vals$NBLAST$matchesDF[selected_row,"colour"] <- input$NBLAST_NewColourMultiple
  })
  
  ###########################
  # Show Maximal Projections #
  ##########################
  
  output$LineCTs <- renderUI({
    cts = as.character(sort(unique(vals$neurons[,"cell.type"]))) # Selected cell types
    cts = cts[cts!="notLHproper"]
    cts_in_lines = unlist(strsplit(unique(cell_type_summary$cell.type),"/")) # All cell type in split-gAL4 lines
    cts_in_lines = cts_in_lines[cts_in_lines!="notLHproper"]
    chosen_in_lines = cts[cts%in%cts_in_lines]  # Selected cell types in lines
    cts_in_lines = cts_in_lines[!cts_in_lines%in%cts]
    cts_choices = list(`linecodes for neurons in selection table`= chosen_in_lines,`linecodes`=cts_in_lines)
    cts_choices = cts_choices[sapply(cts_choices,length)>1] # get rid of empty fields
    selectInput("LineCodeCTs", label = paste0("Cell types in Dolan et al. 2018 split lines (",length(unlist(cts_choices)),") :"), choices = cts_choices, selected = cts_in_lines[1], multiple=FALSE, selectize=TRUE, width = 500)
  })
  
  output$LineCode <- renderUI({
    linecodes = sort(unique(as.character(subset(lh_line_info,grepl(as.character(input$LineCodeCTs),as.character(cell.type)))[,"linecode"])))
    if(!is.null(lines)&length(lines)>0){
      selectInput("LineCode", label = paste0("Split lines with selected cell type (",length(linecodes),") :"), choices = linecodes, selected = linecodes[1], multiple=FALSE, selectize=TRUE,  width = 500)
    }
  })
     
  output$MaximalProjection <- renderImage({
    maxprojection = split_brain_images[grepl(input$LineCode,split_brain_images)]
    return(list(
      src = paste0("www/",maxprojection),
      filetype = "image/jpeg",width = "auto", height = "auto"
    ))
  }, deleteFile = FALSE)
  
  output$VNCMaximalProjection <- renderImage({
    maxprojection = split_vnc_images[grepl(input$LineCode,split_vnc_images)]
    return(list(
      src = paste0("www/",maxprojection),
      filetype = "image/jpeg",width = "auto", height = "auto"
    ))
  }, deleteFile = FALSE)
  
  #########
  # TEST #
  #########
  
  output$Test = renderPrint({
    #str(vals$CATMAID)
    s =  is_lhn_type(input$Type)
    # if(length(s)>0){
    #   s = update_neurons(input=input,db=s)
    # }
    #if (length(s)) {
    cat(s, sep = ', ')
    #}
  })


})
