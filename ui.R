
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

# Load the packages we need for this App
source("init.R")
source("functions.R")

shinyUI(navbarPage("LH library", id="tab", fluid = TRUE,
  
  # Get a nice Shiny theme
  theme = shinythemes::shinytheme("united"),

  #######################
  # Overview of library #
  #######################
  
  shiny::tabPanel("atlas",
            shiny::h3("the lateral horn library",align="left"),
            shiny::br(),
            shiny::HTML("Welcome to the <a href='https://en.wikipedia.org/wiki/Lateral_horn_of_insect_brain'>lateral horn</a> of the <a href='https://en.wikipedia.org/wiki/Drosophila_melanogaster'>vinegar fly</a> brain. In short, the lateral horn is a brain area in the insect that is thought to help generate innate behaviours in response to different odours.
                        Here in our atlas you can browse through all the known cell types that constitute the Drosophilid lateral horn, in 2D here or 3D in the <strong>data viewer </strong>, as introduced by <a href='https://www.biorxiv.org/content/early/2018/06/05/336982'>Frechter et al. 2018</a>.
                        Neurons of the lateral horn are named by a hierarchical classification system. They are each classified into a <strong>primary neurite cluster</strong>, <strong>anatomy group</strong>
                        and finally <strong>cell type</strong>. See the <strong>naming system</strong> tab for details. The <strong>data viewer</strong> tab lets you display these morphologies in 3D, access data on neurons' odour responses and
                        search for specific split-GAL4 lines from <a href='https://www.biorxiv.org/content/early/2018/06/05/336982'>Dolan et al. 2018</a>."),
            shiny::br(),
            shiny::br(),
            shiny::HTML("The data presented on this site originate primarily from five different sources cited in our <strog>about</strong> section.
                        <strong>Split-GAL4</strong> lines found here can be searched and ordered from <a href='http://splitgal4.janelia.org/cgi-bin/splitgal4.cgi'>here</a>."),
            shiny::br(),
            shiny::br(),
            shiny::HTML("Below, you can click on the primary neurite clusters below to see their constituent anatomy groups and cell types. You can also choose to view the split-GAL4 line collection for
                        sparse lateral horn driver lines collected by <a href='https://www.biorxiv.org/content/early/2018/06/05/336982'>Dolan et al. 2018</a>."),
            shiny::br(),
            shiny::hr(),
            shiny::selectInput(inputId='AtlasContent', label='dataset:', choices = c("neuron skeletons","split-GAL4 lines"), selected = "neuron skeletons", multiple=FALSE, selectize=FALSE),
            conditionalPanel(condition="input.AtlasContent =='neuron skeletons'",
                             tabsetPanel(type = "tabs",
                                         tabPanel("lateral horn neurons",
                                                  fluidRow(
                                                    lapply(1:length(pnt_lhns), function(i) {
                                                      column(3,tags$button(
                                                        id = pnt_lhns[i],
                                                        class = "btn action-button",
                                                        tags$img(src = PNT_images[grepl(paste0(pnt_lhns[i],"_"),PNT_images)][1],height = "313px",width="586px")
                                                      ))
                                                    })
                                                  ),
                                                  #tags$head(tags$style(".modal-dialog{ width:923px}")), # Will this affect modals elsewhere in the app?
                                                  #tags$head(tags$style(".modal-body{ height:600px}")),
                                                  tags$head(tags$style(HTML('.modal-lg { width: 923px;'))),
                                                  tags$head(tags$style(HTML('.modal-lg { height: 750px;'))),
                                                  lapply(1:length(pnt_lhns), function(i) {
                                                    shinyBS::bsModal(id = paste0("AG_modal_",pnt_lhns[i]), title = pnt_lhns[i], trigger = pnt_lhns[i], size = "large",
                                                                     slickR::slickROutput(paste0("Carousel",pnt_lhns[i]), width = 879, height = 469),
                                                                     br(),
                                                                     br(),
                                                                     p("These are all the lateral horn neuron ",strong("anatomy groups")," in",pnt_lhns[i]," primary neurite cluster. Below, you can see individual", strong("cell types"),align = "center"),
                                                                     br(),
                                                                     br(),
                                                                     lapply(1:length(CT_images[grepl(paste0(pnt_lhns[i],"[a-z]"),CT_images)]), function(j) {
                                                                       shiny::div(style=paste0("display:§inline-block; position: absolute; left: 10px; top: ",700+((j-1)*500),"px ; z-index: -10000; "),
                                                                                  tags$button(
                                                                                    id = paste0(i,"_",j),
                                                                                    class = "btn action-button",
                                                                                    tags$img(src = CT_images[grepl(paste0(pnt_lhns[i],"[a-z]"),CT_images)][j],height = "469px",width="879px")
                                                                                  )
                                                                       )
                                                                     })
                                                    )
                                                  })
                                                ),
                                         tabPanel("projection neurons",
                                                  fluidRow(
                                                    lapply(1:length(PN_images), function(i) {
                                                      column(3,tags$button(
                                                        id = PN_images[i],
                                                        class = "btn action-button",
                                                        tags$img(src = PN_images[i],height = "313px",width="586px")
                                                      ))
                                                    })
                                                  )
                                                )
                             )
            ),
            conditionalPanel(condition="input.AtlasContent =='split-GAL4 lines'",
                             fluidRow(
                               column(3,
                                 shiny::radioButtons("splittype", label = "type",choices = list("all" = "all", "LH output neurons" = "ON", "LH local neurons" = "LN", "LH input neurons" = "IN"), selected = "all")
                                ),
                               column(3,
                                 shiny::radioButtons("splitNT", label = "transmitter",choices = list("all" = "all", "Acetylcholine" = "ChA", "GABA" = "GABA", "Glutamate" = "Vglut", "Unknown" = "Unknown"), selected = "all")
                               )
                             ),
                            uiOutput("imageGrid")
            ),
            shiny::hr()
  ),

  ###################
  # View LHN library #
  ###################

tabPanel("data viewer",
          includeCSS("errors.css"),
          shinyURL.ui(display=F),
          sidebarLayout(
            sidebarPanel(
               h2("lateral horn library"),
               icon(">>"), # We seem to need this line for the question bubbles to appear, for some reason?
               shiny::HTML("Enter the instinct centre of the vinegar fly, <i>Drosophila melanogaster</i>"),
               hr(),
               selectInput(inputId='SkeletonType', label='dataset:'%>%label.help("lbl_ds"), choices = sort(unique(all.lh.neurons[,"skeleton.type"])), selected = sort(unique(all.lh.neurons[,"skeleton.type"])), multiple=TRUE, selectize=TRUE),
               hr(),
               selectInput(inputId='Type', label='neuron type:'%>%label.help("lbl_nt"), choices = list(`example (FlyCircuit PD2a1)`="example",`LH ouput neurons`= "ON",`LH local neurons`= "LN",`LH input neurons`= "IN", `MBONs`= "MBON"), selected = list(`example (PD2a1)`="example"), multiple=FALSE, selectize=TRUE),
               hr(),
               conditionalPanel(condition="input.Type =='LN'||input.Type =='ON'", # Why does this sometimes work and sometimes not?
                                h5("Select specific cell types"),
                                uiOutput("LHNselection"),
                                hr()
               ),
               conditionalPanel(condition="input.Type =='LN'||input.Type =='ON'",
                  h5("Or search groups of lateral horn neurons"),
                  uiOutput("PNTselection"),
                  uiOutput("AGselection"),
                  uiOutput("CTselection"),
                  hr()
               ),
               conditionalPanel(condition="input.Type =='MBON'",
                                h5("Or search mushroom body output neurons"),
                                uiOutput("MBONselection"),
                                hr()
               ),
               conditionalPanel(condition="input.Type =='IN'",
                                h5("Or search projection neurons"),
                                uiOutput("PNtype"),
                                uiOutput("PNselection"),
                                hr()
               ),
               # Refresh button
               div(style="display:inline-block",actionButton("Append","append selected neurons")),
               div(style="display:inline-block",actionButton("Clear","clear selection")),
               hr(),
               # Choose to show brain sub-volumes
               selectInput(inputId='neuropils', label= 'see neuropils:'%>%label.help("lbl_ns"), choices = c("all neuropils",sort(FCWBNP.surf$RegionList)), selected = "LH_R", multiple=TRUE, selectize=TRUE),
               # Choose which primary neurite tractsw to plot
               strong("see primary neurite tracts: "%>%label.help("lbl_pnts")),
               br(),
               bootstrapPage(
                 lapply(1:length(pnt_lhns), function(i){
                   div(style="display:inline-block",checkboxInput(inputId=paste0("PNT",i), value=NULL, label = pnt_lhns[i]))
                 })
                ),
               shiny::br(),
               checkboxInput(inputId="BrainMesh", label = "see brain mesh"%>%label.help("lbl_bm"),value=TRUE),
               shiny::hr(),
               # Help text
               bsTooltip(id = "lbl_nt", title = "the broadest category for LH neurons", placement = "right", trigger = "hover"),
               bsTooltip(id = "lbl_pnt", title = "tracts connect soma to neuropil", placement = "right", trigger = "hover"),
               bsTooltip(id = "lbl_ns", title = "neuropils as defined in Ito et al. 2014", placement = "right", trigger = "hover"),
               bsTooltip(id = "lbl_ds", title = "the dataset in which to search for neuron skeletons", placement = "right", trigger = "hover"),
               bsTooltip(id = "lbl_bm", title = "FCWB brain mesh made from FlyCircuit data", placement = "right", trigger = "hover"),
               actionButton("Upload","upload tracing / CATMAID pull"),
               shiny::hr(),
               actionButton("DownloadAll","download all data")
             ),
        # Show a plot of the brain
        mainPanel(
          includeCSS("loader.css"), # Load spinny waiting wheel
          shiny::HTML("<div class='loader' style='position: absolute; left: 400px; top: 320px; z-index: -10000;'>Loading...</div>"),
          shiny::HTML("<div style='position: absolute; left: 220px; top: 270px; z-index: -10000; text-align: center; width: 400px; font-size: 30px;'>Loading...</div>"),
          # Output: Tabset
          tabsetPanel(type = "tabs",
                      tabPanel("3D",
                              rgl::rglwidgetOutput("plot3D", width="1200px", height="700px"),
                              uiOutput("MainTable")
                      ),
                      tabPanel("odour responses",
                                 br(),
                                 uiOutput("ChooseCTs"),
                                 checkboxInput(inputId="CTmean", value=TRUE, label ="use cell type means"),
                                 hr(),
                                 plotly::plotlyOutput("Ephys"),
                                 shiny::br(),
                                 shiny::HTML("<i>smoothed number of spikes in the 500 ms window after odour stimulation period shown. 
                                             See <a href='https://www.biorxiv.org/content/early/2018/06/05/336982'>Frechter et al. 2018</a> for details.</i>")
                      ),
                      tabPanel("odour search",
                               br(),
                               uiOutput("ChooseOdours"),
                               shiny::bootstrapPage(
                                 div(style="display:inline-block",checkboxInput(inputId="OdourMean", value= FALSE, label ="use mean of chosen odours")),
                                 div(style="display:inline-block",checkboxInput(inputId="OdourCTMean", value= TRUE, label ="use mean of cell types"))
                               ),
                               hr(),
                               plotly::plotlyOutput("OdoursResponses"),
                               shiny::br(),
                               shiny::HTML("<i>smoothed number of spikes in the 500 ms window after odour stimulation period shown. 
                                             See <a href='https://www.biorxiv.org/content/early/2018/06/05/336982'>Frechter et al. 2018</a> for details.</i>")
                      ),
                      tabPanel("split-GAL4 lines",
                               br(),
                               uiOutput("LineCTs"),
                               uiOutput("LineCode"),
                               shiny::hr(),
                               tabsetPanel(type = "tabs",
                                           tabPanel("brain", imageOutput("MaximalProjection")),
                                           tabPanel("VNC", imageOutput("VNCMaximalProjection"))
                               )
                      ),
                      tabPanel("uniglomerular PN info",
                          shiny::br(),
                          shiny::tableOutput('PNINFO'),
                          tags$head(tags$style("#PNINFO table {background-color: white; }", media="screen", type="text/css")),
                          shiny::HTML("<i>Information in table primarily collated by Paavo Huoviala and Marta Costa</i>"),
                          shiny::br()
                      ),
                      tabPanel("uniglomerular PN responses",
                               shiny::br(),
                               plotly::plotlyOutput("PNCalicumResponses", width = "1000px", height = "1000px"),
                               shiny::br(),
                               shiny::HTML("<i>Data from a Ca2+ imaging study of PN dendrites in the line <strong>NP225-Gal4</strong> <a href='https://www.ncbi.nlm.nih.gov/pubmed/27321924'>(Badel al. 2017)</a>.</i>")
                      )
                      # tabPanel("predicted connectivity",
                      #          plotly::plotlyOutput("Overlap", width = "100%", height = "1000px"),
                      #          shiny::br(),
                      #          shiny::HTML("<i>Predicted connecvtivity is based on an overlap score between PN axons and LH dendrite (see Methods in <a href='https://www.biorxiv.org/content/early/2018/06/05/336982'>Frechter et al. 2018</a>). This matrix has been averaged across cell types and 
                      #                      normalised so that 1 prepresents a likely strong connection and 0 represents no chance for connectivity.</i>")
                      # )
                  )
          )
       )
    ),
 
##################
# NBLast neurons #
##################

 tabPanel("NBLAST",
          sidebarLayout(
            sidebarPanel(
              shiny::h2("NBLAST against the library"),
              shiny::HTML("Want to know what cell type your neuron belongs to? Or find a genetic line for it?
                   Choose a neuron from our library or a neuron you have uploaded and <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4961245/'>NBLAST</a> it against our library.
                   If the checkbox below is ticked, both forwards and reverse scores will be calculated, normalised and averaged,
                   rather than just using the forwards score. The query neuron will be <b><span style='color: black;'>plotted in black</span></b>
                   in the 3D viewer to the right, alongside the top 10 hits (rainbow coloured from <span style='color: #F21A00;'>red = best</span> to <span style='color: #3B9AB2;'>cyan = worst</span>)."),
              shiny::HTML("See what the scores mean <a href='http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/www/how/'>here</a>"),
              shiny::br(),
              shiny::br(),
              strong("To NBLAST neurons they must be in the neuron selection table, in the data viewer tab"),
              shiny::br(),
              shiny::br(),
              selectInput(inputId='QueryType', label='query type:'%>%label.help("lbl_qt"), choices = list(`LH library neuron(s)` = "Library",`uploaded neuron(s)` = "UserUpload"), selected = list(`LH library neuron(s)` = "Library"), multiple=FALSE, selectize=TRUE),
              uiOutput("ChooseUploadedSkeletons"),
              uiOutput("NBLAST_SkeletonType"),
              uiOutput("NBLAST_ChooseFromLibrary"),
              uiOutput("NBLAST_ChooseID"),
              shiny::HTML("<i>If multiple neurons are chosen, NBLAST scores will be averaged across these neurons. I.e. they will be treated as one amalgamated neuron</i><br /><br />"),
              sliderInput(inputId = "NumHits",label = "no. hits to visualise:", 1, 100, 10, 1),
              checkboxInput("UseMean", label="Use mean scores", value= TRUE),
              shiny::HTML("<i>Using the mean score is useful for finding exact matches, i.e. one in which the target is a good hit for the query and the query is a good hit for the target too.
                   This is particularly useful for clustering neurons into types, rather than,
                   for example, just finding neurons that go through the same tract but branch off differently.</i><br /><br />"),
              actionButton("NBLASTGO","NBLAST")
              ),
          mainPanel(
            h2("3D view"),
            includeCSS("loader.css"),
            shiny::HTML("<div class='loader' style='position: absolute; left: 400px; top: 300px; z-index: -10000;'>Loading...</div>"),
            shiny::HTML("<div style='position: absolute; left: 220px; top: 270px; z-index: -10000; text-align: center; width: 400px; font-size: 30px;'>Loading...</div>"),
            rglwidgetOutput("NBLAST_View3D", width="1200px", height="700px"),
            conditionalPanel(condition = "output.tracing_nblast_complete",
                             h3("Score distribution"),
                             plotOutput("NBLAST_results_plot"),
                             h2("NBLAST results"),
                             uiOutput("NBLAST_MainTable"),
                             br(),
                             downloadButton('NBLAST_results_download', 'Download all scores as CSV')
            )
          )
       )
     ),

###################
# LH Naming System #
###################

tabPanel("naming system",
        shiny::fluidPage(
          shiny::h3("a hierarchical classification system for neurons of the lateral horn", align = "left"),
          shiny::br(),
          shiny::br(),
          shiny::HTML("In order to talk about something, you need to name it. Distilling the brain into its constituent cell types is an important step in not only reducing the dimensionality
                      of the brain's circuitry into more understandable units but also communicating dicoveries in circuit neuroscience efficiently and <a href='https://en.wikipedia.org/wiki/Celestial_Emporium_of_Benevolent_Knowledge'>comprehensibly</a>, enabling us to identify the same units across studies.
                      The lateral horn is a complex neuropil consisting of more than 1300 neurons with diverse morphologies but lacking anatomical landmarks by which to categorise neurons, 
                      such as the glomeruli of the antennal lobe and the compartments of the mushrooom body. An <a href='https://www.ncbi.nlm.nih.gov/pubmed/28334573'>elegant definition</a> of cell type comprises something about a neuron's origins, structure and function."),
          shiny::br(),
          shiny::br(),
          shiny::HTML("Neurons with dendrite in the lateral horn are named by a hierarchical classification system introduced in <a href='https://www.biorxiv.org/content/early/2018/06/05/336982'>Frechter et al. 2018</a>.
                      This system uses three different features of increasing neuroanatomical detail to categorise neurons: <strong>primary neurite cluster</strong>, <strong>anatomy group</strong>
                      and finally <strong>cell type</strong>. Unlike with mammalian neurons, insect neurons' somata exist outside of the neuropil (the brain mesh of axons and dendrites).
                      The primary neurite tract is the region of the neuron separating the soma from the rest of the cell’s axons and dendrites. In the insect brain, 
                      neurons of the same cell type always enter the neuropil via the same primary neurite tract. We chose primary neurite tract as the highest order discriminating factor because each neuron 
                      has just one soma and primary neurite tract and because it groups functionally related neurons e.g. those with common neurotransmitters or similar axonal projections.
                      "),
          shiny::br(),
          shiny::br(),
          shiny::fluidRow(
            column(6,
                shiny::HTML("To classify a neuron into a cell type, we identify the: <strong>(1)</strong> primary neurite tract: the tract connecting the soma to the rest of the neuron, Frechter et al. identified 31 primary neurite tracts, named by their relationship with the LH, e.g. Posterior Ventral tract five or PV5.
                      Next, we use a neuron morphological similarity algorithm NBlAST <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4961245/'>(Costa et al. 2016)</a> <strong>(2)</strong> anatomy group: neurons of the same anatomy group share a common axon tract and have broadly similar arborisations in the lateral horn and their target areas, and
                      <strong>(3)</strong> cell type, this is the finest level – the only difference between cell types of the same anatomy group are reproducible differences in axonal or dendritic arborisation patterns that likely reflect specific differences in connectivity. 
                      These anatomical cell types are likely the functional units of the lateral horn, and neurons of a type have been shown to respond similarly <a href='https://www.biorxiv.org/content/early/2018/06/05/336982'>(Frechter et al. 2018)</a> and
                      morphological similar neurons exhibit similar - though not exactly the same - structural <a href='https://www.biorxiv.org/content/early/2017/08/29/167312'>(Dolan, Ghislain Belliart-Guerin et al. 2018)</a> and functional connectivity 
                      <a href='https://www.ncbi.nlm.nih.gov/pubmed/29909998'>(Fişek et al. 2014;</a> <a href='https://www.ncbi.nlm.nih.gov/pubmed/29909998'>Jeanne, Fişek et al. 2018)</a>. 
                      As such our definition of cell type likely reflects both the developmental origin of the neuron in question, its connectivity and its current function."),
                shiny::br(),
                shiny::br(),
                shiny::HTML("To the side is a summary schematic for the nomenclature system used to define lateral horn anatomy groups and cell types. It illustrates the hierarchical steps of our naming scheme using three PV5 lateral horn output neurons as an example.
                      First the primary neurite tract was identified (left panel, in this case PV5). This is the region of the neuron that connects the soma to axon and dendrite. Next the coarse zones of projection are determined and used to classify the anatomy group
                      (middle panel, PV5b projects to area 1 while PV5a projects to area 2). This can often be done with registered images of full GAL4 or split-GAL4 expression patterns. Finally, fine anatomical differences are determined (right panel, both PV5b1 and PV5b2 project to Area 1 while PV5a1 projects to area 1)
                      This often necessitates morphological clustering using single cell data."),
                shiny::br(),
                shiny::br(),
                shiny::HTML("We have also classified projection neurons to the lateral horn i.e. the major inputs to lateral horn neurons.
                            We divided LH inputs into functional categories based on the sensory modality inferred from their dendritic neuropil and named them by
                            extending the naming system of <a href='https://www.ncbi.nlm.nih.gov/pubmed/22592945'>Tanaka et al. (2012)</a>."),
                shiny::br(),
                shiny::br(),
                shiny::HTML("Below are the results from our attempts to find and quantify the number of neurons in the major LH-bound primary neurite tracts - which had originally been defined
                            at the level of light microscopy - using a recent whole brain dataset for the adult female fly brain gathered by <a href='https://en.wikipedia.org/wiki/Transmission_electron_microscopy'>electron microscopy</a> <a href='https://www.ncbi.nlm.nih.gov/pubmed/30033368'>(Zheng et al. 2018)</a>.
                           (Names with an X indicate 'new' tracts that were in the electron micrographs and found to run approximately parallel with tracts we had identified at light-level, and so in terms of nomenclature neurons therein have stayed as part of the same primary neuritr cluster.)")
                ),
            column(6,
                   img(src='LHN_naming_cartoon.png', width="738px", height="621px", align = "center")
                   )
          ),
          shiny::br(),
          shiny::br(),
          shiny::tableOutput('TRACTS'),
          shiny::br(),
          shiny::br(),
          shiny::HTML("<i>Text based on Frechter et al. 2018 and Dolan et al. 2018</i>"), 
          shiny::hr()
        )
),

####################
# The lateral horn #
####################

tabPanel("the lateral horn",
         shiny::fluidPage(
           shiny::div(img(src='fly_olfactory_system.png', width="1074px", height="907px"),align="center"),
           shiny::br(),
           shiny::br(),
           shiny::br(),
           shiny::br(),
           shiny::h3("about the lateral horn of Drosophila melanogaster", align = "left"),
           shiny::br(),
           shiny::br(),
           shiny::fluidRow(
             column(5,
                    shiny::HTML("<i>The lateral horn of the fly is thought to be an insinct processing center, the <a href='http://flybrain.mrc-lmb.cam.ac.uk/jefferislabwebsite/'>Jefferis group</a> are interested in
                           the statistics of connectivity and convergence between its constituent neurons and how it helps mount
                           behavioural responses to innate, potentially multi-modal stimuli in the face of the fly's ever changing environment.</i>"),
                    shiny::br(),
                    shiny::br(),
                    shiny::HTML("Animal behaviour emerges from structure, wiring biases and activity in the nervous
                                system. These structures and biases can broadly be thought of as having emerged from
                                random, genetically determined and learned processes. This means that valences are
                                ascribed to stimuli in both an experience-independent, and an experience-dependent
                                manner. Coarse neuroanatomical work has generally shown that nervous systems
                                parallelise sensory processing between neural circuits for innate and learned behaviour, 
                                although the view that these streams are completely separate has been challenged <a href='https://www.biorxiv.org/content/early/2017/08/29/167312'>(Dolan, Ghislain Belliart-Guerin et al. 2018)</a>
                                and at some point these two streams must be integrated <a href='https://www.ncbi.nlm.nih.gov/pubmed/27871975'>(Schultzhaus et al. 2017)</a>."),
                    shiny::br(),
                    shiny::br(),
                    shiny::HTML("This is true of the olfactory system. Olfactory processing has been of interest to those studying neural circuits since the
                                inception of the field. This is because olfaction is not only a shallow sensory system, in
                                contrast to vision, but also provides a means by which to elicit a complex array of
                                behaviours with a wide palette of stimuli, in which even subtle changes in chemical
                                composition can generate very different behavioural effects. This has raised interesting
                                questions concerning olfactory perception and discrimination, but has also made the
                                circuit level study of instinct and memory more tractable."),
                    shiny::br(),
                    shiny::br(),
                    shiny::HTML("Owing to the relative simplicity of their neural circuits, the olfactory systems of insects
                                have been well exploited in order to elucidate general principles of sensory processing.
                                Olfactory receptor neurons in peripheral organs (i.e. the antennae and maxillary palps)
                                of the arthropod provide (<strong>ORNs</strong>) an interface between the central nervous system and the
                                environment. Volatile molecules engage seven-transmembrane olfactory receptors on
                                these olfactory receptor neurons, each olfactory receptor neuron expressing on average
                                one of fifty olfactory receptors in the case of the most well studied system, that of the
                                adult fruit fly <i>Drosophila melanogaster</i> <a href='https://www.ncbi.nlm.nih.gov/pubmed/16139208'>(Couto et al. 2005;</a> <a href='https://www.ncbi.nlm.nih.gov/pubmed/16332533'>Fishilevich et al. 2005)</a>.
                                olfactory receptor neurons project into the antennal lobe in the central brain, where they
                                form a highly stereotyped glomerular olfactory map. Second order projection neurons
                                (<strong>PNs</strong>), that are either uniglomerular (uniglomerular PNs), or sample multiple glomeruli
                                as well as areas of gustatory information input (multiglomerular PNs), proceed to largely
                                target just two neuroanatomical areas, the Kenyon cells (<strong>KCs</strong>) of the mushroom body (<strong>MB</strong>) and the
                                lateral horn (<strong>LH</strong>) of the protocerebrum <a href='https://www.ncbi.nlm.nih.gov/pubmed/17382886'>(Jefferis et al. 2007;</a> <a href='https://www.ncbi.nlm.nih.gov/pubmed/12007410/'>Marin et al. 2002;</a> 
                                <a href='https://www.ncbi.nlm.nih.gov/pubmed/12007409'>Wong et al. 2002;</a> <a href='https://www.ncbi.nlm.nih.gov/pubmed/22592945'>Tanaka et al. 2012)</a>. It has been suggested that these two sites, the LH and the mushroom body, represent
                                parallel pathways for not only olfactory, but gustatory, visual, mechanosensory, and
                                thermosensory processing <a href='https://www.biorxiv.org/content/early/2018/06/05/336982'>(Frechter et al. 2018)</a>. Ablation of the mushroom body yields a deficit in olfactory
                                learning but not in innate responses to olfactory stimuli <a href='https://www.ncbi.nlm.nih.gov/pubmed/8303280'>(de Belle and Heisenberg,
                                1994;</a> <a href='https://www.ncbi.nlm.nih.gov/pubmed/12210097'>Kido and Ito, 2002)</a>, and transgenic expression of tetanus toxin in PNs that
                                directly target the mushroom body and directly and indirectly target the LH produces a
                                deficit in innate courtship behaviour <a href='https://www.ncbi.nlm.nih.gov/m/pubmed/11742061/'>(Heimbeck et al. 2001)</a>."),
                    shiny::br(),
                    shiny::br(),
                    shiny::HTML("Despite receiving similar projections from the antennal lobe, the MB and the LH exhibit very different connectivity rules. 
                                In the MB, sparse, random connectivity with KCs <a href='https://www.ncbi.nlm.nih.gov/pubmed/23615618'>(Caron et al. 2013;</a> 
                                <a href='https://www.ncbi.nlm.nih.gov/pubmed/28796202'>Eichler et al. 2017;</a> <a href='https://www.ncbi.nlm.nih.gov/pubmed/24141312'>Gruntman and Turner 2013)</a>, with each KC receiving pooling an average of 5 PNs, each of which connecting to many other KCs <a href='https://www.ncbi.nlm.nih.gov/pubmed/23615618'>(Caron et al. 2013)</a>.
                                The KCs themselves only consist of ~2000 individual cells <a href='https://www.ncbi.nlm.nih.gov/pubmed/19706282'>(Masse et al. 2009)</a> split into only three canonical cell types (gamma, alpha'/beta', alpha/beta) synapse upon only ~22 mushroom body output neuron (<strong>MBON</strong>) cell types consisting of just ~35 individual cells
                                <a href='https://www.ncbi.nlm.nih.gov/pubmed/25535793'(Aso et al.2014;</a> <a href='https://www.ncbi.nlm.nih.gov/pubmed/28718765'>Takemura et al. 2017)</a>. 
                                It is thought that memories are stored in the KC->MBON connection weight which can be depressed by dopaminergic inputs. 
                                Combined with local inhibition <a href='https://www.ncbi.nlm.nih.gov/pubmed/24561998'>(Lin et al. 2014)</a>, this set up means that memories have a sparse KC representation <a href='https://www.ncbi.nlm.nih.gov/pubmed/12130775'>(Perez-Orive et al. 2002)</a> which helps to minimise conflict between memory representations.
                                In stark contrast, it seems that LH neurons exhibit stereotyped functional connectivity across animals <a href='https://www.ncbi.nlm.nih.gov/pubmed/29909998'>(Fişek et al. 2014;</a> <a href='https://www.ncbi.nlm.nih.gov/pubmed/29909998'>Jeanne, Fişek et al. 2018)</a>
                                and structural connectivity within a cell type in the same animal <a href='https://www.biorxiv.org/content/early/2017/08/29/167312'>(Dolan, Ghislain Belliart-Guerin et al. 2018)</a>. LH neuron responses to dours can be explained as a linear summation
                                of their PN inputs <a href='https://www.ncbi.nlm.nih.gov/pubmed/29909998'>(Fişek et al. 2014)</a> and neurons of similar morphology that share a primary neurite tract (see <strong>naming</strong> tab) respond similarly <a href='https://www.biorxiv.org/content/early/2018/06/05/336982'>(Frechter et al. 2018)</a>. 
                                In contract to KCs they comprise likely more than ~1390 neurons dividing into ~250 cell types of which ~150 have most of their dendrite within the LH ('core LH' neurons) <a href='https://www.biorxiv.org/content/early/2018/06/05/336982'>(Frechter et al. 2018)</a>.
                                This means that the actual number of KCs and LH neurons is similar, but the diversity of LH neurons is far greater than their MB counterparts, including MBONs. 
                                In other words, olfactory second->third order neuron divergence in the MB is ~1:15 in terms of cell number, but leads to a 9:1 reduction in dimensionality in terms of MBON putput. 
                                In the LH, divergence is ~1:9 in terms of cell numbers but ~1:1 in terms of core LH output cell types."),
                    shiny::br(),
                    shiny::br(),
                    shiny::HTML("Part of the reason why the LH is relatively ill-understood as compared with the
                                mushroom body is that its anatomical structure is less clear, and has proven less
                                targetable by genetic techniques than the more distinctive mushroom bodies. 
                                To this end, we have produced a set of genetic reagents to target the lateral horn  <a href='https://www.biorxiv.org/content/early/2018/06/05/336982'>(Dolan et al. 2018)</a>.
                                However, in order to resolve outstanding questions about its function, its structure in terms of
                                circuit logic needs to be fully understood, and this will require high-resolution, synaptic level reconstruction of LH circuitry.
                                The large number of KCs enables sparse odor coding, which is proposed to avoid synaptic interference during memory formation.
                                Why should the lateral horn also have such a large number of neurons and cell types?"),
                    shiny::br(),
                    shiny::br(),
                    shiny::HTML("<i>Diagram by Philipp Schlegel</i>")
                    ),
             column(7,
                    shiny::div(shiny::img(src='neuroanatomy.png', width="738px", height="434px"),align="center"),
                    shiny::br(),
                    shiny::br(),
                    shiny::h4("a lateral horn output neuron - PV5a1"),
                    shiny::div(shiny::img(src='LHON_3A_PV5a.png', width="412px", height="295px"),align="center"),
                    shiny::br(),
                    shiny::br(),
                    shiny::h4("a lateral horn local neuron - PV5a1"),
                    shiny::div(shiny::img(src='LHLN_1D_AV4a1.png', width="308px", height="267px"),align="center"),
                    shiny::br(),
                    shiny::br(),
                    shiny::h4("a lateral horn input neuron - WED-PN1"),
                    shiny::div(shiny::img(src='LHPN_70C_WED-PN1.png', width="504px", height="621px"),align="center"),
                    shiny::br(),
                    shiny::br(),
                    shiny::HTML("<i>Black, DA1 PN. Coloured neuron are reconstructions from electron microscopy, purple - primary neurite tract,
                                blue - dendrite, orange - axon, green - intervening cable, cyan - input synapses, red - output synapses.</i>")
             )
                    ),
           shiny::hr(),
           shiny::HTML("") 
      )
 ),


#########
# About #
#########

 tabPanel("about",
          h3("Purpose"),
          shiny::HTML("Olfactory information in <i>Drosophila melanogaster</i> may be processed by as few as three synapses before engaging motor programmes. Previous research has largely focused on the more superficial components of this shallow system.
                In the the <a href='http://flybrain.mrc-lmb.cam.ac.uk/jefferislabwebsite/'> Jefferis group</a> at the <a href='https://www2.mrc-lmb.cam.ac.uk/'>MRC LMB</a> in Cambridge, UK, we aim to describe the internal circuitry of the lateral horn (LH), the insect analogue of the mammalian cortical amygdala.
                This web app accompanies two publications, <a href='https://www.biorxiv.org/content/early/2018/06/05/336982'>Frechter et al. 2018</a> and <a href='https://www.biorxiv.org/content/early/2018/06/05/336982'>Dolan et al. 2018</a>,
                and aims to bring together datasets that enrich our knowledge of cell types of the lateral horn"
                      ),
          h3("R tools"),
          shiny::HTML("We have developed a <a href='https://github.com/jefferislab'>suite of tools in R</a> to enable users to work with morphological skeleton data for neurons. 
                      Video demos showing how to use NBLAST and other related resources are available <a href='http://jefferislab.org/si/nblast/www/demos/'>here</a>. 
                      You can also use NBLAST with data from <a href='http://www.flycircuit.tw/'>FlyCircuit</a> without needed to use R, through another R Shiny app, <a href='http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/www/nblast_online/'>NBLAST-on-the-fly</a> by James Manton."),
          shiny::br(),
          shiny::div(shiny::HTML('<iframe width="420px" height="345px" src="https://www.youtube.com/watch?v=tyg6kbixuaM" frameborder="0" allowfullscreen></iframe>'),align="center"),
          h3("Source code"),
          shiny::HTML("The full code for this web app can be downloaded from <a href='https://github.com/jefferislab'>GitHub</a>."),
          h3("Preparing own data"),
          shiny::HTML("Protocols for <a href='http://cshprotocols.cshlp.org/content/2013/4/pdb.prot071720.full'>immunostaining and imaging fly brains</a>, as well as <a href='http://cshprotocols.cshlp.org/content/2013/4/pdb.prot071738.full'>registration of the resulting images</a> are available from Cold Spring Harbor Protocols.
                      We recommend the use of <a href='http://fiji.sc/Simple_Neurite_Tracer'>Simple Neurite Tracer</a> for tracing neurons from the acquired images, detailed instructions for which are available from <a href='http://fiji.sc/Simple_Neurite_Tracer:_Step-By-Step_Instructions'>here</a>.
                      In order to see single cell morphologies in genetic lines that label multiple neurons per brain, <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4460454/'>MultiColor FlpOut (MCFO)</a> is recommended."),
          h3("Split-GAL4 lines"),
          shiny::HTML("Split-GAL4 lines found here can be searched and ordered from <a href='http://splitgal4.janelia.org/cgi-bin/splitgal4.cgi'>here</a>"),
          h3("Data sources"),
          shiny::br(),          
          shiny::HTML("<strong>1.</strong> Frechter, S., Bates, A.S., Tootoonian, S., Dolan, M.-J., Manton, J.D., Jamasb, A., Kohl, J., Bock, D., and Jefferis, G.S.X.E. (2018). Functional and Anatomical Specificity in a Higher Olfactory Centre. Biorxiv.
                      <i>[dye fills for LH morphologies and odour response data from whole-cell patch recordings of single, identified LH neurons]</i>"),
          shiny::br(),
          shiny::br(),
          shiny::HTML("<strong>2.</strong> Dolan, M-J, Frechter, S., Bates, A.S., Dan, C., Huoviala, P., Roberts, R.J.V., Schlegel, P., Dhawan, S., Tabano, R., Dionne, H., Christoforou, C., Close, K., Sutcliffe, B.,  Giuliani, B., Feng, L., Costa, M., Ihrke, G., Meissner, G., Bock, D., Aso, Y., Rubin, G.M. and Jefferis, G.S.X.E. (2018). Functional and Anatomical Specificity in a Higher Olfactory Centre. Biorxiv.
                      <i>[split-GAL4 lines for lateral horn neurons, cell type segmentations from confical stack data and single cell <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4460454/'>MCFO</a> skeletons]</i>"),
          shiny::br(),
          shiny::br(),
          shiny::HTML("<strong>3.</strong> Jeanne, J.M., Fişek, M., and Wilson, R.I. (2018). The Organization of Projections from Olfactory Glomeruli onto Higher-Order Neurons. Neuron.
                      <i>[some dye fill morphologies, paper reports functional olfactory PN->LH neuron connectivity]</i>"),
          shiny::br(),
          shiny::br(),          
          shiny::HTML("<strong>4.</strong> Chiang, A.-S., Lin, C.-Y., Chuang, C.-C., Chang, H.-M., Hsieh, C.-H., Yeh, C.-W., Shih, C.-T., Wu, J.-J., Wang, G.-T., Chen, Y.-C., et al. (2011). Three-dimensional reconstruction of brain-wide wiring networks in Drosophila at single-cell resolution. Curr. Biol.
                      <i>[over 16,000 single cell morphologies from <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4460454/'>MCFO</a>, increased number skeletons available from <a href='http://www.flycircuit.tw/'>FlyCircuit</a>]</i>"),
          
          shiny::br(),
          shiny::br(),
          shiny::HTML("<strong>5.</strong> Badel, L., Ohta, K., Tsuchimoto, Y., and Kazama, H. (2016). Decoding of context-dependent olfactory behavior in Drosophila. Neuron.<i>
                      [Calcium imaging of PN dendrites in response to odours</i>"),
          
          shiny::br(),
          shiny::br(),
          shiny::HTML("<i>Note: You can cite this Web app by citing Frechter et al. and in addition the relevant publications from which the data you wish to mention originate, e.g. Chiang et al. 2011 for FlyCircuit skeletons.</i>"),
          h3("Contact us"),
          shiny::HTML("If you require more information about this work, please contact <a href='https://www.linkedin.com/in/alex-bates-22a265a7/'>Alex Bates</a> at <strong>ab2248[at]cam.ac.uk</strong> or <a href='https://www2.mrc-lmb.cam.ac.uk/group-leaders/h-to-m/gregory-jefferis/'>Gregory Jefferis</a> at <strong>jefferis[at]mrc-lmb.cam.ac.uk</strong>"
          ),
          h3("Acknowledgements"),
          shiny::HTML("This Shiny App was built by Alex Bates, in part based on code by <a href='http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/www/nblast_online/'>James Manton</a>.
                      It relies on light-level data collected by Gregory Jefferis, Shahar Frechter, Michael-John Dolan (along with the FlyLight team at Janelia Research Campus), Ann-Shyn Chiang's group, Mehmet Fisek and Jamie Jeanne and collated by Alex Bates, 
                      and EM data reconstructed primarily by Alex Bates, Ruairi Roberts, Philipp Schlegel and Gregory Jefferis using a nanoscale resolution dataset for a single adult female fly brain from the Bock group at Janelia Research Campus <a href='https://www.ncbi.nlm.nih.gov/pubmed/30033368'>(Zheng et al. 2018)</a>."
          ),
          shiny::hr()
        )
  )
)
