
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

# Load the packages we need for this App
source("packages.R")
source("functions.R")

shinyUI(navbarPage("LH Library", id="tab", fluid = TRUE,
  
  # Get a nice Shiny theme
  theme = shinythemes::shinytheme("united"),

  ###################
  # View LHN library #
  ###################
  
tabPanel("Explore",
          includeCSS("errors.css"),
          shinyURL.ui(display=F),
          sidebarLayout(
            sidebarPanel(
               h2("Lateral Horn Library"),
               icon(">>"), # We seem to need this line for the question bubbles to appear, for some reason?
               HTML("Enter the instinct centre of the vinegar fly, <i>Drosophila melanogaster</i>"),
               hr(),
               selectInput(inputId='SkeletonType', label='dataset:'%>%label.help("lbl_ds"), choices = sort(unique(all.neurons[,"skeleton.type"])), selected = "FlyCircuit", multiple=TRUE, selectize=TRUE),
               hr(),
               selectInput(inputId='Type', label='neuron type:'%>%label.help("lbl_nt"), choices = list(`example (pd2a1)`="example",`LH ouput neurons`= "ON",`LH local neurons`= "LN",`LH input neurons`= "IN", `MBONs`= "MBON"), selected = list(`example (pd2a1)`="example"), multiple=FALSE, selectize=TRUE),
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
               selectInput(inputId='neuropils', label= 'see neuropils:'%>%label.help("lbl_ns"), choices = c("all neuropils",sort(FCWBNP.surf$RegionList)), selected = "LH_R", multiple=TRUE, selectize=TRUE),
               # Choose which primary neurite tractsw to plot
               strong("see primary neurite tracts: "%>%label.help("lbl_pnts")),
               br(),
               bootstrapPage(
                          div(style="display:inline-block",checkboxInput(inputId="PNT1", value=NULL, label = all.lh.pnts[1])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT2", value=NULL, label = all.lh.pnts[2])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT3", value=NULL, label = all.lh.pnts[3])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT4", value=NULL, label = all.lh.pnts[4])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT5", value=NULL, label = all.lh.pnts[5])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT6", value=NULL, label = all.lh.pnts[6])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT7", value=NULL, label = all.lh.pnts[7])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT8", value=NULL, label = all.lh.pnts[8])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT9", value=NULL, label = all.lh.pnts[9])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT10", value=NULL, label = all.lh.pnts[10])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT11", value=NULL, label = all.lh.pnts[11])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT12", value=NULL, label = all.lh.pnts[12])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT13", value=NULL, label = all.lh.pnts[13])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT14", value=NULL, label = all.lh.pnts[14])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT15", value=NULL, label = all.lh.pnts[15])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT16", value=NULL, label = all.lh.pnts[16])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT17", value=NULL, label = all.lh.pnts[17])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT18", value=NULL, label = all.lh.pnts[18])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT19", value=NULL, label = all.lh.pnts[18])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT20", value=NULL, label = all.lh.pnts[20])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT21", value=NULL, label = all.lh.pnts[21])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT22", value=NULL, label = all.lh.pnts[22])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT23", value=NULL, label = all.lh.pnts[23])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT24", value=NULL, label = all.lh.pnts[24])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT25", value=NULL, label = all.lh.pnts[25])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT26", value=NULL, label = all.lh.pnts[26])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT27", value=NULL, label = all.lh.pnts[27])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT28", value=NULL, label = all.lh.pnts[28])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT29", value=NULL, label = all.lh.pnts[29])),
                          div(style="display:inline-block",checkboxInput(inputId="PNT30", value=NULL, label = all.lh.pnts[30]))
                ),
               br(),
               checkboxInput(inputId="BrainMesh", label = "see brain mesh"%>%label.help("lbl_bm"),value=TRUE),
               hr(),
               # Refresh button
               actionButton("Append","append selected neurons"),
               hr(),
               # Help text
               bsTooltip(id = "lbl_nt", title = "the broadest category for LH neurons", placement = "right", trigger = "hover"),
               bsTooltip(id = "lbl_pnt", title = "tracts connect soma to neuropil", placement = "right", trigger = "hover"),
               bsTooltip(id = "lbl_ns", title = "neuropils as defined in Ito et al. 2014", placement = "right", trigger = "hover"),
               bsTooltip(id = "lbl_ds", title = "the dataset in which to search for neuron skeletons", placement = "right", trigger = "hover"),
               bsTooltip(id = "lbl_bm", title = "FCWB brain mesh made from FlyCircuit data", placement = "right", trigger = "hover"),
               actionButton("Upload","upload tracing / CATMAID pull")
             ),
        # Show a plot of the brain
        mainPanel(
          includeCSS("loader.css"), # Load spinny waiting wheel
          HTML("<div class='loader' style='position: absolute; left: 400px; top: 300px; z-index: -10000;'>Loading...</div>"),
          HTML("<div style='position: absolute; left: 220px; top: 270px; z-index: -10000; text-align: center; width: 400px; font-size: 30px;'>Loading...</div>"),
          # Output: Tabset
          tabsetPanel(type = "tabs",
                      tabPanel("Visualise", 
                                 rglwidgetOutput("plot3D", width="1200px", height="700px"),
                                 uiOutput("MainTable")),
                      tabPanel("E-Phys",
                                 br(),
                                 uiOutput("ChooseCTs"),
                                 checkboxInput(inputId="CTmean", value=TRUE, label ="use cell type means"),
                                 hr(),
                                 plotly::plotlyOutput("Ephys")
                              ),
                      tabPanel("Odours",
                               br(),
                               uiOutput("ChooseOdours"),
                               shiny::bootstrapPage(
                                 div(style="display:inline-block",checkboxInput(inputId="OdourMean", value= FALSE, label ="use mean of chosen odours")),
                                 div(style="display:inline-block",checkboxInput(inputId="OdourCTMean", value= TRUE, label ="use mean of cell.types"))
                               ),
                               hr(),
                               plotly::plotlyOutput("OdoursResponses")
                      ),
                      tabPanel("Split Gal4 lines",
                               br(),
                               uiOutput("LineCTs"),
                               uiOutput("LineCode"),
                               #imageOutput("MaximalProjection")
                               img(src = "example.png")
                      )

                  )
          )
       )
    ),

###################
# NBLast neurons #
###################

tabPanel("NBLAST",
         sidebarLayout(
           sidebarPanel(
             h2("NBLAST Against the LH Library"),
             HTML("Want to know what cell type your neuron belongs to? Or find a genetic line for it? 
                  Choose a neuron from out library or a neuron you have uploaded and blast it against our library.
                  If the checkbox below is ticked, both forwards and reverse scores will be calculated, normalised and averaged,
                  rather than just using the forwards score. The query neuron will be <b><span style='color: black;'>plotted in black</span></b>
                  in the 3D viewer to the right, alongside the top 10 hits (rainbow coloured from <span style='color: #F21A00;'>red = best</span> to <span style='color: #3B9AB2;'>cyan = worst</span>)."),
             HTML("See what the scores mean <a href='http://flybrain.mrc-lmb.cam.ac.uk/si/nblast/www/how/'>here</a>"),
             hr(),
             strong("To NBLAST neurons they must be in the neuron selection table, in the Explore tab"),
             hr(),
             selectInput(inputId='QueryType', label='query type:'%>%label.help("lbl_qt"), choices = list(`LH library neuron(s)` = "Library",`uploaded neuron(s)` = "UserUpload"), selected = list(`LH library neuron(s)` = "Library"), multiple=FALSE, selectize=TRUE),
             uiOutput("ChooseUploadedSkeletons"),
             uiOutput("NBLAST_SkeletonType"),
             uiOutput("NBLAST_ChooseFromLibrary"),
             uiOutput("NBLAST_ChooseID"),
             HTML("<i>If multiple neurons are chosen, NBLAST scores will be averaged across these neurons. I.e. they will be treated as one amalgamated neuron</i><br /><br />"),
             sliderInput(inputId = "NumHits",label = "no. hits to visualise:", 1, 100, 10, 1),
             checkboxInput("UseMean", label="Use mean scores", value= TRUE),
             HTML("<i>Using the mean score is useful for finding exact matches, i.e. one in which the target is a good hit for the query and the query is a good hit for the target too.
                  This is particularly useful for clustering neurons into types, rather than,
                  for example, just finding neurons that go through the same tract but branch off differently.</i><br /><br />"),
             actionButton("NBLASTGO","NBLAST")
             ),
         mainPanel(
           h2("3D view"),
           includeCSS("loader.css"),
           HTML("<div class='loader' style='position: absolute; left: 400px; top: 300px; z-index: -10000;'>Loading...</div>"),
           HTML("<div style='position: absolute; left: 220px; top: 270px; z-index: -10000; text-align: center; width: 400px; font-size: 30px;'>Loading...</div>"),
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
tabPanel("LH Naming System",
         HTML("This web app accompanies <a href='http://dx.doi.org/10.1016/j.neuron.2016.06.012'>Costa et al. NBLAST: Rapid, sensitive comparison of neuronal structure and construction of neuron family databases. Neuron (2016)</a>. A pre-print version is available from <a href='http://dx.doi.org/10.1101/006346'>BiorXiv: Costa et al. (2014)</a>. More information on other NBLAST resources is available <a href='http://jefferislab.org/si/nblast'>here</a>. NBLAST on-the-fly acts as a demonstration of the core NBLAST algorithm (package <a href='https://github.com/jefferislab/nat.nblast'>nat.nblast</a>), along with some features of the <a href='https://github.com/jefferis/nat'>NeuroAnatomy Toolbox</a> and its helper packages: <a href='https://github.com/jefferislab/nat.templatebrains'>nat.templatebrains</a> and <a href='https://github.com/jefferislab/nat.flybrains'>nat.flybrains</a>. Other resources available are listed <a href='http://jefferislab.org/si/nblast/www/'>here</a>. For further information on how we convert data between template brains, see <a href='http://jefferislab.org/si/bridging/'>here</a>."),
         img(src='LHN_naming_cartoon.png', width="732px", height="1170", align = "center"),
         hr(),
         HTML("Protocols for <a href='http://cshprotocols.cshlp.org/content/2013/4/pdb.prot071720.full'>immunostaining and imaging fly brains</a>, as well as <a href='http://cshprotocols.cshlp.org/content/2013/4/pdb.prot071738.full'>registration of the resulting images</a> are available from Cold Spring Harbor Protocols. We recommend the use of <a href='http://fiji.sc/Simple_Neurite_Tracer'>Simple Neurite Tracer</a> for tracing neurons from the acquired images, detailed instructions for which are available from <a href='http://fiji.sc/Simple_Neurite_Tracer:_Step-By-Step_Instructions'>here</a>.")
),

###############
# Publications #
###############
tabPanel("Publications",
         HTML("This web app accompanies <a href='http://dx.doi.org/10.1016/j.neuron.2016.06.012'>Costa et al. NBLAST: Rapid, sensitive comparison of neuronal structure and construction of neuron family databases. Neuron (2016)</a>. A pre-print version is available from <a href='http://dx.doi.org/10.1101/006346'>BiorXiv: Costa et al. (2014)</a>. More information on other NBLAST resources is available <a href='http://jefferislab.org/si/nblast'>here</a>. NBLAST on-the-fly acts as a demonstration of the core NBLAST algorithm (package <a href='https://github.com/jefferislab/nat.nblast'>nat.nblast</a>), along with some features of the <a href='https://github.com/jefferis/nat'>NeuroAnatomy Toolbox</a> and its helper packages: <a href='https://github.com/jefferislab/nat.templatebrains'>nat.templatebrains</a> and <a href='https://github.com/jefferislab/nat.flybrains'>nat.flybrains</a>. Other resources available are listed <a href='http://jefferislab.org/si/nblast/www/'>here</a>. For further information on how we convert data between template brains, see <a href='http://jefferislab.org/si/bridging/'>here</a>."),
         h3("Video demos"),
         HTML("Video demos showing how to use this web app and other related resources are available <a href='http://jefferislab.org/si/nblast/www/demos/'>here</a>."),
         h3("More help"),
         HTML("If you require information not contained in the manuscript, you can use the <a href='https://groups.google.com/forum/#!forum/nat-user'>nat-user google group</a> shown below. Searching the archive is available to all. To post a question you will first need to request to join the group.<br />
              
              <iframe id='forum_embed' src='javascript:void(0)' scrolling='no' frameborder='0' width='900' height='700'>
              </iframe>
              
              <script type='text/javascript'>
              document.getElementById('forum_embed').src =
              'https://groups.google.com/forum/embed/?place=forum/nat-user' +
              '&showsearch=true&showpopout=true&hideforumtitle=true&h1&fragments=true&parenturl=' +
              encodeURIComponent(window.location.href);
              </script>"),
         h3("Local installation"),
         HTML("Instructions on how to install this app locally are <a href='https://github.com/jefferislab/NBLAST_on-the-fly'>here</a>, along with a video demo <a href='http://jefferislab.org/si/nblast/www/demos/#nblast-online'>here</a>."),
         h3("Source code"),
         HTML("The full code for this web app can be downloaded from <a href='https://github.com/jefferislab/NBLAST_online'>GitHub</a>."),
         h3("Preparing own data"),
         HTML("Protocols for <a href='http://cshprotocols.cshlp.org/content/2013/4/pdb.prot071720.full'>immunostaining and imaging fly brains</a>, as well as <a href='http://cshprotocols.cshlp.org/content/2013/4/pdb.prot071738.full'>registration of the resulting images</a> are available from Cold Spring Harbor Protocols. We recommend the use of <a href='http://fiji.sc/Simple_Neurite_Tracer'>Simple Neurite Tracer</a> for tracing neurons from the acquired images, detailed instructions for which are available from <a href='http://fiji.sc/Simple_Neurite_Tracer:_Step-By-Step_Instructions'>here</a>.")
),


#########
# About #
#########
tabPanel("About",
         HTML("This web app accompanies <a href='http://dx.doi.org/10.1016/j.neuron.2016.06.012'>Costa et al. NBLAST: Rapid, sensitive comparison of neuronal structure and construction of neuron family databases. Neuron (2016)</a>. A pre-print version is available from <a href='http://dx.doi.org/10.1101/006346'>BiorXiv: Costa et al. (2014)</a>. More information on other NBLAST resources is available <a href='http://jefferislab.org/si/nblast'>here</a>. NBLAST on-the-fly acts as a demonstration of the core NBLAST algorithm (package <a href='https://github.com/jefferislab/nat.nblast'>nat.nblast</a>), along with some features of the <a href='https://github.com/jefferis/nat'>NeuroAnatomy Toolbox</a> and its helper packages: <a href='https://github.com/jefferislab/nat.templatebrains'>nat.templatebrains</a> and <a href='https://github.com/jefferislab/nat.flybrains'>nat.flybrains</a>. Other resources available are listed <a href='http://jefferislab.org/si/nblast/www/'>here</a>. For further information on how we convert data between template brains, see <a href='http://jefferislab.org/si/bridging/'>here</a>."),
         h3("Video demos"),
         HTML("Video demos showing how to use this web app and other related resources are available <a href='http://jefferislab.org/si/nblast/www/demos/'>here</a>."),
         h3("More help"),
         HTML("If you require information not contained in the manuscript, you can use the <a href='https://groups.google.com/forum/#!forum/nat-user'>nat-user google group</a> shown below. Searching the archive is available to all. To post a question you will first need to request to join the group.<br />
              
              <iframe id='forum_embed' src='javascript:void(0)' scrolling='no' frameborder='0' width='900' height='700'>
              </iframe>
              
              <script type='text/javascript'>
              document.getElementById('forum_embed').src =
              'https://groups.google.com/forum/embed/?place=forum/nat-user' +
              '&showsearch=true&showpopout=true&hideforumtitle=true&h1&fragments=true&parenturl=' +
              encodeURIComponent(window.location.href);
              </script>"),
         h3("Local installation"),
         HTML("Instructions on how to install this app locally are <a href='https://github.com/jefferislab/NBLAST_on-the-fly'>here</a>, along with a video demo <a href='http://jefferislab.org/si/nblast/www/demos/#nblast-online'>here</a>."),
         h3("Source code"),
         HTML("The full code for this web app can be downloaded from <a href='https://github.com/jefferislab/NBLAST_online'>GitHub</a>."),
         h3("Preparing own data"),
         HTML("Protocols for <a href='http://cshprotocols.cshlp.org/content/2013/4/pdb.prot071720.full'>immunostaining and imaging fly brains</a>, as well as <a href='http://cshprotocols.cshlp.org/content/2013/4/pdb.prot071738.full'>registration of the resulting images</a> are available from Cold Spring Harbor Protocols. We recommend the use of <a href='http://fiji.sc/Simple_Neurite_Tracer'>Simple Neurite Tracer</a> for tracing neurons from the acquired images, detailed instructions for which are available from <a href='http://fiji.sc/Simple_Neurite_Tracer:_Step-By-Step_Instructions'>here</a>.")
         )

  )

)
