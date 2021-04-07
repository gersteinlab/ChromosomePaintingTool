#============================ ACKNOWLEDGMENT ============================================#
# Code written by Gabriel Conte Cortez Martins, Yale 2020 and Daniel Farid, Yale 2023. Help from Mark Gerstein, 
# Joel Rosowsky, Gamze Gursoy, Timur Galeev, and Kun Xiong among other members of the Gerstein Lab at Yale University
#============================ NECESSARY LIBRARIES FOR CHROMOMAPS AND APP ============================================#
library(shinydashboard)
library(stringr)
library(chromoMap)
library(shiny)
library(htmlwidgets)
# install.packages('rmarkdown')
# library(rmarkdown)
#============================ LIBRARIES FOR SHINYAPP.IO DEPLOYMENT (CAN KEEP COMMENTED OUT) ============================================#
#install.packages('rsconnect')
# rsconnect::setAccountInfo(name='chromosomepainting', token='E48DEFDBE0B5EB7A76E9EACC0555E6B1', secret='/sIPciOe23PyR+l2D9Ou0e+3NK1lhZaeV9qThgOI')
#library(rsconnect)
#rsconnect::deployApp('gersteinENTExChromosomePainting.R')
#deployApp('version5')
#============================ UI ============================================#

ui <- dashboardPage(
  dashboardHeader(title = "ENTEx Chromosome Painting Tool",titleWidth  = 350),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem(
        sliderInput('number_of_tracks',"Number of Tracks", 
                    min = 1, max = 4, step = 1, value = 1)
      ),
      sidebarMenuOutput("menu"),
      actionButton('submit',"Submit"),
      downloadButton('download', "Download")
    )),
  dashboardBody(
    tabBox(
      title = "Chromosome Painting",
      id = "chromomap",
      tabPanel("Information", uiOutput("infopanel")),
      tabPanel("chromoMap", shinydashboard::box(htmlOutput("myChromoMap"), width = 5000, height = 2500)),
      #shinydashboard::box(htmlOutput("myChromoMap"), width = 2000, height = 1700),
      width = 5000,
      height = 2500
    )
)
)

#============================= Server =======================================#



server <- function(input, output, session) {
  
  output$menu <- renderMenu({
    ###########################################################################
    ### THIS IS THE TRACK WE ARE REFERING TO AT SERVER AT THE MOMENT. FOR   ###
    ### MAXIMUM CUSTOMIZABILITY, THE CODE FOR EACH INSTANCE OF MORE THAN    ###
    ### ONE TRACK IS SEPARATED, SO THE BEHAVIOR CAN BE CHANGED INDIVIDUALLY ###
    ###########################################################################
    
    ############################################################################
    ### ADDITIONALLY, THE CODE FOR EACH TRACK'S MENU ITEM IS REPEATED EVERY  ###
    ### TIME, SO THAT THE MENU ITEMS CAN BE CUSTOMIZED DEPENDING ON HOW MANY ###
    ### TRACKS ARE SELECTED.                                                 ###
    ############################################################################
    
    indiv_menu = c("ENC001", "ENC002", "ENC003", "ENC004")
    tissue_menu = c("Adrenal Gland", "Transverse Colon", "Esophagus Muscularis Mucosa", "Upper Lobe Of Left Lung")
    assay_menu = c("H3K27ac", "ATAC-seq", "total_RNA-seq", "POLR2A", "CTCF", "H3K9me3")
    ploidy_menu = list("Haplotype 1","Haplotype 2")
    chrmselection_menu = list("ALL", "Extra Functionality Coming Soon")
      
    # list("ALL","chr1","chr2","chr3","chr4","chr5","chr6", 
    #      "chr7","chr8","chr9","chr10","chr11","chr12", "chr13",
    #      "chr14","chr15","chr16","chr17","chr18", "chr19","chr20",
    #      "chr21","chr22","chrX","chrY")
    
    if (input$number_of_tracks == 1) { #ONE TRACK
      sidebarMenu(
        menuItem("Track 1",
                 
                 selectInput('individual1', 'Individual',
                             indiv_menu,
                             selected = "ENC003"
                 ),
                 selectInput('assay1', 'Assay',
                             assay_menu,
                             selected = "H3K27ac"
                 ),
                 selectInput('tissue1', 'Tissue',
                             tissue_menu,
                             selected = "Transverse Colon"
                 ),
                 selectInput('ploidy1', 'Ploidy',
                             ploidy_menu,
                             selected = "Haplotype 1"
                 ),
                 selectInput('heat_map_colors1', 'Annotation Color',
                             list("red","blue","gold","green","purple"))
        ),
        menuItem("Advanced",
                 selectInput('chromosomes', 'Chromosomes',
                             chrmselection_menu
                 ),
                 textInput('region',"Region", placeholder = "Extra Functionality Coming Soon"),
                 selectInput('plot_type',"Plot Type",list("Heat Map","Histogram","Scatterplot"))
                 
                 
        ))
    }
    
    else if (input$number_of_tracks == 2) { #TWO TRACKS
      
      sidebarMenu(
        menuItem("Track 1",
                 selectInput('individual1', 'Individual',
                             indiv_menu
                 ),
                 selectInput('assay1', 'Assay',
                             assay_menu
                 ),
                 selectInput('tissue1', 'Tissue',
                             tissue_menu
                 ),
                 selectInput('ploidy1', 'Ploidy',
                             ploidy_menu
                 ),
                 selectInput('heat_map_colors1', 'Annotation Color',
                             list("red","blue","gold","green","purple"))
        ),
        menuItem("Track 2",
                 selectInput('individual2', 'Individual',
                             indiv_menu
                 ),
                 selectInput('assay2', 'Assay',
                             assay_menu
                 ),
                 selectInput('tissue2', 'Tissue',
                             tissue_menu
                 ),
                 selectInput('ploidy2', 'Ploidy',
                             ploidy_menu
                 ),
                 selectInput('heat_map_colors2', 'Annotation Color',
                             list("red","blue","gold","green","purple")
                 )
        ),
        menuItem("Advanced",
                 selectInput('chromosomes', 'Chromosomes',
                             chrmselection_menu
                 ),
                 textInput('region',"Region"),
                 selectInput('plot_type',"Plot Type",list("Heat Map","Histogram","Scatterplot"))
                 
                 
        )
      )
      
    }
    
    else if (input$number_of_tracks == 3) { #THREE TRACKS
      
      sidebarMenu(
        menuItem("Track 1",
                 selectInput('individual1', 'Individual',
                             indiv_menu
                 ),
                 selectInput('assay1', 'Assay',
                             assay_menu
                 ),
                 selectInput('tissue1', 'Tissue',
                             tissue_menu
                 ),
                 selectInput('ploidy1', 'Ploidy',
                             ploidy_menu
                 ),
                 selectInput('heat_map_colors1', 'Annotation Color',
                             list("red","blue","gold","green","purple"))
        ),
        menuItem("Track 2",
                 selectInput('individual2', 'Individual',
                             indiv_menu
                 ),
                 selectInput('assay2', 'Assay',
                             assay_menu
                 ),
                 selectInput('tissue2', 'Tissue',
                             tissue_menu
                 ),
                 selectInput('ploidy2', 'Ploidy',
                             ploidy_menu
                 ),
                 selectInput('heat_map_colors2', 'Annotation Color',
                             list("red","blue","gold","green","purple")
                 )
        ),
        menuItem("Track 3",
                 
                 selectInput('individual3', 'Individual',
                             indiv_menu
                 ),
                 selectInput('assay3', 'Assay',
                             assay_menu
                 ),
                 selectInput('tissue3', 'Tissue',
                             tissue_menu
                 ),
                 selectInput('ploidy3', 'Ploidy',
                             ploidy_menu
                 ),
                 selectInput('heat_map_colors3', 'Annotation Color',
                             list("red","blue","gold","green","purple")
                 )
        ),
        menuItem("Advanced",
                 selectInput('chromosomes', 'Chromosomes',
                             chrmselection_menu
                 ),
                 textInput('region',"Region"),
                 selectInput('plot_type',"Plot Type",list("Heat Map","Histogram","Scatterplot"))
        )
      )
    }
    
    else if (input$number_of_tracks == 4) { #4 TRACKS
      
      sidebarMenu(
        menuItem("Track 1",
                 selectInput('individual1', 'Individual',
                             indiv_menu
                 ),
                 selectInput('assay1', 'Assay',
                             assay_menu
                 ),
                 selectInput('tissue1', 'Tissue',
                             tissue_menu
                 ),
                 selectInput('ploidy1', 'Ploidy',
                             ploidy_menu
                 ),
                 selectInput('heat_map_colors1', 'Annotation Color',
                             list("red","blue","gold","green","purple"))
        ),
        menuItem("Track 2",
                 selectInput('individual2', 'Individual',
                             indiv_menu
                 ),
                 selectInput('assay2', 'Assay',
                             assay_menu
                 ),
                 selectInput('tissue2', 'Tissue',
                             tissue_menu
                 ),
                 selectInput('ploidy2', 'Ploidy',
                             ploidy_menu
                 ),
                 selectInput('heat_map_colors2', 'Annotation Color',
                             list("red","blue","gold","green","purple")
                 )
        ),
        menuItem("Track 3",
                 
                 selectInput('individual3', 'Individual',
                             indiv_menu
                 ),
                 selectInput('assay3', 'Assay',
                             assay_menu
                 ),
                 selectInput('tissue3', 'Tissue',
                             tissue_menu
                 ),
                 selectInput('ploidy3', 'Ploidy',
                             ploidy_menu
                 ),
                 selectInput('heat_map_colors3', 'Annotation Color',
                             list("red","blue","gold","green","purple")
                 )
        ),
        menuItem("Track 4",
                 selectInput('individual4', 'Individual',
                             indiv_menu
                 ),
                 selectInput('assay4', 'Assay',
                             assay_menu
                 ),
                 selectInput('tissue4', 'Tissue',
                             tissue_menu
                 ),
                 selectInput('ploidy4', 'Ploidy',
                             ploidy_menu
                 ),
                 selectInput('heat_map_colors4', 'Annotation Color',
                             list("red","blue","gold","green","purple")
                 )
        ),
        menuItem("Advanced",
                 selectInput('chromosomes', 'Chromosomes',
                             chrmselection_menu
                 ),
                 textInput('region',"Region"),
                 selectInput('plot_type',"Plot Type",list("Heat Map","Histogram","Scatterplot"))
        )
      )
    }
  })
  
  ################################################################################
  ### AFTER SUBMIT BUTTON IS CLICKED, PULL THE RIGHT ANNOTATION FILE DEPENDING ###
  ### ON THE SELECTED INPUTS (INDIVIDUAL, ASSAY, TISSUE, PLOIDY)               ###
  ################################################################################
  
  ################################################################################
  ### ALSO, CONFIGURE THE CHROMOMAP BASED ON ADVANCED SETTINGS, LIKE PLOT TYPE ###
  ### AND OTHER SETTINGS LIKE COLOR                                            ###
  ################################################################################
  
  output$infopanel <- renderUI({
    tabPanel("Information", HTML("<h1> Welcome to the <b>Chromosome Painting Tool</b></h1>
                                 <h3> This tab, the <b>Information</b> tab, shows information about the currently selected chromosome painting </h3>
                                 <h3> This includes the <b>individual</b>, <b>tissue</b>, and <b>assay</b> for each selected track </h3>
                                 <br>
                                 <h3> Select the <b>Number of Tracks</b> at the top of the sidebar on the left, and select the individual, tissue, and assay using each track's <b>dropdown menu</b> </h3>
                                 <h3> Then, click <b>submit</b> and wait for plot to generate (may take up to 15-20 seconds in some cases) </h3>"
                                 ))
  })
  
  observeEvent(input$submit,{
    
      if (input$chromosomes != "ALL" || input$region != "") {
        updateTabItems(session, "chromomap", selected = "Information")
        panelText = HTML("<h2>Functionality for selecting individual chromosomes and specific regions will be added to this tool shortly.</h2>
                             <h3>For now, please select <b>'ALL' chromosomes</b> and leave <b>Regions blank</b>, and click <b>submit</b> again</h3>
                             <br>
                             Note: the chromoMap tab will show an error")
      }
      else if (input$number_of_tracks == 1) {
        if ((input$individual1 == "ENC001" && input$assay1 == "POLR2A" && input$tissue1 == "Esophagus Muscularis Mucosa") ||
            (input$individual1 == "ENC001" && input$assay1 == "POLR2A" && input$tissue1 == "Adrenal Gland") ||
            (input$individual1 == "ENC001" && input$assay1 == "ATAC-seq" && input$tissue1 == "Esophagus Muscularis Mucosa") ||
            (input$individual1 == "ENC001" && input$assay1 == "ATAC-seq" && input$tissue1 == "Upper Lobe Of Left Lung") ||
            (input$individual1 == "ENC002" && input$assay1 == "POLR2A" && input$tissue1 == "Adrenal Gland") ||
            (input$individual1 == "ENC002" && input$assay1 == "POLR2A" && input$tissue1 == "Esophagus Muscularis Mucosa") ||
            (input$individual1 == "ENC002" && input$assay1 == "ATAC-seq" && input$tissue1 == "Upper Lobe Of Left Lung") ||
            (input$individual1 == "ENC003" && input$assay1 == "ATAC-seq" && input$tissue1 == "Adrenal Gland") ||
            (input$individual1 == "ENC003" && input$assay1 == "ATAC-seq" && input$tissue1 == "Esophagus Muscularis Mucosa") ||
            (input$individual1 == "ENC003" && input$assay1 == "H3K9me3" && input$tissue1 == "Transverse Colon") ||
            (input$individual1 == "ENC003" && input$assay1 == "ATAC-seq" && input$tissue1 == "Upper Lobe Of Left Lung") ||
            (input$individual1 == "ENC004" && input$assay1 == "ATAC-seq" && input$tissue1 == "Esophagus Muscularis Mucosa") || 
            (input$individual1 == "ENC004" && input$assay1 == "POLR2A" && input$tissue1 == "Esophagus Muscularis Mucosa") ||
            (input$individual1 == "ENC004" && input$assay1 == "H3K9me3" && input$tissue1 == "Transverse Colon") ||
            (input$individual1 == "ENC004" && input$assay1 == "ATAC-seq" && input$tissue1 == "Upper Lobe Of Left Lung"))
        {
          updateTabItems(session, "chromomap", selected = "Information")
          panelText = HTML("<h2>Data for this particular combination of individual, assay, and tissue is <b>not available.</b> </h2>
                             <h3>This could be because this specific experiment was never performed, or the data is not available on the ENCODE online portal.</h3>
                             <h3>Please re-select inputs on the left and click submit again</h3>
                             <br>
                             Note: <b>the chromoMap tab will show an error")
        }
        else {
          panelText = HTML(sprintf("<h2> chromoMap <strong>generated</strong></h2>
                                    <h5> Note: chromoMap is generated on the <b>chromoMap</b> tab to the right of 'Information'</h5>
                                    <h5> Any error means there is missing data that will be added in the future </h5>
                                   <h4># of tracks: <b>1</b> </h4>
                                   <h4> Individual: <b>%s</b> </h4>
                                   <h4> Tissue: <b>%s</b></h4>
                                   <h4> Assay: <b> %s </b></h4>
                                   <h4> Haplotype: <b> %s </b></h4>
                                   <h4> Color: <b> %s </b></h4>
                                   <br>
                                   <h4>Click on <strong> chromoMap </strong> tab to view painting (may take up to 15 seconds after switching tabs to generate and appear).</h4>", 
                                   input$individual1, input$tissue1, input$assay1, input$ploidy1, input$heat_map_colors1))
        }
      }
      else if (input$number_of_tracks == 2) {
        if ((input$individual1 == "ENC001" && input$assay1 == "POLR2A" && input$tissue1 == "Esophagus Muscularis Mucosa") ||
            (input$individual1 == "ENC001" && input$assay1 == "POLR2A" && input$tissue1 == "Adrenal Gland") ||
            (input$individual1 == "ENC001" && input$assay1 == "ATAC-seq" && input$tissue1 == "Esophagus Muscularis Mucosa") ||
            (input$individual1 == "ENC001" && input$assay1 == "ATAC-seq" && input$tissue1 == "Upper Lobe Of Left Lung") ||
            (input$individual1 == "ENC002" && input$assay1 == "POLR2A" && input$tissue1 == "Adrenal Gland") ||
            (input$individual1 == "ENC002" && input$assay1 == "POLR2A" && input$tissue1 == "Esophagus Muscularis Mucosa") ||
            (input$individual1 == "ENC002" && input$assay1 == "ATAC-seq" && input$tissue1 == "Upper Lobe Of Left Lung") ||
            (input$individual1 == "ENC003" && input$assay1 == "ATAC-seq" && input$tissue1 == "Adrenal Gland") ||
            (input$individual1 == "ENC003" && input$assay1 == "ATAC-seq" && input$tissue1 == "Esophagus Muscularis Mucosa") ||
            (input$individual1 == "ENC003" && input$assay1 == "H3K9me3" && input$tissue1 == "Transverse Colon") ||
            (input$individual1 == "ENC003" && input$assay1 == "ATAC-seq" && input$tissue1 == "Upper Lobe Of Left Lung") ||
            (input$individual1 == "ENC004" && input$assay1 == "ATAC-seq" && input$tissue1 == "Esophagus Muscularis Mucosa") || 
            (input$individual1 == "ENC004" && input$assay1 == "POLR2A" && input$tissue1 == "Esophagus Muscularis Mucosa") ||
            (input$individual1 == "ENC004" && input$assay1 == "H3K9me3" && input$tissue1 == "Transverse Colon") ||
            (input$individual1 == "ENC004" && input$assay1 == "ATAC-seq" && input$tissue1 == "Upper Lobe Of Left Lung") ||
            
            (input$individual2 == "ENC001" && input$assay2 == "POLR2A" && input$tissue2 == "Esophagus Muscularis Mucosa") ||
            (input$individual2 == "ENC001" && input$assay2 == "POLR2A" && input$tissue2 == "Adrenal Gland") ||
            (input$individual2 == "ENC001" && input$assay2 == "ATAC-seq" && input$tissue2 == "Esophagus Muscularis Mucosa") ||
            (input$individual2 == "ENC001" && input$assay2 == "ATAC-seq" && input$tissue2 == "Upper Lobe Of Left Lung") ||
            (input$individual2 == "ENC002" && input$assay2 == "POLR2A" && input$tissue2 == "Adrenal Gland") ||
            (input$individual2 == "ENC002" && input$assay2 == "POLR2A" && input$tissue2 == "Esophagus Muscularis Mucosa") ||
            (input$individual2 == "ENC002" && input$assay2 == "ATAC-seq" && input$tissue2 == "Upper Lobe Of Left Lung") ||
            (input$individual2 == "ENC003" && input$assay2 == "ATAC-seq" && input$tissue2 == "Adrenal Gland") ||
            (input$individual2 == "ENC003" && input$assay2 == "ATAC-seq" && input$tissue2 == "Esophagus Muscularis Mucosa") ||
            (input$individual2 == "ENC003" && input$assay2 == "H3K9me3" && input$tissue2 == "Transverse Colon") ||
            (input$individual2 == "ENC003" && input$assay2 == "ATAC-seq" && input$tissue2 == "Upper Lobe Of Left Lung") ||
            (input$individual2 == "ENC004" && input$assay2 == "ATAC-seq" && input$tissue2 == "Esophagus Muscularis Mucosa") || 
            (input$individual2 == "ENC004" && input$assay2 == "POLR2A" && input$tissue2 == "Esophagus Muscularis Mucosa") ||
            (input$individual2 == "ENC004" && input$assay2 == "H3K9me3" && input$tissue2 == "Transverse Colon") ||
            (input$individual2 == "ENC004" && input$assay2 == "ATAC-seq" && input$tissue2 == "Upper Lobe Of Left Lung"))
        {
          updateTabItems(session, "chromomap", selected = "Information")
          panelText = HTML("<h2>Data for this particular combination of individual, assay, and tissue is <b>not available.</b> </h2>
                             <h3>This could be because this specific experiment was never performed, or the data is not available on the ENCODE online portal.</h3>
                             <h3>Please re-select inputs on the left and click submit again</h3>
                             <br>
                             Note: <b>the chromoMap tab will show an error")
        }
        else {
          panelText = HTML(sprintf("<h2> chromoMap <strong>generated</strong></h2>
                                    <h5> Note: chromoMap is generated on the <b>chromoMap</b> tab to the right of 'Information'</h5>
                                    <h5> Any error means there is missing data that will be added in the future </h5>
                                   <h4># of tracks: <b>2</b> </h4>
                                   <br>
                                   <h4> Track 1 Individual: <b>%s</b> </h4>
                                   <h4> Track 1 Tissue: <b>%s</b></h4>
                                   <h4> Track 1 Assay: <b> %s </b></h4>
                                   <h4> Track 1 Haplotype: <b> %s </b></h4>
                                   <h4> Track 1 Color: <b> %s </b></h4>
                                   <br>
                                   <h4> Track 2 Individual: <b>%s</b> </h4>
                                   <h4> Track 2 Tissue: <b>%s</b></h4>
                                   <h4> Track 2 Assay: <b> %s </b></h4>
                                   <h4> Track 2 Haplotype: <b> %s </b></h4>
                                   <h4> Track 2 Color: <b> %s </b></h4>
                                   <br>
                                   <h4>Click on <strong> chromoMap </strong> tab to view painting (may take up to 15 seconds after switching tabs to generate and appear).</h4>", 
                                   input$individual1, input$tissue1, input$assay1, input$ploidy1, input$heat_map_colors1,
                                   input$individual2, input$tissue2, input$assay2, input$ploidy2, input$heat_map_colors2 ))
        }
      }
      else if (input$number_of_tracks == 3) {
        if ((input$individual1 == "ENC001" && input$assay1 == "POLR2A" && input$tissue1 == "Esophagus Muscularis Mucosa") ||
            (input$individual1 == "ENC001" && input$assay1 == "POLR2A" && input$tissue1 == "Adrenal Gland") ||
            (input$individual1 == "ENC001" && input$assay1 == "ATAC-seq" && input$tissue1 == "Esophagus Muscularis Mucosa") ||
            (input$individual1 == "ENC001" && input$assay1 == "ATAC-seq" && input$tissue1 == "Upper Lobe Of Left Lung") ||
            (input$individual1 == "ENC002" && input$assay1 == "POLR2A" && input$tissue1 == "Adrenal Gland") ||
            (input$individual1 == "ENC002" && input$assay1 == "POLR2A" && input$tissue1 == "Esophagus Muscularis Mucosa") ||
            (input$individual1 == "ENC002" && input$assay1 == "ATAC-seq" && input$tissue1 == "Upper Lobe Of Left Lung") ||
            (input$individual1 == "ENC003" && input$assay1 == "ATAC-seq" && input$tissue1 == "Adrenal Gland") ||
            (input$individual1 == "ENC003" && input$assay1 == "ATAC-seq" && input$tissue1 == "Esophagus Muscularis Mucosa") ||
            (input$individual1 == "ENC003" && input$assay1 == "H3K9me3" && input$tissue1 == "Transverse Colon") ||
            (input$individual1 == "ENC003" && input$assay1 == "ATAC-seq" && input$tissue1 == "Upper Lobe Of Left Lung") ||
            (input$individual1 == "ENC004" && input$assay1 == "ATAC-seq" && input$tissue1 == "Esophagus Muscularis Mucosa") || 
            (input$individual1 == "ENC004" && input$assay1 == "POLR2A" && input$tissue1 == "Esophagus Muscularis Mucosa") ||
            (input$individual1 == "ENC004" && input$assay1 == "H3K9me3" && input$tissue1 == "Transverse Colon") ||
            (input$individual1 == "ENC004" && input$assay1 == "ATAC-seq" && input$tissue1 == "Upper Lobe Of Left Lung") ||
            
            (input$individual2 == "ENC001" && input$assay2 == "POLR2A" && input$tissue2 == "Esophagus Muscularis Mucosa") ||
            (input$individual2 == "ENC001" && input$assay2 == "POLR2A" && input$tissue2 == "Adrenal Gland") ||
            (input$individual2 == "ENC001" && input$assay2 == "ATAC-seq" && input$tissue2 == "Esophagus Muscularis Mucosa") ||
            (input$individual2 == "ENC001" && input$assay2 == "ATAC-seq" && input$tissue2 == "Upper Lobe Of Left Lung") ||
            (input$individual2 == "ENC002" && input$assay2 == "POLR2A" && input$tissue2 == "Adrenal Gland") ||
            (input$individual2 == "ENC002" && input$assay2 == "POLR2A" && input$tissue2 == "Esophagus Muscularis Mucosa") ||
            (input$individual2 == "ENC002" && input$assay2 == "ATAC-seq" && input$tissue2 == "Upper Lobe Of Left Lung") ||
            (input$individual2 == "ENC003" && input$assay2 == "ATAC-seq" && input$tissue2 == "Adrenal Gland") ||
            (input$individual2 == "ENC003" && input$assay2 == "ATAC-seq" && input$tissue2 == "Esophagus Muscularis Mucosa") ||
            (input$individual2 == "ENC003" && input$assay2 == "H3K9me3" && input$tissue2 == "Transverse Colon") ||
            (input$individual2 == "ENC003" && input$assay2 == "ATAC-seq" && input$tissue2 == "Upper Lobe Of Left Lung") ||
            (input$individual2 == "ENC004" && input$assay2 == "ATAC-seq" && input$tissue2 == "Esophagus Muscularis Mucosa") || 
            (input$individual2 == "ENC004" && input$assay2 == "POLR2A" && input$tissue2 == "Esophagus Muscularis Mucosa") ||
            (input$individual2 == "ENC004" && input$assay2 == "H3K9me3" && input$tissue2 == "Transverse Colon") ||
            (input$individual2 == "ENC004" && input$assay2 == "ATAC-seq" && input$tissue2 == "Upper Lobe Of Left Lung") ||
            
            (input$individual3 == "ENC001" && input$assay3 == "POLR2A" && input$tissue3 == "Esophagus Muscularis Mucosa") ||
            (input$individual3 == "ENC001" && input$assay3 == "POLR2A" && input$tissue3 == "Adrenal Gland") ||
            (input$individual3 == "ENC001" && input$assay3 == "ATAC-seq" && input$tissue3 == "Esophagus Muscularis Mucosa") ||
            (input$individual3 == "ENC001" && input$assay3 == "ATAC-seq" && input$tissue3 == "Upper Lobe Of Left Lung") ||
            (input$individual3 == "ENC002" && input$assay3 == "POLR2A" && input$tissue3 == "Adrenal Gland") ||
            (input$individual3 == "ENC002" && input$assay3 == "POLR2A" && input$tissue3 == "Esophagus Muscularis Mucosa") ||
            (input$individual3 == "ENC002" && input$assay3 == "ATAC-seq" && input$tissue3 == "Upper Lobe Of Left Lung") ||
            (input$individual3 == "ENC003" && input$assay3 == "ATAC-seq" && input$tissue3 == "Adrenal Gland") ||
            (input$individual3 == "ENC003" && input$assay3 == "ATAC-seq" && input$tissue3 == "Esophagus Muscularis Mucosa") ||
            (input$individual3 == "ENC003" && input$assay3 == "H3K9me3" && input$tissue3 == "Transverse Colon") ||
            (input$individual3 == "ENC003" && input$assay3 == "ATAC-seq" && input$tissue3 == "Upper Lobe Of Left Lung") ||
            (input$individual3 == "ENC004" && input$assay3 == "ATAC-seq" && input$tissue3 == "Esophagus Muscularis Mucosa") || 
            (input$individual3 == "ENC004" && input$assay3 == "POLR2A" && input$tissue3 == "Esophagus Muscularis Mucosa") ||
            (input$individual3 == "ENC004" && input$assay3 == "H3K9me3" && input$tissue3 == "Transverse Colon") ||
            (input$individual3 == "ENC004" && input$assay3 == "ATAC-seq" && input$tissue3 == "Upper Lobe Of Left Lung"))
        {
          updateTabItems(session, "chromomap", selected = "Information")
          panelText = HTML("<h2>Data for this particular combination of individual, assay, and tissue is <b>not available.</b> </h2>
                             <h3>This could be because this specific experiment was never performed, or the data is not available on the ENCODE online portal.</h3>
                             <h3>Please re-select inputs on the left and click submit again</h3>
                             <br>
                             Note: <b>the chromoMap tab will show an error")
        }
        else {
          panelText = HTML(sprintf("<h2> chromoMap <strong>generated</strong></h2>
                                    <h5> Note: chromoMap is generated on the <b>chromoMap</b> tab to the right of 'Information'</h5>
                                    <h5> Any error means there is missing data that will be added in the future </h5>
                                   <h4># of tracks: <b>3</b> </h4>
                                   <br>
                                   <h4> Track 1 Individual: <b>%s</b> </h4>
                                   <h4> Track 1 Tissue: <b>%s</b></h4>
                                   <h4> Track 1 Assay: <b> %s </b></h4>
                                   <h4> Track 1 Haplotype: <b> %s </b></h4>
                                   <h4> Track 1 Color: <b> %s </b></h4>
                                   <br>
                                   <h4> Track 2 Individual: <b>%s</b> </h4>
                                   <h4> Track 2 Tissue: <b>%s</b></h4>
                                   <h4> Track 2 Assay: <b> %s </b></h4>
                                   <h4> Track 2 Haplotype: <b> %s </b></h4>
                                   <h4> Track 2 Color: <b> %s </b></h4>
                                   <br>
                                   <h4> Track 3 Individual: <b>%s</b> </h4>
                                   <h4> Track 3 Tissue: <b>%s</b></h4>
                                   <h4> Track 3 Assay: <b> %s </b></h4>
                                   <h4> Track 3 Haplotype: <b> %s </b></h4>
                                   <h4> Track 3 Color: <b> %s </b></h4>
                                   <br>
                                   <h4>Click on <strong> chromoMap </strong> tab to view painting (may take up to 15 seconds after switching tabs to generate and appear).</h4>", 
                                   input$individual1, input$tissue1, input$assay1, input$ploidy1, input$heat_map_colors1,
                                   input$individual2, input$tissue2, input$assay2, input$ploidy2, input$heat_map_colors2,
                                   input$individual3, input$tissue3, input$assay3, input$ploidy3, input$heat_map_colors3))
        }
      }
      else if (input$number_of_tracks == 4) {
        if ((input$individual1 == "ENC001" && input$assay1 == "POLR2A" && input$tissue1 == "Esophagus Muscularis Mucosa") ||
            (input$individual1 == "ENC001" && input$assay1 == "POLR2A" && input$tissue1 == "Adrenal Gland") ||
            (input$individual1 == "ENC001" && input$assay1 == "ATAC-seq" && input$tissue1 == "Esophagus Muscularis Mucosa") ||
            (input$individual1 == "ENC001" && input$assay1 == "ATAC-seq" && input$tissue1 == "Upper Lobe Of Left Lung") ||
            (input$individual1 == "ENC002" && input$assay1 == "POLR2A" && input$tissue1 == "Adrenal Gland") ||
            (input$individual1 == "ENC002" && input$assay1 == "POLR2A" && input$tissue1 == "Esophagus Muscularis Mucosa") ||
            (input$individual1 == "ENC002" && input$assay1 == "ATAC-seq" && input$tissue1 == "Upper Lobe Of Left Lung") ||
            (input$individual1 == "ENC003" && input$assay1 == "ATAC-seq" && input$tissue1 == "Adrenal Gland") ||
            (input$individual1 == "ENC003" && input$assay1 == "ATAC-seq" && input$tissue1 == "Esophagus Muscularis Mucosa") ||
            (input$individual1 == "ENC003" && input$assay1 == "H3K9me3" && input$tissue1 == "Transverse Colon") ||
            (input$individual1 == "ENC003" && input$assay1 == "ATAC-seq" && input$tissue1 == "Upper Lobe Of Left Lung") ||
            (input$individual1 == "ENC004" && input$assay1 == "ATAC-seq" && input$tissue1 == "Esophagus Muscularis Mucosa") || 
            (input$individual1 == "ENC004" && input$assay1 == "POLR2A" && input$tissue1 == "Esophagus Muscularis Mucosa") ||
            (input$individual1 == "ENC004" && input$assay1 == "H3K9me3" && input$tissue1 == "Transverse Colon") ||
            (input$individual1 == "ENC004" && input$assay1 == "ATAC-seq" && input$tissue1 == "Upper Lobe Of Left Lung") ||
            
            (input$individual2 == "ENC001" && input$assay2 == "POLR2A" && input$tissue2 == "Esophagus Muscularis Mucosa") ||
            (input$individual2 == "ENC001" && input$assay2 == "POLR2A" && input$tissue2 == "Adrenal Gland") ||
            (input$individual2 == "ENC001" && input$assay2 == "ATAC-seq" && input$tissue2 == "Esophagus Muscularis Mucosa") ||
            (input$individual2 == "ENC001" && input$assay2 == "ATAC-seq" && input$tissue2 == "Upper Lobe Of Left Lung") ||
            (input$individual2 == "ENC002" && input$assay2 == "POLR2A" && input$tissue2 == "Adrenal Gland") ||
            (input$individual2 == "ENC002" && input$assay2 == "POLR2A" && input$tissue2 == "Esophagus Muscularis Mucosa") ||
            (input$individual2 == "ENC002" && input$assay2 == "ATAC-seq" && input$tissue2 == "Upper Lobe Of Left Lung") ||
            (input$individual2 == "ENC003" && input$assay2 == "ATAC-seq" && input$tissue2 == "Adrenal Gland") ||
            (input$individual2 == "ENC003" && input$assay2 == "ATAC-seq" && input$tissue2 == "Esophagus Muscularis Mucosa") ||
            (input$individual2 == "ENC003" && input$assay2 == "H3K9me3" && input$tissue2 == "Transverse Colon") ||
            (input$individual2 == "ENC003" && input$assay2 == "ATAC-seq" && input$tissue2 == "Upper Lobe Of Left Lung") ||
            (input$individual2 == "ENC004" && input$assay2 == "ATAC-seq" && input$tissue2 == "Esophagus Muscularis Mucosa") || 
            (input$individual2 == "ENC004" && input$assay2 == "POLR2A" && input$tissue2 == "Esophagus Muscularis Mucosa") ||
            (input$individual2 == "ENC004" && input$assay2 == "H3K9me3" && input$tissue2 == "Transverse Colon") ||
            (input$individual2 == "ENC004" && input$assay2 == "ATAC-seq" && input$tissue2 == "Upper Lobe Of Left Lung") ||
            
            (input$individual3 == "ENC001" && input$assay3 == "POLR2A" && input$tissue3 == "Esophagus Muscularis Mucosa") ||
            (input$individual3 == "ENC001" && input$assay3 == "POLR2A" && input$tissue3 == "Adrenal Gland") ||
            (input$individual3 == "ENC001" && input$assay3 == "ATAC-seq" && input$tissue3 == "Esophagus Muscularis Mucosa") ||
            (input$individual3 == "ENC001" && input$assay3 == "ATAC-seq" && input$tissue3 == "Upper Lobe Of Left Lung") ||
            (input$individual3 == "ENC002" && input$assay3 == "POLR2A" && input$tissue3 == "Adrenal Gland") ||
            (input$individual3 == "ENC002" && input$assay3 == "POLR2A" && input$tissue3 == "Esophagus Muscularis Mucosa") ||
            (input$individual3 == "ENC002" && input$assay3 == "ATAC-seq" && input$tissue3 == "Upper Lobe Of Left Lung") ||
            (input$individual3 == "ENC003" && input$assay3 == "ATAC-seq" && input$tissue3 == "Adrenal Gland") ||
            (input$individual3 == "ENC003" && input$assay3 == "ATAC-seq" && input$tissue3 == "Esophagus Muscularis Mucosa") ||
            (input$individual3 == "ENC003" && input$assay3 == "H3K9me3" && input$tissue3 == "Transverse Colon") ||
            (input$individual3 == "ENC003" && input$assay3 == "ATAC-seq" && input$tissue3 == "Upper Lobe Of Left Lung") ||
            (input$individual3 == "ENC004" && input$assay3 == "ATAC-seq" && input$tissue3 == "Esophagus Muscularis Mucosa") || 
            (input$individual3 == "ENC004" && input$assay3 == "POLR2A" && input$tissue3 == "Esophagus Muscularis Mucosa") ||
            (input$individual3 == "ENC004" && input$assay3 == "H3K9me3" && input$tissue3 == "Transverse Colon") ||
            (input$individual3 == "ENC004" && input$assay3 == "ATAC-seq" && input$tissue3 == "Upper Lobe Of Left Lung") ||
            
            (input$individual4 == "ENC001" && input$assay4 == "POLR2A" && input$tissue4 == "Esophagus Muscularis Mucosa") ||
            (input$individual4 == "ENC001" && input$assay4 == "POLR2A" && input$tissue4 == "Adrenal Gland") ||
            (input$individual4 == "ENC001" && input$assay4 == "ATAC-seq" && input$tissue4 == "Esophagus Muscularis Mucosa") ||
            (input$individual4 == "ENC001" && input$assay4 == "ATAC-seq" && input$tissue4 == "Upper Lobe Of Left Lung") ||
            (input$individual4 == "ENC002" && input$assay4 == "POLR2A" && input$tissue4 == "Adrenal Gland") ||
            (input$individual4 == "ENC002" && input$assay4 == "POLR2A" && input$tissue4 == "Esophagus Muscularis Mucosa") ||
            (input$individual4 == "ENC002" && input$assay4 == "ATAC-seq" && input$tissue4 == "Upper Lobe Of Left Lung") ||
            (input$individual4 == "ENC003" && input$assay4 == "ATAC-seq" && input$tissue4 == "Adrenal Gland") ||
            (input$individual4 == "ENC003" && input$assay4 == "ATAC-seq" && input$tissue4 == "Esophagus Muscularis Mucosa") ||
            (input$individual4 == "ENC003" && input$assay4 == "H3K9me3" && input$tissue4 == "Transverse Colon") ||
            (input$individual4 == "ENC003" && input$assay4 == "ATAC-seq" && input$tissue4 == "Upper Lobe Of Left Lung") ||
            (input$individual4 == "ENC004" && input$assay4 == "ATAC-seq" && input$tissue4 == "Esophagus Muscularis Mucosa") || 
            (input$individual4 == "ENC004" && input$assay4 == "POLR2A" && input$tissue4 == "Esophagus Muscularis Mucosa") ||
            (input$individual4 == "ENC004" && input$assay4 == "H3K9me3" && input$tissue4 == "Transverse Colon") ||
            (input$individual4 == "ENC004" && input$assay4 == "ATAC-seq" && input$tissue4 == "Upper Lobe Of Left Lung"))
        {
          updateTabItems(session, "chromomap", selected = "Information")
          panelText = HTML("<h2>Data for this particular combination of individual, assay, and tissue is <b>not available.</b> </h2>
                             <h3>This could be because this specific experiment was never performed, or the data is not available on the ENCODE online portal.</h3>
                             <h3>Please re-select inputs on the left and click submit again</h3>
                             <br>
                             Note: <b>the chromoMap tab will show an error")
        }
        else {
          panelText = HTML(sprintf("<h2> chromoMap <strong>generated</strong></h2>
                                    <h5> Note: chromoMap is generated on the <b>chromoMap</b> tab to the right of 'Information'</h5>
                                    <h5> Any error means there is missing data that will be added in the future </h5>
                                   <h4># of tracks: <b>4</b> </h4>
                                   <br>
                                   <h4> Track 1 Individual: <b>%s</b> </h4>
                                   <h4> Track 1 Tissue: <b>%s</b></h4>
                                   <h4> Track 1 Assay: <b> %s </b></h4>
                                   <h4> Track 1 Haplotype: <b> %s </b></h4>
                                   <h4> Track 1 Color: <b> %s </b></h4>
                                   <br>
                                   <h4> Track 2 Individual: <b>%s</b> </h4>
                                   <h4> Track 2 Tissue: <b>%s</b></h4>
                                   <h4> Track 2 Assay: <b> %s </b></h4>
                                   <h4> Track 2 Haplotype: <b> %s </b></h4>
                                   <h4> Track 2 Color: <b> %s </b></h4>
                                   <br>
                                   <h4> Track 3 Individual: <b>%s</b> </h4>
                                   <h4> Track 3 Tissue: <b>%s</b></h4>
                                   <h4> Track 3 Assay: <b> %s </b></h4>
                                   <h4> Track 3 Haplotype: <b> %s </b></h4>
                                   <h4> Track 3 Color: <b> %s </b></h4>
                                   <br>
                                   <h4> Track 4 Individual: <b>%s</b> </h4>
                                   <h4> Track 4 Tissue: <b>%s</b></h4>
                                   <h4> Track 4 Assay: <b> %s </b></h4>
                                   <h4> Track 4 Haplotype: <b> %s </b></h4>
                                   <h4> Track 4 Color: <b> %s </b></h4>
                                   <br>
                                   <h4>Click on <strong> chromoMap </strong> tab to view painting (may take up to 15 seconds after switching tabs to generate and appear).</h4>", 
                                   input$individual1, input$tissue1, input$assay1, input$ploidy1, input$heat_map_colors1,
                                   input$individual2, input$tissue2, input$assay2, input$ploidy2, input$heat_map_colors2,
                                   input$individual3, input$tissue3, input$assay3, input$ploidy3, input$heat_map_colors3,
                                   input$individual4, input$tissue4, input$assay4, input$ploidy4, input$heat_map_colors4))
        }
      }
    
    output$myChromoMap <- renderUI({ 
      
      isolate ({
        
        if(input$individual1 == "ENC001") {pre_chromosome_file_1 <- "chromosome_file_XY.txt"
        } else if (input$individual1 == "ENC002") {print(pre_chromosome_file_1 <- "chromosome_file_XY.txt")
        } else if (input$individual1 == "ENC003") {print (pre_chromosome_file_1 <- "chromosome_file_XX.txt")
        } else if (input$individual1 == "ENC004") {print (pre_chromosome_file_1 <- "chromosome_file_XX.txt")
        } else {print ("ERROR")}
        
        if (input$number_of_tracks == 1) {
          
          print("1 track")
          
          chromosome_file_1 <-paste("chromosome_files",
                                    pre_chromosome_file_1,sep = "/")
          
          input$heat_map_colors1
          tissue1_nospaces = str_replace_all(string=input$tissue1, pattern=" ",
                                             repl="")
          assay1_nospaces = str_replace_all(string=input$assay1, pattern=" ",
                                            repl="")
          ploidy1_nospaces = str_replace_all(string=input$ploidy1, pattern=" ",
                                             repl="")
          
          if (input$chromosomes == "ALL") {print ("ALL CHROMOSOMES")
            
            print ("All chr and no region")
            
            pre_annotation_file_1 <- paste(input$individual1,tissue1_nospaces,
                                           assay1_nospaces,
                                           ploidy1_nospaces,"txt",sep=".")
            annotation_file_1 <- paste("annotation_files",
                                       pre_annotation_file_1,sep = "/")
            
            
            if (input$plot_type == "Heat Map") {plot_type <- "heatmap"
            } else if (input$plot_type == "Histogram") { plot_type <- "histogram"
            } else if (input$plot_type == "Scatterplot") { plot_type <- "scatterplot"
            } else {print ("ERROR in  plot type.")}
            
            html_file_name = paste(annotation_file_1,
                                   input$heat_map_colors1,plot_type,"html",sep=".")
            html_file_name_copy = paste(pre_annotation_file_1,
                                        input$heat_map_colors1,plot_type,"html",sep=".")
            
            if (file.exists(html_file_name)) {
              file.copy(html_file_name,"ENTEX_chromosome_visualization_download.html", overwrite = TRUE)
              ##file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
              includeHTML(html_file_name)
              
            } else {
              
              if (plot_type == "heatmap") {
                
                html_output =  chromoMap(chromosome_file_1,annotation_file_1, 
                                         legend = "T",chr_width = 8, 
                                         chr_length = 6, canvas_width = 2000,
                                         canvas_height = 3000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1)),
                                         chr_color = c("white"))
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                
                includeHTML(html_file_name)
                
              } else if (plot_type == "histogram") {
                
                html_output =  chromoMap(chromosome_file_1,annotation_file_1, 
                                         legend = "T",chr_width = 8, 
                                         chr_length = 6, canvas_width = 2000,
                                         canvas_height = 3000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1)),
                                         chr_color = c("white"), plots = "bar", plot_color = c(input$heat_map_colors1)
                )
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                
                includeHTML(html_file_name)
                
              } else if (plot_type == "scatterplot") { 
                
                html_output =  chromoMap(chromosome_file_1,annotation_file_1, 
                                         legend = "T",chr_width = 8, 
                                         chr_length = 6, canvas_width = 2000,
                                         canvas_height = 3000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1)),
                                         chr_color = c("white"), plots = "scatter", plot_color = c(input$heat_map_colors1))
                
                saveWidget(html_output,html_file_name, background = "white")
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                
                includeHTML(html_file_name)
                
              } else {print ("ERROR in  plot type.")}
            }
            
          } else {test <- paste("This chromosome: ",input$chromosomes,sep = "") 
          print(test)
          
          #============================= NO SPECIFIED INPUT REGION =======================================#
          
          if (input$region == "") {
            
            print ("specified chr and no region")
            
            chr_command_file_1 <- paste(input$individual1,tissue1_nospaces,
                                        assay1_nospaces,
                                        ploidy1_nospaces,"txt",sep=".")
            
            
            pre_annotation_file_1 <- paste(input$individual1,tissue1_nospaces,
                                           assay1_nospaces,
                                           ploidy1_nospaces,input$chromosomes,"txt",sep=".")
            annotation_file_1 <- paste("annotation_files",
                                       pre_annotation_file_1,sep = "/")
            
            chr_chromosome_file_1 <- paste(input$chromosomes,pre_chromosome_file_1,sep = ".")
            
            chromosome_file_1 <- paste("chromosome_files",
                                       chr_chromosome_file_1,sep = "/")
            
            #########################################################################################
            ### IF ANNOTATION FILE ALREADY EXISTS, WE JUST GENERATE THE MAP BASED ON THE EXISTING ###
            ### FILE. IF NOT, THEN GENERATE THE NEEDED ANNOTATION FILE W/ THE FUNCTION            ###
            ### CHROMOSOME_FILTERER_WITHREGION.SH                                                 ###
            #########################################################################################
            
            if (file.exists(annotation_file_1)) {print("anotation file exists, generating plot ...")
            } else {
              
              chr_command <- paste("./chromosome_filterer_noregion.sh", input$chromosomes,
                                   chr_command_file_1)
              
              system(chr_command)
              
              print(chr_command)
            }
            
            if (input$plot_type == "Heat Map") {plot_type <- "heatmap"
            } else if (input$plot_type == "Histogram") { plot_type <- "histogram"
            } else if (input$plot_type == "Scatterplot") { plot_type <- "scatterplot"
            } else {print ("ERROR in  plot type.")}
            
            html_file_name = paste(annotation_file_1,
                                   input$heat_map_colors1,plot_type,"html",sep=".")
            html_file_name_copy = paste(pre_annotation_file_1,
                                        input$heat_map_colors1,plot_type,"html",sep=".")
            
            #######################################################################################
            ### IF THE HTML FILE NEEDED TO GENERATE THE SELECTED CHROMOMAP ALREADY EXISTS,      ###
            ### WE DON'T NEED TO GENERATE THE CHROMOMAP AGAIN, WE JUST LOAD THE EXISTING HTML   ###
            #######################################################################################
            
            if (file.exists(html_file_name)) {
              file.copy(html_file_name,"ENTEX_chromosome_visualization_download.html", overwrite = TRUE)
              #file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
              includeHTML(html_file_name)
              
            } else {
              
              if (plot_type == "heatmap") {
                
                html_output =  chromoMap(chromosome_file_1,annotation_file_1, 
                                         legend = "T",chr_width = 20
                                         , canvas_width = 2000,
                                         canvas_height = 3000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1)),
                                         chr_color = c("white"))
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
              } else if (plot_type == "histogram") {
                
                html_output =  chromoMap(chromosome_file_1,annotation_file_1, 
                                         legend = "T",chr_width = 20, 
                                         canvas_width = 2000,
                                         canvas_height = 3000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1)),
                                         chr_color = c("white"), plots = "bar", plot_color = c(input$heat_map_colors1)
                )
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
              } else if (plot_type == "scatterplot") { 
                
                html_output =  chromoMap(chromosome_file_1,annotation_file_1, 
                                         legend = "T",chr_width = 20, 
                                         canvas_width = 2000,
                                         canvas_height = 3000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1)),
                                         chr_color = c("white"), plots = "scatter", plot_color = c(input$heat_map_colors1))
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
              } else {print ("ERROR in  plot type.")}
            }
            
          } 
          
          #============================= SPECIFIED INPUT REGION =======================================#
          
          else {
            print("specified chr and specified region")
            
            annotation_command_file_1 <- paste(input$individual1,tissue1_nospaces,
                                               assay1_nospaces,
                                               ploidy1_nospaces,"bed",sep=".")
            chromosome_command_file_1 <- paste(input$chromosomes,pre_chromosome_file_1,sep = ".")
            
            
            pre_annotation_file_1 <- paste(input$individual1,tissue1_nospaces,
                                           assay1_nospaces,
                                           ploidy1_nospaces,input$chromosomes,input$region,"txt",sep=".")
            
            annotation_file_1 <- paste("annotation_files",
                                       pre_annotation_file_1,sep = "/")
            
            chr_chromosome_file_1 <- paste(input$region,input$chromosomes,pre_chromosome_file_1,sep = ".")
            
            chromosome_file_1 <- paste("chromosome_files",
                                       chr_chromosome_file_1,sep = "/")
            
            #########################################################################################
            ### IF ANNOTATION FILE ALREADY EXISTS, WE JUST GENERATE THE MAP BASED ON THE EXISTING ###
            ### FILE. IF NOT, THEN GENERATE THE NEEDED ANNOTATION FILE W/ THE FUNCTION            ###
            ### CHROMOSOME_FILTERER_WITHREGION.SH                                                 ###
            #########################################################################################
            
            
            if (file.exists(annotation_file_1)) {print("anotation file exists, generating plot ...")
            } else {
              
              chr_command <- paste("./chromosome_filterer_withregion.sh", input$chromosomes,input$region,
                                   chromosome_command_file_1,annotation_command_file_1)
              
              system(chr_command)
              
              print(chr_command)
            } 
            
            if (input$plot_type == "Heat Map") {plot_type <- "heatmap"
            } else if (input$plot_type == "Histogram") { plot_type <- "histogram"
            } else if (input$plot_type == "Scatterplot") { plot_type <- "scatterplot"
            } else {print ("ERROR in  plot type.")}
            
            html_file_name = paste(annotation_file_1,
                                   input$heat_map_colors1,plot_type,"html",sep=".")
            html_file_name_copy = paste(pre_annotation_file_1,
                                        input$heat_map_colors1,plot_type,"html",sep=".")
            
            #######################################################################################
            ### IF THE HTML FILE NEEDED TO GENERATE THE SELECTED CHROMOMAP ALREADY EXISTS,      ###
            ### WE DON'T NEED TO GENERATE THE CHROMOMAP AGAIN, WE JUST LOAD THE EXISTING HTML   ###
            #######################################################################################
            
            if (file.exists(html_file_name)) {
              file.copy(html_file_name,"ENTEX_chromosome_visualization_download.html", overwrite = TRUE)
              #file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
              includeHTML(html_file_name)
              
            } else {
              
              if (plot_type == "heatmap") {
                
                html_output =  chromoMap(chromosome_file_1,annotation_file_1, 
                                         legend = "T",chr_width = 20
                                         , canvas_width = 2000,
                                         canvas_height = 3000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1)),
                                         chr_color = c("white"))
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
              } else if (plot_type == "histogram") {
                
                html_output =  chromoMap(chromosome_file_1,annotation_file_1, 
                                         legend = "T",chr_width = 20, 
                                         canvas_width = 2000,
                                         canvas_height = 3000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1)),
                                         chr_color = c("white"), plots = "bar", plot_color = c(input$heat_map_colors1)
                )
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
              } else if (plot_type == "scatterplot") { 
                
                html_output =  chromoMap(chromosome_file_1,annotation_file_1, 
                                         legend = "T",chr_width = 20, 
                                         canvas_width = 2000,
                                         canvas_height = 3000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1)),
                                         chr_color = c("white"), plots = "scatter", plot_color = c(input$heat_map_colors1)
                                         )
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
              } else {print ("ERROR in  plot type.")}
            }
          }
          }
          
          ##########################################################################################
          ### BELOW IS A REPEAT OF LINES 313 - 599, BUT FOR TWO TRACKS. AGAIN, THE CONFIGURATION ###
          ### FOR TRACK 1 CODE IS REPEATED, SO THE BEHAVIOR FOR TRACK 1 GENERATION CAN BE        ###
          ### CUSTOMIZED WHEN MORE THAN ONE TRACK IS SELECTED                                    ###         
          ##########################################################################################
          
        } else if (input$number_of_tracks == 2) {
          
          print("2 tracks")
          
          if(input$individual2 == "ENC001") {pre_chromosome_file_2 <- "chromosome_file_XY.txt"
          } else if (input$individual2 == "ENC002") {print(pre_chromosome_file_2 <- "chromosome_file_XY.txt")
          } else if (input$individual2 == "ENC003") {print (pre_chromosome_file_2 <- "chromosome_file_XX.txt")
          } else if (input$individual2 == "ENC004") {print (pre_chromosome_file_2 <- "chromosome_file_XX.txt")
          } else {print ("ERROR")}
          
          chromosome_file_1 <-paste("chromosome_files",
                                    pre_chromosome_file_1,sep = "/")
          chromosome_file_2 <-paste("chromosome_files",
                                    pre_chromosome_file_2,sep = "/")
          
          input$heat_map_colors1
          tissue1_nospaces = str_replace_all(string=input$tissue1, pattern=" ",
                                             repl="")
          assay1_nospaces = str_replace_all(string=input$assay1, pattern=" ",
                                            repl="")
          ploidy1_nospaces = str_replace_all(string=input$ploidy1, pattern=" ",
                                             repl="")
          
          input$heat_map_colors2
          tissue2_nospaces = str_replace_all(string=input$tissue2, pattern=" ",
                                             repl="")
          assay2_nospaces = str_replace_all(string=input$assay2, pattern=" ",
                                            repl="")
          ploidy2_nospaces = str_replace_all(string=input$ploidy2, pattern=" ",
                                             repl="")
          
          if (input$chromosomes == "ALL") {print ("ALL CHROMOSOMES")
            
            pre_annotation_file_1 <- paste(input$individual1,tissue1_nospaces,
                                           assay1_nospaces,
                                           ploidy1_nospaces,"txt",sep=".")
            annotation_file_1 <- paste("annotation_files",
                                       pre_annotation_file_1,sep = "/")
            
            pre_annotation_file_2 <- paste(input$individual2,tissue2_nospaces,
                                           assay2_nospaces,
                                           ploidy2_nospaces,"txt",sep=".")
            annotation_file_2 <- paste("annotation_files",
                                       pre_annotation_file_2,sep = "/")
            
            if (input$plot_type == "Heat Map") {plot_type <- "heatmap"
            } else if (input$plot_type == "Histogram") { plot_type <- "histogram"
            } else if (input$plot_type == "Scatterplot") { plot_type <- "scatterplot"
            } else {print ("ERROR in  plot type.")}
            
            html_file_name = paste(annotation_file_1, input$heat_map_colors1, 
                                   pre_annotation_file_2, input$heat_map_colors2,
                                   plot_type,"html",sep=".")
            html_file_name_copy = paste(pre_annotation_file_1, input$heat_map_colors1, 
                                        pre_annotation_file_2, input$heat_map_colors2,
                                        plot_type,"html",sep=".")
            
            if (file.exists(html_file_name)) {
              file.copy(html_file_name,"ENTEX_chromosome_visualization_download.html", overwrite = TRUE)
              #file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
              includeHTML(html_file_name)
              
            } else {
              
              if (plot_type == "heatmap") {
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_2),
                                         c(annotation_file_1,annotation_file_2),
                                         ploidy=2,
                                         chr_width = 8, 
                                         chr_length = 6, canvas_width = 2000,
                                         canvas_height = 3000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2)),
                                         chr_color = c("white"))
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
              } else if (plot_type == "histogram") {
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_2),
                                         c(annotation_file_1,annotation_file_2),
                                         ploidy=2,
                                         chr_width = 8, 
                                         chr_length = 6, canvas_width = 2000,
                                         canvas_height = 3000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2)),
                                         chr_color = c("white"), plots = "bar", plot_color = c(input$heat_map_colors1, input$heat_map_colors2))
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
                
              } else if (plot_type == "scatterplot") { 
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_2),
                                         c(annotation_file_1,annotation_file_2),
                                         ploidy=2,
                                         chr_width = 8, 
                                         chr_length = 6, canvas_width = 2000,
                                         canvas_height = 3000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2)),
                                         chr_color = c("white"), plots = "scatter", plot_color = c(input$heat_map_colors1, input$heat_map_colors2)
                                         )
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
                
                
              } else {print ("ERROR in  plot type.")}
            }
            
          } else {test <- paste("This chromosome: ",input$chromosomes,sep = "") 
          print(test)
          if (input$region == "") {print ("specified chr and no region")
            
            chr_command_file_1 <- paste(input$individual1,tissue1_nospaces,
                                        assay1_nospaces,
                                        ploidy1_nospaces,"txt",sep=".")
            chr_command_file_2 <- paste(input$individual2,tissue2_nospaces,
                                        assay2_nospaces,
                                        ploidy2_nospaces,"txt",sep=".")
            
            pre_annotation_file_1 <- paste(input$individual1,tissue1_nospaces,
                                           assay1_nospaces,
                                           ploidy1_nospaces,input$chromosomes,"txt",sep=".")
            annotation_file_1 <- paste("annotation_files",
                                       pre_annotation_file_1,sep = "/")
            pre_annotation_file_2 <- paste(input$individual2,tissue2_nospaces,
                                           assay2_nospaces,
                                           ploidy2_nospaces,input$chromosomes,"txt",sep=".")
            annotation_file_2 <- paste("annotation_files",
                                       pre_annotation_file_2,sep = "/")
            
            chr_chromosome_file_1 <- paste(input$chromosomes,pre_chromosome_file_1,sep = ".")
            
            chromosome_file_1 <- paste("chromosome_files",
                                       chr_chromosome_file_1,sep = "/")
            
            if (file.exists(annotation_file_1)) {print("anotation file exists, generating plot ...")
            } else {
              
              chr_command <- paste("./chromosome_filterer_noregion.sh", input$chromosomes,
                                   chr_command_file_1)
              
              system(chr_command)
              
              print(chr_command)
              
            }
            
            if (file.exists(annotation_file_2)) {print("anotation file exists, generating plot ...")
            } else {
              
              chr_command <- paste("./chromosome_filterer_noregion.sh", input$chromosomes,
                                   chr_command_file_2)
              
              system(chr_command)
              
              print(chr_command)
              
            }
            
            if (input$plot_type == "Heat Map") {plot_type <- "heatmap"
            } else if (input$plot_type == "Histogram") { plot_type <- "histogram"
            } else if (input$plot_type == "Scatterplot") { plot_type <- "scatterplot"
            } else {print ("ERROR in  plot type.")}
            
            html_file_name = paste(annotation_file_1,input$heat_map_colors1,
                                   pre_annotation_file_2,input$heat_map_colors2,
                                   plot_type,"html",sep=".")
            html_file_name_copy = paste(pre_annotation_file_1,input$heat_map_colors1,
                                        pre_annotation_file_2,input$heat_map_colors2,
                                        plot_type,"html",sep=".")
            
            if (file.exists(html_file_name)) {
              file.copy(html_file_name,"ENTEX_chromosome_visualization_download.html", overwrite = TRUE)
              #file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
              includeHTML(html_file_name)
              
            } else {
              
              if (plot_type == "heatmap") {
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_1),
                                         c(annotation_file_1,annotation_file_2),
                                         ploidy=2, 
                                         legend = "T",chr_width = 20
                                         , canvas_width = 2000,
                                         canvas_height = 3000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2)),
                                         chr_color = c("white"))
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
              } else if (plot_type == "histogram") {
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_1),
                                         c(annotation_file_1,annotation_file_2),
                                         ploidy=2, 
                                         legend = "T",chr_width = 20, 
                                         canvas_width = 2000,
                                         canvas_height = 3000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2)),
                                         chr_color = c("white"), plots = "bar", plot_color = c(input$heat_map_colors1, input$heat_map_colors2)
                )
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
                
              } else if (plot_type == "scatterplot") { 
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_1),
                                         c(annotation_file_1,annotation_file_2),
                                         ploidy=2, 
                                         legend = "T",chr_width = 20, 
                                         canvas_width = 2000,
                                         canvas_height = 3000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2)),
                                         chr_color = c("white"), plots = "scatter", plot_color = c(input$heat_map_colors1, input$heat_map_colors2)
                                         )
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
              } else {print ("ERROR in  plot type.")}
            }
            
          } else {print("specified chr and specified region")
            
            annotation_command_file_1 <- paste(input$individual1,tissue1_nospaces,
                                               assay1_nospaces,
                                               ploidy1_nospaces,"bed",sep=".")
            chromosome_command_file_1 <- paste(input$chromosomes,pre_chromosome_file_1,sep = ".")
            annotation_command_file_2 <- paste(input$individual2,tissue2_nospaces,
                                               assay2_nospaces,
                                               ploidy2_nospaces,"bed",sep=".")
            chromosome_command_file_2 <- paste(input$chromosomes,pre_chromosome_file_2,sep = ".")
            
            
            
            pre_annotation_file_1 <- paste(input$individual1,tissue1_nospaces,
                                           assay1_nospaces,
                                           ploidy1_nospaces,input$chromosomes,input$region,"txt",sep=".")
            annotation_file_1 <- paste("annotation_files",
                                       pre_annotation_file_1,sep = "/")
            pre_annotation_file_2 <- paste(input$individual2,tissue2_nospaces,
                                           assay2_nospaces,
                                           ploidy2_nospaces,input$chromosomes,input$region,"txt",sep=".")
            annotation_file_2 <- paste("annotation_files",
                                       pre_annotation_file_2,sep = "/")
            
            chr_chromosome_file_1 <- paste(input$region,input$chromosomes,pre_chromosome_file_1,sep = ".")
            chromosome_file_1 <- paste("chromosome_files",
                                       chr_chromosome_file_1,sep = "/")
            chr_chromosome_file_2 <- paste(input$region,input$chromosomes,pre_chromosome_file_1,sep = ".")
            chromosome_file_2 <- paste("chromosome_files",
                                       chr_chromosome_file_2,sep = "/")
            
            
            if (file.exists(annotation_file_1)) {print("anotation file exists, generating plot ...")
            } else {
              
              chr_command <- paste("./chromosome_filterer_withregion.sh", input$chromosomes,input$region,
                                   chromosome_command_file_1,annotation_command_file_1)
              
              system(chr_command)
              
              print(chr_command)
              
            } 
            
            if (file.exists(annotation_file_2)) {print("anotation file exists, generating plot ...")
            } else {
              
              chr_command <- paste("./chromosome_filterer_withregion.sh", input$chromosomes,input$region,
                                   chromosome_command_file_1,annotation_command_file_2)
              
              system(chr_command)
              
              print(chr_command)
              
            } 
            
            if (input$plot_type == "Heat Map") {plot_type <- "heatmap"
            } else if (input$plot_type == "Histogram") { plot_type <- "histogram"
            } else if (input$plot_type == "Scatterplot") { plot_type <- "scatterplot"
            } else {print ("ERROR in  plot type.")}
            
            html_file_name = paste(annotation_file_1,input$heat_map_colors1,
                                   pre_annotation_file_2,input$heat_map_colors2,
                                   plot_type,"html",sep=".")
            html_file_name_copy = paste(pre_annotation_file_1,input$heat_map_colors1,
                                        pre_annotation_file_2,input$heat_map_colors2,
                                        plot_type,"html",sep=".")
            
            if (file.exists(html_file_name)) {
              file.copy(html_file_name,"ENTEX_chromosome_visualization_download.html", overwrite = TRUE)
              #file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
              includeHTML(html_file_name)
              
            } else {
              
              if (plot_type == "heatmap") {
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_1),
                                         c(annotation_file_1,annotation_file_2),
                                         ploidy=2, 
                                         legend = "T",chr_width = 20, 
                                         canvas_width = 2000,
                                         canvas_height = 3000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2)),
                                         chr_color = c("white"))
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
              } else if (plot_type == "histogram") {
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_1),
                                         c(annotation_file_1,annotation_file_2),
                                         ploidy=2, 
                                         legend = "T",chr_width = 20, 
                                         canvas_width = 2000,
                                         canvas_height = 3000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2)),
                                         chr_color = c("white"), plots = "bar", plot_color = c(input$heat_map_colors1, input$heat_map_colors2)
                )
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
                
              } else if (plot_type == "scatterplot") { 
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_1),
                                         c(annotation_file_1,annotation_file_2),
                                         ploidy=2, 
                                         legend = "T",chr_width = 20, 
                                         canvas_width = 2000,
                                         canvas_height = 3000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2)),
                                         chr_color = c("white"), plots = "scatter", plot_color = c(input$heat_map_colors1, input$heat_map_colors2)
                                         )
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
                
                
              } else {print ("ERROR in  plot type.")}
            }
          }
          }
          
          ############################################################################################
          ### BELOW IS A REPEAT OF LINES 606 - 988, BUT FOR THREE TRACKS. AGAIN, THE CONFIGURATION ###
          ### FOR TRACK 1 AND 2 CODE IS REPEATED, SO THE BEHAVIOR FOR TRACK GENERATION CAN BE      ###
          ### CUSTOMIZED WHEN MORE THAN ONE TRACK IS SELECTED                                      ###         
          ############################################################################################
          
        } else if (input$number_of_tracks == 3) {
          
          print("3 tracks")
          
          chromosome_file_1 <-paste("chromosome_files",
                                    pre_chromosome_file_1,sep = "/")
          if(input$individual2 == "ENC001") {pre_chromosome_file_2 <- "chromosome_file_XY.txt"
          } else if (input$individual2 == "ENC002") {print(pre_chromosome_file_2 <- "chromosome_file_XY.txt")
          } else if (input$individual2 == "ENC003") {print (pre_chromosome_file_2 <- "chromosome_file_XX.txt")
          } else if (input$individual2 == "ENC004") {print (pre_chromosome_file_2 <- "chromosome_file_XX.txt")
          } else {print ("ERROR")}
          chromosome_file_2 <-paste("chromosome_files",
                                    pre_chromosome_file_2,sep = "/")
          if(input$individual3 == "ENC001") {pre_chromosome_file_3 <- "chromosome_file_XY.txt"
          } else if (input$individual3 == "ENC002") {print(pre_chromosome_file_3 <- "chromosome_file_XY.txt")
          } else if (input$individual3 == "ENC003") {print (pre_chromosome_file_3 <- "chromosome_file_XX.txt")
          } else if (input$individual3 == "ENC004") {print (pre_chromosome_file_3 <- "chromosome_file_XX.txt")
          } else {print ("ERROR")}
          chromosome_file_3 <-paste("chromosome_files",
                                    pre_chromosome_file_3,sep = "/")
          
          input$heat_map_colors1
          tissue1_nospaces = str_replace_all(string=input$tissue1, pattern=" ",
                                             repl="")
          assay1_nospaces = str_replace_all(string=input$assay1, pattern=" ",
                                            repl="")
          ploidy1_nospaces = str_replace_all(string=input$ploidy1, pattern=" ",
                                             repl="")
          input$heat_map_colors2
          tissue2_nospaces = str_replace_all(string=input$tissue2, pattern=" ",
                                             repl="")
          assay2_nospaces = str_replace_all(string=input$assay2, pattern=" ",
                                            repl="")
          ploidy2_nospaces = str_replace_all(string=input$ploidy2, pattern=" ",
                                             repl="")
          input$heat_map_colors3
          tissue3_nospaces = str_replace_all(string=input$tissue3, pattern=" ",
                                             repl="")
          assay3_nospaces = str_replace_all(string=input$assay3, pattern=" ",
                                            repl="")
          ploidy3_nospaces = str_replace_all(string=input$ploidy3, pattern=" ",
                                             repl="")
          
          
          
          if (input$chromosomes == "ALL") {print ("ALL CHROMOSOMES")
            
            
            
            pre_annotation_file_1 <- paste(input$individual1,tissue1_nospaces,
                                           assay1_nospaces,
                                           ploidy1_nospaces,"txt",sep=".")
            annotation_file_1 <- paste("annotation_files",
                                       pre_annotation_file_1,sep = "/")
            
            pre_annotation_file_2 <- paste(input$individual2,tissue2_nospaces,
                                           assay2_nospaces,
                                           ploidy2_nospaces,"txt",sep=".")
            annotation_file_2 <- paste("annotation_files",
                                       pre_annotation_file_2,sep = "/")
            pre_annotation_file_3 <- paste(input$individual3,tissue3_nospaces,
                                           assay3_nospaces,
                                           ploidy3_nospaces,"txt",sep=".")
            annotation_file_3 <- paste("annotation_files",
                                       pre_annotation_file_3,sep = "/")
            
            
            
            if (input$plot_type == "Heat Map") {plot_type <- "heatmap"
            } else if (input$plot_type == "Histogram") { plot_type <- "histogram"
            } else if (input$plot_type == "Scatterplot") { plot_type <- "scatterplot"
            } else {print ("ERROR in  plot type.")}
            
            html_file_name = paste(annotation_file_1, input$heat_map_colors1, 
                                   pre_annotation_file_2, input$heat_map_colors2,
                                   pre_annotation_file_3, input$heat_map_colors3,
                                   plot_type,"html",sep=".")
            html_file_name_copy = paste(pre_annotation_file_1, input$heat_map_colors1, 
                                        pre_annotation_file_2, input$heat_map_colors2,
                                        pre_annotation_file_3, input$heat_map_colors3,
                                        plot_type,"html",sep=".")
            
            
            if (file.exists(html_file_name)) {
              file.copy(html_file_name,"ENTEX_chromosome_visualization_download.html", overwrite = TRUE)
              #file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
              includeHTML(html_file_name)
              
            } else {
              
              if (plot_type == "heatmap") {
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_2,
                                           chromosome_file_3),
                                         c(annotation_file_1,annotation_file_2,
                                           annotation_file_3),
                                         ploidy=3,
                                         chr_width = 8, 
                                         chr_length = 6, canvas_width = 2000,
                                         canvas_height = 4000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2),
                                                            c("white",input$heat_map_colors3)),
                                         chr_color = c("white"))
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
              } else if (plot_type == "histogram") {
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_2,
                                           chromosome_file_3),
                                         c(annotation_file_1,annotation_file_2,
                                           annotation_file_3),
                                         ploidy=3,
                                         chr_width = 8, 
                                         chr_length = 6, canvas_width = 2000,
                                         canvas_height = 4000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2),
                                                            c("white",input$heat_map_colors3)),
                                         chr_color = c("white"), plots = "bar", plot_color = c(input$heat_map_colors1, input$heat_map_colors2, input$heat_map_colors3)
                )
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
                
              } else if (plot_type == "scatterplot") { 
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_2,
                                           chromosome_file_3),
                                         c(annotation_file_1,annotation_file_2,
                                           annotation_file_3),
                                         ploidy=3,
                                         chr_width = 8, 
                                         chr_length = 6, canvas_width = 2000,
                                         canvas_height = 4000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2),
                                                            c("white",input$heat_map_colors3)),
                                         chr_color = c("white"), plots = "scatter", plot_color = c(input$heat_map_colors1, input$heat_map_colors2, input$heat_map_colors3)
                                         )
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
                
                
              } else {print ("ERROR in  plot type.")}
            }
            
            
          } else {test <- paste("This chromosome: ",input$chromosomes,sep = "") 
          print(test)
          if (input$region == "") {print ("specified chr and no region")
            
            chr_command_file_1 <- paste(input$individual1,tissue1_nospaces,
                                        assay1_nospaces,
                                        ploidy1_nospaces,"txt",sep=".")
            chr_command_file_2 <- paste(input$individual2,tissue2_nospaces,
                                        assay2_nospaces,
                                        ploidy2_nospaces,"txt",sep=".")
            chr_command_file_3 <- paste(input$individual3,tissue3_nospaces,
                                        assay3_nospaces,
                                        ploidy3_nospaces,"txt",sep=".")
            
            
            
            
            pre_annotation_file_1 <- paste(input$individual1,tissue1_nospaces,
                                           assay1_nospaces,
                                           ploidy1_nospaces,input$chromosomes,"txt",sep=".")
            annotation_file_1 <- paste("annotation_files",
                                       pre_annotation_file_1,sep = "/")
            pre_annotation_file_2 <- paste(input$individual2,tissue2_nospaces,
                                           assay2_nospaces,
                                           ploidy2_nospaces,input$chromosomes,"txt",sep=".")
            annotation_file_2 <- paste("annotation_files",
                                       pre_annotation_file_2,sep = "/")
            pre_annotation_file_3 <- paste(input$individual3,tissue3_nospaces,
                                           assay3_nospaces,
                                           ploidy3_nospaces,input$chromosomes,"txt",sep=".")
            annotation_file_3 <- paste("annotation_files",
                                       pre_annotation_file_3,sep = "/")
            
            
            
            chr_chromosome_file_1 <- paste(input$chromosomes,pre_chromosome_file_1,sep = ".")
            chromosome_file_1 <- paste("chromosome_files",
                                       chr_chromosome_file_1,sep = "/")
            
            if (file.exists(annotation_file_1)) {print("anotation file exists, generating plot ...")
            } else {
              
              chr_command <- paste("./chromosome_filterer_noregion.sh", input$chromosomes,
                                   chr_command_file_1)
              
              system(chr_command)
              
              print(chr_command)
              
            }
            
            if (file.exists(annotation_file_2)) {print("anotation file exists, generating plot ...")
            } else {
              
              chr_command <- paste("./chromosome_filterer_noregion.sh", input$chromosomes,
                                   chr_command_file_2)
              
              system(chr_command)
              
              print(chr_command)
              
            }
            
            if (file.exists(annotation_file_3)) {print("anotation file exists, generating plot ...")
            } else {
              
              chr_command <- paste("./chromosome_filterer_noregion.sh", input$chromosomes,
                                   chr_command_file_3)
              
              system(chr_command)
              
              print(chr_command)
              
            }
            
            if (input$plot_type == "Heat Map") {plot_type <- "heatmap"
            } else if (input$plot_type == "Histogram") { plot_type <- "histogram"
            } else if (input$plot_type == "Scatterplot") { plot_type <- "scatterplot"
            } else {print ("ERROR in  plot type.")}
            
            html_file_name = paste(annotation_file_1,input$heat_map_colors1,
                                   pre_annotation_file_2,input$heat_map_colors2,
                                   pre_annotation_file_3,input$heat_map_colors3,
                                   plot_type,"html",sep=".")
            html_file_name_copy = paste(pre_annotation_file_1,input$heat_map_colors1,
                                        pre_annotation_file_2,input$heat_map_colors2,
                                        pre_annotation_file_3,input$heat_map_colors3,
                                        plot_type,"html",sep=".")
            
            if (file.exists(html_file_name)) {
              file.copy(html_file_name,"ENTEX_chromosome_visualization_download.html", overwrite = TRUE)
              #file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
              includeHTML(html_file_name)
              
            } else {
              
              if (plot_type == "heatmap") {
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_1,
                                           chromosome_file_1),
                                         c(annotation_file_1,annotation_file_2,
                                           annotation_file_3),
                                         ploidy=3, 
                                         legend = "T",chr_width = 20
                                         , canvas_width = 2000,
                                         canvas_height = 1000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2),
                                                            c("white",input$heat_map_colors3)),
                                         chr_color = c("white"))
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
              } else if (plot_type == "histogram") {
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_1,
                                           chromosome_file_1),
                                         c(annotation_file_1,annotation_file_2,
                                           annotation_file_3),
                                         ploidy=3, 
                                         legend = "T",chr_width = 20
                                         , canvas_width = 2000,
                                         canvas_height = 1000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2),
                                                            c("white",input$heat_map_colors3)),
                                         chr_color = c("white"), plots = "bar", plot_color = c(input$heat_map_colors1, input$heat_map_colors2, input$heat_map_colors3)
                )
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
                
              } else if (plot_type == "scatterplot") { 
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_1,
                                           chromosome_file_1),
                                         c(annotation_file_1,annotation_file_2,
                                           annotation_file_3),
                                         ploidy=3, 
                                         legend = "T",chr_width = 20
                                         , canvas_width = 2000,
                                         canvas_height = 1000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2),
                                                            c("white",input$heat_map_colors3)),
                                         chr_color = c("white"), plots = "scatter", plot_color = c(input$heat_map_colors1, input$heat_map_colors2, input$heat_map_colors3)
                                         )
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
              } else {print ("ERROR in  plot type.")}
            }
            
            
          } else {print("specified chr and specified region")
            
            annotation_command_file_1 <- paste(input$individual1,tissue1_nospaces,
                                               assay1_nospaces,
                                               ploidy1_nospaces,"bed",sep=".")
            chromosome_command_file_1 <- paste(input$chromosomes,pre_chromosome_file_1,sep = ".")
            annotation_command_file_2 <- paste(input$individual2,tissue2_nospaces,
                                               assay2_nospaces,
                                               ploidy2_nospaces,"bed",sep=".")
            chromosome_command_file_2 <- paste(input$chromosomes,pre_chromosome_file_2,sep = ".")
            annotation_command_file_3 <- paste(input$individual3,tissue3_nospaces,
                                               assay3_nospaces,
                                               ploidy3_nospaces,"bed",sep=".")
            chromosome_command_file_3 <- paste(input$chromosomes,pre_chromosome_file_3,sep = ".")
            
            
            
            pre_annotation_file_1 <- paste(input$individual1,tissue1_nospaces,
                                           assay1_nospaces,
                                           ploidy1_nospaces,input$chromosomes,input$region,"txt",sep=".")
            annotation_file_1 <- paste("annotation_files",
                                       pre_annotation_file_1,sep = "/")
            pre_annotation_file_2 <- paste(input$individual2,tissue2_nospaces,
                                           assay2_nospaces,
                                           ploidy2_nospaces,input$chromosomes,input$region,"txt",sep=".")
            annotation_file_2 <- paste("annotation_files",
                                       pre_annotation_file_2,sep = "/")
            pre_annotation_file_3 <- paste(input$individual3,tissue3_nospaces,
                                           assay3_nospaces,
                                           ploidy3_nospaces,input$chromosomes,input$region,"txt",sep=".")
            annotation_file_3 <- paste("annotation_files",
                                       pre_annotation_file_3,sep = "/")
            
            
            
            chr_chromosome_file_1 <- paste(input$region,input$chromosomes,pre_chromosome_file_1,sep = ".")
            chromosome_file_1 <- paste("chromosome_files",
                                       chr_chromosome_file_1,sep = "/")
            chr_chromosome_file_2 <- paste(input$region,input$chromosomes,pre_chromosome_file_1,sep = ".")
            chromosome_file_2 <- paste("chromosome_files",
                                       chr_chromosome_file_2,sep = "/")
            chr_chromosome_file_3 <- paste(input$region,input$chromosomes,pre_chromosome_file_1,sep = ".")
            chromosome_file_3 <- paste("chromosome_files",
                                       chr_chromosome_file_3,sep = "/")
            
            
            if (file.exists(annotation_file_1)) {print("anotation file exists, generating plot ...")
            } else {
              
              chr_command <- paste("./chromosome_filterer_withregion.sh", input$chromosomes,input$region,
                                   chromosome_command_file_1,annotation_command_file_1)
              
              system(chr_command)
              
              print(chr_command)
            } 
            
            if (file.exists(annotation_file_2)) {print("anotation file exists, generating plot ...")
            } else {
              
              chr_command <- paste("./chromosome_filterer_withregion.sh", input$chromosomes,input$region,
                                   chromosome_command_file_1,annotation_command_file_2)
              
              system(chr_command)
              
              print(chr_command)
              
            } 
            
            if (file.exists(annotation_file_3)) {print("anotation file exists, generating plot ...")
            } else {
              
              chr_command <- paste("./chromosome_filterer_withregion.sh", input$chromosomes,input$region,
                                   chromosome_command_file_1,annotation_command_file_3)
              
              system(chr_command)
              
              print(chr_command)
              
            } 
            
            if (input$plot_type == "Heat Map") {plot_type <- "heatmap"
            } else if (input$plot_type == "Histogram") { plot_type <- "histogram"
            } else if (input$plot_type == "Scatterplot") { plot_type <- "scatterplot"
            } else {print ("ERROR in  plot type.")}
            
            html_file_name = paste(annotation_file_1,input$heat_map_colors1,
                                   pre_annotation_file_2,input$heat_map_colors2,
                                   pre_annotation_file_3,input$heat_map_colors3,
                                   plot_type,"html",sep=".")
            html_file_name_copy = paste(pre_annotation_file_1,input$heat_map_colors1,
                                        pre_annotation_file_2,input$heat_map_colors2,
                                        pre_annotation_file_3,input$heat_map_colors3,
                                        plot_type,"html",sep=".")
            
            if (file.exists(html_file_name)) {
              file.copy(html_file_name,"ENTEX_chromosome_visualization_download.html", overwrite = TRUE)
              #file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
              includeHTML(html_file_name)
              
            } else {
              
              if (plot_type == "heatmap") {
                print(chromosome_command_file_1)
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_1,
                                           chromosome_file_1),
                                         c(annotation_file_1,annotation_file_2,
                                           annotation_file_3),
                                         ploidy=3, 
                                         legend = "T",chr_width = 20, 
                                         canvas_width = 2000,
                                         canvas_height = 4000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2),
                                                            c("white",input$heat_map_colors3)),
                                         chr_color = c("white"))
                
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
              } else if (plot_type == "histogram") {
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_1,
                                           chromosome_command_file_3),
                                         c(annotation_file_1,annotation_file_2,
                                           annotation_file_3),
                                         ploidy=3, 
                                         legend = "T",chr_width = 20, 
                                         canvas_width = 2000,
                                         canvas_height = 4000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2),
                                                            c("white",input$heat_map_colors3)),
                                         chr_color = c("white"), plots = "bar", plot_color = c(input$heat_map_colors1, input$heat_map_colors2, input$heat_map_colors3)
                )
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
                
              } else if (plot_type == "scatterplot") { 
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_1,
                                           chromosome_command_file_3),
                                         c(annotation_file_1,annotation_file_2,
                                           annotation_file_3),
                                         ploidy=3, 
                                         legend = "T",chr_width = 20, 
                                         canvas_width = 2000,
                                         canvas_height = 4000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2),
                                                            c("white",input$heat_map_colors3)),
                                         chr_color = c("white"), plots = "scatter", plot_color = c(input$heat_map_colors1, input$heat_map_colors2, input$heat_map_colors3)
                                         )
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
                
                
              } else {print ("ERROR in  plot type.")}
            }
            
          }
          }
          
          #############################################################################################
          ### BELOW IS A REPEAT OF LINES 996 - 1487, BUT FOR FOUR TRACKS. AGAIN, THE CONFIGURATION  ###
          ### FOR TRACK 1, 2, AND 3 CODE IS REPEATED, SO THE BEHAVIOR FOR TRACK GENERATION CAN BE   ###
          ### CUSTOMIZED WHEN MORE THAN ONE TRACK IS SELECTED                                       ###         
          #############################################################################################
          
        } else if (input$number_of_tracks == 4) {
          
          print("4 tracks")
          
          chromosome_file_1 <-paste("chromosome_files",
                                    pre_chromosome_file_1,sep = "/")
          if(input$individual2 == "ENC001") {pre_chromosome_file_2 <- "chromosome_file_XY.txt"
          } else if (input$individual2 == "ENC002") {print(pre_chromosome_file_2 <- "chromosome_file_XY.txt")
          } else if (input$individual2 == "ENC003") {print (pre_chromosome_file_2 <- "chromosome_file_XX.txt")
          } else if (input$individual2 == "ENC004") {print (pre_chromosome_file_2 <- "chromosome_file_XX.txt")
          } else {print ("ERROR")}
          chromosome_file_2 <-paste("chromosome_files",
                                    pre_chromosome_file_2,sep = "/")
          if(input$individual3 == "ENC001") {pre_chromosome_file_3 <- "chromosome_file_XY.txt"
          } else if (input$individual3 == "ENC002") {print(pre_chromosome_file_3 <- "chromosome_file_XY.txt")
          } else if (input$individual3 == "ENC003") {print (pre_chromosome_file_3 <- "chromosome_file_XX.txt")
          } else if (input$individual3 == "ENC004") {print (pre_chromosome_file_3 <- "chromosome_file_XX.txt")
          } else {print ("ERROR")}
          chromosome_file_3 <-paste("chromosome_files",
                                    pre_chromosome_file_3,sep = "/")
          if(input$individual4 == "ENC001") {pre_chromosome_file_4 <- "chromosome_file_XY.txt"
          } else if (input$individual4 == "ENC002") {print(pre_chromosome_file_4 <- "chromosome_file_XY.txt")
          } else if (input$individual4 == "ENC003") {print (pre_chromosome_file_4 <- "chromosome_file_XX.txt")
          } else if (input$individual4 == "ENC004") {print (pre_chromosome_file_4 <- "chromosome_file_XX.txt")
          } else {print ("ERROR")}
          chromosome_file_4 <-paste("chromosome_files",
                                    pre_chromosome_file_4,sep = "/")
          
          input$heat_map_colors1
          tissue1_nospaces = str_replace_all(string=input$tissue1, pattern=" ",
                                             repl="")
          assay1_nospaces = str_replace_all(string=input$assay1, pattern=" ",
                                            repl="")
          ploidy1_nospaces = str_replace_all(string=input$ploidy1, pattern=" ",
                                             repl="")
          input$heat_map_colors2
          tissue2_nospaces = str_replace_all(string=input$tissue2, pattern=" ",
                                             repl="")
          assay2_nospaces = str_replace_all(string=input$assay2, pattern=" ",
                                            repl="")
          ploidy2_nospaces = str_replace_all(string=input$ploidy2, pattern=" ",
                                             repl="")
          input$heat_map_colors3
          tissue3_nospaces = str_replace_all(string=input$tissue3, pattern=" ",
                                             repl="")
          assay3_nospaces = str_replace_all(string=input$assay3, pattern=" ",
                                            repl="")
          ploidy3_nospaces = str_replace_all(string=input$ploidy3, pattern=" ",
                                             repl="")
          input$heat_map_colors4
          tissue4_nospaces = str_replace_all(string=input$tissue4, pattern=" ",
                                             repl="")
          assay4_nospaces = str_replace_all(string=input$assay4, pattern=" ",
                                            repl="")
          ploidy4_nospaces = str_replace_all(string=input$ploidy4, pattern=" ",
                                             repl="")
          
          
          
          if (input$chromosomes == "ALL") {print ("ALL CHROMOSOMES")
            
            
            
            pre_annotation_file_1 <- paste(input$individual1,tissue1_nospaces,
                                           assay1_nospaces,
                                           ploidy1_nospaces,"txt",sep=".")
            annotation_file_1 <- paste("annotation_files",
                                       pre_annotation_file_1,sep = "/")
            pre_annotation_file_2 <- paste(input$individual2,tissue2_nospaces,
                                           assay2_nospaces,
                                           ploidy2_nospaces,"txt",sep=".")
            annotation_file_2 <- paste("annotation_files",
                                       pre_annotation_file_2,sep = "/")
            pre_annotation_file_3 <- paste(input$individual3,tissue3_nospaces,
                                           assay3_nospaces,
                                           ploidy3_nospaces,"txt",sep=".")
            annotation_file_3 <- paste("annotation_files",
                                       pre_annotation_file_3,sep = "/")
            pre_annotation_file_4 <- paste(input$individual4,tissue4_nospaces,
                                           assay4_nospaces,
                                           ploidy4_nospaces,"txt",sep=".")
            annotation_file_4 <- paste("annotation_files",
                                       pre_annotation_file_4,sep = "/")
            
            
            
            if (input$plot_type == "Heat Map") {plot_type <- "heatmap"
            } else if (input$plot_type == "Histogram") { plot_type <- "histogram"
            } else if (input$plot_type == "Scatterplot") { plot_type <- "scatterplot"
            } else {print ("ERROR in  plot type.")}
            
            html_file_name = paste(annotation_file_1, input$heat_map_colors1, 
                                   pre_annotation_file_2, input$heat_map_colors2,
                                   pre_annotation_file_3, input$heat_map_colors3,
                                   pre_annotation_file_4, input$heat_map_colors4,
                                   plot_type,"html",sep=".")
            html_file_name_copy = paste(pre_annotation_file_1, input$heat_map_colors1, 
                                        pre_annotation_file_2, input$heat_map_colors2,
                                        pre_annotation_file_3, input$heat_map_colors3,
                                        pre_annotation_file_4, input$heat_map_colors4,
                                        plot_type,"html",sep=".")
            
            
            if (file.exists(html_file_name)) {
              file.copy(html_file_name,"ENTEX_chromosome_visualization_download.html", overwrite = TRUE)
              #file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
              includeHTML(html_file_name)
              
            } else {
              
              if (plot_type == "heatmap") {
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_2,
                                           chromosome_file_3,chromosome_file_4),
                                         c(annotation_file_1,annotation_file_2,
                                           annotation_file_3,annotation_file_4),
                                         ploidy=4,
                                         chr_width = 8, 
                                         chr_length = 6, canvas_width = 2000,
                                         canvas_height = 5000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2),
                                                            c("white",input$heat_map_colors3),
                                                            c("white",input$heat_map_colors4)),
                                         chr_color = c("white"))
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
              } else if (plot_type == "histogram") {
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_2,
                                           chromosome_file_3,chromosome_file_4),
                                         c(annotation_file_1,annotation_file_2,
                                           annotation_file_3,annotation_file_4),
                                         ploidy=4,
                                         chr_width = 8, 
                                         chr_length = 6, canvas_width = 2000,
                                         canvas_height = 4000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2),
                                                            c("white",input$heat_map_colors3),
                                                            c("white",input$heat_map_colors4)),
                                         chr_color = c("white"), plots = "bar", plot_color = c(input$heat_map_colors1, input$heat_map_colors2, input$heat_map_colors3, input$heat_map_colors4)
                )
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
                
              } else if (plot_type == "scatterplot") { 
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_2,
                                           chromosome_file_3,chromosome_file_4),
                                         c(annotation_file_1,annotation_file_2,
                                           annotation_file_3,annotation_file_4),
                                         ploidy=4,
                                         chr_width = 8, 
                                         chr_length = 6, canvas_width = 2000,
                                         canvas_height = 4000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2),
                                                            c("white",input$heat_map_colors3),
                                                            c("white",input$heat_map_colors4)),
                                         chr_color = c("white"), plots = "scatter", plot_color = c(input$heat_map_colors1, input$heat_map_colors2, input$heat_map_colors3, input$heat_map_colors4))
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
              } else {print ("ERROR in  plot type.")}
            }
            
          } else {test <- paste("This chromosome: ",input$chromosomes,sep = "") 
          print(test)
          if (input$region == "") {print ("specified chr and no region")
            
            chr_command_file_1 <- paste(input$individual1,tissue1_nospaces,
                                        assay1_nospaces,
                                        ploidy1_nospaces,"txt",sep=".")
            chr_command_file_2 <- paste(input$individual2,tissue2_nospaces,
                                        assay2_nospaces,
                                        ploidy2_nospaces,"txt",sep=".")
            chr_command_file_3 <- paste(input$individual3,tissue3_nospaces,
                                        assay3_nospaces,
                                        ploidy3_nospaces,"txt",sep=".")
            chr_command_file_4 <- paste(input$individual4,tissue4_nospaces,
                                        assay4_nospaces,
                                        ploidy4_nospaces,"txt",sep=".")
            
            
            
            pre_annotation_file_1 <- paste(input$individual1,tissue1_nospaces,
                                           assay1_nospaces,
                                           ploidy1_nospaces,input$chromosomes,"txt",sep=".")
            annotation_file_1 <- paste("annotation_files",
                                       pre_annotation_file_1,sep = "/")
            pre_annotation_file_2 <- paste(input$individual2,tissue2_nospaces,
                                           assay2_nospaces,
                                           ploidy2_nospaces,input$chromosomes,"txt",sep=".")
            annotation_file_2 <- paste("annotation_files",
                                       pre_annotation_file_2,sep = "/")
            pre_annotation_file_3 <- paste(input$individual3,tissue3_nospaces,
                                           assay3_nospaces,
                                           ploidy3_nospaces,input$chromosomes,"txt",sep=".")
            annotation_file_3 <- paste("annotation_files",
                                       pre_annotation_file_3,sep = "/")
            pre_annotation_file_4 <- paste(input$individual3,tissue3_nospaces,
                                           assay3_nospaces,
                                           ploidy3_nospaces,input$chromosomes,"txt",sep=".")
            annotation_file_4 <- paste("annotation_files",
                                       pre_annotation_file_3,sep = "/")
            
            
            
            chr_chromosome_file_1 <- paste(input$chromosomes,pre_chromosome_file_1,sep = ".")
            chromosome_file_1 <- paste("chromosome_files",
                                       chr_chromosome_file_1,sep = "/")
            
            if (file.exists(annotation_file_1)) {print("anotation file exists, generating plot ...")
            } else {
              
              chr_command <- paste("./chromosome_filterer_noregion.sh", input$chromosomes,
                                   chr_command_file_1)
              
              system(chr_command)
              
              print(chr_command)
              
            }
            
            if (file.exists(annotation_file_2)) {print("anotation file exists, generating plot ...")
            } else {
              
              chr_command <- paste("./chromosome_filterer_noregion.sh", input$chromosomes,
                                   chr_command_file_2)
              
              system(chr_command)
              
              print(chr_command)
              
            }
            
            if (file.exists(annotation_file_3)) {print("anotation file exists, generating plot ...")
            } else {
              
              chr_command <- paste("./chromosome_filterer_noregion.sh", input$chromosomes,
                                   chr_command_file_3)
              
              system(chr_command)
              
              print(chr_command)
              
            }
            
            if (file.exists(annotation_file_4)) {print("anotation file exists, generating plot ...")
            } else {
              
              chr_command <- paste("./chromosome_filterer_noregion.sh", input$chromosomes,
                                   chr_command_file_4)
              
              system(chr_command)
              
              print(chr_command)
              
            }
            
            
            if (input$plot_type == "Heat Map") {plot_type <- "heatmap"
            } else if (input$plot_type == "Histogram") { plot_type <- "histogram"
            } else if (input$plot_type == "Scatterplot") { plot_type <- "scatterplot"
            } else {print ("ERROR in  plot type.")}
            
            html_file_name = paste(annotation_file_1,input$heat_map_colors1,
                                   pre_annotation_file_2,input$heat_map_colors2,
                                   pre_annotation_file_3,input$heat_map_colors3,
                                   pre_annotation_file_4,input$heat_map_colors4,
                                   plot_type,"html",sep=".")
            html_file_name_copy = paste(pre_annotation_file_1,input$heat_map_colors1,
                                        pre_annotation_file_2,input$heat_map_colors2,
                                        pre_annotation_file_3,input$heat_map_colors3,
                                        pre_annotation_file_4,input$heat_map_colors4,
                                        plot_type,"html",sep=".")
            
            if (file.exists(html_file_name)) {
              file.copy(html_file_name,"ENTEX_chromosome_visualization_download.html", overwrite = TRUE)
              #file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
              includeHTML(html_file_name)
              
            } else {
              
              if (plot_type == "heatmap") {
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_1,
                                           chromosome_file_1,chromosome_file_1),
                                         c(annotation_file_1,annotation_file_2,
                                           annotation_file_3,annotation_file_4),
                                         ploidy=4, 
                                         legend = "T",chr_width = 20
                                         , canvas_width = 2000,
                                         canvas_height = 1000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2),
                                                            c("white",input$heat_map_colors3),
                                                            c("white",input$heat_map_colors4)),
                                         chr_color = c("white"))
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
              } else if (plot_type == "histogram") {
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_1,
                                           chromosome_file_1,chromosome_file_1),
                                         c(annotation_file_1,annotation_file_2,
                                           annotation_file_3,annotation_file_4),
                                         ploidy=4, 
                                         legend = "T",chr_width = 20
                                         , canvas_width = 2000,
                                         canvas_height = 1000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2),
                                                            c("white",input$heat_map_colors3),
                                                            c("white",input$heat_map_colors4)),
                                         chr_color = c("white"), plots = "bar", plot_color = c(input$heat_map_colors1, input$heat_map_colors2, input$heat_map_colors3, input$heat_map_colors4)
                )
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
                
              } else if (plot_type == "scatterplot") { 
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_1,
                                           chromosome_file_1,chromosome_file_1),
                                         c(annotation_file_1,annotation_file_2,
                                           annotation_file_3,annotation_file_4),
                                         ploidy=4, 
                                         legend = "T",chr_width = 20
                                         , canvas_width = 2000,
                                         canvas_height = 1000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2),
                                                            c("white",input$heat_map_colors3),
                                                            c("white",input$heat_map_colors4)),
                                         chr_color = c("white"), plots = "scatter", plot_color = c(input$heat_map_colors1, input$heat_map_colors2, input$heat_map_colors3, input$heat_map_colors4)
                                         )
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
                
                
              } else {print ("ERROR in  plot type.")}
            }
            
            
          } else {print("specified chr and specified region")
            
            annotation_command_file_1 <- paste(input$individual1,tissue1_nospaces,
                                               assay1_nospaces,
                                               ploidy1_nospaces,"bed",sep=".")
            chromosome_command_file_1 <- paste(input$chromosomes,pre_chromosome_file_1,sep = ".")
            annotation_command_file_2 <- paste(input$individual2,tissue2_nospaces,
                                               assay2_nospaces,
                                               ploidy2_nospaces,"bed",sep=".")
            chromosome_command_file_2 <- paste(input$chromosomes,pre_chromosome_file_2,sep = ".")
            annotation_command_file_3 <- paste(input$individual3,tissue3_nospaces,
                                               assay3_nospaces,
                                               ploidy3_nospaces,"bed",sep=".")
            chromosome_command_file_3 <- paste(input$chromosomes,pre_chromosome_file_3,sep = ".")
            annotation_command_file_4 <- paste(input$individual4,tissue4_nospaces,
                                               assay4_nospaces,
                                               ploidy4_nospaces,"bed",sep=".")
            chromosome_command_file_4 <- paste(input$chromosomes,pre_chromosome_file_4,sep = ".")
            
            
            
            pre_annotation_file_1 <- paste(input$individual1,tissue1_nospaces,
                                           assay1_nospaces,
                                           ploidy1_nospaces,input$chromosomes,input$region,"txt",sep=".")
            annotation_file_1 <- paste("annotation_files",
                                       pre_annotation_file_1,sep = "/")
            pre_annotation_file_2 <- paste(input$individual2,tissue2_nospaces,
                                           assay2_nospaces,
                                           ploidy2_nospaces,input$chromosomes,input$region,"txt",sep=".")
            annotation_file_2 <- paste("annotation_files",
                                       pre_annotation_file_2,sep = "/")
            pre_annotation_file_3 <- paste(input$individual3,tissue3_nospaces,
                                           assay3_nospaces,
                                           ploidy3_nospaces,input$chromosomes,input$region,"txt",sep=".")
            annotation_file_3 <- paste("annotation_files",
                                       pre_annotation_file_3,sep = "/")
            pre_annotation_file_4 <- paste(input$individual4,tissue4_nospaces,
                                           assay4_nospaces,
                                           ploidy4_nospaces,input$chromosomes,input$region,"txt",sep=".")
            annotation_file_4 <- paste("annotation_files",
                                       pre_annotation_file_4,sep = "/")
            
            
            
            chr_chromosome_file_1 <- paste(input$region,input$chromosomes,pre_chromosome_file_1,sep = ".")
            chromosome_file_1 <- paste("chromosome_files",
                                       chr_chromosome_file_1,sep = "/")
            chr_chromosome_file_2 <- paste(input$region,input$chromosomes,pre_chromosome_file_1,sep = ".")
            chromosome_file_2 <- paste("chromosome_files",
                                       chr_chromosome_file_2,sep = "/")
            chr_chromosome_file_3 <- paste(input$region,input$chromosomes,pre_chromosome_file_1,sep = ".")
            chromosome_file_3 <- paste("chromosome_files",
                                       chr_chromosome_file_3,sep = "/")
            chr_chromosome_file_4 <- paste(input$region,input$chromosomes,pre_chromosome_file_4,sep = ".")
            chromosome_file_4 <- paste("chromosome_files",
                                       chr_chromosome_file_4,sep = "/")
            
            
            if (file.exists(annotation_file_1)) {print("anotation file exists, generating plot ...")
            } else {
              
              chr_command <- paste("./chromosome_filterer_withregion.sh", input$chromosomes,input$region,
                                   chromosome_command_file_1,annotation_command_file_1)
              
              system(chr_command)
              
              print(chr_command)
              
            } 
            
            if (file.exists(annotation_file_2)) {print("anotation file exists, generating plot ...")
            } else {
              
              chr_command <- paste("./chromosome_filterer_withregion.sh", input$chromosomes,input$region,
                                   chromosome_command_file_1,annotation_command_file_2)
              
              system(chr_command)
              
              print(chr_command)
              
            } 
            
            if (file.exists(annotation_file_3)) {print("anotation file exists, generating plot ...")
            } else {
              
              chr_command <- paste("./chromosome_filterer_withregion.sh", input$chromosomes,input$region,
                                   chromosome_command_file_1,annotation_command_file_3)
              
              system(chr_command)
              
              print(chr_command)
              
            } 
            
            if (file.exists(annotation_file_4)) {print("anotation file exists, generating plot ...")
            } else {
              
              chr_command <- paste("./chromosome_filterer_withregion.sh", input$chromosomes,input$region,
                                   chromosome_command_file_1,annotation_command_file_4)
              
              system(chr_command)
              
              print(chr_command)
              
            }
            
            if (input$plot_type == "Heat Map") {plot_type <- "heatmap"
            } else if (input$plot_type == "Histogram") { plot_type <- "histogram"
            } else if (input$plot_type == "Scatterplot") { plot_type <- "scatterplot"
            } else {print ("ERROR in  plot type.")}
            
            html_file_name = paste(annotation_file_1,input$heat_map_colors1,
                                   chromosome_command_file_2,input$heat_map_colors2,
                                   chromosome_command_file_3,input$heat_map_colors3,
                                   chromosome_command_file_4,input$heat_map_colors4,
                                   plot_type,"html",sep=".")
            html_file_name_copy = paste(pre_annotation_file_1,input$heat_map_colors1,
                                        chromosome_command_file_2,input$heat_map_colors2,
                                        chromosome_command_file_3,input$heat_map_colors3,
                                        chromosome_command_file_4,input$heat_map_colors4,
                                        plot_type,"html",sep=".")
            
            if (file.exists(html_file_name)) {
              file.copy(html_file_name,"ENTEX_chromosome_visualization_download.html", overwrite = TRUE)
              #file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
              includeHTML(html_file_name)
              
            } else {
              
              if (plot_type == "heatmap") {
                print(chromosome_command_file_1)
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_1,
                                           chromosome_file_1,chromosome_file_1),
                                         c(annotation_file_1,annotation_file_2,
                                           annotation_file_3,annotation_file_4),
                                         ploidy=4, 
                                         legend = "T",chr_width = 20, 
                                         canvas_width = 2000,
                                         canvas_height = 4000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2),
                                                            c("white",input$heat_map_colors3),
                                                            c("white",input$heat_map_colors4)),
                                         chr_color = c("white"))
                
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
              } else if (plot_type == "histogram") {
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_1,
                                           chromosome_file_1,chromosome_file_1),
                                         c(annotation_file_1,annotation_file_2,
                                           annotation_file_3,annotation_file_4),
                                         ploidy=4, 
                                         legend = "T",chr_width = 20, 
                                         canvas_width = 2000,
                                         canvas_height = 4000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2),
                                                            c("white",input$heat_map_colors3),
                                                            c("white",input$heat_map_colors4)),
                                         chr_color = c("white"), plots = "bar", plot_color = c(input$heat_map_colors1, input$heat_map_colors2, input$heat_map_colors3, input$heat_map_colors4)
                )
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
                
              } else if (plot_type == "scatterplot") { 
                
                html_output =  chromoMap(c(chromosome_file_1,chromosome_file_1,
                                           chromosome_file_1,chromosome_file_1),
                                         c(annotation_file_1,annotation_file_2,
                                           annotation_file_3,annotation_file_4),
                                         ploidy=4, 
                                         legend = "T",chr_width = 20, 
                                         canvas_width = 2000,
                                         canvas_height = 4000, left_margin = 100,
                                         ch_gap = 6, data_based_color_map =  TRUE,
                                         data_colors = list(c("white",input$heat_map_colors1),
                                                            c("white",input$heat_map_colors2),
                                                            c("white",input$heat_map_colors3),
                                                            c("white",input$heat_map_colors4)),
                                         chr_color = c("white"), plots = "scatter", plot_color = c(input$heat_map_colors1, input$heat_map_colors2, input$heat_map_colors3, input$heat_map_colors4)
                                         )
                
                saveWidget(html_output,html_file_name, background = "white") 
                saveWidget(html_output,"ENTEX_chromosome_visualization_download.html", background = "white")
                includeHTML(html_file_name)
                
              } else {print ("ERROR in  plot type.")}
            }
            
          }
          }
        } else {print("more than three tracks")}
      })
    })
    output$infopanel <- renderUI({
      tabPanel("Information", panelText)
    })
  })
  
  output$download <- downloadHandler(
    filename = function() {
      
      "ENTEX_chromosome_visualization_download.html"
      },
    
    content = function(file) {
      
      file_name <- "ENTEX_chromosome_visualization_download.html"
      file.copy(file_name, file, overwrite = TRUE)
    }
  )
}
#=============================================================================#

shinyApp(ui, server)