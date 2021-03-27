#============================ ACKNOWLEDGMENT ============================================#
# Code written by Gabriel Conte Cortez Martins, Yale 2020 and Daniel Farid, Yale 2023. Help from Mark Gerstein, 
# Joel Rosowsky, Gamze Gursoy, Timur Galeev, and Kun Xiong among other members of the Gerstein Lab at Yale University
#============================ NECESSARY LIBRARIES FOR CHROMOMAPS AND APP ============================================#
library(shinydashboard)
library(stringr)
library(chromoMap)
library(shiny)
library(htmlwidgets)
#============================ LIBRARIES FOR SHINYAPP.IO DEPLOYMENT (CAN KEEP COMMENTED OUT) ============================================#
#install.packages('rsconnect')
#rsconnect::setAccountInfo(name='chromosomepainting', token='E48DEFDBE0B5EB7A76E9EACC0555E6B1', secret='/sIPciOe23PyR+l2D9Ou0e+3NK1lhZaeV9qThgOI')
#library(rsconnect)
#rsconnect::deployApp('version5')
#deployApp()
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
      downloadButton('download', "Download Plot")
    )),
  dashboardBody(shinydashboard::box(htmlOutput("myChromoMap"), width = 2000, height = 1700))
)

#============================= Server =======================================#



server <- function(input, output) {
  
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
    
    if (input$number_of_tracks == 1) { #ONE TRACK
      sidebarMenu(
        menuItem("Track 1",
                 
                 selectInput('individual1', 'Individual',
                             list("ENC001","ENC002","ENC003","ENC004"),
                             selected = "ENC003"
                 ),
                 selectInput('assay1', 'Assay',
                             list("ATAC-seq","HM-ChIP-seq","H3K27ac","H3K9me3"),
                             selected = "H3K27ac"
                 ),
                 selectInput('tissue1', 'Tissue',
                             list("Adrenal Gland","Gastrocnemius Medialis","Transverse Colon"),
                             selected = "Transverse Colon"
                 ),
                 selectInput('ploidy1', 'Ploidy',
                             list("Reference","Haplotype 1","Haplotype 2"),
                             selected = "Haplotype 1"
                 ),
                 selectInput('heat_map_colors1', 'Annotation Color',
                             list("red","blue","gold","green","purple"))
        ),
        menuItem("Advanced",
                 selectInput('chromosomes', 'Chromosomes',
                             list("ALL","chr1","chr2","chr3","chr4","chr5","chr6",
                                  "chr7","chr8","chr9","chr10","chr11","chr12",
                                  "chr13","chr14","chr15","chr16","chr17","chr18",
                                  "chr19","chr20","chr21","chr22","chrX","chrY")
                 ),
                 textInput('region',"Region"),
                 selectInput('plot_type',"Plot Type",list("Heat Map","Histogram","Scatterplot"))
                 
                 
        ))
    }
    
    else if (input$number_of_tracks == 2) { #TWO TRACKS
      
      sidebarMenu(
        menuItem("Track 1",
                 selectInput('individual1', 'Individual',
                             list("ENC001","ENC002","ENC003","ENC004")
                 ),
                 selectInput('assay1', 'Assay',
                             list("ATAC-seq","HM-ChIP-seq","H3K27ac","H3K9me3")
                 ),
                 selectInput('tissue1', 'Tissue',
                             list("Adrenal Gland","Gastrocnemius Medialis","Transverse Colon")
                 ),
                 selectInput('ploidy1', 'Ploidy',
                             list("Reference","Haplotype 1","Haplotype 2")
                 ),
                 selectInput('heat_map_colors1', 'Annotation Color',
                             list("red","blue","gold","green","purple"))
        ),
        menuItem("Track 2",
                 selectInput('individual2', 'Individual',
                             list("ENC001","ENC002","ENC003","ENC004")
                 ),
                 selectInput('assay2', 'Assay',
                             list("ATAC-seq","HM-ChIP-seq","H3K27ac","H3K9me3")
                 ),
                 selectInput('tissue2', 'Tissue',
                             list("Adrenal Gland","Gastrocnemius Medialis","Transverse Colon")
                 ),
                 selectInput('ploidy2', 'Ploidy',
                             list("Reference","Haplotype 1","Haplotype 2")
                 ),
                 selectInput('heat_map_colors2', 'Annotation Color',
                             list("red","blue","gold","green","purple")
                 )
        ),
        menuItem("Advanced",
                 selectInput('chromosomes', 'Chromosomes',
                             list("ALL","chr1","chr2","chr3","chr4","chr5","chr6",
                                  "chr7","chr8","chr9","chr10","chr11","chr12",
                                  "chr13","chr14","chr15","chr16","chr17","chr18",
                                  "chr19","chr20","chr21","chr22","chrX","chrY")
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
                             list("ENC001","ENC002","ENC003","ENC004")
                 ),
                 selectInput('assay1', 'Assay',
                             list("ATAC-seq","HM-ChIP-seq","H3K27ac","H3K9me3")
                 ),
                 selectInput('tissue1', 'Tissue',
                             list("Adrenal Gland","Gastrocnemius Medialis","Transverse Colon")
                 ),
                 selectInput('ploidy1', 'Ploidy',
                             list("Reference","Haplotype 1","Haplotype 2")
                 ),
                 selectInput('heat_map_colors1', 'Annotation Color',
                             list("red","blue","gold","green","purple"))
        ),
        menuItem("Track 2",
                 selectInput('individual2', 'Individual',
                             list("ENC001","ENC002","ENC003","ENC004")
                 ),
                 selectInput('assay2', 'Assay',
                             list("ATAC-seq","HM-ChIP-seq","H3K27ac","H3K9me3")
                 ),
                 selectInput('tissue2', 'Tissue',
                             list("Adrenal Gland","Gastrocnemius Medialis","Transverse Colon")
                 ),
                 selectInput('ploidy2', 'Ploidy',
                             list("Reference","Haplotype 1","Haplotype 2")
                 ),
                 selectInput('heat_map_colors2', 'Annotation Color',
                             list("red","blue","gold","green","purple")
                 )
        ),
        menuItem("Track 3",
                 
                 selectInput('individual3', 'Individual',
                             list("ENC001","ENC002","ENC003","ENC004")
                 ),
                 selectInput('assay3', 'Assay',
                             list("ATAC-seq","HM-ChIP-seq","H3K27ac","H3K9me3")
                 ),
                 selectInput('tissue3', 'Tissue',
                             list("Adrenal Gland","Gastrocnemius Medialis","Transverse Colon")
                 ),
                 selectInput('ploidy3', 'Ploidy',
                             list("Reference","Haplotype 1","Haplotype 2")
                 ),
                 selectInput('heat_map_colors3', 'Annotation Color',
                             list("red","blue","gold","green","purple")
                 )
        ),
        menuItem("Advanced",
                 selectInput('chromosomes', 'Chromosomes',
                             list("ALL","chr1","chr2","chr3","chr4","chr5","chr6",
                                  "chr7","chr8","chr9","chr10","chr11","chr12",
                                  "chr13","chr14","chr15","chr16","chr17","chr18",
                                  "chr19","chr20","chr21","chr22","chrX","chrY")
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
                             list("ENC001","ENC002","ENC003","ENC004")
                 ),
                 selectInput('assay1', 'Assay',
                             list("ATAC-seq","HM-ChIP-seq","H3K27ac","H3K9me3")
                 ),
                 selectInput('tissue1', 'Tissue',
                             list("Adrenal Gland","Gastrocnemius Medialis","Transverse Colon")
                 ),
                 selectInput('ploidy1', 'Ploidy',
                             list("Reference","Haplotype 1","Haplotype 2")
                 ),
                 selectInput('heat_map_colors1', 'Annotation Color',
                             list("red","blue","gold","green","purple"))
        ),
        menuItem("Track 2",
                 selectInput('individual2', 'Individual',
                             list("ENC001","ENC002","ENC003","ENC004")
                 ),
                 selectInput('assay2', 'Assay',
                             list("ATAC-seq","HM-ChIP-seq","H3K27ac","H3K9me3")
                 ),
                 selectInput('tissue2', 'Tissue',
                             list("Adrenal Gland","Gastrocnemius Medialis","Transverse Colon")
                 ),
                 selectInput('ploidy2', 'Ploidy',
                             list("Reference","Haplotype 1","Haplotype 2")
                 ),
                 selectInput('heat_map_colors2', 'Annotation Color',
                             list("red","blue","gold","green","purple")
                 )
        ),
        menuItem("Track 3",
                 
                 selectInput('individual3', 'Individual',
                             list("ENC001","ENC002","ENC003","ENC004")
                 ),
                 selectInput('assay3', 'Assay',
                             list("ATAC-seq","HM-ChIP-seq","H3K27ac","H3K9me3")
                 ),
                 selectInput('tissue3', 'Tissue',
                             list("Adrenal Gland","Gastrocnemius Medialis","Transverse Colon")
                 ),
                 selectInput('ploidy3', 'Ploidy',
                             list("Reference","Haplotype 1","Haplotype 2")
                 ),
                 selectInput('heat_map_colors3', 'Annotation Color',
                             list("red","blue","gold","green","purple")
                 )
        ),
        menuItem("Track 4",
                 selectInput('individual4', 'Individual',
                             list("ENC001","ENC002","ENC003","ENC004")
                 ),
                 selectInput('assay4', 'Assay',
                             list("ATAC-seq","HM-ChIP-seq","H3K27ac","H3K9me3")
                 ),
                 selectInput('tissue4', 'Tissue',
                             list("Adrenal Gland","Gastrocnemius Medialis","Transverse Colon")
                 ),
                 selectInput('ploidy4', 'Ploidy',
                             list("Reference","Haplotype 1","Haplotype 2")
                 ),
                 selectInput('heat_map_colors4', 'Annotation Color',
                             list("red","blue","gold","green","purple")
                 )
        ),
        menuItem("Advanced",
                 selectInput('chromosomes', 'Chromosomes',
                             list("ALL","chr1","chr2","chr3","chr4","chr5","chr6",
                                  "chr7","chr8","chr9","chr10","chr11","chr12",
                                  "chr13","chr14","chr15","chr16","chr17","chr18",
                                  "chr19","chr20","chr21","chr22","chrX","chrY")
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
  
  observeEvent(input$submit,{
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
                file.copy(html_file_name,"")
                file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
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
                                           chr_color = c("white"), plots = "bar")
                  
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
                                           chr_color = c("white"), plots = "scatter")
                  
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
              
              
              pre_annotation_file_1 <- paste(input$individual1,tissue1_nospaces,
                                             assay1_nospaces,
                                             ploidy1_nospaces,input$chromosomes,"txt",sep=".")
              annotation_file_1 <- paste("annotation_files",
                                                              pre_annotation_file_1,sep = "/")
              
              chr_chromosome_file_1 <- paste(input$chromosomes,pre_chromosome_file_1,sep = ".")
              
              chromosome_file_1 <- paste("/Home/chromosome_files",
                                          chr_chromosome_file_1,sep = "/")
              
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
              ### IF THE FILE HTML FILE NEEDED TO GENERATE THE SELECTED CHROMOMAP ALREADY EXISTS, ###
              ### WE DON'T NEED TO GENERATE THE CHROMOMAP AGAIN, WE JUST LOAD THE EXISTING HTML   ###
              #######################################################################################
              
              if (file.exists(html_file_name)) {
                file.copy(html_file_name,"")
                file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
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
                                           chr_color = c("white"), plots = "bar",
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
                                           chr_color = c("white"), plots = "scatter")
                  
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
              
              
              pre_annotation_file_1 <- paste(input$individual1,tissue1_nospaces,
                                             assay1_nospaces,
                                             ploidy1_nospaces,input$chromosomes,input$region,"txt",sep=".")
              
              annotation_file_1 <- paste("annotation_files",
                                         pre_annotation_file_1,sep = "/")
              
              chr_chromosome_file_1 <- paste(input$region,input$chromosomes,pre_chromosome_file_1,sep = ".")
              
              chromosome_file_1 <- paste("chromosome_files",
                                         chr_chromosome_file_1,sep = "/")
              
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
              
              if (file.exists(html_file_name)) {
                file.copy(html_file_name,"")
                file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
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
                                           chr_color = c("white"), plots = "bar",
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
                                           chr_color = c("white"), plots = "scatter")
                  
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
                file.copy(html_file_name,"")
                file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
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
                                           chr_color = c("white"), plots = "bar")
                  
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
                                           chr_color = c("white"), plots = "scatter")
                  
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
              file.copy(html_file_name,"")
              file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
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
                                         chr_color = c("white"), plots = "bar",
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
                                         chr_color = c("white"), plots = "scatter")
                
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
              file.copy(html_file_name,"")
              file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
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
                                         chr_color = c("white"), plots = "bar",
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
                                         chr_color = c("white"), plots = "scatter")
                
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
              file.copy(html_file_name,"")
              file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
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
                                         chr_color = c("white"), plots = "bar")
                
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
                                         chr_color = c("white"), plots = "scatter")
                
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
              file.copy(html_file_name,"")
              file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
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
                                         chr_color = c("white"), plots = "bar",
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
                                         chr_color = c("white"), plots = "scatter")
                
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
              file.copy(html_file_name,"")
              file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
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
                                         chr_color = c("white"), plots = "bar",
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
                                         chr_color = c("white"), plots = "scatter")
                
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
              file.copy(html_file_name,"")
              file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
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
                                         chr_color = c("white"), plots = "bar")
                
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
                                         chr_color = c("white"), plots = "scatter")
                
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
              file.copy(html_file_name,"")
              file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
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
                                         chr_color = c("white"), plots = "bar",
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
                                         chr_color = c("white"), plots = "scatter")
                
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
              file.copy(html_file_name,"")
              file.rename(html_file_name_copy,"ENTEX_chromosome_visualization_download.html")
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
                                         chr_color = c("white"), plots = "bar",
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
                                         chr_color = c("white"), plots = "scatter")
                
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
    })
  
  output$download <- downloadHandler(
    filename = function() {
      
      "ENTEX_chromosome_visualization_download.html"},
    
    content = function(file) {
      
      file_name <- "ENTEX_chromosome_visualization_download.html"
      file.copy(file_name, file)
    }
  )
}
#=============================================================================#

shinyApp(ui, server)