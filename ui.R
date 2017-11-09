library(shiny)



shinyUI(fluidPage(
  
  # Application title
  titlePanel("topGO String Viewer"),
    
  fluidRow(
    tabsetPanel(
                tabPanel('Testing',
                         htmlOutput('table_test')
                ),
                tabPanel('Data',
                         column(4,
                                selectInput('data_name','Select Data',data_name_list,selected = data_name_list,multiple = T)
                                ),
                         column(8,uiOutput('gene_list_2')),
                         column(12,tabsetPanel(
                            tabPanel('Heatmaps',
                                     plotOutput('selected_heatmap'),
                                     plotOutput('NES_Diff_heatmap'),
                                     plotOutput('NS_Diff_heatmap')
                                      ),
                            tabPanel('BoxPlots'),
                         tabPanel('String',
                                  actionButton("run_sn", "Generate Plot"),
                                  actionButton("run_sn_link", "Generate link"),
                                  plotOutput('single_enrich',height = 100),
                                  plotOutput('sn',height = 700),
                                  #HTML(paste(textOutput('get_link'))),
                                  HTML(paste0(htmlOutput('sn_url')))
                                  
                                  # HTML(paste0('<object data="images/STRINGdb/df_SILAC_all_comparison_all_MCT_all_mapping/mean/Component/STRING/axon/axon_STRING.pdf" type="application/pdf" width="750px" height="750px">',
                                  #   '<embed src="images/STRINGdb/df_SILAC_all_comparison_all_MCT_all_mapping/mean/Component/STRING/axon/axon_STRING.pdf" type="application/pdf">',
                                  #   '<p>This browser does not support PDFs. Please download the PDF to view it: <a href="images/STRINGdb/df_SILAC_all_comparison_all_MCT_all_mapping/mean/Component/STRING/axon/axon_STRING.pdf">Download PDF</a>.</p>',
                                  #   '</embed>',
                                  #   '</object>'))
                         ),
                         tabPanel('Neighbours',
                                  uiOutput('gene_list'),
                                  actionButton('run_neigh',"Generate Plot"),
                                  plotOutput('neighbour_plot',height = 700)
                         ),
                         tabPanel('Interactions',
                                  actionButton('run_int',"Generate STRINGdb Plot"),
                                  plotOutput('interaction_plot',height = 700)
                                  
                                  
                         ),
                         tabPanel('Gene',
                                  uiOutput('gene_list_3'),
                                  HTML(paste0(htmlOutput('gene_path'))),
                                  
                                  HTML(paste0(htmlOutput('gene_pdf')))
                                  
                                  
                         )
                         
                         
                         
                         ))),
                tabPanel('GO',
    column(4, align = 'center',
           selectInput(inputId = 'data',  # Drop down menu to select the producer and cultivar
                       label = 'Select data',
                       choices = data_list,
                       selected = data_list[1],
                       multiple = F),
           
           selectInput(inputId = 'limit',  # Drop down menu to select the producer and cultivar
                       label = 'Limit',
                       choices = limit_list,
                       selected = limit_list[3],
                       multiple = F),
           
           selectInput(inputId = 'stat',  # Drop down menu to select the producer and cultivar
                       label = 'Select Stat',
                       choices = stat_list,
                       selected = stat_list[2],
                       multiple = F),
           
           selectInput(inputId = 'mapped',  # Drop down menu to select the producer and cultivar
                       label = 'mapped',
                       choices = mapped_list,
                       selected = mapped_list[2],
                       multiple = F),
           
           selectInput(inputId = 'ontology',  # Drop down menu to select the producer and cultivar
                       label = 'Select Ontology',
                       choices = ontology_list,
                       selected = ontology_list[1],
                       multiple = F),
           
           #HTML(paste(textOutput('table_name'))),
           
           HTML(paste(textOutput('term_num'))),
           
           uiOutput('term_list'),
           
           HTML(paste(textOutput('sn_list'))),
           
           radioButtons('fix_term','Fixed Term',choices = c('y','n'),selected='n',inline = T)
           
           
           
    ),
    
    column(8, align = 'center',
           navbarPage(title = NULL,
                      # tabPanel('Info',
                      #          HTML(paste0(htmlOutput('pdf_file_name_print')))
                      #          ),
                      tabPanel('pdf',
                               #tags$iframe(style='height:750px;width:1000px', src="vacuolar_part_STRING_UP.pdf")
                               HTML(paste0(htmlOutput('GO_composite_pdf'))),
                               HTML(paste0(htmlOutput('string_pdf')))
                               #tags$iframe(style='height:750px;width:1000px', src="images/STRINGdb/df_SILAC_all_comparison_all_MCT_all_mapping/mean/Component/STRING/axon/axon_STRING.pdf")
                               
                               #imageOutput('string_pic')
                      ),
                      tabPanel('Enrichment',
                               uiOutput('enrich_slider'),
                               HTML(paste(textOutput('m_name'))),
                               plotOutput('enrich_barplot',height = 750))
                      #tabPanel('Data',dataTableOutput('topGO_sig')),

                      #tabPanel('GO',
                               #uiOutput('gene_list_3'),
                               #HTML(paste0(htmlOutput('gene_path'))),
                               #HTML(paste0(htmlOutput('GO_composite_path'))),
                               
                               #HTML(paste0(htmlOutput('GO_composite_pdf')))
                               
                               
                     # )
           )#navbarPage
    ) #column
                )
    )
    
  )#fluidRow
)#fluidPage
)
