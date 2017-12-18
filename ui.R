library(shiny)



shinyUI(fluidPage(
  
  # Application title
  titlePanel("topGO String Viewer"),
    
  fluidRow(
    
    
    
    column(4,uiOutput('select_sample_ui')),
    column(4,uiOutput('select_sample_single_ui')), #single_sample
    column(4,
           #textOutput('path_list_print'),
           radioButtons('save_plot','Save Plots',c(T,F),selected = T, inline = T)),
    #column(4, verbatimTextOutput('data_info_print')
           #verbatimTextOutput('path_list_print')
           #),
    column(12,textOutput('thesis_path_print')),
    column(12,tabsetPanel(selected = 'Source',
                tabPanel('Test',textOutput('result')),
                
                #### SOURCE ####
                tabPanel('Source', 
                         shinyDirButton('folder', 'Folder select', 'Please select a folder'),
                  
                         #verbatimTextOutput('dir_text'),
                         verbatimTextOutput('wd_path_print'),
                         #verbatimTextOutput('data_file_list'),
                         tableOutput('data_df_table')
                         #actionButton('add_data','Add Data')
                         ),
                
                #### TABLES ####
                tabPanel('Tables',
                         tabsetPanel(
                           tabPanel('ALL', dataTableOutput('data_table')),
                           tabPanel('sig', dataTableOutput('sig_table')),
                           #tabPanel('GE',dataTableOutput('GE_table')),
                           #tabPanel('SILAC',dataTableOutput('SIALC_table')),
                           #tabPanel('NES_Diff',dataTableOutput('NES_Diff_table')),
                           #tabPanel('NS_Diff',dataTableOutput('NS_Diff_table')),
                           tabPanel('Mapped',
                                    radioButtons('re_run','re-run mapping',choiceNames = c('T','F'),choiceValues = c(T,F),selected = F,inline = T),
                                    dataTableOutput('mapped_table')),
                           tabPanel('Mapped up/down', dataTableOutput('mapped_ud_table')),
                           tabPanel('gene_list',
                                    actionButton('write_gene_list','Write Gene List to file'),
                                    verbatimTextOutput('gene_list_print_test')),
                           tabPanel('m_ts',dataTableOutput('m_ts_table'))
                         )
                ),
      
                ##### SELECT DATA ####
                tabPanel('Select Data',
                         column(6, radioButtons('data_type_radio','Select Data',choiceNames = c('List','Uniprot','Enrichment',"Venn"),choiceValues = c('list','uniprot','enrichment','venn'),selected = 'venn',inline = T)),
                         column(4, actionButton('store_button', 'Store Gene List')),
                         column(12,
                    ### _list ####
                         conditionalPanel(condition = "input.data_type_radio == 'list'",
                                          uiOutput('select_gene_list_ui')),

                    #### _uniprot ####
                         conditionalPanel(condition = "input.data_type_radio == 'uniprot'",
                                          uiOutput('select_gene_file_ui'),
                                          textOutput('gene_list_print')),
                         
                    ######## _Enrichment ###########
                         conditionalPanel(condition = "input.data_type_radio == 'enrichment'",
                         tabPanel('Enrichment',
                                  column(4, align = 'center',
                                         # selectInput(inputId = 'single_data',  # Drop down menu to select the producer and cultivar
                                         #             label = 'Select data',
                                         #             choices = data_list,
                                         #             selected = data_list[2],
                                         #             multiple = F),
                                         #uiOutput('select_sample_single_ui_3'), #single_sample_3
                                         radioButtons('enrichment','Enrichment',choices = c('topGO','STRINGdb','AnimalTFDB'),selected = 'topGO',inline = 'T'),
                                         ### __topGO ####
                                         conditionalPanel(condition = "input.enrichment == 'topGO'",
                                            tags$h5('topGO'),
                                            selectInput(inputId = 'topGO_enrichment',  # Drop down menu to select the producer and cultivar
                                                        label = 'Select Enrichment',
                                                        choices = c('Component', 'Function','Process'),
                                                        selected = data_list[1],
                                                        multiple = F),
                                            
                                            # selectInput(inputId = 'topGO_reduce',  # Drop down menu to select the producer and cultivar
                                            #             label = 'Significance',
                                            #             choices = c('Full','Significant'),
                                            #             selected = c('Full'),
                                            #             multiple = F),
                                            
                                           # selectInput(inputId = 'limit',  # Drop down menu to select the producer and cultivar
                                           #             label = 'Limit',
                                           #             choices = limit_list,
                                           #             selected = limit_list[3],
                                           #             multiple = F),
                                           
                                           selectInput(inputId = 'topGO_stat',  # Drop down menu to select the producer and cultivar
                                                       label = 'Select Stat',
                                                       choices = topGO_stat_list,
                                                       selected = topGO_stat_list[4],
                                                       multiple = F),
                                           
                                           selectInput(inputId = 'topGO_mtc',  # Drop down menu to select the producer and cultivar
                                                       label = 'MTC',
                                                       choices = topGO_mtc_list,
                                                       selected = topGO_mtc_list[1],
                                                       multiple = F),
                                           
                                           # selectInput(inputId = 'ontology',  # Drop down menu to select the producer and cultivar
                                           #             label = 'Select Ontology',
                                           #             choices = ontology_list,
                                           #             selected = ontology_list[1],
                                           #             multiple = F),
                               
                                           
                                           #HTML(paste(textOutput('table_name'))),
                                           
                                           HTML(paste(textOutput('term_num'))),
                                           
                                           
                                           
                                           HTML(paste(textOutput('sn_list')))
                                           
                                           #verbatimTextOutput('enrichment_data_path_print')
                                           
                                           
                                           ),
                                         #### __STRINGdb ####
                                         conditionalPanel(condition = "input.enrichment == 'STRINGdb'",
                                                          tags$h5('STRINGdb'), 
                                                          selectInput(inputId = 'stringdb_enrichment',  # Drop down menu to select the producer and cultivar
                                                                      label = 'Select Enrichment',
                                                                      choices = string_db_enrichment_list,
                                                                      selected = data_list[1],
                                                                      multiple = F),
                                                          selectInput(inputId = 'stringdb_reduce',  # Drop down menu to select the producer and cultivar
                                                                      label = 'Significance',
                                                                      choices = c('Full','Significant'),
                                                                      selected = c('Significant'),
                                                                      multiple = F)
                                                          #uiOutput('term_list')
                                                          #verbatimTextOutput('enrichment_data_path_print')
                                                          
                                         ),
                                                          
                                         
                                         #### __AnimalTF ####
                                         conditionalPanel(condition = "input.enrichment == 'AnimalTFDB'",
                                                          tags$h5('AnimalTFDB'),
                                                          # selectInput(inputId = 'single_data',  # Drop down menu to select the producer and cultivar
                                                          #             label = 'Select data',
                                                          #             choices = data_list,
                                                          #             selected = data_list[2],
                                                          #             multiple = F),
                                                          selectInput('tf_enrichment','Type',  c('Chromatin_Remodelling','co-factors','transcription_factors')),
                                                          radioButtons('tf_map_button', 'select', c('mapped','full'))
                                                          #verbatimTextOutput('enrichment_data_path_print')
                                                          ), 
                                         uiOutput('term_list'),
                                         radioButtons('fix_term','Fixed Term',choices = c('y','n'),selected='n',inline = T),
                                         
                                         verbatimTextOutput('enrichment_data_path_print')
                                         
                                                          
                                         
                                  ),
                                  
                                  column(8, align = 'center',
                                         navbarPage(title = NULL,
                                                    tabPanel('Enrichment',
                                                             uiOutput('enrich_slider_2'),
                                                             plotOutput('enrichment_plot')
                                                             
                                                             #HTML(paste(textOutput('m_name'))),
                                                             #plotOutput('enrich_barplot',height = 750)
                                                    ),
                                                    tabPanel('Data',
                                                             
                                                             tabsetPanel(
                                                               tabPanel('Table', dataTableOutput('enrichment_table')),
                                                               tabPanel('mapped', dataTableOutput('GO_mapped_table')),
                                                               tabPanel('full', dataTableOutput('enrichment_table_full'))
                                                               
                                                               
                                                             )),
                                                    
                                                    # tabPanel('Info',
                                                    #          HTML(paste0(htmlOutput('pdf_file_name_print')))
                                                    #          ),
                                                    tabPanel('pdf',
                                                             #tags$iframe(style='height:750px;width:1000px', src="vacuolar_part_STRING_UP.pdf")
                                                             #uiOutput("pdfview_GO"),
                                                             #uiOutput("pdfview_string"),
                                                             
                                                             HTML(paste0(htmlOutput('GO_composite_pdf'))),
                                                             HTML(paste0(htmlOutput('string_pdf')))
                                                             #tags$iframe(style='height:750px;width:1000px', src="images/STRINGdb/df_SILAC_all_comparison_all_MCT_all_mapping/mean/Component/STRING/axon/axon_STRING.pdf")
                                                             
                                                             #imageOutput('string_pic')
                                                    ),
       
                                                    tabPanel('Run Enrichment',
                                                             radioButtons('topGO_re_run','Re Run',c(F,T),inline = T),
                                                             verbatimTextOutput('geneNames_all_print')
                                                             )
                                                             
                                       
                                       #)
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
                         ),
  
                        #### _VENN #####
                        conditionalPanel(condition = "input.data_type_radio == 'venn'",
                                radioButtons('venn_data_select_button','Select',c('all','gene_list'),selected = 'all',inline = T),
                                 column(6,uiOutput('venn_all_select')),
                                 column(6,uiOutput('venn_intersection_ui')), # input$venn_int
                                 column(12,plotOutput('venn_all')),
                                 textOutput('venn_plot_path_print'),
                                 #plotOutput('venn_all_gplots'),
                                 #uiOutput('venn_intersection_ui'),
                                 column(12,verbatimTextOutput('venn_int_print'))
                                 
                                 #column(12,textOutput('venn_int_print'))
                                 #verbatimTextOutput('venn_all_gplots_print')
                        )
                        
                         
                         
                         #column(12,uiOutput('gene_list_2')),
                         #dataTableOutput('gene_list_df_table'),
                         #textOutput('gene_list_print')
                         
                         )),
             
                    ##### HEATMAPS #####
                            tabPanel('Heatmaps',tabsetPanel(
                              tabPanel('mean',
                                     plotOutput('selected_heatmap')
                                     ),
                              tabPanel('Full',
                                    #uiOutput('select_sample_single_ui_2'),
                                     plotOutput('full_selected_heatmap')
                              )
                                     
                                      )),
                
                #### BOXPLOTS ####
                
                            tabPanel('BoxPlots', id = 'boxplot',
                                     tabsetPanel(
                                       tabPanel('ggplot', 
                                                tabsetPanel(
                                                  tabPanel('All', plotOutput('boxplot_gplot')),
                                                
                                                  tabPanel('Gene',
                                                           actionButton('gene_boxplot','Generate Plots'),
                                                           uiOutput('g_boxplots_plots')),
                                                  tabPanel('Sample',
                                                           actionButton('sample_boxplot','Generate Plots'),
                                                           uiOutput('g_boxplots_plots_sample')),
                                                  tabPanel('time series',
                                                           plotOutput('timeseries_boxplot'),
                                                           actionButton('gene_boxplot_ts','Generate Plots'),
                                                           uiOutput('g_boxplots_plots_ts')
                                                           
                                                           )
                                                )),
                                       tabPanel('pdf',
              
                                          HTML(paste0(htmlOutput('boxplot_pdfs')))
                                         )
                                      )
                                     ),
                #### sn PLOT ####
                            tabPanel('STRINGdb',tabsetPanel(
                              tabPanel('Networks',
 
                                     column(4,radioButtons('sn_select','Select String network type',choiceNames = c('None','Neighbour','Interaction','Sub network','Cluster'),choiceValues = c('none','neighbour','interaction','sub','cluster'),selected = 'none',inline = T)),
                                    
                                    column(2,actionButton("sn_select_button", "Generate Plot")),
                                    
                                    #column(2,uiOutput('ui_generate_link_select')),
                                    
                                    column(2,actionButton("sn_select_button_link", "Generate link")),
                                    
                                    column(2,HTML(paste0(htmlOutput('sn_url_select')))),
                                                     
                                    
                                    column(2,uiOutput('ui_reset_sn_select')),
                                    
                                    #conditionalPanel(condition = "input.sn_select == 'neighbour'",
                                                     column(4, actionButton('store_button_neigbour', 'Store Gene List')),
                                    #),
                                                     
                                    
                                    #uiOutput('gene_list'),
                                    #actionButton('run_neigh',"Generate Plot"),
                                    #column(12,textOutput('sn_thesis_path')),
                                    column(9,
                                    uiOutput('sn_image_ui')
                                    #plotOutput('neighbour_plot',height = 700),
                                    #dataTableOutput('neighbour_table')
                                    #plotOutput('neighbour_heatmap')
                                    
  
                            ),
                           column(3,
                                  plotOutput('legend',height = 1400))
                            
                            ),
          
             tabPanel('Enrichemnt',tabsetPanel(
                                               
                      tabPanel('PPI',plotOutput('string_db_ppi_enrichment')),
                      tabPanel('Enrichment',
                               radioButtons('sn_enrich_radio','Select Enrichment', choices = c('CC','BP','MP','KEGG','Pfam','Prosit'),inline = T),
                               dataTableOutput('sn_enrichment_table')
                               )
                      )))),
             
    ##### ENRICHMENT #####         
     tabPanel('Enrichment',
       column(4,selectInput('select_sn_MT','select methodMT',string_db_methodMT_list)),
       column(4,selectInput('iea','Electronic Inferred Annotations',c(FALSE,TRUE))),
       column(4,selectInput('background','Select backgroundV', c('all_mapped','NULL'))),
       column(12,
       tabsetPanel(
       tabPanel('Heatmap',
                actionButton('generate_enrichment_heatmap','Generate Heatmap'),
                selectInput('eh_fdr','fdr',c(0.05,0.01,0.001,0.0001,0.00001,0.000001),selected = 0.0001),
                uiOutput('enrichement_data_select_ui'), #input$enrichment_data_select
                #verbatimTextOutput('enrichment_list_print')
                plotOutput('enrichment_heatmap')
                ),
       tabPanel("Full",
                #c('Component', 'Function','Process', 'KEGG','Pfam','InterPro', 'Tissue','Disease')
                tags$h5('Be patient this takes some time ... '),
                tags$h3('Component'),
                dataTableOutput('sn_en_table_Component'),
                tags$h3('Function'),
                dataTableOutput('sn_en_table_Function'),
                tags$h3('Process'),
                dataTableOutput('sn_en_table_Process'),
                tags$h3('KEGG'),
                dataTableOutput('sn_en_table_KEGG'),
                tags$h3('InterPro'),
                dataTableOutput('sn_en_table_InterPro'),
                tags$h3('Tissue'),
                dataTableOutput('sn_en_table_Tissue'),
                tags$h3('Disease'),
                dataTableOutput('sn_en_table_Disease')
                ),
       tabPanel('Select',
              column(4,selectInput('select_enrichment',"select_enrichment",string_db_enrichment_list)),
              #column(4,selectInput('select_sn_MT','select methodMT',string_db_methodMT_list)),
              #column(4,selectInput('iea','Electronic Inferred Annotations',c(FALSE,TRUE))),
              column(12,
              #textOutput('sn_en_print'),
              #dataTableOutput('df_gene_list'),
              dataTableOutput('sn_en_table')
              ))))),
             
             
    tabPanel('Tools',tabsetPanel(
      tabPanel('logratio2foldchange',
               textInput('log_2','log 2'),
               textOutput('fc'),
               plotOutput('log2fc')
               )
    
      )),
    #### R info #### 
    tabPanel('R info',
             tabsetPanel(
               tabPanel('ls',verbatimTextOutput('ls')),

               tabPanel('Testing',
                        htmlOutput('table_test')
               )
             ))
    ))
    
  )#fluidRow
)#fluidPage
)
