library(shiny)



shinyUI(fluidPage(
  
  # Application title
  titlePanel("topGO String Viewer"),
    
  fluidRow(
    actionButton('debug_button','debug'),
    
    
    column(4,uiOutput('select_sample_ui')),
    column(2,radioButtons('boxplot_full','Subset',c('full','subset'),selected = 'subset')),
    
    column(2,
           radioButtons('save_plot','Save Plots',c(T,F),selected = T, inline = F)),
    column(4,textInput('taxonomy','Taxonomy','9606')),
    #column(4, verbatimTextOutput('data_info_print')
           #verbatimTextOutput('path_list_print')
           #),
    column(12,textOutput('thesis_path_print')),
    column(12,tabsetPanel(selected = 'Source',
                tabPanel('Test',textOutput('result')),
                
                #### UPLOAD ####
                tabPanel('Upload',
                         column(4,uiOutput('upload_dataset_ui')),
                         column(4,uiOutput('dataset_select_ui')),
                         column(2,radioButtons('show_dataset','Show Table',c(F,T))),
                         column(2,actionButton('save_dataset', "Save")),
                         column(12,dataTableOutput('dataset_table')),
                    
                    tabsetPanel(selected = '',
                                
                    #### _Upload ####
                    tabPanel('Upload',

                            column(12,uiOutput('file_path_ui')),
                            column(12,tags$hr()),
                            tags$h5('File upload properties'),
            
                             # ___Input: Checkbox if file has header ----
                             column(2,uiOutput('file_header_ui')),
                            column(5,uiOutput('file_sep_ui')),
                            column(5,uiOutput('file_quote_ui')),
                            column(12,tags$hr()),
                          
                             
                            
                            column(12),
                            column(12,uiOutput('reload_file_ui')),
                            column(4,uiOutput('show_upload_table_ui')),
                  
                            column(4,textOutput('original_data_detail_text')),
                            
                            column(12,dataTableOutput('original_data')),
                            column(12,tags$hr()),
                          
                            column(6,uiOutput('upload_data_origin_ui')),
                            column(6,uiOutput('upload_data_type_ui')),
                            column(12,tags$hr()),
                            column(4,uiOutput('proteome_type_ui')),
                            column(4,uiOutput('maxquant_ui')),
                            column(4,uiOutput('proteome_label_ui')),
                            column(12,tags$hr()),
                         
                           

                           column(12),
                           column(6,uiOutput('experiment_name_ui')),
                           
                           column(6,uiOutput('experiment_code_ui')),
                           
                           column(12,uiOutput('experiment_description_ui')),
                           column(12,tags$hr()),
                           
                           column(4,uiOutput('id_column_1_ui'),
                                  textOutput('col1_len')),
                           column(4,uiOutput('id_column_2_ui'),
                                  textOutput('col2_len')),
                           column(4,uiOutput('id_column_3_ui'),
                                  textOutput('col3_len')),
                           column(12,tags$hr()),
           
                           column(12),
                           
                           column(3,uiOutput('condition_1_name_ui')),
                           column(3,uiOutput('condition_2_name_ui')),
                           column(3,uiOutput('condition_3_name_ui')),
                           column(3,uiOutput('condition_4_name_ui')),
                           column(12),
                           
                           column(3,uiOutput('condition_1_columns_ui')),
                           column(3,uiOutput('condition_2_columns_ui')),
                           column(3,uiOutput('condition_3_columns_ui')),
                           column(3,uiOutput('condition_4_columns_ui')),
                           
                           column(12),
                           
                           # conditionalPanel(condition = "input.upload_data_type == 'Other'",
                           #                  column(3,uiOutput('condition_1_name_ui'),
                           #                         uiOutput('condition_1_ui')),
                           #                  column(3,uiOutput('condition_2_name_ui'),
                           #                         uiOutput('condition_2_ui')),
                           #                  column(3,uiOutput('condition_3_name_ui'),
                           #                         uiOutput('condition_3_ui')),
                           #                  column(3,uiOutput('condition_4_name_ui'),
                           #                         uiOutput('condition_4_ui'))
                           # ),
                           
                          #  conditionalPanel(condition = "input.mq_type == 'LFQ'",
                          #    column(3,uiOutput('condition_1_name_ui'),
                          #           uiOutput('condition_1_ui')),
                          #    column(3,uiOutput('condition_2_name_ui'),
                          #           uiOutput('condition_2_ui')),
                          #    column(3,uiOutput('condition_3_name_ui'),
                          #           uiOutput('condition_3_ui')),
                          #    column(3,uiOutput('condition_4_name_ui'),
                          #           uiOutput('condition_4_ui'))
                          #  ),
                          # conditionalPanel(condition = "input.mq_type == 'SILAC'",
                          #          column(4,uiOutput('silac_comp_ui'),
                          #                 uiOutput('silac_comp_rev_ui')),
                          #       
                          #          column(4,uiOutput('silac_rep_ui'),
                          #                 uiOutput('silac_rep_rev_ui')),
                          #          column(4,uiOutput('silac_incorp_ui'))
                          # ),
                           #column(12,uiOutput('save_experiment_ui')),
                          column(12,tags$hr()),
                           column(12,uiOutput('save_output_ui'))
                           #column(12,dataTableOutput('experiment_df'))
                         ),
                    ###_ID_mapping ####
                    tabPanel('ID mapping',
                             tabsetPanel(
                               
                               
                               tabPanel('Select id column',
                                         uiOutput('select_id_ui'),
                                         textOutput('mart_percentage_matched'),
                                         dataTableOutput('select_id_df')
                               ),
                               
                               tabPanel('Add BioMart Columns',
                                              
                               #column(5,selectInput('selectMart','Select Mart',biomaRt::listMarts()$biomart,biomaRt::listMarts()$biomart[1]),
                               column(6,
                                uiOutput('select_mart_host_ui'),
                                      
                               uiOutput('select_mart_ui'),
                               uiOutput('list_mart_ui'),
                               uiOutput('mart_column_ui')),
                               column(6,uiOutput('filter_ui'),
                               uiOutput('attributes_ui'),
                               textInput('mart_id_prefix','Mart ID prefix',''),
                               numericInput('mart_id_length','Mart ID length',0),
                               textInput('mart_id_split','Mart ID split','')),
                               column(12,
                               uiOutput('mart_slider'),
                               tags$h5('Mapping is first done of a subset of ids to test the biomart assignments, click Run to do full mapping, it may take up to an hour'),
                               
                               actionButton('run_biomart','Run'),
                              
                               dataTableOutput('bm_df'))
                              #)
                              ),
                            tabPanel("Separate ID's",
                                             column(5,uiOutput('sep_id_ui')),
                                             column(5,textInput('col_sep','id separator','')),
                                             column(2,actionButton('run_sep','Run')),
                                             column(12,dataTableOutput('separate_columns'))
                                            
                            )
                                      
                                      
                             #)
                             )
                    ),
                    ###_Numbers####     
                    tabPanel('Numbers',
                             plotOutput('sample_numbers')
                             ),
                    ###_Ratio####
                    tabPanel('Ratio',
                             #radioButtons('run_ratios','Run Ratios',c(F,T),inline = T),
                             actionButton("run_ratios", "Run Ratios"),
                             
                             #dataTableOutput('expression_data'),
                             
                             #textOutput('dataset_list_print'),
                             #textOutput('dataset_type_print'),
                             column(6,plotOutput('density_plot')),
                             column(6,plotOutput('sd_boxplot')),
                             dataTableOutput('expression_data')
                             
                             #radioButtons('save_ratios','Save Ratios',c(F,T),inline = T)
                             
                                
                                ),
                    ###_Stat####
                    tabPanel('Stat',
                       
                             actionButton('run_stat','Run'),
                             plotOutput('fdr_plot'),
                             plotOutput('volcano_plot'),
                             htmlOutput('stat_info_text'),
                             dataTableOutput('stat_data_table')
                            
                             
                             )
                    
                    
                    
                    
                    )
                  
                    ),
                
                tabPanel('Source', 
                         shinyDirButton('folder', 'Folder select', 'Please select a folder'),
                          #radioButtons('re_melt','re melt',c(F,T)),
                         #verbatimTextOutput('dir_text'),
                         verbatimTextOutput('wd_path_print'),
                         #verbatimTextOutput('data_file_list'),
                         tableOutput('data_df_table')
                         
                         #actionButton('add_data','Add Data')
                         ),
                
                #### TABLES ####
                tabPanel('Tables',
                         tabsetPanel(
                           tabPanel('Single Data Set',
                            uiOutput('select_sample_single_ui'), #single_sample        
                            tabsetPanel(
                              tabPanel('ALL', dataTableOutput('data_table')),
                              tabPanel('sig', dataTableOutput('sig_table'))
                           )),
                           #tabPanel('GE',dataTableOutput('GE_table')),
                           #tabPanel('SILAC',dataTableOutput('SIALC_table')),
                           #tabPanel('NES_Diff',dataTableOutput('NES_Diff_table')),
                           #tabPanel('NS_Diff',dataTableOutput('NS_Diff_table')),
                           tabPanel('Datasets combined',
                             column(3,radioButtons('re_run','re-run mapping', c(T,F),selected = F)),
                             column(3,radioButtons('re_melt','re-melt',c(T,F),selected = F)),
                             
                             column(3,radioButtons('background_re_run','background_re_run',c(T,F),selected = F)),
                             
                             column(3,radioButtons('background','background',c('all_mapped','subset?'),selected = 'all_mapped')),
                             column(12,uiOutput('removed_list_ui')),
                             column(12,tabsetPanel(
                               tabPanel('All Data', dataTableOutput('all_mapped_table')),
                               tabPanel('Significant Data',dataTableOutput('mapped_table')),
                               tabPanel('Significant Data - wide', dataTableOutput('mapped_ud_table')),
                               tabPanel('m',
                                        dataTableOutput('m_table')
                                        ),
                               tabPanel('m_ts',dataTableOutput('m_ts_table'))
                             ))),
                           tabPanel('gene_list',
                                    actionButton('write_gene_list','Write Gene List to file'),
                                    verbatimTextOutput('gene_list_print_test'))
                           #tabPanel('m_ts',dataTableOutput('m_ts_table'))
                         )
                ),
      
                ##### SELECT DATA ####
                tabPanel('Select Data',
                         column(6, radioButtons('data_type_radio','Select Data',choiceNames = c('List','Uniprot','Enrichment',"Venn"),choiceValues = c('list','uniprot','enrichment','venn'),selected = 'list',inline = T)),
                         column(3, actionButton('store_button', 'Store Gene List')),
                         column(1,downloadButton('gene_list_download')),
                         
                         
                         column(12,
                    ### _list ####
                         conditionalPanel(condition = "input.data_type_radio == 'list'",
                                          radioButtons('list_type','List',choices =  c('None','Saved','Prefix','File'),selected = 'None',inline = T),
                                          uiOutput('select_gene_file_prefix_ui'),
                                          uiOutput('select_gene_file_prefix_ui_run'),
                                          uiOutput('select_gene_file_ui_2'),
                                          uiOutput('select_gene_list_ui'),
                                          textOutput('gene_list_save')),

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
                                         radioButtons('fix_term','Fixed Term',choices = c(T,F),selected=F,inline = T),
                                         
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
                                column(12,uiOutput('venn_id_select')),
                                
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
                         
                         ),
                    column(12,plotOutput('selected_heatmap_base'))
                    
                    ),
             
                    ##### HEATMAPS #####
                            tabPanel('Heatmaps',tabsetPanel(
                              tabPanel('mean',
                                    radioButtons('heatmap_sig',"Show only significant data",c(T,F)),
                                    numericInput('heatmap_ratio_factor','Ratio Factor',10),
                                    textInput('heatmap_ylab','y label', 'Gene Symbols'),
                                    textInput('heatmap_xlab','x label', 'Experiments'),
                                    
                                     plotOutput('selected_heatmap')
                                    #plotOutput('selected_heatmap_ggplot')
                                    
                                     ),
                              tabPanel('Full',
                                    #uiOutput('select_sample_single_ui_2'),
                                      uiOutput('heatmap_sample_select'),
                                     plotOutput('full_selected_heatmap')
                              )
                                     
                                      )),
                
                #### BOXPLOTS ####
                
                            tabPanel('BoxPlots', id = 'boxplot',
                                     tabsetPanel(
                                       tabPanel('ggplot', 
                                                  
                                                
                                                  
                                                           #column(6,sliderInput('boxplot_range','Boxplot Range',min = 0,max = 10,sep = 1, value = c(0:3))),
                                                           column(7,#uiOutput('boxplot_range_ui'),
                                                                  uiOutput('boxplot_gene_select')),
                                                           column(2,radioButtons('boxplot_subset','Sub',c('select','all'))),
                                                           column(3,selectInput('boxplot_type_select','Select Plot Type',c('boxplot','dotplot','violin'),c('boxplot','dotplot'), multiple = T)),
                                                           column(12,
                                                           column(3,radioButtons('re_run_boxplots','Re Run Boxplots',c(T,F),selected = F)),
                                                           column(3,radioButtons('boxplot_sd_lim','sd cutoff',c(T,F))),
                                                           column(3,radioButtons('boxplot_p_values','include p value',c(T,F))),
                                                           column(3,radioButtons('boxplot_facets','Add Facets',c(T,F)))
                                                           ),
                                                           #selectInput('boxplot_type_select','Select Plot Type',c('boxplot','dotplot','violin')),
                                                           column(12,
                                                           column(2,numericInput('boxplot_title_size','Title Size',value = 24)),
                                                           column(2,numericInput('boxplot_text_size','Text Size',value = 15)),
                                                           column(2,numericInput('boxplot_x_axis_size','X Axis Size',value = 15)),
                                                           column(2,numericInput('boxplot_y_axis_size','Y Axis Size',value = 14)),
                                                           column(2,numericInput('boxplot_dotplot_size','dot size',value = 0.25)),
                                                           column(2,numericInput('boxplot_p_value_size','p value size',value = 6))
                                                           
                                                           ),
                                                tabsetPanel(selected = 'Gene',
                                                            
                                                           #actionButton('gene_boxplot','Generate Plots'),
                                                  tabPanel('Gene',
                                                           column(6,uiOutput('g_boxplots_plots')),
                                                           column(6,uiOutput('g_boxplots_plots_ts'))),
                                                  tabPanel('Sample',
                                                           actionButton('sample_boxplot','Generate Plots'),
                                                           uiOutput('g_boxplots_plots_sample')),
                                                  tabPanel('All', plotOutput('boxplot_gplot'))
                                                  #tabPanel('time series',
                                                  #         plotOutput('timeseries_boxplot'),
                                                  #         actionButton('gene_boxplot_ts','Generate Plots')
                                                           #uiOutput('g_boxplots_plots_ts')
                                                           
                                                  #         )
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
     # tabPanel('Enrichment',
     #   column(4,selectInput('select_sn_MT','select methodMT',string_db_methodMT_list)),
     #   column(4,selectInput('iea','Electronic Inferred Annotations',c(FALSE,TRUE))),
     #   column(12,
     #   tabsetPanel(
     #   tabPanel('Heatmap',
     #            actionButton('generate_enrichment_heatmap','Generate Heatmap'),
     #            selectInput('eh_fdr','fdr',c(0.05,0.01,0.001,0.0001,0.00001,0.000001),selected = 0.0001),
     #            uiOutput('enrichement_data_select_ui'), #input$enrichment_data_select
     #            #verbatimTextOutput('enrichment_list_print')
     #            plotOutput('enrichment_heatmap')
     #            ),
     #   tabPanel("Full",
     #            #c('Component', 'Function','Process', 'KEGG','Pfam','InterPro', 'Tissue','Disease')
     #            tags$h3('Component'),
     #            dataTableOutput('sn_en_table_Component'),
     #            tags$h3('Function'),
     #            dataTableOutput('sn_en_table_Function'),
     #            tags$h3('Process'),
     #            dataTableOutput('sn_en_table_Process'),
     #            tags$h3('KEGG'),
     #            dataTableOutput('sn_en_table_KEGG'),
     #            tags$h3('InterPro'),
     #            dataTableOutput('sn_en_table_InterPro'),
     #            tags$h3('Tissue'),
     #            dataTableOutput('sn_en_table_Tissue'),
     #            tags$h3('Disease'),
     #            dataTableOutput('sn_en_table_Disease')
     #            ),
     #   tabPanel('Select',
     #          column(4,selectInput('select_enrichment',"select_enrichment",string_db_enrichment_list)),
     #          #column(4,selectInput('select_sn_MT','select methodMT',string_db_methodMT_list)),
     #          #column(4,selectInput('iea','Electronic Inferred Annotations',c(FALSE,TRUE))),
     #          column(12,
     #          #textOutput('sn_en_print'),
     #          #dataTableOutput('df_gene_list'),
     #          dataTableOutput('sn_en_table')
     #          ))))),
    tabPanel('Enrichment',
             column(9,radioButtons('enrichment_select','Enrichment',choices = c('topGO','STRINGdb','AnimalTFDB'),selected = 'topGO',inline = 'T'),
                    #run_enrich_test
                    #plot_values
                    
                    column(3,uiOutput('select_enrichment_stat_ui')),
                    column(3,selectInput('iea','Electronic Inferred Annotations',c(FALSE,TRUE))),
                    column(3,selectInput('background','Select backgroundV', c('all_mapped','sig_mapped', 'NULL')))
             ),
             column(3,
                    radioButtons('enrich_re_run', 'Enrichment re-run', c(F,T),inline = T),
                    radioButtons('background_re_run', 'Background re-run', c(F,T),inline = T)
             ),
 
             
             column(12,            
                    textOutput('backgroundV_length'),
                    
                    #box(verbatimTextOutput('en_console'),width = 12,height = 10),
                    #### _SELECT ####
                    tabsetPanel(selected = 'Select',
                                #tabPanel('Heatmap',
                                #          actionButton('generate_enrichment_heatmap','Generate Heatmap'),
                                #          selectInput('eh_fdr','fdr',c(0.05,0.01,0.001,0.0001,0.00001,0.000001),selected = 0.0001),
                                #          uiOutput('enrichement_data_select_ui'), #input$enrichment_data_select
                                #          #verbatimTextOutput('enrichment_list_print')
                                #          plotOutput('enrichment_heatmap')
                                #          ),
                                #### __RUN ####
                                tabPanel("Run",
                                         
                                         #c('Component', 'Function','Process', 'KEGG','Pfam','InterPro', 'Tissue','Disease')
                                         tags$h5('Be patient this takes some time ... '),
                                         #tags$h5('background length'),
                                         tags$h3('Component'),
                                         dataTableOutput('sn_en_table_Component'),
                                         tags$h3('Function'),
                                         dataTableOutput('sn_en_table_Function'),
                                         tags$h3('Process'),
                                         dataTableOutput('sn_en_table_Process'),
                                         tags$h3('KEGG'),
                                         dataTableOutput('sn_en_table_KEGG'),
                                         tags$h3('Pfam'),
                                         dataTableOutput('sn_en_table_Pfam'),
                                         tags$h3('InterPro'),
                                         dataTableOutput('sn_en_table_InterPro'),
                                         tags$h3('Tissue'),
                                         dataTableOutput('sn_en_table_Tissue'),
                                         tags$h3('Disease'),
                                         dataTableOutput('sn_en_table_Disease')
                                ),
                                #### __Select ####
                                tabPanel('Select',
                                         column(4,selectInput('select_enrichment',"select_enrichment",string_db_enrichment_list,selected = string_db_enrichment_list[1],multiple = T)),
                                         column(4,uiOutput('sn_term_select_ui') 
                                         ),
                                         column(4, actionButton('store_button_3', 'Store Gene List')),
                                         #selectInput('eh_fdr','fdr',c(1,0.1,0.05,0.01,0.001,0.0001,0.00001,0.000001),selected = 0.01),
                                         numericInput('eh_fdr','fdr', value = 0.01),
                                         
                                         
                                         #column(4,selectInput('select_sn_MT','select methodMT',string_db_methodMT_list)),
                                         #column(4,selectInput('iea','Electronic Inferred Annotations',c(FALSE,TRUE))),
                                         column(12,
                                                tabsetPanel(selected = "Combined",
                                                  #### ___Singe ####
                                                  tabPanel('Single', value = 'single',
                                                           textOutput('venn_int_text_print'),
                                                           tabsetPanel(
                                                             tabPanel('Table',
                                                                      #textOutput('sn_en_print'),
                                                                      #dataTableOutput('df_gene_list'),
                                                                      dataTableOutput('sn_en_table')
                                                             ),
                                                             tabPanel('Plot',
                                                                      column(4,
                                                                             selectInput('p_range','p value cutoff', c(0.001,0.01,0.05,1), 0.05),
                                                                             uiOutput('sn_term_select_plot_ui')
                                                                             
                                                                      ),
                                                                      column(8,tabsetPanel(
                                                                        tabPanel('Single Barplot',
                                                                                 
                                                                                 plotOutput('sub_enrichment_plot')
                                                                        ),
                                                                        tabPanel('Heirarchy',
                                                                                 plotOutput('topGO_heirarchyPlot')
                                                                                 
                                                                        )
                                                                      )))
                                                             
                                                           )
                                                  ), ## Single
                                                  #### ___Combined ####
                                                  tabPanel('Combined', value = 'Combined',
                                                           column(12,uiOutput('enrich_combined_slider')),
                                                           column(4,                    
                                                                  radioButtons('run_enrich_test', 'Run Enrichment', c(T,F),inline = T),

                                                                  radioButtons('sub_venn','',c('basic','all','combined'), inline = T),
                                                                  uiOutput('enrichement_data_select_ui'), #input$enrichment_data_select
                                                                  textInput('enrichment_grep','Search (separated by ;)',value = ''),
                                                                  radioButtons('combined_order','Order',c('Alphabetical','p value'), selected = 'p value'),
                                                                  uiOutput('sn_term_select_combined_ui'),
                                                                  radioButtons('fixed_term_combined','Fixed Term',choices = c(F,T),selected=F,inline = T),
                                                                  column(6,numericInput('combined_height', 'height', 0.9)),
                                                                  column(6,numericInput('combined_width', 'width', 0.9)),
                                                                  column(6,numericInput('text_wrap', 'wrap', 30)),
                                                                  column(6,numericInput('border','border',2)),
                                                                  column(6,numericInput('t_size','text size',15))
                                                           ),
                                                           
                                                           column(8, 
                                                                  tabsetPanel(
                                                                    tabPanel('Table',

                                                                             dataTableOutput('combined_enrichment_table')
                                                                    ),
                                                                    tabPanel('Plot',
                                                                             
                                                                             #uiOutput('enrich_combined_slider'),
                                                                             tabsetPanel(
                                                                               tabPanel('Barplot',
                                                                                        selectInput('plot_values','Select plot_values', c('p_log','num')),
                                                                                        
                                                                                        plotOutput('combined_enrichment_plot'), height = 1200, width = 600),
                                                                               tabPanel('Heatmap', 
                                                                                        plotOutput('combined_heatmap_plot')
                                                                               ),
                                                                               tabPanel('Hierarchy',
                                                                                        column(3,numericInput('firstSigNodes','firstSigNodes', min = 0,value = 0)),
                                                                                        column(3,numericInput('putCL','putCL',min = 0,value = 0)),
                                                                                        column(3,radioButtons('putWN','putWN',c(T,F),inline = T)),
                                                                                        column(3,radioButtons('sigForAll','sigForAll',c(T,F),inline = T)),
                                                                                        
                                                                                        column(3,selectInput('useInfo','useInfo',c("none", "pval", "counts", "def", "np", "all"), selected = 'all',multiple = F)),
                                                                                        column(3,radioButtons('heir_sig','Only Significant scores',c(F,T),inline = F)),
                                                                                        column(3,radioButtons('heir_re_run','run',c(T,F), selected = F, inline = F)),
                                                                                        numericInput('heir_w','width',400,step = 100),
                                                                                        numericInput('heir_cex','cex',0.1,step = 0.05),
                                                                                        uiOutput('heir_select_enrichment'),
                                                                                        imageOutput('sig_topGO_heirarchyPlot')
                                                                                        
                                                                                        
                                                                               ),
                                                                               ####___ VENN ####
                                                                               tabPanel('Venn',
                                                                                        radioButtons('sub_venn_select','set', c('all','subset')),
                                                                                        plotOutput('sub_venn_plot'))
                                                                               
                                                                             ))
                                                                  )
                                                           )
                                                  ) ## combined
                                                )))
                                
                                
                                
                    ))),
    
             
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
