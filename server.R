library(shiny)
shinyServer(function(input, output) {
  #########
  observeEvent(input$debug_button,{
    print('debug')
    browser()
    print('debug')
    print('debug')
    print('debug')
  })
  
  shinyDirChoose(input, 'folder', roots=c(data = wd_path))
  
  #shinyDirChoose(input, 'dir', roots = c(home = '/Users/sgarnett/Documents/Doctorate/Thesis/Thesis_Data/Cleanup_Data'), filetypes = c('', 'txt'))
  
  output$dir_text = renderPrint({
    print(input$folder)
    print(paste(paste(input$folder$path),collapse='/'),collapse='/')
    print(paste(c(data_root,paste(paste(input$folder$path),collapse='/')),collapse= '/'))
  })
  
  output$plot_path_text = renderText((values$plot_path))
  #string_db = readRDS("string_db")
  
  wd_path_select = reactive({
    print('wd_path_select')
    if(!is.null(input$folder)){
      wd_path = paste(c(data_root,paste(paste(input$folder$path),collapse='/')),collapse= '/')
      setwd(wd_path_select)
    }else{
      wd_path_select = wd_path
    }
    setwd(wd_path_select)
    print('   wd_path_select : done')
    
    wd_path_select
  })
  
  output$wd_path_print = renderPrint(print(wd_path_select()))
  output$data_file_list = renderPrint({
    #print('data_file_list')
    file_list = list.files('./data/',pattern = '.rds')
    if(length(file_list) == 0){
      file_list = list.files('./data/')
    }else{
      file_list = gsub('.rds','',file_list)
    }
    print(file_list)
  })
  
  
  
  load_data = reactive({
    print('load_data')
    file_path = paste0(shiny_data_path,'data_df.rds')
    data_df = readRDS(file_path)
    ts_file_path = paste0(shiny_data_path,'timeseries_list.rds')
    if(file.exists(ts_file_path)){
      timeseries_list = readRDS(ts_file_path)
    }else{
      timeseries_list = list()
    }
    
    print('   load_data : done')
    
    list(data_df = data_df, timeseries_list = timeseries_list)
    
  })
  
  #data_df = readRDS('data_df.rds')
  data_df = reactive({
    print('data_df')
    #print('shiny_data/data_df.rds')
    file_path = paste0(shiny_data_path,'data_df.rds')
    #print(file_path)
    data_df = readRDS(file_path)
    
    print('   data_df : done')
    
    data_df
    #load_data()$data_df
    })
  
  timeseries_list = reactive({
    print('timeseries_list')
    df = load_data()$timeseries_list
    print('   timeseries_list : done')
    df
    })
  
  output$data_df_table = renderTable(data_df(),rownames = TRUE)
  
  sample_names = reactive({
    print('sample_name')
    df = rownames(data_df())
    print('   sample_name : data')
    df
    })
  
  data_name_list = reactive({
    print('data_name_list')
    data_df = data_df()
    dl = data_df$data_list
    names(dl) = rownames(data_df)
    print('   data_name_list : done')
    
    dl
    #paste(unlist(dl['ESC']))
    })
  ### __select_sample ####
  output$select_sample_ui = renderUI({
    print('select_sample_ui')
    if(input$boxplot_full == 'subset'){
      selectInput(inputId = 'data',  # Drop down menu to select the producer and cultivar
                  label = 'Select Samples',
                  choices = sample_names(),
                  selected = sample_names()[c(1)],
                  multiple = T)

    }else{
      selectInput(inputId = 'data',  # Drop down menu to select the producer and cultivar
                  label = 'Select Samples',
                  choices = sample_names(),
                  selected = sample_names(),
                  multiple = T)
      }
  })
  
  output$select_sample_single_ui = renderUI({
    print('select_sample_single_ui')
    if(length(input$data) == 1){
      data_choices = input$data
    }else(
      data_choices = c(input$data,data_name_collapse())
    )
    selectInput(inputId = 'single_sample',  # Drop down menu to select the producer and cultivar
                label = 'Select Sample',
                choices = data_choices,
                selected = data_choices[1],
                multiple = F)
  })
  
  data_name_collapse = reactive({
    print('data_name_collapse')
    data_list = unlist(input$data)
    data_list = data_list[order(data_list)]
    df = paste(data_list,collapse = '_')
    print('   data_name_collapse : done')
    df
  })
  # output$select_sample_single_ui_1 = renderUI({
  #   selectInput(inputId = 'single_sample_1',  # Drop down menu to select the producer and cultivar
  #               label = 'Select Sample',
  #               choices = sample_names(),
  #               selected = sample_names()[1],
  #               multiple = F)
  # })
  # 
  # output$select_sample_single_ui_2 = renderUI({
  #   selectInput(inputId = 'single_sample_2',  # Drop down menu to select the producer and cultivar
  #               label = 'Select Sample',
  #               choices = sample_names(),
  #               selected = sample_names()[1],
  #               multiple = F)
  # })
  # 
  # output$select_sample_single_ui_3 = renderUI({
  #   selectInput(inputId = 'single_sample_3',  # Drop down menu to select the producer and cultivar
  #               label = 'Select Sample',
  #               choices = sample_names(),
  #               selected = sample_names()[1],
  #               multiple = F)
  # })
  
  #### DATA LISTS #### 
  
  data_df_list <- reactive({
    print('data_df_list : start')
    
    
    #### ADD GSUB here for incorrect id #####
    
    id_sub_list = list(LIN28 = 'LIN28A')
    id_sub_list
    
    
    #print('data_df_list')
    plates.data <- list()
    #plates.data$Plate.1 <- list(subj.ids = sample_names())
    
    for(entry in input$data) {
      #print(getwd())
      #print("data/string_mapped/all_mapped.df_Huang2016_txt_edited_log2_ratio_different_mart_all_MCT_all_t_test.rds")
      cmd = paste0("data = readRDS('",mapped_data_path,"all_mapped.",unlist(data_name_list()[entry]),".rds')")
      cmd
      #print(cmd)
      eval(parse(text = cmd))
      dim(data)
      #print(dim(data))
      for(sub_entry in names(id_sub_list)){
        #print(sub_entry)
        #print(id_sub_list[sub_entry])
        grep(sub_entry,data$id,value = T)
        data$id = gsub(paste0('\\<',sub_entry,'\\>'),id_sub_list[[sub_entry]],data$id)
        }
        
      if(entry == 'ESC'){
        temp_df = plyr::ldply(data$log2_ratio_list,rbind)
        data = cbind(data,temp_df)
        colnames(data)
      }
      #print(dim(data))
      if('Accession' %in% colnames(data)){

        data$id_acc = data$id
        data$id_acc[duplicated(data$id)] = paste(data$id[duplicated(data$id)],data$Accession[duplicated(data$id)],sep = '_')
        #sub_data$id_acc
      }else{
      
        data$Accession = NA
        data$id_acc = data$id
      }
      dup_i = 1
      if(TRUE %in% duplicated(data$id_acc)){
        #print(data$id_acc[duplicated(data$id_acc)])
        
        data$id_acc[duplicated(data$id_acc)] = paste(data$id_acc[duplicated(data$id_acc)],dup_i,sep='__')
        
      }
      while(TRUE %in% duplicated(data$id_acc)){
        #print(data$id_acc[duplicated(data$id_acc)])
        data$id_acc[duplicated(data$id_acc)] = gsub(paste0('__',dup_i),paste0('__',dup_i+1),data$id_acc[duplicated(data$id_acc)])
        dup_i = dup_i + 1
      }
      
      
      #data$type = data_df$data[entry]

      
      print(dim(data))
      plates.data[[entry]] <- data
    }
    
    #print(names(plates.data))
    
    print('   data_df_list : done')
    plates.data
  }) # list of the all mapped dataframes - returned using data_df_list[[data_name]]
  
  all_gene_list = reactive({
    print('all gene_list')
    data_df_list = data_df_list()
    gene_list = c()
    for(entry in input$data){
      gene_list = c(gene_list,data_df_list[[entry]]$id)
    }
    gene_list = unique(gene_list)
    print('   all gene_list : done')
    
    gene_list
  })
  
  output$removed_list_ui = renderUI({
    print('remove_list_ui')
    selectInput('removed_list','Select Genes to Remove',all_gene_list(),c('SEP','HLA'),multiple = T)
  })
  
  sig_data_list = reactive({
    print('sig_data_list')
    sig_list = list()
    for(entry in input$data){
      data = data_df_list()[[entry]]

      t_test_list_entry = data_df()[entry,'p.value']
      cutoff_list_entry = data_df()[entry,'data']
      sd_cutoff = as.numeric(data_df()[entry,'sd_cutoff'])
      cmd = paste("reduced_data = data[data$",t_test_list_entry," < 0.05 & !is.na(data$",t_test_list_entry,"),]",sep='')
      eval(parse(text=cmd))
      sig_data = reduced_data[reduced_data[,cutoff_list_entry] < (-2*sd_cutoff) | reduced_data[,cutoff_list_entry] > (2*sd_cutoff),]
      sig_list[[entry]] = sig_data
      sig_up = sig_data[sig_data[,cutoff_list_entry] > 0,]
      sig_up = sig_up[!is.na(sig_up$STRING_id),]
      sig_list[[paste(entry,'up')]] = sig_up
      sig_down = sig_data[sig_data[,cutoff_list_entry] < 0,]
      sig_down = sig_down[!is.na(sig_down$STRING_id),]
      sig_list[[paste(entry,'down')]] = sig_down
      
    }
    print('   sig_data_list : done')
    
    sig_list
  }) # list of significant data tabels sig_data_list[[data_name]]
  
  
  output$result <- renderPrint({
    print(names(data_df_list()))
    for(entry in names(data_df_list())){
      #print(entry)
      print(dim(data_df_list()[[entry]]))
    }
    data_df_list()
  })
  

  
  #### Data Test #####
  # checks to see if all the id for the mapped files are valid
  table_name = 'SILAC_mapped_all'
  

  output$table_test = renderText({
    print('table_test')
    p_list = c()
    for(entry in data_name_list){
      #print(entry)
      table_name = paste0(entry,'_mapped_all')
      #print(table_name)
      p = test_table_ids_function(table_name)
      p
      p_list = c(p_list,p)
      
    }
    print('   table_test : done')
    
    print(paste(p_list,collapse = ' <br> '))
  })
  #### ####
  
  #### Data Tables ####
  
  all_mapped = reactive({
    print('all_mapped')
    df = data_df_list()[[input$single_sample]]
    print('   all_mapped : done')
    df
  })
  
  sig_mapped = reactive({
    print('sig_mapped')
    df = sig_data_list()[[input$single_sample]]
    print('   sig_mapped : done')
    df
  })

  sig_mapped_up = reactive({
    print('sig_mapped_up')
    data_colname = data_df[input$single_sample,'data']
    df = sig_mapped()[sig_mapped()[,data_colname] > 0,]
    print('   sig_mapped_up : done')
    
    df
  })
  
  sig_mapped_down = reactive({
    print('sig_mapped_down')
    data_colname = data_df[input$single_sample,'data']
    df = sig_mapped()[sig_mapped()[,data_colname] < 0,]
    print('   sig_mapped_down : done')
    df
  })
  
  output$data_table = renderDataTable(all_mapped())
  
  output$sig_table = renderDataTable(sig_mapped())
  output$GE_table = renderDataTable(GE_mapped_all)
  output$SILAC_table = renderDataTable(SILAC_mapped_all)
  output$NES_Diff_table = renderDataTable(NES_Diff_mapped_all)
  output$NS_Diff_table = renderDataTable(NS_Diff_mapped_all)
  output$topGO_sig = renderDataTable(enrichment_table())
  
  
  output$selected_data_table = renderDataTable({
    print('select_data_table')
    df = all_mapped()
    df = df[df$id %in% gene_list(),]
    print('   select_data_table : done')
    df
    
  })
  
  
  ### MAPPING DATA ####
  mapped_data = reactive({
    print('mapped data')
    start_time = Sys.time()
    data_list = input$data
    data_select_list = data_name_list()
    #print(data_list)
    #print(data_select_list)
    #print(data_name_collapse())
    file_name = paste0('common_all_mapped_',data_name_collapse(),'.rds')
    #print(file_name)
    #print(list.files(path = './GO_data/'))
    #print(input$re_run)
    #print(file_name %in% list.files(path = shiny_data_path))
    #print(input$data)
    if(!file_name %in% list.files(path = shiny_data_path)){
      
      mapped_data = common_mapped_function(shiny_data_path,sig_data_list(),input$data,'lightgreen',string_db(),data_df_list(),data_name_collapse(),input$removed_list)
    }else{
      if(input$re_run){
        mapped_data = common_mapped_function(shiny_data_path,sig_data_list(),input$data,'lightgreen',string_db(),data_df_list(),data_name_collapse(),input$removed_list)
        }else{
          print('readRDS - mapped data')
          mapped_data_file_name = paste0(shiny_data_path,'common_sig_mapped_',data_name_collapse(),'.rds')
          all_mapped_data_file_name = paste0(shiny_data_path,'common_all_mapped_',data_name_collapse(),'.rds')
          
          payload_rds_file_name = paste0(shiny_data_path,'common_sig_mapped_payload_id_',data_name_collapse(),'.rds')
          entry_list_file_name = paste0(shiny_data_path,'entry_list_',data_name_collapse(),'.rds')
          mapped_ud_file_name = paste0(shiny_data_path,'sig_mapped_ud_',data_name_collapse(),'.rds')
          colour_file_name = paste0(shiny_data_path,'colour_list_',data_name_collapse(),'.rds')
          id_file_list = paste0(shiny_data_path,'id_list_',data_name_collapse(),'.rds')
          removed_file_list = paste0(shiny_data_path,'removed_list_',data_name_collapse(),'.rds')
          gene_file_list = paste0(shiny_data_path,'gene_list_',data_name_collapse(),'.rds')
          all_gene_file_list = paste0(shiny_data_path,'all_gene_list_',data_name_collapse(),'.rds')
          
          mapped_data = list(mapped_data = readRDS(mapped_data_file_name),
                             all_mapped_data = readRDS(all_mapped_data_file_name),
                             payload_id = readRDS(payload_rds_file_name),
                             entry_list = readRDS(entry_list_file_name),
                             mapped_ud = readRDS(mapped_ud_file_name),
                             colour_list = readRDS(colour_file_name),
                             id_list = readRDS(id_file_list),
                             removed_list = readRDS(removed_file_list),
                             gene_list = readRDS(gene_file_list)
                             #all_gene_list = readRDS(all_gene_file_list)
                             )
        }
    }
    
    #mapped_data = common_mapped_function(input$data,'lightgreen',string_db,data_list)
    #str(mapped_data)
    #mapped_data$mapped_data
    #mapped_data$payload_id

    end_time = Sys.time()
    print(end_time - start_time)
    print('   mapped data : done')
    mapped_data
  })
  output$all_mapped_table = renderDataTable(mapped_data()$all_mapped_data)
  
  output$mapped_table = renderDataTable(mapped_data()$mapped_data)
  output$mapped_ud_table = renderDataTable(mapped_data()$mapped_ud)
 
  mapped_st_single_list = reactive({
    print('mapped_st_single_list')
    file_path = 'data/mapped_st_single_list.rds'
    if(!file.exists(file_path)){
      saveRDS(list(),file_path)
    }
    mapped_st_single_list = readRDS('data/mapped_st_single_list.rds')
    if(length(input$data) == 1){
      mapped_st_single_list[[input$data]] = mapped_st()
    }
    saveRDS(mapped_st_single_list,file_path)
    print('   mapped_st_single_list : done')
    
    mapped_st_single_list
  })
  
  all_mapped_st = reactive({
    print('all_mapped_st')
    all_mapped = FALSE
    for(data_name in input$data){
      if(all_mapped == FALSE){
        all_mapped = mapped_st_single_list()[[data_name]]
      }else{
        all_mapped = rbind(all_mapped,mapped_st_single_list()[[data_name]])
      }
    }
    print('   all_mapped_st : done')
    
    all_mapped
  })
  
  output$all_mapped_table_2 = renderDataTable(all_mapped_st())
  
  #observeEvent(length(input$data) == 1,{
  #  mapped_st_singgle_list()
  #})
  
   mapped_st = reactive({
    print('mapped_st')
    mapped = mapped_data()$mapped_data
    
    # if(length(input$mapped == 'common_mapped'){
    #   #mapped = readRDS('common_mapped.rds')
    #   
    # }else{
    #   mapped = sig_mapped()
    #   #print('readRDS')
    #   #file_name = paste0(input$mapped,".",input$single_data,".rds")
    #   #print(file_name)
    #   #mapped = readRDS(file_name)
    # }
    # print('dim(mapped)')
    # print(dim(mapped))
    # mapped
    print('   mapped_st : done')
    mapped
    
    
  }) # gets the STRINGdb mapping data
  
  payload_id = reactive({
    print('payload_id')
     #if(input$mapped == 'common_mapped'){
       #payload_id = readRDS('common_mapped_payload_id.rds')
       payload_id = mapped_data()$payload_id
     #}else{
       #payload_id = mapped_data()$payload_id
      # payload_id = readRDS(paste0(shiny_data_path,'payload_id.',data_name_collapse(),'.rds'))
     #}
    print('   payload_id : done')
       
    payload_id
    
  })
  
  #### ####
  
  
  output$table_name = renderText(paste("df.topGO_",input$limit,".",input$stat,".",input$ontology,".",input$data,".rds",sep=''))
  
  
  ##### ENRICHMENT ####
  
  output$select_enrichment_stat_ui = renderUI({
    print('select_enrichment_stat_ui : ')
    if(input$enrichment_select == 'STRINGdb'){
      selectInput('select_sn_MT','select methodMT',string_db_methodMT_list)
    }else{
      selectInput('select_sn_MT','select stat',c('classicFIsher','fisher.elim','fisher.weight01','fisher.lea','fisher.parentchild') ,selected = 'fisher.weight01')
    }
    })
  
  enrichment_column = reactive({
    print('enrichment_column')
    up_col = 'pvalue_fdr.up'
    down_col = 'pvalue_fdr.down'
    
    if(input$enrichment == 'topGO'){
      if(input$topGO_stat == 'All'){
        enrichment_table = full_enrichment_table
      }else{
        if(input$topGO_mtc == ''){
          up_col = paste0(input$topGO_stat,'.up')
          down_col = paste0(input$topGO_stat,'.down')
        }else{
          up_col = paste0(input$topGO_stat,'.',input$topGO_mtc,'.up')
          down_col = paste0(input$topGO_stat,'.',input$topGO_mtc,'.down')
        }
      }
    }
    print('   enrichment_column : done')
    
    list(up_col = up_col, down_col = down_col)
    })
  enrichment_table = reactive({
    print('enrichment_table')
    data_name = input$single_sample
    
    #load(paste('topGO_sig',input$ontology,input$data,sep='.'))
    if(input$enrichment == 'topGO'){
      #file_name = paste0(enrichment_data_path(),'/topGO_sig.',enrichment_abreviation_list[input$topGO_enrichment],'.',data_name_list()[data_name],'.rds')
      
      #print(input$data)
      #print(data_list[input$data])
      #file_name = paste("df.topGO_",input$limit,".",input$stat,".",input$ontology,".",input$single_data,".rds",sep='')
      #print(file_name)
      table_path = paste0(column_path(),'/',input$topGO_enrichment,'/topGO/',input$topGO_enrichment,'_all.rds')
      print(table_path)
      full_enrichment_table = readRDS(table_path)
      full_enrichment_table
      if(input$topGO_stat == 'All'){
        enrichment_table = full_enrichment_table
      }else{
        # if(input$topGO_mtc == ''){
        #   up_col = paste0(input$topGO_stat,'.up')
        #   down_col = paste0(input$topGO_stat,'.down')
        # }else{
        #   up_col = paste0(input$topGO_stat,'.',input$topGO_mtc,'.up')
        #   down_col = paste0(input$topGO_stat,'.',input$topGO_mtc,'.down')
        # }
        
        up_col = enrichment_column()$up_col
        down_col = enrichment_column()$down_col
        
        enrichment_table = full_enrichment_table[full_enrichment_table[,down_col] < 0.05 | full_enrichment_table[,up_col] < 0.05,]
        col_list = c("GO.ID" ,"Term" ,"Annotated" ,"Significant.down",down_col, "Expected.down","Significant.up", "Expected.up", up_col)
        enrichment_table = enrichment_table[,col_list]
        }
      
      print(colnames(enrichment_table))
      
      
    }
    if(input$enrichment == 'STRINGdb'){
      table_path = paste0(column_path(),'/',input$stringdb_enrichment,'/STRINGdb/',input$stringdb_enrichment,'_all.rds')
      print(table_path)
      enrichment_table = readRDS(table_path)
      if(input$stringdb_reduce == 'Significant'){
        enrichment_table = enrichment_table[enrichment_table$pvalue_fdr.down < 0.05 | enrichment_table$pvalue_fdr.up < 0.05,]
        enrichment_table = enrichment_table[!is.na(enrichment_table$Term),]
        #enrichment_table_down = enrichment_table[enrichment_table$pvalue_fdr.down < 0.05,]
        #enrichment_table_up = enrichment_table[enrichment_table$pvalue_fdr.up < 0.05,]
        #enrichment_table = rbind(enrichment_table_down,enrichment_table_up)
      }
    }
    if(input$enrichment == 'AnimalTFDB'){
      table_path = paste0('./database/AnimalTFDB/',input$tf_enrichment,'.rds')
      
      if(input$tf_map_button == 'full'){
        table_path = paste0(enrichment_data_path(),'/',input$tf_enrichment,'.full_rds')
      }
      print(table_path)
      enrichment_table = readRDS(table_path)
      enrichment_table
      #table_path = paste0('./images/enrichment/',data_name_list()[[input$single_data]],data_df()[input$single,'data']
    }
    print('   enrichment_table : done')
    
    enrichment_table
  })

  output$sub_enrichment_plot = renderPlot({
    print('sub_enrichment_plot')
    enrichment_table = sn_en()
    enrichment_table = enrichment_table[enrichment_table$term_description %in% input$enrich_select_term_plot,]
    plot_data = enrichment_table
    data_list = input$data
    print(data_list)
    data_df = load_data()$data_df
    data_order = rownames(data_df)
    data_order = data_order[data_order %in% data_list]
    col_list = data_df[data_order,'col']
    col_list
    term_list = rev(input$enrich_select_term_plot)
    print(data_order)
    print(col_list)
    print(term_list)
    
    plot_data$log_p = -log10(plot_data$pvalue_fdr)
    x_order = plot_data$term_description[order(plot_data$log_p)]
    y_limits = c(-log10(0.05),max(plot_data$log_p,na.rm=T))
    print(y_limits)
    p = ggplot(plot_data, aes(y = log_p,x = term_description)) + 
      geom_bar(stat = 'identity',fill = col_list) +
      #scale_x_discrete(limits = x_order) +
      geom_hline(yintercept = -log10(0.05), col = 'blue') +
      scale_x_discrete(limits = term_list, labels = function(x) str_wrap(x, width = input$text_wrap)) + 
      scale_fill_manual(breaks = data_order,values = col_list) +
      ylab('-log10(p value)') + 
      theme(axis.title.y = element_blank(), axis.text.y = element_text(face = 'bold'))+
      ggtitle(input$venn_int) + 
      coord_flip()
    print('   sub_enrichment_plot : done')
    
    print(p)
    
  })
  
  enrichment_image_path_list = reactive({
    print('enrichment_image_path_list')
    GO_terms = input$enrich_select_term_combined
    df = combined_enrichment()
    term_list = unique(df$term_description)
    term_list = term_list[order(term_list)]
    
    GO_terms = GO_terms[order(GO_terms)]
    match_list = match(GO_terms,term_list)
    match_list_line = paste(match_list,collapse = '_')
    en_line = paste(enrichment_abreviation_list[input$select_enrichment],collapse = '_')
    plot_path_list = c(sample_path_line(),'enrichment','Combined',input$enrichment_select,input$select_sn_MT,en_line,length(term_list))
    plot_path_line = path_line_function(plot_path_list)
    print('enrichment_image_path_list : done')
    
    list(plot_path_list = plot_path_list, plot_path_line = plot_path_line, match_list = match_list, match_list_line = match_list_line)
  })
  
  combined_enrichment = reactive({
    print('combined_enrichment')
    
 
          ontology_list = input$select_enrichment
          data_list = input$data
          sub_data_list = input$enrichment_data_select
          sub_data_list
          data_name = sub_data_list[1]
      
       
          enrichment_GO = FALSE
          for(ontology in ontology_list){
            if(input$sub_venn == 'basic'){
              for(data_name in data_list){
               for(sub_data_name in sub_data_list){
                if(grepl(data_name,sub_data_name)){
                  sub_sample_list = c(data_name,'venn',sub_data_name)
                  sub_sample_list
                  #ontology = input$select_enrichment
                  if(input$enrichment_select == 'STRINGdb'){
                    
                    enrichment_path_line = enrichment_path_line_function(shiny_image_path,sub_sample_list,ontology,input$background)
                  }
                  if(input$enrichment_select == 'topGO'){
                    enrichment_path_line = enrichment_path_line_function(shiny_image_path,sub_sample_list,ontology,NULL)
                    venn_path_line = paste(c(shiny_image_path,gene_list_select_list()$path_list),collapse = '/')
                    venn_path_line = latex_filename_function(venn_path_line)
                    venn_path_line
                  }
                  #enrichment_path_line = enrichment_path_line_function(shiny_image_path,sub_sample_list,input$select_enrichment,input$background)
                  file_name = paste0(input$enrichment_select,'.rds')
                  
                  file_path = paste(enrichment_path_line,file_name,sep = '/')
                  print(file_path)
                  #if(file.exists(file_path)){
                    print(data_name)
                    print(sub_data_name)
        
                    mapped_data = mapped_st_single_list()[[data_name]]
                    print('mapped_data')
                    sample_path_line = paste(shiny_image_path,data_name,sep='/')
                    STRING_id_list = unique(mapped_data$STRING_id[mapped_data$label == sub_data_name])
                    print('STRING_id_list')
                    print(length(STRING_id_list))
                    #string_db = string_db()
                    #path_list = sub_sample_list
                    #annot = annot()
                    #ens_list = STRING_id_list
                    
                    #saveRDS(ens_list,'temp/ens_list.rds')
                    #saveRDS(annot,'temp/annot.rds')
                    #ens_list = readRDS('temp/ens_list.rds')
                    #annot = readRDS('temp/annot.rds')
                    
                    print('run_enrichment_function')
                    #if(length(STRING_id_list) > 0){
                      
                      enrichment_GO_n = run_enrichment_function(ontology,sub_sample_list,STRING_id_list,string_db(),mapped_data,annot(),sample_path_line,backgroundV(),input)
                      print(dim(enrichment_GO_n))
                      if(dim(enrichment_GO_n)[1] > 0){
                        
                        #enrichment_GO_n = readRDS(file_path)
                        enrichment_GO_n$data = data_name
                        enrichment_GO_n$sub_data = sub_data_name
                        enrichment_GO_n$ontology = ontology
                        print(dim(enrichment_GO_n))
                        if(enrichment_GO == FALSE){
                          enrichment_GO = enrichment_GO_n
                        }else{
                          enrichment_GO = rbind(enrichment_GO,enrichment_GO_n)
                        }
                      }
                    }
                  #}
                #}
              }
            }
              }
            
            
            if(input$sub_venn == 'all'){
              data_name = data_name_collapse()
              
              for(sub_data_name in sub_data_list){
                #if(grepl(data_name,sub_data_name)){
                  sub_sample_list = c(data_name,'venn',sub_data_name)
                  #ontology = input$select_enrichment
                  if(input$enrichment_select == 'STRINGdb'){
                    
                    enrichment_path_line = enrichment_path_line_function(shiny_image_path,sub_sample_list,ontology,input$background)
                  }
                  if(input$enrichment_select == 'topGO'){
                    enrichment_path_line = enrichment_path_line_function(shiny_image_path,sub_sample_list,ontology,NULL)
                    venn_path_line = paste(c(shiny_image_path,gene_list_select_list()$path_list),collapse = '/')
                    venn_path_line = latex_filename_function(venn_path_line)
                    venn_path_line
                  }
                  enrichment_path_line
                  #enrichment_path_line = enrichment_path_line_function(shiny_image_path,sub_sample_list,input$select_enrichment,input$background)
                  file_name = paste0(input$enrichment_select,'.rds')
                  
                  file_path = paste(enrichment_path_line,file_name,sep = '/')
                  print(file_path)
                  #if(file.exists(file_path)){
                  print(data_name)
                  print(sub_data_name)
                  #if(input$sub_venn == 'all'){
                  mapped_data = mapped_st()
                  sample_path_line = sample_path_line()
                  dim(mapped_data)  
                  #}
                  # if(input$sub_venn == 'basic'){
                  #   #print(mapped_st_single_list())
                  #   mapped_data = mapped_st_single_list()[[data_name]]
                  #   sample_path_line = paste(shiny_image_path,data_name,sep='/')
                  #   sample_path_line
                  # }
                  label = gsub(':',' & ',sub_data_name)
                  print(label)
                  STRING_id_list = unique(mapped_data$STRING_id[mapped_data$label == label])
                  #print(STRING_id_list)
                  print(length(STRING_id_list))
                  #string_db = string_db()
                  #path_list = sub_sample_list
                  #annot = annot()
                  #topGO_all = topGO_all()
                  if(length(STRING_id_list) > 0){
                    enrichment_GO_n = run_enrichment_function(ontology,sub_sample_list,STRING_id_list,string_db(),mapped_data,annot(),sample_path_line,backgroundV(),input)
                    print(dim(enrichment_GO_n))
                    if(dim(enrichment_GO_n)[1] > 0){
                      #enrichment_GO_n = readRDS(file_path)
                      enrichment_GO_n$data = data_name
                      enrichment_GO_n$sub_data = sub_data_name
                      enrichment_GO_n$ontology = ontology
                      
                      print(dim(enrichment_GO_n))
                      if(enrichment_GO == FALSE){
                        enrichment_GO = enrichment_GO_n
                      }else{
                        enrichment_GO = rbind(enrichment_GO,enrichment_GO_n)
                      }
                    }else{
                      
                      print('no enriched terms')
                    }
                  }else{
                    print('no STRING_ids')
                    print(STRING_id_list)
                  }
                #}
                #}
              }
            }
              
              
            if(input$sub_venn == 'combined'){
                print('######### sub_venn == combined ##############')
                direction_list = c('down','up')
                print(direction_list)
                
                data_name = data_name_collapse()
                
                
                for(direction_entry in direction_list){
                  print(direction_entry)
                  print(direction_entry == 'down')
                  print(direction_entry == 'up')
                  #sub_data_list = input$enrichment_data_select
                  
                  if(direction_entry == 'down'){
                    sub_data_list = input$enrichment_data_select
                  }
                  if(direction_entry == 'up'){
                    sub_data_list = input$enrichment_data_select_2
                  }
                  print(sub_data_list)
                  sub_data_list = sub_data_list[order(sub_data_list)]
                  if(length(sub_data_list) > 0){
                #  for(sub_data_name in sub_data_list){
                    #if(grepl(data_name,sub_data_name)){
                    sub_data_name = paste(sub_data_list,collapse = ' & ')
                    sub_sample_list = c(data_name,'venn','combined',sub_data_name)
                    #ontology = input$select_enrichment
                    if(input$enrichment_select == 'STRINGdb'){
                      
                      enrichment_path_line = enrichment_path_line_function(shiny_image_path,sub_sample_list,ontology,input$background)
                    }
                    if(input$enrichment_select == 'topGO'){
                      enrichment_path_line = enrichment_path_line_function(shiny_image_path,sub_sample_list,ontology,NULL)
                      venn_path_line = paste(c(shiny_image_path,gene_list_select_list()$path_list),collapse = '/')
                      venn_path_line = latex_filename_function(venn_path_line)
                      venn_path_line
                    }
                    enrichment_path_line
                    #enrichment_path_line = enrichment_path_line_function(shiny_image_path,sub_sample_list,input$select_enrichment,input$background)
                    file_name = paste0(input$enrichment_select,'.rds')
                    
                    file_path = paste(enrichment_path_line,file_name,sep = '/')
                    print(file_path)
                    #if(file.exists(file_path)){
                    print(data_name)
                    print(sub_data_name)
                    #if(input$sub_venn == 'all'){
                    mapped_data = mapped_st()
                    sample_path_line = sample_path_line()
                    dim(mapped_data)  
                    #}
                    # if(input$sub_venn == 'basic'){
                    #   #print(mapped_st_single_list())
                    #   mapped_data = mapped_st_single_list()[[data_name]]
                    #   sample_path_line = paste(shiny_image_path,data_name,sep='/')
                    #   sample_path_line
                    # }
                    STRING_id_list = c()
                    for(label in sub_data_list){
                      label = gsub(':',' & ',label)
                      print(label)
                      single_STRING_id_list = unique(mapped_data$STRING_id[mapped_data$label == label])
                      single_STRING_id_list
                      STRING_id_list = c(STRING_id_list,single_STRING_id_list)
                    }
                    STRING_id_list = unique(STRING_id_list)
                    STRING_id_list = STRING_id_list[!is.na(STRING_id_list)]
                    #print(STRING_id_list)
                    print(length(STRING_id_list))
                    #string_db = string_db()
                    #path_list = sub_sample_list
                    #annot = annot()
                    #topGO_all = topGO_all()
                    if(length(STRING_id_list) > 0){
                      enrichment_GO_n = run_enrichment_function(ontology,sub_sample_list,STRING_id_list,string_db(),mapped_data,annot(),sample_path_line,backgroundV(),input)
                      print(dim(enrichment_GO_n))
                      if(dim(enrichment_GO_n)[1] > 0){
                        #enrichment_GO_n = readRDS(file_path)
                        enrichment_GO_n$data = data_name
                        enrichment_GO_n$sub_data = direction_entry
                        enrichment_GO_n$ontology = ontology
                        
                        print(dim(enrichment_GO_n))
                        if(enrichment_GO == FALSE){
                          enrichment_GO = enrichment_GO_n
                        }else{
                          enrichment_GO = rbind(enrichment_GO,enrichment_GO_n)
                        }
                      }else{
                        
                        print('no enriched terms')
                      }
                    }else{
                      print('no STRING_ids')
                      print(STRING_id_list)
                    }
                  }else{print('no sub_data_list')}
          
            }
            }
          }
          print(input$sub_venn)
          
          print(dim(enrichment_GO))
          #View(enrichment_GO)
          plot_data = enrichment_GO
      
          print('column data frame')
          if(plot_data != FALSE){
            system.time({
            plot_data$p_log = NA
            plot_data$p_log[grepl('up',plot_data$sub_data)] = -log10(plot_data$pvalue_fdr[grepl('up',plot_data$sub_data)])
            plot_data$p_log[grepl('down',plot_data$sub_data)] = log10(plot_data$pvalue_fdr[grepl('down',plot_data$sub_data)])
            plot_data$p_log
            
            plot_data$num = NA
            plot_data$num[grepl('up',plot_data$sub_data)] = (plot_data$hits[grepl('up',plot_data$sub_data)])
            plot_data$num[grepl('down',plot_data$sub_data)] = -(plot_data$hits[grepl('down',plot_data$sub_data)])
            plot_data$num
            
            })
          }
          
          #print(p_log)
          #plot_data$p_log = p_log
          #View(plot_data)
          print('   combined_enrichment : done')
          
          plot_data
    #}
  })
  
  output$sn_term_select_combined_ui = renderUI({
    print('sn_term_select_combined_ui : ')
    #if(input$run_enrich_test == TRUE){
      term_list = combined_term_list()$term_list
      term_list
      if(!is.null(input$combined_slider[1])){
        selected_list = term_list[c(input$combined_slider[1]:input$combined_slider[2])]
      }else{
        selected_list = term_list
      }
      
      selected_list
      #if(input$fixed_term_combined == T){
      #selected_list = input$enrich_select_term_combined
     # }
        #selected_list = term_list 
        
      selectInput('enrich_select_term_combined','Select Term',term_list, selected_list, multiple = T)
    #}
  })
  
  combined_term_list = reactive({
    print('combined_term_list')
    if(input$fixed_term_combined == F){
      df = combined_enrichment()
      df = df[df$pvalue_fdr < as.numeric(input$eh_fdr),]
      term_list = df$term_description
      term_list
      input$enrichment_grep
      if(input$enrichment_grep != ''){
        if(grepl(';',input$enrichment_grep)){
          search_list = unlist(strsplit(input$enrichment_grep,split = ';'))
        }else{
          search_list = input$enrichment_grep
        }
        #print(search_list)
        search_list = trimws(search_list)
        hit_list = c()
        for(entry in search_list){
          hit_list = c(hit_list,grep(entry,term_list,value = T))
        }
        #print(hit_list)
        term_list = term_list[term_list %in% hit_list]
        #print(term_list)
        df = df[df$term_description %in% hit_list,]
      }
      
      
      if(input$combined_order == 'Alphabetical'){
        df = df[order(toupper(df$term_description)),]
        term_list = df$term_description
        #term_list = term_list[order(term_list)]
      }else{
        df = df[order(df$pvalue_fdr),]
        term_list = df$term_description
        #term_list = term_list[order(term_list)]
      }
      
      
      
      
      #selected_list = term_list[c(input$combined_slider[1]:input$combined_slider[2])]
    }else{
      term_list = input$enrich_select_term_combined
      if(input$combined_order == 'Alphabetical'){
        term_list = term_list[order(term_list)]
      }
      #selected_list = input$enrich_select_term_combined
    }
    
    #print(term_list)
    #term_list = combined_enrichment()$term_description
    term_list = unique(term_list)
    term_list = list(term_list = term_list)
    print('   combined_term_list : done')
    
    term_list
  })
  
  observeEvent(input$fixed_term_combined,{
    if(input$fixed_term_combined == F){
      values$combined_term_list = input$enrich_select_term_combined
    }
  })
  
  output$combined_enrichment_table = renderDataTable({
    print('combined_enrichment_table')
    df = combined_enrichment()
    dim(df)
    if(input$run_enrich_test == TRUE){
      #df = df[df$pvalue_fdr < input$eh_fdr,]
      term_list = input$enrich_select_term_combined
      term_list
      #term_list = df$term_description
      df = df[df$term_description %in% term_list,]
    }
    print('   combined_enrichment_table : data')
    
    df
    })
  
  combined_enrichment_plot_data = reactive({
    print('combined enrichment plot data')
    df = combined_enrichment()
    #View(plot_data)
    plot_data = df[df$pvalue_fdr < input$eh_fdr,]
    #plot_data = plot_data[c(0:10),]
    #print(dim(plot_data))
    
    term_list = input$enrich_select_term_combined
    #print(term_list)
    #term_list = term_list[c(0:10)]
    plot_data = plot_data[plot_data$term_description %in% term_list,]
    plot_data = plot_data[!is.na(plot_data$pvalue_fdr),]
    print('combined enrichment plot data : done')
    
    plot_data
  })
  
  output$enrich_combined_slider = renderUI({
    print('enrich_combined_slider : ')
    if(input$run_enrich_test == TRUE){
       term_list = combined_term_list()$term_list
       sliderInput('combined_slider','Select Subset Range', min = 0, max = length(term_list), value = c(0,10), step = 1,width = 1000)
    }
  })
  
  output$combined_enrichment_plot_ui = renderUI({
    print('combined_enrichment_plot_ui : ')
    if(input$run_enrich_test == TRUE){
      plotOutput('combined_enrichment_plot')
    }
  })
  
  output$combined_enrichment_plot = renderPlot({
    print('combined_enrichment_plot')
    if(input$run_enrich_test == T){
      #print('########### combined_enrichment_plot ##############')
      ontology_list = input$select_enrichment
      
      plot_data = combined_enrichment_plot_data()
      #View(plot_data)
      print(dim(plot_data))
      if(dim(plot_data)[1] > 0){
        #View(plot_data)
        
        #w = 1/(length(unique(plot_data$data))) * 3
        #w = input$combined_width
        w = 0.9
        #print(w)
        text_wrap = input$text_wrap
        data_list = unique(plot_data$data)
        data_list
        if(input$sub_venn == 'basic'){
          data_df = load_data()$data_df
          data_order = rownames(data_df)
          print(data_order)
          data_order = data_order[data_order %in% data_list]
          print(data_order)
          
          col_list = data_df[data_order,'col']
          col_list
          print(col_list)
          data_df$name = rownames(data_df)
          #View(data_df)
          #df_col_list = listpair(data_df[,c('name','col')])
          df_col_list = list()
          df_col_list = apply(data_df,1, function(x) df_col_list[x['name']] = x['col'])
          df_col_list
          #df_col_list['ESC']
          #df_col_list
        }
        if(input$sub_venn == 'all'){
          data_sub_list = unique(plot_data$sub_data)
          data_order = data_sub_list[order(data_sub_list)]
          print(data_order)
          
          col_list = rainbow(length(data_order))
          print(col_list)
          df_col_list = list()
          df_col_list[data_order] = col_list
          df_col_list
        }
        if(input$sub_venn == 'combined'){
          
          
          data_list == unique(plot_data$sub_data)
          data_order = data_list[data_list %in% c('down','up')]
          
          df_col_list = list(up = 'red',down = 'green')
          df_col_list
        }
        
        data_order
        df_col_list
        term_list = rev(input$enrich_select_term_combined)
        term_list
        
        save_test = T
        if(save_test == T){
          load_data = load_data()
          variable_list = c('load_data','plot_data','data_order','df_col_list','term_list')
          cmd_list = save_variable_function(variable_list)
          lapply(cmd_list, function(x) eval(parse(text = x)))
          try(save_input_function(input))
          read_test = F
          if(read_test == T){
            variable_list = c(variable_list)
            variable_list
            cmd_list = read_variable_function(variable_list)
            for(cmd in cmd_list){
              print(cmd)
              try(eval(parse(text = cmd)))
            }
          }
        }
        #test = F
        # if(test == TRUE){
        #   saveRDS(plot_data,'temp/plot_data.rds')
        #   saveRDS(data_order,'temp/data_order.rds')
        #   saveRDS(df_col_list,'temp/df_col_list')
        #   saveRDS(term_list,'temp/term_list')
        #   
        #   #print('test')
        #   
        #   plot_data = readRDS('temp/plot_data.rds')
        #   data_order = readRDS('temp/data_order.rds')
        #   df_col_list = readRDS('temp/df_col_list')
        #   term_list = readRDS('temp/term_list')
        # }
        w = 0.9
        text_wrap = 30
        print(load_data()$data_df)
        print(df_col_list)
        print(data_order)
        print(paste(df_col_list[data_order]))
        d = rev(seq(1,length(data_order)))
        names(d) = data_order
        
        
        e = plot_data[,'data']
        f = reorder(e, sapply(e, function(x) d[x]))
        #View(f)
        #str(f)
        plot_data$data = f
        #saveRDS(plot_data,'temp/plot_data.rds')
        if(input$sub_venn == 'basic'){
          p = ggplot(plot_data,aes_string(x = 'term_description',y = input$plot_values, fill = 'data')) 
          #geom_bar(stat = 'identity', width = w, position = 'dodge')
        }
        if(input$sub_venn == 'all' | input$sub_venn == 'combined'){
          p = ggplot(plot_data,aes_string(x = 'term_description',y = input$plot_values, fill = 'sub_data'))
        }
        if(length(ontology_list) > 1){
          p = p + geom_bar(stat = 'identity', width = w, position = 'dodge', size = input$border, aes(col = ontology))
        }else{
          p = p + geom_bar(stat = 'identity', width = w, position = 'dodge', size = 0.5, col = "black")
        }
       
        if(input$plot_values == 'p_log'){
    
          p = p + geom_hline(yintercept = -log10(0.05), col = 'blue') + 
          geom_hline(yintercept = log10(0.05), col = 'blue') + 
          ylab('log10(p value)')
        }else{
          p = p +  ylab('number')
        }
        p = p + scale_x_discrete(limits = term_list, labels = function(x) str_wrap(x, width = text_wrap))
        
              #scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) + 
        p = p +  geom_hline(yintercept = 0, col = 'black', lwd - 2)
          
          #scale_fill_manual(breaks = data_order,values = col_list) + 
        if(input$sub_venn == 'all'){
          p = p + scale_fill_manual(breaks = data_order, values = paste(df_col_list[data_order]))
        }else{
         p = p + scale_fill_manual(breaks = data_order, values = df_col_list[data_order])
        }
         
          #ylab('log10(p value)') +
          p = p + theme(axis.title.y = element_blank(), axis.text.y = element_text(face = 'bold', size = input$t_size))
          p = p + coord_flip()
          p = p + labs(colour = '', fill = '')
    
        #p = p + theme(legend.position=input$combined_legend)
     
        #}
    
            #geom_bar(stat = 'identity', width = w, position = 'dodge', size = 5, aes(colour = ontology)) + 
            #geom_hline(yintercept = -log10(0.05), col = 'blue') +
            #geom_hline(yintercept = log10(0.05), col = 'blue') +
            #geom_hline(yintercept = 0, col = 'black', lwd - 2) +
            #scale_x_discrete(limits = term_list, labels = function(x) str_wrap(x, width = input$text_wrap)) + 
            #scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) + 
            
            #scale_fill_manual(breaks = data_order,values = col_list) +
            #ylab('log10(p value)') + 
            #theme(axis.title.y = element_blank(), axis.text.y = element_text(face = 'bold')) + 
            #coord_flip()
        #}
      
        #file_name_path = paste(plot_path_line,file_name,sep='/') 
        
        #if(test == T){
        #  saveRDS(p, 'temp/p.rds')
        #}
        #print('almost done')
        print('   combined_enrichment_plot : done')
          
        print(p)
        
        if(input$save_plot == T){
          plot_path_line = enrichment_image_path_list()$plot_path_line
          print(plot_path_line)
          enrichment_path = enrichment_path()
          print(enrichment_path)
          file_name = paste(enrichment_path,paste('barplot',input$plot_values,enrichment_image_path_list()$match_list_line,sep = '_'),sep = '/')
          #print(input$plot_values)
          
          #print(enrichment_image_path_list()$match_list_line)
          print(file_name)
          values$plot_name = file_name
          print(plot_path())
          save_plot_function_2(values$plot_path,file_name)
          
          #save_plot_function_2(plot_path_line,file_name)
        }
      }else{
        print('no data in plot data')
        #View(plot_data)
        
      }
    }
  })
  
  output$combined_heatmap_plot = renderPlot({
    print('combine_heatmap_plot')
    ontology_list = input$select_enrichment
    ontology_list
    #plot_data = readRDS('plot_data.rds')
    plot_data = combined_enrichment_plot_data()
    #save_test = T
    if(save_test == T){
      variable_list = c('plot_data')
      cmd_list = save_variable_function(variable_list)
      lapply(cmd_list, function(x) eval(parse(text = x)))
      try(save_input_function(input))
      read_test = F
      if(read_test == T){
        variable_list = c(variable_list)
        cmd_list = read_variable_function(variable_list)
        for(cmd in cmd_list){
          print(cmd)
          try(eval(parse(text = cmd)))
        }
      }
    }
    
    plot_data = plot_data[!(plot_data$p_log > -1.3 & plot_data$p_log < 1.3),]
    plot_data
    #plot_data = plot_data[!(,]
    #print(dim(plot_data))
    plot_data$enr = NA
    plot_data$enr[plot_data$p_log < -1.3] = 'down'
    plot_data$enr[plot_data$p_log > 1.3] = 'up'
    #plot_data$p_log[plot_data$p_log < -1.3)
    #View(plot_data)
    
    if(input$sub_venn == 'basic'){
      p = ggplot(plot_data,aes(y = term_description,x = data, fill = enr)) 
      #geom_bar(stat = 'identity', width = w, position = 'dodge')
    }else{
    #if(input$sub_venn == 'all'){
      p = ggplot(plot_data,aes(y = term_description,x = sub_data, fill = sub_data))
      p = p + scale_fill_manual(values = cbPalette)
      
    }
    if(length(ontology_list) > 1){
      p = p + geom_tile(aes(col = ontology), size = input$border, width = input$combined_width, height = input$combined_height)
      
      #p = p + geom_bar(stat = 'identity', width = w, position = 'dodge', size = input$border, aes(col = ontology))
    }else{
      p = p + geom_tile(col = "black", size = input$border, width = input$combined_width, height = input$combined_height)
                        
      #p = p + geom_bar(stat = 'identity', width = w, position = 'dodge', size = 0.5, col = "black")
    }
    #p  = ggplot(plot_data, aes(x = data,y = term_description)) + 
    #  geom_tile(aes(fill = enr), colour = 'white') + 
    p = p +  scale_y_discrete(limits = rev(input$enrich_select_term_combined) , labels = function(x) str_wrap(x, width = input$text_wrap))
    p = p +  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_text(face = 'bold')) +
      labs(colour = '', fill = '')
    #if(input$combined_legend == F){
    #  p = p + theme(legend.position="none")
    #}
    print('   combine_heatmap_plot : done')
    
    print(p)
    if(input$save_plot == T){
      enrichment_path = enrichment_path()
      file_name = paste(enrichment_path,paste0('heatmap_',enrichment_image_path_list()$match_list_line),sep = '/')
      values$plot_name = file_name
      print(plot_path())
      
      #plot_path_line = enrichment_image_path_list()$plot_path_line
      #file_name = paste0('heatmap_',enrichment_image_path_list()$match_list_line)
      save_plot_function_2(values$plot_path,file_name)
    }
    print(p)
    #ggplot(plot_data, aes(sub_data,term_description)) + geom_tile(aes(fill = p_log), colour = 'white') + scale_fill_gradient2()
    
  })
 
  GOdata = reactive({
    print('GOdata')
    ontology = input$select_enrichment
    if(length(ontology) > 1){
      ontology = ontology[1]
    }
    path_list = gene_list_select_list()$path_list
    enrichment_path_line = enrichment_path_line_function(shiny_image_path,path_list,ontology,NULL)
    #print('try heirarchy plot')
    GOdata_path = paste0(enrichment_path_line,'/topGO_GOdata.rds')
    GO_data = readRDS(GOdata_path)
    print('   GOdata : data')
    
    GO_data
  })
  
  sig_GOdata = reactive({
    print('sig_GOdata')
    file_name_prefix = paste0(input$taxonomy,'_')
    if(input$iea == 'TRUE'){
      file_name_prefix = paste0(input$taxonomy,'_iea_')
      
    }
    ontology = input$heir_select_enrichment
    if(length(ontology) > 1){
      ontology = ontology[1]
    }
    ont = paste(enrichment_abreviation_list[ontology])
    
    if(input$iea == TRUE){
      annot_file_path = paste0(shiny_data_path,input$taxonomy,'_annot_list_all.rds')
    }else{
      annot_file_path = paste0(shiny_data_path,input$taxonomy,'_annot_list_no_EIA.rds')
    }
    #if(!file.exists(annot_file_path)){
    #  annot = annot()
    #}
    annot_list = readRDS(annot_file_path)
    
    sig_GOdata_path = paste0(sample_path_line(),'/',file_name_prefix,'_',ontology,'_sig_topGO_GOdata.rds')
    #print(sig_GOdata_path)
    re_run = FALSE
    if(!file.exists(sig_GOdata_path) | input$enrich_re_run == TRUE | re_run == TRUE){
      print('re run sigGOdata')
      geneNames_all_path = paste0(sample_path_line(),'/',file_name_prefix,'geneNames_all.rds')
      GO2gene_all_path = paste0(sample_path_line(),'/',file_name_prefix,'GO2gene_all.rds')
      gene2GO_all_path = paste0(sample_path_line(),'/',file_name_prefix,'gene2GO_all.rds')
      geneNames_path_list = list(geneNames_all_path = geneNames_all_path, GO2gene_all_path = GO2gene_all_path, gene2GO_all_path = gene2GO_all_path)
      topGO_geneNames_all(backgroundV,annot_list,geneNames_path_list,file_name_prefix,input$background_re_run)
      geneNames_all = readRDS(geneNames_all_path)
      gene2GO_all = readRDS(gene2GO_all_path)
      sig_geneList = topGO_mapping(mapped_st(),geneNames_all,annot_list)
      sig_GOdata = topGO_enrichment(ont,sig_geneList,gene2GO_all)
      saveRDS(sig_GOdata,sig_GOdata_path)
      
    }else{
      print('readRDS')
      sig_GOdata = readRDS(sig_GOdata_path)
      
    }
    #sig_GO_data = readRDS(sig_GOdata_path)
    print('   sig_GOdata : data')
    
    sig_GOdata
  })
  
  
    
  output$topGO_heirarchyPlot = renderPlot({
    print('topGO_heirarchyPlot')
    library(Rgraphviz)
    GOdata = GOdata()
    CL = 1
    
    GO_terms = input$enrich_select_term_plot
    topGO = sn_en()
    score = as.numeric(topGO[,'pvalue_fdr'])
    names(score) = topGO$GO.ID
    
    test = F
    if(test == T){
      saveRDS(GO_data,'temp/GODdata.rds')
      saveRDS(GO_terms,'temp/GO_terms.rds')
      saveRDS(score, 'temp/score.rds')
      saveRDS(topGO,'temp/topGO.rds')
      GOdata = readRDS('temp/GODdata.rds')
      GO_terms = readRDS('temp/GO_terms.rds')
      score = readRDS('temp/score.rds')
      topGO = readRDS('temp/topGO.rds')
    }
    GO_ids = topGO$GO.ID[topGO$term_description %in% GO_terms]
    GO_ids
    #nAgo = makeNodeAttrs(GOdata,fontsize = 30)
    par(cex = 0.2)
    print('   topGO_heirarchyPlot : done')
    
    showSigOfNodes(GOdata, score, firstSigNodes = NULL, wantedNodes = GO_ids, sigForAll = 0.01,  putCL = CL, useInfo = c('all','def','pval','np')[1], .NO.CHAR = 50)

    detach('package:Rgraphviz')
    
  })
  
  
  output$heir_select_enrichment = renderUI({
    print('heir_select_enrichment : ')
    selectInput('heir_select_enrichment','Select Enrichment',input$select_enrichment)
  })
  output$sig_topGO_heirarchyPlot = renderImage({
    print('sig_topGO_heirarchyPlot')
    enrichment_path = enrichment_path()
    file_name = paste(enrichment_path,paste0('heirarchy_',enrichment_image_path_list()$match_list_line),sep = '/')
    values$plot_name = file_name
    print(plot_path())
    if((!file.exists(png_plot_path())) || input$heir_re_run == TRUE){
      print('re-run')
      #sig_topGO_heirarchy_png = reactive({
        library(Rgraphviz)
        GOdata = sig_GOdata()
        CL = 1
        
        GO_terms = input$enrich_select_term_combined
        full_topGO = combined_enrichment()
        
        full_topGO = full_topGO[full_topGO$ontology == input$heir_select_enrichment,]
        #View(full_topGO)
        topGO = full_topGO
        if(input$heir_sig == T){
          topGO = full_topGO[as.numeric(full_topGO$pvalue_fdr) < as.numeric(input$eh_fdr),]
        }
        
        #View(topGO)
        score = as.numeric(topGO[,'pvalue_fdr'])
        names(score) = topGO$GO.ID
        #View(score)
        test = F
        if(test == T){
          saveRDS(GOdata,'temp/sig_GOdata.rds')
          saveRDS(GO_terms,'temp/GO_terms.rds')
          saveRDS(score, 'temp/score.rds')
          saveRDS(topGO,'temp/topGO.rds')
          GOdata = readRDS('temp/sig_GOdata.rds')
          GO_terms = readRDS('temp/GO_terms.rds')
          score = readRDS('temp/score.rds')
          topGO = readRDS('temp/topGO.rds')
        }
        GO_ids = topGO$term_id[topGO$term_description %in% GO_terms]
    
    
        #View(GO_ids)
        #nAgo = makeNodeAttrs(GOdata,fontsize = 30)
        firstSigNodes = input$firstSigNodes
        if(firstSigNodes == 0){
          firstSigNodes = NULL
        }
        sigForAll = input$sigForAll
        putCL = input$putCL
        putWN = input$putWN
        useInfo = input$useInfo
        print('heirarchy')
        #plot_path_line = enrichment_image_path_list()$plot_path_line
        #file_name = paste0('heirarchy_',enrichment_image_path_list()$match_list_line)
        #file_name_path = paste(plot_path_line,file_name,sep='/')
        
        
        #if(input$save_plot == T){
          #par(cex = input$heir_cex)
        #  showSigOfNodes(GOdata, score, firstSigNodes = firstSigNodes, wantedNodes = GO_ids, sigForAll = sigForAll,  putCL = putCL, useInfo = useInfo, .NO.CHAR = 50)
        #  file_output_path = save_plot_function_2(plot_path_line,file_name,c('pdf'))
        #  values$plot_name = file_output_path
          #par()
        #}
    
        
        #if(input$heir_re_run == TRUE){
          
          png(paste0(plot_path(),'.png'), width = 4, height = 4, units = 'in', res = 600)
          par(cex = input$heir_cex)
          showSigOfNodes(GOdata, score, firstSigNodes = firstSigNodes, wantedNodes = GO_ids, sigForAll = sigForAll,  putCL = putCL, useInfo = useInfo, .NO.CHAR = 50)
          dev.off()
        #}
        
        if(input$save_plot == T){
          #par(cex = 1)
          par(cex = input$heir_cex)
          showSigOfNodes(GOdata, score, firstSigNodes = firstSigNodes, wantedNodes = GO_ids, sigForAll = sigForAll,  putCL = putCL, useInfo = useInfo, .NO.CHAR = 50)
    
          save_plot_function_2(values$plot_path,file_name,c('pdf'))
        }
        
    }else{
      print('found image')
    }
    
    detach('package:Rgraphviz')
    #print(file_name_path)
    #file_name_path
    print('   sig_topGO_heirarchyPlot : done')
    
    list(src = paste0(plot_path(),'.png'), width = input$heir_w)
  })
  

  

  sub_venn_list = reactive({
    print('sub_venn_list')
    mapped_data = mapped_st()
    #print(dim(mapped_data))
    if(input$sub_venn_select == 'subset'){
      sub_data_list = input$enrichment_data_select
      #print(sub_data_list)
      sub_data_list = gsub(':',' & ',sub_data_list)
      #print(sub_data_list)
      mapped_data = mapped_data[mapped_data$label %in% sub_data_list,]
      #print(dim(mapped_data))
    }
    #View(mapped_data)
    
    annot = annot()
    enrichment_table = combined_enrichment()
    venn_gene_list = list()
    for(GO_term in input$enrich_select_term_combined){
      #GO_term = input$enrich_select_term
      
      string_hits = string_hits_function(enrichment_table,GO_term,mapped_data,annot())
      #print(string_hits)
      id_list = mapped_data()$id_list
      #print(id_list)
      gene_list = id_list[string_hits]
    venn_gene_list[[GO_term]] = paste(gene_list)
    }
    #print(venn_gene_list)
    print('   sub_venn_list : done')
    venn_gene_list
    
  })
  
  output$sub_venn_plot = renderPlot({
    print('sub_venn_plot')
    m = 1.5 # numbers
    n = 1 # sample labels
    colour_list = rainbow(length(sub_venn_list()),alpha = 0.5)
    par(bty = 'n', lty = 'blank')
    df = tryCatch({
    venn(sub_venn_list(), 
         cexil = m, cexsn = n, 
         ilabels = T, 
         counts = T, 
         zcolor = colour_list,
         ellipse = T)
    },error = function(e) {
      venn(sub_venn_list(), 
           cexil = m, cexsn = n, 
           ilabels = T, 
           counts = T, 
           zcolor = colour_list,
           ellipse = F)
    })
    
    if(input$save_plot == T){
      plot_path_line = enrichment_image_path_list()$plot_path_line
      print(plot_path_line)
      enrichment_path = enrichment_path()
      print(enrichment_path)
      file_name = paste(enrichment_path,paste0('venn_',enrichment_image_path_list()$match_list_line),sep = '/')
      #print(input$plot_values)
      
      #print(enrichment_image_path_list()$match_list_line)
      print(file_name)
      values$plot_name = file_name
      print(plot_path())
      save_plot_function_2(values$plot_path,file_name)
      
      #save_plot_function_2(plot_path_line,file_name)
    }
    
    print('   sub_venn_plot : done')
    
    df
  })

  
  output$test_table = renderTable(sub_enrichment_table())
  
  enrichment_plot_data = reactive({
    print('enrichment plot data')
    enrichment_table = enrichment_table()
    up_col = enrichment_column()$up_col
    down_col = enrichment_column()$down_col
    plot_data = data.frame(down = log10(as.numeric(enrichment_table[,down_col])), up = (log10(as.numeric(enrichment_table[,up_col]))*-1))
    
    Term = enrichment_table$Term
    #print(Term)
    GO.ID = enrichment_table$GO.ID
    Term[duplicated(Term)] == paste(Term[duplicated(Term)],GO.ID[duplicated(Term)],sep='_')
    #print(Term)
    #print(duplicated(Term))
    
    Term = paste(Term,GO.ID)
    rownames(plot_data) = Term
    #print(dim(plot_data))
    print('   enrichment plot data : data')
    
    plot_data
    })
  
  output$enrich_slider_2 = renderUI({
    print('enriche_slider_2 : ')
    m.enrich = enrichment_plot_data()
    sliderInput(inputId = 'm_range_2','enrichment range',min(0),dim(m.enrich[1]),value = c(0,dim(m.enrich[1]),dragRange=T))
  })
  
  output$enrichment_plot = renderPlot({
    print('enrichment_plot : ')
    
      m.enrich = enrichment_plot_data()
      par(xpd=FALSE,mar = c(4, 25, 4, 1) + 0.1,cex.axis=1,cex.main = 0.5,mfrow = c(1,1)) #c(bottom, left, top, right)
      # try(barplot(rev(m.enrich[,1]),beside=TRUE,names.arg = rev(rownames(m.enrich)),las=1,col=c('red'),xlim=c(min(m.enrich,na.rm=TRUE),max(m.enrich,na.rm=TRUE)),main = paste(enrichment,'topGO',stat_select),horiz = TRUE))
      # try(barplot(rev(m.enrich[,2]),beside=TRUE,col=c('green'),add=TRUE,horiz=TRUE,yaxt='n'))
      # try(abline(v = -log10(topGO_enrich_cutoff),col='blue'))
      # try(abline(v = log10(topGO_enrich_cutoff),col='blue'))
      # try(abline(v = 0,col='black'))
      
      range = c(input$m_range_2[1]:input$m_range_2[2])
      
      #par(xpd=FALSE,mar = c(4, 18, 4, 2) + 0.1,cex.axis=0.6,mfrow = c(1,1)) #c(bottom, left, top, right)
      barplot(rev(m.enrich[range,1]),beside=TRUE,names.arg = rev(rownames(m.enrich)[range]),las=1,col=c('red'),xlim=c(min(m.enrich,na.rm=TRUE),max(m.enrich,na.rm=TRUE)),horiz=TRUE,cex.main=0.5)
      barplot(rev(m.enrich[range,2]),beside=TRUE,yaxt='n',xaxt='n',col=c('green'),add=TRUE,horiz=TRUE,xlab= 'test')
      #barplot(t(m.enrich[c(1:40),]),beside=TRUE,names.arg = rownames(m.enrich)[c(1:40)],las=3,col=c('red','green'))
      abline(v = -log10(topGO_enrich_cutoff),col='blue')
      abline(v = log10(topGO_enrich_cutoff),col='blue')
      abline(v = 0,col='black')
      #m.enrich
    
   # barplot(as.matrix(enrichment_plot_data()),horiz = T,beside = T)
  })
  enrichment_table_full = reactive({
    print('enrichment_table_full')
    data_name = input$single_sample_3
    
    #load(paste('topGO_sig',input$ontology,input$data,sep='.'))
    if(input$enrichment == 'topGO'){
      file_name = paste0(enrichment_data_path(),'/topGO_sig.',enrichment_abreviation_list[input$topGO_enrichment],'.',data_name_list()[data_name],'.rds')
      
      #print(input$data)
      #print(data_list[input$data])
      #file_name = paste("df.topGO_",input$limit,".",input$stat,".",input$ontology,".",input$single_data,".rds",sep='')
      #print(file_name)
      enrichment_table = readRDS(file_name)
      enrichment_table
    }
    if(input$enrichment == 'STRINGdb'){
      table_path = paste0(column_path(),'/',input$stringdb_enrichment,'/STRINGdb/',input$stringdb_enrichment,'_all.rds')
      #print(table_path)
      enrichment_table = readRDS(table_path)
    }
    if(input$enrichment == 'AnimalTFDB'){
      table_path = paste0('./database/AnimalTFDB/',input$tf_enrichment,'.rds')
      
      if(input$tf_map_button == 'full'){
        table_path = paste0(enrichment_data_path(),'/',input$tf_enrichment,'.full_rds')
      }
      #print(table_path)
      enrichment_table = readRDS(table_path)
      enrichment_table
      #table_path = paste0('./images/enrichment/',data_name_list()[[input$single_data]],data_df()[input$single,'data']
    }
    print('   enrichment_table_full : done')
    
    enrichment_table
  })
  
  output$select_term_ui = renderUI({
    print('select_term_ui : ')
    selectInput(inputId = 'term_select',  # Drop down menu to select the producer and cultivar
                label = 'Select Term',
                choices = enrichment_table()$Term,
                selected = enrichment_table()$Term,
                multiple = F)
  })
  
  
  #### __STRINGdb ####
    #### ___new enrichment ####
  
    output$en_console = renderPrint(capture.output(sn_en_o()))  
  
    sn_en = reactive({
      print('sn_en')
      start = Sys.time()
      
      ontology = input$select_enrichment
      if(length(ontology) > 1){
        ontology = ontology[1]
      }
      #print(ontology)
      STRING_id_list = STRING_id_list_function(mapped_st(),gene_list())
      #print(gene_list_select_list()$path_list)
      #print(length(STRING_id_list))
      path_list = gene_list_select_list()$path_list
      string_db = string_db()
      mapped_st = mapped_st()
      annot = annot()
      sample_path_line = sample_path_line()
      backgroundV = backgroundV()
      enrichment_GO = run_enrichment_function(ontology,gene_list_select_list()$path_list,STRING_id_list,string_db(),mapped_st(),annot(),sample_path_line(), backgroundV(),input)
      #run_enrichment_function = function(ontology,path_list,STRING_id_list,string_db,mapped_st,annot,sample_path_line,backgroundV,input){
        
      #print(dim(enrichment_GO))
      print(Sys.time() - start)
      print('sn_en : done')
      return(enrichment_GO)
    })
  
  
  #  reactive({
  #   df = sn_en_o()
  #   if(input$enrichment_select == 'topGO'){
  #     df$term_id = df$GO.ID
  #     df$proteins = as.numeric(df$Annotated)
  #     df$hits = as.numeric(df$Significant)
  #     df$pvalue = as.numeric(df$classicFisher)
  #     df$pvalue_fdr = as.numeric(df[,input$select_sn_MT])
  #     df$term_description = df$Term
  #     
  #   }
  #   df
  #   
  # })
  output$sn_term_select_ui = renderUI({
    print('sn_term_select_ui : ')
    df = sn_en()
    sig_list = df$term_description[df$pvalue_fdr < 0.05]
    selectizeInput('enrich_select_term','Select Term',df$term_description, sig_list, multiple = F)
    
  })
  
  output$sn_term_select_plot_ui = renderUI({
    print('sn_term_select_plot_ui : ')
    df = sn_en()
    sig_list = df$term_description[df$pvalue_fdr < input$eh_fdr]
    sig_list = unique(sig_list[order(sig_list)])
    selectizeInput('enrich_select_term_plot','Select Term',sig_list, sig_list[c(1:10)], multiple = T)
    
  })
  


  

  #c('Component', 'Function','Process', 'KEGG','Pfam','InterPro', 'Tissue','Disease')
  
  output$backgroundV_length = renderText({
    print(paste('Gene list', length(gene_list()), '   ---   Background List',length(string_db()$backgroundV)))
  })
  
  output$sn_en_table_Component = DT::renderDataTable({
    STRING_id_list = STRING_id_list_function(mapped_st(),gene_list())
    enrichment_GO = run_enrichment_function('Component',gene_list_select_list()$path_list,STRING_id_list,string_db(),mapped_st(),annot(),sample_path_line(),backgroundV(),input)

    DT::datatable(enrichment_GO, list(pageLength = 5))
  })
  output$sn_en_table_Function = renderDataTable({
    STRING_id_list = STRING_id_list_function(mapped_st(),gene_list())
    
    enrichment_GO = run_enrichment_function('Function',gene_list_select_list()$path_list,STRING_id_list,string_db(),mapped_st(),annot(),sample_path_line(),backgroundV(),input)
    
    DT::datatable(enrichment_GO, list(pageLength = 5))
    })
  output$sn_en_table_Process = renderDataTable({
    STRING_id_list = STRING_id_list_function(mapped_st(),gene_list())
    
    enrichment_GO = run_enrichment_function('Process',gene_list_select_list()$path_list,STRING_id_list,string_db(),mapped_st(),annot(),sample_path_line(),backgroundV(),input)
    DT::datatable(enrichment_GO, list(pageLength = 5))
    })
  output$sn_en_table_KEGG = renderDataTable({
    STRING_id_list = STRING_id_list_function(mapped_st(),gene_list())
    
    enrichment_GO = run_enrichment_function('KEGG',gene_list_select_list()$path_list,STRING_id_list,string_db(),mapped_st(),annot(),sample_path_line(),backgroundV(),input)
    DT::datatable(enrichment_GO, list(pageLength = 5))
    })
  output$sn_en_table_Pfam = renderDataTable({
    STRING_id_list = STRING_id_list_function(mapped_st(),gene_list())
    
    enrichment_GO = run_enrichment_function('Pfam',gene_list_select_list()$path_list,STRING_id_list,string_db(),mapped_st(),annot(),sample_path_line(),backgroundV(),input)
    #View(enrichment_GO)
    DT::datatable(enrichment_GO, list(pageLength = 5))
  })
  
  output$sn_en_table_InterPro = renderDataTable({
    STRING_id_list = STRING_id_list_function(mapped_st(),gene_list())
    
    enrichment_GO = run_enrichment_function('InterPro',gene_list_select_list()$path_list,STRING_id_list,string_db(),mapped_st(),annot(),sample_path_line(),backgroundV(),input)
    DT::datatable(enrichment_GO, list(pageLength = 5))
    })
  output$sn_en_table_Tissue = renderDataTable({
    STRING_id_list = STRING_id_list_function(mapped_st(),gene_list())
    
    enrichment_GO = run_enrichment_function('Tissue',gene_list_select_list()$path_list,STRING_id_list,string_db(),mapped_st(),annot(),sample_path_line(),backgroundV(),input)
    DT::datatable(enrichment_GO, list(pageLength = 5))
    })
  output$sn_en_table_Disease = renderDataTable({
    STRING_id_list = STRING_id_list_function(mapped_st(),gene_list())
    
    enrichment_GO = run_enrichment_function('Disease',gene_list_select_list()$path_list,STRING_id_list,string_db(),mapped_st(),annot(),sample_path_line(),backgroundV(),input)
    DT::datatable(enrichment_GO, list(pageLength = 5))
     })
  
    output$sn_en_print = renderPrint(head(sn_en()))
    
    output$sn_en_table = renderDataTable({
      print('sn_en_table')
      start = Sys.time()
      
      df = sn_en()
      if(input$enrichment_select == 'STRINGdb'){
        df = df[df$pvalue_fdr < input$eh_fdr,]
      }
      print(Sys.time() - start)
      print('   sn_en_table : done')
      df
      }) 
    
    enrichment_list = reactive({
      print('enrichment_list')
      enrichment_list = list()
      data_list = list()
      for(entry in input$enrichment_data_select){
        sig_data = sig_data_list()[[entry]]
        enrichment_list[[entry]] = sig_data$STRING_id
        data_list[[entry]] = entry
      }
      print('   enrichment_list : done')
      
      list(enrichment_list = enrichment_list,data_list = data_list)
    })
    
    output$enrichment_list_print = renderPrint({
      print(enrichment_list())
    })
    
    observeEvent(input$generate_enrichment_heatmap,{
      output$enrichment_heatmap = renderPlot({
        string_db()$enrichment_heatmap(enrichment_list()$enrichment_list, input$enrichment_data_select, enrichmentType = 'Function', fdr_threshold = input$eh_fdr)
      })
    })
##### PATHS #####
  
  
  table_path = reactive({
    print('table_path')
    data_name = input$single_sample
    table_path = paste0(enrichment_path,data_name_list()[data_name],'/')
    print('   table_path : done')
    
    table_path
    })
  
  column_path = reactive({
    print('column_path')
    data_name =     data_name = input$single_sample
    
    column_path = paste0(table_path(),data_df()[data_name,'data'],'/')
    print('   column_path : done')
    
    column_path
  })
    
  enrichment_data_path = reactive({
    print('enrichment_data_path')
    table_path = ''
    data_name = input$single_sample_3
    if(input$enrichment == 'AnimalTFDB'){
       table_path = paste0(enrichment_path,data_name_list()[data_name],'/',data_df()[data_name,'data'],'/AnimalTFDB/',input$tf_enrichment)
          
    }
    if(input$enrichment == 'topGO'){
      table_path = paste0(enrichment_path,data_name_list()[data_name],'/',data_df()[data_name,'data'],'/',input$topGO_enrichment)
      
    }
    if(input$enrichment == 'STRINGdb'){
      table_path = paste0(enrichment_path,data_name_list()[data_name],'/',data_df()[data_name,'data'],'/',input$tf_enrichment,'/STRINGdb')
      
    }
    print('   enrichment_data_path : done')
    
    table_path
  })
  
  output$enrichment_data_path_print = renderPrint({
    #print('enrichment_data_path_print')
    print(input$tf_enrichment)
    print(enrichment_data_path())
    print(list.files(enrichment_data_path()))
  })
  
  topGO_path = reactive({paste(column_path(),'topGO',sep='/')})  
  
  output$enrichment_table = renderDataTable(enrichment_table())
  output$enrichment_table_full = renderDataTable(enrichment_table_full())
  
  
  output$GO_mapped_table = renderDataTable(mapped_st())
  
  temp = F
  if(temp == T){
    mapped_data = readRDS('sig_mapped.df_GE_log2_ratio_different_all_MCT_all.rds')
    df.topGO_total.sig.stat = readRDS("df.topGO_total.sig.stat.classicFisher.bonferroni.BP.df_GE_log2_ratio_different_all_MCT_all.rds")
    df.topGO_total.sig.stat
    barplot(as.matrix(df.topGO_total.sig.stat[1,c("fisher.weight01.down","fisher.weight01.up")]),beside = T)
    
    m.enrich = readRDS('m.enrich.fisher.weight01.BP.df_GE_log2_ratio_different_all_MCT_all.rds')
    barplot(rev(m.enrich[1,1]),beside=TRUE,names.arg = rev(rownames(m.enrich)[1]),las=1,col=c('red'),xlim=c(min(m.enrich,na.rm=TRUE),max(m.enrich,na.rm=TRUE)),main=paste(enrichment,'topGO',stat_select,r[3],r[4]),horiz=TRUE,cex.main=0.5)
    barplot(rev(m.enrich[1,2]),beside=TRUE,yaxt='n',xaxt='n',col=c('green'),add=TRUE,horiz=TRUE,xlab= 'test')
    #barplot(t(m.enrich[c(1:40),]),beside=TRUE,names.arg = rownames(m.enrich)[c(1:40)],las=3,col=c('red','green'))
    abline(v = -log10(topGO_enrich_cutoff),col='blue')
    abline(v = log10(topGO_enrich_cutoff),col='blue')
    abline(v = 0,col='black')
  }
  
 
  

  #### LISTS ####
  

  
  output$term_list = renderUI({
    print('term_list : ')
    if(input$fix_term == F){
      selectInput('term','Select GO Term',enrichment_table()$Term,selected = NA)
    }else{
      selectInput('term','Select GO Term',input$term)
    }
  })
  
  #full_gene_list = readRDS('./shiny_data/gene_list.rds')
  full_gene_list = reactive(mapped_data()$mapped_data$id)

  
  gene_list_df = reactive({
    print('gene_list_df')
    in_file_name = input$gs_list_filename
    #print(in_file_name$datapath)
    if(is.null(in_file_name)){
      return(NULL)
    }else{
      df = read.table(in_file_name$datapath, stringsAsFactors = F,sep = '\t')
    }
    #print(in_file_name$datapath)
    #print(df)
    print('   gene_list_df : done')
    
    df$V1
  })
  
  gene_list_df_2 = reactive({
    print('gene_list_df_2')
    in_file_name = input$gs_list_filename_2
    #print(in_file_name$datapath)
    if(is.null(in_file_name)){
      return(NULL)
    }else{
      df = read.table(in_file_name$datapath, stringsAsFactors = F,sep = '\t')
    }
    #print(in_file_name$datapath)
    #print(df)
    print('   gene_list_df_2 : done')
    
    df$V1
  })
  
  output$gene_list_df_table = renderDataTable(gene_list_df())
  
  output$data_info_print = renderText({
    selection = input$data_type_radio
    input_path = wd_path_select()
    
    print(input_path)
    print(selection)
    print(head(gene_list()))
    
    print(paste(input_path,selection,paste(head(gene_list()),collapse = ', ')))
  })
  ### _gene_list ####
  gene_list = reactive(gene_list_select_list()$gene_list)
  
  STRING_id_list = reactive({
    print('STRING_id_list')
    mapped_data = mapped_data()$mapped_data
    df = mapped_data$STRING_id[mapped_data$id %in% gene_list()]
    print('   STRING_id_list : done')
    df
    
  })
  

  
  
  output$df_gene_list = renderDataTable(data.frame(gene_list = gene_list()))
  output$df_STRING_id_list = renderDataTable(data.frame(gene_list = STRING_id_list()))
  
  gene_list_select_list = reactive({
    print('gene_list_select_list')
    single_mapped_st = mapped_st_single_list()
    #path_list = c(data_name_collapse(),input$data_type_radio)
    gene_list = c('SOX2','GFAP','NES','LIN28A','LIN28B','LIN28')
      #gene_list = readRDS('common_mapped.rds')
      if(input$data_type_radio == 'list'){
        path_list = c(data_name_collapse(),input$data_type_radio)
        gene_list = input$select_gene_list
        #file_name = basename(gene_list_df())
        path_list = c(path_list,paste(gene_list,collapse='_'))
      }
      if(input$data_type_radio == 'uniprot'){
        path_list = c(data_name_collapse(),input$data_type_radio)
        gene_list = unlist(gene_list_df())
        gene_list
        #file_name = basename(gene_list_df())
        file_name = 'file_name'
        path_list = c(path_list,file_name)
      }
      if(input$data_type_radio == 'enrichment'){
        path_list = c(data_name_collapse(),input$data_type_radio)
        id_list = mapped_data()$id_list
        
        
        if(input$enrichment == 'AnimalTFDB'){
          path_list = c(path_list,input$enrichment)
          
          #id_list = mapped_data()$id_list
          
          tf_list = enrichment_table()$STRING_id
          #print(tf_list)
          gene_list = unlist(id_list[tf_list])
          #print(gene_list)
        }
        if(input$enrichment == 'STRINGdb'){
          path_list = c(path_list,input$stringdb_enrichment,input$term)
          #id_list = mapped_data()$id_list
          gene_list = unlist(id_list[string_hits_list()])
          
        }
        if(input$enrichment == 'topGO'){
          path_list = c(path_list,input$stringdb_enrichment,input$term)
          gene_list = unlist(id_list[string_hits_list()])
          
        }
        
          #gene_list = enrichment_table()$id
        #}else{
        #  id_list = mapped_data()$id_list
        #  print(id_list)
        #  gene_list = unlist(id_list[string_hits_list()])
        #}
      }
      if(input$data_type_radio == 'venn'){
        
        if(input$venn_data_select_button == 'gene_list'){
          path_list = c(venn_values$path_list,'venn')
        }else{
          venn_list = input$venn_int
          venn_list = venn_list[order(venn_list)]
          venn_line = paste(venn_list,collapse= '___')
          path_list = c(data_name_collapse(),input$data_type_radio)
          ## find a rule for applying this line
          #path_list = c(data_name_collapse(),input$data_type_radio)
          
          
        }
        gene_list = venn_gene_list()
        #gene_list = c('SOX2')
      }
    #print(path_list)
    gene_list = gene_list[order(gene_list)]
    print('   gene_list_select_list : done')
    
    list(gene_list = gene_list, path_list = path_list)

  }) # needs work currently only using GE list
  
  path_line = reactive({
    print('path_line')
    path_list = gene_list_select_list()$path_list
    path_list
    #path_list = c(shiny_image_path,path_list)
    path_entry = c()
    #path_entry = thesis_path_sub
    path_root = paste(thesis_path_sub,shiny_image_path,sep = '/')
    path_root = shiny_image_path
    path_root
    if(input$save_plot == T){
      for(entry in path_list){
        print(entry)
        if(nchar(entry) > 30){
          full_length = nchar(entry)
          full_length  
          #print(nchar(entry))
          entry = paste(strtrim(entry,30),full_length,sep='_')
          entry
        }
        path_entry = c(path_entry,entry)
        path_entry
        path_entry_line = latex_filename_function(paste(path_entry,collapse = '/'))
        #print(path_entry_line)
        create_dir_function(paste0(path_root,path_entry_line))
      }
    }
    path_line = paste0(path_root,path_entry_line,'/')
    path_line
    values$plot_path = path_line
    #print(path_line)
    print('   path_line : done')
    
    path_line
  })
  
  
  
  venn_values = reactiveValues(gene_list = c())
  
  
  observeEvent(input$store_button,{
    #print('store_button')
    #print(venn_values$gene_list)
    #print(gene_list())
    venn_values$gene_list = paste(gene_list())
    path_list = gene_list_select_list()$path_list
    
    venn_values$path_list = c(path_list)
    #venn_values$path_list = gene_list_select_list()$path_list
    #print('store venn')
    #print(venn_values$gene_list)
  })
  
  observeEvent(input$store_button_3,{
    print('#### store_button_3 ####')
    #gene_list = c('SOX2','GFAP','NES')
    mapped_data = mapped_st()
    annot = annot()
    enrichment_table = sn_en()
    #GO_term = input$enrich_select_term
    term_list = input$enrich_select_term_combined
    term_list
    STRING_hits_list = c()
    for(GO_term in term_list){

      #GO_term = input$enrich_select_term_combined
      #print(GO_term)
      string_hits = string_hits_function(enrichment_table,GO_term,mapped_data,annot())
      #print(string_hits)
      STRING_hits_list = c(STRING_hits_list,string_hits)
    }
    STRING_hits_list = unique(STRING_hits_list)
    STRING_hits_list = STRING_hits_list[!is.na(STRING_hits_list)]
    STRING_hits_list
    id_list = mapped_data()$id_list
    #print(id_list)
    gene_list = paste(id_list[STRING_hits_list])
    #print(gene_list)
    #gene_list = gene_list[gene_list %in% gene_list()]
    #print(length(gene_list))
    venn_values$gene_list = gene_list
    
    path_list = gene_list_select_list()$path_list
    path_list = c(path_list,input$venn_int,paste(term_list,collapse = '_'))
    #print(path_list)
    venn_values$path_list = c(path_list)
  })
  
  observeEvent(input$store_button_neighbour,{
    #print('store button neighbour')
    int = neighbour_data()$int
    id_list = mapped_data()$id_list
    gene_list = unlist(id_list[tf_list])
    #print(venn_values$gene_list)
    #print(gene_list())
    venn_values$gene_list = paste(gene_list)
    venn_values$path_list = c(gene_list_select_list()$path_list,'neighbour')
    #print('store venn')
    #print(venn_values$gene_list)
  })
  
  venn_gene_list_select = reactive({
    print('venn_gene_list_select')
    # if(!input$data_type_radio == 'venn'){
    #   venn_list = venn_store()
    # }
    venn_column = input$venn_id_select
    venn_column = 'id'
    if(input$venn_data_select_button == 'all' ){
      venn_list = mapped_data()$mapped_ud[,venn_column]
    }else{
      venn_list = venn_values$gene_list
    }
    #print('final venn')
    #print(venn_list)
    venn_list = venn_list[!is.na(venn_list)]
    #print('store button neighbour')
    print('   venn_gene_list_select : done')
    
    venn_list
    
    
  })
  
  

  
  
  output$gene_list_print = renderText({
    print(paste(gene_list()))
  })
  
  output$gene_list_print_test = renderPrint({
    path_list = gene_list_select_list()$path_list
    #print(gene_list_file_path())
    print(paste(gene_list()))
  })
  

  
  gene_list_file_path = reactive({
    print('gene_list_file_path')
    path_list = gene_list_select_list()$path_list
    path_entry_list = c()
    base_data_list = c('data','txt')

    data_list = c(base_data_list,path_list)
    if(input$data_type_radio == 'venn'){
      data_list = c(data_list,input$venn_int)
    }
    
    list_line = create_dir_list_function(data_list)

    sample_data_path_list = c(base_data_list,data_name_collapse())
    sample_line = create_dir_list_function(sample_data_path_list)
    print('   gene_list_file_path : done')
    
    list(sample_line = sample_line, list_line = list_line)
  })
  observeEvent(input$write_gene_list,{
    list_file_name = paste0(gene_list_file_path()$list_line,'/gene_list.txt')
    lapply(gene_list(),write,list_file_name,append=T,ncolumns = 1)
    sample_file_name = paste0(gene_list_file_path()$sample_line,'/gene_list.txt')
    lapply(full_gene_list(),write,sample_file_name,append=T,ncolumns = 1)
    sample_file_name_st = paste0(gene_list_file_path()$sample_line,'/STRING_id_list.txt')
    lapply(mapped_data()$mapped_data$STRING_id,write,sample_file_name_st,append=T,ncolumns = 1)
    
    #sample_file_name = paste0()
    
    
  })
  
  
  

  output$select_gene_file_prefix_ui = renderUI({
    print('select_gene_file_prefix_ui')
    if(input$list_type == 'Prefix'){
      textInput('prefix_input',"Prefix Input (sep = ', ')",value = '')
    }

  })
  
  output$select_gene_file_ui_2 = renderUI({
    print('select_gene_file_ui_2')
    if(input$list_type == 'File'){
      fileInput('gs_list_filename_2', 'Select List', multiple = FALSE, accept = NULL, width = NULL,
                buttonLabel = "Browse...", placeholder = "No file selected")
    }
  })
  
  output$select_gene_file_prefix_ui_run = renderUI({
    print('select_gene_file_prefix_ui_run')
    if(input$list_type == 'Prefix'){
      radioButtons('prefix_run','Run Prefix',choices = c(F,T),inline = T)
    }
  })
  
  
  output$select_gene_list_ui = renderUI({
    print('select_gene_list_ui')
    #print('Start : select_gene_list_ui')
    start_time = Sys.time()
    selected_gene_list = 'SOX2'
    hit = 0
    if(input$list_type == 'Prefix'){
       if(input$prefix_input != '' & input$prefix_run == T){
          prefix_list = unlist(strsplit(input$prefix_input,', '))
          all_gene_list = mapped_data()$all_mapped_data$id
          selected_gene_list = c()
          for(prefix in prefix_list){
            selected_gene_list = c(selected_gene_list,grep(paste0('^',prefix),all_gene_list,value = T))
          }
          hit = 1
       }
    }
    if(input$list_type == 'File'){
        hit = 1
        selected_gene_list = unlist(gene_list_df_2())
    }
    if(input$list_type == 'Saved'){
      hit = 1
      selected_gene_list = readRDS(paste0(shiny_data_path,'saved_gene_list.rds'))
    }
    if(input$list_type == 'None'){
     hit = 1
     selected_gene_list = c()
    }
    selected_gene_list
    end_time = Sys.time()
    print(end_time - start_time)
    print('End : select_gene_list_ui')
    if(hit == 1){
      selectInput('select_gene_list','select genes',choices = mapped_data()$all_mapped_data$id, selected_gene_list, multiple = T)
    }  
  })
  
  output$gene_list_save = renderText({
    print('Start : gene_list_save')
    start_time = Sys.time()
    selected_gene_list = c('SOX2','NES','GFAP','LIN28A','ANXA6')
    selected_gene_list = input$select_gene_list
    if(input$list_type != 'None'){
      saveRDS(selected_gene_list,paste0(shiny_data_path,'saved_gene_list.rds'))
    }
    end_time = Sys.time()
    print(end_time - start_time)
    print('   gene_list_save : done')

    selected_gene_list
  })
  
  output$select_gene_list_remove_ui = renderUI({
    print('select_gene_list_remove_ui')
    removed_file_list = paste0(shiny_data_path,'removed_list_',data_name_collapse(),'.rds')
    gene_file_list = paste0(shiny_data_path,'gene_list_',data_name_collapse(),'.rds')
    
    removed_list = readRDS(removed_file_list)
    gene_list = readRDS(gene_file_list)
    selectInput('removed_list',"Gene's to remove",choices = gene_list, removed_list, multiple = T)
  })
  output$removed_genes_text = renderText(paste(mapped_data()$removed_list))
  
  output$select_gene_file_ui = renderUI({
    print('select_gene_file_ui')
    fileInput('gs_list_filename', 'Select List', multiple = FALSE, accept = NULL, width = NULL,
              buttonLabel = "Browse...", placeholder = "No file selected")
  })
  
  
  # output$select_gene_file_ui = renderUI({
  #   fileInput('gs_list_filename', 'Select List', multiple = FALSE, accept = NULL, width = NULL,
  #             buttonLabel = "Browse...", placeholder = "No file selected")
  # })

  
  output$gene_list_3 = renderUI({
    print('gene_list_3')
    #gene_list = readRDS('common_mapped.rds')
    selectInput('genes_3','select genes',full_gene_list)
  }) 
  
  
  

  
  
  
  
  output$file_list = renderText(go_files)
  
  output$m_name = renderText(paste0('m.enrich.',input$stat,'.',input$ontology,'.',input$data,'.rds'))
  
  m.enrich = reactive({
    print('m.enrich')
    m.enrich = readRDS(paste0('m.enrich.',input$stat,'.',input$ontology,'.',input$single_data,'.rds'))
    print('   m.enrich : done')
    m.enrich
    
    
    })
  output$enrich_slider = renderUI({
    print('enrich_slider :')
    sliderInput(inputId = 'm_range','enrichment range',min(0),dim(m.enrich())[1],value = c(0,dim(m.enrich())[1]),dragRange=T)
  })
  
  output$enrich_barplot = renderPlot({
    print('enrich_barplot :')
    m.enrich = m.enrich()
    par(xpd=FALSE,mar = c(4, 25, 4, 1) + 0.1,cex.axis=1,cex.main = 0.5,mfrow = c(1,1)) #c(bottom, left, top, right)
    # try(barplot(rev(m.enrich[,1]),beside=TRUE,names.arg = rev(rownames(m.enrich)),las=1,col=c('red'),xlim=c(min(m.enrich,na.rm=TRUE),max(m.enrich,na.rm=TRUE)),main = paste(enrichment,'topGO',stat_select),horiz = TRUE))
    # try(barplot(rev(m.enrich[,2]),beside=TRUE,col=c('green'),add=TRUE,horiz=TRUE,yaxt='n'))
    # try(abline(v = -log10(topGO_enrich_cutoff),col='blue'))
    # try(abline(v = log10(topGO_enrich_cutoff),col='blue'))
    # try(abline(v = 0,col='black'))
    
    range = c(input$m_range[1]:input$m_range[2])
    
    #par(xpd=FALSE,mar = c(4, 18, 4, 2) + 0.1,cex.axis=0.6,mfrow = c(1,1)) #c(bottom, left, top, right)
    barplot(rev(m.enrich[range,1]),beside=TRUE,names.arg = rev(rownames(m.enrich)[range]),las=1,col=c('red'),xlim=c(min(m.enrich,na.rm=TRUE),max(m.enrich,na.rm=TRUE)),horiz=TRUE,cex.main=0.5)
    barplot(rev(m.enrich[range,2]),beside=TRUE,yaxt='n',xaxt='n',col=c('green'),add=TRUE,horiz=TRUE,xlab= 'test')
    #barplot(t(m.enrich[c(1:40),]),beside=TRUE,names.arg = rownames(m.enrich)[c(1:40)],las=3,col=c('red','green'))
    abline(v = -log10(topGO_enrich_cutoff),col='blue')
    abline(v = log10(topGO_enrich_cutoff),col='blue')
    abline(v = 0,col='black')
    #m.enrich
  })
  
  output$single_enrich = renderPlot({
    print('single_enrich :')
    m.enrich = m.enrich()
    par(xpd=FALSE,mar = c(1, 20, 1, 1) + 0.1,cex.axis=1,cex.main = 0.5,mfrow = c(1,1)) #c(bottom, left, top, right)
    # try(barplot(rev(m.enrich[,1]),beside=TRUE,names.arg = rev(rownames(m.enrich)),las=1,col=c('red'),xlim=c(min(m.enrich,na.rm=TRUE),max(m.enrich,na.rm=TRUE)),main = paste(enrichment,'topGO',stat_select),horiz = TRUE))
    # try(barplot(rev(m.enrich[,2]),beside=TRUE,col=c('green'),add=TRUE,horiz=TRUE,yaxt='n'))
    # try(abline(v = -log10(topGO_enrich_cutoff),col='blue'))
    # try(abline(v = log10(topGO_enrich_cutoff),col='blue'))
    # try(abline(v = 0,col='black'))
    
    range = grep(input$term,rownames(m.enrich))
    
    #par(xpd=FALSE,mar = c(4, 18, 4, 2) + 0.1,cex.axis=0.6,mfrow = c(1,1)) #c(bottom, left, top, right)
    barplot(rev(m.enrich[range,1]),beside=TRUE,names.arg = rev(rownames(m.enrich)[range]),las=1,col=c('red'),xlim=c(min(m.enrich,na.rm=TRUE),max(m.enrich,na.rm=TRUE)),horiz=TRUE,cex.main=0.5)
    barplot(rev(m.enrich[range,2]),beside=TRUE,yaxt='n',xaxt='n',col=c('green'),add=TRUE,horiz=TRUE,xlab= 'test')
    #barplot(t(m.enrich[c(1:40),]),beside=TRUE,names.arg = rownames(m.enrich)[c(1:40)],las=3,col=c('red','green'))
    abline(v = -log10(topGO_enrich_cutoff),col='blue')
    abline(v = log10(topGO_enrich_cutoff),col='blue')
    abline(v = 0,col='black')
    #m.enrich
  })
  
  
  ### STRINGdb ####
  
  annot = reactive({
    print('annot')
    start = Sys.time()
    
    annot_file_path = paste0(shiny_data_path,input$taxonomy,'_annot.rds')
    annot_list_all_file_path = paste(shiny_data_path,input$taxonomy,'_annot_list_all.rds')
    annot_list_non_IEA_file_path = paste(shiny_data_path,input$taxonomy,'_annot_list_no_EIA.rds')
    #print(annot_file_path)
    if(file.exists(annot_file_path)){

      rds_start = Sys.time()
      annot = readRDS(annot_file_path)
      print(Sys.time() - rds_start)
      print('readRDS')
      
      #annot_list = readRDS(annot_list_file_path)
    }else{
      print('generate annot')
      base_string_db = base_string_db()
      annot = base_string_db$get_annotations()
      saveRDS(annot,annot_file_path)
      print('generating annot list')
      ens_list = unique(annot$STRING_id)
      length(ens_list)
      system.time({
      annot_list_all = lapply(ens_list, function(ens) annot$term_id[annot$STRING_id == ens])
      names(annot_list_all) = ens_list
      })
      saveRDS(annot_list_all,annot_list_all_file_path)
      system.time({
        annot_list_non_IEA = lapply(ens_list, function(ens) annot$term_id[annot$STRING_id == ens & annot$type != 'IEA'])
        names(annot_list_non_IEA) = ens_list
      }) 
      saveRDS(annot_list_non_IEA,annot_list_non_IEA_file_path)
      
    }
    print(Sys.time() - start)
    
    print('   annot : done')
    annot
  })
  
  base_string_db = reactive({
    print('base_string_db')
    #print('STRINGdb$new')
    file_path = paste0(shiny_data_path,input$taxonomy,'base_string_db.rds')
    taxonomy_number = input$taxonomy
    if(file.exists(file_path) & input$background_re_run == F){
      base_string_db = readRDS(file_path)
    }else{
      base_string_db = STRINGdb$new(version="10", species=as.numeric(taxonomy_number), score_threshold=400, input_directory=table_path())
      saveRDS(base_string_db,file_path)
    }
    print('   base_string_db : done')
    
    base_string_db
    })
  
  backgroundV = reactive({
    print('backgroundV')
    data_name_list = input$data
    data_name_list
    id_list = c()
    string_id_list = c()
    if(input$background == 'all_mapped'){
      for(i in c(1:length(data_name_list))){
        #print(i)
        data_name = data_name_list[i]
        id_list = c(id_list,data_df_list()[[data_name]]$id)
        string_id_list = c(string_id_list,data_df_list()[[data_name]]$STRING_id)
      }
    }
    if(input$background == 'sig_mapped'){
      for(i in c(1:length(data_name_list))){
        #print(i)
        data_name = data_name_list[i]
        id_list = c(id_list,sig_data_list()[[data_name]]$id)
        string_id_list = c(string_id_list,sig_data_list()[[data_name]]$STRING_id)
      }
    }
    id_list = unique(id_list[!is.na(id_list)])
    string_id_list = unique(string_id_list[!is.na(string_id_list)])
    
    #capture.output(id_list, file = paste(sample_path_line(),'id_list.txt'))
    #capture.output(string_id_list, file = paste(sample_path_line(),'string_id_list.txt'))
    
    write.table(id_list,file = paste(sample_path_line(),'id_list.txt',sep='/'),row.names = F,col.names = F,quote = F)
    write.table(string_id_list,file = paste(sample_path_line(),'string_id_list.txt',sep='/'),row.names = F, col.names = F, quote = F)
    
    
    backgroundV = string_id_list
    print('   backgroundV : done')
    
    backgroundV
  })
  
  string_db = reactive({
    print('string_db')
    start = Sys.time()
    
    taxonomy_number = input$taxonomy
    file_path = paste0(shiny_data_path,taxonomy_number,'_',data_name_collapse(),'_string_db_all_mapped.rds')
    #print(file_path)
    if(file.exists(file_path) & input$re_run == F & input$background == 'all_mapped'){
      rds_start = Sys.time()
      result = readRDS(file_path)
      print(Sys.time() - rds_start)
      print(paste('readRDS',file_path))
    }else{
      print('generate string_db')
      if(input$background != 'NULL'){
        string_db = base_string_db()
        backgroundV = backgroundV()
        backgroundV = backgroundV[!is.na(backgroundV)]
        string_db$set_background(backgroundV)
      }else{
        string_db = base_string_db()
      }

      result = string_db
      if(input$background == 'all_mapped'){
        rds_start = Sys.time()
        saveRDS(result,file_path)
        print(Sys.time() - rds_start)
        print(paste('saveRDS',file_path))
      }
    }
    print(Sys.time() - start)
    print('   string_db : done')
    
    result
  })
  

  
  #output$string_pic = renderImage(list(src='www/vacuolar_part_STRING_UP.pdf'))
  
  

  
  output$term_num = renderText(paste(dim(enrichment_table())[1],'terms'))
  
  output$sn_list = renderText(paste(length(string_hits_list()),'nodes'))
  
  #values = reactiveValues()
  #values$sn = 0
  
  #observeEvent(input$run_sn, {
  #  values$sn = 1
  #})
  
  values <- reactiveValues(shouldShow_sn_select = FALSE, 
                           shouldShow_sn_select_link = FALSE, 
                           plot_name = '', 
                           enrich_gene_list = FALSE, 
                           plot_path = '',
                           upload_datasets = uploaded_datasets,
                           upload_save = F)
  
  ##### STRING plots ####
  
      #### ___ string_hits_list ####
      string_hits_list = reactive({
        print('string_hits_list')
        #if(values$sn == 1){
        #print('string hits')
        mapped_data = mapped_st()
        GO_term = input$term
        GO_id = enrichment_table()$GO.ID[enrichment_table()$Term == GO_term]
        
        string_GO_members = annot()$STRING_id[annot()$term_id == GO_id]
        string_GO_members
        GO_members = string_GO_members
        STRING_hits = GO_members[GO_members %in% mapped_data$STRING_id]
        #print('string hits done')
        #print(STRING_hits)
        print('   string_hits_list : done')
        
        STRING_hits
        #}else{
        #  string_hits_list()
        #}
        
      })
  

  
  
  
    #   ### __buttons ####
    #     ### ___generate plot ####
    #   observe({
    #     if (input$sn_button == 0) return()
    #     values$shouldShow = TRUE
    #   })
    #   observe({
    #     if (is.null(input$reset_sn_button) || input$reset_sn_button == 0)
    #       return()
    #     values$shouldShow = FALSE
    #   })
    # 
    #   output$ui_reset_sn <- renderUI({
    #     if(values$shouldShow){
    #       actionButton("reset_sn_button","Reset Plot")
    #     }
    #   })
    #   
    #   ##### ######
    #   # output$sn = renderPlot({
    #   #   if(values$shouldShow){
    #   #     print('plot network')
    #   #     STRING_hits = string_hits_list()
    #   #     string_db$plot_network(STRING_hits,payload_id=payload_id(),add_link = FALSE)
    #   #   }
    #   # })
    #   # 
    #   # string_GO_table = reactive({
    #   #   int = string_hits_list()
    #   #   mapped_data = mapped_st()
    #   #   View(mapped_data)
    #   #   plot_data = mapped_data[mapped_data$STRING_id %in% int,]
    #   #   plot_data
    #   #   View(plot_data)
    #   #   rownames(plot_data) = plot_data$id
    #   #   plot_data = plot_data[,c((grep('STRING_id',colnames(plot_data))+1):(grep('col',colnames(plot_data))-1))]
    #   #   #View(plot_data)
    #   #   plot_data
    #   # })
    #   # 
    #   # output$string_GO_heatmap = renderPlot({
    #   #   if(values$shouldShow){
    #   #     if(input$mapped == 'common_mapped'){
    #   #       plot_data = string_GO_table()
    #   #       print(plot_data)
    #   #       plot_data[plot_data == 0] = NA
    #   #       print(plot_data)
    #   #       plot_data = plot_data[rowSums(plot_data,na.rm =T) != 0,]
    #   #       print(plot_data)
    #   #       #col_pallete = colorRampPalette(c("green", "red"), space="rgb")(64)
    #   #       heatmap.2(as.matrix(plot_data),Rowv = F,Colv = F,dendrogram = c("none"),col=redgreen(75),trace = 'none',cexRow = 1,cexCol = 1)
    #   #     }else{
    #   #       paste(' ')
    #   #     }
    #   #   }else{
    #   #     paste(' ')
    #   #   }
    #   # })
    # 
    #     #### ___ link ####
    #       observe({
    #         if (input$sn_link_button == 0) return()
    #         values$shouldShow_link = TRUE
    #       })
    #       observe({
    #         if (is.null(input$reset_sn_link_button) || input$reset_sn_link_button == 0)
    #           return()
    #         values$shouldShow_link = FALSE
    #       })
    #       output$ui_reset_sn_link <- renderUI({
    #         if(values$shouldShow_link){
    #           actionButton("reset_sn_link_button","Reset link")
    #         }
    #       })
    #       output$sn_url = renderText({
    #         if(values$shouldShow_link){
    #           STRING_hits = string_hits_list()
    #           get_link = string_db$get_link(STRING_hits,payload_id = payload_id())
    #           paste('<a href=',get_link,' target="_blank" class="btn btn-default">Go to stringdb.org</a>')
    #         }else{
    #           return()
    #         }
    #       })
    # 
    #       observeEvent(input$run_sn_link,{
    #   output$sn_url = renderText({
    #     STRING_hits = string_hits_list()
    #     get_link = string_db$get_link(STRING_hits,payload_id = payload_id())
    #     paste('<a href=',get_link,' target="_blank" class="btn btn-default">Go to stringdb.org</a>')
    #   })
    # })
      
      
    #### __observe buttons ######
      #### ___ Generate Plot #####
      
      observe({
        if (input$sn_select_button == 0) return()
        values$shouldShow_sn_select = TRUE
      })
      observe({
        if (is.null(input$reset_sn_select_button) || input$reset_sn_select_button == 0)
          return()
        values$shouldShow_sn_select = FALSE
      })
      
      #### ___link #####
      observe({
        if (input$sn_select_button_link == 0) return()
        values$shouldShow_sn_select_link = TRUE
      })
      observe({
        if (is.null(input$reset_sn_select_button) || input$reset_sn_select_button == 0)
          return()
        values$shouldShow_sn_select_link = FALSE
      })
      
      #### ___ Reset ####
      output$ui_reset_sn_select <- renderUI({
        if(values$shouldShow_sn_select){
          actionButton("reset_sn_select_button","Reset")
        }
      })
      
      #### __sn_data ####
          neighbour_data = reactive({
            print('neighbour_data')
            test = F
            if(test == T){
              mapped_data = results_list$mapped_data
              payload_id = results_list$payload_id
              entry_list = results_list$entry_list
              gene_list = c('SOX2','GFAP')
            }
            mapped_data = mapped_data()$mapped_data
            #print(colnames(mapped_data))
            payload_id = mapped_data()$payload_id
            entry_list = mapped_data()$entry_list
            #print(head(mapped_data))
            
            custom_list = mapped_data$STRING_id[mapped_data$id %in% gene_list()]
            #gene_list = gene_list()
            #print(gene_list)

            
            
            
            #custom_list = string_db()$mp(gene_list)
            #print(custom_list)
            int = custom_list
            if(input$sn_select == 'neighbour'){
              print('get_neigbours')
              neighbors = string_db()$get_neighbors(custom_list)
              #print(neighbours)
              int = c(intersect(mapped_data$STRING_id,neighbors),custom_list)
            }
            if(input$sn_select == 'interaction'){
              #print('get_neigbours')
              neighbors = string_db()$get_interactions(custom_list)
              #print(neighbours)
              
              int = c(intersect(mapped_data$STRING_id,neighbors),custom_list)
            }
            if(input$sn_select == 'sub'){
              #print('get_neigbours')
              neighbors = string_db()$get_subnetwork(custom_list)
              int = c(intersect(mapped_data$STRING_id,neighbors),custom_list)
            }
            
            if(input$sn_select == 'cluster'){
              #print('get_neigbours')
              neighbors = string_db()$get_clusters(custom_list)
              #print(neighbours)
              
              int = c(intersect(mapped_data$STRING_id,neighbors),custom_list)
            }
            
            #if(input$sn_select == 'interaction'){
            #  int = custom_list
            #}
            
            #print('plot_network')
            #p = string_db$plot_network(int,payload_id=payload_id,add_link = FALSE)
            #print(p)
            print('   neighbour_data : data')
            
            list(int = int)
            #legend(1,30,legend = names(entry_list),fill = paste(entry_list),cex = 0.5)
            
          })
      ##### __sn_PLOT #####  
          #sn_plot = reactive({
      
          output$sn_image_ui = renderUI({
            print('sn_image_ui : ')
            #image_path = paste0(path_line(),'/string.png')
            #print(image_path)
            #print(file.exists(image_path))
            #run_hit = 0
            if(values$shouldShow_sn_select){
              plotOutput('neighbour_plot')
            }else{
              imageOutput('sn_image')
            }
          })
      
          # output$sn_thesis_path = renderText({
          #   print('sn_thesis_path')
          #   #image_path = paste0(path_line(),'/string.pdf')
          #   plot_name = 'string'
          #   if(input$sn_select == 'neighbour'){
          #    #image_path = paste0(path_line(),'/string_neighbour.pdf')
          #     plot_name = 'string'
          #   }
          #   values$plot_name = plot_name
          #   
          #   if(file.exists(pdf_plot_path())){
          #     pdf_plot_path
          #     #sub('./',thesis_path_sub,image_path)
          #   }else{
          #     print('')
          #   }
          #  
         # })
      
          output$sn_image = renderImage({
            print('sn_image : ')
            #image_path = paste0(path_line(),'/string.png')
            plot_name = 'string'
            
            if(input$sn_select == 'neighbour'){
              #image_path = paste0(path_line(),'/string_neighbour.pdf')
              plot_name = 'string_neighbour'
              
            }    
            
            if(input$data_type_radio == 'venn'){
              plot_name = latex_filename_function(paste(plot_name,paste(input$venn_int,collapse = '_')))
            }
            #values$plot_name = paste0(plot_name)
            values$plot_name = plot_name
            #if(file.exists(image_path)){
            print(getwd())
            print(pdf_plot_path())
            if(file.exists(pdf_plot_path())){
                
              #values$plot_name = 'string.pdf'
              #if(input$sn_select == 'neighbour'){
              #  values$plot_name = 'string_neighbour.pdf'
              #}
              list(src = png_plot_path())
            }else{
              
              list(src = pdf_plot_path(), alt = "No image available, Generate Plot")
            }
          })
      
          output$neighbour_plot = renderPlot({
            print('neighbour_data : ')
            if(values$shouldShow_sn_select){
              int = neighbour_data()$int
              payload_id = mapped_data()$payload_id
              #print('plot_network')
              p = string_db()$plot_network(int,payload_id=payload_id,add_link = FALSE)
              print(p)
              if(input$save_plot == T){
                  #plot_name = 'string'
                  #if(input$sn_select == 'neighbour'){
                  #  plot_name = 'string_neighbour'
                  #}
                  #if(input$data_type_radio == 'venn'){
                  #  plot_name = latex_filename_function(paste(plot_name,paste(input$venn_int,collapse = '_')))
                  #}
                  #values$plot_name = paste0(plot_name)
                  print(path_line())
                  print(plot_path())
                  print(pdf_plot_path())
                  print(thesis_pdf_plot_path())
                  save_plot_function_2(values$plot_path,values$plot_name,c('png','pdf'))
                }
              print(p)

            }else{
              c("")
            }
            
          })
          
      
      # output$neighbour_plot = renderPlot({
      #   p = sn_plot()
      #   #print(p)
      # 
      #   print(p)
      #   })
          
          

          
          neighbour_table = reactive({
            print('neighbour_table')
            int = neighbour_data()$int
            mapped_data = mapped_data()$mapped_data
            plot_data = mapped_data[mapped_data$STRING_id %in% int,]
            plot_data
            rownames(plot_data) = plot_data$id
            plot_data = plot_data[,c((grep('STRING_id',colnames(plot_data))+1):(grep('col',colnames(plot_data))-1))]
            print('   neighbour_table : data')
            
            plot_data
          })
          
          output$neighbour_heatmap = renderPlot({
            print('neighbour_heatmap : ')
            if(values$shouldShow_sn_select){
              plot_data = neighbour_table()
              #print(plot_data)
              plot_data[plot_data == 0] = NA
              #print(plot_data)
              plot_data = plot_data[rowSums(plot_data,na.rm =T) != 0,]
              #print(plot_data)
              #col_pallete = colorRampPalette(c("green", "red"), space="rgb")(64)
              heatmap.2(as.matrix(plot_data),Rowv = F,Colv = F,dendrogram = c("none"),col=redgreen(75),trace = 'none',cexRow = 1,cexCol = 1)
              }else{
              paste(' ')
            }
          })
         ### ____legend ####
          output$legend = renderPlot({
            print('legend')
            #if(values$shouldShow_sn_select){
              entry_list = mapped_data()$entry_list
              plot(c(1,120),c(120,1),axes = F, frame.plot=F, ann= F)
              legend(1,120,legend = names(entry_list),fill = paste(entry_list),cex = 0.7,bty = 'n')
              save_plot_function_2(sample_path_line(),'legend',c('png', 'pdf'))
              #}else{
            #  paste(' ')
            #}
          })
          

          output$ui_reset_sn_select_link <- renderUI({
            if(values$shouldShow_sn_select_link){
              actionButton("reset_sn_select_button","Reset link")
            }
          })
          
          output$ui_generate_link_select = renderUI({
            if(values$shouldShow_sn_select_link){
              actionButton("sn_select_button_link", "Generate link")
            }else{
              actionButton("sn_select_button_link", "")
              
            }
          })
          
          output$sn_url_select = renderText({
            
            if(values$shouldShow_sn_select_link){
              STRING_hits = neighbour_data()$int
              get_link = string_db()$get_link(STRING_hits,payload_id = payload_id())
              paste('<a href=',get_link,' target="_blank" class="btn btn-default">stringdb.org</a>')
            }else{
              paste(' ')
            }
          })
       # }
        
      #})
      
  
  pdf_file_name = reactive({
    print('pdf_file_name')
    #metric = 'mean'
    if(input$single_data %in% timecourse_data){
      metric = 'slope'
    }
    file_name = paste0('images/STRINGdb/',input$single_data,'/',metric,'/',ontology_path_name[input$ontology],'/STRING/',latex_filename_function(input$term),'/',latex_filename_function(input$term),'_STRING.pdf')
    print('   pdf_file_name : done')
    file_name
    })
  
  output$pdf_file_name_print = renderText(pdf_file_name())
    
  output$string_pdf = renderText({
    
        file_name = pdf_file_name()
        paste0('<object data="',file_name,'" type="application/pdf" width="700px" height="700px">',
                                  '<embed src="',file_name,'" type="application/pdf">',
            '<p>This browser does not support PDFs. Please download the PDF to view it: <a href="',file_name,'">Download PDF</a>.</p>',
            '</embed>',
            '</object>')
   })
  
  output$pdfview_string <- renderUI({
    #pdf("www/myreport.pdf")
    #hist(rnorm(100))
    #dev.off()
    print(GO_composite_file())
    file_name = pdf_file_name()
    #file_name = 'vacuolar_part_STRING_UP.pdf'
    tags$iframe(style="height:600px; width:100%", src=file_name)
  })
  
  gene_file_name = reactive({
    file_name = paste0('..///boxplot/ESC_GE_SILAC_Diff/all/',input$genes_3,'.pdf')
    file_name
  })
  
  output$gene_path = renderText(gene_file_name())
  
  output$gene_pdf = renderText({
    
    file_name = gene_file_name()
    file_name
    paste0('<object data="',file_name,'" type="application/pdf" width="700px" height="700px">',
           '<embed src="',file_name,'" type="application/pdf">',
           '<p>This browser does not support PDFs. Please download the PDF to view it: <a href="',file_name,'">Download PDF</a>.</p>',
           '</embed>',
           '</object>')
  })
  
  GO_composite_file = reactive({
    print('GO_composite_file')
    file_name = paste0('images/STRINGdb/GO_full_comparison/0.01/topGO/fisher_weight01/',ontology_path_name[input$ontology],'/single/crop/',latex_filename_function(input$term),'.pdf')
    
    print('   GO_composite_file : done')
    
    file_name
  })
  
  output$pdfview_GO <- renderUI({
    #pdf("www/myreport.pdf")
    #hist(rnorm(100))
    #dev.off()
    #print(GO_composite_file())
    file_name = GO_composite_file()
    #file_name = 'vacuolar_part_STRING_UP.pdf'
    tags$iframe(style="height:600px; width:100%", src=file_name)
  })
  
  output$GO_composite_pdf = renderText({
    
    file_name = GO_composite_file()
    file_name
    print(file_name)
    
    #return(paste('<iframe style="height:100px; width:1000px" src="', file_name, '"></iframe>', sep = ""))
    # paste0('<object data="',file_name,'" type="application/pdf" width="700px" height="700px">',
    #        '<embed src="',file_name,'" type="application/pdf">',
    #        '<p>This browser does not support PDFs. Please download the PDF to view it: <a href="',file_name,'">Download PDF</a>.</p>',
    #        '</embed>',
    #        '</object>')
    
    paste0("<div id='pdf'>",
      "<object width='700' height='100' type='application/pdf' data='",file_name,"?#zoom=100&scrollbar=0&toolbar=0&navpanes=0' id='pdf_content'>",
      "<p>Insert your error message here, if the PDF cannot be displayed.</p>",
      "</object>",
      "</div>")
    #print(file_output)
    #file_output
    
    # paste0('<object data="',file_name,'" type="application/pdf" width="1000px" height="150px">',
    #        '<embed WMODE="transparent" src="',file_name,'" type="application/pdf">',
    #        '<p>This browser does not support PDFs. Please download the PDF to view it: <a href="',file_name,'">Download PDF</a>.</p>',
    #        '</embed>',
    #        '</object>')
  })
  
  output$GO_composite_path = renderText(GO_composite_file())


  ### probably not used anymoer
  select_data = reactive({
    print('select data')
    gene_list = gene_list()
    #print(gene_list)
    ESC_data = ESC_mapped_all[ESC_mapped_all$id %in% gene_list,]
    GE_data = GE_mapped_all[GE_mapped_all$id %in% gene_list,]
    SILAC_data = SILAC_mapped_all[SILAC_mapped_all$id %in% gene_list,]
    NES_data = NES_Diff_mapped_all[NES_Diff_mapped_all$id %in% gene_list,]
    NS_data = NS_Diff_mapped_all[NS_Diff_mapped_all$id %in% gene_list,]
    
    #print('Select')
    #print(ESC_data)
    print('   select data : done')
    
    list(ESC_data = ESC_data, GE_data = GE_data, SILAC_data = SILAC_data, NES_data = NES_data, NS_data = NS_data)
    
  })
  ###

  heatmap_data = reactive({
    print('heatmap_data')
    
    old = F
    if(old == T){
      ESC_sig = sig_data()$ESC_data
      GE_sig = sig_data()$GE_data
      SILAC_sig = sig_data()$SILAC_data
      NES_sig = sig_data()$NES_data
      NS_sig = sig_data()$NS_data
      
      # ESC_selected = sig_ESC[sig_ESC$id %in% gene_list,]
      # GE_selected = sig_GE[sig_GE$id %in% gene_list,]
      # SILAC_selected = sig_SILAC[sig_SILAC$id %in% gene_list,]
      # NES_selected = sig_NES[sig_NES$id %in% gene_list,]
      # NS_selected = sig_NS[sig_NS$id %in% gene_list,]
      # NS_selected
      
      #gene_list = c('SOX2','LIN28','TUBB3','GFAP')
      gene_list = gene_list()
      
      #print('gene_list')
      #print(length(gene_list))
      #print(gene_list)
      plot_data =  data.frame(id = gene_list)
      #print(plot_data)
      plot_data
      #print(ESC_sig)
      #print(GE_sig)
      #print(SILAC_sig)
      #print(NES_sig)
      #print(NS_sig)
      plot_data$ESC = ESC_sig$mean[match(plot_data$id,ESC_sig$id)]
      plot_data$GE = GE_sig$mean[match(plot_data$id,GE_sig$id)]
      plot_data$SILAC = SILAC_sig$mean[match(plot_data$id,SILAC_sig$id)]
      plot_data$NES_Diff = NES_sig$slope[match(plot_data$id,NES_sig$id)]
      plot_data$NS_Diff = NS_sig$slope[match(plot_data$id,NS_sig$id)]
    }
    gene_list = gene_list()
    plot_data =  data.frame(id = gene_list)
    for(entry in sample_names()){
      #print(entry)
      sig_data = sig_data_list()[[entry]]
      cutoff_list_entry = data_df()[entry,'data']
      #print(cutoff_list_entry)
      plot_data[,entry] = sig_data[,cutoff_list_entry][match(plot_data$id,sig_data$id)]
    }
    
    #print(plot_data)
    #View(plot_data)
    plot_data
    rownames(plot_data) = plot_data$id
    plot_data$id = NULL
    #plot_data_list = c('ESC','GE','SILAC')
    #print(colnames(plot_data))
    #selected_data_list = input$data
    #print(selected_data_list)
    #selected_data_list = selected_data_list[selected_data_list %in% plot_data_list]
    #print(selected_data_list)
    #print(names(selected_data_list))
    #selected_data_list = c('ESC','GE','SILAC')
    #print(colnames(plot_data))
    plot_data = plot_data[rownames(plot_data)[order(rownames(plot_data))],]
    plot_data
    full_data = plot_data
    if(input$heatmap_sig == T){
      plot_data = plot_data[apply(plot_data,1,function(x) length(x[!is.na(x)])) > 0,]
    }
    print('   heatmap_data : done')
    
    list(sig_mean = plot_data, full_data = full_data)
    
    #heatmap.2(as.matrix(plot_data),Rowv = F,Colv = F,dendrogram = c("none"),col = greenred(32))
  })
##### HEATMAPS #####   
  output$selected_heatmap = renderPlot({
    print('select_heatmap :')
    plot_data = heatmap_data()$sig_mean
    save_test = T
    if(save_test == T){
      variable_list = c('plot_data')
      cmd_list = save_variable_function(variable_list)
      lapply(cmd_list, function(x) eval(parse(text = x)))
      try(save_input_function(input))
      read_test = F
      if(read_test == T){
        variable_list = c(variable_list)
        cmd_list = read_variable_function(variable_list)
        for(cmd in cmd_list){
          #print(cmd)
          try(eval(parse(text = cmd)))
        }
      }
    }
    
    #View(plot_data)
    #print(colnames(plot_data))
    #print(input$data)
    plot_data = plot_data[,input$data]
    #print(plot_data)
    col_pallete = colorRampPalette(c("green", "red"), space="rgb")(64)
    col_pallete = c('green','red')
    #heatmap.2(as.matrix(plot_data),Rowv = F,Colv = F,dendrogram = c("none"),col = col_pallete,trace = 'none',cexRow = 1,cexCol = 1)
    
    plot_data_g = plot_data
    plot_data_g$id = rownames(plot_data)
     plot_data_l = melt(plot_data_g)
    plot_data_l$sig = NA
    plot_data_l$sig[plot_data_l$value > 0] = 'upregulated'
    plot_data_l$sig[plot_data_l$value < 0] = 'downregulated'
    #plot_data_l$level = plot_data_l$sig
    #plot_data_l$sig = factor(plot_data_l$sig)
    #levels(plot_data_l$sig) = plot_data_l$level
    #plot_data
    head(plot_data_l,30)
    value_list = list('red' = 'upregulated', 'green' = 'downregulated')
    value_list
    length = dim(plot_data)[1]
    length
    ratio = (1/length) * input$heatmap_ratio_factor
    if(ratio > 1){
      ratio == 1
    }
    p = ggplot(plot_data_l) + 
      geom_tile(aes(x = variable, y = id, fill = sig), height = 0.9, width = 0.9) +
      ylab(input$heatmap_ylab) + 
      xlab(input$heatmap_xlab) +
      coord_fixed(ratio = ratio) + 
      #coord_fixed(ylim = 1) +
      #scale_fill_manual(breaks = c('1', '-1'),values = c('red','green'),labels = c('upregulated','downregulated'),name = 'Significantly')
      #scale_fill_manual(breaks = c('downregulated', 'upregulated'),values = c('green','red'),name = 'Significantly')
    #scale_fill_manual(breaks = c('upregulated', 'downregulated'),values = c('green','red'),name = 'Significantly')
    scale_fill_manual(breaks = c('upregulated', 'downregulated'),values = c('green','red'),labels = value_list,name = 'Significantly')
    
      #scale_fill_manual(breaks = c('-1', '1'),values = c('green','red'),labels = c('downregulated','upregulated'),name = 'Significantly')
    print(p)
    if(input$save_plot == T){
      plot_name = 'neatmap_mean'
      if(input$data_type_radio == 'venn'){
        plot_name = latex_filename_function(paste(plot_name,paste(input$venn_int,collapse = '_')))
      }
      values$plot_name = paste0(plot_name)
      save_plot_function_2(path_line(),plot_name,c('pdf'))
    }
  })

  
  output$selected_heatmap_base = renderPlot({
    print('select_heatmap base :')
    plot_data = heatmap_data()$full_data
    #data = data_df_list()[[input$single_sample]]
    if(dim(plot_data)[1]>0){
      plot_data_g = plot_data
      plot_data_g$id = rownames(plot_data)
      plot_data_l = melt(plot_data_g)
      plot_data_l$sig = NA
      plot_data_l$sig[plot_data_l$value > 0] = 'upregulated'
      plot_data_l$sig[plot_data_l$value < 0] = 'downregulated'
      head(plot_data_l,30)
      value_list = list('red' = 'upregulated', 'green' = 'downregulated')
      value_list
      length = dim(plot_data)[1]
      length
      ratio = (1/length) * input$heatmap_ratio_factor
      if(ratio > 1){
        ratio == 1
      }
      p = ggplot(plot_data_l) + 
        geom_tile(aes(x = variable, y = id, fill = sig), height = 0.9, width = 0.9) +
        #ylab(input$heatmap_ylab) + 
        #xlab(input$heatmap_xlab) +
        #coord_fixed(ratio = ratio) + 
        scale_fill_manual(breaks = c('upregulated', 'downregulated'),values = c('green','red'),labels = value_list,name = 'Significantly')
       print(p)
    }
  })
  
  
  output$selected_heatmap_ggplot = renderPlot({
    print('select_heatmap_ggplot')
    #plot_data = heatmap_data()$sig_mean
    plot_data = m()
    plot_data = plot_data[plot_data$id %in% gene_list(),]
    #saveRDS(plot_data,'temp/plot_data.rds')
    
    
    p = ggplot(plot_data, aes(data,id)) + 
      geom_tile(aes(fill = sig))
    print(p)
    q = p +  theme(
      plot.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
    print('   select_heatmap_ggplot : done')
    
    print(q)
    # col_pallete = colorRampPalette(c("green", "red"), space="rgb")(64)
    # heatmap.2(as.matrix(plot_data),Rowv = F,Colv = F,dendrogram = c("none"),col = col_pallete,trace = 'none',cexRow = 1,cexCol = 1)
    # 
    if(input$save_plot == T){
      plot_name = 'neatmap_mean'
      if(input$data_type_radio == 'venn'){
        plot_name = latex_filename_function(paste(plot_name,paste(input$venn_int,collapse = '_')))
      }
      values$plot_name = paste0(plot_name,'.pdf')
      save_plot_function_2(path_line(),plot_name,c('pdf'))
    }
  })
  
  
  #### __full ####
  full_selected_heatmap_data = reactive({
    print('full_selected_heatmap : ')
    gene_list = gene_list()
    #print(gene_list)
    #data = data_df_list()[[input$single_sample]]
    data_list = data_df_list()
    length(data)
    data_df = data_df()
    dim(data_df)
    #data = data
    dim(data)
    sample_name_list = names(data_list)
    sample_name = names(data_list)[2]
    sample_name
    heatmap_data_list = list()
    for(sample_name in sample_name_list){

      data = data_list[[sample_name]]
      dim(data)
      data_cols = (unlist(strsplit(data_df()[sample_name,'cols'],', ')))
      data_cols
      ts_cols = c('id',data_cols)
      ts_cols
      #print(ts_cols)
      colnames(data)
      #data = select_data()$NES_data
      selected_data = data[data$id %in% gene_list,]
      selected_data = selected_data[,ts_cols]
      colnames(selected_data)
      selected_data = delete.na(selected_data, length(ts_cols)-2)
      dups = duplicated(selected_data$id)
      while(TRUE %in% dups){
        selected_data$id[dups] = paste0(selected_data$id[dups],'.1')
        dups = duplicated(selected_data$id)
      }
      selected_data$id
      selected_data = selected_data[order(selected_data$id),]
      plot_data = selected_data
      rownames(plot_data) = plot_data$id
      plot_data$id = NULL
      heatmap_data_list[[sample_name]] = plot_data
    }
    heatmap_data_list
  })
  
  output$heatmap_sample_select = renderUI({
    sample_list = names(data_df_list())
    selectInput('heatmap_full_sample','Select Sample',sample_list)
  })
  
  output$full_selected_heatmap = renderPlot({
    print('full_selected_heatmap')
    data_list = full_selected_heatmap_data()
    length(data_list)
    plot_data = full_selected_heatmap_data()[[input$heatmap_full_sample]]
    dim(plot_data)
    col_pallete = colorRampPalette(c("green",'black', "red"), space="rgb")(64)
    heatmap.2(as.matrix(plot_data),Rowv = F,Colv = F,dendrogram = c("none"),col = col_pallete,trace = 'none',cexRow = 1,cexCol = 1)

    
    if(input$save_plot == T){
      plot_name = paste0('neatmap_',input$single_sample)
      if(input$data_type_radio == 'venn'){
        plot_name = latex_filename_function(paste(plot_name,paste(input$venn_int,collapse = '_')))
      }
      values$plot_name = paste0(plot_name,'.pdf')
      save_plot_function_2(path_line(),plot_name,c('pdf'))
    }
  })
  

  output$ls = renderPrint(print(ls(.GlobalEnv)))
  
  
  #### Venn ####
  
    #### Venn all ####
        output$venn_all_select = renderUI({
          
          entry_list = input$data
          #entry_list = data_name_list
          venn_data_list = unlist(lapply(entry_list,function(x) c(paste0(x,' down'),paste0(x,' up'))))
          venn_data_list
          selectInput(inputId = 'venn_data_select',  # Drop down menu to select the producer and cultivar
                      label = 'Select Venn Data',
                      choices = venn_data_list,
                      selected = venn_data_list,
                      multiple = T)
        })
  # output$venn_id_select = renderUI({ ### NOT SURE WHAT THIS IS MEANT TO BE
  #   
  #   entry_list = input$data
  #   #entry_list = data_name_list
  #   venn_data_list = unlist(lapply(entry_list,function(x) c(paste0(x,' down'),paste0(x,' up'))))
  #   venn_data_list
  #   selectInput(inputId = 'venn_id_select',  # Drop down menu to select the producer and cultivar
  #               label = 'Select Venn Data',
  #               choices = venn_data_list,
  #               selected = venn_data_list,
  #               multiple = T)
  # })
  
  output$enrichement_data_select_ui = renderUI({
    print('enrichment_data_select_ui : ')
    entry_list = input$data
    entry_list
    #entry_list = data_name_list
    if(input$sub_venn == 'basic'){
      venn_data_list = unlist(lapply(entry_list,function(x) c(paste0(x,' down'),paste0(x,' up'))))
      venn_data_list
      selected = venn_data_list
      selected
    }else{
      int = names(attr(venn_all_gplots_data(), 'intersection'))
      int = int[order(int)]
      venn_data_list = int
      selected = venn_data_list[1]
    }
    venn_data_list
    venn_label = 'Select Venn Data'
    if(input$sub_venn == 'combined'){
      venn_label = 'Down Venn Data'
      selected = c()
      for(entry in venn_data_list){
        if(grepl('down',entry)){
          if(!grepl('up',entry)){
            selected = c(selected,entry)
          }
        }
      }
    }
    
    selectInput(inputId = 'enrichment_data_select',  # Drop down menu to select the producer and cultivar
                label = venn_label,
                choices = venn_data_list,
                selected = selected,
                multiple = T)
  })
  output$enrichement_data_select_ui_2 = renderUI({
    print('enrichement_data_select_ui_2 : ')
    entry_list = input$data
    #entry_list = data_name_list
    if(input$sub_venn == 'basic'){
      venn_data_list = unlist(lapply(entry_list,function(x) c(paste0(x,' down'),paste0(x,' up'))))
      selected = venn_data_list
    }else{
      int = names(attr(venn_all_gplots_data(), 'intersection'))
      int = int[order(int)]
      venn_data_list = int
      selected = venn_data_list[2]
    }
    venn_data_list
    if(input$sub_venn == 'combined'){
      selected = c()
      for(entry in venn_data_list){
        if(grepl('up',entry)){
          if(!grepl('down',entry)){
            selected = c(selected,entry)
          }
        }
      }
      selectInput(inputId = 'enrichment_data_select_2',  # Drop down menu to select the producer and cultivar
                  label = 'Up Venn Data',
                  choices = venn_data_list,
                  selected = selected,
                  multiple = T)
    }
  })
        
        
        venn_all_reactive = reactive({
          print('venn_all_reactive')
          
          mapped_data = mapped_data()
          venn_list = venn_gene_list_select()
          if(save_test == T){
            variable_list = c('mapped_data','venn_list')
            cmd_list = save_variable_function(variable_list)
            lapply(cmd_list, function(x) eval(parse(text = x)))
            try(save_input_function(input))
            read_test = F
            if(read_test == T){
              
              cmd_list = read_variable_function(variable_list)
              cmd_list
              for(cmd in cmd_list){
                #print(cmd)
                try(eval(parse(text = cmd)))
              }
            }
          }
          
          entry_list = input$venn_data_select
          entry_list
          #print(entry_list)
          mapped_ud = mapped_data$mapped_ud
          dim(mapped_ud)
          venn_column = input$venn_id_select
          venn_column = 'id'
          
          mapped_ud = mapped_ud[mapped_ud[,venn_column] %in% venn_list,]
          #mapped_ud = mapped_ud[,entry_list]
          dim(mapped_ud)
          
          colour_list = mapped_data$colour_list
          #print(colour_list)
          #print(colour_list[entry_list])
          plot_data_list = list()
          entry = entry_list[1]
          for(entry in entry_list){
            #plot_data_list[entry] = list(mapped_ud[,entry][mapped_ud[,entry] != 0])
            plot_data_list[entry] = list(mapped_ud[,venn_column][mapped_ud[,entry] != 0])
            plot_data_list
            #print(plot_data_list[entry])
          }
          colour_list = colour_list[entry_list]
          print('   venn_all_reactive : done')
          
          list(plot_data_list = plot_data_list, colour_list = colour_list)
        })
        
        
        
        output$venn_plot_path_print = renderText({
          path_print = paste0(venn_plot_path_r(),'/',latex_filename_function(venn_plot_name_r()),'.pdf')
          path_print = sub('./',thesis_path_sub,path_print)
          #path_print = paste0(latex_filename_function(path_print),'.pdf')
          })                          
        #Thesis_Data/Cleanup_Data/images/shiny/Venn/ESC_GE_NES_Diff_NS_Diff_SILAC/ALL_ESC_down_ESC_up_GE_down_GE_up.pdf
        
        output$venn_all = renderPlot({
          print('venn all')
          #plot_path = paste(venn_image_path(),plot_name,spe='/')
          #pdf(plot_name)
          plot_data_list = venn_all_reactive()$plot_data_list
          plot_data_list
          plot_data_list_name = input$venn_data_select
          colour_list = venn_all_reactive()$colour_list
          plot_name = latex_filename_function(paste((plot_data_list_name),collapse='_'))
          values$plot_name = paste0(plot_name)
          m = 1.5 #numbers
          n = 1.5 # sample labels
          par(bty = 'n', lty = 'blank')
          tryCatch({
            p = venn(plot_data_list, cexil = m, cexsn = n,
                 ilabels = T, counts = T,
                 zcolor = colour_list,
                 ellipse = T)
          }, error = function(e) {
            p = venn(plot_data_list, cexil = m, cexsn = n,
                 ilabels = T, counts = T,
                 zcolor = colour_list,
                 ellipse = F)
          })
          if(length(input$venn_data_select) <= 6){
            if(input$save_plot == T){
              #plot_name = 'venn'
              #values$plot_name = paste0(plot_name,'.pdf')
              save_plot_function_2(path_line(),plot_name,c('pdf'))
            }
          }
          
          
        })
      
        output$venn_all_gplots = renderPlot({
          plot_data_list = venn_all_reactive()$plot_data_list
          colour_list = venn_all_reactive()$colour_list
          p = gplots::venn(plot_data_list)
          
        })
        
        venn_all_gplots_data = reactive({
          print('venn_all_gplots_data')
          plot_data_list = venn_all_reactive()$plot_data_list
          colour_list = venn_all_reactive()$colour_list
          p = gplots::venn(plot_data_list,show.plot = F)
          print('   venn_all_gplots_data : done')
          
          p
        })
        
        
        output$venn_intersection_ui = renderUI({
          int = names(attr(venn_all_gplots_data(), 'intersection'))
          int = int[order(int)]
          int_select = int[2]
          if(input$venn_data_select_button == 'gene_list'){
            int_select = int
          }
          selectInput('venn_int','Select Venn Intersections',int,selected = int_select,multiple = T)
        })
        
        output$venn_int_text_print = renderText(print(paste(input$venn_int, collapse = '  &  ')))
        
        venn_gene_list = reactive({
          print('venn_gene_list')
          p = attr(venn_all_gplots_data(), 'intersection')
          #print(paste(c(p[input$venn_int])))
          #v_list = sapply(input$venn_int,function(x) p[x])
          v_list = c()
          int = input$venn_int
          for(entry in int){
            p_list = (p[int])
            for(e in p_list){
              v_list = c(v_list,e)
            }
          }
          #v_list v_list
          mapped_ud = mapped_data()$mapped_ud
          venn_column = input$venn_id_select
          venn_column = 'id'
          v_list_id = mapped_ud$id[mapped_ud[,venn_column] %in% unique(v_list)]
          print('   venn_gene_list : done')
          
          paste(unique(v_list_id))
        })
        
        output$venn_int_print = renderPrint({
          v_list = venn_gene_list()
          #print(str(v_list))
          #print(summary(v_list))
          #print((v_list))
          #print(class(v_list))
          print(v_list)
        })
        
    #### Venn Select ####
        
        output$venn_select_select = renderUI({
          
          entry_list = input$data
          #entry_list = data_name_list
          venn_data_list = unlist(lapply(entry_list,function(x) c(paste0(x,' down'),paste0(x,' up'))))
          venn_data_list
          selectInput(inputId = 'venn_data_select_select',  # Drop down menu to select the producer and cultivar
                      label = 'Select Venn Data',
                      choices = venn_data_list,
                      selected = venn_data_list,
                      multiple = T)
        })
        

        
        
        
        
        
        
        
        output$venn_intersection_ui_select = renderUI({
          int = names(attr(venn_select_gplots_data(), 'intersection'))
          int = int[order(int)]
          selectInput('venn_int','Select Venn Intersections',int,int[1],multiple = T)
        })
        
        venn_select_gplots_data = reactive({
          print('venn_select_gplots_data')
          plot_data_list = venn_select_reactive()$plot_data_list
          colour_list = venn_select_reactive()$colour_list
          p = gplots::venn(plot_data_list,show.plot = F)
          print('   venn_select_gplots_data : done')
          
          p
        })
        
        venn_select_gene_list = reactive({
          print('venn_select_gene_list')
          p = attr(venn_select_gplots_data(), 'intersection')
          #print(paste(c(p[input$venn_int])))
          #v_list = sapply(input$venn_int,function(x) p[x])
          v_list = c()
          int = input$venn_int
          for(entry in int){
            p_list = (p[int])
            for(e in p_list){
              v_list = c(v_list,e)
            }
          }
          #v_list v_list
          print('   venn_select_gene_list : done')
          paste(unique(v_list))
        })
        
        output$venn_select_int_print = renderPrint({
          v_list = venn_gene_list()
          #print(str(v_list))
          #print(summary(v_list))
          #print((v_list))
          #print(class(v_list))
          print(v_list)
        })
        
        venn_select_reactive = reactive({
          print('venn_select_reactive')
          test = F
          if(test == T){
            mapped_ud_full = results_list$mapped_ud
            colour_list = results_list$colour_list
            entry_list = c('ESC up','ESC down')
            
          }
          int = neighbour_data()$int
          entry_list = input$venn_data_select_select
          #print(entry_list)
          mapped_ud_full = mapped_data()$mapped_ud
          head(mapped_ud_full)
          #print(dim(mapped_ud_full))
          mapped_ud = mapped_ud_full[mapped_ud_full$STRING_id %in% int,]
          #print(dim(mapped_ud))
          #print(head(mapped_ud))
          colour_list = mapped_data()$colour_list
          #print(colour_list)
          #print(colour_list[entry_list])
          plot_data_list = list()
          #print(entry_list)
          for(entry in entry_list){
            #print(entry)
            plot_data_list[entry] = list(mapped_ud$id[mapped_ud[,entry] != 0])
            #print(plot_data_list[entry])
          }
          colour_list = colour_list[entry_list]
          print('   venn_select_reactive : done')
          
          list(plot_data_list = plot_data_list, colour_list = colour_list)
        })
        
        output$venn_select = renderPlot({
          print('venn_select : ')
          if(values$shouldShow_sn_select){
            plot_data_list = venn_select_reactive()$plot_data_list
            #print(plot_data_list)
            colour_list = venn_select_reactive()$colour_list
            #print(colour_list)
            tryCatch({
              venn(plot_data_list, cexil = 2, cexsn = 2,
                   ilabels = T, counts = T,
                   zcolor = colour_list,
                   ellipse = T)
            }, error = function(e) {
              venn(plot_data_list, cexil = 2, cexsn = 2,
                   ilabels = T, counts = T,
                   zcolor = colour_list,
                   ellipse = F)
            })
          }
        })

        output$venn_select_gplots_print = renderPrint({
          print('venn_select_gplots_print')
          if(values$shouldShow_sn_select){
            plot_data_list = venn_select_reactive()$plot_data_list
            p = gplots::venn(plot_data_list,show.plot = F)
            #print(class(p))
            #print(names(p))
            #print(str(p))
            print(class(p))
            print(summary(p))
            print(str(p))
            print(names(p))
            print(p)
          }
        })
  
        
        #### Venn GO ####
        
        output$venn_GO_select = renderUI({
          print('venn_GO_select')
          
          entry_list = input$data
          #entry_list = data_name_list
          venn_data_list = unlist(lapply(entry_list,function(x) c(paste0(x,' down'),paste0(x,' up'))))
          venn_data_list
          selectInput(inputId = 'venn_data_GO_select',  # Drop down menu to select the producer and cultivar
                      label = 'Select Venn Data',
                      choices = venn_data_list,
                      selected = venn_data_list,
                      multiple = T)
        })
        
        venn_GO_reactive = reactive({
          print('venn_GO_reactive')
          test = F
          if(test == T){
            mapped_ud_full = results_list$mapped_ud
            colour_list = results_list$colour_list
            entry_list = c('ESC up','ESC down')
            
          }
          int = string_hits_list()
          entry_list = input$venn_data_select_select
          #print(entry_list)
          mapped_ud_full = mapped_data()$mapped_ud
          #View(mapped_ud_full)
          head(mapped_ud_full)
          dim(mapped_ud_full)
          mapped_ud = mapped_ud_full[mapped_ud_full$STRING_id %in% int,]
          dim(mapped_ud)
          head(mapped_ud_full)
          colour_list = mapped_data()$colour_list
          #print(colour_list)
          #print(colour_list[entry_list])
          plot_data_list = list()
          for(entry in entry_list){
            plot_data_list[entry] = list(mapped_ud$id[mapped_ud[,entry] != 0])
            #print(plot_data_list[entry])
          }
          colour_list = colour_list[entry_list]
          print('   venn_GO_reactive : done')
          
          list(plot_data_list = plot_data_list, colour_list = colour_list)
        })
        
        output$venn_GO = renderPlot({
          print('venn_GO :')
          if(values$shouldShow){
            if(input$mapped == 'common_mapped'){
              plot_data_list = venn_GO_reactive()$plot_data_list
              colour_list = venn_GO_reactive()$colour_list
              tryCatch({
                venn(plot_data_list, cexil = 2, cexsn = 2,
                     ilabels = T, counts = T,
                     zcolor = colour_list,
                     ellipse = T)
              }, error = function(e) {
                venn(plot_data_list, cexil = 2, cexsn = 2,
                     ilabels = T, counts = T,
                     zcolor = colour_list,
                     ellipse = F)
              })
            }
          }
        })
        
        output$venn_GO_gplots_print = renderPrint({
          print('venn_GO_gplots_print : ')
          if(values$shouldShow_sn_select){
            plot_data_list = venn_select_reactive()$plot_data_list
            p = gplots::venn(plot_data_list,show.plot = F)
            #print(class(p))
            #print(names(p))
            #print(str(p))
            print(class(p))
            print(summary(p))
            print(str(p))
            print(names(p))
            print(p)
          }
        })
  
    ### BOXPLOTS ####
        m = reactive({
          print('m')
          #print('Start : m')
          start_time = Sys.time()
          data_select_list = input$data
          #if(input$boxplot_full == 'full'){
          #  data_select_list = rownames(data_df())
          #  print(data_select_list)
          #}
          #data_name_collapse = paste(data_select_list,collapse = '_')
          
          m_file = paste0(shiny_data_path,'/',data_name_collapse(),'_m.rds')
          #print(m_file)
          #print(file.exists(m_file))
          if(file.exists(m_file) == FALSE | input$re_melt == T){
            print('re-run m')
 
            m = common_melt_function(data_select_list, data_df_list(), sig_data_list(), data_df(), timeseries_list())
            saveRDS(m,m_file)
          }else{
            print('readRDS(m)')
            m = readRDS(m_file)
            print('done')
          }
          print('End : m')
          print(Sys.time() - start_time)
          print("   m : done")
          m
        })
        
        m_ts = reactive({
          print('m_ts')
          m_file = paste0(shiny_data_path,'/',data_name_collapse(),'_m_ts.rds')
          #print(m_file)
          #print(file.exists(m_file))
          if(file.exists(m_file) == FALSE | input$re_melt == T){
            print('re-running m_ts')
            timeseries_list = timeseries_list()
            m_ts = m()
            n = apply(m_ts, 1, function(x) ifelse(x['variable'] %in% names(timeseries_list),timeseries_list[[x['variable']]],NA))
            #View(n)
            m_ts$ts = n
            #m_ts = m[!is.na(m$ts),]
            #View(m_ts)
            saveRDS(m_ts,m_file)
          }else{
            print('readRDS(m_ts)')
            m_ts = readRDS(m_file)
          }
          print('   m_ts : done')
          m_ts
        })
        output$m_table = renderDataTable(m())
        output$m_ts_table = renderDataTable(m_ts())
      #### ___pdfs ####
        
      output$boxplot_pdfs = renderText({
        gene_list = gene_list()
        html_list = c()
        for(name in gene_list){
          #/Users/sgarnett/Documents/Doctorate/Thesis/Thesis_Pictures/MQ_Gui/marker_lists/ESC_GE_SILAC_Diff/all/A1BG.pdf
          file_name = paste0('all/',name,'.pdf')
          #print(file_name)
          html_entry = paste0("<div id='pdf'>",
                   "<object width='700' height='700' type='application/pdf' data='",file_name,"?#zoom=100&scrollbar=0&toolbar=0&navpanes=0' id='pdf_content'>",
                   "<p>Insert your error message here, if the PDF cannot be displayed.</p>",
                   "</object>",
                   "</div>")
        html_list = c(html_list,html_entry)
        }
        paste(html_list,collapse = c(''))
      })
      
      output$boxplot_pdfs_GO = renderText({
        id_list = mapped_data()$id_list
        print(id_list)
        gene_list = id_list[string_hits_list()]
        print(gene_list)
        html_list = c()
        for(name in gene_list){
          #/Users/sgarnett/Documents/Doctorate/Thesis/Thesis_Pictures/MQ_Gui/marker_lists/ESC_GE_SILAC_Diff/all/A1BG.pdf
          file_name = paste0('all/',name,'.pdf')
          print(file_name)
          html_entry = paste0("<div id='pdf'>",
                              "<object width='700' height='700' type='application/pdf' data='",file_name,"?#zoom=100&scrollbar=0&toolbar=0&navpanes=0' id='pdf_content'>",
                              "<p>Insert your error message here, if the PDF cannot be displayed.</p>",
                              "</object>",
                              "</div>")
          html_list = c(html_list,html_entry)
        }
        paste(html_list,collapse = c(''))
      })
      
      
      ### ___SELECTED ####
      
      ### _____all ####
      output$boxplot_gplot = renderPlot({
        data_select_list = input$data
        #print(data_select_list)
        m = m()
        #print('m')
        #print(dim(m))
        #print(unique(m$data))
        gene_list = gene_list()
        #print(gene_list)
        sub_m = m[m$id == gene_list,]
        #print(dim(sub_m))
        ggplot(sub_m, aes(x = data,y = value,col = id)) + 
          geom_boxplot() +
          geom_hline(yintercept = 0) +
          scale_x_discrete(limits = data_select_list)
        
      })
      
     
    output$boxplot_range_ui = renderUI({
      gene_list = gene_list()
      sliderInput('boxplot_range','boxplots displayed',min = 1,max = length(gene_list), value =  c(1,2),step = 1,width = 800)
    })
    
    output$boxplot_gene_select = renderUI({
      print('boxplot_gene_select')
      gene_list = gene_list()
      gene_list = gene_list[order(gene_list)]
      
      
      if(input$boxplot_subset == 'select'){
        selected_genes = gene_list[1:2]
      }else{
        selected_genes = gene_list
      }
      #sliderInput('boxplot_range','boxplots displayed',min = 1,max = length(gene_list), value =  c(1,2),step = 1,width = 800)
     
        selectInput('boxplot_gene_select', 'Select Genes', gene_list, selected_genes,multiple = T, width = 1000)

      
        })

    
    output$boxplot_plot_text = renderText({
      plot_path = paste0(thesis_path_sub,values$boxplot_path)
      plot_path
    })
    
    output$boxplot_ts_plot_text = renderText({
      plot_path = paste0(thesis_path_sub,values$boxplot_ts_path)
      plot_path
      })
      
    output$g_boxplots_plots <- renderUI({
      gene_list = gene_list()
      gene_list = gene_list[order(gene_list)]
      #gene_list = gene_list[input$boxplot_range[1]:input$boxplot_range[2]]
      gene_list = gene_list[gene_list %in% input$boxplot_gene_select]
      sample_list = input$data
      mean_sample_list = rownames(data_df()[data_df()$data == 'mean',])
      mean_sample_list
      sample_list = mean_sample_list[mean_sample_list %in% sample_list]
      if(length(sample_list) > 0){
        sample_list_collapse = paste(sample_list[order(sample_list)],collapse = '_')
        sample_path_line = paste0(shiny_image_path,sample_list_collapse,'/boxplot/')
        
        values$boxplot_path = sample_path_line
        
        values$plot_path = paste0(sample_path_line)
        values$plot_name = 'GENE_ID'
        #View(m())
        #m_data = m()[m()$id %in% gene_list,]
        boxplot_hit = 1
        for(gene in gene_list){
          plot_path = paste0(sample_path_line,'/',gene,'.png')
          if(file.exists(plot_path) == FALSE){
            boxplot_hit = 0
          }
        }
        #print(boxplot_hit)
        if(boxplot_hit == 0 | input$re_run_boxplots == TRUE){
          #print('generate images')
          #print('test')
          if(input$boxplot_facets == T){
            renderPlots_facets(m(), data_df(), sample_list, gene_list ,sample_path_line,input, output, 'gplot')
          }else{
            renderPlots(m(), data_df(), sample_list, gene_list ,sample_path_line,input, output, 'gplot')
            
          }
          }else{
          #print('upload images')
          renderImages(gene_list,sample_path_line,input,output,'gplot')
        }
        makePlotContainers(gene_list, 'gplot')
        
        }
      

      
    })
     observeEvent(input$gene_boxplot,{
       if(values$gene_boxplot){
         sample_list = input$data
         gene_list = gene_list()
         gene_list = gene_list[order(gene_list)]
         gene_list = gene_list[input$boxplot_range[1]:input$boxplot_range[2]]

         renderPlots(m(), sample_list, gene_list ,sample_path_line(),input, output, 'gplot')
       }
     })
     
     
     #### ____ sample ####
     
     output$g_boxplots_plots_sample <- renderUI({
       sample_list = input$data
       sample_list = input$data
       gene_list = gene_list()
       renderPlots_sample(m(),sample_list, gene_list, path_line(), input, output, 'gplot')
       
       makePlotContainers_sample(sample_list)
     })
     # observeEvent(input$sample_boxplot,{
     #   #data_select_list = input$data
     #   sample_list = input$data
     #   gene_list = gene_list()
     #   renderPlots_sample(m(),sample_list, gene_list, path_line(), input, output, 'gplot')
     # })
     # 
     # observeEvent(input$sample_boxplot,{
     #   #data_select_list = input$data
     #   sample_list = input$data
     #   gene_list = gene_list()
     #   renderPlots_sample(m(),sample_list, gene_list, path_line(), input, output, 'gplot')
     # })
     
     #### ____ timeseries ######
     
     
     output$g_boxplots_plots_ts <- renderUI({
       gene_list = gene_list()
       gene_list = gene_list[order(gene_list)]
       #gene_list = gene_list[input$boxplot_range[1]:input$boxplot_range[2]]
       gene_list = gene_list[gene_list %in% input$boxplot_gene_select]
       
       #View(m_ts())
       
       sample_list = input$data
       mean_sample_list = rownames(data_df()[data_df()$data == 'slope',])
       mean_sample_list
       
       #if(input$boxplot_full == 'subset'){
         #sample_list = sample_list[sample_list %in% mean_sample_list]
       #}else{
        # sample_list = mean_sample_list
       #}
       #sample_list = sample_list[sample_list %in% mean_sample_list]
       sample_list = mean_sample_list[mean_sample_list %in% sample_list]
      if(length(sample_list) > 0){
         sample_list_collapse = paste(sample_list[order(sample_list)],collapse = '_')
         sample_path_line = paste0(shiny_image_path,sample_list_collapse,'/boxplot_ts/')
         
         values$boxplot_ts_path = sample_path_line
         values$plot_name = 'id.pdf'
         
         
         
         boxplot_hit = 1
         for(gene in gene_list){
           plot_path = paste0(sample_path_line,'/',gene,'.png')
           if(file.exists(plot_path) == FALSE){
             boxplot_hit = 0
           }
         }
         #print(boxplot_hit)
         if(boxplot_hit == 0 | input$re_run_boxplots == TRUE){
           #print('generate images ts')
           renderPlots_ts(m_ts(), sample_list, gene_list, timeseries_list() ,sample_path_line,input, output, 'gplot_ts')
         }else{
           #print('upload images ts')
           renderImages(gene_list,sample_path_line,input,output,'gplot_ts')
         }
         makePlotContainers(gene_list, 'gplot')
         
         
         
         makePlotContainers(gene_list, 'gplot_ts')
      }
       
     })
     observeEvent(input$gene_boxplot_ts,{
       sample_list = input$data
       gene_list = gene_list()
       renderPlots_ts(m_ts(), sample_list, gene_list, timeseries_list() ,sample_path_line(),input, output, 'gplot_ts')
     })
     
     output$timeseries_boxplot = renderPlot({
       print('timeseries_boxplot')
       m = m_ts()
       
       m = m[m$id %in% gene_list(),]
       m = m[!is.na(m$ts),]
       ggplot(m, aes(x = ts,y = value, col = data, fill = id)) + 
         geom_boxplot() + 
         scale_x_discrete(limits = timeseries_list()[['order']]) +
         
        stat_summary(fun.y=mean,geom='line',lwd=2,aes(group = id,col=data))
         
       
     })
     
     
     output$string_db_ppi_enrichment = renderPlot({
       string_db$plot_ppi_enrichment(string_hits_list(), quiet = TRUE)
     })
     
     sn_enrichment = reactive({
       print('sn_enrichment : ')
       #print(string_hits_list())
       enrichmentGO <- string_db$get_enrichment(string_hits_list(), category = "Process", methodMT = "fdr", iea = TRUE )
        #print(enrichmentGO)
        print('   sn_enrichment : done')
        
        enrichmentGO
       })
     
     sn_enrichment_table = renderDataTable({
       sn_enrichment()
     })
 
    ### TOPGO ####
     
     # topGO_all = reactive({
     #   print('topGO_all')
     #   geneNames_all_file_path = paste0(table_path(),'/geneNames_all.rds')
     #   GO2gene_all_file_path = paste0(table_path(),'/GO2gene_all.rds')
     #   
     #   if(!file.exists(geneNames_all_file_path) | input$topGO_re_run == T){
     #      print('re running topGO_all')
     #      topGO_geneNames_all(all_mapped(),annot(),column_path())
     #   }
     #   #print(length(geneNames_all))
     #   
     #   print(paste('found file :',geneNames_all_file_path))
     #   geneNames_all = readRDS(geneNames_all_file_path)
     #   GO2gene_all = readRDS(GO2gene_all_file_path)
     #   topGO_all_list = list(geneNames_all = geneNames_all,GO2gene_all = GO2gene_all)
     #   
     #   topGO_all_list
     # })
     
     output$geneNames_all_print = renderPrint({
       print(names(topGO_all()))
       print(length(topGO_all()$GO2gene_all))
       print(names(topGO_geneList()))
     })
     
     topGO_geneList = reactive({
       print('topGO_geneList')
       geneList_sig_file = paste(topGO_path(),'geneList_sig.rds',sep='/')
       geneList_up_file = paste(topGO_path(),'geneList_up.rds',sep='/')
       geneList_down_file = paste(topGO_path(),'geneList_down.rds',sep='/')
       
       #print(geneList_sig_file)
       
       if(!file.exists(geneList_sig_file) | input$topGO_re_run == T){
         print('re running topGO_geneList')
         geneNames_all = topGO_all()$geneNames_all
         geneList_sig = topGO_mapping(sig_mapped(),geneNames_all, annot())
         saveRDS(geneList_sig, geneList_sig_file)
         geneList_up = topGO_mapping(sig_mapped_up(),geneNames_all, annot())
         saveRDS(geneList_up, geneList_up_file)
         geneList_down = topGO_mapping(sig_mapped_down(),geneNames_all, annot())
         saveRDS(geneList_down, geneList_up_file)
       }
       geneList_sig = readRDS(geneList_sig_file)
       geneList_up = readRDS(geneList_up_file)
       geneList_down = readRDS(geneList_down_file)
       print('   topGO_geneList : done')
       
       topGO_gene_List = list(geneList_sig = geneList_sig,
                              geneList_up = geneList_up,
                              geneList_down = geneList_down)
     })
     
  ### plot paths ####
     
     
     venn_plot_path_r = reactive({
       print('venn_plot_path_r')
       if(input$venn_data_select_button == 'all'){
         venn_dc_path = create_dir_function(paste(venn_image_path,data_name_collapse(),sep='/'))
       }else{
         data_list = input$venn_data_select
         data_list = gsub('_down','',data_list)
         data_list = gsub('_up','',data_list)
         data_list = unique(data_list)
         data_list = data_list[order(data_list)]
         venn_dc_path = create_dir_function(paste(venn_image_path,paste(data_list,collapse = '_'),sep='/'))
         
       }
       print('   venn_plot_path_r : done')
       
       venn_dc_path
       
     })
     
     venn_plot_name_r = reactive({
       print('venn_plot_name_r')
       plot_name = 'sub'
       data_list = input$venn_data_select
       data_list = data_list[order(data_list)]
       if(input$venn_data_select_button == 'all'){
         plot_name = paste0('ALL_',paste(data_list,collapse = '_'))
       }
       print('   venn_plot_name_r : done')
       
       plot_name
     })
     
     

    
    sample_path_line = reactive({
      print('sample_path_line')
      path_line = paste0(shiny_image_path,data_name_collapse())
      create_dir_function(path_line)
      print('   sample_path_line : done')
      
      path_line
    })

    
    
    output$path_list_print = renderText(path_line())
    
    plot_path = reactive({
      print('plot_path')
      plot_path = values$plot_path
      plot_name = values$plot_name
      plot_path = paste0(plot_path,plot_name)
      plot_path
    })
    
    enrichment_path = reactive({
      print('enrichment_path')
      plot_path = values$plot_path
      #print(plot_path)
      
      select_enrichment_list = paste(input$enrichment_data_select,collapse = '_')
      path_list = c(input$enrichment_select,input$select_sn_MT,select_enrichment_list)
      #print(path_list)
      path_entry = plot_path
      #print(path_entry)
      
      if(input$save_plot == T){
        for(entry in path_list){
          path_entry = c(path_entry,entry)
          #print(path_entry)
          path_entry_line = latex_filename_function(paste(path_entry,collapse = '/'))
          #print(path_entry_line)
          create_dir_function(paste0(path_entry_line))
        }
      }
      print(path_entry_line)
      path_entry_line = latex_filename_function(paste(path_list,collapse = '/'))
      print(path_entry_line)
      print('    enrichment_path : done ')
      path_entry_line
      #path_entry_line
      #print(enrichment_plot_path_list)
      #enrichment_plot_path_list
      
      })
    pdf_plot_path = reactive(paste0(plot_path(),'.pdf'))
    png_plot_path = reactive(paste0(plot_path(),'.png'))
    
    
    thesis_plot_path = reactive(paste0(thesis_path_sub,plot_path()))
    thesis_pdf_plot_path = reactive(paste0(thesis_path_sub,pdf_plot_path()))
    
    output$thesis_path_print = renderText({
      thesis_pdf_plot_path()
      })
    
    
    
    ####### TOOLS ##########
    #### __foldchange #####
    
    
    output$fc = renderText(logratio2foldchange(as.numeric(input$log_2)))
    
    output$log2fc = renderPlot({
      log_2 = seq(-4,4,0.1)
      log_2
      fold_change = logratio2foldchange(log_2)
      plot(log_2,fold_change)
    })
    
    #### DOWNLOADS ####
    
    output$gene_list_download <- downloadHandler(
      #filename = paste0(getwd(),'/list/gene_list.csv'),
      filename = paste0('list_name'),
      
      content = function(file) {
        write.table(gene_list(), file, col.names = F, row.names = F)
      }
    )
    



####################################################################################################################
##### UPLOAD #####
    process_values = reactiveValues(file_upload = 0,
                                    save_upload = 0, 
                                    run_ratios = 0, 
                                    run_t_test = 0, 
                                    save_mart = 0, 
                                    save_mart_base = 0,
                                    save_mart_full = 0,
                                    run_sep_w = 0,
                                    run_sep_l = 0,
                                    sep_run = 0,
                                    mart_id = 'row_id',
                                    sep_id = 'row_id',
                                    mart_values = 'row_id',
                                    dataset_select = 'expression_data')
    
    output$upload_dataset_ui = renderUI({
      datasets = values$upload_datasets
      selected = values$upload_dataset_selected
      #observeEvent(input$save_upload,{
      #  selected = values$dataset_list$experiment_code
      #  datasets = unique(c(values$dataset_list$experiment_code,datasets))
      #})
      selectInput('upload_dataset','Upload Dataset',datasets,selected)
    })
    
    output$dataset_select_ui = renderUI({
      #dataset_list = values$dataset_list
      if(!is.null(input$upload_dataset)){
        if(input$upload_dataset == '_'){
          values$dataset_list = list()
        }else{
          if(is.null(names(values$dataset_list))){
            withProgress(message = 'Uploading Dataset', {
              values$dataset_list = tryCatch(readRDS(dataset_list_path()), error=function(e) list())
            })
          }else{
            print(names(values$dataset_list))
            
            if(values$dataset_list$input$experiment_code != input$upload_dataset){
              withProgress(message = 'Uploading Dataset', {
                values$dataset_list = tryCatch(readRDS(dataset_list_path()), error=function(e) list())
              })
            }
          }
          values$upload_list$full_path = values$dataset_list$full_path
          values$upload_list$original_data = values$dataset_list$data$original_data
          
          dataset_list = values$dataset_list
        }
        
        #names(dataset_list)
        selectInput('dataset_select','Select Dataset',names(values$dataset_list$data),process_values$dataset_select)
        }
    })
    
    # dataset_list = reactive({
    #   print('dataset_list')
    #   if(input$upload_dataset == '_'){
    #     dataset_list = list()
    #   }else{
    #     print(paste("readRDS : ",dataset_list_path()))
    #     dataset_list = tryCatch(readRDS(dataset_list_path()), error=function(e) list())
    #   }
    #   print(names(dataset_list))
    #   print('   data_set_list : done')
    #   #saveRDS(dataset_list,'temp/dataset_list.rds')
    #   #dataset_list = readRDS('temp/dataset_list.rds')
    #   values$dataset_list = dataset_list
    #   dataset_list
    # })
    
    output$dataset_table = renderDataTable({
      if(input$show_dataset == T){
        if(!is.null(input$dataset_select)){
          df = values$dataset_list$data[[input$dataset_select]]
          process_values$dataset_select = input$dataset_select
          if(!is.null(dim(df))){
            df
          }else{
            df = data.frame(NULL)
          }
        }else{
          df = data.frame(NULL)
        }
      }
      
    })
    

    
    dataset_list_path = reactive({
      paste0("data/data_list/",input$upload_dataset,"_data_list.rds")
      })
    # save_dataset_list_path = reactive({
    #   #dataset_list = values$dataset_list
    #   paste0('data/data_list/',values$dataset_list[['experiment_code']],'_data_list.rds')
    # })
    

    
  ### _upload ####
    
    output$reload_file_ui = renderUI({
      if(input$upload_dataset == '_'){
        actionButton('load_file','Load New File')
      }
    }) # load file action button
    
    output$show_upload_table_ui = renderUI({
      if(!is.null(values$upload_list$original_data)){
        radioButtons("show_table", "Show Uploaded Data Table",c(F,T),inline = T)
      }
    })

    observeEvent(input$load_file,{
      path = paste(file.choose())
      path
      path = gsub(paste0(getwd(),'/'),'',path)
      print(path)
      values$upload_list = list()
      values$dataset_list = list()
      values$upload_list[['full_path']] = path
      withProgress(message = 'read.csv', {
        df <- read.csv(values$upload_list[['full_path']],
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      })
      print(dim(df))
      values$upload_list[['original_data']] = df
    }) # get full path from the load fill action button
    
    output$file_path_ui = renderUI({
      if(!is.null(values$upload_list[['full_path']])){
        textInput("my_file_path", label = "Full path to my file", values$upload_list[['full_path']], width = 1200)
      }
    })
    
    output$file_header_ui = renderUI({
        if(is.null(values$dataset_list[['input']])){
          selected = T
        }else{
          if(is.null(values$dataset_list[['input']][['header']])){
            selected = 'Other'
          }else{
            selected = values$dataset_list[['input']][['header']]
          }
        }
      checkboxInput("header", "Header", TRUE)
      #}
    })
    output$file_sep_ui = renderUI({

      if(is.null(values$dataset_list[['input']])){
        selected = "\t"
      }else{
        if(is.null(values$dataset_list[['input']][['sep']])){
          selected = "\t"
        }else{
          selected = values$dataset_list[['input']][['sep']]
        }
      }
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected,inline = T)
      #}
    })
    output$file_quote_ui = renderUI({

      if(is.null(values$dataset_list[['input']])){
        selected = ""
      }else{
        if(is.null(values$dataset_list[['input']][['quote']])){
          selected = ""
        }else{
          selected = values$dataset_list[['input']][['quote']]
        }
      }
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected,inline = T)
      #}
    })
    
    #shinyFileChoose(input, 'upload_file', roots=c(wd='.'))
    #upload_file <- reactive(input$upload_file)
    
    original_data = reactive({
      if(is.null(values$upload_list$original_data)){
        df = data.frame(NULL)
      # if(input$upload_dataset == '_'){
      #   if(!is.null(values$dataset_list[['full_path']])){
      #     withProgress(message = 'read.csv', {
      #       df <- read.csv(values$dataset_list[['full_path']],
      #                      header = input$header,
      #                      sep = input$sep,
      #                      quote = input$quote)
      #     })
      #     print(dim(df))
      #     values$dataset_list[['original_data']] = df
      #     #values$dataset_list[['input']] = list()
      #     df
      #   }
      }else{
        df = values$upload_list[['original_data']]
      }
      df
    })
    
    output$original_data = renderDataTable({
      if(!is.null(input$show_table)){
        df = original_data()
        if(input$show_table == F){
          df = data.frame(NULL)
        }
        df
      }
    })
      
    
    output$original_data_detail_text = renderText({
      if(!is.null(values$upload_list[['original_data']])){
        paste0('Rows : ',dim(values$upload_list[['original_data']])[1],
               ' Columns : ',dim(values$upload_list[['original_data']])[2])
      }
    })
    
    
    
    

    output$upload_data_origin_ui = renderUI({
      if(!is.null(values$upload_list[['original_data']])){
        if(is.null(values$dataset_list[['input']])){
          selected = ''
        }else{
          if(is.null(values$dataset_list[['input']][['data_origin']])){
            selected = 'Other'
          }else{
            selected = values$dataset_list[['input']][['data_origin']]
          }
        }
        radioButtons('data_origin','Data Origin',c('Transcriptome','Proteome','Other'),selected,inline = T)
      }
      
  }) # data type
    
    output$upload_data_type_ui = renderUI({
      if(!is.null(values$upload_list[['original_data']])& !is.null(input$data_origin)){
        if(is.null(values$dataset_list[['input']])){
          selected = ''
        }else{
          if(is.null(values$dataset_list[['input']][['data_type']])){
            selected = ''
          }else{
            selected = values$dataset_list[['input']][['data_type']]
          }
        }
        radioButtons('data_type','Data Type',c('Expression','Ratio','Timecourse'),selected,inline = T)
      }
      
    })
    
    output$proteome_type_ui = renderUI({
      if(!is.null(values$upload_list[['original_data']]) & !is.null(input$data_type)){
        if(!is.null(input$data_origin)){
          if(input$data_origin == 'Proteome'){
  
            if(is.null(values$dataset_list[['input']][['proteome']][['type']])){
              selected = ''
            }else{
              selected = values$dataset_list[['input']][['proteome']][['type']]
            }
    
          radioButtons('proteome_type','Proteome Data Type',c('Targetted','Discovery' ),selected,inline = T)
          }
        }
      }
      
    })
    
    output$maxquant_ui = renderUI({
      if(!is.null(values$upload_list[['original_data']])){
        if(!is.null(input$proteome_type)){
          if(input$proteome_type == 'Discovery'){
            if(is.null(values$dataset_list[['input']][['proteome']][['maxquant']])){
              selected = ''
            }else{
              selected = values$dataset_list[['input']][['proteome']][['maxquant']]
            }
            radioButtons('maxquant','MaxQuant',c(F,T),selected,inline = T)
          }
        }
      }
      
    })
    
    output$proteome_label_ui = renderUI({
      if(!is.null(values$upload_list[['original_data']])){
        if(!is.null(input$proteome_type)){
          if(input$proteome_type == 'Discovery'){
            if(is.null(values$dataset_list[['input']][['proteome']][['proteome_label']])){
              selected = ''
            }else{
              selected = values$dataset_list[['input']][['proteome']][['proteome_label']]
            }
            radioButtons('proteome_label','Labelled',c("LFQ",'SILAC','iTRAQ','TMT'),selected,inline = T)
          }
        }
      }
      
    })
    
    
    
    

    output$experiment_name_ui = renderUI({
      
      if(!is.null(values$upload_list[['original_data']]) & !is.null(input$data_type)){
        if(is.null(values$dataset_list[['input']][['experiment_name']])){
          selected = ''
        }else{
          selected = values$dataset_list[['input']][['experiment_name']]
        }
        
        
        textInput('experiment_name','Experiment_Name',selected)
        
      }
    })
    
    output$experiment_code_ui = renderUI({
      if(!is.null(values$upload_list[['original_data']]) & !is.null(input$data_type)){
        if(is.null(values$dataset_list[['input']][['experiment_code']])){
          selected = ''
        }else{
          selected = values$dataset_list[['input']][['experiment_code']]
        }
        
        textInput('experiment_code','Experiment Code',selected)
        
      }
    })
    
    output$experiment_description_ui = renderUI({
      if(!is.null(values$upload_list[['original_data']])  & !is.null(input$data_type)){
        if(is.null(values$dataset_list[['input']][['experiment_description']])){
          selected = ''
        }else{
          selected = values$dataset_list[['input']][['experiment_description']]
        }
        textInput('experiment_description','Experiment Description',selected,width = 1200)
        
      }
    })
    
    
    
    #### __ columns ####
    output$id_column_1_ui = renderUI({
      #selectInput('id_column','ID columns',colnames(input_df()))
      if(!is.null(values$upload_list[['original_data']])  & !is.null(input$experiment_code)){
         if(input$experiment_code != ''){
          if(is.null(values$dataset_list[['input']][['id_column']][['1']])){
            selected = colnames(values$upload_list[['original_data']])[1]
          }else{
            selected = values$dataset_list[['input']][['id_column']][['1']]
          }
          selectInput('id_column_1','Column ID 1',c(colnames(values$upload_list[['original_data']])),selected)
         }
      }
    })
    
    output$id_column_2_ui = renderUI({
      if(!is.null(values$upload_list[['original_data']]) & !is.null(input$id_column_1)){
        if(input$id_column_1 != ''){
          if(is.null(values$dataset_list[['input']][['id_column']][['2']])){
            selected = ''
          }else{
            selected = values$dataset_list[['input']][['id_column']][['2']]
          }
          selectInput('id_column_2','Column ID 2',c('',colnames(values$upload_list[['original_data']])),selected)
        }
      }
    })
    
    output$id_column_3_ui = renderUI({
      if(!is.null(values$upload_list[['original_data']])  & !is.null(input$id_column_2)){
         if(input$id_column_2 != ''){
          if(is.null(values$dataset_list[['input']][['id_column']][['3']])){
            selected = ''
          }else{
            selected = values$dataset_list[['input']][['id_column']][['3']]
          }
          selectInput('id_column_3','Column ID 3',c('',colnames(values$upload_list[['original_data']])),selected)
         }
      }
    })
    
    output$id_column_4_ui = renderUI({
      if(!is.null(values$upload_list[['original_data']])  & !is.null(input$id_column_3)){
        if(input$id_column_3 != ''){
          if(is.null(values$dataset_list[['input']][['id_column']][['4']])){
            selected = ''
          }else{
            selected = values$dataset_list[['input']][['id_column']][['4']]
          }
          selectInput('id_column_4','Column ID 4',c('',colnames(values$upload_list[['original_data']])),selected)
        }
      }
    })
    
    output$col1_len = renderText({
      if(!is.null(values$upload_list[['original_data']]) & !is.null(input$id_column_1)){
        if(input$id_column_1 != ''){
        length(unique(values$upload_list[['original_data']][,input$id_column_1]))
        }
      }
    })
    output$col2_len = renderText({
      if(!is.null(values$upload_list[['original_data']]) & !is.null(input$id_column_2)){
        if(input$id_column_2 != ''){
          length(unique(values$upload_list[['original_data']][,input$id_column_2]))
        }
      }
    })
    output$col3_len = renderText({
      if(!is.null(values$upload_list[['original_data']]) & !is.null(input$id_column_3)){
        if(input$id_column_3 != ''){
          length(unique(values$upload_list[['original_data']][,input$id_column_3]))
        }
      }
    })
    output$col4_len = renderText({
      if(!is.null(values$upload_list[['original_data']]) & !is.null(input$id_column_4)){
        if(input$id_column_4 != ''){
          length(unique(values$upload_list[['original_data']][,input$id_column_4]))
        }
      }
    })

  
    
    
    output$condition_1_name_ui = renderUI({
      if(!is.null(values$upload_list[['original_data']])  & !is.null(input$id_column_1)){
        if(is.null(values$dataset_list[['input']][['condition']][['1']][['name']])){
          selected = ""
        }else{
          selected = values$dataset_list[['input']][['condition']][['1']][['name']]
        }
        textInput('condition_1_name','Condition 1 name',selected)
      }
    })
    output$condition_2_name_ui = renderUI({
      if(!is.null(values$upload_list[['original_data']]) & !is.null(input$condition_1_name)){
        if(input$condition_1_name != ''){
          if(is.null(values$dataset_list[['input']][['condition']][['2']][['name']])){
            selected = ""
          }else{
            selected = values$dataset_list[['input']][['condition']][['2']][['name']]
          }
          textInput('condition_2_name','Condition 2 name',selected)
        }
      }
    })
    output$condition_3_name_ui = renderUI({
      if(!is.null(values$upload_list[['original_data']]) & !is.null(input$condition_2_name)){
        if(input$condition_2_name != ''){
          if(is.null(values$dataset_list[['input']][['condition']][['3']][['name']])){
            selected = ""
          }else{
            selected = values$dataset_list[['input']][['condition']][['3']][['name']]
          }
          textInput('condition_3_name','Condition 3 name',selected)
        }
      }
    })
    output$condition_4_name_ui = renderUI({
      if(!is.null(values$upload_list[['original_data']]) & !is.null(input$condition_3_name)){
        if(input$condition_3_name != ''){
          if(is.null(values$dataset_list[['input']][['condition']][['3']][['name']])){
            selected = ""
          }else{
            selected = values$dataset_list[['input']][['condition']][['4']][['name']]
          }
          textInput('condition_4_name','Condition 4 name',selected)
        }
      }
    })
    
    
    output$condition_1_columns_ui = renderUI({
      if(!is.null(values$upload_list[['original_data']]) & !is.null(input$condition_1_name)){
        if(input$condition_1_name != ''){
          if(is.null(values$dataset_list[['input']][['condition']][['1']][['columns']])){
            selected = c()
          }else{
            selected = values$dataset_list[['input']][['condition']][['1']][['columns']]
          }
          selectInput('condition_1','Condition 1 Columns',colnames(values$upload_list[['original_data']]),selected,multiple =T )
          
        }
      }
    })
    
    output$condition_2_columns_ui = renderUI({
      if(!is.null(values$upload_list[['original_data']]) & !is.null(input$condition_2_name)){
        if(input$condition_2_name != ''){
          if(is.null(values$dataset_list[['input']][['condition']][['2']][['columns']])){
            selected = c()
          }else{
            selected = values$dataset_list[['input']][['condition']][['2']][['columns']]
          }
          selectInput('condition_2','Condition 2 Columns',colnames(values$upload_list[['original_data']]),selected,multiple =T )
          
        }
      }
    })
    
    output$condition_3_columns_ui = renderUI({
      if(!is.null(values$upload_list[['original_data']]) & !is.null(input$condition_3_name)){
        if(input$condition_3_name != ''){
          if(is.null(values$dataset_list[['input']][['condition']][['3']][['columns']])){
            selected = c()
          }else{
            selected = values$dataset_list[['input']][['condition']][['3']][['columns']]
          }
          selectInput('condition_3','Condition 3 Columns',colnames(values$upload_list[['original_data']]),selected,multiple =T )
          
        }
      }
    })
    
    output$condition_4_columns_ui = renderUI({
      if(!is.null(values$upload_list[['original_data']]) & !is.null(input$condition_4_name)){
        if(input$condition_4_name != ''){
          if(is.null(values$dataset_list[['input']][['condition']][['4']][['columns']])){
            selected = c()
          }else{
            selected = values$dataset_list[['input']][['condition']][['4']][['columns']]
          }
          selectInput('condition_4','Condition 4 Columns',colnames(values$upload_list[['original_data']]),selected,multiple =T )
          
        }
      }
    })
    
    ##### _SAVE DATASET ####
    
    
    save_dataset = function(values){
      rds_path = values$dataset_list[['rds_path']]
      print(paste('saveRDS : ',rds_path))
      
      #future({
        withProgress(message = 'Saving', {
          #print(paste0('Saving : ',rds_path)
          #print(names(values$dataset_list))
          
          print(paste('saveRDS : ',rds_path))
          saveRDS(values$dataset_list,rds_path)
        })
        print('done save_dataset')
      #})
    }
    observeEvent(input$save_dataset,{
      rds_path = values$dataset_list[['rds_path']]
      future({
        withProgress(message = 'Saving', {
          #print(paste0('Saving : ',rds_path)
          print(names(values$dataset_list))
          
          print(paste('saveRDS : ',rds_path))
          saveRDS(values$dataset_list,rds_path)
        })
        print('done save_dataset')
      })
    })
    
    ### ___save upload ####
    
    output$save_output_ui = renderUI({
      if(!is.null(input$condition_1)){
        actionButton("save_upload", "Save")
      }
    })
    
    observeEvent(input$save_upload,{
      #values$dataset_list = list()
      values$dataset_list[['full_path']] = values$upload_list$full_path
      values$dataset_list$data = list()
      values$dataset_list$data[['original_data']] = values$upload_list$original_data
      values$dataset_list[['input']] = list()
      values$dataset_list[['input']][['header']] = input$header
      values$dataset_list[['input']][['sep']] = input$sep
      values$dataset_list[['input']][['quote']] = input$quote
      
      values$dataset_list[['input']][['data_origin']] = input$data_origin
      values$dataset_list[['input']][['data_type']] = input$data_type
      
      if(input$data_origin == 'Proteome'){
        values$dataset_list$input$proteome = list()
        values$dataset_list[['input']][['proteome']][['type']] = input$proteome_type
        if(input$proteome_type == 'Discover'){
          values$dataset_list[['input']][['proteome']][['maxquant']] = input$maxquant
          if(input$maxquant == T){
            values$dataset_list[['input']][['proteome']][['maxquant_type']] = input$maxquant_type
          }
        }
      }
      
      values$dataset_list[['input']][['experiment_name']] = input$experiment_name
      values$dataset_list[['input']][['experiment_code']] = input$experiment_code
      values$dataset_list[['input']][['experiment_description']] = input$experiment_description
      #str(values$dataset_list[['input']])
      
      values$dataset_list[['input']][['id_column']] = list()
      
      values$dataset_list[['input']][['id_column']][['1']] = input$id_column_1
      if(!is.null(input$id_column_2)){
        if(input$id_column_2 != ''){
          values$dataset_list[['input']][['id_column']][['2']] = input$id_column_2
        }
      }
      if(!is.null(input$id_column_3)){
        if(input$id_column_3 != ''){
          values$dataset_list[['input']][['id_column']][['3']] = input$id_column_3
        }
      }
      if(!is.null(input$id_column_4)){
        if(input$id_column_4 != ''){
          values$dataset_list[['input']][['id_column']][['4']] = input$id_column_4
        }
      }
      
      
      values$dataset_list[['input']][['condition']] = list()
      values$dataset_list[['input']][['condition']][['1']] = list()
      values$dataset_list[['input']][['condition']][['1']][['name']] = input$condition_1_name
      values$dataset_list[['input']][['condition']][['1']][['columns']] = input$condition_1
      values$dataset_list[['input']][['condition_columns']][[input$condition_1_name]] = input$condition_1
      
      
      if(!is.null(input$condition_2_name)){
        if(input$condition_2_name != ''){
          values$dataset_list[['input']][['condition']][['2']] = list()
          
          values$dataset_list[['input']][['condition']][['2']][['name']] = input$condition_2_name
          values$dataset_list[['input']][['condition']][['2']][['columns']] = input$condition_2
          values$dataset_list[['input']][['condition_columns']][[input$condition_2_name]] = input$condition_2
          
        }
      }
      if(!is.null(input$condition_3_name)){
        if(input$condition_3_name != ''){
          values$dataset_list[['input']][['condition']][['3']] = list()
          
          values$dataset_list[['input']][['condition']][['3']][['name']] = input$condition_3_name
          values$dataset_list[['input']][['condition']][['3']][['columns']] = input$condition_3
          values$dataset_list[['input']][['condition_columns']][[input$condition_3_name]] = input$condition_3
          
        }
      }
      if(!is.null(input$condition_4_name)){
        if(input$condition_4_name != ''){
          values$dataset_list[['input']][['condition']][['4']] = list()
          
          values$dataset_list[['input']][['condition']][['4']][['name']] = input$condition_4_name
          values$dataset_list[['input']][['condition']][['4']][['columns']] = input$condition_4
          values$dataset_list[['input']][['condition_columns']][[input$condition_4_name]] = input$condition_4
          
        }
      }
      

      
      id_columns = paste(values$dataset_list$input$id_column)
      id_columns
      expression_columns = c()
      x = '2'
      expression_columns = paste(unlist(sapply(names(values$dataset_list$input$condition), 
                                        function(x) sapply(values$dataset_list$input$condition[[x]][['columns']], 
                                                           function(y)c(expression_columns,y)))))
      expression_columns
      #sapply(expression_columns, function(x) as.character(x))
      #paste(expression_columns)
      values$dataset_list$input$id_columns = id_columns
      values$dataset_list$input$expression_columns = expression_columns
      
      colnames(values$dataset_list$data$original_data)
      expression_data = values$dataset_list$data$original_data[,c(id_columns,expression_columns)] %>% 
        mutate(row_id = as.factor(values$dataset_list$data$original_data[,1])) %>% 
        dplyr::select(row_id, everything())
      expression_data %>%  as.tbl
      
      values$dataset_list$data$expression_data = expression_data
      
      
      dim(values$dataset_list$data$expression_data)
      rds_path = paste0('data/data_list/',input$experiment_code,'_data_list.rds')
      values$dataset_list[['rds_path']] = rds_path
      print(paste('saveRDS :',rds_path))
      
      withProgress(message = 'saveRDS', {
        saveRDS(values$dataset_list,rds_path)
      })
      
      values$upload_datasets = unique(c(values$upload_datasets,input$experiment_code))
      values$upload_dataset_selected = input$experiment_code
      
    
      })

    ####_ID MAPPING ####
    
    ###_ select id ####
    output$select_id_ui = renderUI({
      print('select_mart_id')
      if(is.null(values$dataset_list[['id_mapping']])){
        values$dataset_list[['id_mapping']] = list()
      }
      if(is.null(values$dataset_list[['id_mapping']][['select_id']])){
        selected = colnames(values$dataset_list$data[['expression_data']][1])
      }else{
        selected = values$dataset_list[['id_mapping']][['select_id']]
      }
      selectInput('select_id',
                  'Select new id column',
                  colnames(values$dataset_list$data[['expression_data']]),
                  selected)
    })
    output$select_id_df = renderDataTable({
      print('select_id_df')
      if(!is.null(input$select_id)){
        print('   running ...')
        values$dataset_list[['id_mapping']][['select_id']] = input$select_id
        
        id_mapping_w = values$dataset_list$data[['expression_data']]
        
        
        
        id_mapping_w %>%  as_tibble()
        
        var <- rlang::parse_quosures(paste(input$select_id))[[1]]
        var
        df = id_mapping_w %>% 
          mutate(id = !!var) %>% 
          dplyr::select(id,everything())
        
        
        
        
        
        df %>% as_tibble
        
        
        values$dataset_list$data[['expression_data']] = df
        
        
        if(autosave_datasets == T){
          rds_path = values$dataset_list[['rds_path']]
          print(paste('saveRDS : ',rds_path))
          saveRDS(values$dataset_list,rds_path)
        }
        
        print('   done : mart_experssion_df')
        
        percentage_matched = length(df$id[!is.na(df$id) & df$id != ''])/length(df$id) * 100
        percentage_matched
        
        output$mart_percentage_matched = renderText({
          print(paste0('Percentage of ids matched :',signif(percentage_matched,3),'%'))
        })
        
        df
      }
      
    })
    
    
    ###__biomart ####
    output$select_mart_host_ui = renderUI({
      if(is.null(values$dataset_list[['biomaRt']][['listEnsemblArchives_select']])){
        withProgress(message = 'Calculation biomaRt::listEnsemblArchives', {
          archives = biomaRt::listEnsemblArchives()
          archive_list = archives$url
          names(archive_list) = paste(archives$name,':',archives$date)
          archive_list
          values$dataset_list[['biomaRt']][['listEnsemblArchives']] = archive_list
        })
        selected = values$dataset_list[['biomaRt']][['listEnsemblArchives']][1]
      }else{
        selected = values$dataset_list[['biomaRt']][['listEnsemblArchives_select']]
      }
      #selectInput('selectMart','Select Mart',biomaRt::listMarts()$biomart,biomaRt::listMarts()$biomart[1])
      selectInput('select_Mart_Archive',
                  'Select Mart Archive',
                  values$dataset_list[['biomaRt']][['listEnsemblArchives']],
                  selected)
      
    })
    
    output$select_mart_ui = renderUI({
      if(!is.null(input$select_Mart_Archive)){
        values$dataset_list[['biomaRt']][['listEnsemblArchives_select']] = input$select_Mart_Archive
        
        if(is.null(values$dataset_list[['biomaRt']][['listMarts_select']])){
          withProgress(message = 'Calculation biomaRt::listMarts', {
            values$dataset_list[['biomaRt']][['listMarts']] = biomaRt::listMarts(host = input$select_Mart_Archive)$biomart
          })
          selected = values$dataset_list[['biomaRt']][['listMarts']][1]
        }else{
          if(values$dataset_list[['biomaRt']][['listEnsemblArchives_select']] != input$select_Mart_Archive){
            withProgress(message = 'Calculation biomaRt::listMarts', {
              values$dataset_list[['biomaRt']][['listMarts']] = biomaRt::listMarts(host = input$select_Mart_Archive)$biomart
              selected = values$dataset_list[['biomaRt']][['listMarts']][1]
              
              values$dataset_list[['biomaRt']][['listEnsemblArchives_select']] = input$select_Mart_Archive
            })
          }else{
            selected = values$dataset_list[['biomaRt']][['listMarts_select']]
          }
        }
        #selectInput('selectMart','Select Mart',biomaRt::listMarts()$biomart,biomaRt::listMarts()$biomart[1])
        selectInput('selectMart',
                    'Select Mart',
                    values$dataset_list[['biomaRt']][['listMarts']],
                    selected)
      }
      
    })
    
    output$list_mart_ui = renderUI({
      if(!is.null(input$selectMart)){
        run = 1
        withProgress(message = 'Calculation biomaRt::listDataset', {
          if(!is.null(values$dataset_list[['biomaRt']][['listEnsemblArchives_select']]) &
             !is.null(values$dataset_list[['biomaRt']][['listMarts_select']]) &
             !is.null(values$dataset_list[['biomaRt']][['listDatasets']])){
            if(values$dataset_list[['biomaRt']][['listEnsemblArchives_select']] == input$select_Mart_Archive &
               values$dataset_list[['biomaRt']][['listMarts_select']] == input$selectMart
            ){
              run = 0
            }
          }
          if(run == 1){
            values$dataset_list[['biomaRt']][['listMarts_select']] != input$selectMart
            ensembl = biomaRt::useMart(biomart = input$selectMart, host = input$select_Mart_Archive)
            datasets = biomaRt::listDatasets(ensembl)
            datasets
            mart_list = datasets$dataset
            names(mart_list) = datasets$description
            mart_list
            values$dataset_list[['biomaRt']][['listDatasets']] = mart_list
          }
          if(is.null(values$dataset_list[['biomaRt']][['listDatasets_select']])){
            selected  = "hsapiens_gene_ensembl"
          }else{
            selected = values$dataset_list[['biomaRt']][['listDatasets_select']]
          }
          selectInput('mart_list_datasets','Select Mart Dataset',values$dataset_list[['biomaRt']][['listDatasets']],selected)
        })
      }
      
    })
    
    ensembl = reactive({
      ensembl = NULL
      if(!is.null(input$mart_list_datasets)){
        run = 1
        if(!is.null(values$dataset_list[['biomaRt']][['listDatasets_select']]) & !is.null(values$dataset_list[['biomaRt']][['listMarts_select']])){
          if(values$dataset_list[['biomaRt']][['listMarts_select']] == input$selectMart & 
             values$dataset_list[['biomaRt']][['listDatasets_select']] == input$mart_list_datasets){
            if(!is.null(values$dataset_list[['biomaRt']][['ensembl']])){
              run = 0
              ensembl = values$dataset_list[['biomaRt']][['ensembl']]
            }
          }
        }
        if(run == 1){
          values$dataset_list[['biomaRt']][['listDatasets_select']] = input$mart_list_datasets
          withProgress(message = 'Calculation biomaRt::useMart', {
            
            ensembl = biomaRt::useMart(input$selectMart,dataset=input$mart_list_datasets,host = input$select_Mart_Archive)
          })
        }
      }
      ensembl
    })
    
    output$filter_ui = renderUI({
      if(!is.null(ensembl())){
        run = 1
        ensembl = ensembl()
        values$dataset_list[['biomaRt']][['ensembl']] = ensembl
        
        filters = biomaRt::listFilters(ensembl)
        
        values$dataset_list[['biomaRt']][['listFilters']] = filters
        filters_list = filters$name
        names(filters_list) = filters$description
        filters_list
        
        if(is.null(values$dataset_list[['biomaRt']][['listFilters_select']])){
          selected = 'affy_hg_u133_plus_2'
        }else{
          selected = values$dataset_list[['biomaRt']][['listFilters_select']]
        }
        selectInput('mart_filters','Select Filters',filters_list,selected, width = 800)
      }
    })
    
    output$attributes_ui = renderUI({
      if(!is.null(ensembl())){
        attributes = biomaRt::listAttributes(ensembl())
        attributes
        attributes_list =  attributes$name
        attributes_list
        names(attributes_list) = attributes$description
        values$dataset_list[['biomaRt']][['listAttributes']] = attributes
        if(is.null(values$dataset_list[['biomaRt']][['listAttributes_select']])){
          selected = c('hgnc_symbol','wikigene_description')
        }else{
          selected = values$dataset_list[['biomaRt']][['listAttributes_select']]
        }
        selectInput('mart_attributes','Select Attributes',attributes_list,c('hgnc_symbol','wikigene_description'), multiple = T)
      }
    })
    
    output$mart_column_ui = renderUI({
      #if(!is.null(input$mart_attributes)){
        
        #if(is.null(values$dataset_list$id_mapping[['mart_column']])){
        #  selected = '_'
        #}else{
        #  selected = values$dataset_list$id_mapping[['mart_column']]
        #}
        selectInput('mart_column','Select id column',c('_', colnames(values$dataset_list$data$expression_data)),'_')
      #}
    })
    
    
    output$mart_slider = renderUI({
      df = values$dataset_list$data[["expression_data"]]
      sliderInput('mart_slider','Select IDs to test',min = 1,max = dim(df)[1],value = c(1,100), step = 1, width = 1200)
    })
    
    mart_df = reactive({
      #df = values$dataset_list[["expression_data"]][c(input$mart_slider[1],input$mart_slider[2]),]
      values$dataset_list[['biomaRt']][['listFilters_select']] = input$mart_filters
      values$dataset_list[['biomaRt']][['listAttributes_select']] = input$mart_attributes
      #values$dataset_list[['mart_column']] = input$mart_column
      if(input$run_biomart[1] > process_values$save_mart){
        df = values$dataset_list$data[["expression_data"]]
        process_values$save_mart = process_values$save_mart = 1
        process_values$save_mart_full = 1
        #process_values$save_mart_base = 0
        if(dataset_test == T){
          df = df[c(1:15),]
        }
      }else{
        df = values$dataset_list$data[["expression_data"]][c(input$mart_slider[1]:input$mart_slider[2]),]
        
      }
      df
    })
    
    
    
    
    output$bm_df = renderDataTable({
      values$dataset_list[['biomaRt']][['listFilters_select']] = input$mart_filters
      values$dataset_list[['biomaRt']][['listAttributes_select']] = input$mart_attributes
      
      if(!is.null(input$mart_column)){
        
        if(input$mart_column != '_'){
          
          print('id_mapping')
          
          print('   running ....')
          
          values$dataset_list[['id_mapping']][['mart_column']] = input$mart_column
          values$dataset_list[['biomaRt']][['id']] = input$mart_column
          
          #https://bioconductor.org/packages/devel/bioc/vignettes/biomaRt/inst/doc/biomaRt.html
          
          df = mart_df() %>% mutate(id = as.factor(id))
          df %>% as.tbl
          colnames(df)
          values$dataset_list$data$expression_data$ACCESSION
          
          mart_id = colnames(df)[4]
          mart_id = input$mart_column
          mart_id
          df %>%  as.tbl
          id = unlist(df[,mart_id])
          id
          if(input$mart_id_length == 0){
            if(input$mart_id_prefix != ''){
              id = paste0(input$mart_id_prefix,id)
            }
          }else{
            if(input$mart_id_prefix != ''){
              #x = id[1]
              #x
              #input$mart_id_length
              id = sapply(id, function(x) paste0(paste0(rep(input$mart_id_prefix,input$mart_id_length - nchar(as.character(x))),collapse = ''),x,collapse = ""))
            }
          }
          
          if(input$mart_id_split != ''){
            x = id[1]
            x
            sep = input$mart_id_split
            if(sep == '.'){
              sep = '\\.'
              
            }
            sep
            id = sapply(id, function(x) unlist(strsplit(as.character(x),sep))[1])
          }
          id
          df$bm_id = as.factor(id) 
          df %>% as.tbl
          id = unique(id)
          print(length(id))
          withProgress(message = 'BioMart Calculation in progress', {
            print('running getBM ....')
            bm_df = biomaRt::getBM(attributes=c(input$mart_attributes,input$mart_filters),
                                   filters = input$mart_filters,
                                   values = id, 
                                   mart = values$dataset_list[['biomaRt']][['ensembl']]
            )
            bm_df %>% as.tbl
          })
          test = F
          if(test == T){
            df = values$dataset_list$data$expression_data
            dim(df)
            id = df$id[1:50]
            id
            names(values$dataset_list$biomaRt)
            attributes = values$dataset_list$biomaRt$listAttributes
            attributes
            
            bm_attribute = input$mart_attributes
            bm_attribute
            filters = values$dataset_list$biomaRt$listFilters
            filters
            View(filters)
            bm_filter = input$mart_filters
            bm_filter
            filters$name
            bm_filter = filters[191,]$name
            
            bm_filter = 'entrezgene'
            bm_filter
            str(id)
            id = as.factor(id)
            id = paste0('ILMN_',id)
            id
            str(id)
            mart = values$dataset_list[['biomaRt']][['ensembl']]
            mart
            bm_df = biomaRt::getBM(attributes=c(bm_attribute,bm_filter),
                                   filters = bm_filter,
                                   values = id, 
                                   mart = mart
            )
            bm_df
          }
          
          if(dim(bm_df)[1] > 0){
            filter <- rlang::parse_quosures(paste(input$mart_filters))[[1]]
            filter
            id_column = rlang::parse_quosures(paste(input$mart_column))[[1]]
            id_column
            bm_cols = colnames(bm_df)
            bm_cols
            #bm_df$bm_id = id
            bm_df_collapse = bm_df %>% 
              mutate(bm_id = as.factor(!!filter)) %>% 
              na_if(., "") %>% 
              group_by(bm_id) %>% 
              mutate_at(.funs = funs(paste(unique(na.omit(.)),collapse = ', ')), .vars = bm_cols) %>% 
              ungroup() %>% 
              filter(!duplicated(bm_id))
            bm_df_collapse
            if(input$mart_id_prefix != '' & input$mart_id_length > 0){
              #x = bm_df_collapse$bm_id[1]
              #nchar(x)
              bm_df_collapse$bm_id = sapply(bm_df_collapse$bm_id, function(x) paste0(paste0(rep(input$mart_id_prefix,input$mart_id_length - nchar(as.character(x))),collapse = ''),x,collapse = ""))
            }
            
            bm_df_collapse
            id_mapping_w = full_join(
              
              bm_df_collapse,
              df %>% 
                dplyr::select(-one_of(colnames(bm_df)[colnames(bm_df) != 'bm_id'])), #%>%
                #mutate(bm_id = df$bm_id),
                #mutate(bm_id = as.factor(!!id_column)), 
              by = 'bm_id') 
            id_mapping_w
            #df %>% as.tbl
            if(process_values$save_mart_full == 1){
              
              values$dataset_list$data[['expression_data']] = id_mapping_w
              
              
              if(autosave_datasets == T){
                
                rds_path = values$dataset_list[['rds_path']]
                print(paste('saveRDS : ',rds_path))
                saveRDS(values$dataset_list,rds_path)
              }
              process_values$save_mart_full = 0
            }
          
          
          print('   done : id mapping')
          id_mapping_w
          }else{
             df$bm_id = id 
             df %>% 
              dplyr::select(bm_id, everything())
          }
          
          }
          }
      
    })
    
    ###__separate_column_id ####
    output$sep_id_ui = renderUI({
      print('select_sep_id')
      print('select_mart_id')
      if(is.null(values$dataset_list[['id_mapping']][['sep_id']])){
        selected = 'id'
      }else{
        selected = values$dataset_list[['id_mapping']][['sep_id']]
      }
      selectInput('sep_id',
                  'Select id column to separate',
                  colnames(values$dataset_list$data$expression_data),
                  selected)
    })
    
    output$separate_columns = renderDataTable({
      print('separate_columns')
      df = values$dataset_list$data[['expression_data']]

      if(!is.null(input$sep_id)){
        if(input$col_sep != ''){
          
          print('   running ...')
          
          #input$dataset_select
          values$dataset_list[['id_mapping']][['sep_id']] = input$sep_id
          
          id_mapping_w = values$dataset_list$data[["expression_data"]]
          
          id_mapping_w %>% as.tbl
          
          var <- rlang::parse_quosures(paste(input$sep_id))[[1]]
          var
          df = id_mapping_w %>% 
            mutate(id = !!var) %>% 
            dplyr::select(id,everything())
          
          print('     id sep wide')
          withProgress(message = 'Separating Column', {
            df$id_list = sapply(df$id, function(x) trimws(unlist(strsplit(as.character(x), paste(input$col_sep)))))
          })
          df$id_list[1:5]
          df$id_list[[3]][1]
          df$id_list_first = sapply(df$id_list, function(x) x[1])
          df$id_list_first
          #View(df)
          
          #df$id_1 = sapply(df$id, function(x) trimws(unlist(strsplit(as.character(x), paste(input$col_sep)))[1]))
          df = df %>% dplyr::select(id_list_first,everything()) %>% 
            dplyr::select(-id_list)
          
          
          df %>% as.tbl
          
          
          values$dataset_list$data[['expression_data']] = df
          
          if(autosave_datasets == T){
            rds_path = values$dataset_list[['rds_path']]
            print(paste('saveRDS : ',rds_path))
            saveRDS(dataset_list,rds_path)
          }
          
        }
        
        #values$dataset_list[['expression_data']]
      }
      df
      
    })
    
    

    
  #####_Sample Numbers #####
    output$sample_numbers = renderPlot({
      dataset_list = readRDS(paste0("data/data_list/",input$upload_dataset,"_data_list.rds"))
      #dataset_list = values$dataset_list
      df = dataset_list$data[['expression_data']]
      head(df)
      dim(df)
      df_l = melt(df)
      head(df_l)
      df_l = df_l[df_l$value != 0,]
      df_l = df_l[!is.na(df_l$value),]
      ggplot(df_l) + 
        # geom_count(aes(y = value, x= variable)) + 
        geom_bar(aes(variable, fill = variable), stat = 'count') +
        coord_flip() +
        xlab('Sample Name') +
        theme(legend.position = 'None')
      
    })
    
    
    ## _Ratios ##### 
    
    output$expression_data = renderDataTable({
      print('expression_data')
   
        if(input$run_ratios[1] > process_values$run_ratios){
          print(input$run_ratios[1])
          print(process_values$run_ratios)
          process_values$run_ratios = process_values$run_ratios + 1
          print(input$run_ratios[1])
          print(process_values$run_ratios)
          
          withProgress(message = 'Calculating Ratios', {
          values$dataset_list$ratio = list() 
          #values$dataset_list$data = list()
          #values$dataset_list[['expression_data']]$row_id = values$dataset_list[['expression_data']]$Probe.Set.ID
          
          #values$dataset_list$data$original_data = values$dataset_list[['original_data']]
          #values$dataset_list$data$expression_data = values$dataset_list[['expression_data']]
          
          df = values$dataset_list$data[['expression_data']]
          #save_dataset(values)
          #colnames(df)
          names(values$dataset_list$input)
          df_l = gather(df,key = sample_name, values = values$dataset_list$input$expression_columns) %>% 
            mutate(data_type = 'expression') %>% 
            mutate(sample = NA)
          #df_l
          colnames(df_l)
          
          #names(values$dataset_list$input$condition)
          
          for(condition in names(values$dataset_list$input$condition)){
            #print(values$dataset_list$input$condition[[condition]][['name']])
            #print(values$dataset_list$input$condition[[condition]][['columns']])
            df_l$sample[df_l$sample_name %in% values$dataset_list$input$condition[[condition]][['columns']]] = 
              values$dataset_list$input$condition[[condition]][['name']]
          }
          
          #df_l %>%  as.tbl
          #View(df_l)
          
          #df_l = dataset_list[['expression_data_id_l']] %>% as_tibble
          #df_l
          df_ratio_l = df_l
          #colnames(df_l)
          unique(df_l$data_type)
          #pg = dataset_list['proteinGroups']
          #View(pg)
          #dim(df)
          #names = names(dataset_list)
          #names
          #condition_all = grep('condition',names,value = T)
          #condition_columns = grep('columns',condition_all,value = T)
          #condition_columns
          #condition = condition_columns[1]
          #condition
          for(condition in names(values$dataset_list$input$condition)){
            print(condition)
            condition_name = values$dataset_list$input$condition[[condition]][['name']]
            condition_name
            condition_columns = values$dataset_list$input$condition[[condition]][['columns']]
            condition_columns
            rownames(df) = df$row_id
            rep_df = rep_ratio_function(as.data.frame(df),condition_columns)
            rep_df
            head(rep_df)
            
            #condition_list = dataset_list[[condition]]
            #print(condition_list)
            #col_names = condition_list
            #col_names
            #rownames(df) = df$row_id
            #rep_df = rep_ratio_function(as.data.frame(df),col_names)
            #re
            rep_colnames = colnames(rep_df)
            rep_df[apply(rep_df, 2 , function(x) !is.finite(x))] = NA
            colnames(rep_df)
            rep_df_l <- rep_df %>% as_tibble() %>% 
              mutate(row_id = rownames(rep_df)) %>% 
              gather(., key = sample_name, values = rep_colnames) %>% 
              #rename(sample_name = key) %>% 
              right_join(., dplyr::select(df_l, -c('value','sample','sample_name','data_type')), by = 'row_id') %>% 
              mutate(sample = condition_name) %>% 
              mutate(data_type = 'replicate_ratio') %>% 
              #as_tibble() %>% 
              dplyr::select(colnames(df_l))
            
            #df_l
            rep_df_l
            #View(rep_df_l)
            df_ratio_l = rbind(df_ratio_l,rep_df_l)
            colnames(df_l)
            colnames(rep_df_l)
            
            #dataset_list[[gsub('columns','rep_ratio',condition)]] = rep_df
            #df = cbind(df,rep_df)
          }
          df_ratio_l %>% filter(data_type == 'replicate_ratio')
          unique(df_ratio_l$data_type)
          df
          paired_ratio_df =  paired_ratio_function(as.data.frame(df),values$dataset_list$input$condition[['2']][['columns']],values$dataset_list$input$condition[['1']][['columns']])
          paired_ratio_df
          paired_ratio_df[apply(paired_ratio_df, 2 , function(x) !is.finite(x))] = NA
          head(paired_ratio_df)
          ratio_df_l <- paired_ratio_df %>% 
            as_tibble() %>% 
            mutate(row_id = rownames(paired_ratio_df)) %>% 
            gather(., key = sample_name, values = colnames(paired_ratio_df)) %>% 
            #rename(sample_name = key) %>% 
            right_join(., dplyr::select(df_l, -c('value','sample','sample_name','data_type')), by = 'row_id') %>% 
            mutate(sample = paste0(values$dataset_list$input$condition[['2']][['name']],' / ',values$dataset_list$input$condition[['1']][['name']])) %>% 
            mutate(data_type = 'comparison_ratio') %>% 
            dplyr::select(colnames(df_l)) %>% 
            as_tibble()
          head(ratio_df_l$sample)
          unique(ratio_df_l$data_type)
          ratio_df_l
          colnames(df_ratio_l)
          df_ratio_l = rbind(df_ratio_l,ratio_df_l)
          unique(df_ratio_l$data_type)
          
          values$dataset_list$data$df_ratio_l = df_ratio_l
          
          colnames(df_ratio_l)
          unique(df_ratio_l$sample)
          #unique(df_ratio_l$sample)
          })
          withProgress(message = 'Summarising Data', {
            
          
            df_id_summary <- df_ratio_l %>% 
              filter(id != '' & !is.na(id)) %>% 
              group_by(id, data_type, sample) %>% 
              summarise(mean = mean(value,na.rm = T), sd = sd(value,na.rm = T))
            df_id_summary
            values$dataset_list$data$df_id_summary = df_id_summary
          
          
            df_sample_name_summary = df_ratio_l %>%
              filter(id != '' & !is.na(id)) %>% 
              group_by(data_type, sample_name,sample) %>% 
              summarise(mean = mean(value,na.rm = T), sd = sd(value,na.rm = T))
            df_sample_name_summary
            values$dataset_list$data$df_sample_name_summary = df_sample_name_summary
            
            df_sd_sample = df_sample_name_summary %>% 
              filter(data_type != 'expression') %>% 
              group_by(data_type, sample) %>% 
              summarise(max_sd = max(sd,na.rm = T), mean_sd = max(mean,na.rm = T)) %>% 
              ungroup()
            
            df_sd_sample
            df_sd_data_type = df_sample_name_summary %>% 
              filter(data_type != 'expression') %>% 
              group_by(data_type) %>% 
              summarise(max_sd = max(sd,na.rm = T), mean_sd = max(mean,na.rm = T)) %>% 
              ungroup() %>% 
              mutate(sample = 'All') %>% 
              dplyr::select(colnames(df_sd_sample))
            df_sd_data_type  
            df_sd = rbind(df_sd_sample,df_sd_data_type)
            df_sd
            values$dataset_list$data$df_sd = df_sd
            values$dataset_list$ratio$rep_sd = values$dataset_list$data[['df_sd']] %>% filter(data_type == 'replicate_ratio', sample == 'All') %>%  pull(max_sd)
            #rep_sd
            values$dataset_list$ratio$comp_sd = values$dataset_list$data[['df_sd']] %>% filter(data_type == 'comparison_ratio', sample == 'All') %>%  pull(max_sd)
            #comp_sd
            
          # ggplot(df_mean %>% filter(data_type != 'expression')) +
          #   geom_density(aes(x = mean, col = sample))
          # 
          # ggplot(df_sd %>% filter(data_type != 'expression')) +
          #    geom_boxplot(aes(y = sd,fill = data_type, x = sample)) + 
          
          
          })
          df = cbind(df,paired_ratio_df)
          values$dataset_list$data$df_ratio = df
          
          #dataset_list[['paired_ratio']] = paired_ratio_df
          #names(dataset_list)
          #if(input$save_ratios == T){
          #values$dataset_list = dataset_list
          
          #print(paste('saveRDS :',save_dataset_list_path()))
          #saveRDS(dataset_list,save_dataset_list_path())
          #}
          process_values$run_ratios = process_values$run_ratios + 1
          rds_path = values$dataset_list[['rds_path']]
          print(paste('saveRDS : ',rds_path))
          withProgress(message = 'Saving', {
            #print(paste('saveRDS : ',rds_path))
            saveRDS(values$dataset_list,rds_path)
          })
          #save_dataset(values)
          print('done')
        }else{
          df = values$dataset_list$data$df_ratio
        }
        
      #})
      
      df
      
      
    })
    
    output$density_plot = renderPlot({
      print('density_plot')
      withProgress(message = 'Density plot', {
        #density_plot_data_list = density_plot_data()
        dataset_list = values$dataset_list
        names(dataset_list)
        df_id_summary = dataset_list$data[['df_id_summary']]
        df_id_summary
        df_sd = dataset_list$data[['df_sd']]
        df_sd
        rep_sd = df_sd %>% 
          filter(data_type == 'replicate_ratio', sample == 'All') %>% 
          pull(max_sd)
        rep_sd
        comp_sd = df_sd %>% 
          filter(data_type == 'comparison_ratio', sample == 'All') %>% 
          pull(max_sd)
        comp_sd
        
        comp_mean = df_id_summary %>% 
          filter(data_type == 'comparison_ratio') %>% 
          pull(mean) %>% 
          mean(.,na.rm = T)
        comp_mean
        ggplot(df_id_summary %>% filter(data_type != 'expression')) + 
          geom_density(aes(mean,col = sample),size = 1) + 
          geom_vline(xintercept = 2*rep_sd, col = 'green', alpha = 0.5) +
          geom_vline(xintercept = -2*rep_sd, col = 'green', alpha = 0.5) +
          geom_vline(xintercept = 2*comp_sd, col = 'blue', alpha = 0.5) +
          geom_vline(xintercept = -2*comp_sd, col = 'blue', alpha = 0.5) +
          geom_vline(xintercept = comp_mean, col = 'red', alpha = 0.5) + 
          xlim(-4*comp_sd,4*comp_sd)
      })
      
      
    })
    
    output$sd_boxplot = renderPlot({
      print('df_boxplot')
      withProgress(message = 'Stat upload in progress', {
        dataset_list = values$dataset_list
        
        df_sd = dataset_list$data[['df_sample_name_summary']]
        df_sd
        ggplot(df_sd %>% filter(data_type != 'expression')) +
          geom_boxplot(aes(y = sd, x = sample, fill = data_type)) +
          geom_point(aes(y = sd, x = sample))
      })
      
    }) 
    
    ##_Stat #####
    
    #### _stat_data ####
    
    stat_df = reactiveVal()
    observeEvent(input$run_stat,{
      values$dataset_list$data[['stat']] = NULL
      withProgress(message = 'Calculation in progress', {
        #dataset_list = values$dataset_list
        #names(dataset_list)
        df_ratio_l = values$dataset_list$data[['df_ratio_l']]
        df_ratio_l %>% as.tbl
        #View(df_ratio_l)
        unique(df_ratio_l$data_type)
        unique(df_ratio_l$sample)
        
        sample_names = unique(df_ratio_l %>% filter(data_type == 'expression') %>%  pull(sample))
        var1 = rlang::parse_quosures(paste(sample_names[1]))[[1]]
        var2 = rlang::parse_quosures(paste(sample_names[2]))[[1]]
        df_mean = df_ratio_l %>% as.tbl %>% 
          filter(data_type == 'comparison_ratio') %>% 
          group_by(id) %>% 
          summarise(mean = mean(value,na.rm = T)) %>% 
          ungroup() 
        
        df_t_test <-
          df_ratio_l %>% as.tbl() %>%
          filter(data_type == 'expression') %>% 
          spread(sample,value) %>% 
          group_by(id) %>% 
          summarise(p_value = tryCatch(t.test(as.numeric(unlist(!!var1)),as.numeric(unlist(!!var2)))$p.value, error = function(e) NA)) %>% 
          ungroup() %>% 
          mutate('BH' = p.adjust(p_value,method = 'BH'))
        df_t_test
        df_stat = left_join(df_mean,df_t_test)
        df_stat
        values$dataset_list$data[['stat']] = df_stat
        
        rds_path = values$dataset_list[['rds_path']]
        
        saveRDS(values$dataset_list,rds_path)
        #values$dataset_list = dataset_list
        print('done')
        
      })
    })
    
    
    output$stat_data_table = renderDataTable({
      print('stat_data')
        print('running be patient ...')
        #dataset_list = values$dataset_list
        #names(dataset_list)
        if(!is.null(values$dataset_list$data[['stat']])){
          withProgress(message = 'Stat upload in progress', {
            
          df_stat = values$dataset_list$data[['stat']]
          
          output$fdr_plot = renderPlot({
            print('fdr_plot')
            withProgress(message = 'FDR plot', {
            
            
            
            df_stat
            hist_data = hist(as.numeric(df_stat$p_value),breaks=100,plot=FALSE)
            hist_data
            top_5 = (mean(hist_data$counts[(length(hist_data$counts)*0.8):length(hist_data$counts)]))
            top_5
            #print(str(hist_data$counts))
            #print(top_5)
            count = 0.05*length(hist_data$counts)
            count
            p_values = df_stat$p_value
            p_values = p_values[!is.na(p_values)]
            sig_p_values = p_values[p_values < 0.05]
            t_number = length(sig_p_values)
            t_number
            #print(count)
            #print(top_5)
            fdr = round((top_5*count)/t_number*100,digits=2)
            fdr
            
            
            ggplot(df_stat) + 
              geom_histogram(aes(p_value),binwidth = 0.01) +
              geom_hline(yintercept = top_5, col = 'green') +
              geom_segment(aes(x = 0.8, xend = 1.2, y = top_5, yend = top_5), col = 'blue') +
              geom_vline(xintercept = 0.05, col = 'red') + 
              geom_text(aes(x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, label = paste0('FDR = ',fdr)), size = 5) +
              coord_cartesian(xlim = c(0,1)) +
              xlab('p value')
            })
            
          })
          
          output$volcano_plot = renderPlot({
            print('volcano_plot')
            withProgress(message = 'Volcano plot', {
            rep_sd = values$dataset_list$ratio[['rep_sd']] 
            rep_sd
            comp_sd = values$dataset_list$ratio[['comp_df']]
            comp_sd
            ggplot(df_stat) + 
              geom_point(aes(x = mean,y = -log10(BH)), size = 0.5) + 
              geom_hline(yintercept = -log10(0.05), col = 'red') +
              geom_vline(xintercept = 2* rep_sd, col = 'green') +
              geom_vline(xintercept = -2* rep_sd, col = 'green') +
              geom_vline(xintercept = 2* comp_sd, col = 'blue') +
              geom_vline(xintercept = -2* comp_sd, col = 'blue')
            })
          })
          
          output$stat_info_text = renderText({
            withProgress(message = 'Info text', {
              rep_sd = values$dataset_list$ratio[['rep_sd']] 
              
              df_sig_up = df_stat %>% 
                filter(BH < 0.05 & mean > rep_sd*2)
              dim(df_sig_up)
              df_sig_down = df_stat %>% 
                filter(BH < 0.05 & mean < -rep_sd*2)
              dim(df_sig_down)
              df_sig = rbind(df_sig_up,df_sig_down)
              
              paste0('Total \t\t: ',dim(df_stat)[1],'<br/>',
                         'Significant \t: ',dim(df_sig)[1],'<br/>',
                         'Up \t\t: ',dim(df_sig_up)[1],'<br/>',
                         'Down \t\t: ',dim(df_sig_down)[1],'<br/>'
              )
            })
          })
        })
        #output$stat_data_table = renderDataTable(df_stat)
        df_stat
      }
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #### __experiemnt df #####
    experiment_df = reactive({
      print('experiment_df')
      silac = 0
      #print(input$save_experiment)
      #d = dataset_list()
      #dataset_list = readRDS(save_dataset_list_path())
      dataset_list = values$dataset_list
      length(dataset_list)
      experiment_df = dataset_list$data[['expression_data']]
      experiment_df
      if(print(input$save_upload[1]) > process_values$save_upload){
        
        #if(input$save_experiment == T){
        id_columns = c()
        if(input$id_column != '_'){
          id_columns = c(id_columns,input$id_column)
        }
        if(input$id_column_2 != '_'){
          id_columns = c(id_columns,input$id_column_2)
        }
        if(input$id_column_3 != '_'){
          id_columns = c(id_columns,input$id_column_3)
        }
        if(input$upload_data_type == 'MaxQuant'){
          if(input$mq_type == 'SILAC'){
            silac = 1
          }
        }
        if(silac == 1){
          expression_columns = c(input$silac_comp,input$silac_comp_rev,input$silac_rep,input$silac_rep_rev,input$silac_incorp)
          expression_column_list = list()
          expression_column_list[['silac_comp']] = input$silac_comp
          expression_column_list[['silac_comp_rev']] = input$silac_comp_rev
          expression_column_list[['silac_comp_rev']] = input$silac_rep
          expression_column_list[['silac_rep']] = input$silac_rep_rev
          expression_column_list[['silac_rep_rev']] = input$silac_incorp
          
          expression_column_list
        }else{
          expression_columns = c(input$condition_1,input$condition_2,input$condition_3,input$condition_4)
          expression_column_list = list()
          expression_column_list[[input$condition_1_name]] = input$condition_1
          expression_column_list[[input$condition_2_name]] = input$condition_2
          expression_column_list[[input$condition_3_name]] = input$condition_3
          expression_column_list[[input$condition_4_name]] = input$condition_4
          expression_column_list
          
        }
        
        id_columns
        expression_columns
        #dataset_list[['expression_columns']] = expression_columns
        dataset_list[['expression_columns_list']] = expression_columns_list
        
        columns = c('row_id',id_columns,expression_columns)
        print(columns)
        df = input_df()
        rownames(df) = df[,input$id_column]
        
        colnames(df)
        #View(df)
        df$row_id = rownames(df)
        #df$id = rownames(df)
        #df$row_id
        # if(input$stat_id_column != 'row_id'){ 
        #   df$id = df[,input$stat_id_column]
        # }
        #df$row_id = rownames(df)
        columns = c(columns)
        columns
        df_l = gather(df[,columns],key = sample_name, values = expression_columns) %>% 
          mutate(data_type = 'expression') %>% 
          as_tibble() %>% 
          #rename(sample_name = key) %>% 
          #as_tibble() %>% 
          mutate(sample = NA)
        df_l
        colnames(df_l)
        for(name in names(expression_column_list)){
          print(name)
          df_l$sample[df_l$sample_name %in% expression_column_list[[name]]]  = name
        }
        
        
        #View(df_l)
        #df_l$data = 'expression'
        rownames(df) = df[,input$id_column]
        experiment_df = df[,columns]
        colnames(experiment_df)
        
        dim(experiment_df)
        #dataset_list = dataset_list()
        dataset_list[['data_type']] = input$upload_data_type
        dataset_list[['experiment_type']] = input$mq_type
        
        
        dataset_list[['expression_data']] = experiment_df
        dataset_list[['experiment_path']] = input$experiment_path
        dataset_list[['experiment_name']] = input$experiment_name
        dataset_list[['experiment_code']] = input$experiment_code
        dataset_list[['experiment_description']] = input$experiment_description
        dataset_list[['id_column']] = input$id_column
        dataset_list[['id_column_2']] = input$id_column_2
        dataset_list[['id_column_3']] = input$id_column_3
        #dataset_list[['stat_id_column']] = input$stat_id_column
        dataset_list[['expression_data_l']] = df_l
        
        
        
        if(input$upload_data_type == 'MaxQuant'){
          
          peptides <- read.csv(paste0(upload_dir_path(),'/peptides.txt'),
                               header = input$header,
                               sep = '\t',
                               quote = input$quote)
          evidence <- read.csv(paste0(upload_dir_path(),'/evidence.txt'),
                               header = input$header,
                               sep = '\t',
                               quote = input$quote)
          parameters <- read.csv(paste0(upload_dir_path(),'/parameters.txt'),
                                 header = input$header,
                                 sep = '\t',
                                 quote = input$quote)
          summary <- read.csv(paste0(upload_dir_path(),'/summary.txt'),
                              header = input$header,
                              sep = '\t',
                              quote = input$quote)
          
          dataset_list[['experiment_type']] = input$mq_type
          dataset_list[['proteinGroups']] = input_df()
          dataset_list[['peptides']] = peptides
          dataset_list[['evidence']] = evidence
          dataset_list[['summary']] = summary
          dataset_list[['parameters']]  = parameters
          
        }else{
          dataset_list[['original_data']] = input_df()
        }
        if(silac == 0){               
          if(!is.null(input$condition_1)){
            dataset_list[['condition_1_name']] = input$condition_1_name
            dataset_list[['condition_1_columns']] = input$condition_1
          }
          if(!is.null(input$condition_2)){
            dataset_list[['condition_2_name']] = input$condition_2_name
            dataset_list[['condition_2_columns']] = input$condition_2
          }
          if(!is.null(input$condition_3)){
            dataset_list[['condition_3_name']] = input$condition_3_name
            dataset_list[['condition_3_columns']] = input$condition_3
          }
          if(!is.null(input$condition_4)){
            dataset_list[['condition_4_name']] = input$condition_4_name
            dataset_list[['condition_4_columns']] = input$condition_4
          }
        }else{
          dataset_list[['silac_inc']] = input$silac_inc
          dataset_list[['silac_comp']] = input$silac_comp
          dataset_list[['silac_comp_rev']] = input$silac_comp_rev
          dataset_list[['silac_rep']] = input$silac_rep
          dataset_list[['silac_rep_rev']] = input$silac_rep_rev
        }
        
        #dataset_list[['mart_id']] = 'row_id'
        #dataset_list[['sep_id']] = 'row_id'
        #mart_values = 'row_id'
        #dataset_select = 'expression_data'
        
        print(names(dataset_list))
        values$dataset_list = dataset_list
        rds_path = paste0('data/data_list/',input$experiment_code,'_data_list.rds')
        print(paste('saveRDS : ',rds_path))
        saveRDS(dataset_list,rds_path)
        #uploaded_datasets = readRDS('data/uploaded_datasets.rds')
        #uploaded_datasets
        uploaded_datasets = values$datasets
        new_datasets = 'SH_Diff_RA'
        new_datasets = input$experiment_code
        uploaded_datasets = c(uploaded_datasets, new_datasets)
        uploaded_datasets = unique(uploaded_datasets)
        uploaded_datasets
        values$datasets = uploaded_datasets
        #saveRDS(uploaded_datasets,'data/uploaded_datasets.rds')
        #print('saveRDS : uploaded_datasets')
        values$upload_save = F
        process_values$save_upload = process_values$save_upload + 1
        print('done')
      }
      #}else{
      #  experiment_df = dataset_list[['expression_data']]
      #}
      experiment_df
    })
    
    
    
    
    output$experiment_df = renderDataTable({
      
      experiment_df()
      
    })
    
    
    
    # output$rds_path_text = renderText({
    #   
    #   if(!is.null(values$dataset_list[['original_data']])){
    #     if(is.null(values$dataset_list[['input']])){
    #       values$dataset_list[['input']] = list()
    #     }
    #     if(input$experiment_code != ''){
    #       values$dataset_list[['input']][['upload_data_type']] = input$upload_data_type
    #       
    #       values$dataset_list[['input']][['experiment_name']] = input$experiment_name
    #       values$dataset_list[['input']][['experiment_code']] = input$experiment_code
    #       values$dataset_list[['input']][['experiment_description']] = input$experiment_description
    #       rds_path = paste0('data/data_list/',values$dataset_list[['input']][['experiment_code']],'_data_list.rds')
    #       values$dataset_list[['rds_path']] = rds_path
    #       
    #     }
    #     if(!is.null(values$dataset_list[['rds_path']])){
    #       rds_path = values$dataset_list[['rds_path']] = rds_path
    #       rds_path
    #     }
    #   }
    #   
    # })
    
    
    
    
    
    
    
    
    
    


  
  
  
  

  

    

# 
#     print('input_df')
#     df = data.frame()
#     #upload_file_path()
#     if(input$reload_file == 'False'){
#       if(input$upload_data_type == 'MaxQuant'){
#         df = dataset_list()[['proteinGroups']]
#       }else{
#         df = dataset_list()[['original_data']]
#       }
# 
#     }else{
# 
#       if(input$upload_data_type == 'MaxQuant'){
#         df <- read.csv(paste0(upload_dir_path(),'/proteinGroups.txt'),
#                        header = input$header,
#                        sep = '\t',
#                        quote = input$quote)
#       }else{
#         df <- read.csv(upload_file_path(),
#                        header = input$header,
#                        sep = input$sep,
#                        quote = input$quote)
#       }
#     }
#     df
  #})
  
  

  #shinyFileChoose(input, 'files', root=c(root='.'), filetypes=c('', 'txt'))
  
  
  # upload_file_path = reactive({
  #   if(input$reload_file == 'True'){
  #     #textInput("my_file_path", label = "Full path to my file", value = file.choose(), width = 1200)
  #     
  #     path = file.choose()
  #     path = gsub(paste0(getwd(),'/'),'',path)
  #     #path = paste0('.',paste(unlist(upload_file()[c(1:(length(upload_file())-1))]),collapse = '/'))
  #   }else{
  #     path = dataset_list()['experiment_path']
  #   }
  #  #path = paste(unlist(upload_file()),collapse = '/')
  #  path
  #  
  #  #observeEvent(input$load_file)
  #   
  # })
  output$upload_file_text <- renderText(print(upload_file_path()))

  shinyDirChoose(input, 'upload_dir', roots = c(wd='.'))
  upload_dir <- reactive(input$upload_dir)
  
  upload_dir_path = reactive({
    if(input$reload_file == 'True'){
      print('upload_dir_path')
      #print(upload_dir())
      #path = paste0('.',paste(unlist(upload_dir()[c(1:(length(upload_dir())-1))]),collapse = '/'),'/')
      path = dirname(input$my_file_path)
      path = gsub(paste0(getwd(),'/'),'',path)
      
    }else{
      path = dataset_list()['experiment_path']
    }
    path
  })
  output$upload_dir_text <- renderText(print(upload_dir_path()))
  
  
  output$upload_file = renderText({
    print('upload_file_text')
    print(paste(unlist(upload_file()),collapse = ' '))
    })

  output$input_file_table = renderDataTable({
    #if(input$show_table == T){
      input_df()
    #}
  })
  

  
  
  # output$stat_id_column_ui = renderUI({
  #   #selectInput('id_column','ID columns',colnames(input_df()))
  #   
  #   if(input$upload_dataset == '_'){
  #     selectInput('stat_id_column','Stat ID column',c('row_id',colnames(input_df())),'_')
  #     
  #   }else{
  #     selectInput('stat_id_column','Stat ID column',c('row_id',colnames(input_df())),dataset_list()[['stat_id_column']])
  #   }
  # })
  
  # output$experiment_path_ui = renderUI({
  #   
  #   if(input$upload_data_type == "MaxQuant"){
  #     textInput('experiment_path','Experiment Path',upload_dir_path(),width = 1200)
  #   }else{
  #     textInput('experiment_path','Experiment Path',upload_file_path(),width = 1200)
  #     
  #   }
  # })
  # 
  # 
  # 
  # output$condition_1_name_ui = renderUI({
  #   if(input$upload_dataset == '_'){
  #     textInput('condition_1_name','Condition 1 name')
  #   }else{
  #     textInput('condition_1_name','Condition 1 name',dataset_list()[['condition_1_name']])
  #   }
  # })
  # output$condition_2_name_ui = renderUI({
  #   if(input$upload_dataset == '_'){
  #     textInput('condition_2_name','Condition 2 name')
  #   }else{
  #     textInput('condition_2_name','Condition 2 name',dataset_list()[['condition_2_name']])
  #   }
  # })
  # output$condition_3_name_ui = renderUI({
  #   if(input$upload_dataset == '_'){
  #     textInput('condition_3_name','Condition 3 name')
  #   }else{
  #     textInput('condition_3_name','Condition 3 name',dataset_list()[['condition_3_name']])
  #   }
  # })
  # output$condition_4_name_ui = renderUI({
  #   if(input$upload_dataset == '_'){
  #     textInput('condition_4_name','Condition 4 name')
  #   }else{
  #     textInput('condition_4_name','Condition 4 name',dataset_list()[['condition_4_name']])
  #   }
  # })
  # 
  # 
  # output$condition_1_ui = renderUI({
  #   if(input$upload_dataset == '_'){
  #     selectInput('condition_1','Condition 1',colnames(input_df()),multiple =T )
  #   }else{
  #     selectInput('condition_1','Condition 1',colnames(input_df()),dataset_list()[['condition_1_columns']],multiple =T )
  #   }
  # })
  # output$condition_2_ui = renderUI({
  #   if(input$upload_dataset == '_'){
  #     selectInput('condition_2','Condition 2',colnames(input_df()),multiple =T )
  #   }else{
  #     selectInput('condition_2','Condition 2',colnames(input_df()),dataset_list()[['condition_2_columns']],multiple =T )
  #   }
  # })  
  # output$condition_3_ui = renderUI({
  #   if(input$upload_dataset == '_'){
  #     selectInput('condition_3','Condition 3',colnames(input_df()),multiple =T )
  #   }else{
  #     selectInput('condition_3','Condition 3',colnames(input_df()),dataset_list()[['condition_3_columns']],multiple =T )
  #   }
  # })  
  # output$condition_4_ui = renderUI({
  #   if(input$upload_dataset == '_'){
  #     selectInput('condition_4','Condition 4',colnames(input_df()),multiple =T )
  #   }else{
  #     selectInput('condition_4','Condition 4',colnames(input_df()),dataset_list()[['condition_4_columns']],multiple =T )
  #   }
  # })
  # output$condition_2_ui = renderUI({
  #   selectInput('condition_2','Condition 2',colnames(input_df()),multiple =T )
  # })
  # output$condition_3_ui = renderUI({
  #   selectInput('condition_3','Condition 3',colnames(input_df()),multiple =T )
  # })
  # output$condition_4_ui = renderUI({
  #   selectInput('condition_3','Condition 4',colnames(input_df()),multiple =T )
  # })
  
  
  
  output$silac_comp_ui = renderUI({
    if(input$upload_dataset == '_'){
      selectInput('silac_comp','SILAC Comparison',colnames(input_df()),multiple =T )
    }else{
      selectInput('silac_comp','SILAC Comparison',colnames(input_df()),dataset_list()[['silac_comp']],multiple =T )
    }
  })
  output$silac_comp_rev_ui = renderUI({
    if(input$upload_dataset == '_'){
      selectInput('silac_comp_rev','SILAC Comparison (rev)',colnames(input_df()),multiple =T )
    }else{
      selectInput('silac_comp_rev','SILAC Comparison (rev)',colnames(input_df()),dataset_list()[['silac_comp_rev']],multiple =T )
    }
  })
  
  output$silac_rep_ui = renderUI({
    if(input$upload_dataset == '_'){
      selectInput('silac_rep','SILAC Replicate',colnames(input_df()),multiple =T )
    }else{
      selectInput('silac_rep','SILAC Replicate',colnames(input_df()),dataset_list()[['silac_rep']],multiple =T )
    }
  })
  
  output$silac_rep_rev_ui = renderUI({
    if(input$upload_dataset == '_'){
      selectInput('silac_rep_rev','SILAC Replicate (rev)',colnames(input_df()),multiple =T )
    }else{
      selectInput('silac_rep_rev','SILAC Replicate (rev)',colnames(input_df()),dataset_list()[['silac_rep_rev']],multiple =T )
    }
  })
  
  output$silac_incorp_ui = renderUI({
    if(input$upload_dataset == '_'){
      selectInput('silac_incorp','SILAC Incorporated test',colnames(input_df()),multiple =T )
    }else{
      selectInput('silac_incorp','SILAC Incorporation test',colnames(input_df()),dataset_list()[['silac_incorp']],multiple =T )
    }
  })
  
  # output$silac_rep_ui = renderUI({
  #   selectInput('silac_rep','SILAC Replicates',colnames(input_df()),multiple =T )
  # })
  # output$silac_rep_rev_ui = renderUI({
  #   selectInput('silac_rep_rev','SILAC Replicates (rev)',colnames(input_df()),multiple =T )
  # })
  # output$silac_incorp_ui = renderUI({
  #   selectInput('silac_inc','SILAC Incorporation Test',colnames(input_df()),multiple =T )
  # })
  #### _experiment_df ####

  
    
  
  


  

  output$save_experiment_ui = renderUI({
    radioButtons('save_experiment','Save Experiment',c(F,T), selected = values$upload_save, inline = T)
  })


  
  output$dataset_list_print = renderPrint(print(names(dataset_list())))
  output$dataset_type_print = renderText(dataset_list()[['data_type']])
  



  
  # observerEvent(input$save_dataset,{
  #   
  #   print(paste('saveRDS :',save_dataset_list_path()))
  #   saveRDS(values$dataset_list,save_dataset_list_path())
  # })
  #output$stat_data_table = renderDataTable(stat_data())
  
  
  
  # output$fdr_plot = renderPlot({
  #   print('fdr_plot')
  #   
  #   
  #   
  #   dataset_list = values$dataset_list
  #   df = dataset_list[['stat']]
  #   hist_data = hist(as.numeric(df$p.value),breaks=100,plot=FALSE)
  #   print(hist_data)
  #   top_5 = (mean(hist_data$counts[(length(hist_data$counts)*0.8):length(hist_data$counts)]))
  #   top_5
  #   #print(str(hist_data$counts))
  #   #print(top_5)
  #   count = 0.05*length(hist_data$counts)
  #   count
  #   p_values = df$p.value
  #   p_values = p_values[!is.na(p_values)]
  #   sig_p_values = p_values[p_values < 0.05]
  #   t_number = length(sig_p_values)
  #   t_number
  #   #print(count)
  #   #print(top_5)
  #   fdr = round((top_5*count)/t_number*100,digits=2)
  #   fdr
  #   #print(paste('FDR = ',top_5,'*',count,'/',t_number,'*100 = ',fdr,"%"))
  #   df = dataset_list[['stat']]
  #   dim(df)
  #   ggplot(df) + 
  #     geom_histogram(aes(p.value),binwidth = 0.01) +
  #     geom_hline(yintercept = top_5, col = 'green') +
  #     geom_vline(xintercept = 0.05, col = 'red') + 
  #     geom_text(aes(x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, label = paste0('FDR = ',fdr)), size = 5)
  #   
  #   
  # })
  # 
  # output$volcano_plot = renderPlot({
  #   print('volcano_plot')
  #   dataset_list = values$dataset_list
  #   #saveRDS(dataset_list,'temp/dataset_list.rds')
  #   #dataset_list = readRDS('temp/dataset_list.rds')
  #   names(dataset_list)
  #   df = dataset_list[['stat']]
  #   dim(df)
  #  
  #   dim(df)
  #   ggplot(df) + 
  #     geom_point(aes(x = mean,y = -log10(BH)), size = 0.5) + 
  #     geom_hline(yintercept = -log10(0.05), col = 'red') +
  #     geom_vline(xintercept = 2* dataset_list[['rep_df']], col = 'green') +
  #     geom_vline(xintercept = -2* dataset_list[['rep_df']], col = 'green') +
  #     geom_vline(xintercept = 2* dataset_list[['comp_df']], col = 'blue') +
  #     geom_vline(xintercept = -2* dataset_list[['comp_df']], col = 'blue')
  #   
  # })
  # filedata <- reactive({
  #   inpath <- input$my_file_path
  #   read.csv(inpath)
  # })
  
  output$filetable <- renderTable({
    filedata()
  })
  
  
  #proteinGroups

})