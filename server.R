library(shiny)
shinyServer(function(input, output) {
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
    if(!is.null(input$folder)){
      wd_path = paste(c(data_root,paste(paste(input$folder$path),collapse='/')),collapse= '/')
      print(wd_path_select)
      setwd(wd_path_select)
    }else{
      wd_path_select = wd_path
    }
    setwd(wd_path_select)
    print(wd_path_select)
    wd_path_select
  })
  
  output$wd_path_print = renderPrint(print(wd_path_select()))
  output$data_file_list = renderPrint({
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
    
    
    list(data_df = data_df, timeseries_list = timeseries_list)
    
  })
  
  #data_df = readRDS('data_df.rds')
  data_df = reactive({
    print('data_df')
    print('shiny_data/data_df.rds')
    file_path = paste0(shiny_data_path,'data_df.rds')
    print(file_path)
    data_df = readRDS(file_path)
    data_df
    #load_data()$data_df
    })
  
  timeseries_list = reactive(load_data()$timeseries_list)
  
  output$data_df_table = renderTable(data_df(),rownames = TRUE)
  
  sample_names = reactive(rownames(data_df()))
  
  data_name_list = reactive({
    data_df = data_df()
    dl = data_df$data_list
    names(dl) = rownames(data_df)
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
                  selected = sample_names()[c(2,3)],
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
    data_list = unlist(input$data)
    data_list = data_list[order(data_list)]
    paste(data_list,collapse = '_')
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
    
    plates.data <- list()
    #plates.data$Plate.1 <- list(subj.ids = sample_names())
    
    for(entry in input$data) {
      print(getwd())
      print("data/string_mapped/all_mapped.df_Huang2016_txt_edited_log2_ratio_different_mart_all_MCT_all_t_test.rds")
      cmd = paste0("data = readRDS('",mapped_data_path,"all_mapped.",unlist(data_name_list()[entry]),".rds')")
      print(cmd)
      eval(parse(text = cmd))
      if(entry == 'ESC'){
        temp_df = plyr::ldply(data$log2_ratio_list,rbind)
        data = cbind(data,temp_df)
        colnames(data)
      }
      
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
        print(data$id_acc[duplicated(data$id_acc)])
        
        data$id_acc[duplicated(data$id_acc)] = paste(data$id_acc[duplicated(data$id_acc)],dup_i,sep='__')
        
      }
      while(TRUE %in% duplicated(data$id_acc)){
        print(data$id_acc[duplicated(data$id_acc)])
        data$id_acc[duplicated(data$id_acc)] = gsub(paste0('__',dup_i),paste0('__',dup_i+1),data$id_acc[duplicated(data$id_acc)])
        dup_i = dup_i + 1
      }
      
      
      #data$type = data_df$data[entry]

      
      print(dim(data))
      plates.data[[entry]] <- data
    }
    
    print(names(plates.data))
    
    
    plates.data
  }) # list of the all mapped dataframes - returned using data_df_list[[data_name]]
  
  sig_data_list = reactive({
    sig_list = list()
    for(entry in input$data){
      print(entry)
      data = data_df_list()[[entry]]
      print(dim(data))
      print(colnames(data))
      
      
      t_test_list_entry = data_df()[entry,'p.value']
      print(t_test_list_entry)
      cutoff_list_entry = data_df()[entry,'data']
      print(cutoff_list_entry)
      
      sd_cutoff = as.numeric(data_df()[entry,'sd_cutoff'])
      print(sd_cutoff)
      
      cmd = paste("reduced_data = data[data$",t_test_list_entry," < 0.05 & !is.na(data$",t_test_list_entry,"),]",sep='')
      print(cmd)
      eval(parse(text=cmd))
      print(dim(reduced_data))
      sig_data = reduced_data[reduced_data[,cutoff_list_entry] < (-2*sd_cutoff) | reduced_data[,cutoff_list_entry] > (2*sd_cutoff),]
      print(dim(sig_data))
      
      sig_list[[entry]] = sig_data
      sig_up = sig_data[sig_data[,cutoff_list_entry] > 0,]
      sig_up = sig_up[!is.na(sig_up$STRING_id),]
      sig_list[[paste(entry,'up')]] = sig_up
      sig_down = sig_data[sig_data[,cutoff_list_entry] < 0,]
      sig_down = sig_down[!is.na(sig_down$STRING_id),]
      sig_list[[paste(entry,'down')]] = sig_down
      
    }
    sig_list
  }) # list of significant data tabels sig_data_list[[data_name]]
  
  
  output$result <- renderPrint({
    print(names(data_df_list()))
    for(entry in names(data_df_list())){
      print(entry)
      print(dim(data_df_list()[[entry]]))
    }
    data_df_list()
  })
  

  
  #### Data Test #####
  # checks to see if all the id for the mapped files are valid
  table_name = 'SILAC_mapped_all'
  
  test_table_ids_function = function(table_name){
    cmd = paste('df = ',table_name)
    eval(parse(text=cmd))
    dim(df)
    na_df = df[is.na(df$id),]
    na_df
    na_num = length(na_df$id)
    na_num
    char_df = df[unlist(lapply(df$id, function(x) identical(x, character(0)))),]
    char_df
    char_num = dim(char_df)[1]
    char_num
    
    char_c_df = df[unlist(lapply(df$id, function(x) x == 'CHARACTER(0)')),]
    char_c_df
    char_c_num = dim(char_c_df)[1]
    char_c_num
    
    len_df = df[unlist(lapply(df$id, function(x) !nchar(x) > 0)),]
    len_df
    len_num = dim(char_df)[1]
    len_num 
    
    return(paste(table_name,' : ',length(df$id),',    number of na = ',na_num,'  number of character(0) = ',char_num,'  number of CHARACTER(0)) = ',char_c_num, '  nchar > 0 = ',len_num))
  }
  
  output$table_test = renderText({
    p_list = c()
    for(entry in data_name_list){
      #print(entry)
      table_name = paste0(entry,'_mapped_all')
      #print(table_name)
      p = test_table_ids_function(table_name)
      p
      p_list = c(p_list,p)
      
    }
    print(paste(p_list,collapse = ' <br> '))
  })
  #### ####
  
  #### Data Tables ####
  
  all_mapped = reactive({
    data_df_list()[[input$single_sample]]
  })
  
  sig_mapped = reactive({
    sig_data_list()[[input$single_sample]]
  })

  sig_mapped_up = reactive({
    data_colname = data_df[input$single_sample,'data']
    df = sig_mapped()[sig_mapped()[,data_colname] > 0,]
    df
  })
  
  sig_mapped_up = reactive({
    data_colname = data_df[input$single_sample,'data']
    df = sig_mapped()[sig_mapped()[,data_colname] < 0,]
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
    df = all_mapped()
    df = df[df$id %in% gene_list(),]
    df
    
  })
  
  
  ### MAPPING DATA ####
  mapped_data = reactive({
    data_list = input$data
    data_select_list = data_name_list()
    print(data_list)
    print(data_select_list)
    print(data_name_collapse())
    file_name = paste0('common_sig_mapped_',data_name_collapse(),'.rds')
    print(file_name)
    #print(list.files(path = './GO_data/'))
    #print(input$re_run)
    print(file_name %in% list.files(path = shiny_data_path))
    if(!file_name %in% list.files(path = shiny_data_path)){
      mapped_data = common_mapped_function(shiny_data_path,sig_data_list(),input$data,'lightgreen',string_db,data_select_list,data_name_collapse(),input$removed_list)
    }else{
      if(input$re_run){
        mapped_data = common_mapped_function(shiny_data_path,sig_data_list(),input$data,'lightgreen',string_db,data_select_list,data_name_collapse(),input$removed_list)
        }else{
          print('readRDS - mapped data')
          mapped_data_file_name = paste0(shiny_data_path,'common_sig_mapped_',data_name_collapse(),'.rds')

          payload_rds_file_name = paste0(shiny_data_path,'common_sig_mapped_payload_id_',data_name_collapse(),'.rds')
          entry_list_file_name = paste0(shiny_data_path,'entry_list_',data_name_collapse(),'.rds')
          mapped_ud_file_name = paste0(shiny_data_path,'sig_mapped_ud_',data_name_collapse(),'.rds')
          colour_file_name = paste0(shiny_data_path,'colour_list_',data_name_collapse(),'.rds')
          id_file_list = paste0(shiny_data_path,'id_list_',data_name_collapse(),'.rds')
          removed_file_list = paste0(shiny_data_path,'removed_list_',data_name_collapse(),'.rds')
          gene_file_list = paste0(shiny_data_path,'gene_list_',data_name_collapse(),'.rds')
          
          mapped_data = list(mapped_data = readRDS(mapped_data_file_name),
                             payload_id = readRDS(payload_rds_file_name),
                             entry_list = readRDS(entry_list_file_name),
                             mapped_ud = readRDS(mapped_ud_file_name),
                             colour_list = readRDS(colour_file_name),
                             id_list = readRDS(id_file_list),
                             removed_list = readRDS(removed_file_list),
                             gene_list = readRDS(gene_file_list)
                             )
        }
    }
    
    #mapped_data = common_mapped_function(input$data,'lightgreen',string_db,data_list)
    #str(mapped_data)
    #mapped_data$mapped_data
    #mapped_data$payload_id
    print('mapped')
    mapped_data
  })
  output$mapped_table = renderDataTable(mapped_data()$mapped_data)
  output$mapped_ud_table = renderDataTable(mapped_data()$mapped_ud)
 
  mapped_st_single_list = reactive({
    file_path = 'data/mapped_st_single_list.rds'
    if(!file.exists(file_path)){
      saveRDS(list(),file_path)
    }
    mapped_st_single_list = readRDS('data/mapped_st_single_list.rds')
    if(length(input$data) == 1){
      mapped_st_single_list[[input$data]] = mapped_st()
    }
    saveRDS(mapped_st_single_list,file_path)
    mapped_st_single_list
  })
  
  all_mapped_st = reactive({
    all_mapped = FALSE
    for(data_name in input$data){
      if(all_mapped == FALSE){
        all_mapped = mapped_st_single_list()[[data_name]]
      }else{
        all_mapped = rbind(all_mapped,mapped_st_single_list()[[data_name]])
      }
    }
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
  }) # gets the STRINGdb mapping data
  
  payload_id = reactive({
     #if(input$mapped == 'common_mapped'){
       #payload_id = readRDS('common_mapped_payload_id.rds')
       payload_id = mapped_data()$payload_id
     #}else{
       #payload_id = mapped_data()$payload_id
      # payload_id = readRDS(paste0(shiny_data_path,'payload_id.',data_name_collapse(),'.rds'))
     #}
    payload_id
    
  })
  
  #### ####
  
  
  output$table_name = renderText(paste("df.topGO_",input$limit,".",input$stat,".",input$ontology,".",input$data,".rds",sep=''))
  
  
  ##### ENRICHMENT ####
  
  output$select_enrichment_stat_ui = renderUI({
    if(input$enrichment_select == 'STRINGdb'){
      selectInput('select_sn_MT','select methodMT',string_db_methodMT_list)
    }else{
      selectInput('select_sn_MT','select stat',c('classicFIsher','fisher.elim','fisher.weight01','fisher.lea','fisher.parentchild') ,selected = 'fisher.weight01')
    }
    })
  
  enrichment_column = reactive({
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
    list(up_col = up_col, down_col = down_col)
    })
  enrichment_table = reactive({
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
    enrichment_table
  })

  output$sub_enrichment_plot = renderPlot({
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
    print(p)
    
  })
  
  enrichment_image_path_list = reactive({
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
    list(plot_path_list = plot_path_list, plot_path_line = plot_path_line, match_list = match_list, match_list_line = match_list_line)
  })
  
  combined_enrichment = reactive({
    print('######## combined_enrichment ##########')
    ontology_list = input$select_enrichment
    data_list = input$data
    sub_data_list = input$enrichment_data_select
    data_name = sub_data_list[1]

 
    enrichment_GO = FALSE
    for(ontology in ontology_list){
    if(input$sub_venn == 'basic'){
      for(data_name in data_list){
       for(sub_data_name in sub_data_list){
        if(grepl(data_name,sub_data_name)){
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
    View(enrichment_GO)
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
    print('done')
    plot_data
  })
  
  output$sn_term_select_combined_ui = renderUI({
    if(input$run_enrich_test == TRUE){
      term_list = combined_term_list()$term_list
      selected_list = term_list[c(input$combined_slider[1]:input$combined_slider[2])]
      if(input$fixed_term_combined == T){
        selected_list = input$enrich_select_term_combined
      }
      selectInput('enrich_select_term_combined','Select Term',term_list, selected_list, multiple = T)
    }
  })
  
  combined_term_list = reactive({
    print('###### combined_term_list #########')
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
    print('done')
    term_list
  })
  
  observeEvent(input$fixed_term_combined,{
    if(input$fixed_term_combined == F){
      values$combined_term_list = input$enrich_select_term_combined
    }
  })
  
  output$combined_enrichment_table = renderDataTable({
    df = combined_enrichment()
    #df = df[df$pvalue_fdr < input$eh_fdr,]
    term_list = input$enrich_select_term_combined
    #term_list = df$term_description
    df = df[df$term_description %in% term_list,]
    df
    })
  
  combined_enrichment_plot_data = reactive({
    print('######### combined enrichment plot data ##############')
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
    print('done')
    plot_data
  })
  
  output$enrich_combined_slider = renderUI({
    if(input$run_enrich_test == TRUE){
       term_list = combined_term_list()$term_list
       sliderInput('combined_slider','Select Subset Range', min = 0, max = length(term_list), value = c(0,10), step = 1,width = 1000)
    }
  })
  
  output$combined_enrichment_plot_ui = renderUI({
    if(input$run_enrich_test == TRUE){
      plotOutput('combined_enrichment_plot')
    }
  })
  
  output$combined_enrichment_plot = renderPlot({
    #if(input$enrich_run_test == T){
      print('########### combined_enrichment_plot ##############')
      ontology_list = input$select_enrichment
      
      plot_data = combined_enrichment_plot_data()
      View(plot_data)
      print(dim(plot_data))
      if(dim(plot_data)[1] > 0){
        #View(plot_data)
        
        #w = 1/(length(unique(plot_data$data))) * 3
        w = input$combined_width
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
        }
        
        data_order
        df_col_list
        term_list = rev(input$enrich_select_term_combined)
        term_list
        test = TRUE
        if(test == TRUE){
          saveRDS(plot_data,'temp/plot_data.rds')
          saveRDS(data_order,'temp/data_order.rds')
          saveRDS(df_col_list,'temp/df_col_list')
          saveRDS(term_list,'temp/term_list')
          
          #print('test')
          
          plot_data = readRDS('temp/plot_data.rds')
          data_order = readRDS('temp/data_order.rds')
          df_col_list = readRDS('temp/df_col_list')
          term_list = readRDS('temp/term_list')
        }
        #w = 0.9
        #text_wrap = 30
        saveRDS(plot_data,'temp/plot_data.rds')
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
        p = p + scale_x_discrete(limits = term_list, labels = function(x) str_wrap(x, width = text_wrap)) + 
        
              #scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) + 
          geom_hline(yintercept = 0, col = 'black', lwd - 2) + 
          
          #scale_fill_manual(breaks = data_order,values = col_list) + 
         scale_fill_manual(breaks = data_order, values = c(df_col_list)) + 
          
          #ylab('log10(p value)') +
          theme(axis.title.y = element_blank(), axis.text.y = element_text(face = 'bold', size = input$t_size)) +
          coord_flip() + 
          labs(colour = '', fill = '')
    
        p = p + theme(legend.position=input$combined_legend)
     
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
        print(p)
        
        if(input$save_plot == T){
          plot_path_line = enrichment_image_path_list()$plot_path_line
          file_name = paste0('barplot_',input$plot_values,enrichment_image_path_list()$match_list_line)
          values$plot_path = save_plot_function_2(plot_path_line,file_name)
        }
      }else{
        print('no data in plot data')
        #View(plot_data)
        
      }
     # }
  })
  
  output$combined_heatmap_plot = renderPlot({
    ontology_list = input$select_enrichment
    #plot_data = readRDS('plot_data.rds')
    plot_data = combined_enrichment_plot_data()
    plot_data = plot_data[!(plot_data$p_log > -1.3 & plot_data$p_log < 1.3),]
    #plot_data = plot_data[!(,]
    print(dim(plot_data))
    plot_data$enr = NA
    plot_data$enr[plot_data$p_log < -1.3] = 'down'
    plot_data$enr[plot_data$p_log > 1.3] = 'up'
    #plot_data$p_log[plot_data$p_log < -1.3)
    View(plot_data)
    
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
    if(input$combined_legend == F){
      p = p + theme(legend.position="none")
    }
    
    print(p)
    if(input$save_plot == T){
      plot_path_line = enrichment_image_path_list()$plot_path_line
      file_name = paste0('heatmap_',enrichment_image_path_list()$match_list_line)
      values$plot_path = save_plot_function_2(plot_path_line,file_name)
    }
    #print(p)
    #ggplot(plot_data, aes(sub_data,term_description)) + geom_tile(aes(fill = p_log), colour = 'white') + scale_fill_gradient2()
    
  })
 
  GOdata = reactive({
    print('sn_en')
    ontology = input$select_enrichment
    if(length(ontology) > 1){
      ontology = ontology[1]
    }
    path_list = gene_list_select_list()$path_list
    enrichment_path_line = enrichment_path_line_function(shiny_image_path,path_list,ontology,NULL)
    print('try heirarchy plot')
    GOdata_path = paste0(enrichment_path_line,'/topGO_GOdata.rds')
    GO_data = readRDS(GOdata_path)
    GO_data
  })
  
  sig_GOdata = reactive({
    print('sn_en')
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
    print(sig_GOdata_path)
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
      print('found')
      sig_GOdata = readRDS(sig_GOdata_path)
      
    }
    #sig_GO_data = readRDS(sig_GOdata_path)
    sig_GOdata
  })
  
  
    
  output$topGO_heirarchyPlot = renderPlot({
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
    showSigOfNodes(GOdata, score, firstSigNodes = NULL, wantedNodes = GO_ids, sigForAll = 0.01,  putCL = CL, useInfo = c('all','def','pval','np')[1], .NO.CHAR = 50)

    detach('package:Rgraphviz')
    
  })
  
  
  output$heir_select_enrichment = renderUI({
    selectInput('heir_select_enrichment','Select Enrichment',input$select_enrichment)
  })
  output$sig_topGO_heirarchyPlot = renderImage({
    
    
  #sig_topGO_heirarchy_png = reactive({
    library(Rgraphviz)
    GOdata = sig_GOdata()
    CL = 1
    
    GO_terms = input$enrich_select_term_combined
    full_topGO = combined_enrichment()
    
    full_topGO = full_topGO[full_topGO$ontology == input$heir_select_enrichment,]
    View(full_topGO)
    topGO = full_topGO
    if(input$heir_sig == T){
      topGO = full_topGO[as.numeric(full_topGO$pvalue_fdr) < as.numeric(input$eh_fdr),]
    }
    
    View(topGO)
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
    plot_path_line = enrichment_image_path_list()$plot_path_line
    file_name = paste0('heirarchy_',enrichment_image_path_list()$match_list_line)
    file_name_path = paste(plot_path_line,file_name,sep='/')
    
    
    #if(input$save_plot == T){
      #par(cex = input$heir_cex)
    #  showSigOfNodes(GOdata, score, firstSigNodes = firstSigNodes, wantedNodes = GO_ids, sigForAll = sigForAll,  putCL = putCL, useInfo = useInfo, .NO.CHAR = 50)
    #  file_output_path = save_plot_function_2(plot_path_line,file_name,c('pdf'))
    #  values$plot_name = file_output_path
      #par()
    #}
    
    if(input$heir_re_run == TRUE){
      
      png(paste0(file_name_path,'.png'), width = 4, height = 4, units = 'in', res = 600)
      par(cex = input$heir_cex)
      showSigOfNodes(GOdata, score, firstSigNodes = firstSigNodes, wantedNodes = GO_ids, sigForAll = sigForAll,  putCL = putCL, useInfo = useInfo, .NO.CHAR = 50)
      dev.off()
    }
    
    if(input$save_plot == T){
      par(cex = 1)
      par(cex = input$heir_cex)
      showSigOfNodes(GOdata, score, firstSigNodes = firstSigNodes, wantedNodes = GO_ids, sigForAll = sigForAll,  putCL = putCL, useInfo = useInfo, .NO.CHAR = 50)
      file_output_path = save_plot_function_2(plot_path_line,file_name,c('pdf'))
      print(file_output_path)
      values$plot_name = file_output_path
    }
    
    
    detach('package:Rgraphviz')
    print(file_name_path)
    file_name_path
    
    list(src = paste0(file_name_path,'.png'), width = input$heir_w)
  })
  

  

  sub_venn_list = reactive({
    mapped_data = mapped_st()
    print(dim(mapped_data))
    if(input$sub_venn_select == 'subset'){
      sub_data_list = input$enrichment_data_select
      print(sub_data_list)
      sub_data_list = gsub(':',' & ',sub_data_list)
      print(sub_data_list)
      mapped_data = mapped_data[mapped_data$label %in% sub_data_list,]
      print(dim(mapped_data))
    }
    #View(mapped_data)
    
    annot = annot()
    enrichment_table = combined_enrichment()
    venn_gene_list = list()
    for(GO_term in input$enrich_select_term_combined){
      #GO_term = input$enrich_select_term
      
      string_hits = string_hits_function(enrichment_table,GO_term,mapped_data,annot())
      print(string_hits)
      id_list = mapped_data()$id_list
      print(id_list)
      gene_list = id_list[string_hits]
    venn_gene_list[[GO_term]] = paste(gene_list)
    }
    print(venn_gene_list)
    venn_gene_list
    
  })
  
  output$sub_venn_plot = renderPlot({
    m = 1.5 # numbers
    n = 1 # sample labels
    colour_list = rainbow(length(sub_venn_list()),alpha = 0.5)
    par(bty = 'n', lty = 'blank')
    tryCatch({
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
  })

  
  output$test_table = renderTable(sub_enrichment_table())
  
  enrichment_plot_data = reactive({
    print('enrichment plot data')
    enrichment_table = enrichment_table()
    up_col = enrichment_column()$up_col
    down_col = enrichment_column()$down_col
    plot_data = data.frame(down = log10(as.numeric(enrichment_table[,down_col])), up = (log10(as.numeric(enrichment_table[,up_col]))*-1))
    
    Term = enrichment_table$Term
    print(Term)
    GO.ID = enrichment_table$GO.ID
    Term[duplicated(Term)] == paste(Term[duplicated(Term)],GO.ID[duplicated(Term)],sep='_')
    print(Term)
    print(duplicated(Term))
    
    Term = paste(Term,GO.ID)
    rownames(plot_data) = Term
    print(dim(plot_data))
    plot_data
    })
  
  output$enrich_slider_2 = renderUI({
    m.enrich = enrichment_plot_data()
    sliderInput(inputId = 'm_range_2','enrichment range',min(0),dim(m.enrich[1]),value = c(0,dim(m.enrich[1]),dragRange=T))
  })
  
  output$enrichment_plot = renderPlot({
    
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
    data_name = input$single_sample_3
    
    #load(paste('topGO_sig',input$ontology,input$data,sep='.'))
    if(input$enrichment == 'topGO'){
      file_name = paste0(enrichment_data_path(),'/topGO_sig.',enrichment_abreviation_list[input$topGO_enrichment],'.',data_name_list()[data_name],'.rds')
      
      #print(input$data)
      #print(data_list[input$data])
      #file_name = paste("df.topGO_",input$limit,".",input$stat,".",input$ontology,".",input$single_data,".rds",sep='')
      print(file_name)
      enrichment_table = readRDS(file_name)
      enrichment_table
    }
    if(input$enrichment == 'STRINGdb'){
      table_path = paste0(column_path(),'/',input$stringdb_enrichment,'/STRINGdb/',input$stringdb_enrichment,'_all.rds')
      print(table_path)
      enrichment_table = readRDS(table_path)
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
    enrichment_table
  })
  
  output$select_term_ui = renderUI({
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
      ontology = input$select_enrichment
      if(length(ontology) > 1){
        ontology = ontology[1]
      }
      print(ontology)
      STRING_id_list = STRING_id_list_function(mapped_st(),gene_list())
      print(gene_list_select_list()$path_list)
      print(length(STRING_id_list))
      path_list = gene_list_select_list()$path_list
      string_db = string_db()
      mapped_st = mapped_st()
      annot = annot()
      sample_path_line = sample_path_line()
      backgroundV = backgroundV()
      enrichment_GO = run_enrichment_function(ontology,gene_list_select_list()$path_list,STRING_id_list,string_db(),mapped_st(),annot(),sample_path_line(), backgroundV(),input)
      #run_enrichment_function = function(ontology,path_list,STRING_id_list,string_db,mapped_st,annot,sample_path_line,backgroundV,input){
        
      print(dim(enrichment_GO))
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
    df = sn_en()
    sig_list = df$term_description[df$pvalue_fdr < 0.05]
    selectizeInput('enrich_select_term','Select Term',df$term_description, sig_list, multiple = F)
    
  })
  
  output$sn_term_select_plot_ui = renderUI({
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
    View(enrichment_GO)
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
      df = sn_en()
      if(input$enrichment_select == 'STRINGdb'){
        df = df[df$pvalue_fdr < input$eh_fdr,]
      }
      
      df
      }) 
    
    enrichment_list = reactive({
      enrichment_list = list()
      data_list = list()
      for(entry in input$enrichment_data_select){
        sig_data = sig_data_list()[[entry]]
        enrichment_list[[entry]] = sig_data$STRING_id
        data_list[[entry]] = entry
      }
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
    data_name = input$single_sample
    table_path = paste0(enrichment_path,data_name_list()[data_name],'/')
    table_path
    })
  
  column_path = reactive({
    data_name =     data_name = input$single_sample
    
    column_path = paste0(table_path(),data_df()[data_name,'data'],'/')
    column_path
  })
    
  enrichment_data_path = reactive({
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
    table_path
  })
  
  output$enrichment_data_path_print = renderPrint({
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
    if(input$fix_term == 'n'){
      selectInput('term','Select GO Term',enrichment_table()$Term,selected = NA)
    }else{
      selectInput('term','Select GO Term',input$term)
    }
  })
  
  #full_gene_list = readRDS('./shiny_data/gene_list.rds')
  full_gene_list = reactive(mapped_data()$mapped_data$id)

  
  gene_list_df = reactive({
    in_file_name = input$gs_list_filename
    print(in_file_name$datapath)
    if(is.null(in_file_name)){
      return(NULL)
    }else{
      df = read.table(in_file_name$datapath, stringsAsFactors = F,sep = '\t')
    }
    print(in_file_name$datapath)
    print(df)
    df
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
    mapped_data = mapped_data()$mapped_data
    mapped_data$STRING_id[mapped_data$id %in% gene_list()]
  })
  

  
  
  output$df_gene_list = renderDataTable(data.frame(gene_list = gene_list()))
  output$df_STRING_id_list = renderDataTable(data.frame(gene_list = STRING_id_list()))
  
  gene_list_select_list = reactive({
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
        gene_list = unlist(gene_list_df()[1])
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
          print(tf_list)
          gene_list = unlist(id_list[tf_list])
          print(gene_list)
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
    list(gene_list = gene_list, path_list = path_list)

  }) # needs work currently only using GE list
  
  path_line = reactive({
    path_list = gene_list_select_list()$path_list
    #path_list = c(shiny_image_path,path_list)
    path_entry = c()
    if(input$save_plot == T){
      for(entry in path_list){
        path_entry = c(path_entry,entry)
        path_entry_line = latex_filename_function(paste(path_entry,collapse = '/'))
        create_dir_function(paste0(shiny_image_path,path_entry_line))
      }
    }
    path_line = paste0(shiny_image_path,latex_filename_function(paste(path_list,collapse='/')),'/')
    values$plot_path = path_line
    print(path_line)
    
    path_line
  })
  
  
  
  venn_values = reactiveValues(gene_list = c())
  
  
  observeEvent(input$store_button,{
    print('store_button')
    print(venn_values$gene_list)
    print(gene_list())
    venn_values$gene_list = paste(gene_list())
    path_list = gene_list_select_list()$path_list
    
    venn_values$path_list = c(path_list)
    #venn_values$path_list = gene_list_select_list()$path_list
    print('store venn')
    print(venn_values$gene_list)
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
    print(length(gene_list))
    venn_values$gene_list = gene_list
    
    path_list = gene_list_select_list()$path_list
    path_list = c(path_list,input$venn_int,paste(term_list,collapse = '_'))
    print(path_list)
    venn_values$path_list = c(path_list)
  })
  
  observeEvent(input$store_button_neighbour,{
    print('store button neighbour')
    int = neighbour_data()$int
    id_list = mapped_data()$id_list
    gene_list = unlist(id_list[tf_list])
    #print(venn_values$gene_list)
    #print(gene_list())
    venn_values$gene_list = paste(gene_list)
    venn_values$path_list = c(gene_list_select_list()$path_list,'neighbour')
    print('store venn')
    print(venn_values$gene_list)
  })
  
  venn_gene_list_select = reactive({
    # if(!input$data_type_radio == 'venn'){
    #   venn_list = venn_store()
    # }
    if(input$venn_data_select_button == 'all' ){
      venn_list = mapped_data()$mapped_ud[,input$venn_id_select]
    }else{
      venn_list = venn_values$gene_list
    }
    print('final venn')
    print(venn_list)
    venn_list = venn_list[!is.na(venn_list)]
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
  
  create_dir_list_function = function(path_list){
    path_entry_list = c()
    for(entry in path_list){
      path_entry_list = c(path_entry_list,entry)
      path_entry_line = latex_filename_function(paste(path_entry_list,collapse='/'))
      #print(path_entry_line)
      create_dir_function(path_entry_line)
    }
    return(path_entry_line)
  }
  
  gene_list_file_path = reactive({
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
  
  
  

  
  
  
  output$select_gene_list_ui = renderUI({
    selectInput('select_gene_list','select genes',choices = mapped_data()$mapped_data$id, selected = c('SOX2','NES','GFAP','LIN28A','ANXA6'), multiple = T)
  })
  
  output$select_gene_list_remove_ui = renderUI({
    removed_file_list = paste0(shiny_data_path,'removed_list_',data_name_collapse(),'.rds')
    gene_file_list = paste0(shiny_data_path,'gene_list_',data_name_collapse(),'.rds')
    
    removed_list = readRDS(removed_file_list)
    gene_list = readRDS(gene_file_list)
    selectInput('removed_list',"Gene's to remove",choices = gene_list, removed_list, multiple = T)
  })
  output$removed_genes_text = renderText(paste(mapped_data()$removed_list))
  
  output$select_gene_file_ui = renderUI({
    fileInput('gs_list_filename', 'Select List', multiple = FALSE, accept = NULL, width = NULL,
              buttonLabel = "Browse...", placeholder = "No file selected")
  })
  
  
  # output$select_gene_file_ui = renderUI({
  #   fileInput('gs_list_filename', 'Select List', multiple = FALSE, accept = NULL, width = NULL,
  #             buttonLabel = "Browse...", placeholder = "No file selected")
  # })

  
  output$gene_list_3 = renderUI({
    #gene_list = readRDS('common_mapped.rds')
    selectInput('genes_3','select genes',full_gene_list)
  }) 
  
  
  

  
  
  
  
  output$file_list = renderText(go_files)
  
  output$m_name = renderText(paste0('m.enrich.',input$stat,'.',input$ontology,'.',input$data,'.rds'))
  
  m.enrich = reactive({
    m.enrich = readRDS(paste0('m.enrich.',input$stat,'.',input$ontology,'.',input$single_data,'.rds'))
  })
  output$enrich_slider = renderUI({
    
    sliderInput(inputId = 'm_range','enrichment range',min(0),dim(m.enrich())[1],value = c(0,dim(m.enrich())[1]),dragRange=T)
  })
  
  output$enrich_barplot = renderPlot({
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
    annot_file_path = paste0(shiny_data_path,input$taxonomy,'_annot.rds')
    annot_list_all_file_path = paste(shiny_data_path,input$taxonomy,'_annot_list_all.rds')
    annot_list_non_IEA_file_path = paste(shiny_data_path,input$taxonomy,'_annot_list_no_EIA.rds')
    print(annot_file_path)
    if(file.exists(annot_file_path)){
      print('found')
      annot = readRDS(annot_file_path)
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
    annot
  })
  
  base_string_db = reactive({
    print('STRINGdb$new')
    file_path = paste0(shiny_data_path,input$taxonomy,'base_string_db.rds')
    taxonomy_number = input$taxonomy
    if(file.exists(file_path) & input$background_re_run == F){
      base_string_db = readRDS(file_path)
    }else{
      base_string_db = STRINGdb$new(version="10", species=as.numeric(taxonomy_number), score_threshold=400, input_directory=table_path())
      saveRDS(base_string_db,file_path)
    }
    base_string_db
    })
  
  backgroundV = reactive({
    
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
    backgroundV
  })
  
  string_db = reactive({
    print('string_db')
    taxonomy_number = input$taxonomy
    file_path = paste0(shiny_data_path,taxonomy_number,'_',data_name_collapse(),'_string_db_all_mapped.rds')
    print(file_path)
    if(file.exists(file_path) & input$re_run == F & input$background == 'all_mapped'){
      print('found')
      result = readRDS(file_path)
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
        saveRDS(result,file_path)
      }
    }
    result
  })
  

  
  output$string_pic = renderImage(list(src='www/vacuolar_part_STRING_UP.pdf'))
  
  

  
  output$term_num = renderText(paste(dim(enrichment_table())[1],'terms'))
  
  output$sn_list = renderText(paste(length(string_hits_list()),'nodes'))
  
  #values = reactiveValues()
  #values$sn = 0
  
  #observeEvent(input$run_sn, {
  #  values$sn = 1
  #})
  
  values <- reactiveValues(shouldShow_sn_select = FALSE, shouldShow_sn_select_link = FALSE, plot_name = '', enrich_gene_list = FALSE, plot_path = '')
  
  ##### STRING plots ####
  
      #### ___ string_hits_list ####
      string_hits_list = reactive({
        #if(values$sn == 1){
        print('string hits')
        mapped_data = mapped_st()
        GO_term = input$term
        GO_id = enrichment_table()$GO.ID[enrichment_table()$Term == GO_term]
        
        string_GO_members = annot()$STRING_id[annot()$term_id == GO_id]
        string_GO_members
        GO_members = string_GO_members
        STRING_hits = GO_members[GO_members %in% mapped_data$STRING_id]
        print('string hits done')
        print(STRING_hits)
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
            test = F
            if(test == T){
              mapped_data = results_list$mapped_data
              payload_id = results_list$payload_id
              entry_list = results_list$entry_list
              gene_list = c('SOX2','GFAP')
            }
            mapped_data = mapped_data()$mapped_data
            print(colnames(mapped_data))
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
              print(neighbours)
              int = c(intersect(mapped_data$STRING_id,neighbors),custom_list)
            }
            if(input$sn_select == 'interaction'){
              print('get_neigbours')
              neighbors = string_db()$get_interactions(custom_list)
              print(neighbours)
              
              int = c(intersect(mapped_data$STRING_id,neighbors),custom_list)
            }
            if(input$sn_select == 'sub'){
              print('get_neigbours')
              neighbors = string_db()$get_subnetwork(custom_list)
              int = c(intersect(mapped_data$STRING_id,neighbors),custom_list)
            }
            
            if(input$sn_select == 'cluster'){
              print('get_neigbours')
              neighbors = string_db()$get_clusters(custom_list)
              print(neighbours)
              
              int = c(intersect(mapped_data$STRING_id,neighbors),custom_list)
            }
            
            #if(input$sn_select == 'interaction'){
            #  int = custom_list
            #}
            
            #print('plot_network')
            #p = string_db$plot_network(int,payload_id=payload_id,add_link = FALSE)
            #print(p)
            list(int = int)
            #legend(1,30,legend = names(entry_list),fill = paste(entry_list),cex = 0.5)
            
          })
      ##### __sn_PLOT #####  
          #sn_plot = reactive({
      
          output$sn_image_ui = renderUI({
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
      
          output$sn_thesis_path = renderText({
            image_path = paste0(path_line(),'/string.pdf')
            if(input$sn_select == 'neighbour'){
              image_path = paste0(path_line(),'/string_neighbour.pdf')
            }
            if(file.exists(image_path)){
              sub('./',thesis_path_sub,image_path)
            }else{
              print('')
            }
            
          })
      
          output$sn_image = renderImage({
            image_path = paste0(path_line(),'/string.png')
            if(input$sn_select == 'neighbour'){
              image_path = paste0(path_line(),'/string_neighbour.pdf')
            }
            if(file.exists(image_path)){
              values$plot_name = 'string.pdf'
              if(input$sn_select == 'neighbour'){
                values$plot_name = 'string_neighbour.pdf'
              }
              list(src = image_path)
            }else{
              
              list(src = image_path, alt = "No image available, Generate Plot")
            }
          })
      
          output$neighbour_plot = renderPlot({
            if(values$shouldShow_sn_select){
              int = neighbour_data()$int
              payload_id = mapped_data()$payload_id
              print('plot_network')
              string_db()$plot_network(int,payload_id=payload_id,add_link = FALSE)

              if(input$save_plot == T){
                  plot_name = 'string'
                  if(input$sn_select == 'neighbour'){
                    plot_name = 'string_neighbour'
                  }
                  if(input$data_type_radio == 'venn'){
                    plot_name = latex_filename_function(paste(plot_name,paste(input$venn_int,collapse = '_')))
                  }
                  values$plot_name = paste0(plot_name,'.pdf')
                  save_plot_function_2(path_line(),plot_name,c('png','pdf'))
                }
              

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
            int = neighbour_data()$int
            mapped_data = mapped_data()$mapped_data
            plot_data = mapped_data[mapped_data$STRING_id %in% int,]
            plot_data
            rownames(plot_data) = plot_data$id
            plot_data = plot_data[,c((grep('STRING_id',colnames(plot_data))+1):(grep('col',colnames(plot_data))-1))]
            plot_data
          })
          
          output$neighbour_heatmap = renderPlot({
            if(values$shouldShow_sn_select){
              plot_data = neighbour_table()
              print(plot_data)
              plot_data[plot_data == 0] = NA
              print(plot_data)
              plot_data = plot_data[rowSums(plot_data,na.rm =T) != 0,]
              print(plot_data)
              #col_pallete = colorRampPalette(c("green", "red"), space="rgb")(64)
              heatmap.2(as.matrix(plot_data),Rowv = F,Colv = F,dendrogram = c("none"),col=redgreen(75),trace = 'none',cexRow = 1,cexCol = 1)
              }else{
              paste(' ')
            }
          })
         ### ____legend ####
          output$legend = renderPlot({
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
    metric = 'mean'
    if(input$single_data %in% timecourse_data){
      metric = 'slope'
    }
    file_name = paste0('images/STRINGdb/',input$single_data,'/',metric,'/',ontology_path_name[input$ontology],'/STRING/',latex_filename_function(input$term),'/',latex_filename_function(input$term),'_STRING.pdf')
    print(file_name)
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
    file_name = paste0('images/STRINGdb/GO_full_comparison/0.01/topGO/fisher_weight01/',ontology_path_name[input$ontology],'/single/crop/',latex_filename_function(input$term),'.pdf')
    file_name
  })
  
  output$pdfview_GO <- renderUI({
    #pdf("www/myreport.pdf")
    #hist(rnorm(100))
    #dev.off()
    print(GO_composite_file())
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
    gene_list = gene_list()
    print(gene_list)
    ESC_data = ESC_mapped_all[ESC_mapped_all$id %in% gene_list,]
    GE_data = GE_mapped_all[GE_mapped_all$id %in% gene_list,]
    SILAC_data = SILAC_mapped_all[SILAC_mapped_all$id %in% gene_list,]
    NES_data = NES_Diff_mapped_all[NES_Diff_mapped_all$id %in% gene_list,]
    NS_data = NS_Diff_mapped_all[NS_Diff_mapped_all$id %in% gene_list,]
    
    print('Select')
    print(ESC_data)
    list(ESC_data = ESC_data, GE_data = GE_data, SILAC_data = SILAC_data, NES_data = NES_data, NS_data = NS_data)
    
  })
  ###

  heatmap_data = reactive({
    
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
      
      print('gene_list')
      print(length(gene_list))
      print(gene_list)
      plot_data =  data.frame(id = gene_list)
      print(plot_data)
      plot_data
      print(ESC_sig)
      print(GE_sig)
      print(SILAC_sig)
      print(NES_sig)
      print(NS_sig)
      plot_data$ESC = ESC_sig$mean[match(plot_data$id,ESC_sig$id)]
      plot_data$GE = GE_sig$mean[match(plot_data$id,GE_sig$id)]
      plot_data$SILAC = SILAC_sig$mean[match(plot_data$id,SILAC_sig$id)]
      plot_data$NES_Diff = NES_sig$slope[match(plot_data$id,NES_sig$id)]
      plot_data$NS_Diff = NS_sig$slope[match(plot_data$id,NS_sig$id)]
    }
    gene_list = gene_list()
    plot_data =  data.frame(id = gene_list)
    for(entry in sample_names()){
      print(entry)
      sig_data = sig_data_list()[[entry]]
      cutoff_list_entry = data_df()[entry,'data']
      print(cutoff_list_entry)
      plot_data[,entry] = sig_data[,cutoff_list_entry][match(plot_data$id,sig_data$id)]
    }
    
    #print(plot_data)
    View(plot_data)
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
    
    list(sig_mean = plot_data)
    
    #heatmap.2(as.matrix(plot_data),Rowv = F,Colv = F,dendrogram = c("none"),col = greenred(32))
  })
##### HEATMAPS #####   
  output$selected_heatmap = renderPlot({
    plot_data = heatmap_data()$sig_mean
    #View(plot_data)
    #print(colnames(plot_data))
    #print(input$data)
    plot_data = plot_data[,input$data]
    #print(plot_data)
    col_pallete = colorRampPalette(c("green", "red"), space="rgb")(64)
    heatmap.2(as.matrix(plot_data),Rowv = F,Colv = F,dendrogram = c("none"),col = col_pallete,trace = 'none',cexRow = 1,cexCol = 1)

    if(input$save_plot == T){
      plot_name = 'neatmap_mean'
      if(input$data_type_radio == 'venn'){
        plot_name = latex_filename_function(paste(plot_name,paste(input$venn_int,collapse = '_')))
      }
      values$plot_name = paste0(plot_name,'.pdf')
      save_plot_function_2(path_line(),plot_name,c('pdf'))
    }
  })

  output$selected_heatmap_ggplot = renderPlot({
    #plot_data = heatmap_data()$sig_mean
    plot_data = m()
    plot_data = plot_data[plot_data$id %in% gene_list(),]
    saveRDS(plot_data,'temp/plot_data.rds')
    
    
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
  output$full_selected_heatmap = renderPlot({
    gene_list = gene_list()
    print(gene_list)
    data = data_df_list()[[input$single_sample]]
    print(dim(data))
    #data = NES_Diff_mapped_all
    data_cols = paste(unlist(strsplit(data_df()[input$single_sample,'cols'],', ')))
    print(data_cols)
    ts_cols = c('id',data_cols)
    print(ts_cols)
    colnames(data)
    #data = select_data()$NES_data
    selected_data = data[data$id %in% gene_list,]
    selected_data = selected_data[,ts_cols]
    selected_data = delete.na(selected_data, length(ts_cols)-2)
    dups = duplicated(selected_data$id)
    while(TRUE %in% dups){
      selected_data$id[dups] = paste0(selected_data$id[dups],'.1')
      dups = duplicated(selected_data$id)
    }
    selected_data$id
    plot_data = selected_data
    rownames(plot_data) = plot_data$id
    plot_data$id = NULL
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
  
  output$enrichement_data_select_ui = renderUI({
    entry_list = input$data
    #entry_list = data_name_list
    if(input$sub_venn == 'basic'){
      venn_data_list = unlist(lapply(entry_list,function(x) c(paste0(x,' down'),paste0(x,' up'))))
      selected = venn_data_list
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
          entry_list = input$venn_data_select
          #print(entry_list)
          mapped_ud = mapped_data()$mapped_ud
          mapped_ud = mapped_ud[mapped_ud[,input$venn_id_select] %in% venn_gene_list_select(),]
          colour_list = mapped_data()$colour_list
          #print(colour_list)
          #print(colour_list[entry_list])
          plot_data_list = list()
          for(entry in entry_list){
            plot_data_list[entry] = list(mapped_ud[,input$venn_id_select][mapped_ud[,entry] != 0])
            #print(plot_data_list[entry])
          }
          colour_list = colour_list[entry_list]
          list(plot_data_list = plot_data_list, colour_list = colour_list)
        })
        
        
        
        output$venn_plot_path_print = renderText({
          path_print = paste0(venn_plot_path_r(),'/',latex_filename_function(venn_plot_name_r()),'.pdf')
          path_print = sub('./',thesis_path_sub,path_print)
          #path_print = paste0(latex_filename_function(path_print),'.pdf')
          })                          
        #Thesis_Data/Cleanup_Data/images/shiny/Venn/ESC_GE_NES_Diff_NS_Diff_SILAC/ALL_ESC_down_ESC_up_GE_down_GE_up.pdf
        
        output$venn_all = renderPlot({

          #plot_path = paste(venn_image_path(),plot_name,spe='/')
          #pdf(plot_name)
          plot_data_list = venn_all_reactive()$plot_data_list
          plot_data_list_name = input$venn_data_select
          colour_list = venn_all_reactive()$colour_list
          plot_name = latex_filename_function(paste((plot_data_list_name),collapse='_'))
          values$plot_name = paste0(plot_name,'.pdf')
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
          plot_data_list = venn_all_reactive()$plot_data_list
          colour_list = venn_all_reactive()$colour_list
          p = gplots::venn(plot_data_list,show.plot = F)
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
          v_list_id = mapped_ud$id[mapped_ud[,input$venn_id_select] %in% unique(v_list)]
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
          plot_data_list = venn_select_reactive()$plot_data_list
          colour_list = venn_select_reactive()$colour_list
          p = gplots::venn(plot_data_list,show.plot = F)
          p
        })
        
        venn_select_gene_list = reactive({
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
          test = F
          if(test == T){
            mapped_ud_full = results_list$mapped_ud
            colour_list = results_list$colour_list
            entry_list = c('ESC up','ESC down')
            
          }
          int = neighbour_data()$int
          entry_list = input$venn_data_select_select
          print(entry_list)
          mapped_ud_full = mapped_data()$mapped_ud
          head(mapped_ud_full)
          print(dim(mapped_ud_full))
          mapped_ud = mapped_ud_full[mapped_ud_full$STRING_id %in% int,]
          print(dim(mapped_ud))
          print(head(mapped_ud))
          colour_list = mapped_data()$colour_list
          #print(colour_list)
          #print(colour_list[entry_list])
          plot_data_list = list()
          print(entry_list)
          for(entry in entry_list){
            print(entry)
            plot_data_list[entry] = list(mapped_ud$id[mapped_ud[,entry] != 0])
            print(plot_data_list[entry])
          }
          colour_list = colour_list[entry_list]
          list(plot_data_list = plot_data_list, colour_list = colour_list)
        })
        
        output$venn_select = renderPlot({
          if(values$shouldShow_sn_select){
            plot_data_list = venn_select_reactive()$plot_data_list
            print(plot_data_list)
            colour_list = venn_select_reactive()$colour_list
            print(colour_list)
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
          View(mapped_ud_full)
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
          list(plot_data_list = plot_data_list, colour_list = colour_list)
        })
        
        output$venn_GO = renderPlot({
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
          data_select_list = input$data
          #if(input$boxplot_full == 'full'){
          #  data_select_list = rownames(data_df())
          #  print(data_select_list)
          #}
          #data_name_collapse = paste(data_select_list,collapse = '_')
          
          m_file = paste0(shiny_data_path,'/',data_name_collapse(),'_m.rds')
          print(m_file)
          print(file.exists(m_file))
          if(file.exists(m_file) == FALSE | input$re_melt == T){
            print('re-run m')
 
            m = common_melt_function(data_select_list, data_df_list(), sig_data_list(), data_df(), timeseries_list())
            saveRDS(m,m_file)
          }else{
            print('readRDS(m)')
            m = readRDS(m_file)
          }
          m
        })
        
        m_ts = reactive({
          m_file = paste0(shiny_data_path,'/',data_name_collapse(),'_m_ts.rds')
          print(m_file)
          print(file.exists(m_file))
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
          
          m_ts
        })
        
        output$m_ts_table = renderDataTable(m_ts())
      #### ___pdfs ####
        
      output$boxplot_pdfs = renderText({
        gene_list = gene_list()
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
        print(data_select_list)
        m = m()
        print('m')
        print(dim(m))
        print(unique(m$data))
        gene_list = gene_list()
        print(gene_list)
        sub_m = m[m$id == gene_list,]
        print(dim(sub_m))
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
      gene_list = gene_list()
      #sliderInput('boxplot_range','boxplots displayed',min = 1,max = length(gene_list), value =  c(1,2),step = 1,width = 800)
      selectInput('boxplot_gene_select', 'Select Genes', gene_list, gene_list[1:2],multiple = T, width = 1000)
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
        values$plot_name = 'id.pdf'
        #View(m())
        #m_data = m()[m()$id %in% gene_list,]
        boxplot_hit = 1
        for(gene in gene_list){
          plot_path = paste0(sample_path_line,'/',gene,'.png')
          if(file.exists(plot_path) == FALSE){
            boxplot_hit = 0
          }
        }
        print(boxplot_hit)
        if(boxplot_hit == 0 | input$re_run_boxplots == TRUE){
          print('generate images')
          
          test_plots_save = T
          if(test_plots_save == T){
            print('saveRDS')
            m = m()
            saveRDS(m,'temp/m.rds')
            data_df = data_df()
            saveRDS(data_df,'temp/data_df.rds')
            saveRDS(sample_list,'temp/sample_list.rds')
            saveRDS(sample_path_line,'temp/sample_path_line.rds')
            saveRDS(input,'temp/input.rds')
            saveRDS(output,'temp/output.rds')
          }
          test_plot_read = F
          if(test_plot_read == T){
            m = readRDS('temp/m.rds')
            data_df = readRDS('temp/data_df.rds')
            sample_list = readRDS('temp/sample_list.rds')
            sample_path_line = readRDS('temp/sample_path_line.rds')
            input = readRDS('temp/input.rds')
            output = readRDS('temp/output.rds')
          }
          
          
          renderPlots(m(), data_df(), sample_list, gene_list ,sample_path_line,input, output, 'gplot')
        }else{
          print('upload images')
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
       makePlotContainers_sample(sample_list)
     })
     observeEvent(input$sample_boxplot,{
       #data_select_list = input$data
       sample_list = input$data
       gene_list = gene_list()
       renderPlots_sample(m(),sample_list, gene_list, path_line(), input, output, 'gplot')
     })
     
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
         print(boxplot_hit)
         if(boxplot_hit == 0 | input$re_run_boxplots == TRUE){
           print('generate images ts')
           renderPlots_ts(m_ts(), sample_list, gene_list, timeseries_list() ,sample_path_line,input, output, 'gplot_ts')
         }else{
           print('upload images ts')
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
       #print(string_hits_list())
       enrichmentGO <- string_db$get_enrichment(string_hits_list(), category = "Process", methodMT = "fdr", iea = TRUE )
        print(enrichmentGO)
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
       
       print(geneList_sig_file)
       
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
       
       topGO_gene_List = list(geneList_sig = geneList_sig,
                              geneList_up = geneList_up,
                              geneList_down = geneList_down)
     })
     
  ### plot paths ####
     
     
     venn_plot_path_r = reactive({
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
       venn_dc_path
       
     })
     
     venn_plot_name_r = reactive({
       plot_name = 'sub'
       data_list = input$venn_data_select
       data_list = data_list[order(data_list)]
       if(input$venn_data_select_button == 'all'){
         plot_name = paste0('ALL_',paste(data_list,collapse = '_'))
       }
       plot_name
     })
     
     

    
    sample_path_line = reactive({
      path_line = paste0(shiny_image_path,data_name_collapse())
      create_dir_function(path_line)
      path_line
    })

    
    
    output$path_list_print = renderText(path_line())
    
    output$thesis_path_print = renderText({
      
      plot_name = values$plot_name
      #plot_path =  paste0(path_line(),'/',plot_name)
      
      plot_path = values$plot_path
      #plot_path =  paste0(path_line(),'/',plot_name)
      #values$plot_path = plot_path_2
      plot_path = paste0(thesis_path_sub,plot_path,plot_name)
      plot_path
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
    
})

##### MAXQUANT #####

  #proteinGroups

