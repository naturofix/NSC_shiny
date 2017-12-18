library(shiny)
shinyServer(function(input, output) {
  shinyDirChoose(input, 'folder', roots=c(data = wd_path))
  
  #shinyDirChoose(input, 'dir', roots = c(home = '/Users/sgarnett/Documents/Doctorate/Thesis/Thesis_Data/Cleanup_Data'), filetypes = c('', 'txt'))
  
  output$dir_text = renderPrint({
    print(input$folder)
    print(paste(paste(input$folder$path),collapse='/'),collapse='/')
    print(paste(c(data_root,paste(paste(input$folder$path),collapse='/')),collapse= '/'))
  })
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
    selectInput(inputId = 'data',  # Drop down menu to select the producer and cultivar
                label = 'Select Samples',
                choices = sample_names(),
                selected = sample_names()[1],
                multiple = T)
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
      mapped_data = common_mapped_function(shiny_data_path,sig_data_list(),input$data,'lightgreen',string_db,data_select_list,data_name_collapse())
    }else{
      if(input$re_run){
        mapped_data = common_mapped_function(shiny_data_path,sig_data_list(),input$data,'lightgreen',string_db,data_select_list,data_name_collapse())
        }else{
          print('readRDS - mapped data')
          mapped_data_file_name = paste0(shiny_data_path,'common_sig_mapped_',data_name_collapse(),'.rds')

          payload_rds_file_name = paste0(shiny_data_path,'common_sig_mapped_payload_id_',data_name_collapse(),'.rds')
          entry_list_file_name = paste0(shiny_data_path,'entry_list_',data_name_collapse(),'.rds')
          mapped_ud_file_name = paste0(shiny_data_path,'sig_mapped_ud_',data_name_collapse(),'.rds')
          colour_file_name = paste0(shiny_data_path,'colour_list_',data_name_collapse(),'.rds')
          id_file_list = paste0(shiny_data_path,'id_list_',data_name_collapse(),'.rds')
          
          mapped_data = list(mapped_data = readRDS(mapped_data_file_name),
                             payload_id = readRDS(payload_rds_file_name),
                             entry_list = readRDS(entry_list_file_name),
                             mapped_ud = readRDS(mapped_ud_file_name),
                             colour_list = readRDS(colour_file_name),
                             id_list = readRDS(id_file_list))
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
    sn_en = reactive({
      hits = STRING_id_list()
      print(hits)
      print(input$select_enrichment)
      print(input$select_sn_MT)
      print(input$iea)
      enrichment_GO <- string_db()$get_enrichment(hits, category = input$select_enrichment, methodMT = input$select_sn_MT, iea = FALSE )
      #enrichment_GO <- string_db()$get_enrichment(hits, category = input$select_enrichment, methodMT = input$select_sn_MT)
      
      #enrichment_GO <- string_db()$get_enrichment(hits, category = input$select_enrichment)
      enrichment_GO
    })
  #c('Component', 'Function','Process', 'KEGG','Pfam','InterPro', 'Tissue','Disease')
  
  output$sn_en_table_Component = DT::renderDataTable({
    hits = STRING_id_list()
    #print(str(string_db))
    DT::datatable(string_db()$get_enrichment(hits, category = 'Component', methodMT = input$select_sn_MT, iea = FALSE), list(pageLength = 5))
  })
  output$sn_en_table_Function = renderDataTable({
    hits = STRING_id_list()
    DT::datatable(string_db()$get_enrichment(hits, category = 'Function', methodMT = input$select_sn_MT, iea = FALSE), list(pageLength = 5))
  })
  output$sn_en_table_Process = renderDataTable({
    hits = STRING_id_list()
    DT::datatable(string_db()$get_enrichment(hits, category = 'Process', methodMT = input$select_sn_MT, iea = FALSE), list(pageLength = 5))
  })
  output$sn_en_table_KEGG = renderDataTable({
    hits = STRING_id_list()
    DT::datatable(string_db()$get_enrichment(hits, category = 'KEGG', methodMT = input$select_sn_MT, iea = FALSE), list(pageLength = 5))
  })
  output$sn_en_table_InterPro = renderDataTable({
    hits = STRING_id_list()
    DT::datatable(string_db()$get_enrichment(hits, category = 'InterPro', methodMT = input$select_sn_MT, iea = FALSE), list(pageLength = 5))
  })
  output$sn_en_table_Tissue = renderDataTable({
    hits = STRING_id_list()
    DT::datatable(string_db()$get_enrichment(hits, category = 'Tissue', methodMT = input$select_sn_MT, iea = FALSE), list(pageLength = 5))
  })
  output$sn_en_table_Disease = renderDataTable({
    hits = STRING_id_list()
    DT::datatable(string_db()$get_enrichment(hits, category = 'Disease', methodMT = input$select_sn_MT, iea = FALSE), list(pageLength = 5))
  })
  
    output$sn_en_print = renderPrint(head(sn_en()))
    
    output$sn_en_table = renderDataTable(sn_en()) 
    
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
    data_name =     data_name = input$single_sample
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
          #path_list = c(path_list,input$venn_data_select_button)
        #}else{
          path_list = c(venn_values$path_list,'venn')
          #plot_data_list =  venn_all_reactive()$plot_data_list
          
          #plot_name = latex_filename_function(paste(names(plot_data_list),collapse='_'))
          #path_list = c(path_list,input$venn_data_select_button,plot_name)
          #path_list = c(path_list,input$venn_data_select_button)
          
        }else{
          path_list = c(data_name_collapse(),input$data_type_radio)
          
        }
        gene_list = venn_gene_list()
        #gene_list = c('SOX2')
      }
    #print(path_list)
    gene_list = gene_list[order(gene_list)]
    list(gene_list = gene_list, path_list = path_list)

  }) # needs work currently only using GE list
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
      venn_list = mapped_data()$mapped_ud$id
    }else{
      venn_list = venn_values$gene_list
    }
    print('final venn')
    print(venn_list)
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
    selectInput('select_gene_list','select genes',choices = mapped_data()$mapped_data$id, selected = c('SOX2','NES','GFAP','LIN28A'), multiple = T)
  })
  
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
    annot_file_path = paste0(table_path(),'/annot.rds')
    if(file.exists(annot_file_path)){
      annot = readRDS(annot_file_path)
    }else{
      print('generate annot')
      annot = string_db()$get_annotations()
    }
    annot
  })
  
  base_string_db = reactive({
    print('STRINGdb$new')
    taxonomy_number = 9606
    string_db = STRINGdb$new(version="10", species=taxonomy_number, score_threshold=400, input_directory=table_path())
    string_db
    })
  
  backgroundV = reactive({
    data_list
    print(data_list)
    backgroundV = all_mapped()$STRING_id
    backgroundV
  })
  
  string_db = reactive({
    print('string_db')
    taxonomy_number = 9606
    file_path = paste0(shiny_data_path,data_name_collapse(),'_string_db.rds')
    #print(file_path)
    #if(file.exists(file_path) & input$re_run == F){
    #  result = readRDS(file_path)
    #}else{
      print('generate string_db')
      
      
      if(input$background == 'all_mapped'){
        string_db = STRINGdb$new(version="10", species=taxonomy_number, score_threshold=400, input_directory=table_path())
        
        backgroundV = backgroundV()
        backgroundV = backgroundV[!is.na(backgroundV)]
        string_db$set_background(backgroundV)
        
      }else{
        string_db = STRINGdb$new(version="10", species=taxonomy_number, score_threshold=400, input_directory=table_path())
        
      }
      #backgroundV
      #string_db = base_string_db()
      #taxonomy_number = 9606
      #backgroundV = all_mapped()$STRING_id
      #backgroundV
      #print(backgroundV)
      #print(length(backgroundV))
      #backgroundV = backgroundV()
      #backgroundV
      #print(str(string_db))
      #string_db$set_background(backgroundV)
      print(str(string_db))
      #string_db$load_all()
      result = string_db
      saveRDS(result,file_path)
    #}
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
  
  values <- reactiveValues(shouldShow_sn_select = FALSE, shouldShow_sn_select_link = FALSE, plot_name = '')
  
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
    venn_data_list = unlist(lapply(entry_list,function(x) c(paste0(x,' down'),paste0(x,' up'))))
    venn_data_list
    selectInput(inputId = 'enrichment_data_select',  # Drop down menu to select the producer and cultivar
                label = 'Select Venn Data',
                choices = venn_data_list,
                selected = venn_data_list,
                multiple = T)
  })
        
        venn_all_reactive = reactive({
          entry_list = input$venn_data_select
          #print(entry_list)
          mapped_ud = mapped_data()$mapped_ud
          mapped_ud = mapped_ud[mapped_ud$id %in% venn_gene_list_select(),]
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
          colour_list = venn_all_reactive()$colour_list
          plot_name = latex_filename_function(paste(names(plot_data_list),collapse='_'))
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
              values$plot_name = paste0(plot_name,'.pdf')
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
          int_select = int[1]
          if(input$venn_data_select_button == 'gene_list'){
            int_select = int
          }
          selectInput('venn_int','Select Venn Intersections',int,int_select,multiple = T)
        })
        
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
          paste(unique(v_list))
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
          m = common_melt_function(data_select_list, data_df_list(), sig_data_list(), data_df(), timeseries_list())
          
        })
        
        m_ts = reactive({
          timeseries_list = timeseries_list()
          m_ts = m()
          n = apply(m_ts, 1, function(x) ifelse(x['variable'] %in% names(timeseries_list),timeseries_list[[x['variable']]],NA))
          #View(n)
          m_ts$ts = n
          #m_ts = m[!is.na(m$ts),]
          View(m_ts)
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
      

      
    ##### ______gene ####
     
     output$g_boxplots_plots <- renderUI({
       gene_list = gene_list()
       gene_list = gene_list[order(gene_list)]
       makePlotContainers(gene_list, 'gplot')
     })
     observeEvent(input$gene_boxplot,{
       sample_list = input$data
       gene_list = gene_list()
       renderPlots(m(), sample_list, gene_list ,sample_path_line(),input, output, 'gplot')
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
       makePlotContainers(gene_list, 'gplot_ts')
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
       print(string_hits_list())
       enrichmentGO <- string_db$get_enrichment(string_hits_list(), category = "Process", methodMT = "fdr", iea = TRUE )
        print(enrichmentGO)
        enrichmentGO
       })
     
     sn_enrichment_table = renderDataTable({
       sn_enrichment()
     })
 
    ### TOPGO ####
     
     topGO_all = reactive({
       print('topGO_all')
       geneNames_all_file_path = paste0(table_path(),'/geneNames_all.rds')
       GO2gene_all_file_path = paste0(table_path(),'/GO2gene_all.rds')
       
       if(!file.exists(geneNames_all_file_path) | input$topGO_re_run == T){
          print('re running topGO_all')
          topGO_geneNames_all(all_mapped(),annot(),column_path())
       }
       #print(length(geneNames_all))
       
       print(paste('found file :',geneNames_all_file_path))
       geneNames_all = readRDS(geneNames_all_file_path)
       GO2gene_all = readRDS(GO2gene_all_file_path)
       topGO_all_list = list(geneNames_all = geneNames_all,GO2gene_all = GO2gene_all)
       
       topGO_all_list
     })
     
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
      path_line = paste0(shiny_image_path,latex_filename_function(paste(path_list,collapse='/')))
      print(path_line)
      })
    sample_path_line = reactive({
      path_line = paste0(shiny_image_path,data_name_collapse())
      create_dir_function(path_line)
      path_line
    })
    
    output$path_list_print = renderText(path_line())
    
    output$thesis_path_print = renderText({
      plot_name = values$plot_name
      paste0(sub('./',thesis_path_sub,path_line()),'/',plot_name)
      
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



