save_varaible_code = function(){
  
  
  
  #### COPY AND PAST THIS TO SAVE VARIABLE
  
  
  save_test = F
  if(save_test == T){
    variable_list = c('full_data')
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
  
  
  
  
}

save_variable_function = function(variable_list){
  print('save_variable_function')
  start = Sys.time()
  cmd_list = c()
  
  for(entry in variable_list){
    cmd = paste0("saveRDS(",entry,",'temp/",entry,".rds')")
    print(cmd)
    cmd_list = c(cmd_list,cmd)
    #eval(parse(text = cmd))
  }
  print(Sys.time() - start)
  
  print('   save_variable_function : done')
  
  return(cmd_list)
  
  ### Need to run outside of function
  for(cmd in cmd_list){
    eval(parse(text = cmd))
  }
  
  ### TO GET DATA BACK OUT ###
  #cmd_list = save_variable_function(variable_list)
  #lapply(cmd_list, function(cmd) eval(parse(text = cmd)))
  #save_input_function(input)
}

save_input_function = function(input){
  print('save_input_function')
  save_input = list()
  input_list = names(reactiveValuesToList(input))
  
  for(entry in input_list){
    cmd = paste0("save_input[['",entry,"']] = input$",entry)
    print(cmd)
    
    eval(parse(text = cmd))
  }
  #input = readRDS('temp/save_input.rds')
  print('   save_input_function : done')
  
  saveRDS(save_input,'temp/input.rds')
}

read_variable_function = function(variable_list){
  print('read_variable_function')
  cmd_list = c()
  variable_list = c(variable_list,'input')
  for(entry in variable_list){
    cmd = paste0(entry, " = readRDS('temp/",entry,".rds')")
    print(cmd)
    cmd_list = c(cmd_list,cmd)
    #eval(parse(text = cmd))
  }
  #cmd_list = read_variable_function(variable_list)
  #for(cmd in cmd_list){
  #  eval(parse(text = cmd))
  #}
  print('   read_variable_function : done')
  
  return(cmd_list)
}


test_table_ids_function = function(table_name){
  print('test_table_ids_function')
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
  print('   test_table_ids_function : done')
  
  return(paste(table_name,' : ',length(df$id),',    number of na = ',na_num,'  number of character(0) = ',char_num,'  number of CHARACTER(0)) = ',char_c_num, '  nchar > 0 = ',len_num))
}

run_enrichment_function = function(ontology,path_list,STRING_id_list,string_db,mapped_st,annot,sample_path_line,backgroundV,input){
  print('run_enrichment_function')
  ontology_list = input$select_enrichment
  
  if(input$enrichment_select == 'STRINGdb'){
    
    enrichment_path_line = enrichment_path_line_function(shiny_image_path,path_list,ontology,input$background)
  }
  if(input$enrichment_select == 'topGO'){
    enrichment_path_line = enrichment_path_line_function(shiny_image_path,path_list,ontology,NULL)
    venn_path_line = paste(c(shiny_image_path,path_list),collapse = '/')
    venn_path_line = latex_filename_function(venn_path_line)
  }
  file_name_prefix = paste0(input$taxonomy,'_')
  if(input$iea == 'TRUE'){
    file_name_prefix = paste0(input$taxonomy,'_iea_')
    
  }
  file_name = paste0(file_name_prefix,input$enrichment_select,'.rds')
  
  file_path = paste(enrichment_path_line,file_name,sep = '/')
  #print(file_path)
  file_hit = 0
  if(file.exists(file_path) & input$enrich_re_run == FALSE){
   #print('file exists')
    #file_hit = 1
    file_hit = tryCatch({
      enrichment_GO = readRDS(file_path)
      #print(dim(enrichment_GO))
      file_hit = 1
    }, error = function(e) {file_hit = 0})
    #print(file_hit)
  }
  #print(file_hit)
  if(file_hit == 0){
    #print('re run enrichment')
    hits = STRING_id_list
    if(input$enrichment_select == 'STRINGdb'){
      #print('STRINGdb')
      enrichment_GO <- string_db$get_enrichment(hits, category = ontology, methodMT = input$select_sn_MT, iea = FALSE )
      enrichment_GO$pvalue_fdr = as.numeric(enrichment_GO$pvalue_fdr)
      
      saveRDS(enrichment_GO,file_path)
      
    }
    if(input$enrichment_select == 'topGO'){
      #print('topGO')
      all_mapped = mapped_st
      #print(dim(all_mapped))
      if(input$iea == TRUE){
        annot_file_path = paste0(shiny_data_path,input$taxonomy,'_annot_list_all.rds')
      }else{
        annot_file_path = paste0(shiny_data_path,input$taxonomy,'_annot_list_no_EIA.rds')
      }
      #if(!file.exists(annot_file_path)){
      #  annot = annot()
      #}
      annot_list = readRDS(annot_file_path)
      #topGO_all_list = topGO_all
      sample_path = sample_path_line
      #print(sample_path)
      #saveRDS(sample_path,'temp/path.rds')
      #saveRDS(input,'temp/input.rds')
      #saveRDS(all_mapped,'temp/all_mapped.rds')
      #saveRDS(annot,'temp/annot.rds')
      re_run = input$topGO_re_run
      
      #path = readRDS('temp/enrichment_path.rds')
      #input = readRDS('temp/input.rds')
      #all_mapped = readRDS('temp/all_mapped.rds')
      ##annot = readRDS('temp/annot.rds')
      #re_run = T
      #print(sample_path)
      geneNames_all_path = paste0(sample_path,'/',file_name_prefix,'geneNames_all.rds')
      GO2gene_all_path = paste0(sample_path,'/',file_name_prefix,'GO2gene_all.rds')
      gene2GO_all_path = paste0(sample_path,'/',file_name_prefix,'gene2GO_all.rds')
      geneNames_path_list = list(geneNames_all_path = geneNames_all_path, GO2gene_all_path = GO2gene_all_path, gene2GO_all_path = gene2GO_all_path)
      topGO_geneNames_all(backgroundV,annot_list,geneNames_path_list,file_name_prefix,input$background_re_run)
      
      #print(geneNames_all_path)
      geneNames_all = readRDS(geneNames_all_path)
      #print(gene2GO_all_path)
      gene2GO_all = readRDS(gene2GO_all_path)
      
      ont = 'CC'
      ont = paste(enrichment_abreviation_list[ontology])
      ont
      ens_list = all_mapped$STRING_id
      #print(length(ens_list))
      data_mapped = all_mapped[all_mapped$STRING_id %in% hits,]
      geneList_path = paste0(venn_path_line,'/topGO_geneList.rds')
      #sig_geneList_path = paste0(sample_path_line,'/sig_topGO_geneList.rds')
      
      #print(geneList_path)
      if(!file.exists(geneList_path) | input$enrich_re_run == TRUE){
        geneList = topGO_mapping(data_mapped,geneNames_all,annot_list)
        saveRDS(geneList, geneList_path)
        print('saveRDS')
        #sig_geneList = topGO_mapping(all_mapped,geneNames_all,annot_list)
        #saveRDS(sig_geneList, geneList_path)
        #print('saveRDS')
        
      }else{
        print('readRDS')
        geneList = readRDS(geneList_path)
        #sig_geneList = readRDS(sig_geneList_path)
        #print('found')
        
      }
      GOdata_path = paste0(enrichment_path_line,'/',file_name_prefix,'_topGO_GOdata.rds')
      #sig_GOdata_path = paste0(sample_path_line,'/',file_name_prefix,'_',ontology,'_sig_topGO_GOdata.rds')
      #print(GOdata_path)
      #print(sig_GOdata_path)
      if(!file.exists(GOdata_path) | input$enrich_re_run == TRUE){
        
        GOdata = topGO_enrichment(ont,geneList,gene2GO_all)
        saveRDS(GOdata,GOdata_path)
        print('saveRDS')
        
        #sig_GOdata = topGO_enrichment(ont,sig_geneList,gene2GO_all)
        #saveRDS(GOdata,paste0(sig_GOdata_path))
        #print('saveRDS')
      }else{
        print('found')
        GOdata = readRDS(GOdata_path)
      }
      #print(venn_path_line)
      df = topGO_result(GOdata,venn_path_line,'topGO')
      
      df$term_id = df$GO.ID
      df$proteins = as.numeric(df$Annotated)
      df$hits = as.numeric(df$Significant)
      df$pvalue = as.numeric(df$classicFisher)
      df$pvalue_fdr = as.numeric(df[,input$select_sn_MT])
      df$term_description = df$Term
      #print(file_path)
      
      saveRDS(df ,file_path)
      enrichment_GO = df
      
      #print(enrichment_GO)
    }
    #enrichment_GO <- string_db()$get_enrichment(hits, category = input$select_enrichment, methodMT = input$select_sn_MT)
    #if(input$save_plot == T){
    #  saveRDS(enrichment_GO,file_path)
    #}
  }
  #enrichment_GO <- string_db()$get_enrichment(hits, category = input$select_enrichment)
  #print(file_path)
  #enrichment_GO
  #enrichment_GO$pvalue_fdr = as.numeric(enrichment_GO$pvalue_fdr)
  print('   run_enrichment_function : done')
  
  return(enrichment_GO)
}


path_line_function = function(path_list){
  print('path_line_function')
  #path_list = c(shiny_image_path,path_list)
  path_entry = c()
  for(entry in path_list){
    path_entry = c(path_entry,entry)
    path_line = latex_filename_function(paste(path_entry,collapse = '/'))
    create_dir_function(path_line)
  }
  print('   path_line_function :  done')
  
  return(path_line)
}

enrichment_path_line_function = function(shiny_image_path,path_list,enrichment,background,save_plot){
  print('enrichment_path_line_function')
  #path_list = gene_list_select_list()$path_list
  path_list = c(path_list,'enrichment',enrichment,background)
  path_entry = c()
  
  
  for(entry in path_list){
    path_entry = c(path_entry,entry)
    path_entry_line = latex_filename_function(paste(path_entry,collapse = '/'))
    create_dir_function(paste0(shiny_image_path,path_entry_line))
  }
  
  path_line = paste0(shiny_image_path,latex_filename_function(paste(path_list,collapse='/')))
  #print(path_line)
  print('   enrichment_path_line_function : done')
  return(path_line)
  
}

string_hits_function = function(enrichment_table,GO_term,mapped_data,annot){
  print('string_hits_function')
  #if(values$sn == 1){

  #mapped_data = mapped_st()
  #GO_term = input$term
  GO_id = enrichment_table$term_id[enrichment_table$term_descriptio == GO_term]
  GO_id
  string_GO_members = annot$STRING_id[annot$term_id == GO_id]
  string_GO_members
  GO_members = string_GO_members
  STRING_hits = GO_members[GO_members %in% mapped_data$STRING_id]

  STRING_hits
  print('   string_hits_function : done')
  
  return(STRING_hits)
}

topGO_geneNames_all = function(ens_list,annot_list,path_list,file_name_prefix,re_run = FALSE){
  print('topGO_geneNames_all')
  #print(path_list$gene2GO_all_path)
  if(!file.exists(path_list$gene2GO_all_path) | re_run == TRUE){
    print('re-running topGO enrichment')
    #print(length(ens_list))
    ens_list = ens_list[!is.na(ens_list)]
    GO_terms = annot_list[ens_list]
    
    gene2GO_all = GO_terms
    #print(path_list$gene2GO_all_path)
    saveRDS(gene2GO_all, path_list$gene2GO_all_path)
    GO2gene_all = inverseList(gene2GO_all)
    #print(path_list$GO2gene_all_path)
    saveRDS(GO2gene_all, path_list$GO2gene_all_path)
    geneNames_all <- names(gene2GO_all)
    #print(path_list$geneNames_all_path)
    saveRDS(geneNames_all, path_list$geneNames_all_path)
  }else{
    print('found')
  }
  print('   topGO_geneNames_all : done')
  
}

topGO_mapping = function(mapped,geneNames_all, annot_list){
  print('topGO_mapping')
  ens_list = mapped[,"STRING_id"]
  #GO_terms = list()
  #for(ens in ens_list){
  #  GO_terms[[ens]] = annot$term_id[annot$STRING_id == ens]
  #}
  GO_terms = annot_list[ens_list]
  gene2GO = GO_terms
  myInterestingGenes <- names(gene2GO)
  geneList <- factor(as.integer(geneNames_all %in% myInterestingGenes))
  length(geneList)
  names(geneList) <- geneNames_all
  print('   topGO_mapping : done')
  return(geneList)
}

new_col_mix = function(col1,col2){
  print('new_col_mix_function')
  if(is.na(col1)){
    col = col2
  }else{
    col = hex(mixcolor(0.5,hex2RGB(col1),hex2RGB(col2)))
  }
  print('   new_col_mix_function : done')
  
  return(col)
}

col_select_function = function(num,up_col,down_col,col){
  print('col_select_function')
  #num = 1
  #print(col)
  num = as.numeric(num)
  #print(num)
  if(!is.na(num)){
    if(num > 0){
      col = new_col_mix(col,up_col)
    }
    if(num < 0){
      col = new_col_mix(col,down_col)
    }
  }
  print('   col_select_function : done')
  
  return(col)
  #print(col)
}

label_select_function = function(num,name,label){
  print('label_select_function')
  #num = 1
  #print(col)
  num = as.numeric(num)
  #print(num)
  if(!is.na(num) & num != 0){
    if(num > 0){
      direction = 'up'
      #label = paste0(label,' & ',name,' up')
    }
    if(num < 0){
      direction = 'down'
      #label = paste0(label,' & ',name,' down')
    }
    if(is.na(label)){
      label = paste(name,direction)
    }else{
      label = paste(label,'&',name,direction)
    }
  }
  print('   label_select_function : done')
  
  return(label)
  #print(col)
}

string_id_mapping = function(data,id){
  print('string_id_mapping')
  string_data = string_db$map(data, id, removeUnmappedRows = TRUE)
  print('   string_id_mapping : done')
  return(string_data)
}

prep_tf_files = function(){
  print('prep_tf_files_function')
  AnimalTFDB_path = paste('./database/AnimalTFDB',sep='/')
  #print(AnimalTFDB_path)
  file_list = list.files(AnimalTFDB_path)
  file_list
  tf_path = create_dir_function(paste(column_path,'AnimalTFDB',sep='/'))
  file_list = file_list[grep('.txt',file_list)]
  file_list
  file_name = file_list[1]
  
  for(file_name in file_list){
    #print(file_name)
    if(grepl('.txt',file_name)){
      tf_file_path = create_dir_function(paste(tf_path,gsub('.txt','',file_name),sep='/'))
      #print(file_name)
      cmd = paste("temp_table = read.table(file = '",paste(AnimalTFDB_path,file_name,sep='/'),"',sep='\t',header=TRUE)",sep='')
      #print(cmd)
      eval(parse(text=cmd))
      head(temp_table)
      dim(temp_table)
      sn_temp_table = string_id_mapping(temp_table,'Ensembl.ID')
      
      head(sn_temp_table)
      dim(sn_temp_table)
      saveRDS(sn_temp_table,paste(AnimalTFDB_path,sub('.txt','.rds',file_name),sep='/'))
    }
  }
  print('   prep_tf_files_function : done')
}


common_mapped_function = function(shiny_data_path,sig_data_list,data_select_list,start_col,string_db,data_df_list,data_name_collapse,removed_list){
  print('common_mapped_function')
  variable_list = c('shiny_data_path',"sig_data_list","data_select_list","start_col",'string_db',"data_df_list","data_name_collapse","removed_list")
  
  test = F
  if(test == T){
    cmd_list = save_variable_function(variable_list)
    for(cmd in cmd_list){
      #print(cmd)
      eval(parse(text = cmd))
    }
    #save_input_function(input)
  }
  read_test = 'F'
  if(read_test == T){
    cmd_list = read_variable_function(variable_list)
    for(cmd in cmd_list){
      #print(cmd)
      eval(parse(text = cmd))
    }
  }
  #print(variable_list)
  #data_name_collapse = paste(data_select_list,collapse = '_')
  
 # print('joining selected datasets using commmon_mapped_function, please be patient...')
  d_num = length(data_select_list) * 2
  
  colour_wheel = setColors(start_col, d_num)
  #print(data_select_list)
  #print(data_list)
  print('readRDS - common_mapped_function')
  entry = data_select_list[1]
  
  cmd = paste0(entry,"_sig_mapped = sig_data_list[['",entry,"']]")
  eval(parse(text = cmd))
  cmd = paste0(entry,"_all_mapped = data_df_list[['",entry,"']]")
  eval(parse(text = cmd))
  
  cmd = paste0(entry,"_all_mapped$data = '",entry,"'")
  eval(parse(text = cmd))
  
  cmd = paste0(entry,"_sig_mapped$data = '",entry,"'")
  eval(parse(text = cmd))
  entry = data_select_list[2]
  entry
  if(length(data_select_list) > 1){
    for(entry in data_select_list[2:length(data_select_list)]){ 
      cmd = paste0(entry,"_all_mapped = data_df_list[['",entry,"']]")
      eval(parse(text = cmd))
      
      cmd = paste0(entry,"_all_mapped$data = '",entry,"'")
      eval(parse(text = cmd))
      
      cmd = paste0(entry,"_sig_mapped = sig_data_list[['",entry,"']]")
      eval(parse(text = cmd))
      
      cmd = paste0(entry,"_sig_mapped$data = '",entry,"'")
      eval(parse(text = cmd))
    } # generate dataframes from rds files
  }
  
  print('reshape')
  #column_names = c('data','id','STRING_id','mean')
  if(length(data_select_list) > 0){
    
    cmd = paste0('all_data = ',data_select_list[1],'_all_mapped')
    eval(parse(text = cmd))
    
    cmd = paste0('data = ',data_select_list[1],'_sig_mapped')
    eval(parse(text = cmd))
    
    if('mean' %in% colnames(data)){
      column_names = c('id','STRING_id','mean','Accession','id_acc')
    }else(
      column_names = c('id','STRING_id','slope','Accession','id_acc')
    )
    
    #print(colnames(data))
    #print(column_names)
    
    all_sub_data = all_data[,column_names]
    colnames(all_sub_data) =  c('id','STRING_id',data_select_list[1],paste(data_select_list[1],'Accession',sep='_'),'id_acc')
    
    sub_data = data[,column_names]
    colnames(sub_data) =  c('id','STRING_id',data_select_list[1],paste(data_select_list[1],'Accession',sep='_'),'id_acc')
    
    
    
    all_mapped_combined = all_sub_data
    mapped_combined = sub_data
    if(length(data_select_list) > 1){
      for(entry in data_select_list[2:length(data_select_list)]){
        
        cmd = paste0('all_data = ',entry,'_all_mapped')
        eval(parse(text = cmd))
        cmd = paste0('data = ',entry,'_sig_mapped')
        eval(parse(text = cmd))
        
        
        if('mean' %in% colnames(data)){
          column_names = c('id','STRING_id','mean','Accession','id_acc')
        }else(
          column_names = c('id','STRING_id','slope','Accession','id_acc')
        )
        
        all_sub_data = all_data[,column_names]
        colnames(all_sub_data) =  c('id','STRING_id',entry,paste(entry,'Accession',sep='_'),'id_acc')
        
        sub_data = data[,column_names]
        colnames(sub_data) =  c('id','STRING_id',entry,paste(entry,'Accession',sep='_'),'id_acc')
        
        all_mapped_combined = merge(all_mapped_combined,all_sub_data,by = c('id','id_acc','STRING_id'), all = TRUE)
        mapped_combined = merge(mapped_combined,sub_data,by = c('id','id_acc','STRING_id'), all = TRUE)
        
      }
    }
  } # combine all the data frames togther into a long dataset
  #colnames(SILAC_sub) =  c('data','id','STRING_id',entry,'Accession','id_acc')
  #colnames(GE_sub) =  c('data','id','STRING_id',data_select_list[1],'Accession','id_acc')
  
  #merge_sub = merge(GE_sub,sub_data,by = c('id','id_acc','STRING_id'),
  #by.y = c('id','id_acc'),
  # all = TRUE)
  #head(merge_sub)
  #merge_sub[merge_sub$id == 'NEFL',]
  #merge_sub[merge_sub$id == 'HLA',]
  
  for(entry in data_select_list){
    all_mapped_combined[,entry][is.na(all_mapped_combined[,entry])] = 0
    mapped_combined[,entry][is.na(mapped_combined[,entry])] = 0
  }
  all_mapped_long_w = all_mapped_combined
  mapped_long_w = mapped_combined
  
  #removed_list = c('SEP')
  all_gene_list = all_mapped_long_w$id
  gene_list = mapped_long_w$id
  
  
  all_mapped_long_w = all_mapped_long_w[!is.na(all_mapped_long_w$id),]
  all_mapped_long_w = all_mapped_long_w[!all_mapped_long_w$id %in% removed_list,]
  mapped_long_w = mapped_long_w[!is.na(mapped_long_w$id),]
  mapped_long_w = mapped_long_w[!mapped_long_w$id %in% removed_list,]
  
  head(mapped_combined)
  
  i = grep('STRING_id',colnames(mapped_long_w))
  i
  
  
  check_dfs = F
  if(check_dfs == T){
    mapped_long = melt(mapped_combined, id.vars = c('id','STRING_id','data'))
    for(entry in data_select_list){
      cmd = paste0('original_data = ',entry,'_sig_mapped')
      eval(parse(text = cmd))
      long = (mapped_long[mapped_long$data == entry,])
      diff_result = setdiff(long$id,original_data$id)
      #print(diff_result)
      diff_result = setdiff(original_data$id,long$id)
      #print(diff_result)
    }
  }
  
  
  
  
  
  #ESC_down = '#ffff00' # 'yellow'
  #ESC_up = '#0000FF' #'blue'
  #GE_down = '#98FB98' # palegreen'
  #GE_up = '#ee799f' # palevioletred
  #SILAC_down = '#00ff00' #'green'
  #SILAC_up = '#ff0000' # red'
  
  
  print('colour_list')
  colour_list = c()
  for(i in c(1:length(data_select_list))){
    colour_list[paste0(data_select_list[i],' down')] = colour_wheel[i]
    colour_list[paste0(data_select_list[i],' up')] = complementary(colour_wheel[i],plot = FALSE)[2]
  }
  colour_list
  colour_file_name = paste0(shiny_data_path,'colour_list_',data_name_collapse,'.rds')
  saveRDS(colour_list,colour_file_name)
  
  
  print('colour label')
  
  all_mapped_long_w$col = NA
  all_mapped_long_w$label = NA
  
  mapped_long_w$col = NA
  mapped_long_w$label = NA
  #apply(mapped_long_w, 1, function(x) print(x['col']))
  for(entry in data_select_list){
    mapped_long_w$col = apply(mapped_long_w, 1, function(x) col_select_function(x[entry],colour_list[paste(entry,'up')],colour_list[paste(entry,'down')],x['col']))
    mapped_long_w$label = apply(mapped_long_w, 1, function(x) label_select_function(x[entry],entry,x['label']))
  }
  
  x = all_mapped_long_w[1,]
  all_mapped_long_w$col = sapply(all_mapped_long_w$id, function(x) mapped_long_w$col[match(x['id'], mapped_long_w$id)])
  all_mapped_long_w$label = sapply(all_mapped_long_w$label, function(x) mapped_long_w$col[match(x['id'], mapped_long_w$label)])
  
  
  test_entry_list = setNames(mapped_long_w$col,mapped_long_w$label)
  unique(names(test_entry_list))
  test_entry_list = test_entry_list[unique(names(test_entry_list))]
  entry_list = test_entry_list[order(names(test_entry_list))]
  length(entry_list)
  
  
  
  #pdf('legend.pdf')
  #plot(c(1,30),c(30,1))
  #legend(1,30,legend = names(entry_list),fill = paste(entry_list),cex = 0.7)
  #dev.off()
  #saveRDS(entry_list,'legend_list.rds')
  
  print('payload id')
  #print(mapped_long_w$col)
  payload_id = string_db$post_payload(mapped_long_w$STRING_id, colors=mapped_long_w$col)
  
  #all_payload_id = string_db$post_payload(all_mapped_long_w$STRING_id, colors=all_mapped_long_w$col)
  
  #rds_file_name = paste0(shiny_data_path,'payload_id_',data_name_collapse,'.rds')
  #saveRDS(mapped_ud,rds_file_name)
  
  
  mapped_ud = mapped_long_w
  for(entry in data_select_list){
    mapped_ud[,paste0(entry,' up')] = sapply(mapped_ud[,entry], function(x) ifelse(x > 0,x,0))
    mapped_ud[,paste0(entry,' down')] = sapply(mapped_ud[,entry], function(x) ifelse(x < 0,x,0))
    mapped_ud[,entry] = NULL
  }
  
  #saveRDS(mapped_ud,paste0(shiny_data_path,'mapped_ud.rds')
  
  rds_ud_file_name = paste0(shiny_data_path,'sig_mapped_ud_',data_name_collapse,'.rds')
  saveRDS(mapped_ud,rds_ud_file_name)
  ud_file_name = paste0(shiny_data_path,'sig_mapped_ud_',data_name_collapse,'.txt')
  write.table(mapped_ud, file = ud_file_name,sep = '\t',col.names = NA)
  
  print('saveRDS')
  #saveRDS(mapped_long_w,shiny_data_path,'common_sig_mapped.rds')
  #write.table(mapped_long_w, file = './GO_data/common_sig_mapped.txt',sep = '\t',col.names = NA)
  #saveRDS(payload_id,'./GO_data/common_sig_mapped_payload_id.rds')
  rds_file_name = paste0(shiny_data_path,'common_sig_mapped_',data_name_collapse,'.rds')
  print(rds_file_name)
  saveRDS(mapped_long_w,rds_file_name)
  all_rds_file_name = paste0(shiny_data_path,'common_all_mapped_',data_name_collapse,'.rds')
  print(all_rds_file_name)
  saveRDS(all_mapped_long_w,all_rds_file_name)
  
  
  payload_rds_file_name = paste0(shiny_data_path,'common_sig_mapped_payload_id_',data_name_collapse,'.rds')
  saveRDS(payload_id,payload_rds_file_name)
  #payload_rds_file_name = paste0(shiny_data_path,'common_all_mapped_payload_id_',data_name_collapse,'.rds')
  #saveRDS(all_payload_id,payload_rds_file_name)
  
  entry_list_file_name = paste0(shiny_data_path,'entry_list_',data_name_collapse,'.rds')
  saveRDS(entry_list,entry_list_file_name)
  
  id_list = list()
  id_list[mapped_long_w$STRING_id] = mapped_long_w$id
  id_file_list = paste0(shiny_data_path,'id_list_',data_name_collapse,'.rds')
  saveRDS(id_list,id_file_list)
  
  all_id_list = list()
  all_id_list[all_mapped_long_w$STRING_id] = all_mapped_long_w$id
  all_id_file_list = paste0(shiny_data_path,'all_id_list_',data_name_collapse,'.rds')
  saveRDS(all_id_list,id_file_list)
  
  
  
  removed_file_list = paste0(shiny_data_path,'removed_list_',data_name_collapse,'.rds')
  saveRDS(removed_list,removed_file_list)
  
  gene_file_list = paste0(shiny_data_path,'gene_list_',data_name_collapse,'.rds')
  saveRDS(gene_list,gene_file_list)
  all_gene_file_list = paste0(shiny_data_path,'all_gene_list_',data_name_collapse,'.rds')
  saveRDS(all_gene_list,all_gene_file_list)
  
  
  results_list = list(mapped_data = mapped_long_w,
                      all_mapped_data = all_mapped_long_w,
                      mapped_ud = mapped_ud,
                      payload_id = payload_id,
                      #all_payload_id = all_payload_id,
                      entry_list = entry_list,
                      colour_list = colour_list,
                      id_list = id_list,
                      all_id_list = all_id_list,
                      removed_list = removed_list,
                      gene_line = gene_list,
                      all_gene_list = all_gene_list)
  print('   common_mapped_function : done')
  
  return(results_list)
}

common_melt_function = function(data_select_list, data_df_list, sig_data_list, data_df, timeseries_list){
  print('common_melt_function')
  test_common_melt = F
  if(test_common_melt == T){
    data_select_list = input$data
    data_df_list = data_df_list()
    sig_data_list = sig_data_list()
    data_df = data_df()
    timeseries_list = timeseries_list()
  }
  
  #data_select_list = data_select_list[data_select_list != 'ESC']
  data_select_list
  data_select_list[1]
  data = data_df_list[[data_select_list[1]]]
  head(data)
  #cmd = paste0('data = ',data_select_list[1],'_mapped_all')
  #print(cmd)
  #eval(parse(text = cmd))
  #cmd = paste0('sig_data = ',data_select_list[1],'_mapped_all')
  sig_data = sig_data_list[[data_select_list[1]]]
  head(sig_data)
  # if(data_select_list[1] %in% names(timeseries_list)){
  #   sig_data$ts = data_name
  # }else{
  #   sig_data$ts = NA
  # }
  #cmd = paste0("sig_data = readRDS('sig_mapped.",data_df_list[[data_select_list[1]]],".rds')")
  #print(cmd)
  #eval(parse(text = cmd))
  
  dname = data_df[data_select_list[1],'data']
  dname
  #dname = 'mean'
  #if(!'mean' %in% colnames(data)){
  #  dname = 'slope'
  #}
  dname
  data$type = dname
  data$sig = NA
  up_id = sig_data$id[sig_data[,dname] > 0]
  data$sig[data$id %in% up_id] = 1
  down_id = sig_data$id[sig_data[,dname] < 0]
  data$sig[data$id %in% down_id] = -1
  #data$sig = factor(sapply(data$id, function(x){
  #  y = ifelse(x %in% sig_data$id,ifelse(as.numeric(sig_data[sig_data$id == x,dname]) > 0,1,-1),NA)
  #  y
  #} ))
  head(data)
  data$data = data_select_list[1]
  #print(colnames(data))
  col_index = c(2:(grep(dname,colnames(data))-1))
  col_index
  #print(colnames(data)[col_index])
  if(data_select_list[1] == 'ESC'){
    col_index = c(12:146)
  }
  #print(col_index)
  m_colnames = c("id",'id_acc','Accession',dname,"BH","STRING_id","data","variable","value",'type','sig')
  m = melt(data,measure.vars = col_index)
  #print(colnames(m))
  head(m)
  m = m[,m_colnames]
  m = m[!is.na(m$value),]
  #head(m[!is.na(m$sig),])
  if(length(data_select_list) > 1){
    for(data_name in data_select_list[c(2:length(data_select_list))]){
      #print(data_name)
      data = data_df_list[[data_name]]
      sig_data = sig_data_list[[data_name]]
      # if(data_select_list[data_name] %in% names(timeseries_list)){
      #   sig_data$ts = data_name
      # }else{
      #   sig_data$ts = NA
      # }
      
      dname = data_df[data_name,'data']
      data$type = dname
      
      data$data = data_name
      #cmd = paste0('data = ',data_name,'_mapped_all')
      #print(cmd)
      #eval(parse(text = cmd))
      #cmd = paste0("sig_data = readRDS('sig_mapped.",data_list[data_name],".rds')")
      #print(cmd)
      #eval(parse(text = cmd))
      #dname = 'mean'
      #if(!'mean' %in% colnames(data)){
      #  dname = 'slope'
      #}
      data$sig = NA
      up_id = sig_data$id[sig_data[,dname] > 0]
      data$sig[data$id %in% up_id] = 1
      down_id = sig_data$id[sig_data[,dname] < 0]
      data$sig[data$id %in% down_id] = -1
      #data$sig = factor(sapply(data$id, function(x){
      #  y = ifelse(x %in% sig_data$id,ifelse(as.numeric(sig_data[sig_data$id == x,dname]) > 0,1,-1),character(0))
      #  y
      #}))
      #print(dim(data))
      
      
      #print(colnames(data))
      
      col_index = c(2:(grep(dname,colnames(data))-1))
      col_index
      if(data_name == 'ESC'){
        col_index = c(12:146)
      }
      n = melt(data,measure.vars = col_index)
      #print(dim(n))
      n_colnames = c("id",'id_acc','Accession',dname,"BH","STRING_id","data","variable","value",'type','sig')
      n = n[,n_colnames]
      colnames(n) = m_colnames
      
      m = rbind(m,n)
      
    }
  }
  unique(m$data)
  m$sig = factor(m$sig)
  dim(m)
  #View(m)
  print('   common_melt_function : done')
  
  return(m)
}

makePlotContainers <- function(gene_list, prefix="gplot") {
  print('makePlotContainers')
  lst <- lapply(gene_list, function(gene) {
    output_name = paste(gene,prefix,sep = '_')
    print(output_name)
    plotOutput(paste(gene,prefix,sep = '_'),height = 500)
  })
  
  do.call(tagList, lst)
}

sig_colour_function = function(sub_m){
  print('sig_colour_function')
  col = c()
  if(-1 %in% sub_m$sig){
    col = c(col,'green')
  }
  if(1 %in% sub_m$sig){
    col = c(col,'red')
  }
  if(length(col) == 0){
    col = c('white')
  }
  print('   sig_colour_function : done')
  
  return(col)
}

renderImages = function(gene_list,boxplot_path,input,output,prefix){
  print('renderImage_function')
  for(gene in gene_list){
    local({
      output_name = paste(gene,prefix,sep = '_')
      plot_path = paste0(boxplot_path,'/',gene,'.png')
      #print(plot_path)
      output[[output_name]] = renderImage({
        print('renderImage')
        print(plot_path)
        list(src = plot_path)
      }, deleteFile = FALSE)
    })
  }
}

renderPlots_facets = function(m, data_df, sample_list, gene_list, sample_path_line, input, output, prefix = 'gplot'){
  print('renderPlots_function')
  save_test = F
  
  if(save_test == T){
    variable_list = c('m','data_df','sample_list','gene_list','sample_path_line','prefix')
    
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
  # if(save_test == T){
  #   variable_list = c('m','data_df','sample_list','gene_list','sample_path_line','prefix')
  #   cmd_list = save_variable_function(variable_list)
  #   for(cmd in cmd_list){
  #     print(cmd)
  #     eval(parse(text = cmd))
  #   }
  #   #lapply(cmd_list, function(cmd) eval(parse(text = cmd)))
  #   save_input_function(input)
  # }
  # read_test = F
  # if(read_test == T){
  #   variable_list = c('m','data_df','sample_list','gene_list','sample_path_line','prefix')
  #   
  #   cmd_list = read_variable_function(variable_list)
  #   
  #   for(cmd in cmd_list){
  #     print(cmd)
  #     eval(parse(text = cmd))
  #   }
  #   #sapply(cmd_list, function(cmd) eval(parse(text = cmd)))
  #   
  #   input = readRDS('temp/save_input.rds')
  # }
  
  boxplot_path = sample_path_line
  
  
  #print('gene_list')
  #print(gene_list)
  gene = gene_list[1]
  gene
  for(gene in gene_list){
    local({
      
      
      #print('gene')
      #print(gene)
      
      output_name = paste(gene,prefix,sep = '_')
      name = paste(gene)
      plot_path = paste0(sample_path_line,'/',gene,'.png')
      #print(plot_path)
      #print(file.exists(plot_path))
      
      print('running ')
      print(output_name)
      sub_m = m[m$id == gene,]
      sub_m = sub_m[sub_m$type == 'mean',]
      sub_m = sub_m[!is.na(sub_m$id),]
      sub_m$Accession[is.na(sub_m$Accession)] = ''
      #View(sub_m)
      #sample_list = sample_list[sample_list %in% unique(sub_m$data)]
      
      
      y_max = max(c(sub_m$value,(2*as.numeric(data_df[sample_list,'sd_cutoff']))),na.rm = T)
      y_min = min(c(sub_m$value,(-2*as.numeric(data_df[sample_list,'sd_cutoff']))),na.rm = T)
      y_max_max = max(y_max,abs(y_min))
      y_max_max = y_max_max + (y_max_max*0.1)
      #View(sub_m)
      #print(dim(sub_m))
      if(dim(sub_m)[1] > 0){
        #run = T
        #print(run)
        file_test = file.exists(plot_path)
        #print(file_test)
        #print(input$re_run_boxplots)
        if(file_test == FALSE | input$re_run_boxplots == TRUE){
          #col = sig_colour_function(sub_m)
          col = c('green','red')
          #col = c('red','green')
          #output[[output_name]] = renderPlot({
          
          id_acc_list = unique(sub_m$id_acc)
          id_acc_list = id_acc_list[!is.na(id_acc_list)]
          accession_list = unique(sub_m$Accession)
          accession_list_NA = accession_list[accession_list != '']
          #print(accession_list_NA)
          accession_list = accession_list[order(accession_list)]
          #print(id_acc_list)
          #if('boxplot'%in% input$boxplot_type_select){
          if(length(id_acc_list) > 1 & length(accession_list_NA) > 1){
            
            
            p = ggplot(data = sub_m)
            if('boxplot' %in% input$boxplot_type_select){
              
              p = p + geom_boxplot(position=position_dodge(width=0.9),size = 1, aes(x = data,y = value,fill = sig,col = Accession))
            }
            p
            accession_list = unique(sub_m$Accession)
            accession_list = accession_list[order(accession_list)]
            accession_list
            p = p + scale_color_manual(name = 'Isoforms', breaks = accession_list, values = c('black',cm.colors(length(accession_list)-1)))
            p
            if('dotplot' %in% input$boxplot_type_select){
              p = p + geom_dotplot(data = sub_m,binaxis='y', stackdir='center',fill = 'black',dotsize = input$boxplot_dotplot_size, position = 'dodge', aes(x = data,y = value,fill = sig,col = Accession))
            }
            p
          }else{
            p = ggplot()
            if('boxplot' %in% input$boxplot_type_select){
              
              p = p + geom_boxplot(data = sub_m, aes(x = data,y = value,fill = sig))
            }
            if('dotplot' %in% input$boxplot_type_select){
              
              p = p + geom_dotplot(data = sub_m, aes(x = data,y = value,fill = sig),binaxis='y', stackdir='center',fill = 'black',dotsize = input$boxplot_dotplot_size)
            }
            
          }
          p
          # }
          if('violin' %in% input$boxplot_type_select){
            p = p + geom_violin(data = sub_m, aes(x = data,y = value,fill = sig)) 
          }
          
          simple_plot = 'F'
          
          #p = p + geom_dotplot(binaxis='y', stackdir='center',fill = 'black',dotsize = 0.5)
          #p = p + geom_dotplot(binaxis='y', stackdir='center',fill = 'black',dotsize = 0.5)
          #p = p + geom_jitter(colour = 'black')
          
          ### plot formatting ###
          p = p + labs(x = '', y =  'log2(ratio)') +
            geom_hline(yintercept = 0) +
            #scale_x_discrete(limits = sample_list) + 
            ggtitle(paste(name))  
          p
          #scale_fill_manual(name = 'Significantly', breaks = c(-1,1),values = col,labels = c('downregulated','upregulated'))
          # need this to work
          #sig_colour = c('green','red')
          sig_name = c('-1','1')
          #levels(sig_name) = c(1,0)
          #names(sig_colour) = sig_name
          #p = p + scale_fill_manual(name = 'Significantly', values = sig_colour,labels = c('downregulated','upregulated'),drop = FALSE)
          #p
          p = p + scale_fill_manual(name = 'Significantly', breaks = c('-1','1'),values = c('green','red'),labels = c('downregulated','upregulated'),drop = FALSE)
          
          p
          if(simple_plot == 'F'){
            p = p + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5, size = input$boxplot_title_size), 
                          axis.title = element_text(size = input$boxplot_text_size),
                          #axis.text.x = element_text(size = input$boxplot_x_axis_size,face = 'bold'),
                          #axis.text.x = element_text(size = input$boxplot_x_axis_size,face = 'bold'),
                          axis.text.x = element_blank(),
                          axis.ticks.x=element_blank(),
                          axis.text.y = element_text(size = input$boxplot_y_axis_size),
                          strip.text.x = element_text(size = input$boxplot_x_axis_size,face = 'bold'))
            #pdf(paste('/Users/sgarnett/Documents/Doctorate/Thesis/Thesis_Pictures/MQ_Gui/marker_lists/shiny_gplots//',name,'.pdf'))
            p
            s_coord = 0.5
            t_coord = 1
            p_value_list = c()
            sd_list = c()
            lim_list = c()
            sample = sample_list[1]
            sample
            
            for(sample in sample_list){
              #print(sample)
              values = sub_m$value[sub_m$data == sample]
              values
              lim_value = max(c(abs(max(values,na.rm = T)),abs(min(values,na.rm=T))))
              lim_value
              lim_list = c(lim_list,lim_value)
              sd_cutoff = 2*as.numeric(data_df[sample,'sd_cutoff'])
              sd_cutoff
              sd_list = c(sd_list,sd_cutoff)
              #print(sd_cutoff)
              #print(s_coord)
              p_values = sub_m$BH[sub_m$data == sample]
              p_values
              p_value = signif(unique(p_values[!is.na(p_values)]),3)
              p_value
              #print(length(p_value))
              if(length(p_value) == 0){
                p_value = ''
              }
              if(length(p_value) > 1){
                p_value = paste(p_value,collapse = ', ')
              }
              p_value_list = c(p_value_list,p_value)
              
              #p_value = signif(unique(sub_m$BH[sub_m$data == sample][1]),3)
              #print(p_value)
              #if(input$boxplot_sd_lim == T){
              #  p = p + geom_segment(x = s_coord,xend = (s_coord+1), y = sd_cutoff, yend = sd_cutoff, colour = 'blue')
              #  p = p + geom_segment(x = s_coord,xend = (s_coord+1), y = -sd_cutoff, yend = -sd_cutoff, colour = 'blue')
              #  p = p + coord_cartesian(ylim = c(-y_max_max,y_max_max))
                
              #}
              #if(input$boxplot_p_values == T){
              #  
              #  p = p + annotate('text', label = p_value, x = t_coord, y = y_max_max, size = input$boxplot_p_value_size)
              #  p = p + coord_cartesian(ylim = c(-y_max_max,y_max_max))
              #}
              s_coord = s_coord + 1
              t_coord = t_coord + 1
              #}
              #p = p + scale_y_continuous(limits = c(-y_max_max,y_max_max))
              #p = p + coord_cartesian(ylim = c(-y_max_max,y_max_max))
              
              
            }
          }
          p
          p = p + facet_wrap(~ data,scales = "free")
          p
          
          p_value_list
          sd_list
          lim_list
          p_value_data = data.frame(
            p_value = p_value_list,
            sd = sd_list,
            lim_max = lim_list,
            lim_min = -lim_list,
            data = sample_list
          )
          p_value_data

          p
          p = p + geom_text(
            data = p_value_data,
            mapping = aes(x = data, y = -Inf,label = p_value),
            hjust = -0.1,
            vjust = -1
          )
          p
          p = p + geom_hline(data = p_value_data,
                             aes(x = data, yintercept = sd_list),colour = 'blue')
          p = p + geom_hline(data = p_value_data,
                             aes(x = data, yintercept = -sd_list),colour = 'blue')
          p = p + geom_point(data = p_value_data,
                             aes(x = data, y = lim_max),alpha=0)
          
          p = p + geom_point(data = p_value_data,
                             aes(x = data, y = lim_min),alpha=0)
          
          p
          #p = p + facet_wrap(. ~ data,scales = "free")
          
          print('generated_plot')
          print(p)
          #dev.off()
          #saveRDS(p,'temp/p.rds')
          #saveRDS(sub_m,'temp/sub_m.rds')
          save_plot_function_2(boxplot_path,name,c('pdf','png'))
          
          #})
        }
        
        
        
        
        output[[output_name]] = renderImage({
          print('renderImage')
          print(plot_path)
          list(src = plot_path)
        }, deleteFile = FALSE)
      }
    })
  }
}



renderPlots = function(m, data_df, sample_list, gene_list, sample_path_line, input, output, prefix = 'gplot'){
  print('renderPlots_function')
  save_test = F
  
  if(save_test == T){
    variable_list = c('m','data_df','sample_list','gene_list','sample_path_line','prefix')
    
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
  # if(save_test == T){
  #   variable_list = c('m','data_df','sample_list','gene_list','sample_path_line','prefix')
  #   cmd_list = save_variable_function(variable_list)
  #   for(cmd in cmd_list){
  #     print(cmd)
  #     eval(parse(text = cmd))
  #   }
  #   #lapply(cmd_list, function(cmd) eval(parse(text = cmd)))
  #   save_input_function(input)
  # }
  # read_test = F
  # if(read_test == T){
  #   variable_list = c('m','data_df','sample_list','gene_list','sample_path_line','prefix')
  #   
  #   cmd_list = read_variable_function(variable_list)
  #   
  #   for(cmd in cmd_list){
  #     print(cmd)
  #     eval(parse(text = cmd))
  #   }
  #   #sapply(cmd_list, function(cmd) eval(parse(text = cmd)))
  #   
  #   input = readRDS('temp/save_input.rds')
  # }
  
  boxplot_path = sample_path_line
  
  
  #print('gene_list')
  #print(gene_list)
  gene = gene_list[1]
  gene
  for(gene in gene_list){
    local({
      
      
      #print('gene')
      #print(gene)
      
      output_name = paste(gene,prefix,sep = '_')
      name = paste(gene)
      plot_path = paste0(sample_path_line,'/',gene,'.png')
      #print(plot_path)
      #print(file.exists(plot_path))
      
      print('running ')
      print(output_name)
      sub_m = m[m$id == gene,]
      sub_m = sub_m[sub_m$type == 'mean',]
      sub_m = sub_m[!is.na(sub_m$id),]
      sub_m$Accession[is.na(sub_m$Accession)] = ''
      #View(sub_m)
      #sample_list = sample_list[sample_list %in% unique(sub_m$data)]
      
      
      y_max = max(c(sub_m$value,(2*as.numeric(data_df[sample_list,'sd_cutoff']))),na.rm = T)
      y_min = min(c(sub_m$value,(-2*as.numeric(data_df[sample_list,'sd_cutoff']))),na.rm = T)
      y_max_max = max(y_max,abs(y_min))
      y_max_max = y_max_max + (y_max_max*0.1)
      #View(sub_m)
      #print(dim(sub_m))
      if(dim(sub_m)[1] > 0){
        #run = T
        #print(run)
        file_test = file.exists(plot_path)
        #print(file_test)
        #print(input$re_run_boxplots)
        if(file_test == FALSE | input$re_run_boxplots == TRUE){
          #col = sig_colour_function(sub_m)
          col = c('green','red')
          #col = c('red','green')
          #output[[output_name]] = renderPlot({
          
          id_acc_list = unique(sub_m$id_acc)
          id_acc_list = id_acc_list[!is.na(id_acc_list)]
          accession_list = unique(sub_m$Accession)
          accession_list_NA = accession_list[accession_list != '']
          #print(accession_list_NA)
          accession_list = accession_list[order(accession_list)]
          #print(id_acc_list)
          #if('boxplot'%in% input$boxplot_type_select){
          if(length(id_acc_list) > 1 & length(accession_list_NA) > 1){
            
            
            p = ggplot(sub_m, aes(x = data,y = value,fill = sig,col = Accession))
            if('boxplot' %in% input$boxplot_type_select){
              
              p = p + geom_boxplot(position=position_dodge(width=0.9),size = 1)
            }
            print(p)
            accession_list = unique(sub_m$Accession)
            accession_list = accession_list[order(accession_list)]
            
            p = p + scale_color_manual(name = 'Isoforms', breaks = accession_list, values = c('black',cm.colors(length(accession_list)-1)))
            if('dotplot' %in% input$boxplot_type_select){
              p = p + geom_dotplot(binaxis='y', stackdir='center',fill = 'black',dotsize = input$boxplot_dotplot_size, position = 'dodge')
            }
          }else{
            p = ggplot(sub_m, aes(x = data,y = value,fill = sig))
            if('boxplot' %in% input$boxplot_type_select){
              
              p = p + geom_boxplot()
            }
            if('dotplot' %in% input$boxplot_type_select){
              
              p = p + geom_dotplot(binaxis='y', stackdir='center',fill = 'black',dotsize = input$boxplot_dotplot_size)
            }
            
          }
          # }
          if('violin' %in% input$boxplot_type_select){
            p = p + geom_violin() 
          }
          
          simple_plot = 'F'
          
          #p = p + geom_dotplot(binaxis='y', stackdir='center',fill = 'black',dotsize = 0.5)
          #p = p + geom_dotplot(binaxis='y', stackdir='center',fill = 'black',dotsize = 0.5)
          #p = p + geom_jitter(colour = 'black')
          
          ### plot formatting ###
          p = p + labs(x = '', y =  'log2(ratio)') +
            geom_hline(yintercept = 0) +
            scale_x_discrete(limits = sample_list) + 
            ggtitle(paste(name))  
          #scale_fill_manual(name = 'Significantly', breaks = c(-1,1),values = col,labels = c('downregulated','upregulted'))
          # need this to work 
          p = p + scale_fill_manual(name = 'Significantly', breaks = c(1,-1),values = c('red','green'),labels = c('upregulated','downregulted'),drop = FALSE)
          
          if(simple_plot == 'F'){
            p = p + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5, size = input$boxplot_title_size), 
                          axis.title = element_text(size = input$boxplot_text_size),
                          axis.text.x = element_text(size = input$boxplot_x_axis_size,face = 'bold'),
                          axis.text.y = element_text(size = input$boxplot_y_axis_size))
            #pdf(paste('/Users/sgarnett/Documents/Doctorate/Thesis/Thesis_Pictures/MQ_Gui/marker_lists/shiny_gplots//',name,'.pdf'))
            
            s_coord = 0.5
            t_coord = 1
            for(sample in sample_list){
              #print(sample)
              sd_cutoff = 2*as.numeric(data_df[sample,'sd_cutoff'])
              #print(sd_cutoff)
              #print(s_coord)
              p_values = sub_m$BH[sub_m$data == sample]
              p_value = signif(unique(p_values[!is.na(p_values)]),3)
              #print(length(p_value))
              if(length(p_value) == 0){
                p_value = ''
              }
              if(length(p_value) > 1){
                p_value = paste(p_value,collapse = ', ')
              }
              
              
              #p_value = signif(unique(sub_m$BH[sub_m$data == sample][1]),3)
              #print(p_value)
              if(input$boxplot_sd_lim == T){
                p = p + geom_segment(x = s_coord,xend = (s_coord+1), y = sd_cutoff, yend = sd_cutoff, colour = 'blue')
                p = p + geom_segment(x = s_coord,xend = (s_coord+1), y = -sd_cutoff, yend = -sd_cutoff, colour = 'blue')
                p = p + coord_cartesian(ylim = c(-y_max_max,y_max_max))
                
              }
              if(input$boxplot_p_values == T){
                
                p = p + annotate('text', label = p_value, x = t_coord, y = y_max_max, size = input$boxplot_p_value_size)
                p = p + coord_cartesian(ylim = c(-y_max_max,y_max_max))
              }
              s_coord = s_coord + 1
              t_coord = t_coord + 1
              #}
              #p = p + scale_y_continuous(limits = c(-y_max_max,y_max_max))
              #p = p + coord_cartesian(ylim = c(-y_max_max,y_max_max))
              
            }
          }
          #p = p + facet_wrap(. ~ data,scales = "free")
          #p = p + facet_wrap(~ data,scales = "free")
          
          print('generated_plot')
          print(p)
          #dev.off()
          #saveRDS(p,'temp/p.rds')
          #saveRDS(sub_m,'temp/sub_m.rds')
          save_plot_function_2(boxplot_path,name,c('pdf','png'))
          
          #})
        }
        
        
        
        
        output[[output_name]] = renderImage({
          print('renderImage')
          print(plot_path)
          list(src = plot_path)
        }, deleteFile = FALSE)
      }
    })
  }
}


#renderPlots_ts(m_ts(), sample_list, gene_list, timeseries_list() ,sample_path_line(),input, output, 'gplot_ts')

renderPlots_ts = function(m_ts, sample_list, gene_list, timeseries_list, sample_path, input, output, prefix = 'gplot'){
  print('global : renderPlots_ts')
  save_test = F
  if(save_test == T){
    variable_list = c('m_ts', 'sample_list', 'gene_list', 'timeseries_list', 'sample_path','prefix')
    cmd_list = save_variable_function(variable_list)
    #print(cmd_list)
    lapply(cmd_list, function(cmd) eval(parse(text = cmd)))
    save_input_function(input)
  }
  read_test = F
  if(read_test == T){
    #variable_list = c('m','data_df','sample_list','gene_list','sample_path_line','prefix')
    
    cmd_list = read_variable_function(variable_list)
    
    for(cmd in cmd_list){
      #print(cmd)
      eval(parse(text = cmd))
    }
    #sapply(cmd_list, function(cmd) eval(parse(text = cmd)))
    
    input = readRDS('temp/save_input.rds')
  }
  
  
  boxplot_path = sample_path
  #print(boxplot_path)
  
  create_dir_function(boxplot_path)
  #print('gene_list')
  #print(gene_list)
  gene = gene_list[1]
  gene
  for(gene in gene_list){
    local({
      
      
      #print('gene')
      #print(gene)
      plot_path = paste0(boxplot_path,'/',gene,'.png')
      print('running')
      output_name = paste(gene,prefix,sep = '_')
      name = paste(gene)
      print(output_name)
      sub_m = m_ts[m_ts$id == gene,]
      sub_m = sub_m[!is.na(sub_m$ts),]
      #View(sub_m)
      #print(dim(sub_m))
      if(dim(sub_m[!is.na(sub_m$value),])[1] > 1){        
        if(file.exists(plot_path) == FALSE | input$re_run_boxplots == TRUE){
          print('generating image')
          #col = sig_colour_function(sub_m)
          #col = sig_colour_function(sub_m)
          col = c('green','red')
          #    saveRDS(sub_m,'temp/sub_m.rds')
          
          #pdf(paste('shiny_gplots/',gene,'.pdf'))
          
          p = ggplot(sub_m, aes(x = ts,y = value, col = data, fill = sig))
          
          
          p = p +  geom_boxplot(size = 1, position = position_dodge(width = 0.9)) +
            stat_summary(fun.y=mean,geom='line',lwd=2,aes(group = data)) + 
            geom_smooth(method = 'loess', size = 2) + 
            scale_x_discrete(limits = timeseries_list[['order']]) 
          #scale_y_continuous(name = 'log2(ratio vs Day 0)') + 
          p = p + geom_dotplot(binaxis='y', stackdir='center',fill = 'black',dotsize = 0.5, position = 'dodge')
          p = p + labs(y = 'log2(ratio vs Day 0)', x =  'Day') +
            
            ggtitle(paste(name)) +
            theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5, size = input$boxplot_title_size),
                  axis.title = element_text(size = input$boxplot_text_size),
                  axis.text.x = element_text(size = input$boxplot_x_axis_size,face = 'bold'),
                  axis.text.y = element_text(size = input$boxplot_y_axis_size)
            )
          p = p + scale_fill_manual(name = 'Significantly', breaks = c(-1,1),values = c('red','green'),labels = c('upregulated','downregulted'))
          
          
          # p = ggplot(sub_m, aes(x = data,y = value,fill = sig)) + 
          #   geom_boxplot() +
          #   geom_hline(yintercept = 0) +
          #   scale_x_discrete(limits = sample_list) + 
          #   ggtitle(paste(name)) + 
          #   scale_fill_manual(breaks = c(-1,1),values = col) + 
          #   theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))
          #pdf(paste('/Users/sgarnett/Documents/Doctorate/Thesis/Thesis_Pictures/MQ_Gui/marker_lists/shiny_gplots//',name,'.pdf'))
          print(p)
          #dev.off()
          
          save_plot_function_2(boxplot_path,name,c('pdf','png'))
          
          #})
        }
        
        
        
        
        output[[output_name]] = renderImage({
          print('renderImage')
          print(plot_path)
          list(src = plot_path)
        }, deleteFile = FALSE)
      }
    })
  }
}

makePlotContainers_sample <- function(sample_list,prefix="gplot") {
  ## Validate inputs
  print('makePlotContainers_sample')
  ## Construct plotOutputs
  lst <- lapply(sample_list, function(sample) {
    output_name = paste(sample,prefix,sep = '_')
    print(output_name)
    plotOutput(output_name,height = 500)
  })
  
  ## Make columns
  #lst <- lapply(split(lst, (seq.int(n)-1)%/%ncol), function(x) column(12/ncol, x))
  do.call(tagList, lst)
}

renderPlots_sample = function(m, sample_list, gene_list, path, input, output, prefix = 'gplot'){
  print('renderPlots_sample')
  for(sample in sample_list){
    local({
      #print('gene')
      #print(gene)
      
      output_name = paste(sample,prefix,sep = '_')
      name = paste(sample)
      plot_name = paste0('boxplot_',name)
      print(output_name)
      #boxplot_path = create_dir_function(paste(path,name,sep='/'))
      #boxplot_list_path = create_dir_function(paste(boxplot_path,'list'))
      #print()
      sub_m = m[m$id %in% gene_list & m$data == sample,]
      
      #print(dim(sub_m))
      if(dim(sub_m)[1] > 0){
        #print(colnames(sub_m))
        #if(dim(sub_m)[1] > 0){
        #print(name)
        #print(colnames(sub_m))
        col = sig_colour_function(sub_m)
        
        #col = c('red','green')
        output[[output_name]] = renderPlot({
          #pdf(paste('shiny_gplots/',gene,'.pdf'))
          p = ggplot(sub_m, aes(x = id,y = value,fill = sig)) + 
            geom_boxplot() +
            geom_hline(yintercept = 0) +
            scale_x_discrete(limits = gene_list) + 
            ggtitle(name) + 
            scale_fill_manual(breaks = c(-1,1),values = col) + 
            theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5, size = 22))
          #pdf(paste('/Users/sgarnett/Documents/Doctorate/Thesis/Thesis_Pictures/MQ_Gui/marker_lists/shiny_gplots//',name,'.pdf'))
          print(p)
          save_plot_function_2(path,plot_name)
          #dev.off()
          #print(p)
        })
        #}
      }
    })
  }
}

latex_filename_function = function(name){
  #print('latex_filename_function')
  char_list = c('\\.',' ','&','\\|',':',';','\\)','\\(',"'",',')
  for(char in char_list){
    name = gsub(char,'_',name)
  }
  #print('   latex_filename_function : done')
  
  return(name)
}

delete.na <- function(DF, n=0) {
  print('delete.na_function')
  df = DF[rowSums(is.na(DF)) <= n,]
  print('   delete.na_function : done')
  df
  
}

create_dir_list_function = function(path_list){
  print('create_dir_list_function')
  path_entry_list = c()
  for(entry in path_list){
    path_entry_list = c(path_entry_list,entry)
    path_entry_line = latex_filename_function(paste(path_entry_list,collapse='/'))
    #print(path_entry_line)
    create_dir_function(path_entry_line)
  }
  print('   create_dir_list_function : done')
  
  return(path_entry_line)
}


rep_ratio_function = function(df,col_names){
  df_n  = df[,NULL]
  for(i in c(1:length(col_names))){
    for(j in c(1:length(col_names))){
      if(i < j){
        print(paste(c(i,j),collapse = ' : '))
        name = paste(col_names[i],col_names[j],sep='__')
        name
        ratio = log2(df[,col_names[i]]/df[,col_names[j]])
        #ratio[apply(ratio, 2 , function(x) !is.finite(x))] = NA
        
        ratio
        df_n[,name] = ratio
      }
      #print(j)
    }
    #print(i)
  }
  return(df_n)
  
}

paired_ratio_function = function(df,col_names_1,col_names_2){
  df_n  = df[,NULL]
  for(i in c(1:length(col_names_1))){
    for(j in c(1:length(col_names_2))){
      if(i <= j){
        print(paste(c(i,j),collapse = ' : '))
        name = paste(col_names_1[i],col_names_2[j],sep='__')
        name
        print(name)
        ratio = log2(df[,col_names_1[i]]/df[,col_names_2[j]])
        #ratio[apply(ratio, 2 , function(x) !is.finite(x))] = NA
        ratio
        df_n[,name] = ratio
      }
      #print(j)
    }
    #print(i)
  }
  return(df_n)
  
}

t_test_apply_function = function(row_data,cols1,cols2){
  c1 = row_data[cols1]
  c1
  c1 = c1[!is.na(c1)]
  c1
  c2 = row_data[cols2]
  c2
  c2 = c2[!is.na(c2)]
  c2
  if(length(c1) >= 2 & length(c2) >= 2){
    t = t.test(c1,c2)
    p = t$p.value
  }else{
    p = NA
  }
  p
  return(p)
}
