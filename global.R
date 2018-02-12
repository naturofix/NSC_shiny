#x this file serves as the global interface between UI and server
# timeseries_list = list(S1_00 = "Day  0", S2_00 = "Day  0", S3_00 = "Day  0", S1_01 = "Day  1", S2_01 = "Day  1", S3_01 = "Day  1", 
#                        S2_02 = "Day  2", S1_02 = "Day  2", S3_02 = "Day  2", S1_04 = "Day  4", S2_04 = "Day  4", S3_04 = "Day  4", 
#                        S1_08 = "Day  8", S2_08 = "Day  8", S3_08 = "Day  8", S1_12 = "Day 12  ", S2_12 = "Day 12  ", S3_12 = "Day 12", 
#                        H9_00 = "Day  0", H10_00 = "Day  0", H9_01 = "Day  1", H10_01 = "Day  1", H10_02 = "Day  2", H9_02 = "Day  2", 
#                        H9_04 = "Day  4", H10_04 = "Day  4", H9_08 = "Day  8", H10_08 = "Day  8", H10_12 = "Day 12  ", H9_12 = "Day 12",
#                        order = c('Day  0','Day  1','Day  2', 'Day  4','Day  8','Day 12'))
#saveRDS(timeseries_list,ts_file_path)


# timeseries_list = list(S1_00 = "0", S2_00 = "0", S3_00 = "0", S1_01 = "1", S2_01 = "1", S3_01 = "1",
#                        S2_02 = "2", S1_02 = "2", S3_02 = "2", S1_04 = "4", S2_04 = "4", S3_04 = "4",
#                        S1_08 = "8", S2_08 = "8", S3_08 = "8", S1_12 = "12  ", S2_12 = "12  ", S3_12 = "12",
#                        H9_00 = "0", H10_00 = "0", H9_01 = "1", H10_01 = "1", H10_02 = "2", H9_02 = "2",
#                        H9_04 = "4", H10_04 = "4", H9_08 = "8", H10_08 = "8", H10_12 = "12  ", H9_12 = "12",
#                        order = c('0','1','2', '4','8','12'))
# saveRDS(timeseries_list,ts_file_path)

library(shiny)
library(gplots)
library(ggplot2)
theme_set(theme_bw())
library(gtools)
library(STRINGdb)
library(colortools)
library(colorspace)
library(reshape2)
library(venn)
library(shinyFiles)
#library(biomaRt)
library(DT)
library(stringr)
library(topGO)
library(shinydashboard)

string_access = T

base_path = "/mnt/MSD_128GB/"
function_path = paste0(base_path,"Doctorate/programming/CRUNCH/00_functions/")
source(paste(function_path,'os_functions.R',sep= '/'))
source(paste(function_path,'Graphs_functions.R',sep= '/'))


wd_path = paste0(base_path,"Doctorate/Thesis/Thesis_Data/Cleanup_Data/")
wd_path
setwd(wd_path)
#data_root = '/Users/sgarnett/Documents/Doctorate/Thesis/Thesis_Data'
enrichment_path = 'enrichment/'
shiny_data_path = "shiny_data/"
create_dir_function(shiny_data_path)
data_path = './data/'
create_dir_function(data_path)
mapped_data_path = paste0(data_path,'string_mapped/') 
create_dir_function(mapped_data_path)
#setwd('/Users/sgarnett/Documents/Doctorate/Thesis/Thesis_Data/Cleanup_Data/GO_data/')

thesis_path_sub = 'Thesis_Data/Cleanup_Data/'
create_dir_function(paste('./images/'))
shiny_image_path = create_dir_function(paste('images/shiny/'))
venn_image_path = create_dir_function(paste(shiny_image_path,'Venn',sep='/'))
string_image_path = create_dir_function(paste(shiny_image_path,'STRING',sep='/'))

cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


if(string_access == T){
  string_db = STRINGdb$new(version="10", score_threshold=400, species = 9606, input_directory = getwd())
  #string_db = STRINGdb$load(version="10", score_threshold=400, species = 9606, input_directory = getwd())
  save(string_db,file = 'string_db')
  }else{
    string_db = load('string_db')
  }

STRING_id_list_function = function(mapped_data,gene_list){
  STRING_id_list = mapped_data$STRING_id[mapped_data$id %in% gene_list]
  return(STRING_id_list)
}




run_enrichment_function = function(ontology,path_list,STRING_id_list,string_db,mapped_st,annot,sample_path_line,backgroundV,input){
  ontology_list = input$select_enrichment
  
    if(input$enrichment_select == 'STRINGdb'){
      
      enrichment_path_line = enrichment_path_line_function(shiny_image_path,path_list,ontology,input$background)
    }
    if(input$enrichment_select == 'topGO'){
      enrichment_path_line = enrichment_path_line_function(shiny_image_path,path_list,ontology,NULL)
      venn_path_line = paste(c(shiny_image_path,path_list),collapse = '/')
      venn_path_line = latex_filename_function(venn_path_line)
      #venn_ont_path_line = paste(c(shiny_image_path,path_list,ontology),collapse = '/')
      #venn_ont_path_line = latex_filename_function(venn_ont_path_line)
      #venn_path_line
    }
    file_name_prefix = paste0(input$taxonomy,'_')
    if(input$iea == 'TRUE'){
      file_name_prefix = paste0(input$taxonomy,'_iea_')
      
    }
    file_name = paste0(file_name_prefix,input$enrichment_select,'.rds')
    
    file_path = paste(enrichment_path_line,file_name,sep = '/')
    print(file_path)
    file_hit = 0
    if(file.exists(file_path) & input$enrich_re_run == FALSE){
      print('file exists')
      #file_hit = 1
      file_hit = tryCatch({
        enrichment_GO = readRDS(file_path)
        print(dim(enrichment_GO))
        file_hit = 1
      }, error = function(e) {file_hit = 0})
      print(file_hit)
    }
    #print(file_hit)
    if(file_hit == 0){
      print('re run enrichment')
      hits = STRING_id_list
      if(input$enrichment_select == 'STRINGdb'){
        print('STRINGdb')
        enrichment_GO <- string_db$get_enrichment(hits, category = ontology, methodMT = input$select_sn_MT, iea = FALSE )
        enrichment_GO$pvalue_fdr = as.numeric(enrichment_GO$pvalue_fdr)
        
        saveRDS(enrichment_GO,file_path)
        
      }
      if(input$enrichment_select == 'topGO'){
        print('topGO')
        all_mapped = mapped_st
        print(dim(all_mapped))
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
        
        print(geneNames_all_path)
        geneNames_all = readRDS(geneNames_all_path)
        print(gene2GO_all_path)
        gene2GO_all = readRDS(gene2GO_all_path)

        ont = 'CC'
        ont = paste(enrichment_abreviation_list[ontology])
        ont
        ens_list = all_mapped$STRING_id
        print(length(ens_list))
        data_mapped = all_mapped[all_mapped$STRING_id %in% hits,]
        geneList_path = paste0(venn_path_line,'/topGO_geneList.rds')
        #sig_geneList_path = paste0(sample_path_line,'/sig_topGO_geneList.rds')
        
        print(geneList_path)
        if(!file.exists(geneList_path) | input$enrich_re_run == TRUE){
          geneList = topGO_mapping(data_mapped,geneNames_all,annot_list)
          saveRDS(geneList, geneList_path)
          print('saveRDS')
          #sig_geneList = topGO_mapping(all_mapped,geneNames_all,annot_list)
          #saveRDS(sig_geneList, geneList_path)
          #print('saveRDS')
          
        }else{
          geneList = readRDS(geneList_path)
          #sig_geneList = readRDS(sig_geneList_path)
          print('found')
          
        }
        GOdata_path = paste0(enrichment_path_line,'/',file_name_prefix,'_topGO_GOdata.rds')
        #sig_GOdata_path = paste0(sample_path_line,'/',file_name_prefix,'_',ontology,'_sig_topGO_GOdata.rds')
        print(GOdata_path)
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
        print(venn_path_line)
        df = topGO_result(GOdata,venn_path_line,'topGO')
        
        df$term_id = df$GO.ID
        df$proteins = as.numeric(df$Annotated)
        df$hits = as.numeric(df$Significant)
        df$pvalue = as.numeric(df$classicFisher)
        df$pvalue_fdr = as.numeric(df[,input$select_sn_MT])
        df$term_description = df$Term
        print(file_path)
        
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
    print(file_path)
    #enrichment_GO
    #enrichment_GO$pvalue_fdr = as.numeric(enrichment_GO$pvalue_fdr)
    return(enrichment_GO)
  }


path_line_function = function(path_list){
  #path_list = c(shiny_image_path,path_list)
  path_entry = c()
  for(entry in path_list){
    path_entry = c(path_entry,entry)
    path_line = latex_filename_function(paste(path_entry,collapse = '/'))
    create_dir_function(path_line)
  }
  return(path_line)
}

enrichment_path_line_function = function(shiny_image_path,path_list,enrichment,background,save_plot){
  #path_list = gene_list_select_list()$path_list
  path_list = c(path_list,'enrichment',enrichment,background)
  path_entry = c()
  

    for(entry in path_list){
      path_entry = c(path_entry,entry)
      path_entry_line = latex_filename_function(paste(path_entry,collapse = '/'))
      create_dir_function(paste0(shiny_image_path,path_entry_line))
    }
  
  path_line = paste0(shiny_image_path,latex_filename_function(paste(path_list,collapse='/')))
  print(path_line)
  return(path_line)
  
}

string_hits_function = function(enrichment_table,GO_term,mapped_data,annot){
  #if(values$sn == 1){
  print('string hits')
  #mapped_data = mapped_st()
  #GO_term = input$term
  GO_id = enrichment_table$term_id[enrichment_table$term_descriptio == GO_term]
  GO_id
  string_GO_members = annot$STRING_id[annot$term_id == GO_id]
  string_GO_members
  GO_members = string_GO_members
  STRING_hits = GO_members[GO_members %in% mapped_data$STRING_id]
  print('string hits done')
  print(STRING_hits)
  STRING_hits
  return(STRING_hits)
}

topGO_geneNames_all = function(ens_list,annot_list,path_list,file_name_prefix,re_run = FALSE){
  print(path_list$gene2GO_all_path)
  if(!file.exists(path_list$gene2GO_all_path) | re_run == TRUE){
    print('re-running topGO enrichment')
    print(length(ens_list))
    ens_list = ens_list[!is.na(ens_list)]
    GO_terms = annot_list[ens_list]
  
    gene2GO_all = GO_terms
    print(path_list$gene2GO_all_path)
    saveRDS(gene2GO_all, path_list$gene2GO_all_path)
    GO2gene_all = inverseList(gene2GO_all)
    print(path_list$GO2gene_all_path)
    saveRDS(GO2gene_all, path_list$GO2gene_all_path)
    geneNames_all <- names(gene2GO_all)
    print(path_list$geneNames_all_path)
    saveRDS(geneNames_all, path_list$geneNames_all_path)
  }else{
      print('found')
    }
  }

topGO_mapping = function(mapped,geneNames_all, annot_list){
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
  return(geneList)
}

new_col_mix = function(col1,col2){
  if(is.na(col1)){
    col = col2
  }else{
    col = hex(mixcolor(0.5,hex2RGB(col1),hex2RGB(col2)))
  }
  return(col)
}

col_select_function = function(num,up_col,down_col,col){
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
  return(col)
  #print(col)
}

label_select_function = function(num,name,label){
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
  
  return(label)
  #print(col)
}

string_id_mapping = function(data,id){
  string_data = string_db$map(data, id, removeUnmappedRows = TRUE)
  return(string_data)
}

prep_tf_files = function(){
  AnimalTFDB_path = paste('./database/AnimalTFDB',sep='/')
  print(AnimalTFDB_path)
  file_list = list.files(AnimalTFDB_path)
  file_list
  tf_path = create_dir_function(paste(column_path,'AnimalTFDB',sep='/'))
  file_list = file_list[grep('.txt',file_list)]
  file_list
  file_name = file_list[1]
  
  for(file_name in file_list){
    print(file_name)
    if(grepl('.txt',file_name)){
      tf_file_path = create_dir_function(paste(tf_path,gsub('.txt','',file_name),sep='/'))
      print(file_name)
      cmd = paste("temp_table = read.table(file = '",paste(AnimalTFDB_path,file_name,sep='/'),"',sep='\t',header=TRUE)",sep='')
      print(cmd)
      eval(parse(text=cmd))
      head(temp_table)
      dim(temp_table)
      sn_temp_table = string_id_mapping(temp_table,'Ensembl.ID')
      
      head(sn_temp_table)
      dim(sn_temp_table)
      saveRDS(sn_temp_table,paste(AnimalTFDB_path,sub('.txt','.rds',file_name),sep='/'))
    }
  }
}


common_mapped_function = function(shiny_data_path,sig_data_list,data_select_list,start_col,string_db,data_list,data_name_collapse,removed_list){
  test = F
  if(test == T){
    data_select_list = data_name_list
    start_col = 'red'
    d_num = 5
  }
  #data_name_collapse = paste(data_select_list,collapse = '_')
  
  print('joining selected datasets using commmon_mapped_function, please be patient...')
  d_num = length(data_select_list) * 2

  colour_wheel = setColors(start_col, d_num)
  print(data_select_list)
  print(data_list)
  print('readRDS - common_mapped_function')
  entry = data_select_list[1]
  
  cmd = paste0(entry,"_sig_mapped = sig_data_list[['",entry,"']]")
  eval(parse(text = cmd))
  cmd = paste0(entry,"_sig_mapped_test = readRDS('data/string_mapped/all_mapped.",unlist(data_list[entry]),".rds')")
  eval(parse(text = cmd))
  cmd = paste0(entry,"_sig_mapped$data = '",entry,"'")
  eval(parse(text = cmd))
  
  if(length(data_select_list) > 1){
    for(entry in data_select_list[2:length(data_select_list)]){ 
      cmd = paste0(entry,"_sig_mapped = sig_data_list[['",entry,"']]")
      
      #cmd = paste0(entry,"_sig_mapped = readRDS('./GO_data/sig_mapped.",unlist(data_list[entry]),".rds')")
      eval(parse(text = cmd))
      cmd = paste0(entry,"_sig_mapped$data = '",entry,"'")
      print(cmd)
      eval(parse(text = cmd))
      } # generate dataframes from rds files
  }
   
  print('reshape')
  #column_names = c('data','id','STRING_id','mean')
  if(length(data_select_list) > 0){
    
    
    cmd = paste0('data = ',data_select_list[1],'_sig_mapped')

    eval(parse(text = cmd))
 
    if('mean' %in% colnames(data)){
      column_names = c('id','STRING_id','mean','Accession','id_acc')
    }else(
      column_names = c('id','STRING_id','slope','Accession','id_acc')
    )
    sub_data = data[,column_names]
    # if('Accession' %in% colnames(data)){
    #   column_names = c(column_names,'Accession')
    #   
    #   sub_data$id_acc = sub_data$id
    #   sub_data$id_acc[duplicated(sub_data$id)] = paste(sub_data$id[duplicated(sub_data$id)],sub_data$Accession[duplicated(sub_data$id)],sep = '_')
    #   #sub_data$id_acc
    # }else{
    #   sub_data = data[,column_names]
    #   sub_data$Accession = NA
    #   sub_data$id_acc = sub_data$id
    # }
    # dup_i = 1
    # if(TRUE %in% duplicated(sub_data$id_acc)){
    #   print(sub_data$id_acc[duplicated(sub_data$id_acc)])
    #   
    #   sub_data$id_acc[duplicated(sub_data$id_acc)] = paste(sub_data$id_acc[duplicated(sub_data$id_acc)],dup_i,sep='__')
    #   
    # }
    # while(TRUE %in% duplicated(sub_data$id_acc)){
    #   print(sub_data$id_acc[duplicated(sub_data$id_acc)])
    #   sub_data$id_acc[duplicated(sub_data$id_acc)] = gsub(paste0('__',dup_i),paste0('__',dup_i+1),sub_data$id_acc[duplicated(sub_data$id_acc)])
    #   dup_i = dup_i + 1
    # }

    #sub_data = data[,column_names]
    colnames(sub_data) =  c('id','STRING_id',data_select_list[1],paste(data_select_list[1],'Accession',sep='_'),'id_acc')
    
    mapped_combined = sub_data
    if(length(data_select_list) > 1){
      for(entry in data_select_list[2:length(data_select_list)]){
        cmd = paste0('data = ',entry,'_sig_mapped')

        eval(parse(text = cmd))
        if('mean' %in% colnames(data)){
          column_names = c('id','STRING_id','mean','Accession','id_acc')
        }else(
          column_names = c('id','STRING_id','slope','Accession','id_acc')
        )
        sub_data = data[,column_names]
        # if('Accession' %in% colnames(data)){
        #   column_names = c(column_names,'Accession')
        #   sub_data = data[,column_names]
        #   sub_data$id_acc = sub_data$id
        #   sub_data$id_acc[duplicated(sub_data$id)] = paste(sub_data$id[duplicated(sub_data$id)],sub_data$Accession[duplicated(sub_data$id)],sep = '_')
        #   #sub_data$id_acc
        #   
        # }else{
        #   sub_data = data[,column_names]
        #   sub_data$Accession = NA
        #   sub_data$id_acc = sub_data$id
        # }
        # 
        # if(TRUE %in% duplicated(sub_data$id_acc)){
        #   print(sub_data$id_acc[duplicated(sub_data$id_acc)])
        #   sub_data$id_acc[duplicated(sub_data$id_acc)] = paste(sub_data$id_acc[duplicated(sub_data$id_acc)],dup_i,sep='__')
        # }
        # while(TRUE %in% duplicated(sub_data$id_acc)){
        #   print(sub_data$id_acc[duplicated(sub_data$id_acc)])
        #   sub_data$id_acc[duplicated(sub_data$id_acc)] = gsub(paste0('__',dup_i),paste0('__',dup_i+1),sub_data$id_acc[duplicated(sub_data$id_acc)])
        #   
        #   dup_i = dup_i + 1
        # }
        
        colnames(sub_data) =  c('id','STRING_id',entry,paste(entry,'Accession',sep='_'),'id_acc')

        #colnames(sub_data) =   column_names = c('data','id','STRING_id','value')
        mapped_combined = merge(mapped_combined,sub_data,by = c('id','id_acc','STRING_id'), all = TRUE)
                          
        #mapped_combined = rbind(mapped_combined,sub_data)
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
    mapped_combined[,entry][is.na(mapped_combined[,entry])] = 0
  }
  mapped_long_w = mapped_combined
  
  #removed_list = c('SEP')
  gene_list = mapped_long_w$id
  mapped_long_w = mapped_long_w[!is.na(mapped_long_w$id),]
  mapped_long_w = mapped_long_w[!mapped_long_w$id %in% removed_list,]
  
  head(mapped_combined)
  #mapped_combined_old = mapped_combined[,c(1:4)]
  #mapped_combined_old
  #head(mapped_combined_old)
  #mapped_long_w = reshape(mapped_combined_old, idvar = c('id','STRING_id'), timevar = c('data'),direction = 'wide')
  #head(mapped_long_w)
  #mapped_long_w[mapped_long_w$id == 'NEFL',]
  #mapped_long_w[mapped_long_w$id == 'HLA',]
  #mapped_melt = melt(mapped_combined_old, id.vars = c('id','STRING_id'),measure.vars = c('value','data'))
  
  #mapped_melt = melt(mapped_combined, id.vars = c('id','STRING_id','id_acc','Accession','data'),measure.vars = c('value'))
  #head(mapped_melt)
  #mapped_melt[mapped_melt$id == 'NEFL',]
  #mapped_melt[mapped_melt$id == 'HLA',]
  
  #for(entry in data_select_list){
  #  print(entry)
  #  mapped_melt[,entry] = 0
  #  mapped_melt[,entry][mapped_melt$data == entry] = mapped_melt$value[mapped_melt$data == entry]  
    
  #}
  #colname_melt = colnames(mapped_melt)[!colnames(mapped_melt) %in% c('data','variable','value')]
  #colname_melt = colnames(mapped_melt)[!colnames(mapped_melt) %in% c('data','variable','value')]
  
  #colname_melt
  #mapped_melt = mapped_melt[,colname_melt]
  #head(mapped_melt)
  #mapped_long_w = mapped_melt
  
  #colnames(mapped_combined)
  #colnames(mapped_long_w)
  #colnames(mapped_long_w_melt)
  #head(mapped_long_w_melt)
  i = grep('STRING_id',colnames(mapped_long_w))
  i
  #mapped_long_w = reshape(mapped_combined, idvar = c('id_acc'), timevar = c('data'),direction = 'wide')
  #t_w = reshape(t, idvar = c('id','STRING_id','id_acc'), timevar = c('data'),direction = 'wide')
  #t_w
  #mapped_long_w_melt = melt(mapped_combined, id.vars = c('id','STRING_id','Accession','value'))

  #mapped_long_w[,c((i+1):length(colnames(mapped_long_w)))][is.na(mapped_long_w[,c((i+1):length(colnames(mapped_long_w)))])] = 0
  #print(colnames(mapped_long_w))
  #new_colnames = (sub('value.','',colnames(mapped_long_w)))
  #colnames(mapped_long_w) = new_colnames
  #head(mapped_long_w)
  #print('melt')
  #mapped_long_m = melt(mapped_combined, id.vars = c('id','STRING_id','data'),measure.vars = data_select_list)
  #print(colnames(mapped_long_m))
  
  check_dfs = F
  if(check_dfs == T){
    mapped_long = melt(mapped_combined, id.vars = c('id','STRING_id','data'))
    for(entry in data_select_list){
      cmd = paste0('original_data = ',entry,'_sig_mapped')
      eval(parse(text = cmd))
      long = (mapped_long[mapped_long$data == entry,])
      diff_result = setdiff(long$id,original_data$id)
      print(diff_result)
      diff_result = setdiff(original_data$id,long$id)
      print(diff_result)
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
  mapped_long_w$col = NA
  mapped_long_w$label = NA
  #apply(mapped_long_w, 1, function(x) print(x['col']))
  for(entry in data_select_list){
    mapped_long_w$col = apply(mapped_long_w, 1, function(x) col_select_function(x[entry],colour_list[paste(entry,'up')],colour_list[paste(entry,'down')],x['col']))
    mapped_long_w$label = apply(mapped_long_w, 1, function(x) label_select_function(x[entry],entry,x['label']))
  }

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
  payload_id = string_db()$post_payload(mapped_long_w$STRING_id, colors=mapped_long_w$col)
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
  payload_rds_file_name = paste0(shiny_data_path,'common_sig_mapped_payload_id_',data_name_collapse,'.rds')
  saveRDS(payload_id,payload_rds_file_name)
  entry_list_file_name = paste0(shiny_data_path,'entry_list_',data_name_collapse,'.rds')
  saveRDS(entry_list,entry_list_file_name)
  id_list = list()
  id_list[mapped_long_w$STRING_id] = mapped_long_w$id
  #id_list[mapped_long_w$STRING_id] = list(mapped_long_w$id)
  #lapply(mapped_long_w$STRING_id, function(x) id_list[x] = list(unique(mapped_long_w$id[mapped_long_w$STRING_id == x])))
  id_file_list = paste0(shiny_data_path,'id_list_',data_name_collapse,'.rds')
  saveRDS(id_list,id_file_list)
  
  removed_file_list = paste0(shiny_data_path,'removed_list_',data_name_collapse,'.rds')
  saveRDS(removed_list,removed_file_list)
  
  gene_file_list = paste0(shiny_data_path,'gene_list_',data_name_collapse,'.rds')
  saveRDS(gene_list,gene_file_list)
  
  results_list = list(mapped_data = mapped_long_w,
                      mapped_ud = mapped_ud,
                      payload_id = payload_id,
                      entry_list = entry_list,
                      colour_list = colour_list,
                      id_list = id_list,
                      removed_list = removed_list,
                      gene_line = gene_list)
  return(results_list)
}

common_melt_function = function(data_select_list, data_df_list, sig_data_list, data_df, timeseries_list){
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
  print(colnames(data))
  col_index = c(2:(grep(dname,colnames(data))-1))
  col_index
  print(colnames(data)[col_index])
  if(data_select_list[1] == 'ESC'){
    col_index = c(12:146)
  }
  print(col_index)
  m_colnames = c("id",'id_acc','Accession',dname,"BH","STRING_id","data","variable","value",'type','sig')
  m = melt(data,measure.vars = col_index)
  print(colnames(m))
  head(m)
  m = m[,m_colnames]
  m = m[!is.na(m$value),]
  #head(m[!is.na(m$sig),])
  if(length(data_select_list) > 1){
    for(data_name in data_select_list[c(2:length(data_select_list))]){
      print(data_name)
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
      print(dim(data))
      
      
      print(colnames(data))
      
      col_index = c(2:(grep(dname,colnames(data))-1))
      col_index
      if(data_name == 'ESC'){
        col_index = c(12:146)
      }
      n = melt(data,measure.vars = col_index)
      print(dim(n))
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
  return(col)
}

renderImages = function(gene_list,boxplot_path,input,output,prefix){
  for(gene in gene_list){
    local({
      output_name = paste(gene,prefix,sep = '_')
      plot_path = paste0(boxplot_path,'/',gene,'.png')
      print(plot_path)
      output[[output_name]] = renderImage({
        print('renderImage')
        print(plot_path)
        list(src = plot_path)
      }, deleteFile = FALSE)
    })
  }
}

renderPlots = function(m, data_df, sample_list, gene_list, sample_path_line, input, output, prefix = 'gplot'){
  
  boxplot_path = sample_path_line
  
  
  #print('gene_list')
  #print(gene_list)
  for(gene in gene_list){
    local({

      
      #print('gene')
      #print(gene)
      
      output_name = paste(gene,prefix,sep = '_')
      name = paste(gene)
      plot_path = paste0(boxplot_path,'/',gene,'.png')
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
          col = sig_colour_function(sub_m)
          col
          #col = c('red','green')
          #output[[output_name]] = renderPlot({
          
            id_acc_list = unique(sub_m$id_acc)
            id_acc_list = id_acc_list[!is.na(id_acc_list)]
            accession_list = unique(sub_m$Accession)
            accession_list_NA = accession_list[accession_list != '']
            print(accession_list_NA)
            accession_list = accession_list[order(accession_list)]
            print(id_acc_list)
              #if('boxplot'%in% input$boxplot_type_select){
                if(length(id_acc_list) > 1 & length(accession_list_NA) > 1){
                
                
                  p = ggplot(sub_m, aes(x = data,y = value,fill = sig,col = Accession))
                  if('boxplot' %in% input$boxplot_type_select){
                    
                    p = p + geom_boxplot(position=position_dodge(width=0.9),size = 1)
                  }
      
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
                ggtitle(paste(name)) + 
                 scale_fill_manual(name = 'Significantly', breaks = c(-1,1),values = col,labels = c('downregulated','upregulted'))
                if(simple_plot == 'F'){
                 p = p + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5, size = input$boxplot_title_size), 
                      axis.title = element_text(size = input$boxplot_text_size),
                      axis.text.x = element_text(size = input$boxplot_x_axis_size,face = 'bold'),
                      axis.text.y = element_text(size = input$boxplot_y_axis_size))
               #pdf(paste('/Users/sgarnett/Documents/Doctorate/Thesis/Thesis_Pictures/MQ_Gui/marker_lists/shiny_gplots//',name,'.pdf'))
               
                  s_coord = 0.5
                  t_coord = 1
                  for(sample in sample_list){
                    print(sample)
                    sd_cutoff = 2*as.numeric(data_df[sample,'sd_cutoff'])
                    print(sd_cutoff)
                    print(s_coord)
                    p_values = sub_m$BH[sub_m$data == sample]
                    p_value = signif(unique(p_values[!is.na(p_values)]),3)
                    print(length(p_value))
                    if(length(p_value) == 0){
                      p_value = ''
                    }
                    if(length(p_value) > 1){
                      p_value = paste(p_value,collapse = ', ')
                    }
                    
                    
                    #p_value = signif(unique(sub_m$BH[sub_m$data == sample][1]),3)
                    print(p_value)
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

renderPlots_ts = function(m, sample_list, gene_list, timeseries_list, sample_path, input, output, prefix = 'gplot'){
  test = F
  if(test == T){
    m = m_ts()
    timeseries_list = timeseries_list()
    sample_path = sample_path_line()
    gene = gene_list[1]
    
  }
  boxplot_path = sample_path
  #print(boxplot_path)
  
  create_dir_function(boxplot_path)
  #print('gene_list')
  #print(gene_list)
  for(gene in gene_list){
    local({
      
      
      #print('gene')
      #print(gene)
      plot_path = paste0(boxplot_path,'/',gene,'.png')
      print('running')
      output_name = paste(gene,prefix,sep = '_')
      name = paste(gene)
      print(output_name)
      sub_m = m[m$id == gene,]
      sub_m = sub_m[!is.na(sub_m$ts),]
      #View(sub_m)
      #print(dim(sub_m))
      if(dim(sub_m)[1] > 0){        
         if(file.exists(plot_path) == FALSE | input$re_run_boxplots == TRUE){
          print('generating image')
          col = sig_colour_function(sub_m)
          #col = sig_colour_function(sub_m)
          #col = c('red','green')
          saveRDS(sub_m,'temp/sub_m.rds')
          
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
          p = p + scale_fill_manual(name = 'Significantly', breaks = c(-1,1),values = col,labels = c('downregulated','upregulted'))

            
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
      
      print(dim(sub_m))
      if(dim(sub_m)[1] > 0){
      #print(colnames(sub_m))
        #if(dim(sub_m)[1] > 0){
          print(name)
          print(colnames(sub_m))
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
  char_list = c('\\.',' ','&','\\|',':',';','\\)','\\(',"'",',')
  for(char in char_list){
    name = gsub(char,'_',name)
  }
  return(name)
}

delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}

gene_list = c('SOX2','LIN28A','TUBB3','GFAP')

topGO_enrich_cutoff = 0.05
ontology_path_name = c('CC' = 'Component', 'BP' = 'Process', 'MF' = 'Function')
timecourse_data = c('df_Diff_LFQ_Sai_first_element_ratio_log2_all_MCT_all_mapping','df_Diff_LFQ_HB_first_element_ratio_log2_all_MCT_all_mapping')
# Define server logic required to draw a histogram



#image_folder = '..//'
#image_folder = '/Users/sgarnett/Documents/Doctorate/Thesis/Thesis_Pictures/MQ_Gui/'
#all_folder = '/Users/sgarnett/Documents/Doctorate/Thesis/Thesis_Pictures/MQ_Gui/marker_lists/ESC_GE_SILAC_Diff/all/'
#shiny_gplots = '/Users/sgarnett/Documents/Doctorate/Thesis/Thesis_Pictures/MQ_Gui/marker_lists/shiny_gplots/'
#addResourcePath('images', image_folder)
#addResourcePath('all',all_folder)
#addResourcePath('shiny_gplots',shiny_gplots)


enrichment_abreviation_list = list(Component = 'CC', Function = 'MF', Process = 'BP',KEGG = "KEGG", InterPro = 'InterPro', Pfam = 'Pfam')
enrichment_full_list = list(Component = 'Cellular Component', Function = 'Molecular Function',
                            Process = 'Biological Process', 
                            KEGG = "KEGG", InterPro = 'InterPro', Pfam = 'Pfam')

data_name_list = c('ESC','GE','SILAC','NES_Diff','NS_Diff')
data_list = c('ESC' = c('df_Huang2016_txt_edited_log2_ratio_different_mart_all_MCT_all_t_test' ),
              'GE' = c('df_GE_log2_ratio_different_all_MCT_all'), 
              'SILAC' = c('df_SILAC_all_comparison_all_MCT_all_mapping'),
              'NES_Diff' = c('df_Diff_LFQ_Sai_first_element_ratio_log2_all_MCT_all_mapping'),
              'NS_Diff' = c('df_Diff_LFQ_HB_first_element_ratio_log2_all_MCT_all_mapping')
)

load_data_df = FALSE

if(load_data_df == TRUE){
  data_df = as.data.frame(data_list)
  data_df$cols = NA
  data_df['GE','cols'] = paste(GE_cols,collapse = ', ')
  data_df['ESC','cols'] = ESC_cols
  data_df['SILAC','cols'] = paste(SILAC_cols,collapse = ', ')
  data_df['NES_Diff','cols'] = paste(NES_ts_cols,collapse = ', ')
  data_df['NS_Diff','cols'] = paste(NS_ts_cols,collapse = ', ')
  
  data_df['GE','data'] = 'mean'
  data_df['ESC','data'] = 'mean'
  data_df['SILAC','data'] = 'mean'
  data_df['NES_Diff','data'] = 'slope'
  data_df['NS_Diff','data'] = 'slope'
  
  data_df['GE','p.value'] = 'BH'
  data_df['ESC','p.value'] = 'BH'
  data_df['SILAC','p.value'] = 'BH'
  data_df['NES_Diff','p.value'] = 'BH'
  data_df['NS_Diff','p.value'] = 'BH'
  
  data_df['GE','sd_cutoff'] = '0.06'
  data_df['ESC','sd_cutoff'] = '0.15'
  data_df['SILAC','sd_cutoff'] = '0.29'
  data_df['NES_Diff','sd_cutoff'] = '0'
  data_df['NS_Diff','sd_cutoff'] = '0'
  
  View(data_df)
  saveRDS(data_df,'data_df.rds')
}
load_data = FALSE
if(load_data == TRUE){
  ESC_mapped_all = readRDS(paste0("all_mapped.",data_list['ESC'],".rds"))
  ESC_mapped_all$data = 'ESC'
  colnames(ESC_mapped_all)
  ESC_mapped_all$log2_ratio_list
  ESC_cols = c('log2_ratio_list')
  #ESC_mapped$data = 'ESC'
  GE_mapped_all = readRDS(paste0("all_mapped.",data_list['GE'],".rds"))
  GE_mapped_all$data = 'GE'
  GE_mapped_all$id[GE_mapped_all$id == "LIN28"] = 'LIN28A'
  
  GE_cols = c( "HB901_a__Sai1_a", "HB901_a__Sai1_b", "HB901_a__Sai2_a", "HB901_a__Sai2_b", "HB901_a__Sai3_a",
               "HB901_a__Sai3_b", "HB901_b__Sai1_a", "HB901_b__Sai1_b", "HB901_b__Sai2_a", "HB901_b__Sai2_b", "HB901_b__Sai3_a",
               "HB901_b__Sai3_b", "HB985_a__Sai1_a", "HB985_a__Sai1_b", "HB985_a__Sai2_a", "HB985_a__Sai2_b", "HB985_a__Sai3_a",
               "HB985_a__Sai3_b", "HB985_b__Sai1_a", "HB985_b__Sai1_b", "HB985_b__Sai2_a", "HB985_b__Sai2_b", "HB985_b__Sai3_a",
               "HB985_b__Sai3_b")
  
  
  colnames(GE_mapped_all)
  #GE_mapped$data = 'GE'
  SILAC_mapped_all = readRDS(paste0("all_mapped.",data_list['SILAC'],".rds"))
  SILAC_mapped_all$data = 'SILAC'
  
  colnames(SILAC_mapped_all)
  SILAC_cols = c("S2L_H10H_SILAC_1", "S1L_H10H_SILAC_2", "S1L_H9H_SILAC_2",  "S2L_H10H_SILAC_2",
                 "S2L_H9H_SILAC_2",  "S1H_H9L_SILAC_2r", "S3L1_H9H1_SILAC_3","S3L2_H9H2_SILAC_3","S3H1_H9L1_SILAC_3r",
                 "S3H2_H9L2_SILAC_3r")
  NES_Diff_mapped_all= readRDS(paste0("all_mapped.",data_list['NES_Diff'],".rds"))
  NES_Diff_mapped_all$data = 'NES_Diff'
  
  NES_ts_cols =c("S1_00","S2_00","S3_00","S1_01",  "S2_01",  "S3_01",  "S1_02", 
                 "S2_02",  "S3_02",  "S1_04",  "S2_04",  "S3_04",  "S2_08",  "S3_08",  "S1_12", 
                 "S2_12",  "S3_12")
  NS_Diff_mapped_all = readRDS(paste0("all_mapped.",data_list['NS_Diff'],".rds"))
  NS_Diff_mapped_all$data = 'NS_Diff'
  
  NS_ts_cols = c("H9_00" , "H10_00", "H9_01",  "H10_01", "H9_02",  "H10_02", "H10_04",
                     "H9_08",  "H10_08", "H10_12")
}
  
  limit_list = c('total','total.sig','total.sig.stat')
  stat_list = c('classicFisher.bonferroni','fisher.weight01')
  topGO_stat_list = c("All","classicFisher","fisher.elim","fisher.weight01","fisher.lea", "fisher.parentchild")
  topGO_mtc_list = c('',"BH","bonferroni")
  mapped_list = c('all_mapped','sig_mapped','sig_mapped_up','sig_mapped_down','common_mapped')
  
  topGO_enrich_cutoff = 0.05
  ontology_list = c('Cellular Component' = 'CC','Biological Process' = 'BP','Molecular Function' = 'MF')
  ontology_path_name = c('CC' = 'Component', 'BP' = 'Process', 'MF' = 'Function')

  string_db_enrichment_list  = c('Component', 'Function','Process', 'KEGG','Pfam','InterPro', 'Tissue','Disease')
  string_db_methodMT_list = c('fdr','bonferroni')

