# this file serves as the global interface between UI and server
# timeseries_list = list(S1_00 = "Day  0", S2_00 = "Day  0", S3_00 = "Day  0", S1_01 = "Day  1", S2_01 = "Day  1", S3_01 = "Day  1", 
#                        S2_02 = "Day  2", S1_02 = "Day  2", S3_02 = "Day  2", S1_04 = "Day  4", S2_04 = "Day  4", S3_04 = "Day  4", 
#                        S1_08 = "Day  8", S2_08 = "Day  8", S3_08 = "Day  8", S1_12 = "Day 12  ", S2_12 = "Day 12  ", S3_12 = "Day 12", 
#                        H9_00 = "Day  0", H10_00 = "Day  0", H9_01 = "Day  1", H10_01 = "Day  1", H10_02 = "Day  2", H9_02 = "Day  2", 
#                        H9_04 = "Day  4", H10_04 = "Day  4", H9_08 = "Day  8", H10_08 = "Day  8", H10_12 = "Day 12  ", H9_12 = "Day 12",
#                        order = c('Day  0','Day  1','Day  2', 'Day  4','Day  8','Day 12'))
#saveRDS(timeseries_list,ts_file_path)

library(shiny)
library(gplots)
library(ggplot2)
library(gtools)
library(STRINGdb)
library(colortools)
library(colorspace)
library(reshape2)
library(venn)
library(shinyFiles)
#library(biomaRt)
library(DT)

string_access = T

base_path = "/mnt/MSD_128GB/"
function_path = paste0(base_path,"Doctorate/programming/CRUNCH/00_functions/")
source(paste(function_path,'os_functions.R',sep= '/'))
source(paste(function_path,'Graphs_functions.R',sep= '/'))


wd_path = paste0(base_path,"Doctorate/Thesis/Thesis_Data/Cleanup_Data/")
setwd(wd_path)
#data_root = '/Users/sgarnett/Documents/Doctorate/Thesis/Thesis_Data'
enrichment_path = './enrichment/'
shiny_data_path = "./shiny_data/"
create_dir_function(shiny_data_path)
data_path = './data/'
create_dir_function(data_path)
mapped_data_path = paste0(data_path,'string_mapped/') 
create_dir_function(mapped_data_path)
#setwd('/Users/sgarnett/Documents/Doctorate/Thesis/Thesis_Data/Cleanup_Data/GO_data/')

thesis_path_sub = 'Thesis_Data/Cleanup_Data/'
create_dir_function(paste('./images/'))
shiny_image_path = create_dir_function(paste('./images/shiny/'))
venn_image_path = create_dir_function(paste(shiny_image_path,'Venn',sep='/'))
string_image_path = create_dir_function(paste(shiny_image_path,'STRING',sep='/'))



if(string_access == T){
  string_db = STRINGdb$new(version="10", score_threshold=400, species = 9606, input_directory = getwd())
  #string_db = STRINGdb$load(version="10", score_threshold=400, species = 9606, input_directory = getwd())
  save(string_db,file = 'string_db')
  }else{
    string_db = load('string_db')
  }


topGO_geneNames_all = function(all_mapped,annot,table_path){
  ens_list = all_mapped[,"STRING_id"]
  ens_list = ens_list[!is.na(ens_list)]
  length(ens_list)
  GO_terms = list()
  #for(ens in ens_list){
  #  GO_terms[[ens]] = annot$term_id[annot$STRING_id == ens]
  #}
  GO_terms = lapply(ens_list, function(ens) annot$term_id[annot$STRING_id == ens])
  length(GO_terms)
  gene2GO_all = GO_terms
  GO2gene_all = inverseList(gene2GO_all)
  saveRDS(GO2gene_all, paste0(table_path,'/GO2gene_all.rds'))
  geneNames_all <- names(gene2GO_all)
  saveRDS(geneNames_all, paste0(table_path,'geneNames_all.rds'))
  
  
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
#prep_tf_files()

# tf_enrichment = function(column_path,sig_mapped,enrichment,){
#   sig_mapped = readRDS("/Users/sgarnett/Documents/Doctorate/Thesis/Thesis_Data/Cleanup_Data/GO_data/sig_mapped_down.df_GE_log2_ratio_different_all_MCT_all.rds")
#   column_path = "/Users/sgarnett/Documents/Doctorate/Thesis/Thesis_Data/Cleanup_Data/images/enrichment/df_GE_log2_ratio_different_all_MCT_all/mean/"
#   file_name = paste(enrichmentm'.txt')
#   
#   if(TF == TRUE){
#     
#     # define biomart object
#     mart <- useMart(biomart = "ensembl", dataset = "hsapiens_gene_ensembl")
#     # query biomart
#     results <- getBM(attributes = c("ensembl_gene_id", "ensembl_transcript_id", "ensembl_peptide_id"),
#                      filters = "ensembl_transcript_id", values = "ENST00000296026",
#                      mart = mart)
#     print('TF')
#     #script_path = '/blackburn3/scripts/CRUNCH/V2.3/maxquant_r_gui/'
#     AnimalTFDB_path = paste('./database/AnimalTFDB',sep='/')
#     print(AnimalTFDB_path)
#     file_list = list.files(AnimalTFDB_path)
#     file_list
#     tf_path = create_dir_function(paste(column_path,'AnimalTFDB',sep='/'))
#     file_list = file_list[grep('.txt',file_list)]
#     file_list
#     file_name = file_list[1]
#     
#     for(file_name in file_list){
#       print(file_name)
#       if(grepl('.txt',file_name)){
#         tf_file_path = create_dir_function(paste(tf_path,gsub('.txt','',file_name),sep='/'))
#         print(file_name)
#         cmd = paste("temp_table = read.table(file = '",paste(AnimalTFDB_path,file_name,sep='/'),"',sep='\t',header=TRUE)",sep='')
#         print(cmd)
#         eval(parse(text=cmd))
#         head(temp_table)
#         saveRDS(temp_table,paste(AnimalTFDB_path,sub('.txt','.rds',file_name),sep='/'))
#         dim(temp_table)
#         #rownames(temp_table) = temp_table[,2]
#         
#         stripped_list = gsub('9606.','',sig_mapped$STRING_id)
#         ensg_list = getBM(attributes = c("ensembl_gene_id",'ensembl_peptide_id'),
#                           filters = "ensembl_peptide_id", values = stripped_list,
#                           mart = mart
#                           ,uniqueRows = FALSE)
#         #names(ensg_list) = NULL      ensg_list
#         ensg_mapped_list = c()
#         n = 1
#         for(ensp in stripped_list){
#           #print(ensp)
#           entry_hit = NA
#           if(!is.na(ensp)){
#             entry_hit = unique(ensg_list[ensg_list$ensembl_peptide_id == ensp,'ensembl_gene_id'])
#             #print(entry_hit)
#             if(length(entry_hit) != 1){
#               if(length(entry_hit) > 1){
#                 print('######### ERROR ############')
#                 print(ensp)
#                 print(entry_hit)
#                 print('######### ERROR ############')
#               }
#               entry_hit = NA
#             }
#           }else{
#             entry_hit = NA
#           }
#           ensg_mapped_list = c(ensg_mapped_list,entry_hit)
#           n = n + 1
#           #print(n)
#         }
#         ensg_mapped_list
#         length(ensg_mapped_list)
#         sig_mapped$ensembl_gene_id = ensg_mapped_list
#         
#         overlap = intersect(ensg_list$ensembl_gene_id,temp_table[,2])
#         overlap
#         length(overlap)
#         hit_table = sig_mapped[sig_mapped$ensembl_gene_id %in% overlap,]
#         dim(hit_table)
#         colnames(hit_table)
#         print_table = hit_table[,c('id',cutoff_list_entry,t_test_list_entry,"STRING_id",'ensembl_gene_id')]
#         print_table[,cutoff_list_entry] = signif(print_table[,cutoff_list_entry],digits=3)
#         print_table[,t_test_list_entry] = signif(print_table[,t_test_list_entry],digits=3)
#         
#         #head(print_table)
#         write.table(print_table,file = paste(tf_file_path,'/',gsub('.txt','',file_name),'.txt',sep=''),sep='\t',row.names=FALSE,col.names=TRUE,quote = FALSE)      
#         detach_string()
#         par(xpd=FALSE,mar = c(1, 1, 4, 1) + 0.1,mfrow = c(1,1)) #c(bottom, left, top, right)
#         
#         string_db$plot_network(hit_table$STRING_id,payload_id=payload_id,add_link = TRUE)
#         try(save_plot_function_2(tf_file_path,paste(file_name,'STRING',sep='_')))
#         #string_enrichment(hit_table$STRING_id,tf_file_path,'',file_name,10)
#         tf_file_path
#         dual_enrichment_plot(hit_table,'STRING_id',cutoff_list_entry,'id',tf_file_path)
#       }
#     }
#   }
#   
#   
# }

common_mapped_function = function(shiny_data_path,sig_data_list,data_select_list,start_col,string_db,data_list,data_name_collapse){
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
  #cmd = paste0(entry,"_sig_mapped = readRDS('./GO_data/sig_mapped.",unlist(data_list[entry]),".rds')")
  #eval(parse(text = cmd))
  cmd = paste0(entry,"_sig_mapped$data = '",entry,"'")
  eval(parse(text = cmd))
  
  if(length(data_select_list) > 1){
    for(entry in data_select_list[2:length(data_select_list)]){ 
      cmd = paste0(entry,"_sig_mapped = sig_data_list[['",entry,"']]")
      
      #cmd = paste0(entry,"_sig_mapped = readRDS('./GO_data/sig_mapped.",unlist(data_list[entry]),".rds')")
      eval(parse(text = cmd))
      cmd = paste0(entry,"_sig_mapped$data = '",entry,"'")
      eval(parse(text = cmd))
      } # generate dataframes from rds files
  }
   
  print('reshape')
  #column_names = c('data','id','STRING_id','mean')
  if(length(data_select_list) > 0){
    
    
    cmd = paste0('data = ',data_select_list[1],'_sig_mapped')

    eval(parse(text = cmd))
 
    if('mean' %in% colnames(data)){
      column_names = c('data','id','STRING_id','mean')
    }else(
      column_names = c('data','id','STRING_id','slope')
    )

    sub_data = data[,column_names]
    colnames(sub_data) =   column_names = c('data','id','STRING_id','value')
    
    mapped_combined = sub_data
    if(length(data_select_list) > 1){
      for(entry in data_select_list[2:length(data_select_list)]){
        cmd = paste0('data = ',entry,'_sig_mapped')

        eval(parse(text = cmd))
        if('mean' %in% colnames(data)){
          column_names = c('data','id','STRING_id','mean')
        }else(
          column_names = c('data','id','STRING_id','slope')
        )
        sub_data = data[,column_names]
        colnames(sub_data) =   column_names = c('data','id','STRING_id','value')

        mapped_combined = rbind(mapped_combined,sub_data)
      }
    }
  } # combine all the data frames togther into a long dataset

  mapped_long_w = reshape(mapped_combined, idvar = c('id','STRING_id'), timevar = c('data'),direction = 'wide')
  i = grep('STRING_id',colnames(mapped_long_w))

  mapped_long_w[,c((i+1):length(colnames(mapped_long_w)))][is.na(mapped_long_w[,c((i+1):length(colnames(mapped_long_w)))])] = 0
  print(colnames(mapped_long_w))
  new_colnames = (sub('value.','',colnames(mapped_long_w)))
  colnames(mapped_long_w) = new_colnames
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
  
  results_list = list(mapped_data = mapped_long_w,
                      mapped_ud = mapped_ud,
                      payload_id = payload_id,
                      entry_list = entry_list,
                      colour_list = colour_list,
                      id_list = id_list)
  return(results_list)
}

common_melt_function = function(data_select_list, data_df_list, sig_data_list, data_df, timeseries_list){
  
  
  data_select_list = data_select_list[data_select_list != 'ESC']
  data_select_list
  
  data = data_df_list[[data_select_list[1]]]
  #cmd = paste0('data = ',data_select_list[1],'_mapped_all')
  #print(cmd)
  #eval(parse(text = cmd))
  #cmd = paste0('sig_data = ',data_select_list[1],'_mapped_all')
  sig_data = sig_data_list[[data_select_list[1]]]
  # if(data_select_list[1] %in% names(timeseries_list)){
  #   sig_data$ts = data_name
  # }else{
  #   sig_data$ts = NA
  # }
  #cmd = paste0("sig_data = readRDS('sig_mapped.",data_df_list[[data_select_list[1]]],".rds')")
  #print(cmd)
  #eval(parse(text = cmd))
  
  dname = data_df[data_select_list[1],'data']
  
  #dname = 'mean'
  #if(!'mean' %in% colnames(data)){
  #  dname = 'slope'
  #}
  dname
  data$sig = factor(sapply(data$id, function(x){
    y = ifelse(x %in% sig_data$id,ifelse(as.numeric(sig_data[sig_data$id == x,dname]) > 0,1,-1),character(0))
    y
  } ))
  data$data = data_select_list[1]
  print(colnames(data))
  col_index = c(2:(grep(dname,colnames(data))-1))
  col_index
  print(col_index)
  m_colnames = c("id",dname,"BH","STRING_id","data","variable","value",'sig')
  m = melt(data,measure.vars = col_index)
  print(colnames(m))
  
  m = m[,m_colnames]
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
      data$sig = factor(sapply(data$id, function(x){
        y = ifelse(x %in% sig_data$id,ifelse(as.numeric(sig_data[sig_data$id == x,dname]) > 0,1,-1),character(0))
        y
      }))
      print(dim(data))
      print(colnames(data))
      
      col_index = c(2:(grep(dname,colnames(data))-1))
      col_index
      n = melt(data,measure.vars = col_index)
      print(dim(n))
      n_colnames = c("id",dname,"BH","STRING_id","data","variable","value",'sig')
      n = n[,n_colnames]
      colnames(n) = m_colnames
      
      m = rbind(m,n)
      
    }
  }
  unique(m$data)
  dim(m)
  View(m)
  return(m)
 }


makePlotContainers <- function(gene_list, prefix="gplot") {
  ## Validate inputs
  #validateCssUnit(width)
  #validateCssUnit(height)
  print('makePlotContainers')
  ## Construct plotOutputs
  lst <- lapply(gene_list, function(gene) {
    output_name = paste(gene,prefix,sep = '_')
    print(output_name)
    plotOutput(paste(gene,prefix,sep = '_'))
    })
  
  ## Make columns
  #lst <- lapply(split(lst, (seq.int(n)-1)%/%ncol), function(x) column(12/ncol, x))
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

renderPlots = function(m, sample_list, gene_list, sample_path, input, output, prefix = 'gplot'){

  boxplot_path = paste0(sample_path,'/boxplot/')
  print(boxplot_path)
  create_dir_function(boxplot_path)
  print('gene_list')
  print(gene_list)
  for(gene in gene_list){
    local({

      
      print('gene')
      print(gene)
      
      output_name = paste(gene,prefix,sep = '_')
      name = paste(gene)
      print(output_name)
      sub_m = m[m$id == gene,]
      
      print(dim(sub_m))
      if(dim(sub_m)[1] > 0){
        col = sig_colour_function(sub_m)
        #col = c('red','green')
        output[[output_name]] = renderPlot({
          #pdf(paste('shiny_gplots/',gene,'.pdf'))
           p = ggplot(sub_m, aes(x = data,y = value,fill = sig)) + 
            geom_boxplot() +
            geom_hline(yintercept = 0) +
            scale_x_discrete(limits = sample_list) + 
            ggtitle(paste(name)) + 
             scale_fill_manual(breaks = c(-1,1),values = col) + 
            theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))
           #pdf(paste('/Users/sgarnett/Documents/Doctorate/Thesis/Thesis_Pictures/MQ_Gui/marker_lists/shiny_gplots//',name,'.pdf'))
           print(p)
           #dev.off()
           
           save_plot_function_2(boxplot_path,name)
          
        })
      }
    })
  }
}

renderPlots_ts = function(m, sample_list, gene_list, timeseries_list, sample_path, input, output, prefix = 'gplot'){
  
  boxplot_path = paste0(sample_path,'/boxplot_ts/')
  print(boxplot_path)
  create_dir_function(boxplot_path)
  print('gene_list')
  print(gene_list)
  for(gene in gene_list){
    local({
      
      
      print('gene')
      print(gene)
      
      output_name = paste(gene,prefix,sep = '_')
      name = paste(gene)
      print(output_name)
      sub_m = m[m$id == gene,]
      sub_m = sub_m[!is.na(sub_m$ts),]
      View(sub_m)
      print(dim(sub_m))
      if(dim(sub_m)[1] > 0){
        #col = sig_colour_function(sub_m)
        #col = c('red','green')
        output[[output_name]] = renderPlot({
          #pdf(paste('shiny_gplots/',gene,'.pdf'))
          
          p = ggplot(sub_m, aes(x = ts,y = value, col = data)) + 
            geom_boxplot() +
            stat_summary(fun.y=mean,geom='line',lwd=2,aes(group = data)) + 
            scale_x_discrete(limits = timeseries_list[['order']], name = '') +
            scale_y_continuous(name = 'log2(ratio vs Day 0)') + 
            ggtitle(paste(name)) +
            theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))
            
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
          
          save_plot_function_2(boxplot_path,name)
          
        })
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
    plotOutput(output_name)
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
              theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))
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

#setwd('/mnt/BLACKBURNLAB/2015/Neural_Stem_Cells/Data_for_Thesis/Final_Data/GO_data/')
#setwd('/Users/sgarnett/Documents/Doctorate/Thesis/Thesis_Data/Cleanup_Data/GO_data')
#setwd('../GO_data/GO_data/')
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

