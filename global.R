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
#### CRAN packages ####
library(shiny)
library(gplots)
library(ggplot2)
theme_set(theme_bw())
library(gtools)

library(colortools)
library(colorspace)
library(reshape2)
library(venn)
library(shinyFiles)
#library(biomaRt)
library(DT)
library(stringr)

library(shinydashboard)

library(tidyr)
library(dplyr)
library(purrr)

library(beepr)
#parralel processing
library(promises)
library(future)

plan(multiprocess)

#library(RSQLite)
library(RMySQL)
localuserpassword <- "crunch"
db_type = 'MySQL'
load_database = function(db_type){
  if(db_type == 'MySQL'){
    db = storiesDb <- dbConnect(MySQL(), user='crunch', password=localuserpassword, dbname='crunch', host='127.0.0.1')
  }
  if(db_type == 'SQLite'){
    db <- dbConnect(RSQLite::SQLite(), 'data/sqlite.db')
  }
  return(db)
}
mydb = load_database(db_type)
db_table_list = dbListTables(mydb)
db_table_list
dbDisconnect(mydb)

# MySQL settings
# sudo /usr/local/mysql/support-files/mysql.server start
#create database crunch;
#GRANT ALL PRIVILEGES ON crunch.* TO 'crunch'@'localhost' IDENTIFIED BY 'crunch';

#library(biomaRt)


install_packages = F
if(install_packages == T){
  install.packages(c('shiny','gplots','ggplot2','gtools',
                     'colortools','colorspace',
                     'reshape2','venn','shinyFiles',
                     'DT','stringr','shinydashboard'))
  source("https://bioconductor.org/biocLite.R")
  biocLite(c("topGO",'STRINGdb','Rgraphviz','biomaRt'))
  
}


#### bioconductor packages ####
#source("https://bioconductor.org/biocLite.R")
#biocLite("topGO")
library(STRINGdb)
library(topGO)


string_access = F
save_test = F
autosave_datasets = F
dataset_test = F

base_path = "/mnt/MSD_128GB/"
function_path = paste0(base_path,"Doctorate/programming/CRUNCH/00_functions/")
source(paste(function_path,'os_functions.R',sep= '/'))
source(paste(function_path,'Graphs_functions.R',sep= '/'))
source('functions.R')
wd_path = paste0(base_path,"Doctorate/Thesis/Thesis_Data/Cleanup_Data/")
wd_path
setwd(wd_path)

dataset_files = list.files('data/data_list')
dataset_files
uploaded_datasets = c('_',gsub('_data_list.rds','',dataset_files))
#datasets


#uploaded_datasets = readRDS('data/uploaded_datasets.rds')
#uploaded_datasets
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
silac_ratio_column_list = c('','comparison_ratio','replicate_ratio','rev_comparison_ratio','rev_replicate_ratio','test_incorporation','intensity')

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


#### save variable functions ####




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

