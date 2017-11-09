# this file serves as the global interface between UI and server


library(shiny)
library(gplots)

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

gene_list = c('SOX2','LIN28','TUBB3','GFAP')


setwd('../GO_data/')
topGO_enrich_cutoff = 0.05
ontology_path_name = c('CC' = 'Component', 'BP' = 'Process', 'MF' = 'Function')
timecourse_data = c('df_Diff_LFQ_Sai_first_element_ratio_log2_all_MCT_all_mapping','df_Diff_LFQ_HB_first_element_ratio_log2_all_MCT_all_mapping')
# Define server logic required to draw a histogram

#addResourcePath('images', '/mnt/BLACKBURNLAB/2015/Neural_Stem_Cells/Data_for_Thesis/Final_Data/images/')

data_name_list = c('ESC','GE','SILAC','NES_Diff','NS_Diff')
data_list = c('ESC' = c('df_Huang2016_txt_edited_log2_ratio_different_mart_all_MCT_all_t_test' ),
              'GE' = c('df_GE_log2_ratio_different_all_MCT_all'), 
              'SILAC' = c('df_SILAC_all_comparison_all_MCT_all_mapping'),
              'NES_Diff' = c('df_Diff_LFQ_Sai_first_element_ratio_log2_all_MCT_all_mapping'),
              'NS_Diff' = c('df_Diff_LFQ_HB_first_element_ratio_log2_all_MCT_all_mapping')
)

ESC_mapped_all = readRDS(paste0("all_mapped.",data_list['ESC'],".rds"))
colnames(ESC_mapped_all)
ESC_mapped_all$log2_ratio_list
ESC_cols = c('log2_ratio_list')
#ESC_mapped$data = 'ESC'
GE_mapped_all = readRDS(paste0("all_mapped.",data_list['GE'],".rds"))
GE_cols = c( "HB901_a__Sai1_a", "HB901_a__Sai1_b", "HB901_a__Sai2_a", "HB901_a__Sai2_b", "HB901_a__Sai3_a",
             "HB901_a__Sai3_b", "HB901_b__Sai1_a", "HB901_b__Sai1_b", "HB901_b__Sai2_a", "HB901_b__Sai2_b", "HB901_b__Sai3_a",
             "HB901_b__Sai3_b", "HB985_a__Sai1_a", "HB985_a__Sai1_b", "HB985_a__Sai2_a", "HB985_a__Sai2_b", "HB985_a__Sai3_a",
             "HB985_a__Sai3_b", "HB985_b__Sai1_a", "HB985_b__Sai1_b", "HB985_b__Sai2_a", "HB985_b__Sai2_b", "HB985_b__Sai3_a",
             "HB985_b__Sai3_b")
colnames(GE_mapped_all)
#GE_mapped$data = 'GE'
SILAC_mapped_all = readRDS(paste0("all_mapped.",data_list['SILAC'],".rds"))
colnames(SILAC_mapped_all)
SILAC_cols = c("S2L_H10H_SILAC_1", "S1L_H10H_SILAC_2", "S1L_H9H_SILAC_2",  "S2L_H10H_SILAC_2",
               "S2L_H9H_SILAC_2",  "S1H_H9L_SILAC_2r", "S3L1_H9H1_SILAC_3","S3L2_H9H2_SILAC_3","S3H1_H9L1_SILAC_3r",
               "S3H2_H9L2_SILAC_3r")
NES_Diff_mapped_all= readRDS(paste0("all_mapped.",data_list['NES_Diff'],".rds"))
NES_ts_cols =c("S1_00","S2_00","S3_00","S1_01",  "S2_01",  "S3_01",  "S1_02", 
               "S2_02",  "S3_02",  "S1_04",  "S2_04",  "S3_04",  "S2_08",  "S3_08",  "S1_12", 
               "S2_12",  "S3_12")
NS_Diff_mapped_all = readRDS(paste0("all_mapped.",data_list['NS_Diff'],".rds"))
NS_ts_cols = c("H9_00" , "H10_00", "H9_01",  "H10_01", "H9_02",  "H10_02", "H10_04",
                   "H9_08",  "H10_08", "H10_12")


limit_list = c('total','total.sig','total.sig.stat')
stat_list = c('classicFisher.bonferroni','fisher.weight01')
mapped_list = c('all_mapped','sig_mapped','sig_mapped_up','sig_mapped_down','common_mapped')

topGO_enrich_cutoff = 0.05
ontology_list = c('Cellular Component' = 'CC','Biological Process' = 'BP','Molecular Function' = 'MF')
ontology_path_name = c('CC' = 'Component', 'BP' = 'Process', 'MF' = 'Function')
