library(STRINGdb)
library(gplots)
library(gtools)
library(topGO)
library(biomaRt)
library(reshape)

library(colorspace)
#library(plotly)

source('/blackburn3/scripts/CRUNCH/V2.3/maxquant_r_gui/00_functions/shiny_function.R')




data_list = c('ESC' = c('df_Huang2016_txt_edited_log2_ratio_different_mart_all_MCT_all_t_test' ),
              'GE' = c('df_GE_log2_ratio_different_all_MCT_all'), 
              'SILAC' = c('df_SILAC_all_comparison_all_MCT_all_mapping'),
              'NES-Diff' = c('df_Diff_LFQ_Sai_first_element_ratio_log2_all_MCT_all_mapping'),
              'NS-Diff' = c('df_Diff_LFQ_HB_first_element_ratio_log2_all_MCT_all_mapping')
)


setwd('/mnt/BLACKBURNLAB/2015/Neural_Stem_Cells/Data_for_Thesis/Final_Data/GO_data/')


print(data_list[1])

ESC_mapped_all = readRDS(paste0("all_mapped.",data_list['ESC'],".rds"))
#ESC_mapped$data = 'ESC'
GE_mapped_all = readRDS(paste0("all_mapped.",data_list['GE'],".rds"))
#GE_mapped$data = 'GE'
SILAC_mapped_all = readRDS(paste0("all_mapped.",data_list['SILAC'],".rds"))
#SILAC_mapped$data = 'SILAC'
all_combined = c(ESC_mapped_all$id,GE_mapped_all$id,SILAC_mapped_all$id)
all_combined = unique(all_combined)
length(all_combined)
saveRDS(all_combined,'gene_list.rds')


ESC_mapped = readRDS(paste0("sig_mapped.",data_list['ESC'],".rds"))
ESC_mapped$data = 'ESC'
GE_mapped = readRDS(paste0("sig_mapped.",data_list['GE'],".rds"))
GE_mapped$data = 'GE'
SILAC_mapped = readRDS(paste0("sig_mapped.",data_list['SILAC'],".rds"))
SILAC_mapped$data = 'SILAC'
#ESC_mapped = readRDS(paste0("sig_mapped.",data_list[''],".rds"))

View(SILAC_mapped)
column_names = c('data','id','STRING_id','mean')
mapped_combined = rbind(ESC_mapped[,column_names],GE_mapped[,column_names],SILAC_mapped[,column_names])
mapped_combined
mapped_long = reshape(mapped_combined, idvar = c('id','STRING_id'), timevar = c('data'),direction = 'wide')
View(mapped_long)

dim((SILAC_mapped[!is.na(SILAC_mapped$mean),]))
length(mapped_long$mean.SILAC[!is.na(mapped_long$mean.SILAC)])
setdiff(SILAC_mapped$id,mapped_long$id[!is.na(mapped_long$mean.SILAC)])
View(SILAC_mapped[SILAC_mapped$id == NULL])



dim((GE_mapped[!is.na(GE_mapped$mean),]))
length(mapped_long$mean.GE[!is.na(mapped_long$mean.GE)])
setdiff(GE_mapped$id,mapped_long$id[!is.na(mapped_long$mean.GE)])

dim((ESC_mapped[!is.na(ESC_mapped$mean),]))
length(mapped_long$mean.ESC[!is.na(mapped_long$mean.ESC)])
setdiff(ESC_mapped$id,mapped_long$id[!is.na(mapped_long$mean.ESC)])

mapped_long$mean.ESC[is.na(mapped_long$mean.ESC)] = 0
mapped_long$mean.GE[is.na(mapped_long$mean.GE)] = 0
mapped_long$mean.SILAC[is.na(mapped_long$mean.SILAC)] = 0


new_col_mix = function(col1,col2){
  if(is.na(col1)){
    col = col2
  }else{
    col = hex(mixcolor(0.5,hex2RGB(col1),hex2RGB(col2)))
  }
  return(col)
}


ESC_down = '#ffff00' # 'yellow'
ESC_up = '#0000FF' #'blue'
GE_down = '#98FB98' # palegreen'
GE_up = '#ee799f' # palevioletred
SILAC_down = '#00ff00' #'green'
SILAC_up = '#ff0000' # red'

entry_list = c('ESC down' = ESC_down)
entry_list['ESC up'] = ESC_up
entry_list['GE down'] = GE_down
entry_list['GE up'] = GE_up
entry_list['SILAC down'] = SILAC_down
entry_list['SILAC up'] = SILAC_up

entry_list['ESC down & GE down'] = new_col_mix(entry_list['ESC down'],entry_list['GE down'])
entry_list['ESC down & GE up'] = new_col_mix(entry_list['ESC down'],entry_list['GE up'])

entry_list['ESC up & GE down'] = new_col_mix(entry_list['ESC up'],entry_list['GE down'])
entry_list['ESC up & GE up'] = new_col_mix(entry_list['ESC up'],entry_list['GE up'])

entry_list['ESC down & SILAC up'] = new_col_mix(entry_list['ESC down'],entry_list['SILAC up'])
entry_list['ESC down & SIALC down'] = new_col_mix(entry_list['ESC down'],entry_list['SILAC down'])

entry_list['ESC up & SILAC up'] = new_col_mix(entry_list['ESC up'],entry_list['SILAC up'])
entry_list['ESC up & SIALC down'] = new_col_mix(entry_list['ESC up'],entry_list['SILAC down'])


entry_list['ESC up & GE up & SILAC up'] = new_col_mix(entry_list['ESC up & GE up'],entry_list['SILAC up'])



entry_list

saveRDS(entry_list,'legend_list.rds')


plot(c(1,30),c(30,1))
legend(1,30,legend = names(entry_list),fill = paste(entry_list),cex = 0.7)
    
  


for(i in c(1:dim(mapped_long)[1])){
  col = NA #black
  if(mapped_long$mean.ESC[i] < 0 ){
    new_col = ESC_down
    col = new_col_mix(col,new_col)
  }
  if(mapped_long$mean.ESC[i] > 0){
    new_col = ESC_up
    col = new_col_mix(col,new_col)
  }
  if(mapped_long$mean.GE[i] < 0){
    new_col = GE_down
    col = new_col_mix(col,new_col)
  }
  if(mapped_long$mean.GE[i] > 0){
    new_col = GE_up
    col = new_col_mix(col,new_col)
  }
  if(mapped_long$mean.SILAC[i] < 0){
    new_col = SILAC_down
    col = new_col_mix(col,new_col)
  }
  if(mapped_long$mean.SILAC[i] > 0){
    new_col =  SILAC_up
    col = hex(mixcolor(0.5,hex2RGB(col),hex2RGB(new_col)))
  }
  mapped_long[i,'col'] = col
  print(col)
}

string_db = readRDS("string_db")
payload_id = string_db$post_payload(mapped_long$STRING_id, colors=mapped_long$col)


saveRDS(mapped_long,'common_mapped.rds')
saveRDS(payload_id,'common_mapped_payload_id.rds')


