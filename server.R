library(shiny)
shinyServer(function(input, output) {
  
  string_db = readRDS("string_db")
  
  
  #### Data Test #####
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
    
    return(paste(table_name,' \t : number of na = ',na_num,'  number of character(0) = ',char_num,'  number of CHARACTER(0)) = ',char_c_num, '  nchar > 0 = ',len_num))
  }
  
  output$table_test = renderText({
    p_list = c()
    for(entry in data_name_list){
      #print(entry)
      table_name = paste0(entry,'_mapped_all')
      #print(table_name)
      p = test_table_ids_function(table_name)
      p_list = c(p_list,p)
      
    }
    print(paste(p_list,collapse = ' <br> '))
  })
  
  output$table_name = renderText(paste("df.topGO_",input$limit,".",input$stat,".",input$ontology,".",input$data,".rds",sep=''))
  
  topGO_table = reactive({
    #load(paste('topGO_sig',input$ontology,input$data,sep='.'))
    topGO_table = readRDS(paste("df.topGO_",input$limit,".",input$stat,".",input$ontology,".",input$data,".rds",sep=''))
    topGO_table
    
  })
  
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
  
  output$term_list = renderUI({
    if(input$fix_term == 'n'){
      selectInput('term','Select GO Term',topGO_table()$Term)
    }else{
      selectInput('term','Select GO Term',input$term)
    }
  })
  
  mapped_st = reactive({
    if(input$mapped == 'common_mapped'){
      mapped = readRDS('common_mapped.rds')
    }else{
      mapped = readRDS(paste0(input$mapped,".",input$data,".rds"))
    }
    mapped
  }) # gets the STRINGdb mapping data
  
  full_gene_list = readRDS('gene_list.rds')
  
  output$gene_list = renderUI({
    #mapped_data = readRDS('common_mapped.rds')
    
    selectInput('genes','select genes',full_gene_list,multiple = T)
  }) # needs work currently only using GE list
  
  output$gene_list_2 = renderUI({
    #gene_list = readRDS('common_mapped.rds')
    selectInput('genes_2','select genes',full_gene_list,selected = gene_list, multiple = T)
  }) 
  
  output$gene_list_3 = renderUI({
    #gene_list = readRDS('common_mapped.rds')
    selectInput('genes_3','select genes',full_gene_list)
  }) 
  
  
  
  payload_id = reactive({
    if(input$mapped == 'common_mapped'){
      payload_id = readRDS('common_mapped_payload_id.rds')
    }else{
      payload_id = readRDS(paste0('payload_id.',input$data,'.rds'))
    }
    payload_id
  })
  
  output$topGO_sig = renderDataTable(topGO_table())
  
  
  output$file_list = renderText(go_files)
  
  output$m_name = renderText(paste0('m.enrich.',input$stat,'.',input$ontology,'.',input$data,'.rds'))
  
  m.enrich = reactive({
    m.enrich = readRDS(paste0('m.enrich.',input$stat,'.',input$ontology,'.',input$data,'.rds'))
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
  
  
  annot = readRDS("annot.df_GE_log2_ratio_different_all_MCT_all.rds")
  
  output$string_pic = renderImage(list(src='www/vacuolar_part_STRING_UP.pdf'))
  
  
  string_hits_list = reactive({
    mapped_data = mapped_st()
    GO_term = input$term
    GO_id = topGO_table()$GO.ID[topGO_table()$Term == GO_term]
    string_GO_members = annot$STRING_id[annot$term_id == GO_id]
    string_GO_members
    GO_members = string_GO_members
    STRING_hits = GO_members[GO_members %in% mapped_data$STRING_id]
    STRING_hits
    
  })
  
  output$term_num = renderText(paste( dim(topGO_table())[1],'terms'))
  
  output$sn_list = renderText(paste(length(string_hits_list()),'nodes'))
  observeEvent(input$run_sn, {
    output$sn = renderPlot({
      STRING_hits = string_hits_list()
      string_db$plot_network(STRING_hits,payload_id=payload_id(),add_link = FALSE)
    })
  })
  
  observeEvent(input$run_sn_link,{
    output$sn_url = renderText({
      STRING_hits = string_hits_list()
      get_link = string_db$get_link(STRING_hits,payload_id = payload_id())
      paste('<a href=',get_link,' target="_blank" class="btn btn-default">Go to stringdb.org</a>')
    })
  })
  
  pdf_file_name = reactive({
    metric = 'mean'
    if(input$data %in% timecourse_data){
      metric = 'slope'
    }
    file_name = paste0('..///STRINGdb/',input$data,'/',metric,'/',ontology_path_name[input$ontology],'/STRING/',latex_filename_function(input$term),'/',latex_filename_function(input$term),'_STRING.pdf')
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
    file_name = paste0('..///STRINGdb/GO_full_comparison/0.01/topGO/fisher_weight01/',ontology_path_name[input$ontology],'/single/crop/',latex_filename_function(input$term),'.pdf')
    file_name
  })
  
  output$GO_composite_pdf = renderText({
    
    file_name = GO_composite_file()
    file_name
    
    #return(paste('<iframe style="height:100px; width:1000px" src="', file_name, '"></iframe>', sep = ""))
    
    paste0('<div id="pdf">',
      '<object width="1000" height="100" type="application/pdf" data="',file_name,'?#zoom=400&scrollbar=0&toolbar=0&navpanes=0" id="pdf_content">',
      '<p>Insert your error message here, if the PDF cannot be displayed.</p>',
      '</object>',
      '</div>')
    
    # paste0('<object data="',file_name,'" type="application/pdf" width="1000px" height="150px">',
    #        '<embed WMODE="transparent" src="',file_name,'" type="application/pdf">',
    #        '<p>This browser does not support PDFs. Please download the PDF to view it: <a href="',file_name,'">Download PDF</a>.</p>',
    #        '</embed>',
    #        '</object>')
  })
  
  output$GO_composite_path = renderText(GO_composite_file())

  observeEvent(input$run_neigh,{
    output$neighbour_plot = renderPlot({
      mapped_data = mapped_st()
      gene_list = input$genes
      custom_list = string_db$mp(gene_list)
      custom_list
      neighbors = string_db$get_neighbors(custom_list)
      #string_db$get_interactions(custom_list)
      int = c(intersect(mapped_data$STRING_id,neighbors),custom_list)
      int
      string_db$plot_network(int,payload_id=payload_id(),add_link = FALSE)
    })
  })
  
  observeEvent(input$run_int,{
    output$interaction_plot = renderPlot({
      mapped_data = mapped_st()
      gene_list = input$genes_2
      custom_list = string_db$mp(gene_list)
      custom_list
      string_db$plot_network(custom_list,payload_id=payload_id(),add_link = FALSE)
    })
  })
  
  select_data = reactive({
    gene_list = input$genes_2
    print(gene_list)
    ESC_data = ESC_mapped_all[ESC_mapped_all$id %in% gene_list,]
    GE_data = GE_mapped_all[GE_mapped_all$id %in% gene_list,]
    SILAC_data = SILAC_mapped_all[SILAC_mapped_all$id %in% gene_list,]
    NES_data = NES_mapped_all[NES_mapped_all$id %in% gene_list,]
    NS_data = NS_mapped_all[NS_mapped_all$id %in% gene_list,]
    
    print('Select')
    print(ESC_data)
    list(ESC_data = ESC_data, GE_data = GE_data, SILAC_data = SILAC_data, NES_data = NES_data, NS_data = NS_data)
    
  })
  
  sig_data = reactive({
    
    ESC_data = select_data()$ESC_data
    GE_data = select_data()$GE_data
    SILAC_data = select_data()$SILAC_data
    NES_data = select_data()$NES_data
    NS_data = select_data()$NS_data
    print('sig')
    print(ESC_data)
    print(GE_data)
    print(SILAC_data)
    print(NES_data)
    print(NS_data)
    
    
 
    
    ESC_sig = ESC_data[ESC_data$BH < 0.05,]
    SILAC_sig = SILAC_data[SILAC_data$BH < 0.05,]
    GE_sig = GE_data[GE_data$BH < 0.05,]
    NES_sig = NES_data[NES_data$BH < 0.05,]
    NS_sig = NS_data[NS_data$BH < 0.05,]
    
    print(ESC_sig)
    print(SILAC_sig)
    print(GE_sig)
    print(NES_sig)
    print(NS_sig)
    
    list(ESC_data = ESC_sig,GE_data = GE_sig, SILAC_data = SILAC_sig, NES_data = NES_sig, NS_data = NS_sig)
  })
  
  heatmap_data = reactive({
    
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
    gene_list = input$genes_2
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
    print(plot_data)
    plot_data
    rownames(plot_data) = plot_data$id
    plot_data$id = NULL
    selected_data_list = c('ESC','GE','SILAC')
    
    selected_data_list = input$data_name
    print(selected_data_list)
    #print(names(selected_data_list))
    #selected_data_list = c('ESC','GE','SILAC')
    print(colnames(plot_data))
    plot_data = plot_data[rownames(plot_data)[order(rownames(plot_data))],selected_data_list]
    plot_data
    
    list(sig_mean = plot_data)
    
    #heatmap.2(as.matrix(plot_data),Rowv = F,Colv = F,dendrogram = c("none"),col = greenred(32))
  })
  
  output$selected_heatmap = renderPlot({
    plot_data = heatmap_data()$sig_mean
    col_pallete = colorRampPalette(c("green", "red"), space="rgb")(64)
    heatmap.2(as.matrix(plot_data),Rowv = F,Colv = F,dendrogram = c("none"),col = col_pallete,trace = 'none',cexRow = 1,cexCol = 1)
    
  })
  
  output$NES_Diff_heatmap = renderPlot({
    gene_list = input$genes_2
    
    data = NES_mapped_all
    ts_cols = c('id',NES_ts_cols)
    colnames(data)
    data = select_data()$NES_data
    selected_data = data[,ts_cols]
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
    
  })
  
  output$NS_Diff_heatmap = renderPlot({
    gene_list = input$genes_2
    
    data = NS_mapped_all
    ts_cols = c('id',NS_ts_cols)
    colnames(data)
    selected_data = data[data$id %in% gene_list,][,ts_cols]
    selected_data = delete.na(selected_data, length(ts_cols)-2)
    selected_data
    selected_data$id
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
    
  })
  
  

})



