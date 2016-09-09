# Created by Benjamin Erickson  ----

# expandTk <- function(){
version_num <- 'Ben Tools v.07.b'
# For plotting Bentley lab table files and generating gene list

kHeight <- 5 # 1 for smaller screens and PCs
kWidth <- 30 # use smaller for small screens
# load packages ----

my_packages <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

suppressPackageStartupMessages(my_packages(c("ggplot2", 
                                             "tcltk2", 
                                             "dplyr", 
                                             "fastcluster",
                                             "RColorBrewer")))

# values for comboboxs ----

kDotOptions <- c("none", "square", "circle", "triangle point up", "diamond", 
                "Biger circle", "smaller circle")
kLineOptions <- c("solid line", "dashed line", "dotted line", "dot dash line", 
                 "long dash line", "two dash line", "No line")
kBrewerList <- c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3")
kListColorSet <- brewer.pal(8, kBrewerList[3])
kMathOptions <- c("mean", "sum", "median")
kTopBottomOptions <- c("Top%", "Bottom%") 
kInOutOptions <- c("inclusive", "exclusive") 
kTopBottomNum <- c(1, 2, seq(5, 95, by = 5), 99, 100)
kAccDec <- c("Accend", "Deccend")
kQuntile <- c("0%", "1%", paste0(seq(5, 45, 5),"%"))
kFoldList <- round(c(1, seq(1.1, 2, by = 0.2), 2, seq(2.1, 4, by = 0.2), 4), digits = 4)
# plot lines and ticks with labels
list_plot_lines <- list("543 bins 20,20,40" = c(15.5, 45.5, 20.5, 40.5),
                        "543 bins 10,10,10" = c(5.5, 25.5, 10.5, 20.5),
                        "5 prim 2k 2k 40bins" = c(20.5, "", "", ""),
                        "5 prim 1k 1k 80bins" = c(40.5, "", "", ""))
list_plot_ticks <- list("543 bins 20,20,40" = 
  list('name' = c('-1450 -950 -450 +450 +950 +1450 +1950 +2450 +2950 +3450'),
        'loc' = c(1, 6, 11, 50, 55, 60, 65, 70, 75, 80)),
                        "543 bins 10,10,10" = 
  list('name' = c('-450', '+450'),
         'loc' = c(1,30)),
  "5 prim 2k 2k 40bins" = 
    list('name' = c('-2000', '-1000', '-500', '500', '100', '2000'),
         'loc' = c(1, 10, 15, 25, 30, 40)),
  "5 prim 1k 1k 80bins" = 
    list('name' = c('-1000', '-500', '500', '1000'),
         'loc' = c(1, 20, 60, 80)))
tss_tts_options <- c('TSS', 'PolyA', '500', '500')
 

# tcl starting values ----
tcl_commonfile <- tclVar("All common genes")
tcl_gene1file <- tclVar("gene1")
tcl_gene2file <- tclVar("gene2")
tcl_gene3file <- tclVar("gene3")
tcl_gene4file <- tclVar("gene4")
tcl_list1file <- tclVar("cluster 1")
tcl_list2file <- tclVar("cluster 2")
tcl_list3file <- tclVar("cluster 3")
tcl_list4file <- tclVar("cluster 4")
tcl_listq1file <- tclVar("quantile 1")
tcl_listq2file <- tclVar("quantile 2")
tcl_listq3file <- tclVar("quantile 3")
tcl_listq4file <- tclVar("quantile 4")
tcl_toolfile <- tclVar("tool")
tcl_line_option <- tclVar(kLineOptions[1])
tcl_dot_option <- tclVar(kDotOptions[1])
tcl_top_bottom_option <- tclVar(kTopBottomOptions[1])
tcl_top_bottom_num <- tclVar(kTopBottomNum[7])
tcl_acc_dec <- tclVar(kAccDec[2])
tcl_acc_dec_cdf <- tclVar(kAccDec[2])
tcl_quntile_cdf_bottom <- tclVar(kQuntile[1])
tcl_quntile_cdf_top <- tclVar(kQuntile[1])
tcl_bin_fold_region <- tclVar(kFoldList[6])
tcl_in_out_option_ratios <- tclVar(kInOutOptions[1])
tcl_bin_fold_ratios <- tclVar(kFoldList[6])
tcl_math_option <- tclVar(kMathOptions[1])
tcl_norm_bin <- tclVar(0)
tcl_bin_start <- tclVar(1)
tcl_bin_end <- tclVar(1)
tcl_brewer <- tclVar(kBrewerList[3])
tcl_bin_start_region <- tclVar(1)
tcl_bin_end_region <- tclVar(1)
tcl_bin_start1_ratios <- tclVar(1)
tcl_bin_end1_ratios <- tclVar(1)
tcl_bin_start2_ratios <- tclVar(1)
tcl_bin_end2_ratios <- tclVar(1)
tcl_bin_start1_cdf <- tclVar(1)
tcl_bin_end1_cdf <- tclVar(1)
tcl_bin_start2_cdf <- tclVar(1)
tcl_bin_end2_cdf <- tclVar(1)
tcl_bin_start_cluster <- tclVar(1)
tcl_bin_end_cluster <- tclVar(1)
tcl_bin_cluster_num <- tclVar(4)
tcl_bin_start_sort <- tclVar(1)
tcl_bin_end_sort <- tclVar(1)
tcl_plot_line_name <- tclVar(names(list_plot_lines)[1])
tcl_one_tss_tts_option <- tclVar(tss_tts_options[1])
tcl_two_tss_tts_option <- tclVar(tss_tts_options[2])
tcl_three_tss_tts_option <- tclVar(tss_tts_options[3])
tcl_four_tss_tts_option <- tclVar(tss_tts_options[4])
tcl_label_plot_ticks <- tclVar(list_plot_ticks[[1]][[1]])
tcl_pos_one_line <- tclVar(list_plot_lines[[1]][1])
tcl_pos_two_line <- tclVar(list_plot_lines[[1]][2])
tcl_pos_three_line <- tclVar(list_plot_lines[[1]][3])
tcl_pos_four_line <- tclVar(list_plot_lines[[1]][4])
tcl_pos_plot_ticks <- tclVar(list_plot_ticks[[1]][[2]])
tcl_checkbox_relative_frequency <- tclVar(0)
tcl_checkbox_relative_gene_frequency <- tclVar(0)
tcl_checkbox_log2 <- tclVar(0)
tcl_checkbox_comment <- tclVar(0) # 1 = don't show save comment popup
tcl_checkbox_split <- tclVar(0) # change the output of saved gene lists
tcl_checkbox_cdf_innerjoin <- tclVar(1) # merge the output lists or not

# file list varibles  ----   

# for holding table files in list
LIST_DATA <- list(table_file = list(), # [[]] gene X1 X2 ...
                  gene_file = list(),  # holds $common genes from files and $gene file(s)
                  gene_info = list(),  # for holding gene file info in a list of lists, a set for $common and each $gene file(s) [c("dot", "line", "color", plot?, NickName)]
                  clust = list())      # Cluster holder

STATE <- c(0, 'common', 'cluster') # [1] for open option window, [2] for a selected box, [3] tool use help

# functions ----

# tests weather a option window is open and promps you to close it
OpenWindowControl <- function(){
  if(STATE[1] == 1){
    STATE[1] <<- 0
    tkdestroy(SampleOptionsFrame)
    tkraise(root)
  } else if(STATE[1] == 3){
    STATE[1] <<- 0
    tkdestroy(CommentFrame)
    tkraise(root)
  } else if(STATE[1] == 4){
    STATE[1] <<- 0
    tkdestroy(OptionsFrame)
    tkraise(root)
  }
  if(STATE[1] == 2){
    # tklower(root)
    # tkmessageBox(message = "close window please")
  }
}

# if option window is destroyed update state
OnDestroy <- function(){
  STATE[1] <<- 0
  tkraise(root)
}

# clears selection of boxes 
SelectionControl <- function(box_set, on_off){
  tkselection.clear(get(paste("listbox_" , STATE[2], on_off, sep = "")), 0, 'end')
  if ( box_set != STATE[2]){
    onlist <- get(paste("listbox_" , STATE[2], "_on", sep = ""))
    offlist <- get(paste("listbox_" , STATE[2], "_off", sep = ""))
    tkselection.clear(onlist, 0, 'end')
    tkselection.clear(offlist, 0, 'end')
    STATE[2] <<- box_set
  }
}

# moves all items from one list to the other
MoveAllToOtherEntry <- function(onlist, offlist, direction, file_name = "common"){
  if(length(LIST_DATA$table_file) > 0){
    listnames <- names(LIST_DATA$gene_info[[file_name]])
    
    if (direction == "on"){
      tkdelete(offlist, 0, 'end')
      tkdelete(onlist, 0, 'end')
      tkconfigure(onlist, listvariable = tclVar(listnames))
      lc <- 0
      for(i in listnames){
        LIST_DATA$gene_info[[file_name]][[i]][4] <<- 1
        tkitemconfigure(onlist, lc, foreground = LIST_DATA$gene_info[[file_name]][[i]][3])
        lc <- lc + 1
      }
    }
    if (direction == "off"){
      tkdelete(offlist, 0, 'end')
      tkdelete(onlist, 0, 'end')
      tkconfigure(offlist, listvariable = tclVar(listnames))
      lc <- 0
      for(i in listnames){
        LIST_DATA$gene_info[[file_name]][[i]][4] <<- 0
        tkitemconfigure(offlist, lc, foreground = LIST_DATA$gene_info[[file_name]][[i]][3])
        lc <- lc + 1
      }
    }
  }
}

#moves selected items from one list to the other
MoveSelectToOtherEntry <- function(onlist, offlist, file_name = "common"){
  for (i in rev(as.integer(tkcurselection(onlist)))){
    tkinsert(offlist, "end", tclvalue(tkget(onlist, i)))
    LIST_DATA$gene_info[[file_name]][[tclvalue(tkget(onlist, i))]][4] <<- 0
    tkitemconfigure(offlist, "end", foreground = LIST_DATA$gene_info[[file_name]][[tclvalue(tkget(onlist, i))]][3])
    tkdelete(onlist, i)
  }
  for (i in rev(as.integer(tkcurselection(offlist)))){
    tkinsert(onlist, "end", tclvalue(tkget(offlist, i)))
    LIST_DATA$gene_info[[file_name]][[tclvalue(tkget(offlist, i))]][4] <<- 1
    tkitemconfigure(onlist, "end", foreground = LIST_DATA$gene_info[[file_name]][[tclvalue(tkget(offlist, i))]][3])
    tkdelete(offlist, i)
  }
}

# keeps numbers, empty string for the rest
# from https://github.com/gsk3/taRifx/blob/master/R/Rfunctions.R#L1161
Destring <- function(x, keep = "0-9.-") {
  return(as.numeric(gsub(paste("[^", keep, "]+", sep=""), "", x)))
}

# basic test for valid colors
isColor <- function(x) {
  res <- try(col2rgb(x),silent=TRUE)
  return(!"try-error"%in%class(res))
}

# helper for keeping start and end bins in check
BinStartEndHelper <- function(startbin, endbin, combostart, comboend, num) {
  start <- as.numeric(tclvalue(startbin))
  end <- as.numeric(tclvalue(endbin))
  if (num == 1 && start > end) {
    tkset(comboend, start)
  } else if (num == 2 && start > end) {
    tkset(combostart, end)
  }
}

# find quantiles
FindQuantile <- function() {
  if (as.integer(tksize(listbox_active_cluster)) < 1) {
    return ()
  }
  STATE[3] <<- "quantile"
  pb <<- tkProgressBar(title = "Finding clusters, please be patient!!",
                       width = 300)
  setTkProgressBar(pb, 100, label =  "Finding clusters, please be patient!!")
  
  R_start_bin <- as.integer(tclvalue(tcl_bin_start_cluster)) + 1
  R_stop_bin <- as.integer(tclvalue(tcl_bin_end_cluster)) + 1
  
  nick_name <-  strsplit(sub('-', '\n!', 
                             as.character(tkget(listbox_active_cluster, 0, 'end'))), '\n!')[[1]]
  my_ref  <- LIST_DATA$gene_info[[nick_name[1]]][[nick_name[2]]][5]
  enesg <- data.frame(gene = LIST_DATA$gene_file[[nick_name[1]]]$use, stringsAsFactors = F) # here
  df <- list()
  df[[nick_name[2]]] <- inner_join(enesg, LIST_DATA$table_file[[my_ref]], by = 'gene')
  df[[2]] <- data.frame(gene = df[[nick_name[2]]]$gene, value = rowMeans(df[[nick_name[2]]][,R_start_bin:R_stop_bin][ ,-1], na.rm = T), stringsAsFactors = F)
  qq <- quantile(df[[2]]$value, na.rm = TRUE)
  enesg1 <- df[[2]]$gene[df[[2]]$value < qq[2]]
  enesg2 <- df[[2]]$gene[df[[2]]$value >= qq[2] & df[[2]]$value < qq[3]]
  enesg3 <- df[[2]]$gene[df[[2]]$value >= qq[3] & df[[2]]$value < qq[4]]
  enesg4 <- df[[2]]$gene[df[[2]]$value >= qq[4]]
  clist <- list()
  gene_info <- list(list())
  for( i in 1 : 4){
    color_safe <- i %% length(kListColorSet)
    if (color_safe == 0) {
      color_safe <- 1
    }
    my_gene_list <- unlist(get(paste("enesg", i, sep = "")))
    if(length(my_gene_list) > 0){
      clist[[paste("listq", i, sep = "")]]$use <- my_gene_list
      color_select <- kListColorSet[color_safe]
      gene_info[[paste("listq", i, sep = "")]][[my_ref]] <- c(kDotOptions[1],
                                                             kLineOptions[1], 
                                                             color_select, 
                                                             1,
                                                             nick_name[2])
    }
    gene_list_label <- get(paste("label_cluster_list", i, sep = ""))
    gene_list <- get(paste("listbox_gene_cluster_list", i, sep = ""))
    tkconfigure(gene_list, listvariable = tclVar(my_gene_list))
    tkconfigure(gene_list_label, text = paste('quantile', i, ' n = ', (as.integer(tksize(gene_list)))))
  }
  close(pb)
  MakeDataFrame(sel_list = NULL, table_file = df,
                gene_file = clist, gene_info = gene_info)
  tkraise(root)
}

# Change the number of clusters
ClusterNumList <- function(cm){
  if(as.numeric(tksize(listbox_active_cluster)) < 1 | length(cm) < 1 | STATE[3] != 'cluster'){
    return()
  }
  DActLst(entry_cluster_search, sapply(c(1:4), function(x){
    paste("listbox_gene_cluster_list" , x, sep = "")}),
    sapply(c(1:4), function(x){
      paste("label_cluster_list" , x, sep = "")}))
  R_cluster <- as.integer(tclvalue(tcl_bin_cluster_num))
  nick_name <-  strsplit(sub('-', '\n!', 
                             as.character(tkget(listbox_active_cluster, 0, 'end'))), '\n!')[[1]]
  my_ref  <- LIST_DATA$gene_info[[nick_name[1]]][[nick_name[2]]][5]
  enesg <- data.frame(gene = LIST_DATA$gene_file[[nick_name[1]]]$use, stringsAsFactors = F) # here
  if(length(enesg$gene) < R_cluster){
    return()
  }
  df <- list()
  df[[nick_name[2]]] <- inner_join(enesg, LIST_DATA$table_file[[my_ref]], by = 'gene')
  clist <- list()
  gene_info <- list(list())
  for( i in 1 : R_cluster){
    clist[[paste("list", i, sep = "")]]$use <- df[[nick_name[2]]][cutree(cm, R_cluster) == i,][,1]
    
    color_safe <- i %% length(kListColorSet)
    if (color_safe == 0) {
      color_safe <- 1
    }
    
    color_select <- kListColorSet[color_safe]
    gene_info[[paste("list", i, sep = "")]][[my_ref]] <- c(kDotOptions[1],
                                                           kLineOptions[1], 
                                                           color_select, 
                                                           1,
                                                           nick_name[2])
    gene_list_label <- get(paste("label_cluster_list", i, sep = ""))
    gene_list <- get(paste("listbox_gene_cluster_list", i, sep = ""))
    tkconfigure(gene_list, listvariable = tclVar(paste(clist[[i]]$use)))
    tkconfigure(gene_list_label, text = paste('cluster', i, ' n = ', (as.integer(tksize(gene_list)))))
  }
  MakeDataFrame(sel_list = NULL, table_file = df,
                gene_file = clist, gene_info = gene_info)
  
}

# helper for finding what gene list is active
ListBoxSelectHelper <- function(listboxgene){
  if (listboxgene[1] == "region"){
    my_tab <- tk2notetab.text(notebook_region)
    if(my_tab == "File 2 up"){
      listboxgene <- listbox_gene_region_down
    } else if(my_tab == "Fold inbetween"){
      listboxgene <- listbox_gene_region_between
    } else{
      listboxgene <- listbox_gene_region_up
    }
  } else if (listboxgene[1] == "ratios"){
    my_tab <- tk2notetab.text(notebook_ratios)
    if(my_tab == "File 2 up"){
      listboxgene <- listbox_gene_ratios_down
    } else if(my_tab == "Fold inbetween"){
      listboxgene <- listbox_gene_ratios_between
    } else{
      listboxgene <- listbox_gene_ratios_up
    }
  } else if (listboxgene[1] == "cluster"){
    my_tab <- strsplit(tk2notetab.text(notebook_cluster), split = " ")[[1]]
    listboxgene <- get(paste("listbox_gene_cluster_list" ,my_tab[2] , sep = ""))
  } else if (listboxgene[1] == "intersect"){
    my_tab <- tk2notetab.text(notebook_intersect)
    if(my_tab == "Inclusive"){
      listboxgene <- get("listbox_intersect_inclusive")
    } else {
      listboxgene <- get("listbox_intersect_exclusive")
    }
  } else if (listboxgene[1] == "cdf"){
    my_tab <- strsplit(tk2notetab.text(notebook_cluster), split = " ")[[1]]
    listboxgene <- get(paste("listbox_gene_cdf_list" ,my_tab[2] , sep = ""))
  }
  return(listboxgene)
}

# helper for ploting selected gene list
SelectGeneListPlotHelper <- function(listboxgene){
  listname <- tolower(strsplit(tk2notetab.text(notebook_tool),split = "\n")[[1]][1])
  
  if(listname == "sort"){
    legend_nickname <- paste(listname, "tool")
  } else if(listboxgene == 'cluster'){
    legend_nickname <- paste(STATE[3], "tool", tk2notetab.text(get(paste0("notebook_",listname))), "tab")
  } else {
    legend_nickname <- paste(listname, "tool", tk2notetab.text(get(paste0("notebook_",listname))), "tab")
  }
  
  if (is.character(listboxgene)){
    listboxgene <- ListBoxSelectHelper(listboxgene)
  }
  
  num <- as.integer(tkcurselection(listboxgene))
  sel <- NULL
  for(i in num){
    sel <- c(sel, tclvalue(tkget(listboxgene,(i))))
  }
  MakeDataFrame(sel, nickname = legend_nickname)
}

# helper for plotting all genes in sub list
GeneListPlotHelper <- function(listboxgene){
  listname <- tolower(strsplit(tk2notetab.text(notebook_tool),split = "\n")[[1]][1])
  if(listname == "sort"){
    legend_nickname <- paste(listname, "tool")
  } else if(listboxgene == 'cluster'){
    legend_nickname <- paste(STATE[3], "tool", tk2notetab.text(get(paste0("notebook_",listname))), "tab")
  } else {
    legend_nickname <- paste(listname, "tool", tk2notetab.text(get(paste0("notebook_",listname))), "tab")
  }
  
  if (is.character(listboxgene)){
    listboxgene <- ListBoxSelectHelper(listboxgene)
  }
  if(as.integer(tksize(listboxgene)) > 0){
    MakeDataFrame(paste(tkget(listboxgene, 0, 'end')), nickname = legend_nickname)
  }
}

# helper for making new gene list for tool tab
ToolListTabHelper <- function(){
  listname <- tolower(strsplit(tk2notetab.text(notebook_tool),split = "\n")[[1]][1])
  if(listname == "sort"){
    legend_nickname <- paste(listname, "tool")
    listboxname <- listbox_active_gene_sort
  } else {
    legend_nickname <- paste(listname, "tool", tk2notetab.text(get(paste0("notebook_",listname))), "tab")
    listboxname <- ListBoxSelectHelper(listname)
  }
  my_list <- as.character(tkget(listboxname, 0, 'end'))
  if(length(my_list) > 0){
    LIST_DATA$gene_file$tool$full <<- my_list
    LIST_DATA$gene_file$tool$use <<- my_list
    LIST_DATA$gene_info$tool <<- lapply(setNames(names(LIST_DATA$gene_info$common),
                                                 names(LIST_DATA$gene_info$common)),
                                        function(i) c(kDotOptions[1],
                                                      kLineOptions[1], 
                                                      LIST_DATA$gene_info$common[[i]][3], 
                                                      1,
                                                      LIST_DATA$gene_info$common[[i]][5]))
    
    tclvalue(tcl_toolfile) <<- legend_nickname
    tkconfigure(label_tool_file, textvariable = tcl_toolfile)
    tkconfigure(label_tool_length, text = paste("n = ", length(my_list)))
    MoveAllToOtherEntry(listbox_tool_on, listbox_tool_off, "on", file_name = "tool") 
  }
}

# clears tools
DActLst <- function(listboxfile, listboxgenes, listboxlengths){
  tkdelete(listboxfile, 0, 'end')
  if(!is.null(listboxgenes)){
    for(i in listboxgenes){
      tkdelete(get(i), 0, "end")
    }
  }
  if(!is.null(listboxlengths)){
    for(i in listboxlengths){
      tkconfigure(get(i), text = paste("n = 0"))
    }
  }
}

# clears all tools
DActAll <- function(){
  DActLst(listbox_active_sort, "listbox_active_gene_sort", 
          "label_active_sort_length")
  
  DActLst(listbox_active_region, sapply(c("up","down","between"), function(x){
    paste("listbox_gene_region_" , x, sep = "")}),
  sapply(c("up","down","between"), function(x){
    paste("label_region_" , x, sep = "")}))
  
  DActLst(listbox_active_ratios, sapply(c("up","down","between"), function(x){
    paste("listbox_gene_ratios_" , x, sep = "")}),
    sapply(c("up","down","between"), function(x){
      paste("label_ratios_" , x, sep = "")}))
  
  DActLst(listbox_active_cluster, sapply(c(1:4), function(x){
    paste("listbox_gene_cluster_list" , x, sep = "")}),
    sapply(c(1:4), function(x){
      paste("label_cluster_list" , x, sep = "")}))
  
  DActLst(listbox_active_intersect, c("listbox_intersect_inclusive", 
                                      "listbox_intersect_exclusive", "listbox_intersect_combind"),
          c("label_intersect_inclusive", "label_intersect_exclusive", "label_intersect_combind"))
  
  DActLst(listbox_active_cdf, sapply(c(1:4), function(x){
    paste("listbox_gene_cdf_list" , x, sep = "")}),
    sapply(c(1:4), function(x){
      paste("label_cdf_list" , x, sep = "")}))
}

# adds selected item(s) to active tool list
ActLst <- function(ActList, MyMax){
  InList <- get(paste("listbox_" , STATE[2], "_on", sep = ""))
  sel <- as.integer(tkcurselection(InList))
  for (i in sel){
    new_name <- paste(STATE[2], tclvalue(tkget(InList, i)), sep = '-')
    tkinsert(ActList, 0, new_name)
    if(as.integer(tksize(ActList)) == MyMax){
      tkdelete(ActList, "end")
    }
  }
  outList <- get(paste("listbox_" , STATE[2], "_off", sep = ""))
  sel <- as.integer(tkcurselection(outList))
  for (i in sel){
    new_name <- paste(STATE[2], tclvalue(tkget(outList, i)), sep = '-')
    tkinsert(ActList, 0, new_name)
    if(as.integer(tksize(ActList)) == MyMax){
      tkdelete(ActList, "end")
    }
  }
}

# reads in file, tests, fills out info 
LoadTableFile <- function() {
  if (is.null(names(LIST_DATA$table_file))) {
      file_count <- 0
    } else {
      file_count <- length(LIST_DATA$table_file)
      if (file_count > 12) { 
        tkmessageBox(message = "I have lots of files, 
                   you might want to remove some files")
      }
  }
  
  STATE[1] <<- 2
  full_file_name <- paste(as.character(tkgetOpenFile(multiple = TRUE, filetypes = 
                                               "{{Table Files} {.table .tab .Table}} {{All files} *}")))
  STATE[1] <<- 0
  if (!nchar(full_file_name[1])) { ## file select test   
    tkraise(root)
    return ()
  }
  pb <- tkProgressBar(title = "Loading file, please be patient!!",
                      width = 500)
  gene_list <- NULL
  for(x in full_file_name){
    file_name <- paste(strsplit(as.character(x), 
                              '/')[[1]][(length(strsplit(as.character(x), 
                                                         '/')[[1]]))])
    legend_nickname <- strsplit(as.character(file_name), '.tab')[[1]][1]
    if (any(legend_nickname == names(LIST_DATA$table_file))) {
      tkmessageBox(message = "This file has already been loaded")
      next ()
    } else {
      tablefile <- read.table(x, header = TRUE, 
                              stringsAsFactors = FALSE, comment.char = "")
      names(tablefile)[1] <- paste("gene")
      num_bins <- dim(tablefile)
      if (file_count > 0) {
          if (num_bins[2] != length(LIST_DATA$table_file[[1]])) {
            tkmessageBox(message = "Can't load file, different number of bins")
            break ()
          }
        if(!exists("gene_names")){
          gene_names <- LIST_DATA$gene_file$common$full
        }
        gene_names <- c(unique(tablefile[ ,1]), gene_names)
        gene_names <- gene_names[duplicated(gene_names)]

        if (length(gene_names) > 0) {
          LIST_DATA$gene_file$common$full <<- gene_names
          DActAll()
        } else {
          close(pb)
          tkmessageBox(message = "Can't load file, no genes in common or 
                       remake your table files all the same way.")
          break ()
          }
        } else {  # first time loading a file set up
          gene_names <- unique(tablefile[ ,1])
          file_count <- 1
          LIST_DATA$gene_file$common$full <<- unique(tablefile[ ,1])
          LIST_DATA$gene_info$common[[legend_nickname]] <<- c(kDotOptions[1],
                                           kLineOptions[1],
                                           kListColorSet[1], 
                                           1,
                                           legend_nickname)
          SetComboBoxes(num_bins) # fills out and sets start and stop bins
        }
        color_safe <- (length(LIST_DATA$table_file)+1) %% length(kListColorSet)
        if (color_safe == 0) {
          color_safe <- 1
        }
        color_select <- kListColorSet[color_safe]
      
        # generate info for new file for $common list and each $gene file loadded
        LIST_DATA$gene_info <<- lapply(LIST_DATA$gene_info, function(k) {
          k[[legend_nickname]] <- c(kDotOptions[1],
                                    kLineOptions[1],
                                    color_select, 
                                    1,
                                    legend_nickname)
          k
        })
        
        LIST_DATA$table_file[legend_nickname] <<- list(tablefile)
    }
    setTkProgressBar(pb, legend_nickname, label = paste("loading " , legend_nickname))
      sapply(names(LIST_DATA$gene_file), function(x){
        onlist <- get(paste("listbox_" , x, "_on", sep = ""))
        if(length(LIST_DATA$gene_file[[x]]$full) > 0){
          tkinsert(onlist, 'end', legend_nickname)
          onlabel <- get(paste("label_" , x, "_length", sep = ""))
          enesg <- c(gene_names, LIST_DATA$gene_file[[x]]$full)
          enesg <- enesg[duplicated(enesg)]
          LIST_DATA$gene_file[[x]]$use <<- enesg
          tkconfigure(onlabel, text = paste("n = " , 
                                            length(enesg)))
          tkitemconfigure(onlist, 'end', foreground = color_select)
        }
      })
  }
      tkconfigure(combobox_numerator, 
                  values = names(LIST_DATA$gene_info$common))
      tkconfigure(combobox_denominator, 
                  values = names(LIST_DATA$gene_info$common))
    close(pb)
    tkraise(root)
    return()
}

# reads in gene list files
LoadGeneFile <- function(listboxname) {
  on_listbox <- get(paste("listbox_" , listboxname, "_on", sep = ""))
  off_listbox <- get(paste("listbox_" , listboxname, "_off", sep = ""))
  label_file <- get(paste("label_" , listboxname, "_file", sep = ""))
  myTCL <- get(paste0("tcl_", listboxname, "file"))
  label_count <- get(paste("label_" , listboxname, "_length", sep = ""))
  
  file_count <- length(LIST_DATA$table_file)
  if (file_count < 1) {
    return ()
  }
  STATE[1] <<- 2
  full_file_name <- tclvalue(tkgetOpenFile(filetypes = 
                                               "{{Gene List} {.txt}} {{All files} *}"))
  STATE[1] <<- 0
  if (!nchar(full_file_name)) {
    tkraise(root)
    LIST_DATA$gene_file[[listboxname]] <<- NULL
    LIST_DATA$gene_info[[listboxname]] <<- NULL
    tclvalue(myTCL) <- listboxname
    tkconfigure(label_file, textvariable = myTCL)
    tkconfigure(label_count, text = "n = 0")
    tkdelete(on_listbox, 0, 'end')
    tkdelete(off_listbox, 0, 'end')
    return ()
    } else {
      file_name <- paste(strsplit(as.character(full_file_name), 
                                  '/')[[1]][(length(strsplit(as.character(full_file_name), 
                                                             '/')[[1]]))])
      legend_nickname <- strsplit(as.character(file_name), '.txt')[[1]][1]
      
      genefile <- read.table(full_file_name, stringsAsFactors = FALSE,
                             header = FALSE)
     
      enesg <- c(unique(genefile[ , 1]), 
                 LIST_DATA$gene_file$common$full)
      enesg <- enesg[duplicated(enesg)]
      if (length(enesg) == 0) {
        tkmessageBox(message = "No genes in common with loaded files")
        tkraise(root)
        return ()
      }
      LIST_DATA$gene_file[[listboxname]]$full <<- unique(genefile[ , 1])
      LIST_DATA$gene_file[[listboxname]]$use <<- enesg
      LIST_DATA$gene_info[[listboxname]] <<- lapply(setNames(names(LIST_DATA$gene_info$common),
                                                                  names(LIST_DATA$gene_info$common)),
                                                         function(i) c(kDotOptions[1],
                                                                       kLineOptions[1], 
                                                                       LIST_DATA$gene_info$common[[i]][3], 
                                                                       1,
                                                                       LIST_DATA$gene_info$common[[i]][5]))
    tclvalue(myTCL) <- legend_nickname
    tkconfigure(label_file, textvariable = myTCL)
    tkconfigure(label_count, text = paste("n = ", length(enesg)))
    MoveAllToOtherEntry(on_listbox, off_listbox, "on", listboxname)
    tkraise(root)
  }
}

# reads in gene list for intersect
IntersectLoadFile <- function(){
  if (length(LIST_DATA$table_file) < 1) {
    return ()
  }
  STATE[1] <<- 2
  full_file_name <- tclvalue(tkgetOpenFile(filetypes = 
                                             "{{Gene List} {.txt}} {{All files} *}"))
  STATE[1] <<- 0
  if (!nchar(full_file_name)) {
    tkraise(root)
    return ()
  } else {
    file_name <- paste(strsplit(as.character(full_file_name), 
                                '/')[[1]][(length(strsplit(as.character(full_file_name), 
                                                           '/')[[1]]))])
    legend_nickname <- strsplit(as.character(file_name), '.txt')[[1]][1]
    
    genefile <- read.table(full_file_name, stringsAsFactors = FALSE,
                           header = FALSE)
    genefile2 <- c(genefile[ ,1], LIST_DATA$gene_file$common$use)
    genefile2 <- unique(genefile2[duplicated(genefile2)])
    tkraise(root)
    if(length(genefile2) > 0){
      return(IntersectGeneLists(genefile2, legend_nickname))
    } else{
      tkmessageBox(message = "Can't use this gene list")
    }
  }
}

# saves gene list to a file
SaveGenelist <- function(listboxgene, activelistbox, toolinfo = " "){
  if (is.character(listboxgene)){
    listboxgene <- ListBoxSelectHelper(listboxgene)
  }
  # make toolinfo
  if(toolinfo == "sort"){
    R_start_bin <- as.integer(tclvalue(tcl_bin_start_sort))
    R_end_bin <- as.integer(tclvalue(tcl_bin_end_sort))
    R_num <- as.integer(tclvalue(tcl_top_bottom_num))
    R_option <- tclvalue(tcl_top_bottom_option)
    R_order <- tclvalue(tcl_acc_dec)
    toolinfo <- paste0("sort tool: ", R_option, R_num, ": bins ", R_start_bin, " to ", R_end_bin, " ", R_order)
    
  } else if (toolinfo == "region"){
    R_start_bin <- as.integer(tclvalue(tcl_bin_start_region))
    R_end_bin <- as.integer(tclvalue(tcl_bin_end_region))
    R_num <- as.numeric(tclvalue(tcl_bin_fold_region))
    my_tab <- strsplit(x = tk2notetab.text(notebook_region), split = " ")[[1]][2]
    if(my_tab == 1 | my_tab == 2){
      my_file <- paste(R_num, "fold up in", as.character(tkget(activelistbox, as.numeric(my_tab)-1)))
    } else {
      my_file <- paste(my_tab, "fold change", R_num)
    }
    toolinfo <- paste("region tool:", my_file, ": bins", R_start_bin, "to", R_end_bin)
  } else if(toolinfo == "ratios"){
    R_start1_bin <- as.integer(tclvalue(tcl_bin_start1_ratios))
    R_end1_bin <- as.integer(tclvalue(tcl_bin_end1_ratios))
    R_start2_bin <- as.integer(tclvalue(tcl_bin_start2_ratios))
    R_end2_bin <- as.integer(tclvalue(tcl_bin_end2_ratios))
    R_num <- as.numeric(tclvalue(tcl_bin_fold_ratios))
    my_tab <- strsplit(x = tk2notetab.text(notebook_ratios), split = " ")[[1]][2]
    if(my_tab == 1 | my_tab == 2){
      my_file <- paste(R_num, "fold up in", as.character(tkget(activelistbox, as.numeric(my_tab)-1)))
    } else {
      my_file <- paste(my_tab, "fold change", R_num)
    }
    toolinfo <- paste0("region tool: ", my_file, ": bins ", R_start1_bin, ":", R_end1_bin, "/", 
                       R_start2_bin, ":", R_end2_bin)
  }else if(toolinfo == "cluster"){
    R_start_bin <- as.integer(tclvalue(tcl_bin_start_cluster))
    R_stop_bin <- as.integer(tclvalue(tcl_bin_end_cluster))
    R_cluster <- as.integer(tclvalue(tcl_bin_cluster_num))
    my_tab <- strsplit(x = tk2notetab.text(notebook_cluster), split = " ")[[1]][2]
    toolinfo <- paste("cluster tool: ", R_cluster, "clusters, cluster #", my_tab, ": bins", R_start_bin, "to", R_stop_bin)
  }else if(toolinfo == "quantile"){
    R_start_bin <- as.integer(tclvalue(tcl_bin_start_cluster))
    R_stop_bin <- as.integer(tclvalue(tcl_bin_end_cluster))
    my_tab <- tk2notetab.text(notebook_cluster)
    toolinfo <- paste("quantile tool:", my_tab, ", bins", R_start_bin, "to", R_stop_bin)
  }else if(toolinfo == "intersect"){
    my_tab <- tk2notetab.text(notebook_intersect)
    toolinfo <- paste("intersect tool:", my_tab)
  } else if(toolinfo == "cdf"){
    R_start1_bin <- as.integer(tclvalue(tcl_bin_start1_cdf))
    R_end1_bin <- as.integer(tclvalue(tcl_bin_end1_cdf))
    R_start2_bin <- as.integer(tclvalue(tcl_bin_start2_cdf))
    R_end2_bin <- as.integer(tclvalue(tcl_bin_end2_cdf))
    R_order <- tclvalue(tcl_acc_dec_cdf)
    toolinfo <- paste0("cdf tool: ", R_order, ": bins ", R_start1_bin, ":", R_end1_bin, "/", 
                       R_start2_bin, ":", R_end2_bin)
  }
  
  OnUpdate <- function(){
    new_comments <- tclvalue(tkget(NickName, "0.0", "end"))
    if(nchar(new_comments) < 1){
      new_comments <- paste("#", Sys.Date())
    }
    tkdestroy(CommentFrame)
    STATE[1] <<- 2
    file_name <- tclvalue(tkgetSaveFile(filetypes = "{{Gene txt Files} {.txt}}", initialfile = toolinfo))
    STATE[1] <<- 0
    if (!nchar(file_name)) { ## file save test
      return()
    }else{
      if(tclvalue(tcl_checkbox_split) == 1){
        tt <- c(new_comments, gsub("\\+;|\\-;|\\|", "\t",paste(tkget(listboxgene, 0, 'end'))))
      } else{
        tt <- c(new_comments, paste(tkget(listboxgene, 0, 'end')))
      }
      write.table(tt, file_name, col.names = FALSE, row.names=FALSE, quote=FALSE)
    }
  }
  
  if(as.integer(tksize(listboxgene)) > 0){
    CommentFrame <<- tktoplevel()
    STATE[1] <<- 3
    tkbind(CommentFrame, "<Destroy>", function() OnDestroy())
    tkgrid(tklabel(CommentFrame,text="Add or edit comments"), 
           columnspan = 2)
    
    tkgrid(NickName <<- tk2text(CommentFrame, width = kWidth + 40, height = kHeight + 30), 
           column = 1, row = 2, pady = c(10, 10))
    tkgrid(tk2button(CommentFrame, text = "Save", 
                     command = function() OnUpdate()))
    tkinsert(NickName, "end", paste("#", Sys.Date(), "\n")) 
    tkinsert(NickName, "end", paste("#", version_num, "\n"))
    tkinsert(NickName, "end", paste("#", toolinfo, "\n"))
    tkinsert(NickName, "end", paste("# Lists and file used in tool: \n"))
    if(tk2notetab.text(notebook_tool) != "Intersect\nTool"){
      nick_name <- as.character(tkget(activelistbox, 0, 'end'))
      for(i in nick_name){
        use_nickname <- strsplit(sub("-", " ", i), " ")[[1]]
        myTCL <- tclvalue(get(paste0("tcl_", use_nickname[1], "file")))
        tkinsert(NickName, "end", paste("#", myTCL, "-", use_nickname[2], "\n"))
      }
    } else {
      nick_name <- as.character(tkget(activelistbox, 0, 'end'))
      tkinsert(NickName, "end", paste("### Gene lists used ###"))
      for(i in nick_name){
        tkinsert(NickName, "end", paste("#", i, "\n"))
      }
      tkinsert(NickName, "end", paste("### List of files loaded ###"))
      nick_name <- names(LIST_DATA$table_file)
      for(i in nick_name){
        tkinsert(NickName, "end", paste("#", i, "\n"))
      }
    }
    tkinsert(NickName, "end", paste("####  Please start all comment lines with '#'  ####"))
    if(tclvalue(tcl_checkbox_comment) == 1){
      OnUpdate()
    }
    
  }
}

# removes selected file(s)
RemoveFile <- function() {
  if (length(names(LIST_DATA$table_file)) > 0) {
    for (i in as.integer(tkcurselection(listbox_common_on))) {
      filename <- tclvalue(tkget(listbox_common_on, i))
      nickname <- LIST_DATA$gene_info$common[[filename]][5]
      LIST_DATA$table_file[[nickname]] <<- NULL
      sapply(names(LIST_DATA$gene_info),
             function(k) LIST_DATA$gene_info[[k]][[filename]] <<-NULL)
      kListColorSet <<- c(kListColorSet[seq_along(kListColorSet) != i + 1], 
                          kListColorSet[seq_along(kListColorSet) == i + 1])
    }
    for (i in as.integer(tkcurselection(listbox_common_off))) {
      filename <- tclvalue(tkget(listbox_common_off, i))
      nickname <- LIST_DATA$gene_info$common[[filename]][5]
      LIST_DATA$table_file[[nickname]] <<- NULL
      sapply(names(LIST_DATA$gene_info),
             function(k) LIST_DATA$gene_info[[k]][[filename]] <<-NULL)
      kListColorSet <<- c(kListColorSet[seq_along(kListColorSet) != i + 1], 
                          kListColorSet[seq_along(kListColorSet) == i + 1])
    }
    if (length(names(LIST_DATA$table_file)) > 1) {
      gene_names <- LIST_DATA$table_file[[1]][,1]
      for (i in LIST_DATA$table_file){
        gene_names <- c(gene_names, i[,1])
        gene_names <- gene_names[duplicated(gene_names)]
      }
    } else if (length(names(LIST_DATA$table_file)) == 1 ){
      gene_names <- unique(LIST_DATA$table_file[[1]][ ,1])
  } else {
    ClearTableFile()
      return()
  }
    LIST_DATA$gene_file$common$full <<- gene_names
    LIST_DATA$gene_file$common$use <<- gene_names
    sapply(names(LIST_DATA$gene_file), function(x){
      onlist <- get(paste("listbox_" , x, "_on", sep = ""))
      offlist <- get(paste("listbox_" , x, "_off", sep = ""))
      if(length(LIST_DATA$gene_file[[x]]$full) > 0){
        gene_names2 <- c(gene_names, LIST_DATA$gene_file[[x]]$full)
        LIST_DATA$gene_file[[x]]$use <<- gene_names2[duplicated(gene_names2)]
        tkdelete(onlist, 0, 'end')
        tkdelete(offlist, 0, 'end')
        lapply(names(LIST_DATA$gene_info[[x]]), function(j){
          if(LIST_DATA$gene_info[[x]][[j]][4] == 1){
            tkinsert(onlist, 'end', j)
            tkitemconfigure(onlist, "end", foreground = LIST_DATA$gene_info[[x]][[j]][3])
          } else {
            tkinsert(offlist, 'end', j)
            tkitemconfigure(offlist, "end", foreground = LIST_DATA$gene_info[[x]][[j]][3])
          }
        })
      onlabel <- get(paste("label_" , x, "_length", sep = ""))
      tkconfigure(onlabel, text = paste("n = " , 
                                          length(LIST_DATA$gene_file[[x]]$use)))
      }
        
    })
    tkconfigure(combobox_numerator, 
                values = names(LIST_DATA$gene_info$common))
    tkconfigure(combobox_denominator, 
                values = names(LIST_DATA$gene_info$common))
    DActAll()
  }
}

# sets the comboboxes with the number of bins
SetComboBoxes <- function(num_bins){
  num <- tclvalue(tkget(combobox_plot_lines))
  tkconfigure(combobox_norm_bin, values = c(0:(num_bins[2] - 1)))
  tkconfigure(combobox_bin_start, values = c(1:(num_bins[2] - 1)))
  tkconfigure(combobox_bin_end, values = c(1:(num_bins[2] - 1)))
  tkconfigure(combobox_bin_start_region, values = c(1:(num_bins[2] - 1)))
  tkconfigure(combobox_bin_end_region, values = c(1:(num_bins[2] - 1)))
  tkset(combobox_bin_end, num_bins[2] - 1)
  tkset(combobox_bin_end_region, num_bins[2] - 1)
  tkconfigure(combobox_bin_start1_ratios, values = c(1:(num_bins[2] - 1)))
  tkconfigure(combobox_bin_end1_ratios, values = c(1:(num_bins[2] - 1)))
  tkset(combobox_bin_start1_ratios, min(floor(list_plot_lines[[num]][3])/2, num_bins[2] - 1))
  tkset(combobox_bin_end1_ratios, min(floor(list_plot_lines[[num]][3]), num_bins[2] - 1))
  tkconfigure(combobox_bin_start2_ratios, values = c(1:(num_bins[2] - 1)))
  tkconfigure(combobox_bin_end2_ratios, values = c(1:(num_bins[2] - 1)))
  tkset(combobox_bin_start2_ratios, min(floor(list_plot_lines[[num]][3]) + 1, num_bins[2] - 1))
  tkset(combobox_bin_end2_ratios, min(floor(list_plot_lines[[num]][4]), num_bins[2] - 1))
  tkconfigure(combobox_bin_start1_cdf, values = c(1:(num_bins[2] - 1)))
  tkconfigure(combobox_bin_end1_cdf, values = c(1:(num_bins[2] - 1)))
  tkset(combobox_bin_start1_cdf, min(floor(list_plot_lines[[num]][3])/2, num_bins[2] - 1))
  tkset(combobox_bin_end1_cdf, min(floor(list_plot_lines[[num]][3]), num_bins[2] - 1))
  tkconfigure(combobox_bin_start2_cdf, values = c(1:(num_bins[2] - 1)))
  tkconfigure(combobox_bin_end2_cdf, values = c(1:(num_bins[2] - 1)))
  tkset(combobox_bin_start2_cdf, min(floor(list_plot_lines[[num]][3]) +1, num_bins[2] - 1))
  tkset(combobox_bin_end2_cdf, min(floor(list_plot_lines[[num]][4]), num_bins[2] - 1))
  tkconfigure(combobox_bin_start_cluster, values = c(1:(num_bins[2] - 1)))
  tkconfigure(combobox_bin_end_cluster, values = c(1:(num_bins[2] - 1)))
  tkset(combobox_bin_end_cluster, num_bins[2] - 1)
  tkconfigure(combobox_bin_start_sort, values = c(1:(num_bins[2] - 1)))
  tkconfigure(combobox_bin_end_sort, values = c(1:(num_bins[2] - 1)))
  tkset(combobox_bin_end_sort, num_bins[2] - 1)
  tkconfigure(combobox_bin_cluster_num, values = c(2:4))
}

# global options box
GlobalOptions <- function(){
  onOK <- function(){
    tkdestroy(OptionsFrame)
    STATE[1] <<- 0
  }
  OptionsFrame <<- tktoplevel()
  STATE[1] <<- 4
  tkbind(OptionsFrame, "<Destroy>", function() OnDestroy())
  tkgrid(tklabel(OptionsFrame,text="Edit options"), 
         columnspan = 2)
  
  tkgrid(tkcheckbutton(OptionsFrame, variable = tcl_checkbox_comment, text = "Don't Show save comment box?"))
  
  tkgrid(tkcheckbutton(OptionsFrame, variable = tcl_checkbox_split, text = "Split saved gene list?"))
  
  tkgrid(tk2button(OptionsFrame, text = "OK", 
                   command = function() onOK()))
  
  
}

# change colors to diffrnet color brewer sets
BrewerSet <- function(){
  color_file <- brewer.pal(8, as.character(tclvalue(tcl_brewer)))
  lapply(names(LIST_DATA$gene_info), function(i){ 
    onlist <- get(paste("listbox_" , i, "_on", sep = ""))
    offlist <- get(paste("listbox_" , i, "_off", sep = ""))
    tkdelete(onlist, 0, 'end')
    tkdelete(offlist, 0, 'end')
    lapply(seq_along(LIST_DATA$gene_info[[i]]), function(j){
      color_safe <- j %% length(color_file)
      if (color_safe == 0) {
        color_safe <- 1
      }
      LIST_DATA$gene_info[[i]][[j]][3] <<- color_file[color_safe]
      if(LIST_DATA$gene_info[[i]][[j]][4] == 1){
        tkinsert(onlist, 'end', names(LIST_DATA$gene_info[[i]][j]))
        tkitemconfigure(onlist, "end", foreground = LIST_DATA$gene_info[[i]][[j]][3])
      } else {
        tkinsert(offlist, 'end', names(LIST_DATA$gene_info[[i]][j]))
        tkitemconfigure(offlist, "end", foreground = LIST_DATA$gene_info[[i]][[j]][3])
      }
    })
  })
}

# get file to replace the color list and update used ones
GetColor <- function() {
  STATE[1] <<- 2
  full_file_name <- tclvalue(tkgetOpenFile(filetypes = 
                                               "{{color.txt Files} {.color.txt}} {{All files} *}"))
  STATE[1] <<- 0
  if (!nchar(full_file_name)) { ## file select test
    color_file<- brewer.pal(8, as.character(tclvalue(tcl_brewer)))
  } else { 
  color_file <- read.table(full_file_name, header = FALSE, 
                               stringsAsFactors = FALSE)
  # checks if in rgb format and converts to hex and if is a color
  sapply(seq_along(color_file[[1]]), function(i) {
    if (suppressWarnings(!is.na(as.numeric(
        substr(color_file[[1]][i], 1, 1)))) == TRUE) { 
          red_green_blue <- strsplit(color_file[[1]][i], ",")
          if (length(red_green_blue[[1]]) == 3) {
            color_file[[1]][i] <<- rgb(as.numeric(red_green_blue[[1]])[1], 
                                       as.numeric(red_green_blue[[1]])[2], 
                                       as.numeric(red_green_blue[[1]])[3], 
                                       maxColorValue = 255)
          } else {
            color_file[[1]][i] <<- "black"
          }
        }
        if (!isColor(color_file[[1]][i])){
          color_file[[1]][i] <<- "black"
        }
      })
  }
  color_file <- unlist(color_file)
  if(length(color_file) > 0){
    color_file <- c(color_file, "black")
    kListColorSet <<-  color_file
    print(list("I now have these as default colors" = as.character(color_file)))
    lapply(names(LIST_DATA$gene_info), function(i){ 
      onlist <- get(paste("listbox_" , i, "_on", sep = ""))
      offlist <- get(paste("listbox_" , i, "_off", sep = ""))
      tkdelete(onlist, 0, 'end')
      tkdelete(offlist, 0, 'end')
      lapply(seq_along(LIST_DATA$gene_info[[i]]), function(j){
        color_safe <- j %% length(color_file)
        if (color_safe == 0) {
          color_safe <- 1
        }
        LIST_DATA$gene_info[[i]][[j]][3] <<- color_file[color_safe]
        if(LIST_DATA$gene_info[[i]][[j]][4] == 1){
          tkinsert(onlist, 'end', names(LIST_DATA$gene_info[[i]][j]))
          tkitemconfigure(onlist, "end", foreground = LIST_DATA$gene_info[[i]][[j]][3])
        } else {
          tkinsert(offlist, 'end', names(LIST_DATA$gene_info[[i]][j]))
          tkitemconfigure(offlist, "end", foreground = LIST_DATA$gene_info[[i]][[j]][3])
        }
      })
    })
  } else {
    tkmessageBox(message = "could not make any colors from file")
    
  }
  
  tkraise(root)
}

# make normalized file ... devide one by the other
MakeNormFile <- function() {
  nom <- tclvalue(tkget(combobox_numerator))
  dnom <- tclvalue(tkget(combobox_denominator))
  if (nom != "numerator" && dnom != "denominator") {  
    mynom <- LIST_DATA$gene_info$common[[nom]][5]
    mydnom <- LIST_DATA$gene_info$common[[dnom]][5]
    new_gene_list <- data.frame(inner_join(LIST_DATA$table_file[[mynom]], 
                                        LIST_DATA$table_file[[mydnom]], by = "gene"), 
                                stringsAsFactors = FALSE)
    new_gene_list[is.na(new_gene_list)] <- 0  # replace NAs with 0
    len <- (dim(new_gene_list)[2] - 1) / 2  # find number of bins
    # find min value /2 to replace 0s 
    new_min_for_na <- min(new_gene_list[, -1][new_gene_list[ ,-1] > 0])/2
    # replace 0's with min/2
    new_gene_list[ , -1] <- 
      as.data.frame(lapply(new_gene_list[ , -1], 
                           function(x) {replace(x, x == 0, new_min_for_na)}), 
                    stringsAsFactors = FALSE)
    new_tablefile <- data.frame(gene = new_gene_list[ ,1], 
                                new_gene_list[ ,2:(len + 1)] / 
                                  new_gene_list[ ,(len + 2):((len * 2) + 1)], 
                                stringsAsFactors = FALSE)
    file_name <- paste(nom, dnom, sep = "/")
    
    LIST_DATA$table_file[[paste(mynom, mydnom, sep = "/")]] <<- new_tablefile
    file_count <- length(LIST_DATA$table_file)
    
    color_safe <- file_count %% length(kListColorSet)
    if (color_safe == 0 ) {
      color_safe <- 1
    }
    for (p in names(LIST_DATA$gene_info)) {
      LIST_DATA$gene_info[[p]][[file_name]] <<- c(kDotOptions[1], kLineOptions[1], 
                                                    kListColorSet[color_safe],
                                                    1, 
                                                    paste(mynom, mydnom, sep = "/"))
    }
    
    sapply(names(LIST_DATA$gene_file), function(x){
      onlist <- get(paste("listbox_" , x, "_on", sep = ""))
      tkinsert(onlist, 'end', file_name)
      tkitemconfigure(onlist, 'end', foreground = kListColorSet[color_safe])
    })
    tkconfigure(combobox_numerator, 
                values = names(LIST_DATA$gene_info$common))
    tkconfigure(combobox_denominator, 
                values = names(LIST_DATA$gene_info$common))
    tkset(combobox_numerator, "numerator")
    tkset(combobox_denominator, "denominator")

  }  
}

# Makes data frame and gathers plot settings for plotting active samples
MakeDataFrame <- function(sel_list = NULL, table_file = LIST_DATA$table_file,
                          gene_file = LIST_DATA$gene_file, gene_info = LIST_DATA$gene_info, nickname = NULL) {
  if (is.null(names(table_file))) {
    return ()
  } else {
    use_col <- NULL
    use_dot <- NULL
    use_line <- NULL
    use_size <- NULL
    use_nickname <- NULL
    use_x_label <- NULL
    legend_space <- 1
    list_wide_data_frame <- list()
    pb <<- tkProgressBar(title = "be patient!! ",  min = 0, max = 100, width = 300 )
    for (i in names(gene_file)) {
      # checks to see if at least one file in list is acitve
      if (sum(as.numeric(sapply(gene_info[[i]], "[[", 4))) == 0) { 
        next ()
      } else {
         if (!is.null(sel_list)) {
          enesg <- c(sel_list, gene_file[[i]]$use)
          enesg <- data.frame(gene = enesg[duplicated(enesg)], 
                              stringsAsFactors = FALSE)
          myTCL <- tclvalue(get(paste0("tcl_", i, "file")))
          use_x_label <- paste(use_x_label, paste(myTCL, "n = ",
                                                  length(enesg[[1]])), sep = '  ') 
          if(length(enesg[[1]]) == 0){
            break()
          }
          } else {
          enesg <- data.frame(gene = gene_file[[i]]$use, 
                              stringsAsFactors = FALSE)
          myTCL <- tclvalue(get(paste0("tcl_", i, "file")))
          use_x_label <- paste(use_x_label, paste(myTCL, "n = ", 
                                                  length(enesg[[1]])), sep = '  ') 
        }
        lapply(names(gene_info[[i]]), function(k) 
          # uses only acive lists  
          if (as.numeric(gene_info[[i]][[k]][4]) == 1) {
            list_wide_data_frame[[paste(i, k, sep = '-')]] <<- data.frame(inner_join(enesg,
                                                                                     table_file[[gene_info[[i]][[k]][5]]], 
                                                                                     by = "gene"), 
                                                                          stringsAsFactors = FALSE)
            
            dot <- as.numeric(which(kDotOptions == gene_info[[i]][[k]][1]) - 1)
            if (dot == 0){
              dot <- 19
              mysize <- .01
            } else {
              dot <- dot + 14
              mysize <- 4.5
            }
            if (dot > 20) {
              dot <- 19
              mysize <- 6
            }
            line <- which(kLineOptions == gene_info[[i]][[k]][2])
            if (line > 6) {
              line <- 0
            }
            
            use_col <<- c(use_col, gene_info[[i]][[k]][3])
            use_dot <<- c(use_dot, dot)
            use_line <<- c(use_line, line)
            use_size <<- c(use_size, mysize)
            current_nickname <- paste(gsub("(.{17})", "\\1\n", myTCL), gsub("(.{17})", "\\1\n", k), sep = '-\n')
            legend_space <<- max(legend_space, (length(strsplit(current_nickname, "\n")[[1]]) - 0.5))
            use_nickname <<- c(use_nickname, current_nickname)
          }
        )
      }
    }
  }
  if(!is.null(nickname)){
    use_x_label <- paste(nickname, use_x_label)
  }
  setTkProgressBar(pb, 50, label = paste(round(50, 0), "Gathered Data"))
  if (!is.null(names(list_wide_data_frame))) {
    ApplyMath(list_wide_data_frame, use_col, use_dot, use_line, use_size, use_nickname,
              use_x_label, legend_space)
  } else{
    close(pb)
  }
}

# Applys math to long list
ApplyMath <- function(list_wide_data_frame, use_col, use_dot, use_line, use_size,
                      use_nickname, use_x_label, legend_space) {
  
  # math set and y label
  use_math <- as.character(tclvalue(tcl_math_option))
  if (use_math == "mean") {
    use_apply <- function(x) colMeans(x, na.rm = TRUE)
    use_y_label <- "Mean of bin counts"
  } else if (use_math == "sum") {
    use_apply <- function(x) colSums(x, na.rm = TRUE)
    use_y_label <- "Sum of bin counts"
  } else if (use_math == "median") {
    use_apply <- function(x) apply(x, 2, median, na.rm = TRUE)
    use_y_label <- "Median of bin counts"
  } 
  
  # set normilization to relative frequency or bin number or 1 for none, 
  # and update y label
  r_checkbox_relative_frequency <- as.character(tclvalue(
    tcl_checkbox_relative_frequency))
  r_checkbox_gene_relative_frequency <- as.character(tclvalue(
    tcl_checkbox_relative_gene_frequency))
  norm_bin <- as.numeric(tclvalue(tcl_norm_bin))
  if ( r_checkbox_gene_relative_frequency == 1 ){
    if (r_checkbox_relative_frequency == 1) {
      tktoggle(checkbox_relative_frequency)
    }
    lapply(seq_along(list_wide_data_frame), 
           function(i) 
             list_wide_data_frame[[i]][ ,-1] <<- list_wide_data_frame[[i]][ ,-1]/rowSums(list_wide_data_frame[[i]][ ,-1], na.rm = TRUE))
    use_y_label <- paste("RF per gene :", use_y_label)
  }
  
  if (r_checkbox_relative_frequency == 1 && norm_bin == 0) {
    use_y_label <- paste(strsplit(use_y_label, split = " ")[[1]][1], 
                         "bins : RF")
    norm <- lapply(seq_along(list_wide_data_frame), 
                   function(i) 
                     sum(use_apply(list_wide_data_frame[[i]][ ,-1])))
  } else if (norm_bin > 0) {
    if (r_checkbox_relative_frequency == 1) {
      tktoggle(checkbox_relative_frequency)
    }
    if (r_checkbox_gene_relative_frequency == 1){
      use_y_label <- paste(use_y_label, " : Norm bin ", norm_bin)
    } else {
      use_y_label <- paste(strsplit(use_y_label, split = " ")[[1]][1], 
                         "bins : Normalize to bin ", norm_bin)
    }
    norm <- as.numeric(lapply(seq_along(list_wide_data_frame), 
                              function(i) 
                                use_apply(list_wide_data_frame[[i]][ ,-1])[norm_bin]))
  } else {
    norm <- lapply(seq_along(list_wide_data_frame), function(i) 1)
  }
  
  # TODO add bin control?
  list_applied_math_data_frame <- lapply(seq_along(list_wide_data_frame), 
                                         function(i) 
                                           data.frame(bin = (seq_along(list_wide_data_frame[[i]][-1])), 
                                                      set = (as.character(use_nickname[i])), 
                                                      value = use_apply(list_wide_data_frame[[i]][,-1]) / norm[[i]],
                                                      set2 = strsplit(names(list_wide_data_frame)[i], split = "-")[[1]][1],
                                                      stringsAsFactors = FALSE))
  
  list_long_data_frame <- bind_rows(list_applied_math_data_frame)
  
  # update y label if log2
  if (tclvalue(tcl_checkbox_log2) == 1) {
    use_y_label <- paste("log2(", use_y_label, ")", sep = "")
    if(sum(list_long_data_frame$value) != 0){
    list_long_data_frame$value[list_long_data_frame$value == 0] <- min(list_long_data_frame$value[list_long_data_frame$value > 0])/2
    } else {
      list_long_data_frame$value <- 1
    }
  }
  
  use_pos_plot_ticks <- Destring(unlist(strsplit(tclvalue(
    tcl_pos_plot_ticks), " ")))
  use_label_plot_ticks <- unlist(strsplit(tclvalue(tcl_label_plot_ticks), 
                                          " "))
  if (length(use_label_plot_ticks) != length(use_pos_plot_ticks)) {
    tkmessageBox(message = "The number of Positions and labels are unequal")
    return ()
  }
  
  use_plot_breaks <- c(Destring(tclvalue(tcl_pos_one_line)), 
                       Destring(tclvalue(tcl_pos_two_line)), 
                       Destring(tclvalue(tcl_pos_three_line)), 
                       Destring(tclvalue(tcl_pos_four_line)),
                       use_pos_plot_ticks)
  
  use_plot_breaks_labels <- c(tclvalue(tcl_one_tss_tts_option), 
                              tclvalue(tcl_two_tss_tts_option),
                              tclvalue(tcl_three_tss_tts_option),
                              tclvalue(tcl_four_tss_tts_option),
                              use_label_plot_ticks)
  use_virtical_line_type <- c(3,3,1,1)  # TODO add change virtical line type
  use_virtical_line_color <- c("green", "red", "black", "black")
  use_plot_breaks_labels <- use_plot_breaks_labels[!is.na(use_plot_breaks)]
  use_virtical_line_type <- use_virtical_line_type[!is.na(
    use_plot_breaks[1:4])]
  use_virtical_line_color <- use_virtical_line_color[!is.na(
    use_plot_breaks[1:4])]
  use_virtical_line <- use_plot_breaks[1:4][!is.na(use_plot_breaks[1:4])]
  use_plot_breaks <- use_plot_breaks[!is.na(use_plot_breaks)]
  
  # TODO need controls?
  virtical_line_data_frame <- data.frame(use_virtical_line, 
                                         use_virtical_line_type, 
                                         use_virtical_line_color, 
                                         stringsAsFactors = FALSE)
  names(use_col) <- use_nickname
  names(use_dot) <- use_nickname
  names(use_line) <- use_nickname
  use_plot_limits <- c(as.integer(tclvalue(tcl_bin_start)), as.integer(tclvalue(tcl_bin_end)))
  use_y_limits <- c(min(list_long_data_frame$value[list_long_data_frame$bin >= use_plot_limits[1] & 
                                                     list_long_data_frame$bin <= use_plot_limits[2]]),
                    max(list_long_data_frame$value[list_long_data_frame$bin >= use_plot_limits[1] & 
                                                     list_long_data_frame$bin <= use_plot_limits[2]]))
  setTkProgressBar(pb, 75, label = paste(round(75, 0), "Applied Math"))
  GGplotF(list_long_data_frame, use_col, use_dot, use_line, use_size, use_y_label,
          use_x_label, use_plot_breaks, virtical_line_data_frame,
          use_plot_breaks_labels, use_plot_limits, use_y_limits, legend_space)
}

# main ggplot function
GGplotF <- function(list_long_data_frame, use_col, use_dot, use_line, use_size,
                    use_y_label, use_x_label, use_plot_breaks, 
                    virtical_line_data_frame, use_plot_breaks_labels, use_plot_limits, use_y_limits, legend_space) {
  if (tclvalue(tcl_checkbox_log2) == 1) {
    gp <- ggplot(list_long_data_frame, aes(x = bin, y = log2(value), 
                                           color = set, shape = set, linetype = set, size = set))
  } else {
    gp <- ggplot(list_long_data_frame, aes(x = bin, y = value, color = set, 
                                           shape = set, linetype = set, size = set))
  }
  gp <- gp +
    geom_line(size = 2.5) + 
    geom_point(stroke = .001) + 
    scale_size_manual(name = "Sample", values = use_size) +
    scale_color_manual(name = "Sample", values = use_col) +
    scale_shape_manual(name = "Sample", values = use_dot) + 
    scale_linetype_manual(name = "Sample", values = use_line) +
    xlab(use_x_label) + ylab(use_y_label) +  # Set axis labels
    scale_x_continuous(breaks = use_plot_breaks, 
                       labels = use_plot_breaks_labels) +

    geom_vline(data = virtical_line_data_frame, 
               aes(xintercept = use_virtical_line), 
               size = 2, 
               linetype = virtical_line_data_frame$use_virtical_line_type, 
               color = virtical_line_data_frame$use_virtical_line_color) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.title.y = element_text(size =  15)) +
    theme(axis.title.x = element_text(size =  10, vjust = .5)) +
    theme(axis.text.x = element_text( size = 12,  angle = -45, hjust = .1, 
                                      vjust = .9,   face = 'bold')) +
    theme(legend.title=element_blank(), legend.key = element_rect(size = 5, color = 'white'), 
          legend.key.height=unit(legend_space,"line"),legend.text=element_text(size=9)) +
    coord_cartesian(xlim = use_plot_limits, ylim = use_y_limits)
  setTkProgressBar(pb, 99, label = paste(round(100, 0), "Ready to plot"))
  print(gp)
  close(pb)
}

# plot cumlitave frequency
GGplotC <- function(df2, my_xlab, use_col, use_header, legend_space){
  gp <- ggplot(df2, aes(log2(value), color = set)) + 
  stat_ecdf(show.legend = TRUE, size = 1.8) + 
  scale_color_manual(name = "Sample", values = use_col) +
  xlab(paste(my_xlab, collapse=",  ")) + 
  ylab("Fraction of genes") +  
  ggtitle(use_header) +
  theme_bw()+
  theme(legend.title=element_blank()) +
  theme(axis.title.y = element_text(size =  15)) +
  theme(axis.title.x = element_text(size =  10, vjust = .5)) +
  theme(axis.text.x = element_text(size = 12,  angle = -45, hjust = .1, 
                                   vjust = .9,   face = 'bold')) +
  theme(legend.title=element_blank(), legend.key = element_rect(size = 5, color = 'white'), 
        legend.key.height=unit(legend_space,"line"),legend.text=element_text(size=9)) # + coord_cartesian(xlim = c(-4,6))
  print(gp)

}

# finds genes in list and moves them to top of list and hightlights them
FindGene <- function(my_word_box, gene_list_box){
  my_word <- as.character(tkget(my_word_box))
  if (is.character(gene_list_box)){
    gene_list_box <- ListBoxSelectHelper(gene_list_box)
  }
  if(as.numeric(tksize(gene_list_box)) > 0){
    for( i in 1:as.numeric(tksize(gene_list_box))){
      tkitemconfigure(gene_list_box, i - 1, foreground = "black")
    }
  } else {
    return()
  }
  if(length(my_word) == 1){
    my_gene <- NULL
    nums <- grep(my_word, tkget(gene_list_box, 0, 'end'), ignore.case = T)
    for(i in rev(nums)){
      my_gene <- c(tkget(gene_list_box, i - 1), my_gene)
      tkdelete(gene_list_box, i - 1)
    }
    for(i in my_gene){
      tkinsert(gene_list_box, 0, i)
      tkitemconfigure(gene_list_box, 0, foreground = "red")
    }
    if(length(my_gene) == 0){
      tkdelete(my_word_box, 0, 'end')
      tkinsert(my_word_box, 0, "Not_found")
    }
  } else if (length(my_word) > 1){
    tkdelete(my_word_box, 0, 'end')
    tkinsert(my_word_box, 0, "no_spaces_between_words")
  }
}

# sorts active gene list contain top % signal based on selected bins and file
SortTop <- function() { 
    if (as.integer(tksize(listbox_active_sort)) < 1) {
      return ()
    }
    R_start_bin <- as.integer(tclvalue(tcl_bin_start_sort))
    R_end_bin <- as.integer(tclvalue(tcl_bin_end_sort))
    R_num <- as.integer(tclvalue(tcl_top_bottom_num))
    R_option <- tclvalue(tcl_top_bottom_option)
    R_order <- tclvalue(tcl_acc_dec)
    # for item in active list make sorted list, then merge sort=T, then pull out request
    lc <- 0
    outlist <- NULL
    nick_name <- as.character(tkget(listbox_active_sort, 0, 'end'))
    lapply(nick_name, function(j){
      nick_name2 <- strsplit(sub('-', '\n!',j), '\n!')[[1]]
      my_ref  <- LIST_DATA$gene_info[[nick_name2[1]]][[nick_name2[2]]][5]
      enesg <- data.frame(gene = LIST_DATA$gene_file[[nick_name2[1]]]$use, stringsAsFactors = F)
      df <- inner_join(enesg, LIST_DATA$table_file[[my_ref]], by = 'gene')
      apply_bins <- rowSums(df[,-1][R_start_bin:R_end_bin],	na.rm = T)
      ix <- sort(apply_bins, decreasing = T, index = T)$ix
      gene_count <- nrow(df)
      if(R_option == "Top%") {
        num <- c(1, ceiling(gene_count * (R_num/100)))
      } else {
        num <- c(ceiling((gene_count +1) - (gene_count * (R_num/100))), gene_count)
      }
      outlist <<- c(outlist, df[ix,1][num[1]:num[2]])
      if(lc > 0){
        outlist <<- unique(outlist[duplicated(outlist)])
      }
      lc <<- lc + 1
    } )
    if(R_order == 'Accend'){
      outlist <- rev(outlist)
    }
    tkdelete(listbox_active_gene_sort, 0, 'end')
    if(length(outlist) > 0){
      tkconfigure(listbox_active_gene_sort, listvariable = tclVar(as.character(outlist)))
    }
    tkconfigure(label_active_sort_length, text = paste('n = ', (as.integer(tksize(listbox_active_gene_sort)))))
}	

# a[1]/b[2] make gene list
CompareRegions <- function() { 
  if (as.integer(tksize(listbox_active_region)) < 2) {
    return ()
  }
  R_start_bin <- as.integer(tclvalue(tcl_bin_start_region))
  R_end_bin <- as.integer(tclvalue(tcl_bin_end_region))
  R_num <- as.numeric(tclvalue(tcl_bin_fold_region))
  lc <- 0
  outlist <- NULL
  nick_name <- as.character(tkget(listbox_active_region, 0, 'end'))
  lapply(nick_name, function(j){
    nick_name2 <- strsplit(sub('-', '\n!',j), '\n!')[[1]]
    my_ref  <- LIST_DATA$gene_info[[nick_name2[1]]][[nick_name2[2]]][5]
    enesg <- data.frame(gene = LIST_DATA$gene_file[[nick_name2[1]]]$use, stringsAsFactors = F)
    df <- inner_join(enesg, LIST_DATA$table_file[[my_ref]], by = 'gene')
    df[is.na(df)] <- 0  # replace NAs with 0
    # find min value /2 to replace if sum = 0s 
    new_min <- min(df[, -1][df[ , -1] > 0]) / 2
    apply_bins <- data.frame(gene = df[ , 1], sum = rowSums(df[ , -1][R_start_bin:R_end_bin],	na.rm = T))
    lc <<- lc + 1
    outlist[[lc]] <<- replace(apply_bins, apply_bins == 0, new_min)
    if(lc > 1){
      outlist[[1]] <<- merge(outlist[[1]], outlist[[lc]], by = 'gene', sort = FALSE)
      outlist[[1]][ , 4] <<- outlist[[1]][ , 2] / outlist[[1]][ , 3]
      ix <- sort(outlist[[1]][ , 4], decreasing = T, index = T)$ix
      outlist[[1]] <<- outlist[[1]][ix,]
    }
  } )
  tkdelete(listbox_gene_region_up, 0, 'end')
  if(sum(outlist[[1]][ ,4] > R_num) > 0){
    tkconfigure(listbox_gene_region_up, listvariable = tclVar(as.character(outlist[[1]][ , 1][outlist[[1]][ ,4] > R_num])))
  }
  tkconfigure(label_region_up, text = paste('n = ', (as.integer(tksize(listbox_gene_region_up)))))
  
  tkdelete(listbox_gene_region_down, 0, 'end')
  if(sum(outlist[[1]][ ,4] < 1/R_num) > 0){
    tkconfigure(listbox_gene_region_down, listvariable = tclVar(as.character(outlist[[1]][ , 1][outlist[[1]][ ,4] < 1/R_num])))
  }
  tkconfigure(label_region_down, text = paste('n = ', (as.integer(tksize(listbox_gene_region_down)))))
    
  tkdelete(listbox_gene_region_between, 0, 'end')
  if(sum(outlist[[1]][ ,4] <= R_num & outlist[[1]][ ,4] >= 1/R_num) > 0){
    tkconfigure(listbox_gene_region_between, listvariable = 
                  tclVar(as.character(outlist[[1]][ , 1][outlist[[1]][ ,4] <= R_num & outlist[[1]][ ,4] >= 1/R_num])))
  }
  tkconfigure(label_region_between, text = paste('n = ', (as.integer(tksize(listbox_gene_region_between)))))
      
  # add ploting?
}	

# (a[1]/a[2])/(b[1]/b[2]) make gene list
CompareRatios <- function(){
    if (as.integer(tksize(listbox_active_ratios)) < 2) {
      return ()
    }
    R_start1_bin <- as.integer(tclvalue(tcl_bin_start1_ratios))
    R_end1_bin <- as.integer(tclvalue(tcl_bin_end1_ratios))
    R_start2_bin <- as.integer(tclvalue(tcl_bin_start2_ratios))
    R_end2_bin <- as.integer(tclvalue(tcl_bin_end2_ratios))
    R_num <- as.numeric(tclvalue(tcl_bin_fold_ratios))
    lc <- 0
    outlist <- NULL
    nick_name <- as.character(tkget(listbox_active_ratios, 0, 'end'))
    lapply(nick_name, function(j){
      nick_name2 <- strsplit(sub('-', '\n!',j), '\n!')[[1]]
      my_ref  <- LIST_DATA$gene_info[[nick_name2[1]]][[nick_name2[2]]][5]
      
      enesg <- data.frame(gene = LIST_DATA$gene_file[[nick_name2[1]]]$use, stringsAsFactors = F)
      
      df <- inner_join(enesg, LIST_DATA$table_file[[my_ref]], by = 'gene')
      df[is.na(df)] <- 0  # replace NAs with 0
      # find min value /2 to replace if sum = 0s 
      new_min <- min(df[, -1][df[ , -1] > 0]) / 2
      apply_bins <- data.frame(gene = df[ , 1], sum1 = rowSums(df[ , -1][R_start1_bin:R_end1_bin],	na.rm = T),  
                                 sum2 = rowSums(df[ , -1][R_start2_bin:R_end2_bin],	na.rm = T), stringsAsFactors = FALSE)
      apply_bins1 <- replace(apply_bins, apply_bins == 0, new_min)
      lc <<- lc + 1
      outlist[[lc]] <<- data.frame(gene = apply_bins1[ , 1], sum = apply_bins1$sum1 / apply_bins1$sum2, stringsAsFactors = FALSE)
      if(lc > 1){
        outlist[[1]] <<- inner_join(outlist[[1]], outlist[[lc]], by = 'gene')
        outlist[[1]][ , 4] <<- outlist[[1]][ , 2] / outlist[[1]][ , 3]
        ix <- sort(outlist[[1]][ , 4], decreasing = T, index = T)$ix
        outlist[[1]] <<- outlist[[1]][ix,]
      }
    } )
    tkdelete(listbox_gene_ratios_up, 0, 'end')
    if(sum(outlist[[1]][ ,4] > R_num) > 0){
      tkconfigure(listbox_gene_ratios_up, listvariable = tclVar(as.character(outlist[[1]][ , 1][outlist[[1]][ ,4] > R_num])))
    }
    tkconfigure(label_ratios_up, text = paste('n = ', (as.integer(tksize(listbox_gene_ratios_up)))))
    
    tkdelete(listbox_gene_ratios_down, 0, 'end')
    if(sum(outlist[[1]][ ,4] < 1/R_num) > 0){
      tkconfigure(listbox_gene_ratios_down, listvariable = tclVar(as.character(outlist[[1]][ , 1][outlist[[1]][ ,4] < 1/R_num])))
    }
    tkconfigure(label_ratios_down, text = paste('n = ', (as.integer(tksize(listbox_gene_ratios_down)))))
    
    tkdelete(listbox_gene_ratios_between, 0, 'end')
    if(sum(outlist[[1]][ ,4] <= R_num & outlist[[1]][ ,4] >= 1/R_num) > 0){
      tkconfigure(listbox_gene_ratios_between, listvariable = 
                    tclVar(as.character(outlist[[1]][ , 1][outlist[[1]][ ,4] <= R_num & outlist[[1]][ ,4] >= 1/R_num])))
    }
    tkconfigure(label_ratios_between, text = paste('n = ', (as.integer(tksize(listbox_gene_ratios_between)))))
}

# Cumulative Distribution plot
CumulativeDistribution <- function(){
  if (as.integer(tksize(listbox_active_cdf)) < 1) {
    return ()
  }
  R_start1_bin <- as.integer(tclvalue(tcl_bin_start1_cdf))
  R_end1_bin <- as.integer(tclvalue(tcl_bin_end1_cdf))
  R_start2_bin <- as.integer(tclvalue(tcl_bin_start2_cdf))
  R_end2_bin <- as.integer(tclvalue(tcl_bin_end2_cdf))
  R_order <- tclvalue(tcl_acc_dec_cdf)
  if(sum(R_start1_bin,R_end1_bin) > sum(R_start2_bin,R_end2_bin)){
    use_header <- "Log2 EI Cumulative plot"
  } else {
    use_header <- "Log2 PI Cumulative plot"
  }
  use_per_top <- as.numeric(strsplit(tclvalue(tcl_quntile_cdf_top),split = "%")[[1]])/100
  use_per_bottom <- as.numeric(strsplit(tclvalue(tcl_quntile_cdf_bottom),split = "%")[[1]])/100
  
  legend_space <- 1
 
  outlist <- NULL
  use_col <- NULL
  nick_name <- as.character(tkget(listbox_active_cdf, 0, 'end'))
  lapply(nick_name, function(j){
    nick_name2 <- strsplit(sub('-', '\n!',j), '\n!')[[1]]
    use_col <<- c(use_col, LIST_DATA$gene_info[[nick_name2[1]]][[nick_name2[2]]][3])
    my_ref  <- LIST_DATA$gene_info[[nick_name2[1]]][[nick_name2[2]]][5]
    
    enesg <- data.frame(gene = LIST_DATA$gene_file[[nick_name2[1]]]$use, stringsAsFactors = F)
    
    df <- inner_join(enesg, unique(LIST_DATA$table_file[[my_ref]]), by = 'gene')
    df[is.na(df)] <- 0  # replace NAs with 0
    # find min value /2 to replace if sum = 0s 
    new_min <- min(df[, -1][df[ , -1] > 0]) / 2
    apply_bins <- data.frame(gene = df[ , 1], sum1 = rowSums(df[ , -1][R_start1_bin:R_end1_bin],	na.rm = T),  
                             sum2 = rowSums(df[ , -1][R_start2_bin:R_end2_bin],	na.rm = T), stringsAsFactors = FALSE)
    apply_bins1 <- replace(apply_bins, apply_bins == 0, new_min)
    
    outlist[[j]] <<- data.frame(gene = apply_bins1[ , 1], sum = apply_bins1$sum1 / apply_bins1$sum2, stringsAsFactors = FALSE)
  } )
  nick_name2 <- NULL
  my_xlab <- NULL
  for(i in nick_name){
    use_nickname <- strsplit(sub("-", " ", i), " ")[[1]]
    myTCL <- tclvalue(get(paste0("tcl_", use_nickname[1], "file")))
    current_nickname <- paste(gsub("(.{17})", "\\1\n", myTCL), gsub("(.{17})", "\\1\n", use_nickname[2]), sep = '- \n')
    nick_name2 <- c(nick_name2, current_nickname)
    legend_space <<- max(legend_space, (length(strsplit(current_nickname, "\n")[[1]]) - 0.5))
    my_xlab <- c(my_xlab, paste(myTCL))
  }
  outlist2 <- NULL
  df <- lapply(seq_along(outlist), function(i){
    if(R_order == 'Accend'){
      ix <- sort(outlist[[i]][,2], decreasing = FALSE, index = T)$ix
    } else {
      ix <- sort(outlist[[i]][,2], decreasing = TRUE, index = T)$ix
    }
    ix <- ix[ceiling(length(ix)*use_per_top):(length(ix) - ceiling(length(ix)*use_per_bottom))]
    outlist2 <<- c(outlist2, outlist[[i]][,1][ix])
    if(i > 1){
      outlist2 <<- outlist2[duplicated(outlist2)]
    }
    data.frame(bin = (seq_along(outlist[[i]][,1][ix])), 
               set = (as.character(nick_name2[i])), 
               value = outlist[[i]][,2][ix],
               gene = outlist[[i]][,1][ix],
               stringsAsFactors = FALSE)
    })
  for(i in 1:4){
    gene_list_label <- get(paste("label_cdf_list", i, sep = ""))
    gene_list <- get(paste("listbox_gene_cdf_list", i, sep = ""))
    tkdelete(gene_list, 0,"end")
    tkconfigure(gene_list_label, text = paste(' n = ', (as.integer(tksize(gene_list)))))
  }  
  enesg <- data.frame(gene = outlist2, stringsAsFactors = F)
  
  for(i in seq_along(df)){
    names(df[[i]]) <- c("bin", "set", "value", "gene")
    names(use_col)[i] <- unique(df[[i]]$set)
    gene_list_label <- get(paste("label_cdf_list", i, sep = ""))
    gene_list <- get(paste("listbox_gene_cdf_list", i, sep = ""))
    if(tclvalue(tcl_checkbox_cdf_innerjoin) == 1){
      df[[i]] <- inner_join(df[[i]], enesg, by = 'gene')
      my_xlab <- unique(my_xlab)
      if(!is.na(my_xlab[i])){
        my_xlab[i] <- paste(my_xlab[i], ' n = ', length(df[[i]]$gene))
      }
    } else{
      my_xlab[i] <- paste(my_xlab[i], ' n = ', length(df[[i]]$gene))
    }
    tkconfigure(gene_list, listvariable = tclVar(paste(df[[i]]$gene)))
    tkconfigure(gene_list_label, text = paste("File", i, ' n = ', (as.integer(tksize(gene_list)))))
    
  }
  df2 <- bind_rows(df)
  GGplotC(df2, unique(my_xlab), use_col, use_header, legend_space)
  
}

# finds 2 - 4 clusers from the one active file, plotting the patterns and displaying the gene lists
FindClusters <- function() {
  if (as.integer(tksize(listbox_active_cluster)) < 1) {
    return ()
  }
  STATE[3] <<- "cluster"
  pb <<- tkProgressBar(title = "Finding clusters, please be patient!!",
                      width = 300)
  setTkProgressBar(pb, 100, label =  "Finding clusters, please be patient!!")

      R_start_bin <- as.integer(tclvalue(tcl_bin_start_cluster))
      R_stop_bin <- as.integer(tclvalue(tcl_bin_end_cluster))
      
      nick_name <-  strsplit(sub('-', '\n!', 
                                 as.character(tkget(listbox_active_cluster, 0, 'end'))), '\n!')[[1]]
      my_ref  <- LIST_DATA$gene_info[[nick_name[1]]][[nick_name[2]]][5]
      enesg <- data.frame(gene = LIST_DATA$gene_file[[nick_name[1]]]$use, stringsAsFactors = F) # here
      df <- list()
      df[[nick_name[2]]] <- inner_join(enesg, LIST_DATA$table_file[[my_ref]], by = 'gene')
      cm <- hclust.vector(df[[1]][ ,c((R_start_bin:R_stop_bin) + 1)], method = "ward")
      LIST_DATA[["clust"]] <<- cm
      close(pb)
      ClusterNumList(cm)
      
  tkraise(root)
}

# intersect gene lists creating a new one
IntersectGeneLists <- function(genelist, genename) {
  if(is.null(genelist)){
    return()
  }
  if(as.integer(tksize(listbox_intersect_combind)) > 0){
    genelist1 <- unique(c(genelist, as.character(tkget(listbox_intersect_combind, 0, 'end'))))
  }else{
    genelist1 <- unique(genelist)
  }
  # if first time load in inclusive
  if(length(genelist) < 1){
    tkmessageBox(message = "no genes in common")
    return()
  }
  genelist2 <- character()
  if(as.integer(tksize(listbox_intersect_inclusive)) > 0){
    genelist <- c(genelist, as.character(tkget(listbox_intersect_inclusive, 0, 'end')))
    genelist2 <- genelist[!(duplicated(genelist) | duplicated(genelist, fromLast = TRUE)) ]
    genelist <- unique(genelist[duplicated(genelist)])
  }
  if(as.integer(tksize(listbox_intersect_exclusive)) > 0){
    genelist2 <- c(genelist2, as.character(tkget(listbox_intersect_exclusive, 0, 'end')))
    genelist2 <- unique(genelist2)
  }
  
  tkinsert(listbox_active_intersect, 0, genename)
  tkconfigure(listbox_intersect_combind, listvariable = tclVar(genelist1))
  tkconfigure(label_intersect_combind, text = paste('n = ', (as.integer(tksize(listbox_intersect_combind)))))
  tkconfigure(listbox_intersect_inclusive, listvariable = tclVar(genelist))
  tkconfigure(label_intersect_inclusive, text = paste('n = ', (as.integer(tksize(listbox_intersect_inclusive)))))
  tkconfigure(listbox_intersect_exclusive, listvariable = tclVar(genelist2))
  tkconfigure(label_intersect_exclusive, text = paste('n = ', (as.integer(tksize(listbox_intersect_exclusive)))))
  
}

# pop up box to change color, and name, along with other plot options
handleColorSel <- function(in_list, list_set) { # lst, list_num){
  if(length(LIST_DATA$table_file) > 0 ){
    sel <-as.integer(tkcurselection(in_list))
    if(length(sel) == 0){
      return()
    }
    SampleOptionsFrame <<- tktoplevel()
    STATE[1] <<- 1
    tkbind(SampleOptionsFrame, "<Destroy>", function() OnDestroy())
    nick_name <- tclvalue(tkget(in_list, sel))
    mydot <- LIST_DATA$gene_info[[list_set]][[nick_name]][1]
    myline <- LIST_DATA$gene_info[[list_set]][[nick_name]][2]
    mycolor <- LIST_DATA$gene_info[[list_set]][[nick_name]][3]
    tmp <- mycolor
    OnUpdate <- function(){
      DActAll()
      new_name <- tclvalue(tkget(NickName))
      if(nchar(new_name) > 0){
        if(new_name != nick_name && new_name %in% names(LIST_DATA$gene_info[[list_set]])){
          new_name <- paste(new_name, '-rep?')
        }
        LIST_DATA$gene_info[[list_set]][[nick_name]][1] <<- tclvalue(tcl_dot_option)
        LIST_DATA$gene_info[[list_set]][[nick_name]][2] <<- tclvalue(tcl_line_option)
        if(new_name != nick_name){
          tkdelete(in_list, sel)
          tkinsert(in_list, sel, new_name)
          names(LIST_DATA$gene_info[[list_set]])[names(LIST_DATA$gene_info[[list_set]]) == nick_name] <<- new_name
          tkconfigure(combobox_numerator, 
                      values = names(LIST_DATA$gene_info$common))
          tkconfigure(combobox_denominator, 
                      values = names(LIST_DATA$gene_info$common))
        }
        LIST_DATA$gene_info[[list_set]][[new_name]][3] <<- tmp
        tkitemconfigure(in_list, sel, foreground = tmp)
        tkdestroy(SampleOptionsFrame)
        STATE[1] <<- 0
        tkraise(root)
      }
    }
  
    OnCOL <- function(...) {
    tmp <<- tclvalue(tcl("tk_chooseColor", initialcolor = mycolor, 
                        title="Choose color"))
    if(nchar(tmp) < 1 ){
     tmp <<- mycolor 
    }
  }
  
    tkgrid(tklabel(SampleOptionsFrame,text="Set new name and color"), 
           columnspan = 2)
    tkgrid(tk2button(SampleOptionsFrame,text = "Set Color", 
                   command = function() OnCOL()), 
         columnspan = 2, pady = c(10, 10))
  
    tkgrid(tklabel(SampleOptionsFrame, text = "Nick name"), 
           padx = c(0, 0), pady = c(10, 10))
    tkgrid(NickName <- tk2entry(SampleOptionsFrame, width = kWidth + 40), 
         column = 1, row = 2, pady = c(10, 10))
    tkinsert(NickName, 0, nick_name)
    tkgrid(tklabel(SampleOptionsFrame, text = "line"), padx = c(0, 0))
  
    combobox_lines <- tk2combobox(SampleOptionsFrame, 
                                value = kLineOptions, 
                                textvariable = tcl_line_option, 
                                state="readonly",
                                width = 10)
  tkgrid(combobox_lines, sticky = "w", column = 1, row = 3, padx = c(0, 16)) 
  tkset(combobox_lines, myline)
  
  tkgrid(tklabel(SampleOptionsFrame, text = "dot"), padx = c(0, 0), pady = c(10, 10))
  
  combobox_dots <- tk2combobox(SampleOptionsFrame, 
                               value = kDotOptions, 
                               textvariable = tcl_dot_option, state="readonly", 
                               width = 10) 
  tkgrid(combobox_dots, sticky = "w", column = 1, row = 4, padx = c(0, 16), pady = c(10, 10)) 
  tkset(combobox_dots, mydot)

  tkgrid(tk2button(SampleOptionsFrame, text = "   OK   ", command = OnUpdate), 
         columnspan = 2, pady = c(10, 10))
  }
} 

# quits program and closes plot window
QuitProgram <- function() {
  if(STATE[1] == 0){
    tkdestroy(root)
  }else{
    return()
  }
}

# restart, removes all files and lists 
ClearTableFile <- function() { 
  if(STATE[1] == 0){
      DActAll()
      sapply(names(LIST_DATA$gene_file), function(x){
        tkdelete(get(paste("listbox_" , x, "_on", sep = "")), 0, 'end')
        tkdelete(get(paste("listbox_" , x, "_off", sep = "")), 0, 'end')
        tkconfigure(get(paste("label_" , x, "_length", sep = "")), text = 'n = 0')
        if(x != "common"){
          myTCL <- get(paste0("tcl_", x, "file"))
          tclvalue(myTCL) <- x
          tkconfigure(get(paste("label_" , x, "_file", sep = "")), textvariable = myTCL)
        }
      })  
      tkconfigure(combobox_numerator, values = "numerator")
      tkset(combobox_numerator, "numerator")
      tkconfigure(combobox_denominator, values = "denominator")
      tkset(combobox_denominator, "denominator")

      LIST_DATA <<- list(table_file = list(), # [[]] gene X1 X2 ...
                        gene_file = list(),  # holds $common genes from files and $gene file(s)
                        gene_info = list())  # for holding gene file info in a list of lists, a set for $common and each $gene file(s)
      # [c("dot", "line", "color", plot?)] 
      STATE <<- c(0, 'common') # [1] for open option window, [2] for a selected box
    }
}

# Change plot axis labels + lines 
PlotLines <- function() {
  num <- tclvalue(tkget(combobox_plot_lines))
  tkdelete(entry_line_tick_pos_one, 0, 'end')
  tkinsert(entry_line_tick_pos_one, 0, list_plot_lines[[num]][1])
  tkdelete(entry_line_tick_pos_two, 0, 'end')
  tkinsert(entry_line_tick_pos_two, 0, list_plot_lines[[num]][2])
  tkdelete(entry_line_tick_pos_three, 0, 'end')
  tkinsert(entry_line_tick_pos_three, 0, list_plot_lines[[num]][3])
  tkdelete(entry_line_tick_pos_four, 0, 'end')
  tkinsert(entry_line_tick_pos_four, 0, list_plot_lines[[num]][4])
  tkdelete(entry_line_tick_pos_five, 0, 'end')
  tkinsert(entry_line_tick_pos_five, 0, list_plot_ticks[[num]][["loc"]])
  tkdelete(entry_line_tick_label_five, 0, 'end')
  tkinsert(entry_line_tick_label_five, 0, list_plot_ticks[[num]][["name"]])
  SetComboBoxes(dim(LIST_DATA$table_file[[1]]))
}

# set up root window ---- 

root <- tktoplevel() #container for it all
tkwm.title(root, version_num)

# menu setup ----
menu_top <- tk2menu(root)           # Create a menu
tkconfigure(root, menu = menu_top)  # Add it to the main window
menu_top_file <- tkmenu(menu_top, tearoff = FALSE)
tkadd(menu_top_file, "command", label = "Load table file",
      command = function() LoadTableFile())
tkadd(menu_top_file, "command", label = "Load color pallet",
      command = function() GetColor())
tkadd(menu_top_file, "command", label = "Quit", 
      command = function() QuitProgram())
tkadd(menu_top_file, "command", label = "Restart",
      command = function() ClearTableFile())
tkadd(menu_top, "cascade", label = "File", menu = menu_top_file) 
menu_top_options <- tkmenu(menu_top, tearoff = FALSE)
tkadd(menu_top_options, "command", label = "Options",
      command = function() GlobalOptions())
tkadd(menu_top, "cascade", label = "Extra's", menu = menu_top_options) 

# FILE frame for loading files and main controls ----

# frame for left half
left_frame <- tkframe(root)

# frame for notebook, math, line dot plot options

notebook_file <- tk2notebook(left_frame,
                             tabs = c("Load file", "Plot Options",
                                      "Lines & Labels", "Norm Files"))

# FILE: load file tab for Loading files ----

notebook_load_file_tab <- tk2notetab(notebook_file, "Load file")

frame_notebook_load_file_tab <- tkframe(notebook_load_file_tab, 
                                    relief = 'ridge', borderwidth = 5)

tkgrid(tklabel(frame_notebook_load_file_tab, text = tclvalue(tcl_commonfile)), columnspan = 3)

tkgrid(label_common_length <- tklabel(frame_notebook_load_file_tab, text = "n = "),
       columnspan = 3)

tkgrid(listbox_common_on <- tk2listbox(frame_notebook_load_file_tab, width = kWidth + 8, height = kHeight, 
                                       autoscroll = "none", tip = 
                                         "Double click on a file name to open options for that entry"),
       columnspan = 3)
tkgrid.columnconfigure(frame_notebook_load_file_tab,listbox_common_on,weight=1)
tkgrid.rowconfigure(frame_notebook_load_file_tab,listbox_common_on,weight=1)

tkbind(listbox_common_on, "<Double-ButtonPress-1>", function() handleColorSel(listbox_common_on, 'common'))
tkbind(listbox_common_on, "<<ListboxSelect>>", function() SelectionControl('common',  '_off'))

tkgrid(tk2button(frame_notebook_load_file_tab, text = "  <<Switch>>  ", command = function() 
  MoveSelectToOtherEntry(listbox_common_on, listbox_common_off)), 
  tk2button(frame_notebook_load_file_tab, text = "  <<All On>>  ", command = function() 
    MoveAllToOtherEntry(listbox_common_on, listbox_common_off, "on")), 
  tk2button(frame_notebook_load_file_tab, text = "  <<All Off>>  ", command = function() 
    MoveAllToOtherEntry(listbox_common_on, listbox_common_off, "off")), 
  sticky = 'we', padx = c(2,2))

tkgrid(listbox_common_off <- tk2listbox(frame_notebook_load_file_tab, width = kWidth + 8, height = kHeight, 
                                        autoscroll = "none", tip = 
                                          "files in this list wont show up on plot"),
       columnspan = 3)
tkgrid.columnconfigure(frame_notebook_load_file_tab,listbox_common_off,weight=1)
tkgrid.rowconfigure(frame_notebook_load_file_tab,listbox_common_off,weight=1)

tkbind(listbox_common_off, "<Double-ButtonPress-1>", function() handleColorSel(listbox_common_off, 'common'))
tkbind(listbox_common_off, "<<ListboxSelect>>", function() SelectionControl('common',  '_on'))

tkgrid(tk2button(frame_notebook_load_file_tab, text = " Load table file ", 
                command =  function()  LoadTableFile()), row = 7, 
       column = 0)

tkgrid(tk2button(frame_notebook_load_file_tab, text = " Intersect list ", command = function()
  IntersectGeneLists(LIST_DATA$gene_file$common$use, tclvalue(tcl_commonfile))), column = 1, row = 7)

tkgrid(tk2button(frame_notebook_load_file_tab, text = " Remove file",
                command =  function() RemoveFile()), row = 7, 
       column = 2)

tkgrid(frame_notebook_load_file_tab, sticky = 'nsew')
tkgrid.columnconfigure(notebook_load_file_tab, frame_notebook_load_file_tab,weight=1)
tkgrid.rowconfigure(notebook_load_file_tab, frame_notebook_load_file_tab,weight=1)

tkpack.configure(frame_notebook_load_file_tab, fill ="both", expand = 1)

# FILE: common Plot Option math norm and bins ----

notebook_options_tab <- tk2notetab(notebook_file, 
                                   "Plot Options")

tab_plot_options_frame <- tkframe(notebook_options_tab, 
                                  relief = 'ridge', borderwidth = 5) 

tkgrid(tk2combobox(tab_plot_options_frame,
                   value = kMathOptions,
                   textvariable = tcl_math_option, 
                   state = "readonly", width = 6), column = 0 , row = 0, padx = c(10,10), pady = c(10,10))

tkgrid(tklabel(tab_plot_options_frame, text = "Norm to bin"), column = 1 , row = 0, padx = c(10,0), pady = c(10,10))

tkgrid(combobox_norm_bin <- tk2combobox(tab_plot_options_frame,
                                        textvariable = tcl_norm_bin, state="readonly", width = 3),
       column = 2, row = 0, pady = c(10,10))

tkgrid(tkcheckbutton(tab_plot_options_frame, 
                     variable = tcl_checkbox_log2, text = "log2"), column = 0, 
       row = 1, pady = c(10,10))

checkbox_relative_frequency <- tkcheckbutton(tab_plot_options_frame, 
                                             variable = tcl_checkbox_relative_frequency, 
                                             text = "Rel Freq" )
tkgrid(checkbox_relative_frequency, column = 1 , row = 1, pady = c(10,10))

checkbox_relative_gene_frequency <- tkcheckbutton(tab_plot_options_frame, 
                                                  variable = tcl_checkbox_relative_gene_frequency, 
                                                  text = "Rel Gene  " )
tkgrid(checkbox_relative_gene_frequency, column = 2 , row = 1, pady = c(10,10))

tkgrid(tklabel(tab_plot_options_frame, text = "Zoom into bins"), column = 0 , row = 2, pady = c(10,10))

combobox_bin_start <- tk2combobox(tab_plot_options_frame, 
                                  textvariable = tcl_bin_start,
                                  state = "readonly", width = 3) 
tkgrid(combobox_bin_start, padx = c(0, 0), column = 1, row = 2, padx = c(0, 0), pady = c(10,10))
tkbind(combobox_bin_start, "<<ComboboxSelected>>", function()
  BinStartEndHelper(tcl_bin_start, tcl_bin_end, combobox_bin_start, combobox_bin_end, 1))

tkgrid(tklabel(tab_plot_options_frame, text = "to"), column = 2, row = 2, 
       padx = c(0, 0), pady = c(10,10), sticky = "w")

combobox_bin_end <- tk2combobox(tab_plot_options_frame, 
                                textvariable = tcl_bin_end,
                                state = "readonly", width = 3) 
tkgrid(combobox_bin_end, column = 2, row = 2, padx = c(0, 0), sticky = "e", pady = c(10,10))
tkbind(combobox_bin_end, "<<ComboboxSelected>>", function()
  BinStartEndHelper(tcl_bin_start, tcl_bin_end, combobox_bin_start, combobox_bin_end, 2))

tkgrid(tklabel(tab_plot_options_frame, text = "Color sets"), column = 0, row = 3, 
       padx = c(10, 0), pady = c(10,10), sticky = "w")

combobox_brewer <- tk2combobox(tab_plot_options_frame,
                               value = kBrewerList,
                                textvariable = tcl_brewer,
                                state = "readonly", width = 7) 
tkgrid(combobox_brewer, column = 0, row = 3, padx = c(0, 0), pady = c(10,10), columnspan = 2, sticky = 'e')
tkbind(combobox_brewer, "<<ComboboxSelected>>", function() BrewerSet())

tkgrid(tk2button(tab_plot_options_frame, text = '  Load color set file  ', 
                 command = function() GetColor()), columnspan = 4, pady = c(10,10)) 

tkgrid(tab_plot_options_frame, column = 0, row = 0, sticky = 'nsew')
tkpack.configure(tab_plot_options_frame, fill ="both", expand = 1)

# FILE: line and tick tab for line and tick plot options ---- 

notebook_lines_Labels_tab <- tk2notetab(notebook_file, 
                                        "Lines & Labels")

tab_plot_options_line_tick_frame <- tkframe(notebook_lines_Labels_tab, 
                                            relief = 'ridge', borderwidth = 5) 

tkgrid(tklabel(tab_plot_options_line_tick_frame, text = ' Plot Options '),
       columnspan = 6)

tkgrid(tklabel(tab_plot_options_line_tick_frame, 
               text = "Plot lines and labels"), 
       pady = c(5, 5), row = 7, column = 0, columnspan = 6)

combobox_plot_lines <- tk2combobox(tab_plot_options_line_tick_frame, 
                                   value = names(list_plot_lines), 
                                   textvariable = tcl_plot_line_name, 
                                   state = "readonly", width = 20)
tkgrid(combobox_plot_lines, column = 0, columnspan = 6, row = 6) 
tkbind(combobox_plot_lines, '<<ComboboxSelected>>', function() PlotLines())

tkgrid(tk2entry(tab_plot_options_line_tick_frame, width = 5, 
                textvariable = tcl_one_tss_tts_option),  
       padx = c(10, 0), pady = c(5, 0), column = 0, row = 8)
tkgrid(tklabel(tab_plot_options_line_tick_frame, text = 'Pos'), padx = c(5, 3), 
       pady = c(5, 0), column = 1, row = 8, sticky = "w")
entry_line_tick_pos_one <- tk2entry(tab_plot_options_line_tick_frame, 
                                    width = 4, 
                                    textvariable = tcl_pos_one_line)
tkgrid(entry_line_tick_pos_one, column = 2, row = 8, sticky = "w", 
       padx = c(0, 0), pady = c(5, 0))

tkgrid(tk2entry(tab_plot_options_line_tick_frame, width = 5, 
                textvariable = tcl_two_tss_tts_option),  
       padx = c(10, 0), pady = c(3, 0), column = 0, row = 9)
tkgrid(tklabel(tab_plot_options_line_tick_frame, text = 'Pos'), padx = c(5, 3), 
       pady = c(3, 0), column = 1, row = 9, sticky = "w")
entry_line_tick_pos_two <- tk2entry(tab_plot_options_line_tick_frame, 
                                    width = 4, 
                                    textvariable = tcl_pos_two_line)
tkgrid(entry_line_tick_pos_two, column = 2 , row = 9, sticky = "w", 
       padx = c(0, 0), pady = c(3, 0))

tkgrid(tk2entry(tab_plot_options_line_tick_frame, width = 5, 
                textvariable = tcl_three_tss_tts_option),  
       padx = c(10, 0), pady = c(5, 0), column = 3, row = 8)
tkgrid(tklabel(tab_plot_options_line_tick_frame, text = 'Pos'), 
       padx = c(5, 3), pady = c(5, 0), column = 4, row = 8, sticky = "w")
entry_line_tick_pos_three <- tk2entry(tab_plot_options_line_tick_frame, 
                                      width = 4, 
                                      textvariable = tcl_pos_three_line)
tkgrid(entry_line_tick_pos_three, column = 5, row = 8, sticky = "w", 
       padx = c(0, 0), pady = c(3, 0))

tkgrid(tk2entry(tab_plot_options_line_tick_frame, width = 5, 
                textvariable = tcl_four_tss_tts_option),  
       padx = c(10, 0), pady = c(5, 0), column = 3, row = 9)
tkgrid(tklabel(tab_plot_options_line_tick_frame, text = 'Pos'), column = 4, 
       row = 9, sticky = "w", padx = c(5, 3), pady = c(5, 0))
entry_line_tick_pos_four <- tk2entry(tab_plot_options_line_tick_frame, 
                                     width = 4, 
                                     textvariable = tcl_pos_four_line)
tkgrid(entry_line_tick_pos_four, column = 5, row = 9, sticky = "w", 
       padx = c(0, 0), pady = c(3, 0))

tkgrid(tklabel(tab_plot_options_line_tick_frame, text = "More Bin labels"), 
       row = 11, columnspan = 6, pady = c(10,2))

tkgrid(tklabel(tab_plot_options_line_tick_frame, text = 'Pos'), padx = c(5, 0), 
       column = 0, row = 12, sticky = "w")
entry_line_tick_pos_five <- tk2entry(tab_plot_options_line_tick_frame, 
                                     width = kWidth + 2,
                                     textvariable = tcl_pos_plot_ticks)
tkgrid(entry_line_tick_pos_five, column = 1, row = 12, padx = c(0, 0), 
       columnspan = 5, sticky = "w")
tkgrid(tklabel(tab_plot_options_line_tick_frame, text = 'label'), 
       padx = c(5, 0), column = 0, row = 13, sticky = "w")
entry_line_tick_label_five <- tk2entry(tab_plot_options_line_tick_frame, 
                                    width = kWidth + 2, 
                                    textvariable = tcl_label_plot_ticks)
tkgrid(entry_line_tick_label_five, column = 1, row = 13, padx = c(0, 0), 
       columnspan = 5)
tkgrid(tab_plot_options_line_tick_frame, column = 0, row = 0, sticky = 'nsew')
tkpack.configure(tab_plot_options_line_tick_frame, fill ="both", expand = 1)

# FILE: norm files tab for making norm file ----

notebook_norm_file_tab <- tk2notetab(notebook_file, 
                                             "Norm Files")
frame_norm_files_tab <- tkframe(notebook_norm_file_tab, relief = 'ridge',
                           borderwidth = 5)

tkgrid(tklabel(frame_norm_files_tab, text = "select samples for normalization"), 
       columnspan = 2, pady = c(0,10))

tkgrid(tklabel(frame_norm_files_tab, text = "divide:"), 
       padx = c(20, 0), pady = c(5, 0), column = 0, row = 1)  

combobox_numerator <- tk2combobox(frame_norm_files_tab, state = "readonly")
tkset(combobox_numerator, "numerator")
tkgrid(combobox_numerator, sticky = "w", column = 1, row = 1, padx = c(0, 16), 
       pady = c(5, 0)) 

tkgrid(tklabel(frame_norm_files_tab, text = "by:"),
       padx = c(20, 0), pady = c(0, 5), column = 0, row = 2)  

combobox_denominator <- tk2combobox(frame_norm_files_tab, state = "readonly")
tkset(combobox_denominator, "denominator")
tkgrid(combobox_denominator, sticky = "w", column = 1, row = 2, padx = c(0, 16)) 

tkgrid(tk2button(frame_norm_files_tab, text = '      Create       ', 
                command = function() MakeNormFile()), columnspan = 2, pady = c(10, 5), padx = c(20, 0))

tkgrid(frame_norm_files_tab, column = 0, row = 0, sticky = 'nsew')

tkpack.configure(frame_norm_files_tab, fill ="both", expand = 1)

# FILE grid ----

tkgrid(notebook_file, column = 0, row = 0, sticky = 'nsew')

# buttons for plot button and options ----

# tkgrid(plot_button <- tkbutton(frame_file_notebook, text = '  Plot  ', font = c('bold', 15),
#                 command = function() OnOk()), columnspan = 4) 
tkgrid(frame_plot_button <- tkframe(left_frame, relief = "groove", bg = "blue", borderwidth = 5), 
       columnspan = 4, column = 0, row = 1, sticky = 'nsew', pady = c(10,10), padx = c(20,20) ) # raised, sunken, flat, ridge, solid, and groove
tkgrid(plot_button <- tk2button(frame_plot_button, text = '  Plot  ', 
                               width = kWidth + 5, command = function() MakeDataFrame()), sticky = 'ew', padx = c(5,5)) 

# LIST frame for loading gene lists and controls ----

notebook_list <- tk2notebook(left_frame, tabs = c("Gene\nlist 1",
                                                   "Gene\nlist 2", 
                                                   "Gene\nlist 3",
                                                   "Gene\nlist 4",
                                                  "Tool\nlist"))

# LIST: tab gene list 1 ----

notebook_list_gene1_tab <- tk2notetab(notebook_list, "Gene\nlist 1")

frame_gene1_tab <- tkframe(notebook_list_gene1_tab, 
                           relief = 'ridge', borderwidth = 5)

label_gene1_file <- tklabel(frame_gene1_tab, text = tclvalue(tcl_gene1file))
tkgrid(label_gene1_file, columnspan = 3)
label_gene1_length <- tklabel(frame_gene1_tab, text = "n = ")
tkgrid(label_gene1_length, columnspan = 3)

listbox_gene1_on <- tk2listbox(frame_gene1_tab, width = kWidth + 8, height = kHeight, autoscroll = 'none',
                               tip = "load a txt file of gene lists to plot sub set")
tkgrid(listbox_gene1_on, columnspan = 3)
tkbind(listbox_gene1_on, "<Double-ButtonPress-1>", function() handleColorSel(listbox_gene1_on, 'gene1'))
tkbind(listbox_gene1_on, "<<ListboxSelect>>", function() SelectionControl('gene1',  '_off'))

tkgrid(tk2button(frame_gene1_tab, text = "<<Switch>>", command = function() 
  MoveSelectToOtherEntry(listbox_gene1_on, listbox_gene1_off, 'gene1')), 
  tk2button(frame_gene1_tab, text = "<<All On>>", command = function() 
    MoveAllToOtherEntry(listbox_gene1_on, listbox_gene1_off, "on", file_name = "gene1")), 
  tk2button(frame_gene1_tab, text = "<<All Off>>", command = function() 
    MoveAllToOtherEntry(listbox_gene1_on, listbox_gene1_off, "off", file_name = "gene1")), sticky = 'we', padx = c(2,2))
listbox_gene1_off <- tk2listbox(frame_gene1_tab, width = kWidth + 8, height = kHeight, autoscroll = 'none', 
                                tip = "files in this list wont show up on plot")
tkgrid(listbox_gene1_off, columnspan = 3)
tkbind(listbox_gene1_off, "<Double-ButtonPress-1>", function() handleColorSel(listbox_gene1_off, 'gene1'))
tkbind(listbox_gene1_off, "<<ListboxSelect>>", function() SelectionControl('gene1',  '_on'))

tkgrid(tk2button(frame_gene1_tab, text = " Load gene list ", 
                command =  function() LoadGeneFile('gene1')), 
       row = 7, 
       columnspan = 2, sticky = 'w', padx = c(3, 0))
tkgrid(tk2button(frame_gene1_tab, text = " Intersect list ",
                command =  function() IntersectGeneLists(LIST_DATA$gene_file$gene1$use, tclvalue(tcl_gene1file))), row = 7, 
       column = 1, columnspan = 2, sticky = 'e', padx = c(0, 3))
tkgrid(frame_gene1_tab, sticky = "nesw")
tkpack.configure(frame_gene1_tab, fill ="both", expand = 1)

# LIST: tab gene list 2 ----

notebook_list_gene2_tab <- tk2notetab(notebook_list, "Gene\nlist 2")

frame_gene2_tab <- tkframe(notebook_list_gene2_tab, 
                           relief = 'ridge', borderwidth = 5)

label_gene2_file <- tklabel(frame_gene2_tab, text = tclvalue(tcl_gene2file))
tkgrid(label_gene2_file, columnspan = 3)
label_gene2_length <- tklabel(frame_gene2_tab, text = "n = ")
tkgrid(label_gene2_length, columnspan = 3)

listbox_gene2_on <- tk2listbox(frame_gene2_tab, width = kWidth + 8, height = kHeight, autoscroll = 'none',
                               tip = "load a txt file of gene lists to plot sub set")
tkgrid(listbox_gene2_on, columnspan = 3)
tkbind(listbox_gene2_on, "<Double-ButtonPress-1>", function() handleColorSel(listbox_gene2_on, 'gene2'))
tkbind(listbox_gene2_on, "<<ListboxSelect>>", function() SelectionControl('gene2',  '_off'))

tkgrid(tk2button(frame_gene2_tab, text = "<<Switch>>", command = function() 
  MoveSelectToOtherEntry(listbox_gene2_on, listbox_gene2_off, 'gene2')), 
  tk2button(frame_gene2_tab, text = "<<All On>>", command = function() 
    MoveAllToOtherEntry(listbox_gene2_on, listbox_gene2_off, "on", file_name = "gene2")), 
  tk2button(frame_gene2_tab, text = "<<All Off>>", command = function() 
    MoveAllToOtherEntry(listbox_gene2_on, listbox_gene2_off, "off", file_name = "gene2")), sticky = 'we', padx = c(2,2))
listbox_gene2_off <- tk2listbox(frame_gene2_tab, width = kWidth + 8, height = kHeight, autoscroll = 'none', 
                                tip = "files in this list wont show up on plot")
tkgrid(listbox_gene2_off, columnspan = 3)
tkbind(listbox_gene2_off, "<Double-ButtonPress-1>", function() handleColorSel(listbox_gene2_off, 'gene2'))
tkbind(listbox_gene2_off, "<<ListboxSelect>>", function() SelectionControl('gene2',  '_on'))

tkgrid(tk2button(frame_gene2_tab, text = " Load gene list ", 
                command =  function() LoadGeneFile('gene2')), 
       row = 7, 
       columnspan = 2, sticky = 'w', padx = c(3, 0))
tkgrid(tk2button(frame_gene2_tab, text = " Intersect list ",
                command =  function() IntersectGeneLists(LIST_DATA$gene_file$gene2$use, tclvalue(tcl_gene2file))), row = 7, 
       column = 1, columnspan = 2, sticky = 'e', padx = c(0, 3))
tkgrid(frame_gene2_tab)
tkpack.configure(frame_gene2_tab, fill ="both", expand = 1)

# LIST: tab gene list 3 ----

notebook_list_gene3_tab <- tk2notetab(notebook_list, "Gene\nlist 3")

frame_gene3_tab <- tkframe(notebook_list_gene3_tab, 
                           relief = 'ridge', borderwidth = 5)

label_gene3_file <- tklabel(frame_gene3_tab, text = tclvalue(tcl_gene3file))
tkgrid(label_gene3_file, columnspan = 3)
label_gene3_length <- tklabel(frame_gene3_tab, text = "n = ")
tkgrid(label_gene3_length, columnspan = 3)

listbox_gene3_on <- tk2listbox(frame_gene3_tab, width = kWidth + 8, height = kHeight, autoscroll = 'none',
                               tip = "load a txt file of gene lists to plot sub set")
tkgrid(listbox_gene3_on, columnspan = 3)
tkbind(listbox_gene3_on, "<Double-ButtonPress-1>", function() handleColorSel(listbox_gene3_on, 'gene3'))
tkbind(listbox_gene3_on, "<<ListboxSelect>>", function() SelectionControl('gene3',  '_off'))

tkgrid(tk2button(frame_gene3_tab, text = "<<Switch>>", command = function() 
  MoveSelectToOtherEntry(listbox_gene3_on, listbox_gene3_off, 'gene3')), 
  tk2button(frame_gene3_tab, text = "<<All On>>", command = function() 
    MoveAllToOtherEntry(listbox_gene3_on, listbox_gene3_off, "on", file_name = "gene3")), 
  tk2button(frame_gene3_tab, text = "<<All Off>>", command = function() 
    MoveAllToOtherEntry(listbox_gene3_on, listbox_gene3_off, "off", file_name = "gene3")), sticky = 'we', padx = c(2,2))
listbox_gene3_off <- tk2listbox(frame_gene3_tab, width = kWidth + 8, height = kHeight, autoscroll = 'none', 
                                tip = "files in this list wont show up on plot")
tkgrid(listbox_gene3_off, columnspan = 3)
tkbind(listbox_gene3_off, "<Double-ButtonPress-1>", function() handleColorSel(listbox_gene3_off, 'gene3'))
tkbind(listbox_gene3_off, "<<ListboxSelect>>", function() SelectionControl('gene3',  '_on'))

tkgrid(tk2button(frame_gene3_tab, text = " Load gene list ", 
                command =  function() LoadGeneFile('gene3')), 
       row = 7, 
       columnspan = 2, sticky = 'w', padx = c(3, 0))
tkgrid(tk2button(frame_gene3_tab, text = " Intersect list ",
                command =  function() IntersectGeneLists(LIST_DATA$gene_file$gene3$use, tclvalue(tcl_gene3file))), row = 7, 
       column = 1, columnspan = 2, sticky = 'e', padx = c(0, 3))
tkgrid(frame_gene3_tab)
tkpack.configure(frame_gene3_tab, fill ="both", expand = 1)

# LIST: tab gene list 4 ----

notebook_list_gene4_tab <- tk2notetab(notebook_list, "Gene\nlist 4")

frame_gene4_tab <- tkframe(notebook_list_gene4_tab, 
                           relief = 'ridge', borderwidth = 5)

label_gene4_file <- tklabel(frame_gene4_tab, text = tclvalue(tcl_gene4file))
tkgrid(label_gene4_file, columnspan = 3)
label_gene4_length <- tklabel(frame_gene4_tab, text = "n = ")
tkgrid(label_gene4_length, columnspan = 3)

listbox_gene4_on <- tk2listbox(frame_gene4_tab, width = kWidth + 8, height = kHeight, autoscroll = 'none',
                               tip = "load a txt file of gene lists to plot sub set")
tkgrid(listbox_gene4_on, columnspan = 3)
tkbind(listbox_gene4_on, "<Double-ButtonPress-1>", function() handleColorSel(listbox_gene4_on, 'gene4'))
tkbind(listbox_gene4_on, "<<ListboxSelect>>", function() SelectionControl('gene4',  '_off'))

tkgrid(tk2button(frame_gene4_tab, text = "<<Switch>>", command = function() 
  MoveSelectToOtherEntry(listbox_gene4_on, listbox_gene4_off, 'gene4')), 
  tk2button(frame_gene4_tab, text = "<<All On>>", command = function() 
    MoveAllToOtherEntry(listbox_gene4_on, listbox_gene4_off, "on", file_name = "gene4")), 
  tk2button(frame_gene4_tab, text = "<<All Off>>", command = function() 
    MoveAllToOtherEntry(listbox_gene4_on, listbox_gene4_off, "off", file_name = "gene4")), sticky = 'we', padx = c(2,2))
listbox_gene4_off <- tk2listbox(frame_gene4_tab, width = kWidth + 8, height = kHeight, autoscroll = 'none', 
                                tip = "files in this list wont show up on plot")
tkgrid(listbox_gene4_off, columnspan = 3)
tkbind(listbox_gene4_off, "<Double-ButtonPress-1>", function() handleColorSel(listbox_gene4_off, 'gene4'))
tkbind(listbox_gene4_off, "<<ListboxSelect>>", function() SelectionControl('gene4',  '_on'))

tkgrid(tk2button(frame_gene4_tab, text = " Load gene list ", 
                command =  function() LoadGeneFile('gene4')), 
       row = 7, 
       columnspan = 2, sticky = 'w', padx = c(3, 0))
tkgrid(tk2button(frame_gene4_tab, text = " Intersect list ",
                command =  function() IntersectGeneLists(LIST_DATA$gene_file$gene4$use, tclvalue(tcl_gene4file))), row = 7, 
       column = 1, columnspan = 2, sticky = 'e', padx = c(0, 3))
tkgrid(frame_gene4_tab)
tkpack.configure(frame_gene4_tab, fill ="both", expand = 1)

# LIST: tab tool list ----

notebook_list_tool_tab <- tk2notetab(notebook_list, "Tool\nlist")

frame_tool_tab <- tkframe(notebook_list_tool_tab, 
                           relief = 'ridge', borderwidth = 5)

label_tool_file <- tklabel(frame_tool_tab, text = tclvalue(tcl_toolfile))
tkgrid(label_tool_file, columnspan = 3)
label_tool_length <- tklabel(frame_tool_tab, text = "n = ")
tkgrid(label_tool_length, columnspan = 3)

listbox_tool_on <- tk2listbox(frame_tool_tab, width = kWidth + 8, height = kHeight, autoscroll = 'none',
                               tip = "load a txt file of gene lists to plot sub set")
tkgrid(listbox_tool_on, columnspan = 3)
tkbind(listbox_tool_on, "<Double-ButtonPress-1>", function() handleColorSel(listbox_tool_on, 'tool'))
tkbind(listbox_tool_on, "<<ListboxSelect>>", function() SelectionControl('tool',  '_off'))

tkgrid(tk2button(frame_tool_tab, text = "<<Switch>>", command = function() 
  MoveSelectToOtherEntry(listbox_tool_on, listbox_tool_off, 'tool')), 
  tk2button(frame_tool_tab, text = "<<All On>>", command = function() 
    MoveAllToOtherEntry(listbox_tool_on, listbox_tool_off, "on", file_name = "tool")), 
  tk2button(frame_tool_tab, text = "<<All Off>>", command = function() 
    MoveAllToOtherEntry(listbox_tool_on, listbox_tool_off, "off", file_name = "tool")), sticky = 'we', padx = c(2,2))
listbox_tool_off <- tk2listbox(frame_tool_tab, width = kWidth + 8, height = kHeight, autoscroll = 'none', 
                                tip = "files in this list wont show up on plot")
tkgrid(listbox_tool_off, columnspan = 3)
tkbind(listbox_tool_off, "<Double-ButtonPress-1>", function() handleColorSel(listbox_tool_off, 'tool'))
tkbind(listbox_tool_off, "<<ListboxSelect>>", function() SelectionControl('tool',  '_on'))

tkgrid(tk2button(frame_tool_tab, text = " Grab tool list ", 
                 command =  function() ToolListTabHelper()), 
       row = 7, 
       columnspan = 2, sticky = 'w', padx = c(3, 0))
tkgrid(tk2button(frame_tool_tab, text = " Intersect list ",
                 command =  function() IntersectGeneLists(LIST_DATA$gene_file$tool$use, tclvalue(tcl_toolfile))), row = 7, 
       column = 1, columnspan = 2, sticky = 'e', padx = c(0, 3))
tkgrid(frame_tool_tab)
tkpack.configure(frame_tool_tab, fill ="both", expand = 1)

# LIST grid ----

tkgrid(notebook_list, column = 0, row = 3, sticky = 'nsew')

# Left half grid ----

tkgrid(left_frame, column = 0, row = 0, sticky = 'nsew')
tkgrid.columnconfigure(root,left_frame,weight=1)
tkgrid.rowconfigure(root,left_frame,weight=5)

# TOOL frame for finding gene lists ----

frame_tool <- tkframe(root)

frame_notebook_tool <- tkframe(frame_tool)

notebook_tool <- tk2notebook(frame_notebook_tool, tabs = c("Sort\nTool",
                                                           "Region\nTool", 
                                                           "Ratios\nTool",
                                                           "Cluster\nTool",
                                                           "Intersect\nTool",
                                                           "CDF\nTool"))
tkgrid(notebook_tool)

# TOOL: tab Sort tool ----

notebook_sort_tab <- tk2notetab(notebook_tool, "Sort\nTool")

frame_sort_tab <- tkframe(notebook_sort_tab, 
                          relief = 'ridge', borderwidth = 5)

label_active_sort <- tklabel(frame_sort_tab, text = "Active table files")
tkgrid(label_active_sort, columnspan = 3)

listbox_active_sort <- tk2listbox(frame_sort_tab, height = kHeight + 1, width = kWidth + 8, autoscroll = 'none',
                                  tip = "activate file(s) to use tool")
tkgrid(listbox_active_sort, columnspan = 3)

tkgrid(tk2button(frame_sort_tab, text = " Activate file(s) ", command = function()
  ActLst(listbox_active_sort, 0)), tk2button(frame_sort_tab, text = "Clear Tool", command = function()
    DActLst(listbox_active_sort, "listbox_active_gene_sort", 
            "label_active_sort_length")), padx = c(20,20))

frame_sort_tab_buttons <- tkframe(frame_sort_tab, relief = 'ridge',
                      borderwidth = 5) 

tkgrid(tklabel(frame_sort_tab_buttons, text = "Sort tools"), columnspan = 4) 

tkgrid(tk2combobox(frame_sort_tab_buttons,
                   values =  kTopBottomOptions, 
                   textvariable = tcl_top_bottom_option,
                   state = "readonly", width = 8), sticky = "w", 
       columnspan = 2, column = 0, row = 1, padx = c(50, 0)) 

tkgrid(tk2combobox(frame_sort_tab_buttons,
                   values =  kTopBottomNum, 
                   textvariable = tcl_top_bottom_num, 
                   state = "readonly", width = 3), sticky = "w", 
       columnspan = 2, column = 2, row = 1, padx = c(0, 10), pady = c(10, 10))

tkgrid(tklabel(frame_sort_tab_buttons, text = "bins"),
       padx = c(50, 0), column = 0, row = 3, sticky ="e")

combobox_bin_start_sort <- tk2combobox(frame_sort_tab_buttons, 
                                  textvariable = tcl_bin_start_sort,
                                  state = "readonly", width = 3) 
tkgrid(combobox_bin_start_sort, padx = c(0, 0), column = 1, row = 3, sticky ="e")
tkbind(combobox_bin_start_sort, "<<ComboboxSelected>>", function()
  BinStartEndHelper(tcl_bin_start_sort, tcl_bin_end_sort, combobox_bin_start_sort, combobox_bin_end_sort, 1))

tkgrid(tklabel(frame_sort_tab_buttons, text = "to"),
       column = 2, row = 3, padx = c(0, 0), sticky ="w")

combobox_bin_end_sort <- tk2combobox(frame_sort_tab_buttons, 
                                textvariable = tcl_bin_end_sort,
                                state = "readonly", width = 3) 
tkgrid(combobox_bin_end_sort, column = 3, row = 3, padx = c(0, 10), sticky ="w")
tkbind(combobox_bin_end_sort, "<<ComboboxSelected>>", function()
  BinStartEndHelper(tcl_bin_start_sort, tcl_bin_end_sort, combobox_bin_start_sort, combobox_bin_end_sort, 2))


tkgrid(tk2button(frame_sort_tab_buttons, text = "   Sort   ", 
                command =  function() SortTop()), 
       row = 4, columnspan = 2, sticky = "e", pady = c(10, 10), padx = c(10, 0))

tkgrid(tk2combobox(frame_sort_tab_buttons,
                   values =  kAccDec, 
                   textvariable = tcl_acc_dec, 
                   state = "readonly", width = 7), sticky = "w", 
       columnspan = 2, column = 2, row = 4, padx = c(0, 5), pady = c(10, 10))



tkgrid(frame_sort_tab_buttons, columnspan = 4, sticky = 'nswe')

label_active_sort_length <- tklabel(frame_sort_tab, text = "n = 0")
tkgrid(label_active_sort_length, columnspan = 3)

listbox_active_gene_sort <- tk2listbox(frame_sort_tab, height = kHeight + 7, width = kWidth + 8, 
                                  selectmode = "extended", autoscroll = 'none')
tkgrid(listbox_active_gene_sort, columnspan = 3, pady = c(5,5))

tkgrid(tk2button(frame_sort_tab, text = "plot list", command = function()
  GeneListPlotHelper(listbox_active_gene_sort), width = 15), column = 0, row = 7, pady = c(5,5))

tkgrid(tk2button(frame_sort_tab, text = "plot selected", command = function()
  SelectGeneListPlotHelper(listbox_active_gene_sort), width = 15), column = 1, row = 7, pady = c(5,5))

tkgrid(tk2button(frame_sort_tab, text = "Intersect list", command = function()
  IntersectGeneLists(as.character(tkget(listbox_active_gene_sort, 0, 'end')),
                     "sort tool"), width = 15), column = 0, row = 8, pady = c(5,5))

tkgrid(tk2button(frame_sort_tab, text = "Save list", command = function()
  SaveGenelist(listbox_active_gene_sort, listbox_active_sort, "sort"), width = 15), column = 1, row = 8, pady = c(5,5))

tkgrid(tk2button(frame_sort_tab, text = "Search list", command = function()
  FindGene(entry_sort_search, listbox_active_gene_sort), width = 15), column = 0, row = 9, pady = c(10,10))

tkgrid(entry_sort_search <- tk2entry(frame_sort_tab, tip = "Enter gene name to search", width = 20), 
       column = 1, row = 9, pady = c(10,10))

tkgrid(frame_sort_tab)
tkpack.configure(frame_sort_tab, fill = 'both', expand = 1)

# TOOL: tab Region ----

notebook_region_tab <- tk2notetab(notebook_tool, "Region\nTool")

frame_region_tab <- tkframe(notebook_region_tab, 
                            relief = 'ridge', borderwidth = 5)

label_active_region <- tklabel(frame_region_tab, text = "No Active table files")
tkgrid(label_active_region, columnspan = 3)

listbox_active_region <- tk2listbox(frame_region_tab, height = 2, width = kWidth + 10, 
                                   tip = "activate 2 files to use tool",  scroll = 'x', 
                                   autoscroll = 'none')
tkgrid(listbox_active_region, columnspan = 3)

tkgrid(tk2button(frame_region_tab, text = "Activate file(s)", command = function()
  ActLst(listbox_active_region, 3)), tk2button(frame_region_tab, text = "Clear Tool", command = function()
    DActLst(listbox_active_region, sapply(c("up","down","between"), function(x){
      paste("listbox_gene_region_" , x, sep = "")}),
      sapply(c("up","down","between"), function(x){
        paste("label_region_" , x, sep = "")}))))

frame_region_tab_buttons <- tkframe(frame_region_tab, relief = 'ridge',
                                  borderwidth = 5) 

tkgrid(tklabel(frame_region_tab_buttons, text = "region tools"), columnspan = 4, padx = c(50, 0)) 

tkgrid(tklabel(frame_region_tab_buttons,
                   text = "fold change"), sticky = "w", 
       columnspan = 2, column = 0, row = 1, padx = c(50, 0)) 

tkgrid(tk2combobox(frame_region_tab_buttons,
                   values =  kFoldList, 
                   textvariable = tcl_bin_fold_region, 
                   state = "readonly", width = 3), sticky = "we", 
       columnspan = 2, column = 2, row = 1, padx = c(0, 5), pady = c(10, 10))

tkgrid(tklabel(frame_region_tab_buttons, text = "bins"),
       padx = c(50, 0), column = 0, row = 3, sticky ="e")

combobox_bin_start_region <- tk2combobox(frame_region_tab_buttons, 
                                  textvariable = tcl_bin_start_region,
                                  state = "readonly", width = 3) 
tkgrid(combobox_bin_start_region, padx = c(0, 0), column = 1, row = 3, sticky ="w")
tkbind(combobox_bin_start_region, "<<ComboboxSelected>>", function()
  BinStartEndHelper(tcl_bin_start_region, tcl_bin_end_region, combobox_bin_start_region, combobox_bin_end_region, 1))

tkgrid(tklabel(frame_region_tab_buttons, text = "to"),
       column = 2, row = 3, padx = c(0, 0), sticky ="w")
combobox_bin_end_region <- tk2combobox(frame_region_tab_buttons, 
                                textvariable = tcl_bin_end_region,
                                state = "readonly", width = 3) 
tkgrid(combobox_bin_end_region, column = 3, row = 3, padx = c(0, 10), sticky ="w")
tkbind(combobox_bin_end_region, "<<ComboboxSelected>>", function()
  BinStartEndHelper(tcl_bin_start_region, tcl_bin_end_region, combobox_bin_start_region, combobox_bin_end_region, 2))


tkgrid(tk2button(frame_region_tab_buttons, text = "   region   ", 
                command =  function() CompareRegions()), 
       column = 0, row = 4, columnspan = 4, pady = c(10, 10), padx = c(50, 0))

tkgrid(frame_region_tab_buttons, columnspan = 4, sticky = 'nsew')

notebook_region <- tk2notebook(frame_region_tab, 
                             tabs = c("File 1 up", "File 2 up",
                                      "Fold inbetween"))
tkgrid(notebook_region, columnspan = 4)

notebook_region_up_tab <- tk2notetab(notebook_region, "File 1 up")

frame_region_notbook_up <- tkframe(notebook_region_up_tab)

tkgrid(label_region_up <- tklabel(frame_region_notbook_up, text = "n = "), 
       columnspan = 3)

tkgrid(listbox_gene_region_up <- tk2listbox(frame_region_notbook_up, height = kHeight + 9, width = kWidth + 8,
                                           selectmode = "extended", autoscroll = 'none'),
       columnspan = 3, pady = c(5,5))
tkgrid(frame_region_notbook_up)

notebook_region_down_tab <- tk2notetab(notebook_region, "File 2 up")


frame_region_notbook_down <- tkframe(notebook_region_down_tab)

tkgrid(label_region_down <- tklabel(frame_region_notbook_down, text = "n = "), 
       columnspan = 3)

tkgrid(listbox_gene_region_down <- tk2listbox(frame_region_notbook_down, height = kHeight + 9, width = kWidth + 8,
                                           selectmode = "extended", autoscroll = 'none'),
       columnspan = 3, pady = c(5,5))
tkgrid(frame_region_notbook_down)

notebook_region_between_tab <- tk2notetab(notebook_region, "Fold inbetween")

frame_region_notbook_between <- tkframe(notebook_region_between_tab)

tkgrid(label_region_between <- tklabel(frame_region_notbook_between, text = "n = "), 
       columnspan = 3)

tkgrid(listbox_gene_region_between <- tk2listbox(frame_region_notbook_between, height = kHeight + 9 , width = kWidth + 8, 
                                       selectmode = "extended", autoscroll = 'none'), 
       columnspan = 3, pady = c(5,5))
tkgrid(frame_region_notbook_between)

tkgrid(tk2button(frame_region_tab, text = "plot list", command = function()
  GeneListPlotHelper("region"), width = 15), column = 0, row = 7, pady = c(5,5))

tkgrid(tk2button(frame_region_tab, text = "plot selected", command = function()
  SelectGeneListPlotHelper("region"), width = 15), column = 1, row = 7, pady = c(5,5))

tkgrid(tk2button(frame_region_tab, text = "Intersect list", command = function()
  IntersectGeneLists(as.character(tkget(ListBoxSelectHelper("region"), 0, 'end')),
                     paste("region-", tk2notetab.text(notebook_region))),
  width = 15), column = 0, row = 8, pady = c(5,5))

tkgrid(tk2button(frame_region_tab, text = "Save list", command = function()
  SaveGenelist("region", listbox_active_region, "region"), width = 15), column = 1, row = 8, pady = c(5,5))

tkgrid(tk2button(frame_region_tab, text = "Search list", command = function()
  FindGene(entry_region_search, "region"), width = 15), column = 0, row = 9, pady = c(10,10))

tkgrid(entry_region_search <- tk2entry(frame_region_tab, tip = "Enter gene name to search", width = 20), 
       column = 1, row = 9, pady = c(10,10))

tkgrid(frame_region_tab)
tkpack.configure(frame_region_tab, fill = 'both', expand = 1)

# TOOL: tab Ratios ----

notebook_ratios_tab <- tk2notetab(notebook_tool, "Ratios\nTool")

frame_ratios_tab <- tkframe(notebook_ratios_tab, 
                            relief = 'ridge', borderwidth = 5)

label_active_ratios <- tklabel(frame_ratios_tab, text = "No Active table files")
tkgrid(label_active_ratios, columnspan = 3)

listbox_active_ratios <- tk2listbox(frame_ratios_tab, height = 2, width = kWidth + 10, scroll = 'x',
                                    autoscroll = 'none')
tkgrid(listbox_active_ratios, columnspan = 3)

tkgrid(tk2button(frame_ratios_tab, text = "Activate file(s)", command = function()
  ActLst(listbox_active_ratios, 3)), tk2button(frame_ratios_tab, text = "Clear Tool", command = function()
    DActLst(listbox_active_ratios, sapply(c("up","down","between"), function(x){
      paste("listbox_gene_ratios_" , x, sep = "")}),
      sapply(c("up","down","between"), function(x){
        paste("label_ratios_" , x, sep = "")}))))

frame_ratios_tab_buttons <- tkframe(frame_ratios_tab, relief = 'ridge',
                                   borderwidth = 5) 

tkgrid(tklabel(frame_ratios_tab_buttons, text = "ratios tools"), columnspan = 4, padx = c(50, 0)) 

tkgrid(tklabel(frame_ratios_tab_buttons,
               text = "fold change"), sticky = "w", 
       columnspan = 2, column = 0, row = 1, padx = c(50, 0)) 

tkgrid(tk2combobox(frame_ratios_tab_buttons,
                   values =  kFoldList, 
                   textvariable = tcl_bin_fold_ratios, 
                   state = "readonly", width = 3), sticky = "we", 
       columnspan = 2, column = 2, row = 1, padx = c(0, 5), pady = c(10, 10))

tkgrid(tklabel(frame_ratios_tab_buttons, text = "bins"),
       padx = c(50, 0), column = 0, row = 3, sticky ="e")

combobox_bin_start1_ratios <- tk2combobox(frame_ratios_tab_buttons, 
                                        textvariable = tcl_bin_start1_ratios,
                                        state = "readonly", width = 3) 
tkgrid(combobox_bin_start1_ratios, padx = c(0, 0), column = 1, row = 3, sticky ="w")
tkbind(combobox_bin_start1_ratios, "<<ComboboxSelected>>", function()
  BinStartEndHelper(tcl_bin_start1_ratios, tcl_bin_end1_ratios, combobox_bin_start1_ratios, combobox_bin_end1_ratios, 1))

tkgrid(tklabel(frame_ratios_tab_buttons, text = "to"),
       column = 2, row = 3, padx = c(0, 0), sticky ="w")
combobox_bin_end1_ratios <- tk2combobox(frame_ratios_tab_buttons, 
                                      textvariable = tcl_bin_end1_ratios,
                                      state = "readonly", width = 3) 
tkgrid(combobox_bin_end1_ratios, column = 3, row = 3, padx = c(0, 0), sticky ="w")
tkbind(combobox_bin_end1_ratios, "<<ComboboxSelected>>", function()
  BinStartEndHelper(tcl_bin_start1_ratios, tcl_bin_end1_ratios, combobox_bin_start1_ratios, combobox_bin_end1_ratios, 2))

tkgrid(tklabel(frame_ratios_tab_buttons, text = " / "),
       padx = c(0, 0), column = 4, row = 3, sticky ="w")

tkgrid(tklabel(frame_ratios_tab_buttons, text = "bins"),
       padx = c(50, 0), column = 0, row = 4, sticky ="e")

combobox_bin_start2_ratios <- tk2combobox(frame_ratios_tab_buttons, 
                                         textvariable = tcl_bin_start2_ratios,
                                         state = "readonly", width = 3) 
tkgrid(combobox_bin_start2_ratios, padx = c(0, 0), column = 1, row = 4, sticky ="w")
tkbind(combobox_bin_start2_ratios, "<<ComboboxSelected>>", function()
  BinStartEndHelper(tcl_bin_start2_ratios, tcl_bin_end2_ratios, combobox_bin_start2_ratios, combobox_bin_end2_ratios, 1))


tkgrid(tklabel(frame_ratios_tab_buttons, text = "to"),
       column = 2, row = 4, padx = c(0, 0), sticky ="w")
combobox_bin_end2_ratios <- tk2combobox(frame_ratios_tab_buttons, 
                                       textvariable = tcl_bin_end2_ratios,
                                       state = "readonly", width = 3) 
tkgrid(combobox_bin_end2_ratios, column = 3, row = 4, padx = c(0, 0), sticky ="w")
tkbind(combobox_bin_end2_ratios, "<<ComboboxSelected>>", function()
  BinStartEndHelper(tcl_bin_start2_ratios, tcl_bin_end2_ratios, combobox_bin_start2_ratios, combobox_bin_end2_ratios, 2))

tkgrid(tk2button(frame_ratios_tab_buttons, text = "   ratios   ", 
                command =  function() CompareRatios()), 
       column = 0, row = 5, columnspan = 4, pady = c(10, 10), padx = c(50, 0))

tkgrid(frame_ratios_tab_buttons, columnspan = 4, sticky = 'nsew')

notebook_ratios <- tk2notebook(frame_ratios_tab, 
                              tabs = c("File 1 up", "File 2 up",
                                       "Fold inbetween"))
tkgrid(notebook_ratios, columnspan = 4)

notebook_ratios_up_tab <- tk2notetab(notebook_ratios, "File 1 up")


frame_ratios_notbook_up <- tkframe(notebook_ratios_up_tab)

tkgrid(label_ratios_up <- tklabel(frame_ratios_notbook_up, text = "n = "), 
       columnspan = 3)

tkgrid(listbox_gene_ratios_up <- tk2listbox(frame_ratios_notbook_up, height = kHeight + 7, width = kWidth + 8,
                                           selectmode = "extended", autoscroll = 'none'),
       columnspan = 3, pady = c(5,5))
tkgrid(frame_ratios_notbook_up)

notebook_ratios_down_tab <- tk2notetab(notebook_ratios, "File 2 up")


frame_ratios_notbook_down <- tkframe(notebook_ratios_down_tab)

tkgrid(label_ratios_down <- tklabel(frame_ratios_notbook_down, text = "n = "), 
       columnspan = 3)

tkgrid(listbox_gene_ratios_down <- tk2listbox(frame_ratios_notbook_down, height = kHeight + 7, width = kWidth + 8,
                                             selectmode = "extended", autoscroll = 'none'),
       columnspan = 3, pady = c(5,5))
tkgrid(frame_ratios_notbook_down)

notebook_ratios_between_tab <- tk2notetab(notebook_ratios, "Fold inbetween")

frame_ratios_notbook_between <- tkframe(notebook_ratios_between_tab)

tkgrid(label_ratios_between <- tklabel(frame_ratios_notbook_between, text = "n = "), 
       columnspan = 3)

tkgrid(listbox_gene_ratios_between <- tk2listbox(frame_ratios_notbook_between, height = kHeight + 7, width = kWidth + 8, 
                                                selectmode = "extended", autoscroll = 'none'), 
       columnspan = 3, pady = c(5,5))
tkgrid(frame_ratios_notbook_between)

tkgrid(tk2button(frame_ratios_tab, text = "plot list", command = function()
  GeneListPlotHelper("ratios"), width = 15), column = 0, row = 7, pady = c(5,5))

tkgrid(tk2button(frame_ratios_tab, text = "plot selected", command = function()
  SelectGeneListPlotHelper("ratios"), width = 15), column = 1, row = 7, pady = c(5,5))

tkgrid(tk2button(frame_ratios_tab, text = "Intersect list", command = function()
  IntersectGeneLists(as.character(tkget(ListBoxSelectHelper("ratios"), 0, 'end')),
                     paste("ratios-", tk2notetab.text(notebook_ratios))),
  width = 15), column = 0, row = 8, pady = c(5,5))

tkgrid(tk2button(frame_ratios_tab, text = "Save list", command = function()
  SaveGenelist("ratios", listbox_active_ratios, "ratios"), width = 15), column = 1, row = 8, pady = c(5,5))

tkgrid(tk2button(frame_ratios_tab, text = "Search list", command = function()
  FindGene(entry_ratios_search, "ratios"), width = 15), column = 0, row = 9, pady = c(10,10))

tkgrid(entry_ratios_search <- tk2entry(frame_ratios_tab, tip = "Enter gene name to search", width = 20), 
       column = 1, row = 9, pady = c(10,10))

tkgrid(frame_ratios_tab)
tkpack.configure(frame_ratios_tab, fill = 'both', expand = 1)

# TOOL: tab cluster ----

notebook_cluster_tab <- tk2notetab(notebook_tool, "Cluster\nTool")

frame_cluster_tab <- tkframe(notebook_cluster_tab, 
                             relief = 'ridge', borderwidth = 5)

label_active_cluster <- tklabel(frame_cluster_tab, text = "No Active table files")
tkgrid(label_active_cluster, columnspan = 3)

listbox_active_cluster <- tk2listbox(frame_cluster_tab, height = 1, width = kWidth + 10,
                                     scroll = 'x', autoscroll = 'none')
tkgrid(listbox_active_cluster, columnspan = 3)

tkgrid(tk2button(frame_cluster_tab, text = "Activate file(s)", command = function()
  ActLst(listbox_active_cluster, 2)), tk2button(frame_cluster_tab, text = "Clear Tool", command = function()
    DActLst(listbox_active_cluster, sapply(c(1:4), function(x){
      paste("listbox_gene_cluster_list" , x, sep = "")}),
      sapply(c(1:4), function(x){
        paste("label_cluster_list" , x, sep = "")}))))

frame_cluster_tab_buttons <- tkframe(frame_cluster_tab, relief = 'ridge',
                                   borderwidth = 5) 

tkgrid(tklabel(frame_cluster_tab_buttons, text = "cluster tools"), columnspan = 4, 
       pady = c(0, 10), padx = c(50, 0)) 

tkgrid(tklabel(frame_cluster_tab_buttons, text = "bins"),
       padx = c(50, 0), column = 0, row = 3, sticky ="e")

combobox_bin_start_cluster <- tk2combobox(frame_cluster_tab_buttons, 
                                        textvariable = tcl_bin_start_cluster,
                                        state = "readonly", width = 3) 
tkgrid(combobox_bin_start_cluster, padx = c(0, 0), 
       column = 1, row = 3, sticky ="w")
tkbind(combobox_bin_start_cluster, "<<ComboboxSelected>>", function()
  BinStartEndHelper(tcl_bin_start_cluster, tcl_bin_end_cluster, combobox_bin_start_cluster, combobox_bin_end_cluster, 1))

tkgrid(tklabel(frame_cluster_tab_buttons, text = "to"),
       column = 2, row = 3, padx = c(0, 0), sticky ="w")

combobox_bin_end_cluster <- tk2combobox(frame_cluster_tab_buttons, 
                                      textvariable = tcl_bin_end_cluster,
                                      state = "readonly", width = 3) 
tkgrid(combobox_bin_end_cluster, column = 3, row = 3, padx = c(0, 10), sticky ="w")
tkbind(combobox_bin_end_cluster, "<<ComboboxSelected>>", function()
  BinStartEndHelper(tcl_bin_start_cluster, tcl_bin_end_cluster, combobox_bin_start_cluster, combobox_bin_end_cluster, 2))

tkgrid(tk2button(frame_cluster_tab_buttons, text = "   cluster   ", 
                command =  function() FindClusters()), 
       column = 0, row = 4, columnspan = 3, pady = c(10, 10), padx = c(50, 0))

combobox_bin_cluster_num <- tk2combobox(frame_cluster_tab_buttons, 
                                          textvariable = tcl_bin_cluster_num,
                                          state = "readonly", width = 3) 
tkgrid(combobox_bin_cluster_num, padx = c(0, 0), 
       column = 3, row = 4, sticky ="w")
tkbind(combobox_bin_cluster_num, "<<ComboboxSelected>>", function() ClusterNumList(LIST_DATA$clust))
tkgrid(frame_cluster_tab_buttons, columnspan = 4, sticky = 'nsew')

tkgrid(tk2button(frame_cluster_tab_buttons, text = "   Quantiles   ", 
                 command =  function() FindQuantile()), 
       column = 0, row = 5, columnspan = 4, pady = c(5, 5))

notebook_cluster <- tk2notebook(frame_cluster_tab, 
                              tabs = c("list 1", "list 2",
                                       "list 3", "list 4"))
tkgrid(notebook_cluster, columnspan = 4)

notebook_cluster_list1_tab <- tk2notetab(notebook_cluster, "list 1")


frame_cluster_notbook_list1 <- tkframe(notebook_cluster_list1_tab)

tkgrid(label_cluster_list1 <- tklabel(frame_cluster_notbook_list1, text = "n = "), 
       columnspan = 3)

tkgrid(listbox_gene_cluster_list1 <- tk2listbox(frame_cluster_notbook_list1, height = kHeight + 10, width = kWidth + 8,
                                           selectmode = "extended", autoscroll = 'none'),
       columnspan = 3, pady = c(5,5))
tkgrid(frame_cluster_notbook_list1)

notebook_cluster_list2_tab <- tk2notetab(notebook_cluster, "list 2")


frame_cluster_notbook_list2 <- tkframe(notebook_cluster_list2_tab)

tkgrid(label_cluster_list2 <- tklabel(frame_cluster_notbook_list2, text = "n = "), 
       columnspan = 3)

tkgrid(listbox_gene_cluster_list2 <- tk2listbox(frame_cluster_notbook_list2, height = kHeight + 10, width = kWidth + 8,
                                             selectmode = "extended", autoscroll = 'none'),
       columnspan = 3, pady = c(5,5))
tkgrid(frame_cluster_notbook_list2)

notebook_cluster_list3_tab <- tk2notetab(notebook_cluster, "list 3")

frame_cluster_notbook_list3 <- tkframe(notebook_cluster_list3_tab)

tkgrid(label_cluster_list3 <- tklabel(frame_cluster_notbook_list3, text = "n = "), 
       columnspan = 3)

tkgrid(listbox_gene_cluster_list3 <- tk2listbox(frame_cluster_notbook_list3, height = kHeight + 10, width = kWidth + 8, 
                                                selectmode = "extended", autoscroll = 'none'), 
       columnspan = 3, pady = c(5,5))
tkgrid(frame_cluster_notbook_list3)

notebook_cluster_list4_tab <- tk2notetab(notebook_cluster, "list 4")

frame_cluster_notbook_list4 <- tkframe(notebook_cluster_list4_tab)

tkgrid(label_cluster_list4 <- tklabel(frame_cluster_notbook_list4, text = "n = "), 
       columnspan = 3)

tkgrid(listbox_gene_cluster_list4 <- tk2listbox(frame_cluster_notbook_list4, height = kHeight + 10, width = kWidth + 8, 
                                                selectmode = "extended", autoscroll = 'none'), 
       columnspan = 3, pady = c(5,5))
tkgrid(frame_cluster_notbook_list4)

tkgrid(tk2button(frame_cluster_tab, text = "plot list", command = function()
  GeneListPlotHelper("cluster"), width = 15), column = 0, row = 7, pady = c(5,5))

tkgrid(tk2button(frame_cluster_tab, text = "plot selected", command = function()
  SelectGeneListPlotHelper("cluster"), width = 15), column = 1, row = 7, pady = c(5,5))

tkgrid(tk2button(frame_cluster_tab, text = "Intersect list", command = function()
  IntersectGeneLists(as.character(tkget(ListBoxSelectHelper("cluster"), 0, 'end')),
                     paste("cluster-", tk2notetab.text(notebook_cluster))),
  width = 15), column = 0, row = 8, pady = c(5,5))

tkgrid(tk2button(frame_cluster_tab, text = "Save list", command = function()
  SaveGenelist("cluster", listbox_active_cluster, STATE[3]), width = 15), column = 1, row = 8, pady = c(5,5))

tkgrid(tk2button(frame_cluster_tab, text = "Search list", command = function()
  FindGene(entry_cluster_search, "cluster"), width = 15), column = 0, row = 9, pady = c(10,10))

tkgrid(entry_cluster_search <- tk2entry(frame_cluster_tab, tip = "Enter gene name to search", width = 20), 
       column = 1, row = 9, pady = c(10,10))

tkgrid(frame_cluster_tab)
tkpack.configure(frame_cluster_tab, fill = 'both', expand = 1)

# TOOL: tab Intersect ----

notebook_intersect_tab <- tk2notetab(notebook_tool, "Intersect\nTool")

frame_intersect_tab <- tkframe(notebook_intersect_tab, 
                               relief = 'ridge', borderwidth = 5)

label_active_intersect <- tklabel(frame_intersect_tab, text = "Active table files")
tkgrid(label_active_intersect, columnspan = 3)

listbox_active_intersect <- tk2listbox(frame_intersect_tab, height = kHeight + 1, width = kWidth + 8, autoscroll = 'none',
                                  tip = "activate file(s) to use tool")
tkgrid(listbox_active_intersect, columnspan = 3)

tkgrid(tk2button(frame_intersect_tab, text = "Clear Tool", command = function()
  DActLst(listbox_active_intersect, c("listbox_intersect_inclusive", 
                                      "listbox_intersect_exclusive", "listbox_intersect_combind"),
          c("label_intersect_inclusive", "label_intersect_exclusive", "label_intersect_combind"))), columnspan = 4)

frame_intersect_tab_buttons <- tkframe(frame_intersect_tab, relief = 'ridge',
                                  borderwidth = 5) 

tkgrid(tklabel(frame_intersect_tab_buttons, text = "Intersect tools"), padx = c(20,20)) 

tkgrid(tk2button(frame_intersect_tab_buttons, text = "   load file   ", 
                 command =  function() IntersectLoadFile()),pady = c(10, 10), padx = c(20,20))

tkgrid(frame_intersect_tab_buttons, columnspan = 2, sticky = 'nsew')

notebook_intersect <- tk2notebook(frame_intersect_tab, 
                                tabs = c("Inclusive", "Exclusive", "Combind"))
tkgrid(notebook_intersect, columnspan = 4)

notebook_intersect_inclusive_tab <- tk2notetab(notebook_intersect, "Inclusive")


frame_intersect_inclusive <- tkframe(notebook_intersect_inclusive_tab)

tkgrid(label_intersect_inclusive <- tklabel(frame_intersect_inclusive, text = "n = "), 
       columnspan = 3)

tkgrid(listbox_intersect_inclusive <- tk2listbox(frame_intersect_inclusive, height = kHeight + 10, width = kWidth + 8,
                                                selectmode = "extended", autoscroll = 'none'),
       columnspan = 3, pady = c(5,5))
tkgrid(frame_intersect_inclusive)

notebook_intersect_exclusive_tab <- tk2notetab(notebook_intersect, "Exclusive")


frame_intersect_exclusive <- tkframe(notebook_intersect_exclusive_tab)

tkgrid(label_intersect_exclusive <- tklabel(frame_intersect_exclusive, text = "n = "), 
       columnspan = 3)

tkgrid(listbox_intersect_exclusive <- tk2listbox(frame_intersect_exclusive, height = kHeight + 10, width = kWidth + 8,
                                                 selectmode = "extended", autoscroll = 'none'),
       columnspan = 3, pady = c(5,5))
tkgrid(frame_intersect_exclusive)

notebook_intersect_combind_tab <- tk2notetab(notebook_intersect, "Combind")


frame_intersect_combind <- tkframe(notebook_intersect_combind_tab)

tkgrid(label_intersect_combind <- tklabel(frame_intersect_combind, text = "n = "), 
       columnspan = 3)

tkgrid(listbox_intersect_combind <- tk2listbox(frame_intersect_combind, height = kHeight + 10, width = kWidth + 8,
                                                 selectmode = "extended", autoscroll = 'none'),
       columnspan = 3, pady = c(5,5))
tkgrid(frame_intersect_combind)

tkgrid(tk2button(frame_intersect_tab, text = "plot list", command = function()
  GeneListPlotHelper("intersect"), width = 15), column = 0, row = 7, pady = c(5,5))

tkgrid(tk2button(frame_intersect_tab, text = "plot selected", command = function()
  SelectGeneListPlotHelper("intersect"), width = 15), column = 1, row = 7, pady = c(5,5))

tkgrid(tk2button(frame_intersect_tab, text = "Save list", command = function()
  SaveGenelist("intersect", listbox_active_intersect, "intersect"), width = 15), column = 1, row = 8, pady = c(5,5))

tkgrid(tk2button(frame_intersect_tab, text = "Search list", command = function()
  FindGene(entry_intersect_search, "intersect"), width = 15), column = 0, row = 9, pady = c(10,10))

tkgrid(entry_intersect_search <- tk2entry(frame_intersect_tab, tip = "Enter gene name to search", width = 20), 
       column = 1, row = 9, pady = c(10,10))

tkgrid(frame_intersect_tab)
tkpack.configure(frame_intersect_tab, fill = 'both', expand = 1)

# TOOL: tab Cumulative Distribution  ----

notebook_cdf_tab <- tk2notetab(notebook_tool, "CDF\nTool")

frame_cdf_tab <- tkframe(notebook_cdf_tab, 
                            relief = 'ridge', borderwidth = 5)

label_active_cdf <- tklabel(frame_cdf_tab, text = "No Active table files")
tkgrid(label_active_cdf, columnspan = 3)

listbox_active_cdf <- tk2listbox(frame_cdf_tab, height = 4, width = kWidth + 8, 
                                 tip = "activate 4 files to use tool",  scroll = 'x',
                                    autoscroll = 'none')
tkgrid(listbox_active_cdf, columnspan = 3)

tkgrid(tk2button(frame_cdf_tab, text = "Activate file(s)", command = function()
  ActLst(listbox_active_cdf, 5)), tk2button(frame_cdf_tab, text = "Clear Tool", command = function()
    DActLst(listbox_active_cdf, NULL, NULL)))

frame_cdf_tab_buttons <- tkframe(frame_cdf_tab, relief = 'ridge',
                                    borderwidth = 5) 

tkgrid(tklabel(frame_cdf_tab_buttons, text = "cdf tools"), columnspan = 4, padx = c(50, 0)) 

tkgrid(tklabel(frame_cdf_tab_buttons, text = "bins"),
       padx = c(10, 0), column = 0, row = 3, sticky ="e")

combobox_bin_start1_cdf <- tk2combobox(frame_cdf_tab_buttons, 
                                          textvariable = tcl_bin_start1_cdf,
                                          state = "readonly", width = 3) 
tkgrid(combobox_bin_start1_cdf, padx = c(0, 0), column = 1, row = 3, sticky ="w")
tkbind(combobox_bin_start1_cdf, "<<ComboboxSelected>>", function()
  BinStartEndHelper(tcl_bin_start1_cdf, tcl_bin_end1_cdf, combobox_bin_start1_cdf, combobox_bin_end1_cdf, 1))

tkgrid(tklabel(frame_cdf_tab_buttons, text = "to"),
       column = 2, row = 3, padx = c(0, 0), sticky ="w")
combobox_bin_end1_cdf <- tk2combobox(frame_cdf_tab_buttons, 
                                        textvariable = tcl_bin_end1_cdf,
                                        state = "readonly", width = 3) 
tkgrid(combobox_bin_end1_cdf, column = 3, row = 3, padx = c(0, 0), sticky ="w")
tkbind(combobox_bin_end1_cdf, "<<ComboboxSelected>>", function()
  BinStartEndHelper(tcl_bin_start1_cdf, tcl_bin_end1_cdf, combobox_bin_start1_cdf, combobox_bin_end1_cdf, 2))

tkgrid(tklabel(frame_cdf_tab_buttons, text = " / "),
       padx = c(0, 0), column = 4, row = 3, sticky ="w")

tkgrid(tklabel(frame_cdf_tab_buttons, text = "bins"),
       padx = c(10, 0), column = 0, row = 4, sticky ="e")

combobox_bin_start2_cdf <- tk2combobox(frame_cdf_tab_buttons, 
                                          textvariable = tcl_bin_start2_cdf,
                                          state = "readonly", width = 3) 
tkgrid(combobox_bin_start2_cdf, padx = c(0, 0), column = 1, row = 4, sticky ="w")
tkbind(combobox_bin_start2_cdf, "<<ComboboxSelected>>", function()
  BinStartEndHelper(tcl_bin_start2_cdf, tcl_bin_end2_cdf, combobox_bin_start2_cdf, combobox_bin_end2_cdf, 1))


tkgrid(tklabel(frame_cdf_tab_buttons, text = "to"),
       column = 2, row = 4, padx = c(0, 0), sticky ="w")
combobox_bin_end2_cdf <- tk2combobox(frame_cdf_tab_buttons, 
                                        textvariable = tcl_bin_end2_cdf,
                                        state = "readonly", width = 3) 
tkgrid(combobox_bin_end2_cdf, column = 3, row = 4, padx = c(0, 0), sticky ="w")
tkbind(combobox_bin_end2_cdf, "<<ComboboxSelected>>", function()
  BinStartEndHelper(tcl_bin_start2_cdf, tcl_bin_end2_cdf, combobox_bin_start2_cdf, combobox_bin_end2_cdf, 2))

tkgrid(tk2button(frame_cdf_tab_buttons, text = "   cdf plot  ", 
                 command =  function() CumulativeDistribution()), 
       column = 0, row = 5, columnspan = 2, pady = c(5, 5), padx = c(10, 0))

tkgrid(tk2combobox(frame_cdf_tab_buttons,
                   values =  kAccDec, 
                   textvariable = tcl_acc_dec_cdf, 
                   state = "readonly", width = 3), sticky = "w", 
       columnspan = 2, column = 1, row = 5, padx = c(55, 5), pady = c(5, 5))

tkgrid(tkcheckbutton(frame_cdf_tab_buttons, variable = tcl_checkbox_cdf_innerjoin, text = "merge lists?"),
       columnspan = 2, column = 3, row = 5)

tkgrid(tklabel(frame_cdf_tab_buttons, text = "rm low hi"),
       padx = c(10, 0), column = 0, row = 6, sticky ="w")

tkgrid(tk2combobox(frame_cdf_tab_buttons,
                   values =  kQuntile, 
                   textvariable = tcl_quntile_cdf_bottom, 
                   state = "readonly", width = 4), sticky = "w", 
       column = 1, row = 6, padx = c(0, 5), pady = c(5, 5))

tkgrid(tk2combobox(frame_cdf_tab_buttons,
                   values =  kQuntile, 
                   textvariable = tcl_quntile_cdf_top, 
                   state = "readonly", width = 4), sticky = "w", 
       columnspan = 3, column = 2, row = 6, padx = c(0, 5), pady = c(5, 5))

tkgrid(frame_cdf_tab_buttons, columnspan = 4, sticky = 'nsew')

notebook_cdf <- tk2notebook(frame_cdf_tab, 
                                tabs = c("list 1", "list 2",
                                         "list 3", "list 4"))
tkgrid(notebook_cdf, columnspan = 4)

notebook_cdf_list1_tab <- tk2notetab(notebook_cdf, "list 1")


frame_cdf_notbook_list1 <- tkframe(notebook_cdf_list1_tab)

tkgrid(label_cdf_list1 <- tklabel(frame_cdf_notbook_list1, text = "n = "), 
       columnspan = 3)

tkgrid(listbox_gene_cdf_list1 <- tk2listbox(frame_cdf_notbook_list1, height = kHeight + 7, width = kWidth + 8,
                                                selectmode = "extended", autoscroll = 'none'),
       columnspan = 3, pady = c(5,5))
tkgrid(frame_cdf_notbook_list1)

notebook_cdf_list2_tab <- tk2notetab(notebook_cdf, "list 2")


frame_cdf_notbook_list2 <- tkframe(notebook_cdf_list2_tab)

tkgrid(label_cdf_list2 <- tklabel(frame_cdf_notbook_list2, text = "n = "), 
       columnspan = 3)

tkgrid(listbox_gene_cdf_list2 <- tk2listbox(frame_cdf_notbook_list2, height = kHeight + 7, width = kWidth + 8,
                                                selectmode = "extended", autoscroll = 'none'),
       columnspan = 3, pady = c(5,5))
tkgrid(frame_cdf_notbook_list2)

notebook_cdf_list3_tab <- tk2notetab(notebook_cdf, "list 3")

frame_cdf_notbook_list3 <- tkframe(notebook_cdf_list3_tab)

tkgrid(label_cdf_list3 <- tklabel(frame_cdf_notbook_list3, text = "n = "), 
       columnspan = 3)

tkgrid(listbox_gene_cdf_list3 <- tk2listbox(frame_cdf_notbook_list3, height = kHeight + 7, width = kWidth + 8, 
                                                selectmode = "extended", autoscroll = 'none'), 
       columnspan = 3, pady = c(5,5))
tkgrid(frame_cdf_notbook_list3)

notebook_cdf_list4_tab <- tk2notetab(notebook_cdf, "list 4")

frame_cdf_notbook_list4 <- tkframe(notebook_cdf_list4_tab)

tkgrid(label_cdf_list4 <- tklabel(frame_cdf_notbook_list4, text = "n = "), 
       columnspan = 3)

tkgrid(listbox_gene_cdf_list4 <- tk2listbox(frame_cdf_notbook_list4, height = kHeight + 7, width = kWidth + 8, 
                                                selectmode = "extended", autoscroll = 'none'), 
       columnspan = 3, pady = c(5,5))
tkgrid(frame_cdf_notbook_list4)

tkgrid(tk2button(frame_cdf_tab, text = "plot list", command = function()
  GeneListPlotHelper("cdf"), width = 15), column = 0, row = 7, pady = c(5,5))

tkgrid(tk2button(frame_cdf_tab, text = "plot selected", command = function()
  SelectGeneListPlotHelper("cdf"), width = 15), column = 1, row = 7, pady = c(5,5))

tkgrid(tk2button(frame_cdf_tab, text = "Intersect list", command = function()
  IntersectGeneLists(as.character(tkget(ListBoxSelectHelper("cdf"), 0, 'end')),
                     paste("cdf-", tk2notetab.text(notebook_cluster)))
  , width = 15), column = 0, row = 8, pady = c(5,5))

tkgrid(tk2button(frame_cdf_tab, text = "Save list", command = function()
  SaveGenelist('cdf', listbox_active_cdf, "cdf"), width = 15), column = 1, row = 8, pady = c(5,5))

tkgrid(tk2button(frame_cdf_tab, text = "Search list", command = function()
  FindGene(entry_cdf_search, 'cdf'), width = 15), column = 0, row = 9, pady = c(10,10))

tkgrid(entry_cdf_search <- tk2entry(frame_cdf_tab, tip = "Enter gene name to search", width = 20), 
       column = 1, row = 9, pady = c(10,10))

tkgrid(frame_cdf_tab)
tkpack.configure(frame_cdf_tab, fill = 'both', expand = 1)

# TOOL grid ----

tkgrid(frame_notebook_tool, sticky = 'nsew')
tkgrid(frame_tool, column = 1, row = 0, sticky = 'nsew')

# end ----

tkgrid.columnconfigure(root,frame_tool,weight=1)
tkgrid.rowconfigure(root,frame_tool,weight=1)

try(tk2theme("keramik"), silent = TRUE)
tkraise(root)
tkbind(root,"<FocusIn>", function() OpenWindowControl())
# }
# expandTk()
