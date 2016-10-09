
my_clust <-NULL


# finds 1st median mean and 3ed max bin locations within bin ranges
# one or multiple files
# inputs peak width, file(s)
# output four tabs
#set +/- amount ... change max to peaks
SortFindMaxPeaks <- function() { 
  if (as.integer(tksize(listbox_active_cdf)) < 1 ) {
    return ()
  }
  nick_name <- as.character(tkget(listbox_active_cdf, 0, 'end'))
  lc <- 0
  my_ref <- NULL
  nick_name2 <-NULL
  enesg1 <- NULL
  enesg2 <- NULL
  enesg3 <- NULL
  enesg4 <- NULL
  df <- list()
  clist <- list()
  gene_info <- list(list())
  lapply(nick_name, function(j){
    nick_name1 <- strsplit(sub('-', '\n!',j), '\n!')[[1]]
    nick_name2 <<- c(nick_name2, nick_name1[2], nick_name1[2], nick_name1[2], nick_name1[2])
    my_ref1  <- LIST_DATA$gene_info[[nick_name1[1]]][[nick_name1[2]]][5]
    my_ref <<- c(my_ref, my_ref1, my_ref1,my_ref1,my_ref1)
    enesg <- data.frame(gene = LIST_DATA$gene_file[[nick_name1[1]]]$use, stringsAsFactors = F)
    df[[nick_name1[2]]] <<- inner_join(enesg, LIST_DATA$table_file[[my_ref1]], by = 'gene')
    ttt <- apply(df[[nick_name1[2]]][,-1], 1, function(x) which.max(x))
    tt <- summary(ttt)
    my_clust[[j]] <<- pam((ttt), 6)
    print(tt)
    enesg1 <<- c(enesg1, df[[nick_name1[2]]][findInterval(ttt, c(floor(tt[[2]]-2),ceiling(tt[[2]]+2)),rightmost.closed = T)==1L,1])
    enesg2 <<- c(enesg2, df[[nick_name1[2]]][findInterval(ttt, c(floor(tt[[3]]-2),ceiling(tt[[3]]+2)),rightmost.closed = T)==1L,1])
    enesg3 <<- c(enesg3, df[[nick_name1[2]]][findInterval(ttt, c(floor(tt[[4]]-2),ceiling(tt[[4]]+2)),rightmost.closed = T)==1L,1])
    enesg4 <<- c(enesg4, df[[nick_name1[2]]][findInterval(ttt, c(floor(tt[[5]]-2),ceiling(tt[[5]]+2)),rightmost.closed = T)==1L,1])
    if(lc > 0){
      enesg1 <<- unique(enesg1[duplicated(enesg1)])
      enesg2 <<- unique(enesg2[duplicated(enesg2)])
      enesg3 <<- unique(enesg3[duplicated(enesg3)])
      enesg4 <<- unique(enesg4[duplicated(enesg4)])
    }
    lc <<- lc + 1
  })
  
    for(i in 1:4){
      color_safe <- i %% length(kListColorSet)
      if (color_safe == 0) {
        color_safe <- 1
      }
      gene_list_label <- get(paste("label_cdf_list", i, sep = ""))
      gene_list <- get(paste("listbox_gene_cdf_list", i, sep = ""))
      my_gene_list <- unlist(get(paste("enesg", i, sep = "")))
     
      if(length(my_gene_list) > 0){
        clist[[paste("max", i, sep = "")]]$use <- my_gene_list
        color_select <- kListColorSet[color_safe]
        gene_info[[paste("max", i, sep = "")]][[my_ref[i]]] <- c(kDotOptions[1],
                                                                kLineOptions[1], 
                                                                color_select, 
                                                                1,
                                                                nick_name2[i])
      }
      tkdelete(gene_list, 0,"end")
      tkconfigure(gene_list_label, text = paste(' n = ', (as.integer(tksize(gene_list)))))
      tkconfigure(gene_list, listvariable = tclVar(my_gene_list))
      tkconfigure(gene_list_label, text = paste(tclvalue((get(paste0("tcl_max",i,"file")))), 
                                                ' n = ', (as.integer(tksize(gene_list)))))
    }
 MakeDataFrame(sel_list = NULL, table_file = df, gene_file = clist, gene_info = gene_info)
    
}	

SortFindMaxPeaks()

# reports bins within range of focuse bin
# num_bins and bin focus
# one tab
TssMaxPeaks <- function(num_bins = 2, bin_focuse = 40) { 
  if (as.integer(tksize(listbox_active_sort)) < 1) {
    return ()
  }
  lc <- 0
  outlist <- NULL
  nick_name <- as.character(tkget(listbox_active_sort, 0, 'end'))
  lapply(nick_name, function(j){
    nick_name2 <- strsplit(sub('-', '\n!',j), '\n!')[[1]]
    my_ref  <- LIST_DATA$gene_info[[nick_name2[1]]][[nick_name2[2]]][5]
    enesg <- data.frame(gene = LIST_DATA$gene_file[[nick_name2[1]]]$use, stringsAsFactors = F)
    df <- inner_join(enesg, LIST_DATA$table_file[[my_ref]], by = 'gene')
    ttt <- apply(df[,-1], 1, function(x) which.max(x))
    outlist <<- c(outlist, df[findInterval(ttt, c(floor(bin_focuse-num_bins),ceiling(bin_focuse+num_bins)),rightmost.closed = T)==1L,1])
    if(lc > 0){
      outlist <<- unique(outlist[duplicated(outlist)])
    }
    lc <<- lc + 1
  } )
    tkdelete(listbox_active_gene_sort, 0, 'end')
  if(length(outlist) > 0){
    tkconfigure(listbox_active_gene_sort, listvariable = tclVar(as.character(outlist)))
  }
  tkconfigure(label_active_sort_length, text = paste('n = ', (as.integer(tksize(listbox_active_gene_sort)))))
}	

TssMaxPeaks()

# finds peaks within range of width, can be restricted to a position, and start width
# max_width >= num_bins <= min_width, center +/- start end be part on ccbox control 
# add max min peak range
# one tab output
#make own tool
TssMaxPeaks2 <- function(center_peak = NULL, num_bins = 1, peak_width_max = 5, peak_hight_fold = 2) { 
  if (as.integer(tksize(listbox_active_sort)) < 1) {
    return ()
  }
  R_start_bin <- as.integer(tclvalue(tcl_bin_start_sort))
  R_end_bin <- as.integer(tclvalue(tcl_bin_end_sort))
  # for item in active list make sorted list, then merge sort=T, then pull out request
  lc <- 0
  outlist <- NULL
  nick_name <- as.character(tkget(listbox_active_sort, 0, 'end'))
  lapply(nick_name, function(j){
    nick_name2 <- strsplit(sub('-', '\n!',j), '\n!')[[1]]
    my_ref  <- LIST_DATA$gene_info[[nick_name2[1]]][[nick_name2[2]]][5]
    enesg <- data.frame(gene = LIST_DATA$gene_file[[nick_name2[1]]]$use, stringsAsFactors = F)
    df <- inner_join(enesg, LIST_DATA$table_file[[my_ref]], by = 'gene')
    ttt <- apply(df[,-1], 1, function(x) which.max(x))
    if(!is.null(center_peak)){
      ix <- findInterval(ttt, c(floor(center_peak-num_bins),ceiling(center_peak+num_bins)),rightmost.closed = T)==1L
      df <- df[ix,]
      ttt <- ttt[ix]
    }
    
    tt <- sapply(seq_along(ttt), function(i){
      if(R_start_bin <= (ttt[i]-num_bins) & (ttt[i]+num_bins) <= R_end_bin){
        background <- mean(unlist(df[i,-1][R_start_bin:R_end_bin][findInterval(R_start_bin:R_end_bin, c(ttt[i]-num_bins,ttt[i]+num_bins),rightmost.closed = T)==1L]),	na.rm = T)
        peak <- mean(unlist(df[i,-1][(ttt[i]-num_bins):(ttt[i]+num_bins)]),	na.rm = T)
        while(peak*peak_hight_fold > background & (ttt[i]-num_bins) > 0 & (ttt[i]+num_bins) <= ncol(df[,-1])){
          num_bins <- num_bins + 1
          peak <- mean(unlist(df[i,ttt[i]-num_bins:ttt[i]+num_bins]),	na.rm = T)
        }
      } else{
        num_bins <- ncol(df[,-1])
      }
      num_bins*2
    })
    print(tt)
    outlist <<- c(outlist, df[tt <= peak_width_max, 1])
    if(lc > 0){
      outlist <<- unique(outlist[duplicated(outlist)])
    }
    lc <<- lc + 1
  } )
  tkdelete(listbox_active_gene_sort, 0, 'end')
  if(length(outlist) > 0){
    tkconfigure(listbox_active_gene_sort, listvariable = tclVar(as.character(outlist)))
  }
  tkconfigure(label_active_sort_length, text = paste('n = ', (as.integer(tksize(listbox_active_gene_sort)))))
}	

TssMaxPeaks2(center_peak = NULL, peak_width_max = 5, peak_hight_fold = 5)
