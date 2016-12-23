# Created by Benjamin Erickson  ----

# expandTk <- function(){
version_num <- 'Ben Tools v.07.c'
# For plotting Bentley lab table files and generating gene list

kHeight <- 5 # 1 for smaller screens and PCs
kWidth <- 30 # use smaller for small screens

# load packages ----

# program for loading packages
my_packages <- function(x) {
  for (i in x) {
    #  require returns TRUE invisibly if it was able to load package
    if (!require(i , character.only = TRUE)) {
      #  If package was not able to be loaded then re-install
      install.packages(i , dependencies = TRUE)
      #  Load package after installing
      require(i , character.only = TRUE)
    }
  }
}

# run load needed pakages using my_pakages(x)
suppressPackageStartupMessages(my_packages(
  c(
    "ggplot2",
    "tcltk2",
    "dplyr",
    "tidyr",
    "readr",
    "fastcluster",
    "RColorBrewer"
  )
))

# values for comboboxs ----

# types of dots to be plotted
kDotOptions <-
  c(
    "none",
    "square",
    "circle",
    "triangle point up",
    "diamond",
    "Biger circle",
    "smaller circle"
  )

# types lines to be plotted
kLineOptions <-
  c(
    "solid line",
    "dashed line",
    "dotted line",
    "dot dash line",
    "long dash line",
    "two dash line",
    "No line"
  )

# Brewer color sets to be avalible
kBrewerList <-
  c("Accent",
    "Dark2",
    "Paired",
    "Pastel1",
    "Pastel2",
    "Set1",
    "Set2",
    "Set3")

# color Brewer set that is active to use in plot
kListColorSet <- brewer.pal(8, kBrewerList[3])

# math options avalible
kMathOptions <- c("mean", "sum", "median", "var")

# dropdown otions avalible
kTopBottomOptions <- c("Top%", "Bottom%", "Quick%")
kInOutOptions <- c("inclusive", "exclusive")
kTopBottomNum <- c(1, 2, seq(5, 95, by = 5), 99, 100)
kAccDec <- c("Accend", "Deccend")
kQuntile <- c("0%", "1%", paste0(seq(5, 45, 5), "%"))
kFoldList <-
  round(c(1, seq(1.1, 2, by = 0.2), 2, seq(2.1, 4, by = 0.2), 4), digits = 4)
kLineType <- c("dotted", "solid")

# plot TSS, TTS and body transistion locations
list_plot_lines <-
  list(
    "543 bins 20,20,40" = c(15.5, 45.5, 20.5, 40.5),
    "543 bins 10,10,10" = c(5.5, 25.5, 10.5, 20.5),
    "5 prim 2k 2k 40bins" = c(20.5, 0, 0, 0),
    "5 prim 1k 1k 80bins" = c(40.5, 0, 0, 0)
  )

# plot axis ticks and lables
list_plot_ticks <- list(
  "543 bins 20,20,40" =
    list(
      'name' = c('-1450 -950 -450 450 950 1450 1950 2450 2950 3450'),
      'loc' = c(1, 6, 11, 50, 55, 60, 65, 70, 75, 80)
    ),
  "543 bins 10,10,10" =
    list('name' = c('-450', '450'),
         'loc' = c(1, 30)),
  "5 prim 2k 2k 40bins" =
    list(
      'name' = c('-2000', '-1000', '-500', '500', '1000', '2000'),
      'loc' = c(1, 10, 15, 25, 30, 40)
    ),
  "5 prim 1k 1k 80bins" =
    list(
      'name' = c('-1000', '-500', '500', '1000'),
      'loc' = c(1, 20, 60, 80)
    )
)

# plot TSS, TTS and body transistion lables
tss_tts_options <- c('TSS', 'PolyA', '500', '500')

# tcl starting values ----
tcl_tss_start_bin <- tclVar(1)
tcl_tss_start_label <- tclVar(-1450)
tcl_tss_num_bp <- tclVar(500)
tcl_tss_size_bin <- tclVar(100)
tcl_tss_number_entry <- tclVar(3)
tcl_tts_start_bin <- tclVar(50)
tcl_tts_start_label <- tclVar(450)
tcl_tts_num_bp <- tclVar(500)
tcl_tts_size_bin <- tclVar(100)
tcl_tts_number_entry <- tclVar(7)
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
tcl_peaks1file <- tclVar("max bins 1")
tcl_peaks2file <- tclVar("max bins 2")
tcl_peaks3file <- tclVar("max bins 3")
tcl_peaks4file <- tclVar("max bins 4")
tcl_toolfile <- tclVar("tool")
tcl_line_option <- tclVar(kLineOptions[1])
tcl_dot_option <- tclVar(kDotOptions[1])
tcl_top_bottom_option <- tclVar(kTopBottomOptions[1])
tcl_top_bottom_num <- tclVar(kTopBottomNum[7])
tcl_acc_dec <- tclVar(kAccDec[2])
tcl_acc_dec_cdf <- tclVar(kAccDec[2])
tcl_quntile_cdf_bottom <- tclVar(kQuntile[1])
tcl_quntile_cdf_top <- tclVar(kQuntile[1])
tcl_bin_center_width_range <- tclVar(3)
tcl_bin_end_width_peaks <- tclVar(5)
tcl_in_out_option_ratios <- tclVar(kInOutOptions[1])
tcl_bin_fold_ratios <- tclVar(kFoldList[6])
tcl_math_option <- tclVar(kMathOptions[1])
tcl_norm_bin <- tclVar(0)
tcl_bin_start <- tclVar(1)
tcl_bin_end <- tclVar(1)
tcl_brewer <- tclVar(kBrewerList[3])
tcl_bin_start_peaks <- tclVar(1)
tcl_bin_end_peaks <- tclVar(1)
tcl_bin_start1_ratios <- tclVar(0)
tcl_bin_end1_ratios <- tclVar(0)
tcl_bin_start2_ratios <- tclVar(0)
tcl_bin_end2_ratios <- tclVar(0)
tcl_bin_start1_cdf <- tclVar(1)
tcl_bin_end1_cdf <- tclVar(1)
tcl_bin_start2_cdf <- tclVar(1)
tcl_bin_end2_cdf <- tclVar(1)
tcl_bin_start_cluster <- tclVar(1)
tcl_bin_end_cluster <- tclVar(1)
tcl_bin_cluster_num <- tclVar(4)
tcl_bin_start_sort <- tclVar(1)
tcl_bin_end_sort <- tclVar(1)
tcl_Y_axis_min <- tclVar()
tcl_Y_axis_max <- tclVar()
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
tcl_checkbox_Y_axis <- tclVar(0)
tcl_smooth <- tclVar(0)
tcl_checkbox_relative_gene_frequency <- tclVar(0)
tcl_checkbox_log2 <- tclVar(0)
tcl_checkbox_comment <-
  tclVar(0) # 1 = don't show save comment popup
tcl_checkbox_split <-
  tclVar(0) # change the output of saved gene lists
tcl_checkbox_cdf_innerjoin <-
  tclVar(1) # merge the output lists or not
tcl_tss_tts_line_type <- tclVar(kLineType[1])
tcl_body_line_type <- tclVar(kLineType[2])

# file list varibles  ----

# Master Data list container for holding table files, gene lists, and file and plot option info
LIST_DATA <- list(
  table_file = list(),
  # [[]] gene X1 X2 ...
  gene_file = list(),
  # holds $common genes from files and $gene file(s)
  gene_info = list(),
  # for holding gene file info in a list of lists, a set for $common and each $gene file(s) [c("dot", "line", "color", plot?, NickName)]
  clust = list()
)      # Cluster holder

# contol varible for keeping track when a second window is open
STATE <-
  c(0, 'common', 'cluster') # [1] for open option window, [2] for a selected box, [3] tool use help

# functions ----

# tests weather a option window is open and promps you to close it
OpenWindowControl <- function() {
  if (STATE[1] == 1) {
    STATE[1] <<- 0
    tkdestroy(SampleOptionsFrame)
    tkraise(root)
  } else if (STATE[1] == 3) {
    STATE[1] <<- 0
    tkdestroy(CommentFrame)
    tkraise(root)
  } else if (STATE[1] == 4) {
    STATE[1] <<- 0
    #print("close OptionsFrame")
    tkdestroy(OptionsFrame)
    tkraise(root)
  }
  if (STATE[1] == 2) {
    # tklower(root)
    # tkmessageBox(message = "close window please", icon = "warning")
  }
}

# if option window is destroyed update state
OnDestroy <- function() {
  STATE[1] <<- 0
  tkraise(root)
}

# helper for filling up lines and positons entries
LableMaker <- function() {
  tss_start_bin <- as.numeric(tclvalue(tcl_tss_start_bin))
  tss_start_label <- as.numeric(tclvalue(tcl_tss_start_label))
  tss_num_bp <- as.numeric(tclvalue(tcl_tss_num_bp))
  tss_size_bin <- as.numeric(tclvalue(tcl_tss_size_bin))
  tss_number_entry <- as.numeric(tclvalue(tcl_tss_number_entry))
  tts_start_bin <- as.numeric(tclvalue(tcl_tts_start_bin))
  tts_start_label <- as.numeric(tclvalue(tcl_tts_start_label))
  tts_num_bp <- as.numeric(tclvalue(tcl_tts_num_bp))
  tts_size_bin <- as.numeric(tclvalue(tcl_tts_size_bin))
  tts_number_entry <- as.numeric(tclvalue(tcl_tts_number_entry))
  
  # TODO include some other protections to some entries being bigger then others that cause problmes
  tssbin <- NULL
  ttsbin <- NULL
  tsslab <- NULL
  ttslab <- NULL
  if (tss_number_entry != 0 &
      !is.na(tss_start_bin) &
      !is.na(tss_start_label) &
      !is.na(tss_num_bp) & !is.na(tss_number_entry) &
      !is.na(tss_size_bin)) {
    if (tss_number_entry > 1) {
      tss_number_entry2 <- 2:tss_number_entry
    } else {
      tss_number_entry2 <- tss_number_entry
    }
    mysteptss <- floor(tss_num_bp / tss_size_bin)
    tssbin <- c(tss_start_bin,
                seq(
                  tss_start_bin,
                  (mysteptss * tss_number_entry) + tss_start_bin,
                  mysteptss
                )[tss_number_entry2])
    tsslab <- c(tss_start_label,
                seq(
                  tss_start_label,
                  (tss_num_bp * tss_number_entry) + tss_start_label,
                  tss_num_bp
                )[tss_number_entry2])
  }
  if (tts_number_entry != 0 &
      !is.na(tts_start_bin) &
      !is.na(tts_start_label) &
      !is.na(tts_num_bp) & !is.na(tts_number_entry) &
      !is.na(tts_size_bin)) {
    if (tts_number_entry > 1) {
      tts_number_entry2 <- 2:tts_number_entry
    } else {
      tts_number_entry2 <- tts_number_entry
    }
    mysteptts <- floor(tts_num_bp / tts_size_bin)
    ttsbin <- c(tts_start_bin,
                seq(
                  tts_start_bin,
                  (mysteptts * tts_number_entry) + tts_start_bin,
                  mysteptts
                )[tts_number_entry2])
    ttslab <- c(tts_start_label,
                seq(
                  tts_start_label,
                  (tts_num_bp * tts_number_entry) +
                    tts_start_label,
                  tts_num_bp
                )[tts_number_entry2])
  }
  tt <- c(tssbin, ttsbin)
  ttt <- c(tsslab, ttslab)
  tt <- unique(sort(tt))#[ttt != 0]))
  ttt <- unique(sort(ttt))#[ttt != 0]))
  if (!is.null(tt)) {
    tkdelete(entry_line_tick_pos_five, 0, "end")
    tkinsert(entry_line_tick_pos_five, 0, tt)
  }
  
  if (!is.null(ttt)) {
    tkdelete(entry_line_tick_label_five, 0, "end")
    tkinsert(entry_line_tick_label_five, 0, ttt)
  }
}

# clears selection of boxes
SelectionControl <- function(box_set, on_off) {
  tkselection.clear(get(paste("listbox_" , STATE[2], on_off, sep = "")), 0, 'end')
  if (box_set != STATE[2]) {
    onlist <- get(paste("listbox_" , STATE[2], "_on", sep = ""))
    offlist <- get(paste("listbox_" , STATE[2], "_off", sep = ""))
    tkselection.clear(onlist, 0, 'end')
    tkselection.clear(offlist, 0, 'end')
    STATE[2] <<- box_set
  }
}

# moves all items from one list to the other
MoveAllToOtherEntry <-
  function(onlist, offlist, direction, file_name = "common") {
    if (length(LIST_DATA$table_file) > 0) {
      listnames <- names(LIST_DATA$gene_info[[file_name]])
      
      if (direction == "on") {
        tkdelete(offlist, 0, 'end')
        tkdelete(onlist, 0, 'end')
        tkconfigure(onlist, listvariable = tclVar(listnames))
        lc <- 0
        for (i in listnames) {
          LIST_DATA$gene_info[[file_name]][[i]][4] <<- 1
          tkitemconfigure(onlist, lc, foreground = LIST_DATA$gene_info[[file_name]][[i]][3])
          lc <- lc + 1
        }
      }
      if (direction == "off") {
        tkdelete(offlist, 0, 'end')
        tkdelete(onlist, 0, 'end')
        tkconfigure(offlist, listvariable = tclVar(listnames))
        lc <- 0
        for (i in listnames) {
          LIST_DATA$gene_info[[file_name]][[i]][4] <<- 0
          tkitemconfigure(offlist, lc, foreground = LIST_DATA$gene_info[[file_name]][[i]][3])
          lc <- lc + 1
        }
      }
    }
  }

#moves selected items from one list to the other
MoveSelectToOtherEntry <-
  function(onlist, offlist, file_name = "common") {
    for (i in rev(as.integer(tkcurselection(onlist)))) {
      tkinsert(offlist, "end", tclvalue(tkget(onlist, i)))
      LIST_DATA$gene_info[[file_name]][[tclvalue(tkget(onlist, i))]][4] <<-
        0
      tkitemconfigure(offlist, "end", foreground = LIST_DATA$gene_info[[file_name]][[tclvalue(tkget(onlist, i))]][3])
      tkdelete(onlist, i)
    }
    for (i in rev(as.integer(tkcurselection(offlist)))) {
      tkinsert(onlist, "end", tclvalue(tkget(offlist, i)))
      LIST_DATA$gene_info[[file_name]][[tclvalue(tkget(offlist, i))]][4] <<-
        1
      tkitemconfigure(onlist, "end", foreground = LIST_DATA$gene_info[[file_name]][[tclvalue(tkget(offlist, i))]][3])
      tkdelete(offlist, i)
    }
  }

# keeps numbers, empty string for the rest
# from https://github.com/gsk3/taRifx/blob/master/R/Rfunctions.R#L1161
Destring <- function(x, keep = "0-9.-") {
  return(as.numeric(gsub(paste(
    "[^", keep, "]+", sep = ""
  ), "", x)))
}

# basic test for valid colors
isColor <- function(x) {
  res <- try(col2rgb(x), silent = TRUE)
  return(!"try-error" %in% class(res))
}

# helper for keeping start and end bins in check
BinStartEndHelper <-
  function(startbin,
           endbin,
           combostart,
           comboend,
           num) {
    start <- as.numeric(tclvalue(startbin))
    end <- as.numeric(tclvalue(endbin))
    if (num == 1 && start > end) {
      tkset(comboend, start)
    } else if (num == 2 && start > end) {
      tkset(combostart, end)
    } else if (start == 0) {
      tkset(comboend, start)
    }
  }

# find quantiles
FindQuantile <- function() {
  if (as.integer(tksize(listbox_active_cluster)) < 1) {
    return ()
  }
  STATE[3] <<- "quantile"
  pb <<-
    tkProgressBar(title = "Finding clusters, please be patient!!",
                  width = 300)
  setTkProgressBar(pb, 100, label =  "Finding clusters, please be patient!!")
  
  R_start_bin <- as.integer(tclvalue(tcl_bin_start_cluster)) + 1
  R_end_bin <- as.integer(tclvalue(tcl_bin_end_cluster)) + 1
  
  nick_name <-  strsplit(sub('-', '\n!',
                             as.character(
                               tkget(listbox_active_cluster, 0, 'end')
                             )), '\n!')[[1]]
  my_ref  <- LIST_DATA$gene_info[[nick_name[1]]][[nick_name[2]]][5]
  enesg <-
    data_frame(gene = LIST_DATA$gene_file[[nick_name[1]]]$use) # here
  df <- list()
  df[[nick_name[2]]] <-
    semi_join(LIST_DATA$table_file[[my_ref]], enesg, by = 'gene')
  
  apply_bins <- group_by(df[[1]], gene) %>%
    filter(bin %in% R_start_bin:R_end_bin) %>%
    summarise(mysum = sum(score, na.rm = TRUE)) %>%
    mutate(q1 = ntile(mysum, 4)) %>%
    ungroup()
  
  clist <- list()
  gene_info <- list(list())
  for (i in 1:4) {
    color_safe <- i %% length(kListColorSet)
    if (color_safe == 0) {
      color_safe <- 1
    }
    
    my_gene_list <- collapse(select(arrange(filter(
      apply_bins,
      q1 == i
    ), mysum), gene))[[1]]
    
    if (length(my_gene_list) > 0) {
      clist[[paste("listq", i, sep = "")]]$use <- my_gene_list
      color_select <- kListColorSet[color_safe]
      gene_info[[paste("listq", i, sep = "")]][[my_ref]] <-
        c(kDotOptions[1],
          kLineOptions[1],
          color_select,
          1,
          nick_name[2])
    }
    gene_list_label <- get(paste("label_cluster_list", i, sep = ""))
    gene_list <-
      get(paste("listbox_gene_cluster_list", i, sep = ""))
    tkconfigure(gene_list, listvariable = tclVar(my_gene_list))
    tkconfigure(gene_list_label, text = paste('quantile', i, ' n = ', (as.integer(
      tksize(gene_list)
    ))))
  }
  close(pb)
  MakeDataFrame(
    sel_list = NULL,
    table_file = df,
    gene_file = clist,
    gene_info = gene_info
  )
  tkraise(root)
}

# Change the number of clusters
ClusterNumList <- function(cm) {
  if (as.numeric(tksize(listbox_active_cluster)) < 1 |
      length(cm$cm) < 1 | STATE[3] != 'cluster') {
    return()
  }
  DActLst(listbox_active_sort,
          sapply(c(1:4), function(x) {
            paste("listbox_gene_cluster_list" , x, sep = "")
          }),
          sapply(c(1:4), function(x) {
            paste("label_cluster_list" , x, sep = "")
          }))
  R_cluster <- as.integer(tclvalue(tcl_bin_cluster_num))
  nick_name <-  strsplit(sub('-', '\n!',
                             as.character(
                               tkget(listbox_active_cluster, 0, 'end')
                             )), '\n!')[[1]]
  my_ref  <- LIST_DATA$gene_info[[nick_name[1]]][[nick_name[2]]][5]
  enesg <-
    data_frame(gene = (cm$use))# here
  if (length(enesg$gene) < R_cluster) {
    return()
  }
  df <- list()
  df[[nick_name[2]]] <-
    semi_join(LIST_DATA$table_file[[my_ref]], enesg, by = 'gene')
  clist <- list()
  gene_info <- list(list())
  
  for (i in 1:R_cluster) {
    clist[[paste("list", i, sep = "")]]$use <-
      as.data.frame(enesg[cutree(cm$cm, R_cluster) == i, ])$gene
    
    color_safe <- i %% length(kListColorSet)
    if (color_safe == 0) {
      color_safe <- 1
    }
    
    color_select <- kListColorSet[color_safe]
    gene_info[[paste("list", i, sep = "")]][[my_ref]] <-
      c(kDotOptions[1],
        kLineOptions[1],
        color_select,
        1,
        nick_name[2])
    gene_list_label <- get(paste("label_cluster_list", i, sep = ""))
    gene_list <-
      get(paste("listbox_gene_cluster_list", i, sep = ""))
    tkconfigure(gene_list, listvariable = tclVar(paste(clist[[i]]$use)))
    tkconfigure(gene_list_label, text = paste('cluster', i, ' n = ', (as.integer(
      tksize(gene_list)
    ))))
  }
  
  MakeDataFrame(
    sel_list = NULL,
    table_file = df,
    gene_file = clist,
    gene_info = gene_info
  )
  
}

# helper for finding what gene list is active
ListBoxSelectHelper <- function(listboxgene) {
  if (listboxgene[1] == "peaks") {
    my_tab <-
      strsplit(tk2notetab.text(notebook_peaks), split = " ")[[1]]
    listboxgene <-
      get(paste("listbox_gene_peaks_list" , my_tab[2] , sep = ""))
  } else if (listboxgene[1] == "ratios") {
    my_tab <- tk2notetab.text(notebook_ratios)
    if (my_tab == "File 2 up") {
      listboxgene <- listbox_gene_ratios_down
    } else if (my_tab == "Fold inbetween") {
      listboxgene <- listbox_gene_ratios_between
    } else{
      listboxgene <- listbox_gene_ratios_up
    }
  } else if (listboxgene[1] == "cluster") {
    my_tab <-
      strsplit(tk2notetab.text(notebook_cluster), split = " ")[[1]]
    listboxgene <-
      get(paste("listbox_gene_cluster_list" , my_tab[2] , sep = ""))
  } else if (listboxgene[1] == "intersect") {
    my_tab <- tk2notetab.text(notebook_intersect)
    if (my_tab == "Inclusive") {
      listboxgene <- get("listbox_intersect_inclusive")
    } else {
      listboxgene <- get("listbox_intersect_exclusive")
    }
  } else if (listboxgene[1] == "cdf") {
    my_tab <- strsplit(tk2notetab.text(notebook_cdf), split = " ")[[1]]
    listboxgene <-
      get(paste("listbox_gene_cdf_list" , my_tab[2] , sep = ""))
  }
  return(listboxgene)
}

# helper for ploting selected gene list
SelectGeneListPlotHelper <- function(listboxgene) {
  listname <-
    tolower(strsplit(tk2notetab.text(notebook_tool), split = "\n")[[1]][1])
  
  if (listname == "sort") {
    legend_nickname <- paste(listname, "tool")
  } else if (listboxgene == 'cluster') {
    legend_nickname <-
      paste(STATE[3], "tool", tk2notetab.text(get(paste0(
        "notebook_", listname
      ))), "tab")
  } else {
    legend_nickname <-
      paste(listname, "tool", tk2notetab.text(get(paste0(
        "notebook_", listname
      ))), "tab")
  }
  
  if (is.character(listboxgene)) {
    listboxgene <- ListBoxSelectHelper(listboxgene)
  }
  
  num <- as.integer(tkcurselection(listboxgene))
  sel <- NULL
  for (i in num) {
    sel <- c(sel, tclvalue(tkget(listboxgene, (i))))
  }
  MakeDataFrame(sel, nickname = legend_nickname)
}

# helper for plotting all genes in sub list
GeneListPlotHelper <- function(listboxgene) {
  listname <-
    tolower(strsplit(tk2notetab.text(notebook_tool), split = "\n")[[1]][1])
  if (listname == "sort") {
    legend_nickname <- paste(listname, "tool")
  } else if (listboxgene == 'cluster') {
    legend_nickname <-
      paste(STATE[3], "tool", tk2notetab.text(get(paste0(
        "notebook_", listname
      ))), "tab")
  } else {
    legend_nickname <-
      paste(listname, "tool", tk2notetab.text(get(paste0(
        "notebook_", listname
      ))), "tab")
  }
  
  if (is.character(listboxgene)) {
    listboxgene <- ListBoxSelectHelper(listboxgene)
  }
  if (as.integer(tksize(listboxgene)) > 0) {
    MakeDataFrame(paste(tkget(listboxgene, 0, 'end')), nickname = legend_nickname)
  }
}

# helper for making new gene list for tool tab
ToolListTabHelper <-
  function(listname = tolower(strsplit(tk2notetab.text(notebook_tool), split = "\n")[[1]][1])) {
    if (listname == "sort") {
      legend_nickname <- paste(listname, "tool")
      listboxname <- listbox_active_gene_sort
    } else {
      legend_nickname <-
        paste(listname, "tool", tk2notetab.text(get(paste0(
          "notebook_", listname
        ))), "tab")
      listboxname <- ListBoxSelectHelper(listname)
    }
    my_list <- as.character(tkget(listboxname, 0, 'end'))
    if (length(my_list) > 0) {
      LIST_DATA$gene_file$tool$full <<- my_list
      LIST_DATA$gene_file$tool$use <<- my_list
      LIST_DATA$gene_info$tool <<-
        lapply(setNames(
          names(LIST_DATA$gene_info$common),
          names(LIST_DATA$gene_info$common)
        ),
        function(i)
          c(
            kDotOptions[1],
            kLineOptions[1],
            LIST_DATA$gene_info$common[[i]][3],
            1,
            LIST_DATA$gene_info$common[[i]][5]
          ))
      
      tclvalue(tcl_toolfile) <<- legend_nickname
      tkconfigure(label_tool_file, textvariable = tcl_toolfile)
      tkconfigure(label_tool_length, text = paste("n = ", length(my_list)))
      MoveAllToOtherEntry(listbox_tool_on, listbox_tool_off, "on", file_name = "tool")
    }
  }

# clears tools
DActLst <- function(listboxfile,
                    listboxgenes,
                    listboxlengths,
                    test = 0) {
  sel <- as.integer(tclvalue(tkcurselection(listboxfile)))
  if (!is.na(sel) & test == 1) {
    for (i in sel) {
      tkdelete(listboxfile, i)
    }
  } else {
    tkdelete(listboxfile, 0, 'end')
  }
  
  if (!is.null(listboxgenes)) {
    for (i in listboxgenes) {
      tkdelete(get(i), 0, "end")
    }
  }
  if (!is.null(listboxlengths)) {
    for (i in listboxlengths) {
      tkconfigure(get(i), text = paste("n = 0"))
    }
  }
}

# clears all tools
DActAll <- function() {
  DActLst(listbox_active_sort,
          "listbox_active_gene_sort",
          "label_active_sort_length")
  
  DActLst(listbox_active_ratios,
          sapply(c("up", "down", "between"), function(x) {
            paste("listbox_gene_ratios_" , x, sep = "")
          }),
          sapply(c("up", "down", "between"), function(x) {
            paste("label_ratios_" , x, sep = "")
          }))
  
  DActLst(listbox_active_cluster,
          sapply(c(1:4), function(x) {
            paste("listbox_gene_cluster_list" , x, sep = "")
          }),
          sapply(c(1:4), function(x) {
            paste("label_cluster_list" , x, sep = "")
          }))
  
  DActLst(
    listbox_active_intersect,
    c(
      "listbox_intersect_inclusive",
      "listbox_intersect_exclusive",
      "listbox_intersect_combind"
    ),
    c(
      "label_intersect_inclusive",
      "label_intersect_exclusive",
      "label_intersect_combind"
    )
  )
  
  DActLst(listbox_active_cdf,
          sapply(c(1:4), function(x) {
            paste("listbox_gene_cdf_list" , x, sep = "")
          }),
          sapply(c(1:4), function(x) {
            paste("label_cdf_list" , x, sep = "")
          }))
  
  DActLst(listbox_active_peaks,
          sapply(c(1:4), function(x) {
            paste("listbox_gene_peaks_list" , x, sep = "")
          }),
          sapply(c(1:4), function(x) {
            paste("label_peaks_list" , x, sep = "")
          }))
}

# adds selected item(s) to active tool list
ActLst <- function(ActList,
                   MyMax,
                   listboxgenes,
                   listboxlengths) {
  if (!is.null(listboxgenes)) {
    for (i in listboxgenes) {
      tkdelete(get(i), 0, "end")
    }
  }
  if (!is.null(listboxlengths)) {
    for (i in listboxlengths) {
      tkconfigure(get(i), text = paste("n = 0"))
    }
  }
  InList <- get(paste("listbox_" , STATE[2], "_on", sep = ""))
  sel <- as.integer((tkcurselection(InList)))
  if (length(sel) == 0) {
    if (MyMax == 0) {
      sel <- c(0:(as.integer(tclvalue(
        tksize(InList)
      )) - 1))
    } else{
      sel <- c(0:(MyMax - 2))
    }
  }
  for (i in sel) {
    new_name <- paste(STATE[2], tclvalue(tkget(InList, i)), sep = '-')
    tkinsert(ActList, 0, new_name)
    if (as.integer(tksize(ActList)) == MyMax) {
      tkdelete(ActList, "end")
    }
  }
  outList <- get(paste("listbox_" , STATE[2], "_off", sep = ""))
  sel <- as.integer(tkcurselection(outList))
  for (i in sel) {
    new_name <- paste(STATE[2], tclvalue(tkget(outList, i)), sep = '-')
    tkinsert(ActList, 0, new_name)
    if (as.integer(tksize(ActList)) == MyMax) {
      tkdelete(ActList, "end")
    }
  }
  tkconfigure(ActList, listvariable = tclVar(unique(paste(
    tkget(ActList, 0, "end")
  ))))
  
}

# reads in file, tests, fills out info
LoadTableFile <- function() {
  if (is.null(names(LIST_DATA$table_file))) {
    file_count <- 0
  } else {
    file_count <- length(LIST_DATA$table_file)
    if (file_count > 12) {
      tkmessageBox(message = "I have lots of files,
                   you might want to remove some files",
                   icon = "warning")
    }
  }
  
  STATE[1] <<- 2
  full_file_name <-
    paste(as.character(
      tkgetOpenFile(multiple = TRUE, filetypes =
                      "{{Table Files} {.table .tab .Table}} {{All files} *}"))
    )
  STATE[1] <<- 0
  if (length(full_file_name) == 0) {
    ## file select test
    tkraise(root)
    return ()
  }
  pb <- tkProgressBar(title = "Loading file, please be patient!!",
                      width = 500)
  if(length(full_file_name) == 1 && length(grep(".url$",full_file_name))==1){
    full_file_name <- read_lines(full_file_name)
  }
  gene_list <- NULL
  for (x in full_file_name) {
    file_name <- paste(strsplit(as.character(x),
                                '/')[[1]][(length(strsplit(as.character(x),
                                                           '/')[[1]]))])
    legend_nickname <-
      strsplit(as.character(file_name), '.tab')[[1]][1]
    if (any(legend_nickname == names(LIST_DATA$table_file))) {
      tkmessageBox(message = "This file has already been loaded", icon = "info")
      next ()
    } else {
      num_bins <-
        count_fields(x,
                     n_max = 1,
                     skip = 1,
                     tokenizer = tokenizer_tsv()) - 1
      
      if (num_bins == 2) {
        tablefile <- suppressMessages(read_tsv(x,
                                               col_names = c("gene", "bin", "score"))) %>%
          mutate(set = gsub("(.{17})", "\\1\n", legend_nickname)) %>% na_if(Inf)
        num_bins <- collapse(summarise(tablefile, max(bin)))[[1]]
        if (file_count > 0) {
          if (summarise(LIST_DATA$table_file[[1]], n_distinct(bin)) != num_bins) {
            tkmessageBox(message = "Can't load file, different number of bins", icon = "error")
            break ()
          }
        }
      } else if (num_bins == 5) {
        tablefile <- suppressMessages(read_tsv(
          x,
          col_names = c("chr", "start", "end", "gene", "bin", "score")
        )) %>%
          select(gene, bin, score) %>%
          mutate(set = gsub("(.{17})", "\\1\n", legend_nickname)) %>% na_if(Inf)
        num_bins <- collapse(summarise(tablefile, max(bin)))[[1]]
        if (file_count > 0) {
          if (summarise(LIST_DATA$table_file[[1]], n_distinct(bin)) != num_bins) {
            tkmessageBox(message = "Can't load file, different number of bins", icon = "error")
            break ()
          }
        }
      } else{
        if (file_count > 0) {
          if (summarise(LIST_DATA$table_file[[1]], n_distinct(bin)) != num_bins) {
            tkmessageBox(message = "Can't load file, different number of bins", icon = "error")
            break ()
          }
        }
        
        tablefile <- suppressMessages(read_tsv (x,
                                                col_names = c("gene", 1:(num_bins)),
                                                skip = 1) %>% gather(., bin, score, 2:(num_bins + 1))) %>% mutate(set = gsub("(.{17})", "\\1\n", legend_nickname),
                                                                                                                  bin = as.numeric(bin)) %>% na_if(Inf)
      }
      if (file_count > 0) {
        if (!exists("gene_names")) {
          gene_names <- LIST_DATA$gene_file$common$full
        }
        zero_genes <-
          group_by(tablefile, gene) %>% summarise(test = sum(score, na.rm = T)) %>% filter(test ==
                                                                                             0)
        zero_genes <-
          c(collapse(distinct(tablefile, gene))[[1]], zero_genes$gene)
        zero_genes <- zero_genes[!duplicated(zero_genes)]
        gene_names <- c(zero_genes, gene_names)
        gene_names <- gene_names[duplicated(gene_names)]
        
        
        if (length(gene_names) > 0) {
          LIST_DATA$gene_file$common$full <<- gene_names
          DActAll()
        } else {
          close(pb)
          tkmessageBox(
            message = "Can't load file, no genes in common or
            remake your table files all the same way.",
            icon = "error"
          )
          break ()
        }
      } else {
        # first time loading a file set up
        zero_genes <-
          group_by(tablefile, gene) %>% summarise(test = sum(score, na.rm = T)) %>% filter(test ==
                                                                                             0)
        zero_genes <-
          c(collapse(distinct(tablefile, gene))[[1]], zero_genes$gene)
        gene_names <- zero_genes[!duplicated(zero_genes)]
        LIST_DATA$gene_file$common$full <<- gene_names
        LIST_DATA$gene_info$common[[legend_nickname]] <<-
          c(kDotOptions[1],
            kLineOptions[1],
            kListColorSet[1],
            1,
            legend_nickname)
        SetComboBoxes(num_bins) # fills out and sets start and stop bins
      }
      color_safe <-
        (length(LIST_DATA$table_file) + 1) %% length(kListColorSet)
      if (color_safe == 0) {
        color_safe <- 1
      }
      color_select <- kListColorSet[color_safe]
      
      # generate info for new file for $common list and each $gene file loadded
      LIST_DATA$gene_info <<-
        lapply(LIST_DATA$gene_info, function(k) {
          k[[legend_nickname]] <- c(kDotOptions[1],
                                    kLineOptions[1],
                                    color_select,
                                    1,
                                    legend_nickname)
          k
        })
      
      LIST_DATA$table_file[[legend_nickname]] <<- tablefile
    }
    setTkProgressBar(pb, legend_nickname, label = paste("loading " , legend_nickname))
    sapply(names(LIST_DATA$gene_file), function(x) {
      onlist <- get(paste("listbox_" , x, "_on", sep = ""))
      if (length(LIST_DATA$gene_file[[x]]$full) > 0) {
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
    file_count <- 1
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
  off_listbox <-
    get(paste("listbox_" , listboxname, "_off", sep = ""))
  label_file <-
    get(paste("label_" , listboxname, "_file", sep = ""))
  myTCL <- get(paste0("tcl_", listboxname, "file"))
  label_count <-
    get(paste("label_" , listboxname, "_length", sep = ""))
  
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
    legend_nickname <-
      strsplit(as.character(file_name), '.txt')[[1]][1]
    
    genefile <-
      suppressMessages(read_tsv(
        full_file_name,
        col_names = "gene",
        comment = "#",
        cols(gene = col_character())
      ))
    
    enesg <- c(unique(genefile$gene),
               LIST_DATA$gene_file$common$full)
    enesg <- enesg[duplicated(enesg)]
    if (length(enesg) == 0) {
      tcl_answer <-
        tkmessageBox(
          message = "Can't find full name matches, want to try a partial match (takes a long time)?",
          icon = "question",
          type = "yesnocancel",
          default = "no"
        )
      if (tclvalue(tcl_answer) == "no") {
        tkraise(root)
        return()
      } else {
        tkraise(root)
        pb <<-
          tkProgressBar(
            title = "be patient!! ",
            min = 0,
            max = 100,
            width = 300
          )
        enesg <-
          unique(names(unlist(
            sapply(
              LIST_DATA$gene_file$common$full,
              grep,
              genefile$gene[!duplicated(genefile$gene)]
            )
          )))
        genefile <- data.frame(gene = enesg)
        if (length(enesg) == 0) {
          tkmessageBox(message = "Still can't find any matching genes",
                       icon = "error",
                       type = "ok")
          close(pb)
        } else {
          tkmessageBox(message = "Found some matching genes, please save the file to avoid wasting time",
                       icon = "warning",
                       type = "ok")
          close(pb)
          STATE[1] <<- 2
          file_name2 <-
            tclvalue(
              tkgetSaveFile(filetypes = "{{Gene txt Files} {.txt}}", initialfile = file_name)
            )
          STATE[1] <<- 0
          if (!nchar(file_name2)) {
            ## file save test
            return()
          } else {
            write.table(
              genefile$gene,
              file_name2,
              col.names = FALSE,
              row.names = FALSE,
              quote = FALSE
            )
          }
        }
      }
    }
    LIST_DATA$gene_file[[listboxname]]$full <<-
      unique(genefile[, 1])
    LIST_DATA$gene_file[[listboxname]]$use <<- enesg
    LIST_DATA$gene_info[[listboxname]] <<-
      lapply(setNames(
        names(LIST_DATA$gene_info$common),
        names(LIST_DATA$gene_info$common)
      ),
      function(i)
        c(
          kDotOptions[1],
          kLineOptions[1],
          LIST_DATA$gene_info$common[[i]][3],
          1,
          LIST_DATA$gene_info$common[[i]][5]
        ))
    tclvalue(myTCL) <- legend_nickname
    tkconfigure(label_file, textvariable = myTCL)
    tkconfigure(label_count, text = paste("n = ", length(enesg)))
    MoveAllToOtherEntry(on_listbox, off_listbox, "on", listboxname)
    tkraise(root)
  }
}

# reads in gene list for intersect
IntersectLoadFile <- function() {
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
    legend_nickname <-
      strsplit(as.character(file_name), '.txt')[[1]][1]
    
    genefile <- read.table(full_file_name,
                           stringsAsFactors = FALSE,
                           header = FALSE)
    genefile2 <- c(genefile[, 1], LIST_DATA$gene_file$common$use)
    genefile2 <- unique(genefile2[duplicated(genefile2)])
    tkraise(root)
    if (length(genefile2) > 0) {
      return(IntersectGeneLists(genefile2, legend_nickname))
    } else{
      tkmessageBox(message = "Can't use this gene list", icon = "error")
    }
  }
}

# saves gene list to a file
SaveGenelist <-
  function(listboxgene, activelistbox, toolinfo = " ") {
    if (is.character(listboxgene)) {
      listboxgene <- ListBoxSelectHelper(listboxgene)
    }
    # make toolinfo
    if (toolinfo == "sort") {
      R_start_bin <- as.integer(tclvalue(tcl_bin_start_sort))
      R_end_bin <- as.integer(tclvalue(tcl_bin_end_sort))
      R_num <- as.integer(tclvalue(tcl_top_bottom_num))
      R_option <- tclvalue(tcl_top_bottom_option)
      R_order <- tclvalue(tcl_acc_dec)
      if (!suppressWarnings(is.na(as.numeric(tclvalue(
        tkget(entry_dist_span)
      ))))) {
        R_dist_span <-
          paste("seprated by more than", as.numeric(tclvalue(tkget(
            entry_dist_span
          ))), "bp")
      } else{
        R_dist_span <- ""
      }
      toolinfo <-
        paste0(
          "sort tool: ",
          R_option,
          R_num,
          ": bins ",
          R_start_bin,
          " to ",
          R_end_bin,
          " ",
          R_order,
          " ",
          R_dist_span
        )
      tabinfo <-
        paste("n = ", as.integer(tksize(listbox_active_gene_sort)))
    } else if (toolinfo == "peaks") {
      R_start1_bin <- as.integer(tclvalue(tcl_bin_start_peaks))
      R_end1_bin <- as.integer(tclvalue(tcl_bin_end_peaks))
      my_tab <-
        strsplit(x = tk2notetab.text(notebook_peaks), split = " ")[[1]][2]
      toolinfo <-
        paste0("peak tool: ", ": bins ", R_start1_bin, ":", R_end1_bin)
      tabinfo <- tclvalue(get(paste0("tcl_peaks", my_tab, "file")))
    } else if (toolinfo == "ratios") {
      R_start1_bin <- as.integer(tclvalue(tcl_bin_start1_ratios))
      R_end1_bin <- as.integer(tclvalue(tcl_bin_end1_ratios))
      R_start2_bin <- as.integer(tclvalue(tcl_bin_start2_ratios))
      R_end2_bin <- as.integer(tclvalue(tcl_bin_end2_ratios))
      R_num <- as.numeric(tclvalue(tcl_bin_fold_ratios))
      my_tab <-
        strsplit(x = tk2notetab.text(notebook_ratios), split = " ")[[1]][2]
      if (my_tab == 1 | my_tab == 2) {
        my_file <-
          paste(R_num, "fold up in", as.character(tkget(
            activelistbox, as.numeric(my_tab) - 1
          )))
      } else {
        my_file <- paste(my_tab, "fold change", R_num)
      }
      toolinfo <-
        paste0(
          "ratios tool: ",
          my_file,
          "  : bins ",
          R_start1_bin,
          ":",
          R_end1_bin,
          "/",
          R_start2_bin,
          ":",
          R_end2_bin
        )
      tabinfo <- " "
    } else if (toolinfo == "cluster") {
      R_start_bin <- as.integer(tclvalue(tcl_bin_start_cluster))
      R_stop_bin <- as.integer(tclvalue(tcl_bin_end_cluster))
      R_cluster <- as.integer(tclvalue(tcl_bin_cluster_num))
      my_tab <-
        strsplit(x = tk2notetab.text(notebook_cluster), split = " ")[[1]][2]
      toolinfo <-
        paste(
          "cluster tool: ",
          R_cluster,
          "clusters, cluster #",
          my_tab,
          ": bins",
          R_start_bin,
          "to",
          R_stop_bin
        )
      tabinfo <- tclvalue(get(paste0("tcl_list", my_tab, "file")))
    } else if (toolinfo == "quantile") {
      R_start_bin <- as.integer(tclvalue(tcl_bin_start_cluster))
      R_stop_bin <- as.integer(tclvalue(tcl_bin_end_cluster))
      my_tab <- tk2notetab.text(notebook_cluster)
      toolinfo <-
        paste("quantile tool:",
              my_tab,
              ", bins",
              R_start_bin,
              "to",
              R_stop_bin)
      my_tab2 <-
        strsplit(x = tk2notetab.text(notebook_cluster), split = " ")[[1]][2]
      tabinfo <- tclvalue(get(paste0("tcl_listq", my_tab2, "file")))
    } else if (toolinfo == "intersect") {
      my_tab <- tk2notetab.text(notebook_intersect)
      toolinfo <- paste("intersect tool:", my_tab)
      tabinfo <- " "
    } else if (toolinfo == "cdf") {
      R_start1_bin <- as.integer(tclvalue(tcl_bin_start1_cdf))
      R_end1_bin <- as.integer(tclvalue(tcl_bin_end1_cdf))
      R_start2_bin <- as.integer(tclvalue(tcl_bin_start2_cdf))
      R_end2_bin <- as.integer(tclvalue(tcl_bin_end2_cdf))
      R_order <- tclvalue(tcl_acc_dec_cdf)
      toolinfo <-
        paste0(
          "cdf tool: ",
          R_order,
          ": bins ",
          R_start1_bin,
          ":",
          R_end1_bin,
          "/",
          R_start2_bin,
          ":",
          R_end2_bin
        )
      tabinfo <- " "
    }
    
    OnUpdate <- function() {
      new_comments <- tclvalue(tkget(NickName, "0.0", "end"))
      if (nchar(new_comments) < 1) {
        new_comments <- paste("#", Sys.Date())
      }
      tkdestroy(CommentFrame)
      STATE[1] <<- 2
      file_name <-
        tclvalue(tkgetSaveFile(filetypes = "{{Gene txt Files} {.txt}}", initialfile = toolinfo))
      STATE[1] <<- 0
      if (!nchar(file_name)) {
        ## file save test
        return()
      } else{
        if (tclvalue(tcl_checkbox_split) == 1) {
          tt <-
            c(new_comments, gsub(";|\\+;|\\-;|\\|", "\t", paste(tkget(
              listboxgene, 0, 'end'
            ))))
        } else{
          tt <- c(new_comments, paste(tkget(listboxgene, 0, 'end')))
        }
        write.table(
          tt,
          file_name,
          col.names = FALSE,
          row.names = FALSE,
          quote = FALSE
        )
      }
    }
    
    if (as.integer(tksize(listboxgene)) > 0) {
      CommentFrame <<- tktoplevel()
      STATE[1] <<- 3
      tkbind(CommentFrame, "<Destroy>", function()
        OnDestroy())
      tkgrid(tklabel(CommentFrame, text = "Add or edit comments"),
             columnspan = 2)
      
      tkgrid(
        NickName <<-
          tk2text(CommentFrame, width = kWidth + 40, height = kHeight + 30),
        column = 1,
        row = 2,
        pady = c(10, 10)
      )
      tkgrid(tk2button(
        CommentFrame,
        text = "Save",
        command = function()
          OnUpdate()
      ))
      tkinsert(NickName, "end", paste("#", Sys.Date(), "\n"))
      tkinsert(NickName, "end", paste("#", version_num, "\n"))
      tkinsert(NickName, "end", paste("#", toolinfo, "\n"))
      tkinsert(NickName, "end", paste("#", tabinfo, "\n"))
      tkinsert(NickName,
               "end",
               paste("# List(s) and file(s) used in tool: \n"))
      if (tk2notetab.text(notebook_tool) != "Intersect\nTool") {
        nick_name <- as.character(tkget(activelistbox, 0, 'end'))
        for (i in nick_name) {
          use_nickname <- strsplit(sub("-", " ", i), " ")[[1]]
          myTCL <-
            tclvalue(get(paste0("tcl_", use_nickname[1], "file")))
          tkinsert(NickName,
                   "end",
                   paste("#", myTCL, "-", use_nickname[2], "\n"))
        }
      } else {
        nick_name <- as.character(tkget(activelistbox, 0, 'end'))
        tkinsert(NickName, "end", paste("\n### Gene lists used ### \n"))
        for (i in nick_name) {
          tkinsert(NickName, "end", paste("#   ", i, "\n"))
        }
        tkinsert(NickName, "end", paste("\n### List of files loaded ### \n"))
        nick_name <- names(LIST_DATA$table_file)
        for (i in nick_name) {
          tkinsert(NickName, "end", paste("#   ", i, "\n"))
        }
      }
      tkinsert(NickName,
               "end",
               paste("\n####  Please start all comment lines with '#'  ####"))
      if (tclvalue(tcl_checkbox_comment) == 1) {
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
             function(k)
               sapply(names(LIST_DATA$gene_info[[k]]), function(ref)
                 if (LIST_DATA$gene_info[[k]][[ref]][5] == nickname) {
                   LIST_DATA$gene_info[[k]][[ref]] <<- NULL
                 }))
      kListColorSet <<-
        c(kListColorSet[seq_along(kListColorSet) != i + 1],
          kListColorSet[seq_along(kListColorSet) == i + 1])
    }
    for (i in as.integer(tkcurselection(listbox_common_off))) {
      filename <- tclvalue(tkget(listbox_common_off, i))
      nickname <- LIST_DATA$gene_info$common[[filename]][5]
      LIST_DATA$table_file[[nickname]] <<- NULL
      sapply(names(LIST_DATA$gene_info),
             function(k)
               sapply(names(LIST_DATA$gene_info[[k]]), function(ref)
                 if (LIST_DATA$gene_info[[k]][[ref]][5] == nickname) {
                   LIST_DATA$gene_info[[k]][[ref]] <<- NULL
                 }))
      kListColorSet <<-
        c(kListColorSet[seq_along(kListColorSet) != i + 1],
          kListColorSet[seq_along(kListColorSet) == i + 1])
    }
    if (length(names(LIST_DATA$table_file)) > 1) {
      gene_names <-
        collapse(distinct(LIST_DATA$table_file[[1]], gene))[[1]]
      for (i in LIST_DATA$table_file) {
        gene_names <- c(gene_names, collapse(distinct(i, gene))[[1]])
        gene_names <- gene_names[duplicated(gene_names)]
      }
    } else if (length(names(LIST_DATA$table_file)) == 1) {
      gene_names <-
        collapse(distinct(LIST_DATA$table_file[[1]], gene))[[1]]
    } else{
      ClearTableFile()
      return()
    }
    LIST_DATA$gene_file$common$full <<- gene_names
    LIST_DATA$gene_file$common$use <<- gene_names
    sapply(names(LIST_DATA$gene_file), function(x) {
      onlist <- get(paste("listbox_" , x, "_on", sep = ""))
      offlist <- get(paste("listbox_" , x, "_off", sep = ""))
      if (length(LIST_DATA$gene_file[[x]]$full) > 0) {
        gene_names2 <- c(gene_names, LIST_DATA$gene_file[[x]]$full)
        LIST_DATA$gene_file[[x]]$use <<-
          gene_names2[duplicated(gene_names2)]
        tkdelete(onlist, 0, 'end')
        tkdelete(offlist, 0, 'end')
        lapply(names(LIST_DATA$gene_info[[x]]), function(j) {
          if (LIST_DATA$gene_info[[x]][[j]][4] == 1) {
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
SetComboBoxes <- function(num_bins) {
  num <- tclvalue(tkget(combobox_plot_lines))
  tkconfigure(combobox_norm_bin, values = c(0, 1:num_bins))
  tkconfigure(combobox_bin_start, values = c(1:num_bins))
  tkconfigure(combobox_bin_end, values = c(1:num_bins))
  tkconfigure(combobox_bin_start_peaks, values = c(1:num_bins))
  tkconfigure(combobox_bin_width_peaks, values = c(seq(1, num_bins, 2)))
  tkconfigure(combobox_bin_end_peaks, values = c(1:num_bins))
  tkset(combobox_bin_end, num_bins)
  tkset(combobox_bin_end_peaks, num_bins)
  tkconfigure(combobox_bin_start1_ratios, values = c(1:num_bins))
  tkconfigure(combobox_bin_end1_ratios, values = c(1:num_bins))
  tkset(combobox_bin_start1_ratios, min(floor(list_plot_lines[[num]][3]) /
                                          2, num_bins))
  tkset(combobox_bin_end1_ratios, min(floor(list_plot_lines[[num]][3]), num_bins))
  tkconfigure(combobox_bin_start2_ratios, values = c(0, 1:num_bins))
  tkconfigure(combobox_bin_end2_ratios, values = c(0, 1:num_bins))
  tkset(combobox_bin_start2_ratios, min(floor(list_plot_lines[[num]][3]) + 1, num_bins))
  tkset(combobox_bin_end2_ratios, min(floor(list_plot_lines[[num]][4]), num_bins))
  tkconfigure(combobox_bin_start1_cdf, values = c(1:num_bins))
  tkconfigure(combobox_bin_end1_cdf, values = c(1:num_bins))
  tkset(combobox_bin_start1_cdf, min(floor(list_plot_lines[[num]][3]) /
                                       2, num_bins))
  tkset(combobox_bin_end1_cdf, min(floor(list_plot_lines[[num]][3]), num_bins))
  tkconfigure(combobox_bin_start2_cdf, values = c(1:num_bins))
  tkconfigure(combobox_bin_end2_cdf, values = c(1:num_bins))
  tkset(combobox_bin_start2_cdf, min(floor(list_plot_lines[[num]][3]) +
                                       1, num_bins))
  tkset(combobox_bin_end2_cdf, min(floor(list_plot_lines[[num]][4]), num_bins))
  tkconfigure(combobox_bin_start_cluster, values = c(1:num_bins))
  tkconfigure(combobox_bin_end_cluster, values = c(1:num_bins))
  tkset(combobox_bin_end_cluster, num_bins)
  tkconfigure(combobox_bin_start_sort, values = c(1:num_bins))
  tkconfigure(combobox_bin_end_sort, values = c(1:num_bins))
  tkset(combobox_bin_end_sort, num_bins)
  tkconfigure(combobox_bin_cluster_num, values = c(2:4))
}

# global options box
GlobalOptions <- function() {
  OptionsFrame <<- tktoplevel()
  STATE[1] <<- 4
  tkbind(OptionsFrame, "<Destroy>", function()
    OnDestroy())
  
  onOK <- function() {
    tkdestroy(OptionsFrame)
    STATE[1] <<- 0
  }
  
  tkgrid(tklabel(OptionsFrame, text = "Edit options"),
         columnspan = 4)
  
  tkgrid(
    tkcheckbutton(OptionsFrame, variable = tcl_checkbox_comment, text = "Don't Show save comment box?"),
    columnspan = 4
  )
  
  tkgrid(
    tkcheckbutton(OptionsFrame, variable = tcl_checkbox_split, text = "Split saved gene list?"),
    columnspan = 4
  )
  
  tkgrid(
    tklabel(OptionsFrame, text = "TSS TTS line type"),
    column = 0,
    row = 6,
    pady = c(15, 5)
  )
  
  combobox_tss_tts_lines <- tk2combobox(
    OptionsFrame,
    value = kLineType,
    textvariable = tcl_tss_tts_line_type,
    state = "readonly",
    width = 10
  )
  tkgrid(
    combobox_tss_tts_lines,
    column = 1,
    row = 6,
    padx = c(0, 16),
    pady = c(15, 5)
  )
  
  tkgrid(
    tklabel(OptionsFrame, text = "transition line type"),
    column = 0,
    row = 7,
    pady = c(5, 15)
  )
  
  combobox_body_lines <- tk2combobox(
    OptionsFrame,
    value = kLineType,
    textvariable = tcl_body_line_type,
    state = "readonly",
    width = 10
  )
  tkgrid(
    combobox_body_lines,
    column = 1,
    row = 7,
    padx = c(0, 16),
    pady = c(5, 15)
  )
  
  tkgrid(tk2button(
    OptionsFrame,
    text = "OK",
    command = function()
      onOK()
  ),
  columnspan = 6)
  
}

# change colors to diffrnet color brewer sets
BrewerSet <- function() {
  color_file <- brewer.pal(8, as.character(tclvalue(tcl_brewer)))
  lapply(names(LIST_DATA$gene_info), function(i) {
    onlist <- get(paste("listbox_" , i, "_on", sep = ""))
    offlist <- get(paste("listbox_" , i, "_off", sep = ""))
    tkdelete(onlist, 0, 'end')
    tkdelete(offlist, 0, 'end')
    lapply(seq_along(LIST_DATA$gene_info[[i]]), function(j) {
      color_safe <- j %% length(color_file)
      if (color_safe == 0) {
        color_safe <- 1
      }
      LIST_DATA$gene_info[[i]][[j]][3] <<- color_file[color_safe]
      if (LIST_DATA$gene_info[[i]][[j]][4] == 1) {
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
  if (!nchar(full_file_name)) {
    ## file select test
    color_file <- brewer.pal(8, as.character(tclvalue(tcl_brewer)))
  } else {
    color_file <- read.table(full_file_name,
                             header = FALSE,
                             stringsAsFactors = FALSE)
    # checks if in rgb format and converts to hex and if is a color
    sapply(seq_along(color_file[[1]]), function(i) {
      if (suppressWarnings(!is.na(as.numeric(substr(
        color_file[[1]][i], 1, 1
      )))) == TRUE) {
        red_green_blue <- strsplit(color_file[[1]][i], ",")
        if (length(red_green_blue[[1]]) == 3) {
          color_file[[1]][i] <<- rgb(
            as.numeric(red_green_blue[[1]])[1],
            as.numeric(red_green_blue[[1]])[2],
            as.numeric(red_green_blue[[1]])[3],
            maxColorValue = 255
          )
        } else {
          color_file[[1]][i] <<- "black"
        }
      }
      if (!isColor(color_file[[1]][i])) {
        color_file[[1]][i] <<- "black"
      }
    })
  }
  color_file <- unlist(color_file)
  if (length(color_file) > 0) {
    color_file <- c(color_file, "black")
    kListColorSet <<-  color_file
    print(list("I now have these as default colors" = as.character(color_file)))
    lapply(names(LIST_DATA$gene_info), function(i) {
      onlist <- get(paste("listbox_" , i, "_on", sep = ""))
      offlist <- get(paste("listbox_" , i, "_off", sep = ""))
      tkdelete(onlist, 0, 'end')
      tkdelete(offlist, 0, 'end')
      lapply(seq_along(LIST_DATA$gene_info[[i]]), function(j) {
        color_safe <- j %% length(color_file)
        if (color_safe == 0) {
          color_safe <- 1
        }
        LIST_DATA$gene_info[[i]][[j]][3] <<- color_file[color_safe]
        if (LIST_DATA$gene_info[[i]][[j]][4] == 1) {
          tkinsert(onlist, 'end', names(LIST_DATA$gene_info[[i]][j]))
          tkitemconfigure(onlist, "end", foreground = LIST_DATA$gene_info[[i]][[j]][3])
        } else {
          tkinsert(offlist, 'end', names(LIST_DATA$gene_info[[i]][j]))
          tkitemconfigure(offlist, "end", foreground = LIST_DATA$gene_info[[i]][[j]][3])
        }
      })
    })
  } else {
    tkmessageBox(message = "could not make any colors from file", icon = "error")
    
  }
  
  tkraise(root)
}

# make normalized file ... devide one by the other
MakeNormFile <- function() {
  nom <- tclvalue(tkget(combobox_numerator))
  dnom <- tclvalue(tkget(combobox_denominator))
  if (nom != "numerator" && dnom != "denominator") {
    pb <<-
      tkProgressBar(
        title = "be patient!! ",
        min = 0,
        max = 100,
        width = 300
      )
    mynom <- LIST_DATA$gene_info$common[[nom]][5]
    mydnom <- LIST_DATA$gene_info$common[[dnom]][5]
    
    new_gene_list <-
      inner_join(LIST_DATA$table_file[[mynom]],
                 LIST_DATA$table_file[[mydnom]],
                 by = c("gene", "bin")) %>%
      na_if(0)
    # find min value /2 to replace 0s
    setTkProgressBar(pb,
                     50,
                     label = paste(round(25, 0),
                                   "finding and replacing 0's and NA's with min/2"))
    new_min_for_na <-
      min(c(new_gene_list$score.x, new_gene_list$score.y), na.rm = TRUE) / 2
    # replace 0's with min/2
    new_gene_list <-
      replace_na(new_gene_list,
                 list(score.x = new_min_for_na,
                      score.y = new_min_for_na))
    setTkProgressBar(pb, 50, label = paste(round(50, 0),
                                           "deviding one by other"))
    LIST_DATA$table_file[[paste(mynom, mydnom, sep = "/")]] <<-
      transmute(
        new_gene_list,
        gene = gene,
        bin = bin,
        set = paste0(
          gsub("(.{17})", "\\1\n", set.x),
          "/\n",
          gsub("(.{17})", "\\1\n", set.y)
        ),
        score = score.x / score.y
      )
    setTkProgressBar(pb, 50, label = paste(round(75, 0), "Building up info"))
    file_name <- paste(nom, dnom, sep = "/")
    
    file_count <- length(LIST_DATA$table_file)
    
    color_safe <- file_count %% length(kListColorSet)
    if (color_safe == 0) {
      color_safe <- 1
    }
    for (p in names(LIST_DATA$gene_info)) {
      LIST_DATA$gene_info[[p]][[file_name]] <<-
        c(kDotOptions[1],
          kLineOptions[1],
          kListColorSet[color_safe],
          1,
          paste(mynom, mydnom, sep = "/"))
    }
    
    sapply(names(LIST_DATA$gene_file), function(x) {
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
    close(pb)
  }
}

# Makes data frame and gathers plot settings for plotting active samples
MakeDataFrame <-
  function(sel_list = NULL,
           table_file = LIST_DATA$table_file,
           gene_file = LIST_DATA$gene_file,
           gene_info = LIST_DATA$gene_info,
           nickname = NULL) {
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
      list_data_frame <- NULL
      pb <<-
        tkProgressBar(
          title = "be patient!! ",
          min = 0,
          max = 100,
          width = 300
        )
      for (i in names(gene_file)) {
        # checks to see if at least one file in list is acitve
        if (sum(as.numeric(sapply(gene_info[[i]], "[[", 4))) == 0) {
          next ()
        } else {
          if (!is.null(sel_list)) {
            enesg <- c(sel_list, gene_file[[i]]$use)
            enesg <- data_frame(gene = enesg[duplicated(enesg)])
            myTCL <- tclvalue(get(paste0("tcl_", i, "file")))
            use_x_label <- paste(use_x_label, paste(myTCL, "n = ",
                                                    length(enesg[[1]])), sep = '  ')
            if (length(enesg[[1]]) == 0) {
              break()
            }
          } else {
            enesg <- data_frame(gene = gene_file[[i]]$use)
            myTCL <- tclvalue(get(paste0("tcl_", i, "file")))
            use_x_label <- paste(use_x_label, paste(myTCL, "n = ",
                                                    length(enesg[[1]])), sep = '  ')
          }
          tf <-
            c((as.numeric(sapply(
              gene_info[[i]], "[[", 4
            ))) == 1)
          list_data_frame[[i]] <-
            bind_rows(table_file[tf]) %>%
            semi_join(., enesg, by = "gene") %>%
            mutate(., set = paste(gsub("(.{17})", "\\1\n", myTCL), set, sep = '-\n'))
          
          dots <-
            match(sapply(gene_info[[i]][tf], "[[", 1), kDotOptions)
          use_dot <- c(use_dot,
                       if_else(dots == 1, 0, dots + 13))
          use_size <- c(use_size, if_else(use_dot == 0, 0.01, 4.5))
          my_lines <-
            match(sapply(gene_info[[i]][tf], "[[", 2), kLineOptions)
          use_line <- c(use_line,
                        if_else(my_lines > 6, 0, as.double(my_lines)))
          use_col <- c(use_col, sapply(gene_info[[i]][tf], "[[", 3))
          current_nickname <- select(list_data_frame[[i]], set) %>%
            distinct()
          use_nickname <-
            c(use_nickname, current_nickname$set)
          legend_space <-
            max(legend_space, (lengths(
              strsplit(current_nickname$set, "\n")
            ) - 0.75))
        }
      }
      if (!is.null(nickname)) {
        use_x_label <- paste(nickname, use_x_label)
      }
      setTkProgressBar(pb, 50, label = paste(round(50, 0), "Gathered Data"))
      if (!is.null(names(list_data_frame))) {
        ApplyMath(
          list_data_frame,
          use_col,
          use_dot,
          use_line,
          use_size,
          use_nickname,
          use_x_label,
          legend_space
        )
      } else{
        close(pb)
      }
    }
  }

# Applys math to long list
ApplyMath <-
  function(list_data_frame,
           use_col,
           use_dot,
           use_line,
           use_size,
           use_nickname,
           use_x_label,
           legend_space) {
    # math set and y label
    use_math <- as.character(tclvalue(tcl_math_option))
    use_y_label <- paste(use_math, "of bin counts")
    # set normilization to relative frequency or bin number or 1 for none,
    # and update y label
    r_checkbox_relative_frequency <-
      as.character(tclvalue(tcl_checkbox_relative_frequency))
    r_checkbox_gene_relative_frequency <-
      as.character(tclvalue(tcl_checkbox_relative_gene_frequency))
    norm_bin <- as.numeric(tclvalue(tcl_norm_bin))
    if (r_checkbox_gene_relative_frequency == 1) {
      if (r_checkbox_relative_frequency == 1) {
        tktoggle(checkbox_relative_frequency)
      }
      list_long_data_frame <- bind_rows(list_data_frame) %>%
        group_by(set, gene) %>%
        mutate(score = score / sum(score, na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(set, bin) %>%
        summarise(value = get(as.character(tclvalue(
          tcl_math_option
        )))(score, na.rm = T)) %>%
        ungroup()
      
      use_y_label <- paste("RF per gene :", use_y_label)
    } else {
      list_long_data_frame <- bind_rows(list_data_frame) %>%
        group_by(set, bin) %>%
        summarise(value = get(as.character(tclvalue(
          tcl_math_option
        )))(score, na.rm = T)) %>%
        ungroup()
    }
    
    if (r_checkbox_relative_frequency == 1 && norm_bin == 0) {
      use_y_label <- paste(strsplit(use_y_label, split = " ")[[1]][1],
                           "bins : RF")
      list_long_data_frame <-
        group_by(list_long_data_frame, set) %>%
        mutate(value = value / sum(value)) %>%
        ungroup()
    } else if (norm_bin > 0) {
      if (r_checkbox_relative_frequency == 1) {
        tktoggle(checkbox_relative_frequency)
      }
      if (r_checkbox_gene_relative_frequency == 1) {
        use_y_label <- paste(use_y_label, " : Norm bin ", norm_bin)
      } else {
        use_y_label <- paste(strsplit(use_y_label, split = " ")[[1]][1],
                             "bins : Normalize to bin ",
                             norm_bin)
      }
      list_long_data_frame <-
        group_by(list_long_data_frame, set) %>%
        mutate(value = value / nth(value, norm_bin)) %>%
        ungroup()
    }
    
    use_plot_limits <-
      c(as.integer(tclvalue(tcl_bin_start)), as.integer(tclvalue(tcl_bin_end)))
    
    # update y label if log2
    if (tclvalue(tcl_checkbox_log2) == 1) {
      use_y_label <- paste("log2(", use_y_label, ")", sep = "")
      if (summarise(list_long_data_frame, sum(value, na.rm = TRUE)) != 0) {
        list_long_data_frame$value[list_long_data_frame$value == 0] <-
          min(list_long_data_frame$value[list_long_data_frame$value > 0]) / 2
      } else {
        list_long_data_frame$value <- 1
      }
      use_y_limits <- group_by(list_long_data_frame, set) %>%
        filter(bin %in% use_plot_limits[1]:use_plot_limits[2]) %>%
        ungroup() %>%
        summarise(min(log2(value), na.rm = T), max(log2(value), na.rm =
                                                     T))
    } else {
      use_y_limits <- group_by(list_long_data_frame, set) %>%
        filter(bin %in% use_plot_limits[1]:use_plot_limits[2]) %>%
        ungroup() %>%
        summarise(min(value, na.rm = T), max(value, na.rm = T))
    }
    if (tclvalue(tcl_checkbox_Y_axis) == 1 &
        !suppressWarnings(is.na(as.numeric(tclvalue(tcl_Y_axis_min)))) &
        !suppressWarnings(is.na(as.numeric(tclvalue(tcl_Y_axis_max))))) {
      use_y_limits <-
        c(as.numeric(tclvalue(tcl_Y_axis_min)), as.numeric(tclvalue(tcl_Y_axis_max)))
    } else {
      tkdelete(entry_Y_axis_min, 0, "end")
      tkdelete(entry_Y_axis_max, 0, "end")
      tkinsert(entry_Y_axis_min, 0, round(unlist(use_y_limits)[1], digits = 3))
      tkinsert(entry_Y_axis_max, 0,  round(unlist(use_y_limits)[2], digits = 3))
    }
    use_pos_plot_ticks <- Destring(unlist(strsplit(tclvalue(
      tcl_pos_plot_ticks
    ), " ")))
    use_label_plot_ticks <-
      unlist(strsplit(tclvalue(tcl_label_plot_ticks),
                      " "))
    if (length(use_label_plot_ticks) != length(use_pos_plot_ticks)) {
      tkmessageBox(message = "The number of Positions and labels are unequal", icon = "error")
      return ()
    }
    
    use_plot_breaks <- c(
      Destring(tclvalue(tcl_pos_one_line)),
      Destring(tclvalue(tcl_pos_two_line)),
      Destring(tclvalue(tcl_pos_three_line)),
      Destring(tclvalue(tcl_pos_four_line)),
      use_pos_plot_ticks
    )
    use_plot_breaks[use_plot_breaks == 0] <- NA
    
    use_plot_breaks_labels <- c(
      tclvalue(tcl_one_tss_tts_option),
      tclvalue(tcl_two_tss_tts_option),
      tclvalue(tcl_three_tss_tts_option),
      tclvalue(tcl_four_tss_tts_option),
      use_label_plot_ticks
    )
    use_virtical_line_type <-
      c(
        as.character(tclvalue(tcl_tss_tts_line_type)),
        as.character(tclvalue(tcl_tss_tts_line_type)),
        as.character(tclvalue(tcl_body_line_type)),
        as.character(tclvalue(tcl_body_line_type))
      )
    use_virtical_line_color <- c("green", "red", "black", "black")
    use_plot_breaks_labels <-
      use_plot_breaks_labels[!is.na(use_plot_breaks)]
    use_virtical_line_type <-
      use_virtical_line_type[!is.na(use_plot_breaks[1:4])]
    use_virtical_line_color <-
      use_virtical_line_color[!is.na(use_plot_breaks[1:4])]
    use_virtical_line <-
      use_plot_breaks[1:4][!is.na(use_plot_breaks[1:4])]
    use_plot_breaks <- use_plot_breaks[!is.na(use_plot_breaks)]
    
    # TODO need controls?
    virtical_line_data_frame <- data.frame(
      use_virtical_line,
      use_virtical_line_type,
      use_virtical_line_color,
      stringsAsFactors = FALSE
    )
    names(use_col) <- use_nickname
    names(use_dot) <- use_nickname
    names(use_line) <- use_nickname
    (tclvalue(tcl_smooth) == 1){
      use_y_label <- paste0("smoothed(", use_y_label, ")")
    }
    setTkProgressBar(pb, 75, label = paste(round(75, 0), "Applied Math"))
    GGplotF(
      list_long_data_frame,
      use_col,
      use_dot,
      use_line,
      use_size,
      use_y_label,
      use_x_label,
      use_plot_breaks,
      virtical_line_data_frame,
      use_plot_breaks_labels,
      use_plot_limits,
      use_y_limits,
      legend_space
    )
  }

# main ggplot function
GGplotF <-
  function(list_long_data_frame,
           use_col,
           use_dot,
           use_line,
           use_size,
           use_y_label,
           use_x_label,
           use_plot_breaks,
           virtical_line_data_frame,
           use_plot_breaks_labels,
           use_plot_limits,
           use_y_limits,
           legend_space) {
    if (tclvalue(tcl_checkbox_log2) == 1) {
      gp <- ggplot(
        list_long_data_frame,
        aes(
          x = as.numeric(bin),
          y = log2(value),
          color = set,
          shape = set,
          linetype = set,
          size = set
        )
      )
    } else {
      gp <-
        ggplot(
          list_long_data_frame,
          aes(
            x = as.numeric(bin),
            y = value,
            color = set,
            shape = set,
            linetype = set,
            size = set
          )
        )
    }
    if (tclvalue(tcl_smooth) == 1) {
      gp <- gp +
        geom_smooth(se = FALSE,
                    size = 2.5,
                    span = .2)
    } else{
      gp <- gp +
        geom_line(size = 2.5)
    }
    gp <- gp +
      geom_point(stroke = .001) +
      scale_size_manual(name = "Sample", values = use_size) +
      scale_color_manual(name = "Sample", values = use_col) +
      scale_shape_manual(name = "Sample", values = use_dot) +
      scale_linetype_manual(name = "Sample", values = use_line) +
      xlab(use_x_label) + ylab(use_y_label) +  # Set axis labels
      scale_x_continuous(breaks = use_plot_breaks,
                         labels = use_plot_breaks_labels) +
      
      geom_vline(
        data = virtical_line_data_frame,
        aes(xintercept = use_virtical_line),
        size = 2,
        linetype = virtical_line_data_frame$use_virtical_line_type,
        color = virtical_line_data_frame$use_virtical_line_color
      ) +
      theme_bw() +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_blank()) +
      theme(axis.title.y = element_text(size =  15)) +
      theme(axis.title.x = element_text(size =  10, vjust = .5)) +
      theme(axis.text.x = element_text(
        size = 12,
        angle = -45,
        hjust = .1,
        vjust = .9,
        face = 'bold'
      )) +
      theme(
        legend.title = element_blank(),
        legend.key = element_rect(size = 5, color = 'white'),
        legend.key.height = unit(legend_space, "line"),
        legend.text = element_text(size = 9)
      ) +
      coord_cartesian(xlim = use_plot_limits, ylim = unlist(use_y_limits))
    setTkProgressBar(pb, 99, label = paste(round(100, 0), "Ready to plot"))
    suppressMessages(print(gp))
    close(pb)
  }

# plot cumlitave frequency
GGplotC <-
  function(df2,
           my_xlab,
           use_col,
           use_header,
           legend_space) {
    gp <- ggplot(df2, aes(log2(value), color = set)) +
      stat_ecdf(show.legend = TRUE, size = 1.8) +
      scale_color_manual(name = "Sample", values = use_col) +
      xlab(paste(my_xlab, collapse = ",  ")) +
      ylab("Fraction of genes") +
      ggtitle(use_header) +
      theme_bw() +
      theme(legend.title = element_blank()) +
      theme(axis.title.y = element_text(size =  15)) +
      theme(axis.title.x = element_text(size =  10, vjust = .5)) +
      theme(axis.text.x = element_text(
        size = 12,
        angle = -45,
        hjust = .1,
        vjust = .9,
        face = 'bold'
      )) +
      theme(
        legend.title = element_blank(),
        legend.key = element_rect(size = 5, color = 'white'),
        legend.key.height = unit(legend_space, "line"),
        legend.text = element_text(size = 9)
      ) # + coord_cartesian(xlim = c(-4,6))
    print(gp)
    
  }

# finds genes in list and moves them to top of list and hightlights them
FindGene <- function(my_word_box, gene_list_box) {
  my_word <- as.character(tkget(my_word_box))
  if (is.character(gene_list_box)) {
    gene_list_box <- ListBoxSelectHelper(gene_list_box)
  }
  if (as.numeric(tksize(gene_list_box)) > 0) {
    for (i in 1:as.numeric(tksize(gene_list_box))) {
      tkitemconfigure(gene_list_box, i - 1, foreground = "black")
    }
  } else {
    return()
  }
  if (length(my_word) == 1) {
    my_gene <- NULL
    nums <-
      grep(my_word, tkget(gene_list_box, 0, 'end'), ignore.case = T)
    for (i in rev(nums)) {
      my_gene <- c(tkget(gene_list_box, i - 1), my_gene)
      tkdelete(gene_list_box, i - 1)
    }
    for (i in my_gene) {
      tkinsert(gene_list_box, 0, i)
      tkitemconfigure(gene_list_box, 0, foreground = "red")
    }
    if (length(my_gene) == 0) {
      tkdelete(my_word_box, 0, 'end')
      tkinsert(my_word_box, 0, "Not_found")
    }
  } else if (length(my_word) > 1) {
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
  pb <<-
    tkProgressBar(title = "Sorting gene lists",
                  width = 300)
  setTkProgressBar(pb, 300, label = "Sorting gene lists")
  # for item in active list make sorted list, then merge sort=T, then pull out request
  lc <- 0
  outlist <- NULL
  nick_name <- as.character(tkget(listbox_active_sort, 0, 'end'))
  lapply(nick_name, function(j) {
    nick_name2 <- strsplit(sub('-', '\n!', j), '\n!')[[1]]
    my_ref  <-
      LIST_DATA$gene_info[[nick_name2[1]]][[nick_name2[2]]][5]
    enesg <-
      data_frame(gene = LIST_DATA$gene_file[[nick_name2[1]]]$use)
    df <-
      semi_join(LIST_DATA$table_file[[my_ref]], enesg, by = 'gene')
    apply_bins <- group_by(df, gene) %>%
      filter(bin %in% R_start_bin:R_end_bin) %>%
      summarise(mysums = sum(score, na.rm = TRUE)) %>%
      mutate(myper = percent_rank(mysums),
             gene = gsub("(:|\\-;|\\+;)", "~\\1~", sub("(-)", "~\\1~", gene))) %>%
      ungroup()
    
    gene_count <- nrow(apply_bins)
    
    if (R_option == "Top%") {
      num <- c(1, ceiling(gene_count * (R_num / 100)))
    } else if (R_option == "Quick%") {
      num <-
        c(1, count(apply_bins, myper >= mean(myper, na.rm = TRUE))[[2]][2])
    } else {
      num <-
        c(ceiling((gene_count + 1) - (gene_count * (R_num / 100))), gene_count)
    }
    outlist2 <- arrange(apply_bins, desc(mysums)) %>%
      slice(num[1]:num[2])
    if (lc > 0) {
      outlist <<- inner_join(outlist, outlist2, by = 'gene')
      names(outlist)[2] <<- "mysums"
    } else {
      outlist <<- outlist2
    }
    lc <<- lc + 1
  })
  outlist <-
    separate(outlist,
             gene,
             c("chr", "co", "s", "dash", "e", "sign", "gene"),
             "~",
             convert = T) %>%
    arrange(chr, s)
  
  if (!suppressWarnings(is.na(as.numeric(tclvalue(
    tkget(entry_dist_span)
  ))))) {
    dist_span <- as.numeric(tclvalue(tkget(entry_dist_span)))
    setTkProgressBar(pb,
                     nrow(outlist),
                     label = paste("removing overlapped genes plus", dist_span, "bp"))
    too_close <- NULL
    outlist2 <- outlist[1, ]
    for (i in 2:(nrow(outlist) - 1)) {
      if (outlist[i, 1] == outlist2[1]) {
        if (any(
          between(
            as.numeric(outlist[i, 3]),
            outlist2$s - dist_span,
            outlist2$e + dist_span
          ),
          between(
            as.numeric(outlist[i, 5]),
            outlist2$s - dist_span,
            outlist2$e + dist_span
          )
        )) {
          outlist2 <- outlist[i, ]
          too_close <- unique(c(too_close, i, i - 1))
        } else {
          outlist2 <- outlist[i, ]
        }
      } else {
        outlist2 <- outlist[i, ]
      }
    }
    if (!is.null(too_close)) {
      outlist <- slice(outlist,-too_close)
    }
  }
  outlist <-
    unite(outlist,
          col = gene,
          chr,
          co,
          s,
          dash,
          e,
          sign,
          gene,
          sep = "") %>% arrange(desc(mysums))
  
  if (R_order == 'Accend') {
    outlist$gene <- rev(outlist$gene)
  }
  tkdelete(listbox_active_gene_sort, 0, 'end')
  if (length(outlist$gene) > 0) {
    tkconfigure(listbox_active_gene_sort, listvariable = tclVar(as.character(outlist$gene)))
  }
  tkconfigure(label_active_sort_length, text = paste('n = ', (as.integer(
    tksize(listbox_active_gene_sort)
  ))))
  close(pb)
}

# finds 1st median mean and 3ed max bin locations within bin ranges
# one or multiple files
SortFindMaxPeaks <- function() {
  if (as.integer(tksize(listbox_active_peaks)) < 1) {
    return()
  }
  nick_name <- as.character(tkget(listbox_active_peaks, 0, 'end'))
  Start_peak_width <-
    (as.numeric(tclvalue(tcl_bin_center_width_range)) - 1) / 2
  R_start_bin <- as.integer(tclvalue(tcl_bin_start_peaks))
  R_end_bin <- as.integer(tclvalue(tcl_bin_end_peaks))
  lc <- 0
  my_ref <- NULL
  nick_name2 <- NULL
  tt <- NULL
  enesg1 <- NULL
  enesg2 <- NULL
  enesg3 <- NULL
  enesg4 <- NULL
  enesg5 <- NULL
  df <- list()
  df2 <- list()
  clist <- list()
  gene_info <- list(list())
  lapply(nick_name, function(j) {
    nick_name1 <- strsplit(sub('-', '\n!', j), '\n!')[[1]]
    nick_name2 <<-
      c(nick_name2,
        nick_name1[2],
        nick_name1[2],
        nick_name1[2],
        nick_name1[2])
    my_ref1  <-
      LIST_DATA$gene_info[[nick_name1[1]]][[nick_name1[2]]][5]
    my_ref <<- c(my_ref, my_ref1, my_ref1, my_ref1, my_ref1)
    enesg <-
      data_frame(gene = LIST_DATA$gene_file[[nick_name1[1]]]$use)
    df2[[nick_name1[2]]] <<-
      semi_join(LIST_DATA$table_file[[my_ref1]], enesg, by = 'gene')
    df[[nick_name1[2]]] <<-
      group_by(df2[[nick_name1[2]]], gene) %>%
      slice(which.max(score)) %>%
      filter(between(bin, R_start_bin, R_end_bin)) %>%
      ungroup()
    
    if (lc == 0) {
      #hist(ttt)
      print("Use first entry for max bins")
      print(nick_name1[2])
      tt <<- summary(c((select(df[[nick_name1[2]]], bin))$bin))
      print(tt)
      # tt <<-
      #   sort(c(round(kmeans(
      #     ttt,
      #     centers = 4, #c(tt[[1]], tt[[2]], tt[[3]], tt[[4]], tt[[5]], tt[[6]]),
      #     nstart = 200
      #   )$center)))
    }
    tcl_peaks1file <<-
      tclVar(paste(names(tt)[2], "max bins center", floor(tt[[2]])))
    tcl_peaks2file <<-
      tclVar(paste(names(tt)[3], "max bins center", floor(tt[[3]])))
    tcl_peaks3file <<-
      tclVar(paste(names(tt)[4], "max bins center", floor(tt[[4]])))
    tcl_peaks4file <<-
      tclVar(paste(names(tt)[5], "max bins center", floor(tt[[5]])))
    enesg1 <<-
      c(enesg1, collapse(filter(df[[nick_name1[2]]], between(
        bin,
        floor(tt[[2]] - Start_peak_width),
        floor(tt[[2]] + Start_peak_width)
      )) %>%
        select(gene))[[1]])
    enesg2 <<-
      c(enesg2, collapse(filter(df[[nick_name1[2]]], between(
        bin,
        floor(tt[[3]] - Start_peak_width),
        floor(tt[[3]] + Start_peak_width)
      )) %>%
        select(gene))[[1]])
    enesg3 <<-
      c(enesg3, collapse(filter(df[[nick_name1[2]]], between(
        bin,
        floor(tt[[4]] - Start_peak_width),
        floor(tt[[4]] + Start_peak_width)
      )) %>%
        select(gene))[[1]])
    enesg4 <<-
      c(enesg4, collapse(filter(df[[nick_name1[2]]], between(
        bin,
        floor(tt[[5]] - Start_peak_width),
        floor(tt[[5]] + Start_peak_width)
      )) %>%
        select(gene))[[1]])
    if (lc > 0) {
      enesg1 <<- unique(enesg1[duplicated(enesg1)])
      enesg2 <<- unique(enesg2[duplicated(enesg2)])
      enesg3 <<- unique(enesg3[duplicated(enesg3)])
      enesg4 <<- unique(enesg4[duplicated(enesg4)])
    }
    lc <<- lc + 1
  })
  
  for (i in 1:4) {
    color_safe <- i %% length(kListColorSet)
    if (color_safe == 0) {
      color_safe <- 1
    }
    gene_list_label <- get(paste("label_peaks_list", i, sep = ""))
    gene_list <- get(paste("listbox_gene_peaks_list", i, sep = ""))
    my_gene_list <- get(paste("enesg", i, sep = ""))
    enesg <- c(my_gene_list, enesg5)
    enesg <- enesg[duplicated(enesg)]
    if (length(enesg) > 0 & length(my_gene_list) > 0) {
      my_gene_list <-
        my_gene_list[!duplicated(c(my_gene_list, enesg), fromLast = T)[1:length(my_gene_list)]]
    }
    enesg5 <- c(my_gene_list, enesg5)
    if (length(my_gene_list) > 0) {
      clist[[paste("peaks", i, sep = "")]]$use <- my_gene_list
      color_select <- kListColorSet[color_safe]
      gene_info[[paste("peaks", i, sep = "")]][[my_ref[i]]] <-
        c(kDotOptions[1],
          kLineOptions[1],
          color_select,
          1,
          nick_name2[1])
    }
    tkdelete(gene_list, 0, "end")
    tkconfigure(gene_list_label, text = paste(' n = ', (as.integer(
      tksize(gene_list)
    ))))
    tkconfigure(gene_list, listvariable = tclVar(my_gene_list))
    tkconfigure(gene_list_label, text = paste(tclvalue((get(
      paste0("tcl_peaks", i, "file")
    ))),
    ' n = ', (as.integer(
      tksize(gene_list)
    ))))
  }
  if (length(nick_name) == 1) {
    MakeDataFrame(
      sel_list = NULL,
      table_file = df2,
      gene_file = clist,
      gene_info = gene_info
    )
  }
  # print(qplot(data=df[[1]], bin, geom = "histogram", bins = R_end_bin*2))
}

# finds peaks within range of width, can be restricted to a position, and start width
TssMaxPeaks <- function() {
  if (as.integer(tksize(listbox_active_peaks)) < 1) {
    return()
  }
  R_start_bin <- as.numeric(tclvalue(tcl_bin_start_peaks))
  R_end_bin <- as.numeric(tclvalue(tcl_bin_end_peaks))
  pb <- tkProgressBar(title = "Finding peaks, please be patient!!",
                      width = 300)
  
  lc <- 0
  outlist <- NULL
  nick_name <- as.character(tkget(listbox_active_peaks, 0, 'end'))
  lapply(nick_name, function(j) {
    nick_name2 <- strsplit(sub('-', '\n!', j), '\n!')[[1]]
    setTkProgressBar(pb,
                     0,
                     label =  paste("Finding peaks in", nick_name2[1], "please be patient!!"))
    my_ref  <-
      LIST_DATA$gene_info[[nick_name2[1]]][[nick_name2[2]]][5]
    enesg <-
      data_frame(gene = LIST_DATA$gene_file[[nick_name2[1]]]$use)
    
    outlist <<-
      c(outlist,
        collapse(
          semi_join(LIST_DATA$table_file[[my_ref]], enesg, by = 'gene') %>%
            group_by(gene) %>%
            slice(which.max(score)) %>%
            filter(between(bin, R_start_bin, R_end_bin)) %>%
            select(gene)
        )[[1]])
    
    if (lc > 0) {
      outlist <<- unique(outlist[duplicated(outlist)])
    }
    lc <<- lc + 1
  })
  for (i in 1:4) {
    gene_list_label <- get(paste("label_peaks_list", i, sep = ""))
    gene_list <- get(paste("listbox_gene_peaks_list", i, sep = ""))
    tkdelete(gene_list, 0, "end")
    tkconfigure(gene_list_label, text = paste(' n = ', (as.integer(
      tksize(gene_list)
    ))))
    
  }
  if (length(outlist) == 0) {
    print("empty list")
    return()
    close(pb)
  }
  ii <-
    as.integer(strsplit(tk2notetab.text(notebook_peaks), " ")[[1]][2])
  gene_list_label <- get(paste("label_peaks_list", ii, sep = ""))
  gene_list <- get(paste("listbox_gene_peaks_list", ii, sep = ""))
  tkconfigure(gene_list, listvariable = tclVar(as.character(outlist)))
  tkconfigure(gene_list_label, text = paste(' n = ', as.integer(tksize(gene_list))))
  close(pb)
  GeneListPlotHelper("peaks")
}


# a[1]/b[2] or (a[1]/a[2])/(b[1]/b[2]) make gene list
CompareRatios <- function() {
  if (as.integer(tksize(listbox_active_ratios)) < 2) {
    return()
  }
  R_start1_bin <- as.integer(tclvalue(tcl_bin_start1_ratios))
  R_end1_bin <- as.integer(tclvalue(tcl_bin_end1_ratios))
  R_start2_bin <- as.integer(tclvalue(tcl_bin_start2_ratios))
  R_end2_bin <- as.integer(tclvalue(tcl_bin_end2_ratios))
  R_num <- as.numeric(tclvalue(tcl_bin_fold_ratios))
  lc <- 0
  outlist <- NULL
  nick_name <- as.character(tkget(listbox_active_ratios, 0, 'end'))
  lapply(nick_name, function(j) {
    nick_name2 <- strsplit(sub('-', '\n!', j), '\n!')[[1]]
    my_ref  <-
      LIST_DATA$gene_info[[nick_name2[1]]][[nick_name2[2]]][5]
    
    enesg <-
      data_frame(gene = LIST_DATA$gene_file[[nick_name2[1]]]$use)
    
    df <-
      semi_join(LIST_DATA$table_file[[my_ref]], enesg, by = 'gene')
    # find min value /2 to replace 0s
    
    new_min_for_na <-
      min(na_if(df$score, 0), na.rm = TRUE) / 2
    # replace 0's with min/2
    df <-
      group_by(df, gene) %>%
      summarise(sum1 = sum(score[R_start1_bin:R_end1_bin],	na.rm = T),
                sum2 = sum(score[R_start2_bin:R_end2_bin],	na.rm = T)) %>%
      na_if(0) %>%
      replace_na(list(sum1 = new_min_for_na, sum2 = new_min_for_na))
    
    
    lc <<- lc + 1
    if (R_start2_bin == 0 | R_end2_bin == 0) {
      outlist[[lc]] <<- df
      
    } else {
      outlist[[lc]] <<-
        transmute(df, gene = gene, sum1 = sum1 / sum2)
    }
    
    if (lc > 1) {
      outlist[[1]] <<-
        inner_join(outlist[[1]], outlist[[lc]], by = 'gene') %>%
        transmute(gene = gene, sum2 = sum1.x / sum1.y) %>%
        arrange(desc(sum2))
      
    }
  })
  
  tkdelete(listbox_gene_ratios_up, 0, 'end')
  if (sum(outlist[[1]]$sum2 > R_num) > 0) {
    tkconfigure(listbox_gene_ratios_up, listvariable =
                  tclVar(collapse(
                    filter(outlist[[1]], sum2 > R_num) %>%
                      select(gene)
                  )[[1]]))
  }
  tkconfigure(label_ratios_up, text = paste('n = ', (as.integer(
    tksize(listbox_gene_ratios_up)
  ))))
  
  tkdelete(listbox_gene_ratios_down, 0, 'end')
  if (sum(outlist[[1]]$sum2 < 1 / R_num) > 0) {
    tkconfigure(listbox_gene_ratios_down, listvariable =
                  tclVar(collapse(
                    filter(outlist[[1]], sum2 < 1 / R_num) %>%
                      select(gene)
                  )[[1]]))
  }
  tkconfigure(label_ratios_down, text = paste('n = ', (as.integer(
    tksize(listbox_gene_ratios_down)
  ))))
  
  tkdelete(listbox_gene_ratios_between, 0, 'end')
  if (sum(outlist[[1]]$sum2 <= R_num &
          outlist[[1]]$sum2 >= 1 / R_num) > 0) {
    tkconfigure(listbox_gene_ratios_between,
                listvariable =
                  tclVar(collapse(
                    filter(outlist[[1]], sum2 <= R_num & sum2 >= 1 / R_num) %>%
                      select(gene)
                  )[[1]]))
  }
  tkconfigure(label_ratios_between, text = paste('n = ', (as.integer(
    tksize(listbox_gene_ratios_between)
  ))))
}

# Cumulative Distribution plot
CumulativeDistribution <- function() {
  if (as.integer(tksize(listbox_active_cdf)) < 1) {
    return ()
  }
  R_start1_bin <- as.integer(tclvalue(tcl_bin_start1_cdf))
  R_end1_bin <- as.integer(tclvalue(tcl_bin_end1_cdf))
  R_start2_bin <- as.integer(tclvalue(tcl_bin_start2_cdf))
  R_end2_bin <- as.integer(tclvalue(tcl_bin_end2_cdf))
  R_order <- tclvalue(tcl_acc_dec_cdf)
  if (sum(R_start1_bin, R_end1_bin) > sum(R_start2_bin, R_end2_bin)) {
    use_header <- "Log2 EI Cumulative plot"
  } else {
    use_header <- "Log2 PI Cumulative plot"
  }
  use_per_top <-
    as.numeric(strsplit(tclvalue(tcl_quntile_cdf_top), split = "%")[[1]]) /
    100
  use_per_bottom <-
    as.numeric(strsplit(tclvalue(tcl_quntile_cdf_bottom), split = "%")[[1]]) /
    100
  
  legend_space <- 1
  
  outlist <- NULL
  use_col <- NULL
  my_xlab <- NULL
  nick_name2 <- NULL
  enesg2 <- NULL
  nick_name <- as.character(tkget(listbox_active_cdf, 0, 'end'))
  lapply(nick_name, function(j) {
    use_nickname <- strsplit(sub("-", " ", j), " ")[[1]]
    myTCL <- tclvalue(get(paste0("tcl_", use_nickname[1], "file")))
    my_xlab <<- c(my_xlab, myTCL)
    current_nickname <-
      paste(gsub("(.{17})", "\\1\n", myTCL),
            gsub("(.{17})", "\\1\n", use_nickname[2]),
            sep = '- \n')
    nick_name2 <<- c(nick_name2, current_nickname)
    legend_space <<-
      max(legend_space, (length(strsplit(
        current_nickname, "\n"
      )[[1]]) - 0.5))
    use_col <<-
      c(use_col, LIST_DATA$gene_info[[use_nickname[1]]][[use_nickname[2]]][3])
    my_ref  <-
      LIST_DATA$gene_info[[use_nickname[1]]][[use_nickname[2]]][5]
    
    enesg <-
      data_frame(gene = LIST_DATA$gene_file[[use_nickname[1]]]$use)
    if (is.null(enesg2)) {
      enesg2 <<- collapse(distinct(enesg, gene))[[1]]
    } else{
      enesg2 <<- c(enesg2, collapse(distinct(enesg, gene))[[1]])
      enesg2 <<- enesg2[duplicated(enesg2)]
    }
    df <-
      semi_join(LIST_DATA$table_file[[my_ref]], enesg, by = 'gene')
    # find min value /2 to replace 0s
    
    new_min_for_na <-
      min(na_if(df$score, 0), na.rm = TRUE) / 2
    # replace 0's with min/2
    outlist[[j]] <<-
      group_by(df, set, gene) %>%
      summarise(sum1 = sum(score[R_start1_bin:R_end1_bin],	na.rm = T),
                sum2 = sum(score[R_start2_bin:R_end2_bin],	na.rm = T)) %>%
      na_if(0) %>%
      replace_na(list(sum1 = new_min_for_na, sum2 = new_min_for_na)) %>%
      transmute(set = current_nickname,
                gene = gene,
                value = sum1 / sum2) %>%
      arrange(desc(value)) %>% mutate(bin = 1:n()) %>%
      filter(between(bin, ceiling(length(enesg$gene) * use_per_top),
                     (
                       length(enesg$gene) - ceiling(length(enesg$gene) * use_per_bottom)
                     )))
  })
  
  for (i in 1:4) {
    gene_list_label <- get(paste("label_cdf_list", i, sep = ""))
    gene_list <- get(paste("listbox_gene_cdf_list", i, sep = ""))
    tkdelete(gene_list, 0, "end")
    tkconfigure(gene_list_label, text = paste(' n = ', (as.integer(
      tksize(gene_list)
    ))))
  }
  enesg2 <- data_frame(gene = enesg2)
  for (i in seq_along(outlist)) {
    if (tclvalue(tcl_checkbox_cdf_innerjoin) == 1) {
      outlist[[i]] <- semi_join(outlist[[i]], enesg2, by = 'gene')
    }
    gene_list_label <- get(paste("label_cdf_list", i, sep = ""))
    gene_list <- get(paste("listbox_gene_cdf_list", i, sep = ""))
    gene_names <- paste(collapse(distinct(outlist[[i]], gene))[[1]])
    if (R_order == 'Accend') {
      gene_names <- rev(gene_names)
    }
    tkconfigure(gene_list, listvariable =
                  tclVar(gene_names))
    tkconfigure(gene_list_label, text =
                  paste("File", i, ' n = ', (as.integer(
                    tksize(gene_list)
                  ))))
    
    my_xlab[i] <- paste(my_xlab[i], "n =",
                        length(gene_names))
    
  }
  
  if (length(outlist) == 2) {
    tt <- suppressWarnings(ks.test((outlist[[1]]$value),
                                   (outlist[[2]]$value)))
    if (tt[[2]] == 0) {
      ttt <- "< 2.2e-16"
    } else {
      ttt <- tt[[2]]
    }
    use_header <-
      paste(use_header, paste("  p-value = ", format(ttt, scientific = TRUE)))
    print(tt)
  }
  names(use_col) <- nick_name2
  df <- bind_rows(outlist)
  GGplotC(df, unique(my_xlab), use_col, use_header, legend_space)
  
}

# finds 2 - 4 clusers from the one active file, plotting the patterns and displaying the gene lists
FindClusters <- function() {
  if (as.integer(tksize(listbox_active_cluster)) < 1) {
    return ()
  }
  STATE[3] <<- "cluster"
  pb <<-
    tkProgressBar(title = "Finding clusters, please be patient!!",
                  width = 300)
  setTkProgressBar(pb, 100, label =  "Finding clusters, please be patient!!")
  
  R_start_bin <- as.integer(tclvalue(tcl_bin_start_cluster))
  R_stop_bin <- as.integer(tclvalue(tcl_bin_end_cluster))
  
  nick_name <-  strsplit(sub('-', '\n!',
                             as.character(
                               tkget(listbox_active_cluster, 0, 'end')
                             )), '\n!')[[1]]
  my_ref  <- LIST_DATA$gene_info[[nick_name[1]]][[nick_name[2]]][5]
  enesg <-
    data_frame(gene = LIST_DATA$gene_file[[nick_name[1]]]$use)
  df <-
    semi_join(LIST_DATA$table_file[[my_ref]], enesg, by = 'gene')
  df2 <- as.data.frame(spread(df, bin, score))
  cm <-
    hclust.vector(df2[, c((R_start_bin:R_stop_bin) + 2)], method = "ward")
  LIST_DATA[["clust"]][["cm"]] <<- cm
  LIST_DATA[["clust"]][["use"]] <<- df2[, 1]
  close(pb)
  ClusterNumList(LIST_DATA$clust)
  
  tkraise(root)
}

# intersect gene lists creating a new one
IntersectGeneLists <- function(genelist, genename) {
  if (is.null(genelist)) {
    return()
  }
  if (as.integer(tksize(listbox_intersect_combind)) > 0) {
    genelist1 <-
      unique(c(genelist, as.character(
        tkget(listbox_intersect_combind, 0, 'end')
      )))
  } else{
    genelist1 <- unique(genelist)
  }
  # if first time load in inclusive
  if (length(genelist) < 1) {
    tkmessageBox(message = "no genes in common", icon = "info")
    return()
  }
  genelist2 <- character()
  if (as.integer(tksize(listbox_intersect_inclusive)) > 0) {
    genelist <-
      c(genelist, as.character(tkget(
        listbox_intersect_inclusive, 0, 'end'
      )))
    genelist2 <-
      genelist[!(duplicated(genelist) |
                   duplicated(genelist, fromLast = TRUE))]
    genelist <- unique(genelist[duplicated(genelist)])
  }
  if (as.integer(tksize(listbox_intersect_exclusive)) > 0) {
    genelist2 <-
      c(genelist2, as.character(tkget(
        listbox_intersect_exclusive, 0, 'end'
      )))
    genelist2 <- unique(genelist2)
  }
  
  tkinsert(listbox_active_intersect, 0, genename)
  tkconfigure(listbox_intersect_combind, listvariable = tclVar(genelist1))
  tkconfigure(label_intersect_combind, text = paste('n = ', (as.integer(
    tksize(listbox_intersect_combind)
  ))))
  tkconfigure(listbox_intersect_inclusive, listvariable = tclVar(genelist))
  tkconfigure(label_intersect_inclusive, text = paste('n = ', (as.integer(
    tksize(listbox_intersect_inclusive)
  ))))
  tkconfigure(listbox_intersect_exclusive, listvariable = tclVar(genelist2))
  tkconfigure(label_intersect_exclusive, text = paste('n = ', (as.integer(
    tksize(listbox_intersect_exclusive)
  ))))
  
}

# pop up box to change color, and name, along with other plot options
handleColorSel <- function(in_list, list_set) {
  if (length(LIST_DATA$table_file) > 0) {
    sel <- as.integer(tkcurselection(in_list))
    if (length(sel) == 0) {
      return()
    }
    SampleOptionsFrame <<- tktoplevel()
    STATE[1] <<- 1
    tkbind(SampleOptionsFrame, "<Destroy>", function()
      OnDestroy())
    nick_name <- tclvalue(tkget(in_list, sel))
    mydot <- LIST_DATA$gene_info[[list_set]][[nick_name]][1]
    myline <- LIST_DATA$gene_info[[list_set]][[nick_name]][2]
    mycolor <- LIST_DATA$gene_info[[list_set]][[nick_name]][3]
    myname <- LIST_DATA$gene_info[[list_set]][[nick_name]][5]
    tmp <- mycolor
    OnUpdate <- function() {
      DActAll()
      new_name <- tclvalue(tkget(NickName))
      if (nchar(new_name) > 0) {
        if (new_name != nick_name &&
            new_name %in% names(LIST_DATA$gene_info[[list_set]])) {
          new_name <- paste(new_name, '-rep?')
        }
        LIST_DATA$gene_info[[list_set]][[nick_name]][1] <<-
          tclvalue(tcl_dot_option)
        LIST_DATA$gene_info[[list_set]][[nick_name]][2] <<-
          tclvalue(tcl_line_option)
        if (new_name != nick_name) {
          tkdelete(in_list, sel)
          tkinsert(in_list, sel, new_name)
          names(LIST_DATA$gene_info[[list_set]])[names(LIST_DATA$gene_info[[list_set]]) == nick_name] <<-
            new_name
          LIST_DATA$table_file[[myname]] <<-
            mutate(LIST_DATA$table_file[[myname]],
                   set = gsub("(.{17})", "\\1\n", new_name))
          tkconfigure(combobox_numerator,
                      values = names(LIST_DATA$gene_info$common))
          tkconfigure(combobox_denominator,
                      values = names(LIST_DATA$gene_info$common))
        }
        LIST_DATA$gene_info[[list_set]][[new_name]][3] <<- tmp
        tkitemconfigure(in_list, sel, foreground = tmp)
        norm_factor <- tclvalue(tkget(EntryNormFactor))
        if (!suppressWarnings(is.na(as.numeric(norm_factor))) &
            norm_factor != 0) {
          LIST_DATA$table_file[[myname]] <<-
            mutate(LIST_DATA$table_file[[myname]],
                   score = score / as.numeric(norm_factor))
        }
        tkdestroy(SampleOptionsFrame)
        STATE[1] <<- 0
        tkraise(root)
      }
    }
    
    OnCOL <- function(...) {
      tmp <<- tclvalue(tcl(
        "tk_chooseColor",
        initialcolor = mycolor,
        title = "Choose color"
      ))
      if (nchar(tmp) < 1) {
        tmp <<- mycolor
      }
    }
    
    tkgrid(tklabel(SampleOptionsFrame, text = "Set new name and color"),
           columnspan = 2)
    tkgrid(
      tk2button(
        SampleOptionsFrame,
        text = "Set Color",
        command = function()
          OnCOL()
      ),
      columnspan = 2,
      pady = c(10, 10)
    )
    
    tkgrid(
      tklabel(SampleOptionsFrame, text = "Nick name"),
      padx = c(0, 0),
      pady = c(10, 10)
    )
    tkgrid(
      NickName <- tk2entry(SampleOptionsFrame, width = kWidth + 40),
      column = 1,
      row = 2,
      pady = c(10, 10)
    )
    tkinsert(NickName, 0, nick_name)
    
    tkgrid(
      tklabel(SampleOptionsFrame, text = "Norm factor"),
      padx = c(0, 0),
      pady = c(10, 10)
    )
    tkgrid(
      EntryNormFactor <- tk2entry(SampleOptionsFrame, width = 10),
      column = 1,
      row = 3,
      pady = c(10, 10),
      sticky = "w"
    )
    
    tkgrid(tklabel(SampleOptionsFrame, text = "line"), padx = c(0, 0))
    combobox_lines <- tk2combobox(
      SampleOptionsFrame,
      value = kLineOptions,
      textvariable = tcl_line_option,
      state = "readonly",
      width = 10
    )
    tkgrid(
      combobox_lines,
      sticky = "w",
      column = 1,
      row = 4,
      padx = c(0, 16)
    )
    tkset(combobox_lines, myline)
    
    tkgrid(
      tklabel(SampleOptionsFrame, text = "dot"),
      padx = c(0, 0),
      pady = c(10, 10)
    )
    
    combobox_dots <- tk2combobox(
      SampleOptionsFrame,
      value = kDotOptions,
      textvariable = tcl_dot_option,
      state = "readonly",
      width = 10
    )
    tkgrid(
      combobox_dots,
      sticky = "w",
      column = 1,
      row = 5,
      padx = c(0, 16),
      pady = c(10, 10)
    )
    tkset(combobox_dots, mydot)
    
    tkgrid(
      tk2button(SampleOptionsFrame, text = "   OK   ", command = OnUpdate),
      columnspan = 2,
      pady = c(10, 10)
    )
  }
}

# quits program and closes plot window
QuitProgram <- function() {
  if (STATE[1] == 0) {
    tkdestroy(root)
  } else{
    return()
  }
}

# restart, removes all files and lists
ClearTableFile <- function() {
  if (STATE[1] == 0) {
    DActAll()
    sapply(names(LIST_DATA$gene_file), function(x) {
      tkdelete(get(paste("listbox_" , x, "_on", sep = "")), 0, 'end')
      tkdelete(get(paste("listbox_" , x, "_off", sep = "")), 0, 'end')
      tkconfigure(get(paste("label_" , x, "_length", sep = "")), text = 'n = 0')
      if (x != "common") {
        myTCL <- get(paste0("tcl_", x, "file"))
        tclvalue(myTCL) <- x
        tkconfigure(get(paste("label_" , x, "_file", sep = "")), textvariable = myTCL)
      }
    })
    tkconfigure(combobox_numerator, values = "numerator")
    tkset(combobox_numerator, "numerator")
    tkconfigure(combobox_denominator, values = "denominator")
    tkset(combobox_denominator, "denominator")
    
    LIST_DATA <<- list(
      table_file = list(),
      # [[]] gene X1 X2 ...
      gene_file = list(),
      # holds $common genes from files and $gene file(s)
      gene_info = list()
    )  # for holding gene file info in a list of lists, a set for $common and each $gene file(s)
    # [c("dot", "line", "color", plot?)]
    STATE <<-
      c(0, 'common') # [1] for open option window, [2] for a selected box
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
  SetComboBoxes(unlist(summarise(
    LIST_DATA$table_file[[1]], n_distinct(bin)
  )))
}

# set up root window ----

root <- tktoplevel() #container for it all
tkwm.title(root, version_num)

# menu setup ----
menu_top <- tk2menu(root)           # Create a menu
tkconfigure(root, menu = menu_top)  # Add it to the main window
menu_top_file <- tkmenu(menu_top, tearoff = FALSE)
tkadd(
  menu_top_file,
  "command",
  label = "Load table file",
  command = function()
    LoadTableFile()
)
tkadd(
  menu_top_file,
  "command",
  label = "Load color pallet",
  command = function()
    GetColor()
)
tkadd(
  menu_top_file,
  "command",
  label = "Quit",
  command = function()
    QuitProgram()
)
tkadd(
  menu_top_file,
  "command",
  label = "Restart",
  command = function()
    ClearTableFile()
)
tkadd(menu_top, "cascade", label = "File", menu = menu_top_file)

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
                                        relief = 'ridge',
                                        borderwidth = 5)

tkgrid(tklabel(frame_notebook_load_file_tab, text = tclvalue(tcl_commonfile)),
       columnspan = 3)

tkgrid(
  label_common_length <-
    tklabel(frame_notebook_load_file_tab, text = "n = "),
  columnspan = 3
)

tkgrid(
  listbox_common_on <-
    tk2listbox(
      frame_notebook_load_file_tab,
      width = kWidth + 8,
      height = kHeight,
      autoscroll = "none",
      tip =
        "Double click on a file name to open options for that entry"
    ),
  columnspan = 3
)
tkgrid.columnconfigure(frame_notebook_load_file_tab, listbox_common_on, weight =
                         1)
tkgrid.rowconfigure(frame_notebook_load_file_tab, listbox_common_on, weight =
                      1)

tkbind(listbox_common_on, "<Double-ButtonPress-1>", function()
  handleColorSel(listbox_common_on, 'common'))
tkbind(listbox_common_on, "<<ListboxSelect>>", function()
  SelectionControl('common',  '_off'))

tkgrid(
  tk2button(
    frame_notebook_load_file_tab,
    text = "  <<Switch>>  ",
    command = function()
      MoveSelectToOtherEntry(listbox_common_on, listbox_common_off)
  ),
  tk2button(
    frame_notebook_load_file_tab,
    text = "  <<All On>>  ",
    command = function()
      MoveAllToOtherEntry(listbox_common_on, listbox_common_off, "on")
  ),
  tk2button(
    frame_notebook_load_file_tab,
    text = "  <<All Off>>  ",
    command = function()
      MoveAllToOtherEntry(listbox_common_on, listbox_common_off, "off")
  ),
  sticky = 'we',
  padx = c(2, 2)
)

tkgrid(
  listbox_common_off <-
    tk2listbox(
      frame_notebook_load_file_tab,
      width = kWidth + 8,
      height = kHeight,
      autoscroll = "none",
      tip =
        "files in this list wont show up on plot"
    ),
  columnspan = 3
)
tkgrid.columnconfigure(frame_notebook_load_file_tab, listbox_common_off, weight =
                         1)
tkgrid.rowconfigure(frame_notebook_load_file_tab, listbox_common_off, weight =
                      1)

tkbind(listbox_common_off, "<Double-ButtonPress-1>", function()
  handleColorSel(listbox_common_off, 'common'))
tkbind(listbox_common_off, "<<ListboxSelect>>", function()
  SelectionControl('common',  '_on'))

tkgrid(
  tk2button(
    frame_notebook_load_file_tab,
    text = " Load table file ",
    command =  function()
      LoadTableFile()
  ),
  row = 7,
  column = 0
)

tkgrid(
  tk2button(
    frame_notebook_load_file_tab,
    text = " Intersect list ",
    command = function()
      IntersectGeneLists(LIST_DATA$gene_file$common$use, tclvalue(tcl_commonfile))
  ),
  column = 1,
  row = 7
)

tkgrid(
  tk2button(
    frame_notebook_load_file_tab,
    text = " Remove file",
    command =  function()
      RemoveFile()
  ),
  row = 7,
  column = 2
)

tkgrid(frame_notebook_load_file_tab, sticky = 'nsew')
tkgrid.columnconfigure(notebook_load_file_tab,
                       frame_notebook_load_file_tab,
                       weight = 1)
tkgrid.rowconfigure(notebook_load_file_tab,
                    frame_notebook_load_file_tab,
                    weight = 1)

tkpack.configure(frame_notebook_load_file_tab,
                 fill = "both",
                 expand = 1)

# FILE: common Plot Option math norm and bins ----

notebook_options_tab <- tk2notetab(notebook_file,
                                   "Plot Options")

tab_plot_options_frame <- tkframe(notebook_options_tab,
                                  relief = 'ridge',
                                  borderwidth = 5)

tkgrid(
  tk2combobox(
    tab_plot_options_frame,
    value = kMathOptions,
    textvariable = tcl_math_option,
    state = "readonly",
    width = 6
  ),
  column = 0 ,
  row = 0,
  padx = c(10, 10),
  pady = c(10, 10)
)

tkgrid(
  tklabel(tab_plot_options_frame, text = "Norm to bin"),
  column = 1 ,
  row = 0,
  padx = c(10, 0),
  pady = c(10, 10)
)

tkgrid(
  combobox_norm_bin <- tk2combobox(
    tab_plot_options_frame,
    textvariable = tcl_norm_bin,
    state = "readonly",
    width = 3
  ),
  column = 2,
  row = 0,
  pady = c(10, 10)
)



checkbox_relative_frequency <-
  tkcheckbutton(tab_plot_options_frame,
                variable = tcl_checkbox_relative_frequency,
                text = "Rel Freq")
tkgrid(
  checkbox_relative_frequency,
  column = 0 ,
  row = 1,
  pady = c(10, 10)
)

checkbox_relative_gene_frequency <-
  tkcheckbutton(tab_plot_options_frame,
                variable = tcl_checkbox_relative_gene_frequency,
                text = "Rel Gene  ")
tkgrid(
  checkbox_relative_gene_frequency,
  column = 1 ,
  row = 1,
  pady = c(10, 10)
)
tkgrid(
  tkcheckbutton(tab_plot_options_frame,
                variable = tcl_smooth, text = "smooth"),
  column = 2,
  row = 1,
  pady = c(10, 10)
)

tkgrid(
  tkcheckbutton(tab_plot_options_frame,
                variable = tcl_checkbox_log2, text = "log2"),
  column = 3,
  row = 1,
  pady = c(10, 10),
  sticky = "w"
)
tkgrid(
  tklabel(tab_plot_options_frame, text = "Zoom into bins"),
  column = 0 ,
  row = 2,
  pady = c(10, 10)
)

combobox_bin_start <- tk2combobox(
  tab_plot_options_frame,
  textvariable = tcl_bin_start,
  state = "readonly",
  width = 3
)
tkgrid(
  combobox_bin_start,
  padx = c(0, 0),
  column = 1,
  row = 2,
  padx = c(0, 0),
  pady = c(10, 10)
)

tkgrid(
  tklabel(tab_plot_options_frame, text = "to"),
  column = 2,
  row = 2,
  padx = c(0, 0),
  pady = c(10, 10),
  sticky = "w"
)

combobox_bin_end <- tk2combobox(
  tab_plot_options_frame,
  textvariable = tcl_bin_end,
  state = "readonly",
  width = 3
)
tkgrid(
  combobox_bin_end,
  column = 2,
  row = 2,
  padx = c(0, 0),
  pady = c(10, 10),
  columnspan = 2
)

tkbind(combobox_bin_start, "<<ComboboxSelected>>", function()
  BinStartEndHelper(
    tcl_bin_start,
    tcl_bin_end,
    combobox_bin_start,
    combobox_bin_end,
    1
  ))
tkbind(combobox_bin_end, "<<ComboboxSelected>>", function()
  BinStartEndHelper(
    tcl_bin_start,
    tcl_bin_end,
    combobox_bin_start,
    combobox_bin_end,
    2
  ))

tkgrid(
  checkbox_y_axis <-
    tkcheckbutton(tab_plot_options_frame,
                  variable = tcl_checkbox_Y_axis,
                  text = "Set Y axis  "),
  column = 0,
  row = 3,
  padx = c(5, 0),
  pady = c(10, 10),
  sticky = "w"
)

tkgrid(
  entry_Y_axis_min <- tk2entry(
    tab_plot_options_frame,
    width = 5,
    textvariable = tcl_Y_axis_min
  ),
  column = 1,
  row = 3,
  padx = c(5, 0),
  pady = c(10, 10),
  sticky = "w"
)

tkgrid(
  tklabel(tab_plot_options_frame, text = "to"),
  column = 1,
  row = 3,
  padx = c(0, 10),
  pady = c(10, 10),
  sticky = "e"
)

tkgrid(
  entry_Y_axis_max <- tk2entry(
    tab_plot_options_frame,
    width = 5,
    textvariable = tcl_Y_axis_max
  ),
  column = 2,
  row = 3,
  columnspan = 2,
  padx = c(10, 0),
  pady = c(10, 10),
  sticky = "w"
)

tkgrid(
  tklabel(tab_plot_options_frame, text = "Color sets"),
  column = 0,
  row = 4,
  padx = c(5, 0),
  pady = c(10, 10),
  sticky = "w"
)

combobox_brewer <- tk2combobox(
  tab_plot_options_frame,
  value = kBrewerList,
  textvariable = tcl_brewer,
  state = "readonly",
  width = 7
)
tkgrid(
  combobox_brewer,
  column = 0,
  row = 4,
  padx = c(0, 5),
  pady = c(10, 10),
  columnspan = 2,
  sticky = 'e'
)
tkbind(combobox_brewer, "<<ComboboxSelected>>", function()
  BrewerSet())

tkgrid(
  tk2button(
    tab_plot_options_frame,
    text = '  Load color file  ',
    command = function()
      GetColor()
  ),
  column = 2,
  row = 4,
  columnspan = 3,
  pady = c(10, 10)
)

tkgrid(
  tk2button(
    tab_plot_options_frame,
    text = '   Extra Options   ',
    command = function()
      GlobalOptions()
  ),
  columnspan = 5,
  pady = c(10, 10)
)


tkgrid(
  tab_plot_options_frame,
  column = 0,
  row = 0,
  sticky = 'nsew'
)
tkpack.configure(tab_plot_options_frame, fill = "both", expand = 1)

# FILE: line and tick tab for line and tick plot options ----

notebook_lines_Labels_tab <- tk2notetab(notebook_file,
                                        "Lines & Labels")

tab_plot_options_line_tick_frame <-
  tkframe(notebook_lines_Labels_tab,
          relief = 'ridge',
          borderwidth = 5)

tkgrid(tklabel(tab_plot_options_line_tick_frame, text = ' Plot Options '),
       columnspan = 6)

tkgrid(
  tklabel(tab_plot_options_line_tick_frame,
          text = "Plot lines and labels"),
  pady = c(5, 5),
  row = 7,
  column = 0,
  columnspan = 6
)

combobox_plot_lines <-
  tk2combobox(
    tab_plot_options_line_tick_frame,
    value = names(list_plot_lines),
    textvariable = tcl_plot_line_name,
    state = "readonly",
    width = 20
  )
tkgrid(
  combobox_plot_lines,
  column = 0,
  columnspan = 6,
  row = 6
)
tkbind(combobox_plot_lines, '<<ComboboxSelected>>', function()
  PlotLines())

tkgrid(
  tk2entry(
    tab_plot_options_line_tick_frame,
    width = 5,
    textvariable = tcl_one_tss_tts_option
  ),
  padx = c(10, 0),
  pady = c(5, 0),
  column = 0,
  row = 8
)
tkgrid(
  tklabel(tab_plot_options_line_tick_frame, text = 'Pos'),
  padx = c(5, 3),
  pady = c(5, 0),
  column = 1,
  row = 8,
  sticky = "w"
)
entry_line_tick_pos_one <-
  tk2entry(tab_plot_options_line_tick_frame,
           width = 4,
           textvariable = tcl_pos_one_line)
tkgrid(
  entry_line_tick_pos_one,
  column = 2,
  row = 8,
  sticky = "w",
  padx = c(0, 0),
  pady = c(5, 0)
)

tkgrid(
  tk2entry(
    tab_plot_options_line_tick_frame,
    width = 5,
    textvariable = tcl_two_tss_tts_option
  ),
  padx = c(10, 0),
  pady = c(3, 0),
  column = 0,
  row = 9
)
tkgrid(
  tklabel(tab_plot_options_line_tick_frame, text = 'Pos'),
  padx = c(5, 3),
  pady = c(3, 0),
  column = 1,
  row = 9,
  sticky = "w"
)
entry_line_tick_pos_two <-
  tk2entry(tab_plot_options_line_tick_frame,
           width = 4,
           textvariable = tcl_pos_two_line)
tkgrid(
  entry_line_tick_pos_two,
  column = 2 ,
  row = 9,
  sticky = "w",
  padx = c(0, 0),
  pady = c(3, 0)
)

tkgrid(
  tk2entry(
    tab_plot_options_line_tick_frame,
    width = 5,
    textvariable = tcl_three_tss_tts_option
  ),
  padx = c(10, 0),
  pady = c(5, 0),
  column = 3,
  row = 8
)
tkgrid(
  tklabel(tab_plot_options_line_tick_frame, text = 'Pos'),
  padx = c(5, 3),
  pady = c(5, 0),
  column = 4,
  row = 8,
  sticky = "w"
)
entry_line_tick_pos_three <-
  tk2entry(tab_plot_options_line_tick_frame,
           width = 4,
           textvariable = tcl_pos_three_line)
tkgrid(
  entry_line_tick_pos_three,
  column = 5,
  row = 8,
  sticky = "w",
  padx = c(0, 0),
  pady = c(3, 0)
)

tkgrid(
  tk2entry(
    tab_plot_options_line_tick_frame,
    width = 5,
    textvariable = tcl_four_tss_tts_option
  ),
  padx = c(10, 0),
  pady = c(5, 0),
  column = 3,
  row = 9
)
tkgrid(
  tklabel(tab_plot_options_line_tick_frame, text = 'Pos'),
  column = 4,
  row = 9,
  sticky = "w",
  padx = c(5, 3),
  pady = c(5, 0)
)
entry_line_tick_pos_four <-
  tk2entry(tab_plot_options_line_tick_frame,
           width = 4,
           textvariable = tcl_pos_four_line)
tkgrid(
  entry_line_tick_pos_four,
  column = 5,
  row = 9,
  sticky = "w",
  padx = c(0, 0),
  pady = c(3, 0)
)

tkgrid(
  tklabel(tab_plot_options_line_tick_frame, text = "More Bin labels"),
  row = 11,
  columnspan = 6,
  pady = c(10, 2)
)

tkgrid(
  tklabel(tab_plot_options_line_tick_frame, text = 'Pos'),
  padx = c(5, 0),
  column = 0,
  row = 12,
  sticky = "w"
)
entry_line_tick_pos_five <-
  tk2entry(tab_plot_options_line_tick_frame,
           width = kWidth + 2,
           textvariable = tcl_pos_plot_ticks)
tkgrid(
  entry_line_tick_pos_five,
  column = 1,
  row = 12,
  padx = c(0, 0),
  columnspan = 6,
  sticky = "w"
)
tkgrid(
  tklabel(tab_plot_options_line_tick_frame, text = 'label'),
  padx = c(5, 0),
  column = 0,
  row = 13,
  sticky = "w"
)
entry_line_tick_label_five <-
  tk2entry(tab_plot_options_line_tick_frame,
           width = kWidth + 2,
           textvariable = tcl_label_plot_ticks)
tkgrid(
  entry_line_tick_label_five,
  column = 1,
  row = 13,
  padx = c(0, 0),
  columnspan = 6,
  sticky = "w"
)

tkgrid(
  tklabel(tab_plot_options_line_tick_frame, text = ' 1st lable,   pos,  every,  size bin,  # entries '),
  columnspan = 6,
  row = 14,
  sticky = "w",
  pady = c(15, 0)
)

tkgrid(
  tk2entry(
    tab_plot_options_line_tick_frame,
    width = 5,
    textvariable = tcl_tss_start_label
  ),
  padx = c(10, 0),
  pady = c(5, 5),
  column = 0,
  row = 15
)

tkgrid(
  tk2entry(
    tab_plot_options_line_tick_frame,
    width = 5,
    textvariable = tcl_tss_start_bin
  ),
  padx = c(10, 0),
  pady = c(5, 5),
  column = 1,
  row = 15
)

tkgrid(
  tk2entry(
    tab_plot_options_line_tick_frame,
    width = 5,
    textvariable = tcl_tss_num_bp
  ),
  padx = c(10, 0),
  pady = c(5, 5),
  column = 2,
  row = 15
)

tkgrid(
  tk2entry(
    tab_plot_options_line_tick_frame,
    width = 5,
    textvariable = tcl_tss_size_bin
  ),
  padx = c(10, 0),
  pady = c(5, 5),
  column = 3,
  row = 15
)

tkgrid(
  tk2entry(
    tab_plot_options_line_tick_frame,
    width = 5,
    textvariable = tcl_tss_number_entry
  ),
  padx = c(10, 10),
  pady = c(5, 5),
  column = 4,
  row = 15
)

tkgrid(
  tk2entry(
    tab_plot_options_line_tick_frame,
    width = 5,
    textvariable = tcl_tts_start_label
  ),
  padx = c(10, 0),
  pady = c(5, 5),
  column = 0,
  row = 16
)

tkgrid(
  tk2entry(
    tab_plot_options_line_tick_frame,
    width = 5,
    textvariable = tcl_tts_start_bin
  ),
  padx = c(10, 0),
  pady = c(5, 5),
  column = 1,
  row = 16
)

tkgrid(
  tk2entry(
    tab_plot_options_line_tick_frame,
    width = 5,
    textvariable = tcl_tts_num_bp
  ),
  padx = c(10, 0),
  pady = c(5, 5),
  column = 2,
  row = 16
)

tkgrid(
  tk2entry(
    tab_plot_options_line_tick_frame,
    width = 5,
    textvariable = tcl_tts_size_bin
  ),
  padx = c(10, 0),
  pady = c(5, 5),
  column = 3,
  row = 16
)

tkgrid(
  tk2entry(
    tab_plot_options_line_tick_frame,
    width = 5,
    textvariable = tcl_tts_number_entry
  ),
  padx = c(10, 10),
  pady = c(5, 5),
  column = 4,
  row = 16
)

tkgrid(
  tk2button(
    tab_plot_options_line_tick_frame,
    text = " OK ",
    command = function()
      LableMaker()
  ),
  column = 5,
  row = 15,
  rowspan = 2
)



tkgrid(
  tab_plot_options_line_tick_frame,
  column = 0,
  row = 0,
  sticky = 'nsew'
)
tkpack.configure(tab_plot_options_line_tick_frame,
                 fill = "both",
                 expand = 1)

# FILE: norm files tab for making norm file ----

notebook_norm_file_tab <- tk2notetab(notebook_file,
                                     "Norm Files")
frame_norm_files_tab <-
  tkframe(notebook_norm_file_tab,
          relief = 'ridge',
          borderwidth = 5)

tkgrid(
  tklabel(frame_norm_files_tab, text = "select samples for normalization"),
  columnspan = 2,
  pady = c(0, 10)
)

tkgrid(
  tklabel(frame_norm_files_tab, text = "divide:"),
  padx = c(20, 0),
  pady = c(5, 0),
  column = 0,
  row = 1
)

combobox_numerator <-
  tk2combobox(frame_norm_files_tab, state = "readonly")
tkset(combobox_numerator, "numerator")
tkgrid(
  combobox_numerator,
  sticky = "w",
  column = 1,
  row = 1,
  padx = c(0, 16),
  pady = c(5, 0)
)

tkgrid(
  tklabel(frame_norm_files_tab, text = "by:"),
  padx = c(20, 0),
  pady = c(0, 5),
  column = 0,
  row = 2
)

combobox_denominator <-
  tk2combobox(frame_norm_files_tab, state = "readonly")
tkset(combobox_denominator, "denominator")
tkgrid(
  combobox_denominator,
  sticky = "w",
  column = 1,
  row = 2,
  padx = c(0, 16)
)

tkgrid(
  tk2button(
    frame_norm_files_tab,
    text = '      Create       ',
    command = function()
      MakeNormFile()
  ),
  columnspan = 2,
  pady = c(10, 5),
  padx = c(20, 0)
)

tkgrid(
  frame_norm_files_tab,
  column = 0,
  row = 0,
  sticky = 'nsew'
)

tkpack.configure(frame_norm_files_tab, fill = "both", expand = 1)

# FILE grid ----

tkgrid(notebook_file,
       column = 0,
       row = 0,
       sticky = 'nsew')

# buttons for plot button and options ----

# tkgrid(plot_button <- tkbutton(frame_file_notebook, text = '  Plot  ', font = c('bold', 15),
#                 command = function() OnOk()), columnspan = 4)
tkgrid(
  frame_plot_button <-
    tkframe(
      left_frame,
      relief = "groove",
      bg = "blue",
      borderwidth = 5
    ),
  columnspan = 4,
  column = 0,
  row = 1,
  sticky = 'nsew',
  pady = c(10, 10),
  padx = c(20, 20)
) # raised, sunken, flat, ridge, solid, and groove
tkgrid(
  plot_button <- tk2button(
    frame_plot_button,
    text = '  Plot  ',
    width = kWidth + 5,
    command = function()
      MakeDataFrame()
  ),
  sticky = 'ew',
  padx = c(5, 5)
)

# LIST frame for loading gene lists and controls ----

notebook_list <- tk2notebook(
  left_frame,
  tabs = c(
    "Gene\nlist 1",
    "Gene\nlist 2",
    "Gene\nlist 3",
    "Gene\nlist 4",
    "Tool\nlist"
  )
)

# LIST: tab gene list 1 ----

notebook_list_gene1_tab <- tk2notetab(notebook_list, "Gene\nlist 1")

frame_gene1_tab <- tkframe(notebook_list_gene1_tab,
                           relief = 'ridge',
                           borderwidth = 5)

label_gene1_file <-
  tklabel(frame_gene1_tab, text = tclvalue(tcl_gene1file))
tkgrid(label_gene1_file, columnspan = 3)
label_gene1_length <- tklabel(frame_gene1_tab, text = "n = ")
tkgrid(label_gene1_length, columnspan = 3)

listbox_gene1_on <-
  tk2listbox(
    frame_gene1_tab,
    width = kWidth + 8,
    height = kHeight,
    autoscroll = 'none',
    tip = "load a txt file of gene lists to plot sub set"
  )
tkgrid(listbox_gene1_on, columnspan = 3)
tkbind(listbox_gene1_on, "<Double-ButtonPress-1>", function()
  handleColorSel(listbox_gene1_on, 'gene1'))
tkbind(listbox_gene1_on, "<<ListboxSelect>>", function()
  SelectionControl('gene1',  '_off'))

tkgrid(
  tk2button(
    frame_gene1_tab,
    text = "<<Switch>>",
    command = function()
      MoveSelectToOtherEntry(listbox_gene1_on, listbox_gene1_off, 'gene1')
  ),
  tk2button(
    frame_gene1_tab,
    text = "<<All On>>",
    command = function()
      MoveAllToOtherEntry(listbox_gene1_on, listbox_gene1_off, "on", file_name = "gene1")
  ),
  tk2button(
    frame_gene1_tab,
    text = "<<All Off>>",
    command = function()
      MoveAllToOtherEntry(listbox_gene1_on, listbox_gene1_off, "off", file_name = "gene1")
  ),
  sticky = 'we',
  padx = c(2, 2)
)
listbox_gene1_off <-
  tk2listbox(
    frame_gene1_tab,
    width = kWidth + 8,
    height = kHeight,
    autoscroll = 'none',
    tip = "files in this list wont show up on plot"
  )
tkgrid(listbox_gene1_off, columnspan = 3)
tkbind(listbox_gene1_off, "<Double-ButtonPress-1>", function()
  handleColorSel(listbox_gene1_off, 'gene1'))
tkbind(listbox_gene1_off, "<<ListboxSelect>>", function()
  SelectionControl('gene1',  '_on'))

tkgrid(
  tk2button(
    frame_gene1_tab,
    text = " Load gene list ",
    command =  function()
      LoadGeneFile('gene1')
  ),
  row = 7,
  columnspan = 2,
  sticky = 'w',
  padx = c(3, 0)
)
tkgrid(
  tk2button(
    frame_gene1_tab,
    text = " Intersect list ",
    command =  function()
      IntersectGeneLists(LIST_DATA$gene_file$gene1$use, tclvalue(tcl_gene1file))
  ),
  row = 7,
  column = 1,
  columnspan = 2,
  sticky = 'e',
  padx = c(0, 3)
)
tkgrid(frame_gene1_tab, sticky = "nesw")
tkpack.configure(frame_gene1_tab, fill = "both", expand = 1)

# LIST: tab gene list 2 ----

notebook_list_gene2_tab <- tk2notetab(notebook_list, "Gene\nlist 2")

frame_gene2_tab <- tkframe(notebook_list_gene2_tab,
                           relief = 'ridge',
                           borderwidth = 5)

label_gene2_file <-
  tklabel(frame_gene2_tab, text = tclvalue(tcl_gene2file))
tkgrid(label_gene2_file, columnspan = 3)
label_gene2_length <- tklabel(frame_gene2_tab, text = "n = ")
tkgrid(label_gene2_length, columnspan = 3)

listbox_gene2_on <-
  tk2listbox(
    frame_gene2_tab,
    width = kWidth + 8,
    height = kHeight,
    autoscroll = 'none',
    tip = "load a txt file of gene lists to plot sub set"
  )
tkgrid(listbox_gene2_on, columnspan = 3)
tkbind(listbox_gene2_on, "<Double-ButtonPress-1>", function()
  handleColorSel(listbox_gene2_on, 'gene2'))
tkbind(listbox_gene2_on, "<<ListboxSelect>>", function()
  SelectionControl('gene2',  '_off'))

tkgrid(
  tk2button(
    frame_gene2_tab,
    text = "<<Switch>>",
    command = function()
      MoveSelectToOtherEntry(listbox_gene2_on, listbox_gene2_off, 'gene2')
  ),
  tk2button(
    frame_gene2_tab,
    text = "<<All On>>",
    command = function()
      MoveAllToOtherEntry(listbox_gene2_on, listbox_gene2_off, "on", file_name = "gene2")
  ),
  tk2button(
    frame_gene2_tab,
    text = "<<All Off>>",
    command = function()
      MoveAllToOtherEntry(listbox_gene2_on, listbox_gene2_off, "off", file_name = "gene2")
  ),
  sticky = 'we',
  padx = c(2, 2)
)
listbox_gene2_off <-
  tk2listbox(
    frame_gene2_tab,
    width = kWidth + 8,
    height = kHeight,
    autoscroll = 'none',
    tip = "files in this list wont show up on plot"
  )
tkgrid(listbox_gene2_off, columnspan = 3)
tkbind(listbox_gene2_off, "<Double-ButtonPress-1>", function()
  handleColorSel(listbox_gene2_off, 'gene2'))
tkbind(listbox_gene2_off, "<<ListboxSelect>>", function()
  SelectionControl('gene2',  '_on'))

tkgrid(
  tk2button(
    frame_gene2_tab,
    text = " Load gene list ",
    command =  function()
      LoadGeneFile('gene2')
  ),
  row = 7,
  columnspan = 2,
  sticky = 'w',
  padx = c(3, 0)
)
tkgrid(
  tk2button(
    frame_gene2_tab,
    text = " Intersect list ",
    command =  function()
      IntersectGeneLists(LIST_DATA$gene_file$gene2$use, tclvalue(tcl_gene2file))
  ),
  row = 7,
  column = 1,
  columnspan = 2,
  sticky = 'e',
  padx = c(0, 3)
)
tkgrid(frame_gene2_tab)
tkpack.configure(frame_gene2_tab, fill = "both", expand = 1)

# LIST: tab gene list 3 ----

notebook_list_gene3_tab <- tk2notetab(notebook_list, "Gene\nlist 3")

frame_gene3_tab <- tkframe(notebook_list_gene3_tab,
                           relief = 'ridge',
                           borderwidth = 5)

label_gene3_file <-
  tklabel(frame_gene3_tab, text = tclvalue(tcl_gene3file))
tkgrid(label_gene3_file, columnspan = 3)
label_gene3_length <- tklabel(frame_gene3_tab, text = "n = ")
tkgrid(label_gene3_length, columnspan = 3)

listbox_gene3_on <-
  tk2listbox(
    frame_gene3_tab,
    width = kWidth + 8,
    height = kHeight,
    autoscroll = 'none',
    tip = "load a txt file of gene lists to plot sub set"
  )
tkgrid(listbox_gene3_on, columnspan = 3)
tkbind(listbox_gene3_on, "<Double-ButtonPress-1>", function()
  handleColorSel(listbox_gene3_on, 'gene3'))
tkbind(listbox_gene3_on, "<<ListboxSelect>>", function()
  SelectionControl('gene3',  '_off'))

tkgrid(
  tk2button(
    frame_gene3_tab,
    text = "<<Switch>>",
    command = function()
      MoveSelectToOtherEntry(listbox_gene3_on, listbox_gene3_off, 'gene3')
  ),
  tk2button(
    frame_gene3_tab,
    text = "<<All On>>",
    command = function()
      MoveAllToOtherEntry(listbox_gene3_on, listbox_gene3_off, "on", file_name = "gene3")
  ),
  tk2button(
    frame_gene3_tab,
    text = "<<All Off>>",
    command = function()
      MoveAllToOtherEntry(listbox_gene3_on, listbox_gene3_off, "off", file_name = "gene3")
  ),
  sticky = 'we',
  padx = c(2, 2)
)
listbox_gene3_off <-
  tk2listbox(
    frame_gene3_tab,
    width = kWidth + 8,
    height = kHeight,
    autoscroll = 'none',
    tip = "files in this list wont show up on plot"
  )
tkgrid(listbox_gene3_off, columnspan = 3)
tkbind(listbox_gene3_off, "<Double-ButtonPress-1>", function()
  handleColorSel(listbox_gene3_off, 'gene3'))
tkbind(listbox_gene3_off, "<<ListboxSelect>>", function()
  SelectionControl('gene3',  '_on'))

tkgrid(
  tk2button(
    frame_gene3_tab,
    text = " Load gene list ",
    command =  function()
      LoadGeneFile('gene3')
  ),
  row = 7,
  columnspan = 2,
  sticky = 'w',
  padx = c(3, 0)
)
tkgrid(
  tk2button(
    frame_gene3_tab,
    text = " Intersect list ",
    command =  function()
      IntersectGeneLists(LIST_DATA$gene_file$gene3$use, tclvalue(tcl_gene3file))
  ),
  row = 7,
  column = 1,
  columnspan = 2,
  sticky = 'e',
  padx = c(0, 3)
)
tkgrid(frame_gene3_tab)
tkpack.configure(frame_gene3_tab, fill = "both", expand = 1)

# LIST: tab gene list 4 ----

notebook_list_gene4_tab <- tk2notetab(notebook_list, "Gene\nlist 4")

frame_gene4_tab <- tkframe(notebook_list_gene4_tab,
                           relief = 'ridge',
                           borderwidth = 5)

label_gene4_file <-
  tklabel(frame_gene4_tab, text = tclvalue(tcl_gene4file))
tkgrid(label_gene4_file, columnspan = 3)
label_gene4_length <- tklabel(frame_gene4_tab, text = "n = ")
tkgrid(label_gene4_length, columnspan = 3)

listbox_gene4_on <-
  tk2listbox(
    frame_gene4_tab,
    width = kWidth + 8,
    height = kHeight,
    autoscroll = 'none',
    tip = "load a txt file of gene lists to plot sub set"
  )
tkgrid(listbox_gene4_on, columnspan = 3)
tkbind(listbox_gene4_on, "<Double-ButtonPress-1>", function()
  handleColorSel(listbox_gene4_on, 'gene4'))
tkbind(listbox_gene4_on, "<<ListboxSelect>>", function()
  SelectionControl('gene4',  '_off'))

tkgrid(
  tk2button(
    frame_gene4_tab,
    text = "<<Switch>>",
    command = function()
      MoveSelectToOtherEntry(listbox_gene4_on, listbox_gene4_off, 'gene4')
  ),
  tk2button(
    frame_gene4_tab,
    text = "<<All On>>",
    command = function()
      MoveAllToOtherEntry(listbox_gene4_on, listbox_gene4_off, "on", file_name = "gene4")
  ),
  tk2button(
    frame_gene4_tab,
    text = "<<All Off>>",
    command = function()
      MoveAllToOtherEntry(listbox_gene4_on, listbox_gene4_off, "off", file_name = "gene4")
  ),
  sticky = 'we',
  padx = c(2, 2)
)
listbox_gene4_off <-
  tk2listbox(
    frame_gene4_tab,
    width = kWidth + 8,
    height = kHeight,
    autoscroll = 'none',
    tip = "files in this list wont show up on plot"
  )
tkgrid(listbox_gene4_off, columnspan = 3)
tkbind(listbox_gene4_off, "<Double-ButtonPress-1>", function()
  handleColorSel(listbox_gene4_off, 'gene4'))
tkbind(listbox_gene4_off, "<<ListboxSelect>>", function()
  SelectionControl('gene4',  '_on'))

tkgrid(
  tk2button(
    frame_gene4_tab,
    text = " Load gene list ",
    command =  function()
      LoadGeneFile('gene4')
  ),
  row = 7,
  columnspan = 2,
  sticky = 'w',
  padx = c(3, 0)
)
tkgrid(
  tk2button(
    frame_gene4_tab,
    text = " Intersect list ",
    command =  function()
      IntersectGeneLists(LIST_DATA$gene_file$gene4$use, tclvalue(tcl_gene4file))
  ),
  row = 7,
  column = 1,
  columnspan = 2,
  sticky = 'e',
  padx = c(0, 3)
)
tkgrid(frame_gene4_tab)
tkpack.configure(frame_gene4_tab, fill = "both", expand = 1)

# LIST: tab tool list ----

notebook_list_tool_tab <- tk2notetab(notebook_list, "Tool\nlist")

frame_tool_tab <- tkframe(notebook_list_tool_tab,
                          relief = 'ridge',
                          borderwidth = 5)

label_tool_file <-
  tklabel(frame_tool_tab, text = tclvalue(tcl_toolfile))
tkgrid(label_tool_file, columnspan = 3)
label_tool_length <- tklabel(frame_tool_tab, text = "n = ")
tkgrid(label_tool_length, columnspan = 3)

listbox_tool_on <-
  tk2listbox(
    frame_tool_tab,
    width = kWidth + 8,
    height = kHeight,
    autoscroll = 'none',
    tip = "load a txt file of gene lists to plot sub set"
  )
tkgrid(listbox_tool_on, columnspan = 3)
tkbind(listbox_tool_on, "<Double-ButtonPress-1>", function()
  handleColorSel(listbox_tool_on, 'tool'))
tkbind(listbox_tool_on, "<<ListboxSelect>>", function()
  SelectionControl('tool',  '_off'))

tkgrid(
  tk2button(
    frame_tool_tab,
    text = "<<Switch>>",
    command = function()
      MoveSelectToOtherEntry(listbox_tool_on, listbox_tool_off, 'tool')
  ),
  tk2button(
    frame_tool_tab,
    text = "<<All On>>",
    command = function()
      MoveAllToOtherEntry(listbox_tool_on, listbox_tool_off, "on", file_name = "tool")
  ),
  tk2button(
    frame_tool_tab,
    text = "<<All Off>>",
    command = function()
      MoveAllToOtherEntry(listbox_tool_on, listbox_tool_off, "off", file_name = "tool")
  ),
  sticky = 'we',
  padx = c(2, 2)
)
listbox_tool_off <-
  tk2listbox(
    frame_tool_tab,
    width = kWidth + 8,
    height = kHeight,
    autoscroll = 'none',
    tip = "files in this list wont show up on plot"
  )
tkgrid(listbox_tool_off, columnspan = 3)
tkbind(listbox_tool_off, "<Double-ButtonPress-1>", function()
  handleColorSel(listbox_tool_off, 'tool'))
tkbind(listbox_tool_off, "<<ListboxSelect>>", function()
  SelectionControl('tool',  '_on'))

tkgrid(
  tk2button(
    frame_tool_tab,
    text = " Grab tool list ",
    command =  function()
      ToolListTabHelper()
  ),
  row = 7,
  columnspan = 2,
  sticky = 'w',
  padx = c(3, 0)
)
tkgrid(
  tk2button(
    frame_tool_tab,
    text = " Intersect list ",
    command =  function()
      IntersectGeneLists(LIST_DATA$gene_file$tool$use, tclvalue(tcl_toolfile))
  ),
  row = 7,
  column = 1,
  columnspan = 2,
  sticky = 'e',
  padx = c(0, 3)
)
tkgrid(frame_tool_tab)
tkpack.configure(frame_tool_tab, fill = "both", expand = 1)

# LIST grid ----

tkgrid(notebook_list,
       column = 0,
       row = 3,
       sticky = 'nsew')

# Left half grid ----

tkgrid(left_frame,
       column = 0,
       row = 0,
       sticky = 'nsew')
tkgrid.columnconfigure(root, left_frame, weight = 1)
tkgrid.rowconfigure(root, left_frame, weight = 5)

# TOOL frame for finding gene lists ----

frame_tool <- tkframe(root)

frame_notebook_tool <- tkframe(frame_tool)

notebook_tool <-
  tk2notebook(
    frame_notebook_tool,
    tabs = c(
      "Sort\nTool",
      "Peaks\nTool",
      "Ratios\nTool",
      "Cluster\nTool",
      "Intersect\nTool",
      "CDF\nTool"
    )
  )
tkgrid(notebook_tool)

# TOOL: tab Sort tool ----

notebook_sort_tab <- tk2notetab(notebook_tool, "Sort\nTool")

frame_sort_tab <- tkframe(notebook_sort_tab,
                          relief = 'ridge', borderwidth = 5)

label_active_sort <-
  tklabel(frame_sort_tab, text = "Active table files")
tkgrid(label_active_sort, columnspan = 3)

listbox_active_sort <-
  tk2listbox(
    frame_sort_tab,
    height = kHeight + 1,
    width = kWidth + 8,
    autoscroll = 'none',
    tip = "activate file(s) to use tool"
  )
tkgrid(listbox_active_sort, columnspan = 3)

tkgrid(
  tk2button(
    frame_sort_tab,
    text = " Activate file(s) ",
    command = function()
      ActLst(
        listbox_active_sort,
        0,
        "listbox_active_gene_sort",
        "label_active_sort_length"
      )
  ),
  tk2button(
    frame_sort_tab,
    text = "Clear file(s)",
    command = function()
      DActLst(
        listbox_active_sort,
        "listbox_active_gene_sort",
        "label_active_sort_length"
        ,
        test = 1
      )
  ),
  padx = c(20, 20)
)

frame_sort_tab_buttons <- tkframe(frame_sort_tab, relief = 'ridge',
                                  borderwidth = 5)

tkgrid(tklabel(frame_sort_tab_buttons, text = "Sort tools"),
       columnspan = 4)

tkgrid(
  tk2combobox(
    frame_sort_tab_buttons,
    values =  kTopBottomOptions,
    textvariable = tcl_top_bottom_option,
    state = "readonly",
    width = 8
  ),
  sticky = "w",
  columnspan = 2,
  column = 0,
  row = 1,
  padx = c(20, 0)
)

tkgrid(
  tk2combobox(
    frame_sort_tab_buttons,
    values =  kTopBottomNum,
    textvariable = tcl_top_bottom_num,
    state = "readonly",
    width = 3
  ),
  sticky = "w",
  columnspan = 2,
  column = 2,
  row = 1,
  padx = c(0, 10),
  pady = c(10, 10)
)
tkgrid(
  tklabel(frame_sort_tab_buttons, text = "Separated by"),
  column = 4,
  row = 1,
  padx = c(0, 0),
  pady = c(10, 10)
)
tkgrid(
  entry_dist_span <- tk2entry(frame_sort_tab_buttons, width = 4,
                              tip = "enter number of bp to remove overlapping genes"),
  column = 5,
  row = 1,
  sticky = 'e',
  pady = c(10, 10)
)
tkgrid(
  tklabel(frame_sort_tab_buttons, text = "bins"),
  padx = c(20, 0),
  column = 0,
  row = 3,
  sticky = "e"
)
combobox_bin_start_sort <- tk2combobox(
  frame_sort_tab_buttons,
  textvariable = tcl_bin_start_sort,
  state = "readonly",
  width = 3
)
tkgrid(
  combobox_bin_start_sort,
  padx = c(0, 0),
  column = 1,
  row = 3,
  sticky = "e"
)
tkbind(combobox_bin_start_sort, "<<ComboboxSelected>>", function()
  BinStartEndHelper(
    tcl_bin_start_sort,
    tcl_bin_end_sort,
    combobox_bin_start_sort,
    combobox_bin_end_sort,
    1
  ))

tkgrid(
  tklabel(frame_sort_tab_buttons, text = "to"),
  column = 2,
  row = 3,
  padx = c(0, 0),
  sticky = "w"
)

combobox_bin_end_sort <- tk2combobox(
  frame_sort_tab_buttons,
  textvariable = tcl_bin_end_sort,
  state = "readonly",
  width = 3
)
tkgrid(
  combobox_bin_end_sort,
  column = 3,
  row = 3,
  padx = c(0, 10),
  sticky = "w"
)
tkbind(combobox_bin_end_sort, "<<ComboboxSelected>>", function()
  BinStartEndHelper(
    tcl_bin_start_sort,
    tcl_bin_end_sort,
    combobox_bin_start_sort,
    combobox_bin_end_sort,
    2
  ))


tkgrid(
  tk2button(
    frame_sort_tab_buttons,
    text = "   Sort   ",
    command =  function()
      SortTop()
  ),
  row = 4,
  columnspan = 2,
  sticky = "e",
  pady = c(10, 10),
  padx = c(10, 0)
)

tkgrid(
  tk2combobox(
    frame_sort_tab_buttons,
    values =  kAccDec,
    textvariable = tcl_acc_dec,
    state = "readonly",
    width = 7
  ),
  sticky = "w",
  columnspan = 2,
  column = 2,
  row = 4,
  padx = c(0, 5),
  pady = c(10, 10)
)



tkgrid(frame_sort_tab_buttons,
       columnspan = 4,
       sticky = 'nswe')

label_active_sort_length <- tklabel(frame_sort_tab, text = "n = 0")
tkgrid(label_active_sort_length, columnspan = 3)

listbox_active_gene_sort <-
  tk2listbox(
    frame_sort_tab,
    height = kHeight + 7,
    width = kWidth + 8,
    selectmode = "extended",
    autoscroll = 'none'
  )
tkgrid(listbox_active_gene_sort,
       columnspan = 3,
       pady = c(5, 5))

tkgrid(
  tk2button(
    frame_sort_tab,
    text = "plot list",
    command = function()
      GeneListPlotHelper(listbox_active_gene_sort),
    width = 15
  ),
  column = 0,
  row = 7,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_sort_tab,
    text = "plot selected",
    command = function()
      SelectGeneListPlotHelper(listbox_active_gene_sort),
    width = 15
  ),
  column = 1,
  row = 7,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_sort_tab,
    text = "Intersect list",
    command = function()
      IntersectGeneLists(as.character(
        tkget(listbox_active_gene_sort, 0, 'end')
      ),
      "sort tool"),
    width = 15
  ),
  column = 0,
  row = 8,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_sort_tab,
    text = "Save list",
    command = function()
      SaveGenelist(listbox_active_gene_sort, listbox_active_sort, "sort"),
    width = 15
  ),
  column = 1,
  row = 8,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_sort_tab,
    text = "Search list",
    command = function()
      FindGene(entry_sort_search, listbox_active_gene_sort),
    width = 15
  ),
  column = 0,
  row = 9,
  pady = c(10, 10)
)

tkgrid(
  entry_sort_search <-
    tk2entry(frame_sort_tab, tip = "Enter gene name to search", width = 20),
  column = 1,
  row = 9,
  pady = c(10, 10)
)

tkgrid(frame_sort_tab)
tkpack.configure(frame_sort_tab, fill = 'both', expand = 1)

#TOOL: tab peak tool  ----

notebook_peaks_tab <- tk2notetab(notebook_tool, "Peaks\nTool")

frame_peaks_tab <- tkframe(notebook_peaks_tab,
                           relief = 'ridge',
                           borderwidth = 5)

label_active_peaks <-
  tklabel(frame_peaks_tab, text = "No Active table files")
tkgrid(label_active_peaks, columnspan = 3)

listbox_active_peaks <-
  tk2listbox(
    frame_peaks_tab,
    height = 4,
    width = kWidth + 8,
    tip = "activate 4 files to use tool",
    scroll = 'x',
    autoscroll = 'none'
  )
tkgrid(listbox_active_peaks, columnspan = 3)

tkgrid(
  tk2button(
    frame_peaks_tab,
    text = "Activate file(s)",
    command = function()
      ActLst(
        listbox_active_peaks,
        5,
        sapply(c(1:4), function(x) {
          paste("listbox_gene_peaks_list" , x, sep = "")
        }),
        sapply(c(1:4), function(x) {
          paste("label_peaks_list" , x, sep = "")
        })
      )
  ),
  tk2button(
    frame_peaks_tab,
    text = "Clear file(s)",
    command = function()
      DActLst(
        listbox_active_peaks,
        sapply(c(1:4), function(x) {
          paste("listbox_gene_peaks_list" , x, sep = "")
        }),
        sapply(c(1:4), function(x) {
          paste("label_peaks_list" , x, sep = "")
        })
        ,
        test = 1
      )
  )
)

frame_peaks_tab_buttons <-
  tkframe(frame_peaks_tab, relief = 'ridge',
          borderwidth = 5)

tkgrid(
  tklabel(frame_peaks_tab_buttons, text = "peaks tools"),
  columnspan = 4,
  padx = c(50, 0)
)

tkgrid(
  tklabel(frame_peaks_tab_buttons, text = "bins"),
  padx = c(10, 0),
  column = 0,
  row = 2,
  sticky = "e"
)

combobox_bin_start_peaks <- tk2combobox(
  frame_peaks_tab_buttons,
  textvariable = tcl_bin_start_peaks,
  state = "readonly",
  width = 3
)
tkgrid(
  combobox_bin_start_peaks,
  padx = c(0, 0),
  column = 1,
  row = 2,
  sticky = "w"
)
tkbind(combobox_bin_start_peaks, "<<ComboboxSelected>>", function()
  BinStartEndHelper(
    tcl_bin_start_peaks,
    tcl_bin_end_peaks,
    combobox_bin_start_peaks,
    combobox_bin_end_peaks,
    1
  ))

tkgrid(
  tklabel(frame_peaks_tab_buttons, text = "to"),
  column = 2,
  row = 2,
  padx = c(0, 0),
  sticky = "w"
)
combobox_bin_end_peaks <- tk2combobox(
  frame_peaks_tab_buttons,
  textvariable = tcl_bin_end_peaks,
  state = "readonly",
  width = 3
)
tkgrid(
  combobox_bin_end_peaks,
  column = 3,
  row = 2,
  padx = c(0, 0),
  sticky = "w"
)
tkbind(combobox_bin_end_peaks, "<<ComboboxSelected>>", function()
  BinStartEndHelper(
    tcl_bin_start_peaks,
    tcl_bin_end_peaks,
    combobox_bin_start_peaks,
    combobox_bin_end_peaks,
    2
  ))

tkgrid(
  tk2button(
    frame_peaks_tab_buttons,
    text = " find peaks ",
    command =  function()
      TssMaxPeaks()
  ),
  column = 1,
  row = 3,
  columnspan = 3,
  pady = c(5, 5),
  padx = c(10, 0)
)


tkgrid(
  tklabel(frame_peaks_tab_buttons, text = "width"),
  column = 2,
  row = 4,
  padx = c(0, 0),
  sticky = "w"
)
combobox_bin_width_peaks <- tk2combobox(
  frame_peaks_tab_buttons,
  textvariable = tcl_bin_center_width_range,
  state = "readonly",
  width = 3
)
tkgrid(
  combobox_bin_width_peaks,
  column = 3,
  row = 4,
  padx = c(0, 0),
  sticky = "w"
)


tkgrid(
  tk2button(
    frame_peaks_tab_buttons,
    text = " peak quartile ",
    command =  function()
      SortFindMaxPeaks()
  ),
  column = 0,
  row = 4,
  columnspan = 2,
  pady = c(5, 5),
  padx = c(10, 0)
)




tkgrid(frame_peaks_tab_buttons,
       columnspan = 4,
       sticky = 'nsew')

notebook_peaks <- tk2notebook(frame_peaks_tab,
                              tabs = c("list 1", "list 2",
                                       "list 3", "list 4"))
tkgrid(notebook_peaks, columnspan = 4)

notebook_peaks_list1_tab <- tk2notetab(notebook_peaks, "list 1")


frame_peaks_notbook_list1 <- tkframe(notebook_peaks_list1_tab)

tkgrid(label_peaks_list1 <-
         tklabel(frame_peaks_notbook_list1, text = "n = "),
       columnspan = 3)

tkgrid(
  listbox_gene_peaks_list1 <-
    tk2listbox(
      frame_peaks_notbook_list1,
      height = kHeight + 7,
      width = kWidth + 8,
      selectmode = "extended",
      autoscroll = 'none'
    ),
  columnspan = 3,
  pady = c(5, 5)
)
tkgrid(frame_peaks_notbook_list1)

notebook_peaks_list2_tab <- tk2notetab(notebook_peaks, "list 2")


frame_peaks_notbook_list2 <- tkframe(notebook_peaks_list2_tab)

tkgrid(label_peaks_list2 <-
         tklabel(frame_peaks_notbook_list2, text = "n = "),
       columnspan = 3)

tkgrid(
  listbox_gene_peaks_list2 <-
    tk2listbox(
      frame_peaks_notbook_list2,
      height = kHeight + 7,
      width = kWidth + 8,
      selectmode = "extended",
      autoscroll = 'none'
    ),
  columnspan = 3,
  pady = c(5, 5)
)
tkgrid(frame_peaks_notbook_list2)

notebook_peaks_list3_tab <- tk2notetab(notebook_peaks, "list 3")

frame_peaks_notbook_list3 <- tkframe(notebook_peaks_list3_tab)

tkgrid(label_peaks_list3 <-
         tklabel(frame_peaks_notbook_list3, text = "n = "),
       columnspan = 3)

tkgrid(
  listbox_gene_peaks_list3 <-
    tk2listbox(
      frame_peaks_notbook_list3,
      height = kHeight + 7,
      width = kWidth + 8,
      selectmode = "extended",
      autoscroll = 'none'
    ),
  columnspan = 3,
  pady = c(5, 5)
)
tkgrid(frame_peaks_notbook_list3)

notebook_peaks_list4_tab <- tk2notetab(notebook_peaks, "list 4")

frame_peaks_notbook_list4 <- tkframe(notebook_peaks_list4_tab)

tkgrid(label_peaks_list4 <-
         tklabel(frame_peaks_notbook_list4, text = "n = "),
       columnspan = 3)

tkgrid(
  listbox_gene_peaks_list4 <-
    tk2listbox(
      frame_peaks_notbook_list4,
      height = kHeight + 7,
      width = kWidth + 8,
      selectmode = "extended",
      autoscroll = 'none'
    ),
  columnspan = 3,
  pady = c(5, 5)
)
tkgrid(frame_peaks_notbook_list4)

tkgrid(
  tk2button(
    frame_peaks_tab,
    text = "plot list",
    command = function()
      GeneListPlotHelper("peaks"),
    width = 15
  ),
  column = 0,
  row = 7,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_peaks_tab,
    text = "plot selected",
    command = function()
      SelectGeneListPlotHelper("peaks"),
    width = 15
  ),
  column = 1,
  row = 7,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_peaks_tab,
    text = "Intersect list",
    command = function()
      IntersectGeneLists(as.character(tkget(
        ListBoxSelectHelper("peaks"), 0, 'end'
      )),
      paste(
        "peaks-", tk2notetab.text(notebook_peaks)
      ))
    ,
    width = 15
  ),
  column = 0,
  row = 8,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_peaks_tab,
    text = "Save list",
    command = function()
      SaveGenelist('peaks', listbox_active_peaks, "peaks"),
    width = 15
  ),
  column = 1,
  row = 8,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_peaks_tab,
    text = "Search list",
    command = function()
      FindGene(entry_peaks_search, 'peaks'),
    width = 15
  ),
  column = 0,
  row = 9,
  pady = c(10, 10)
)

tkgrid(
  entry_peaks_search <-
    tk2entry(frame_peaks_tab, tip = "Enter gene name to search", width = 20),
  column = 1,
  row = 9,
  pady = c(10, 10)
)

tkgrid(frame_peaks_tab)
tkpack.configure(frame_peaks_tab, fill = 'both', expand = 1)


# TOOL: tab Ratios ----

notebook_ratios_tab <- tk2notetab(notebook_tool, "Ratios\nTool")

frame_ratios_tab <- tkframe(notebook_ratios_tab,
                            relief = 'ridge',
                            borderwidth = 5)

label_active_ratios <-
  tklabel(frame_ratios_tab, text = "No Active table files")
tkgrid(label_active_ratios, columnspan = 3)

listbox_active_ratios <-
  tk2listbox(
    frame_ratios_tab,
    height = 2,
    width = kWidth + 10,
    scroll = 'x',
    autoscroll = 'none'
  )
tkgrid(listbox_active_ratios, columnspan = 3)

tkgrid(
  tk2button(
    frame_ratios_tab,
    text = "Activate file(s)",
    command = function()
      ActLst(
        listbox_active_ratios,
        3,
        sapply(c("up", "down", "between"), function(x) {
          paste("listbox_gene_ratios_" , x, sep = "")
        }),
        sapply(c("up", "down", "between"), function(x) {
          paste("label_ratios_" , x, sep = "")
        })
      )
  ),
  tk2button(
    frame_ratios_tab,
    text = "Clear file(s)",
    command = function()
      DActLst(
        listbox_active_ratios,
        sapply(c("up", "down", "between"), function(x) {
          paste("listbox_gene_ratios_" , x, sep = "")
        }),
        sapply(c("up", "down", "between"), function(x) {
          paste("label_ratios_" , x, sep = "")
        })
        ,
        test = 1
      )
  )
)

frame_ratios_tab_buttons <-
  tkframe(frame_ratios_tab, relief = 'ridge',
          borderwidth = 5)

tkgrid(
  tklabel(frame_ratios_tab_buttons, text = "ratios tools"),
  columnspan = 4,
  padx = c(50, 0)
)

tkgrid(
  tklabel(frame_ratios_tab_buttons,
          text = "fold change"),
  sticky = "w",
  columnspan = 2,
  column = 0,
  row = 1,
  padx = c(50, 0)
)

tkgrid(
  tk2combobox(
    frame_ratios_tab_buttons,
    values =  kFoldList,
    textvariable = tcl_bin_fold_ratios,
    state = "readonly",
    width = 3
  ),
  sticky = "we",
  columnspan = 2,
  column = 2,
  row = 1,
  padx = c(0, 5),
  pady = c(10, 10)
)

tkgrid(
  tklabel(frame_ratios_tab_buttons, text = "bins"),
  padx = c(50, 0),
  column = 0,
  row = 3,
  sticky = "e"
)

combobox_bin_start1_ratios <- tk2combobox(
  frame_ratios_tab_buttons,
  textvariable = tcl_bin_start1_ratios,
  state = "readonly",
  width = 3
)
tkgrid(
  combobox_bin_start1_ratios,
  padx = c(0, 0),
  column = 1,
  row = 3,
  sticky = "w"
)
tkbind(combobox_bin_start1_ratios, "<<ComboboxSelected>>", function()
  BinStartEndHelper(
    tcl_bin_start1_ratios,
    tcl_bin_end1_ratios,
    combobox_bin_start1_ratios,
    combobox_bin_end1_ratios,
    1
  ))

tkgrid(
  tklabel(frame_ratios_tab_buttons, text = "to"),
  column = 2,
  row = 3,
  padx = c(0, 0),
  sticky = "w"
)
combobox_bin_end1_ratios <- tk2combobox(
  frame_ratios_tab_buttons,
  textvariable = tcl_bin_end1_ratios,
  state = "readonly",
  width = 3
)
tkgrid(
  combobox_bin_end1_ratios,
  column = 3,
  row = 3,
  padx = c(0, 0),
  sticky = "w"
)
tkbind(combobox_bin_end1_ratios, "<<ComboboxSelected>>", function()
  BinStartEndHelper(
    tcl_bin_start1_ratios,
    tcl_bin_end1_ratios,
    combobox_bin_start1_ratios,
    combobox_bin_end1_ratios,
    2
  ))

tkgrid(
  tklabel(frame_ratios_tab_buttons, text = " / "),
  padx = c(0, 0),
  column = 4,
  row = 3,
  sticky = "w"
)

tkgrid(
  tklabel(frame_ratios_tab_buttons, text = "bins"),
  padx = c(50, 0),
  column = 0,
  row = 4,
  sticky = "e"
)

combobox_bin_start2_ratios <- tk2combobox(
  frame_ratios_tab_buttons,
  textvariable = tcl_bin_start2_ratios,
  state = "readonly",
  width = 3
)
tkgrid(
  combobox_bin_start2_ratios,
  padx = c(0, 0),
  column = 1,
  row = 4,
  sticky = "w"
)
tkbind(combobox_bin_start2_ratios, "<<ComboboxSelected>>", function()
  BinStartEndHelper(
    tcl_bin_start2_ratios,
    tcl_bin_end2_ratios,
    combobox_bin_start2_ratios,
    combobox_bin_end2_ratios,
    1
  ))


tkgrid(
  tklabel(frame_ratios_tab_buttons, text = "to"),
  column = 2,
  row = 4,
  padx = c(0, 0),
  sticky = "w"
)
combobox_bin_end2_ratios <- tk2combobox(
  frame_ratios_tab_buttons,
  textvariable = tcl_bin_end2_ratios,
  state = "readonly",
  width = 3,
  tip = "can't be less then start, won't change if start is 0"
)
tkgrid(
  combobox_bin_end2_ratios,
  column = 3,
  row = 4,
  padx = c(0, 0),
  sticky = "w"
)
tkbind(combobox_bin_end2_ratios, "<<ComboboxSelected>>", function()
  BinStartEndHelper(
    tcl_bin_start2_ratios,
    tcl_bin_end2_ratios,
    combobox_bin_start2_ratios,
    combobox_bin_end2_ratios,
    2
  ))

tkgrid(
  tk2button(
    frame_ratios_tab_buttons,
    text = "   ratios   ",
    command =  function()
      CompareRatios()
  ),
  column = 0,
  row = 5,
  columnspan = 4,
  pady = c(10, 10),
  padx = c(50, 0)
)

tkgrid(frame_ratios_tab_buttons,
       columnspan = 4,
       sticky = 'nsew')

notebook_ratios <- tk2notebook(frame_ratios_tab,
                               tabs = c("File 1 up", "File 2 up",
                                        "Fold inbetween"))
tkgrid(notebook_ratios, columnspan = 4)

notebook_ratios_up_tab <- tk2notetab(notebook_ratios, "File 1 up")


frame_ratios_notbook_up <- tkframe(notebook_ratios_up_tab)

tkgrid(label_ratios_up <-
         tklabel(frame_ratios_notbook_up, text = "n = "),
       columnspan = 3)

tkgrid(
  listbox_gene_ratios_up <-
    tk2listbox(
      frame_ratios_notbook_up,
      height = kHeight + 7,
      width = kWidth + 8,
      selectmode = "extended",
      autoscroll = 'none'
    ),
  columnspan = 3,
  pady = c(5, 5)
)
tkgrid(frame_ratios_notbook_up)

notebook_ratios_down_tab <- tk2notetab(notebook_ratios, "File 2 up")


frame_ratios_notbook_down <- tkframe(notebook_ratios_down_tab)

tkgrid(label_ratios_down <-
         tklabel(frame_ratios_notbook_down, text = "n = "),
       columnspan = 3)

tkgrid(
  listbox_gene_ratios_down <-
    tk2listbox(
      frame_ratios_notbook_down,
      height = kHeight + 7,
      width = kWidth + 8,
      selectmode = "extended",
      autoscroll = 'none'
    ),
  columnspan = 3,
  pady = c(5, 5)
)
tkgrid(frame_ratios_notbook_down)

notebook_ratios_between_tab <-
  tk2notetab(notebook_ratios, "Fold inbetween")

frame_ratios_notbook_between <- tkframe(notebook_ratios_between_tab)

tkgrid(
  label_ratios_between <-
    tklabel(frame_ratios_notbook_between, text = "n = "),
  columnspan = 3
)

tkgrid(
  listbox_gene_ratios_between <-
    tk2listbox(
      frame_ratios_notbook_between,
      height = kHeight + 7,
      width = kWidth + 8,
      selectmode = "extended",
      autoscroll = 'none'
    ),
  columnspan = 3,
  pady = c(5, 5)
)
tkgrid(frame_ratios_notbook_between)

tkgrid(
  tk2button(
    frame_ratios_tab,
    text = "plot list",
    command = function()
      GeneListPlotHelper("ratios"),
    width = 15
  ),
  column = 0,
  row = 7,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_ratios_tab,
    text = "plot selected",
    command = function()
      SelectGeneListPlotHelper("ratios"),
    width = 15
  ),
  column = 1,
  row = 7,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_ratios_tab,
    text = "Intersect list",
    command = function()
      IntersectGeneLists(as.character(tkget(
        ListBoxSelectHelper("ratios"), 0, 'end'
      )),
      paste(
        "ratios-", tk2notetab.text(notebook_ratios)
      )),
    width = 15
  ),
  column = 0,
  row = 8,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_ratios_tab,
    text = "Save list",
    command = function()
      SaveGenelist("ratios", listbox_active_ratios, "ratios"),
    width = 15
  ),
  column = 1,
  row = 8,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_ratios_tab,
    text = "Search list",
    command = function()
      FindGene(entry_ratios_search, "ratios"),
    width = 15
  ),
  column = 0,
  row = 9,
  pady = c(10, 10)
)

tkgrid(
  entry_ratios_search <-
    tk2entry(frame_ratios_tab, tip = "Enter gene name to search", width = 20),
  column = 1,
  row = 9,
  pady = c(10, 10)
)

tkgrid(frame_ratios_tab)
tkpack.configure(frame_ratios_tab, fill = 'both', expand = 1)

# TOOL: tab cluster ----

notebook_cluster_tab <- tk2notetab(notebook_tool, "Cluster\nTool")

frame_cluster_tab <- tkframe(notebook_cluster_tab,
                             relief = 'ridge',
                             borderwidth = 5)

label_active_cluster <-
  tklabel(frame_cluster_tab, text = "No Active table files")
tkgrid(label_active_cluster, columnspan = 3)

listbox_active_cluster <-
  tk2listbox(
    frame_cluster_tab,
    height = 1,
    width = kWidth + 10,
    scroll = 'x',
    autoscroll = 'none'
  )
tkgrid(listbox_active_cluster, columnspan = 3)

tkgrid(
  tk2button(
    frame_cluster_tab,
    text = "Activate file(s)",
    command = function()
      ActLst(
        listbox_active_cluster,
        2,
        sapply(c(1:4), function(x) {
          paste("listbox_gene_cluster_list" , x, sep = "")
        }),
        sapply(c(1:4), function(x) {
          paste("label_cluster_list" , x, sep = "")
        })
      )
  ),
  tk2button(
    frame_cluster_tab,
    text = "Clear file(s)",
    command = function()
      DActLst(
        listbox_active_cluster,
        sapply(c(1:4), function(x) {
          paste("listbox_gene_cluster_list" , x, sep = "")
        }),
        sapply(c(1:4), function(x) {
          paste("label_cluster_list" , x, sep = "")
        })
        ,
        test = 1
      )
  )
)

frame_cluster_tab_buttons <-
  tkframe(frame_cluster_tab, relief = 'ridge',
          borderwidth = 5)

tkgrid(
  tklabel(frame_cluster_tab_buttons, text = "cluster tools"),
  columnspan = 4,
  pady = c(0, 10),
  padx = c(50, 0)
)

tkgrid(
  tklabel(frame_cluster_tab_buttons, text = "bins"),
  padx = c(50, 0),
  column = 0,
  row = 3,
  sticky = "e"
)

combobox_bin_start_cluster <-
  tk2combobox(
    frame_cluster_tab_buttons,
    textvariable = tcl_bin_start_cluster,
    state = "readonly",
    width = 3
  )
tkgrid(
  combobox_bin_start_cluster,
  padx = c(0, 0),
  column = 1,
  row = 3,
  sticky = "w"
)
tkbind(combobox_bin_start_cluster, "<<ComboboxSelected>>", function()
  BinStartEndHelper(
    tcl_bin_start_cluster,
    tcl_bin_end_cluster,
    combobox_bin_start_cluster,
    combobox_bin_end_cluster,
    1
  ))

tkgrid(
  tklabel(frame_cluster_tab_buttons, text = "to"),
  column = 2,
  row = 3,
  padx = c(0, 0),
  sticky = "w"
)

combobox_bin_end_cluster <- tk2combobox(
  frame_cluster_tab_buttons,
  textvariable = tcl_bin_end_cluster,
  state = "readonly",
  width = 3
)
tkgrid(
  combobox_bin_end_cluster,
  column = 3,
  row = 3,
  padx = c(0, 10),
  sticky = "w"
)
tkbind(combobox_bin_end_cluster, "<<ComboboxSelected>>", function()
  BinStartEndHelper(
    tcl_bin_start_cluster,
    tcl_bin_end_cluster,
    combobox_bin_start_cluster,
    combobox_bin_end_cluster,
    2
  ))

tkgrid(
  tk2button(
    frame_cluster_tab_buttons,
    text = "   cluster   ",
    command =  function()
      FindClusters()
  ),
  column = 0,
  row = 4,
  columnspan = 3,
  pady = c(10, 10),
  padx = c(50, 0)
)

combobox_bin_cluster_num <- tk2combobox(
  frame_cluster_tab_buttons,
  textvariable = tcl_bin_cluster_num,
  state = "readonly",
  width = 3
)
tkgrid(
  combobox_bin_cluster_num,
  padx = c(0, 0),
  column = 3,
  row = 4,
  sticky = "w"
)
tkbind(combobox_bin_cluster_num, "<<ComboboxSelected>>", function()
  ClusterNumList(LIST_DATA$clust))
tkgrid(frame_cluster_tab_buttons,
       columnspan = 4,
       sticky = 'nsew')

tkgrid(
  tk2button(
    frame_cluster_tab_buttons,
    text = "   Quartiles   ",
    command =  function()
      FindQuantile()
  ),
  column = 0,
  row = 5,
  columnspan = 4,
  pady = c(5, 5)
)

notebook_cluster <- tk2notebook(frame_cluster_tab,
                                tabs = c("list 1", "list 2",
                                         "list 3", "list 4"))
tkgrid(notebook_cluster, columnspan = 4)

notebook_cluster_list1_tab <- tk2notetab(notebook_cluster, "list 1")


frame_cluster_notbook_list1 <- tkframe(notebook_cluster_list1_tab)

tkgrid(
  label_cluster_list1 <-
    tklabel(frame_cluster_notbook_list1, text = "n = "),
  columnspan = 3
)

tkgrid(
  listbox_gene_cluster_list1 <-
    tk2listbox(
      frame_cluster_notbook_list1,
      height = kHeight + 10,
      width = kWidth + 8,
      selectmode = "extended",
      autoscroll = 'none'
    ),
  columnspan = 3,
  pady = c(5, 5)
)
tkgrid(frame_cluster_notbook_list1)

notebook_cluster_list2_tab <- tk2notetab(notebook_cluster, "list 2")


frame_cluster_notbook_list2 <- tkframe(notebook_cluster_list2_tab)

tkgrid(
  label_cluster_list2 <-
    tklabel(frame_cluster_notbook_list2, text = "n = "),
  columnspan = 3
)

tkgrid(
  listbox_gene_cluster_list2 <-
    tk2listbox(
      frame_cluster_notbook_list2,
      height = kHeight + 10,
      width = kWidth + 8,
      selectmode = "extended",
      autoscroll = 'none'
    ),
  columnspan = 3,
  pady = c(5, 5)
)
tkgrid(frame_cluster_notbook_list2)

notebook_cluster_list3_tab <- tk2notetab(notebook_cluster, "list 3")

frame_cluster_notbook_list3 <- tkframe(notebook_cluster_list3_tab)

tkgrid(
  label_cluster_list3 <-
    tklabel(frame_cluster_notbook_list3, text = "n = "),
  columnspan = 3
)

tkgrid(
  listbox_gene_cluster_list3 <-
    tk2listbox(
      frame_cluster_notbook_list3,
      height = kHeight + 10,
      width = kWidth + 8,
      selectmode = "extended",
      autoscroll = 'none'
    ),
  columnspan = 3,
  pady = c(5, 5)
)
tkgrid(frame_cluster_notbook_list3)

notebook_cluster_list4_tab <- tk2notetab(notebook_cluster, "list 4")

frame_cluster_notbook_list4 <- tkframe(notebook_cluster_list4_tab)

tkgrid(
  label_cluster_list4 <-
    tklabel(frame_cluster_notbook_list4, text = "n = "),
  columnspan = 3
)

tkgrid(
  listbox_gene_cluster_list4 <-
    tk2listbox(
      frame_cluster_notbook_list4,
      height = kHeight + 10,
      width = kWidth + 8,
      selectmode = "extended",
      autoscroll = 'none'
    ),
  columnspan = 3,
  pady = c(5, 5)
)
tkgrid(frame_cluster_notbook_list4)

tkgrid(
  tk2button(
    frame_cluster_tab,
    text = "plot list",
    command = function()
      GeneListPlotHelper("cluster"),
    width = 15
  ),
  column = 0,
  row = 7,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_cluster_tab,
    text = "plot selected",
    command = function()
      SelectGeneListPlotHelper("cluster"),
    width = 15
  ),
  column = 1,
  row = 7,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_cluster_tab,
    text = "Intersect list",
    command = function()
      IntersectGeneLists(as.character(tkget(
        ListBoxSelectHelper("cluster"), 0, 'end'
      )),
      paste(
        "cluster-", tk2notetab.text(notebook_cluster)
      )),
    width = 15
  ),
  column = 0,
  row = 8,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_cluster_tab,
    text = "Save list",
    command = function()
      SaveGenelist("cluster", listbox_active_cluster, STATE[3]),
    width = 15
  ),
  column = 1,
  row = 8,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_cluster_tab,
    text = "Search list",
    command = function()
      FindGene(entry_cluster_search, "cluster"),
    width = 15
  ),
  column = 0,
  row = 9,
  pady = c(10, 10)
)

tkgrid(
  entry_cluster_search <-
    tk2entry(frame_cluster_tab, tip = "Enter gene name to search", width = 20),
  column = 1,
  row = 9,
  pady = c(10, 10)
)

tkgrid(frame_cluster_tab)
tkpack.configure(frame_cluster_tab, fill = 'both', expand = 1)

# TOOL: tab Intersect ----

notebook_intersect_tab <-
  tk2notetab(notebook_tool, "Intersect\nTool")

frame_intersect_tab <- tkframe(notebook_intersect_tab,
                               relief = 'ridge',
                               borderwidth = 5)

label_active_intersect <-
  tklabel(frame_intersect_tab, text = "Active table files")
tkgrid(label_active_intersect, columnspan = 3)

listbox_active_intersect <-
  tk2listbox(
    frame_intersect_tab,
    height = kHeight + 1,
    width = kWidth + 8,
    autoscroll = 'none',
    tip = "activate file(s) to use tool"
  )
tkgrid(listbox_active_intersect, columnspan = 3)

tkgrid(
  tk2button(
    frame_intersect_tab,
    text = "Clear file(s)",
    command = function()
      DActLst(
        listbox_active_intersect,
        c(
          "listbox_intersect_inclusive",
          "listbox_intersect_exclusive",
          "listbox_intersect_combind"
        ),
        c(
          "label_intersect_inclusive",
          "label_intersect_exclusive",
          "label_intersect_combind"
        )
        ,
        test = 1
      )
  ),
  columnspan = 4
)

frame_intersect_tab_buttons <-
  tkframe(frame_intersect_tab,
          relief = 'ridge',
          borderwidth = 5)

tkgrid(tklabel(frame_intersect_tab_buttons, text = "Intersect tools"),
       padx = c(20, 20))

tkgrid(
  tk2button(
    frame_intersect_tab_buttons,
    text = "   load file   ",
    command =  function()
      IntersectLoadFile()
  ),
  pady = c(10, 10),
  padx = c(20, 20)
)

tkgrid(frame_intersect_tab_buttons,
       columnspan = 2,
       sticky = 'nsew')

notebook_intersect <- tk2notebook(frame_intersect_tab,
                                  tabs = c("Inclusive", "Exclusive", "Combind"))
tkgrid(notebook_intersect, columnspan = 4)

notebook_intersect_inclusive_tab <-
  tk2notetab(notebook_intersect, "Inclusive")


frame_intersect_inclusive <-
  tkframe(notebook_intersect_inclusive_tab)

tkgrid(
  label_intersect_inclusive <-
    tklabel(frame_intersect_inclusive, text = "n = "),
  columnspan = 3
)

tkgrid(
  listbox_intersect_inclusive <-
    tk2listbox(
      frame_intersect_inclusive,
      height = kHeight + 10,
      width = kWidth + 8,
      selectmode = "extended",
      autoscroll = 'none'
    ),
  columnspan = 3,
  pady = c(5, 5)
)
tkgrid(frame_intersect_inclusive)

notebook_intersect_exclusive_tab <-
  tk2notetab(notebook_intersect, "Exclusive")


frame_intersect_exclusive <-
  tkframe(notebook_intersect_exclusive_tab)

tkgrid(
  label_intersect_exclusive <-
    tklabel(frame_intersect_exclusive, text = "n = "),
  columnspan = 3
)

tkgrid(
  listbox_intersect_exclusive <-
    tk2listbox(
      frame_intersect_exclusive,
      height = kHeight + 10,
      width = kWidth + 8,
      selectmode = "extended",
      autoscroll = 'none'
    ),
  columnspan = 3,
  pady = c(5, 5)
)
tkgrid(frame_intersect_exclusive)

notebook_intersect_combind_tab <-
  tk2notetab(notebook_intersect, "Combind")


frame_intersect_combind <- tkframe(notebook_intersect_combind_tab)

tkgrid(
  label_intersect_combind <-
    tklabel(frame_intersect_combind, text = "n = "),
  columnspan = 3
)

tkgrid(
  listbox_intersect_combind <-
    tk2listbox(
      frame_intersect_combind,
      height = kHeight + 10,
      width = kWidth + 8,
      selectmode = "extended",
      autoscroll = 'none'
    ),
  columnspan = 3,
  pady = c(5, 5)
)
tkgrid(frame_intersect_combind)

tkgrid(
  tk2button(
    frame_intersect_tab,
    text = "plot list",
    command = function()
      GeneListPlotHelper("intersect"),
    width = 15
  ),
  column = 0,
  row = 7,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_intersect_tab,
    text = "plot selected",
    command = function()
      SelectGeneListPlotHelper("intersect"),
    width = 15
  ),
  column = 1,
  row = 7,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_intersect_tab,
    text = "Save list",
    command = function()
      SaveGenelist("intersect", listbox_active_intersect, "intersect"),
    width = 15
  ),
  column = 1,
  row = 8,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_intersect_tab,
    text = "Search list",
    command = function()
      FindGene(entry_intersect_search, "intersect"),
    width = 15
  ),
  column = 0,
  row = 9,
  pady = c(10, 10)
)

tkgrid(
  entry_intersect_search <-
    tk2entry(frame_intersect_tab, tip = "Enter gene name to search", width = 20),
  column = 1,
  row = 9,
  pady = c(10, 10)
)

tkgrid(frame_intersect_tab)
tkpack.configure(frame_intersect_tab, fill = 'both', expand = 1)

# TOOL: tab Cumulative Distribution  ----

notebook_cdf_tab <- tk2notetab(notebook_tool, "CDF\nTool")

frame_cdf_tab <- tkframe(notebook_cdf_tab,
                         relief = 'ridge', borderwidth = 5)

label_active_cdf <-
  tklabel(frame_cdf_tab, text = "No Active table files")
tkgrid(label_active_cdf, columnspan = 3)

listbox_active_cdf <-
  tk2listbox(
    frame_cdf_tab,
    height = 4,
    width = kWidth + 8,
    tip = "activate 4 files to use tool",
    scroll = 'x',
    autoscroll = 'none'
  )
tkgrid(listbox_active_cdf, columnspan = 3)

tkgrid(
  tk2button(
    frame_cdf_tab,
    text = "Activate file(s)",
    command = function()
      ActLst(
        listbox_active_cdf,
        5,
        sapply(c(1:4), function(x) {
          paste("listbox_gene_cdf_list" , x, sep = "")
        }),
        sapply(c(1:4), function(x) {
          paste("label_cdf_list" , x, sep = "")
        })
      )
  ),
  tk2button(
    frame_cdf_tab,
    text = "Clear file(s)",
    command = function()
      DActLst(
        listbox_active_cdf,
        sapply(c(1:4), function(x) {
          paste("listbox_gene_cdf_list" , x, sep = "")
        }),
        sapply(c(1:4), function(x) {
          paste("label_cdf_list" , x, sep = "")
        })
        ,
        test = 1
      )
  )
)

frame_cdf_tab_buttons <- tkframe(frame_cdf_tab, relief = 'ridge',
                                 borderwidth = 5)

tkgrid(
  tklabel(frame_cdf_tab_buttons, text = "cdf tools"),
  columnspan = 4,
  padx = c(50, 0)
)

tkgrid(
  tklabel(frame_cdf_tab_buttons, text = "bins"),
  padx = c(10, 0),
  column = 0,
  row = 3,
  sticky = "e"
)

combobox_bin_start1_cdf <- tk2combobox(
  frame_cdf_tab_buttons,
  textvariable = tcl_bin_start1_cdf,
  state = "readonly",
  width = 3
)
tkgrid(
  combobox_bin_start1_cdf,
  padx = c(0, 0),
  column = 1,
  row = 3,
  sticky = "w"
)
tkbind(combobox_bin_start1_cdf, "<<ComboboxSelected>>", function()
  BinStartEndHelper(
    tcl_bin_start1_cdf,
    tcl_bin_end1_cdf,
    combobox_bin_start1_cdf,
    combobox_bin_end1_cdf,
    1
  ))

tkgrid(
  tklabel(frame_cdf_tab_buttons, text = "to"),
  column = 2,
  row = 3,
  padx = c(0, 0),
  sticky = "w"
)
combobox_bin_end1_cdf <- tk2combobox(
  frame_cdf_tab_buttons,
  textvariable = tcl_bin_end1_cdf,
  state = "readonly",
  width = 3
)
tkgrid(
  combobox_bin_end1_cdf,
  column = 3,
  row = 3,
  padx = c(0, 0),
  sticky = "w"
)
tkbind(combobox_bin_end1_cdf, "<<ComboboxSelected>>", function()
  BinStartEndHelper(
    tcl_bin_start1_cdf,
    tcl_bin_end1_cdf,
    combobox_bin_start1_cdf,
    combobox_bin_end1_cdf,
    2
  ))

tkgrid(
  tklabel(frame_cdf_tab_buttons, text = " / "),
  padx = c(0, 0),
  column = 4,
  row = 3,
  sticky = "w"
)

tkgrid(
  tklabel(frame_cdf_tab_buttons, text = "bins"),
  padx = c(10, 0),
  column = 0,
  row = 4,
  sticky = "e"
)

combobox_bin_start2_cdf <- tk2combobox(
  frame_cdf_tab_buttons,
  textvariable = tcl_bin_start2_cdf,
  state = "readonly",
  width = 3
)
tkgrid(
  combobox_bin_start2_cdf,
  padx = c(0, 0),
  column = 1,
  row = 4,
  sticky = "w"
)
tkbind(combobox_bin_start2_cdf, "<<ComboboxSelected>>", function()
  BinStartEndHelper(
    tcl_bin_start2_cdf,
    tcl_bin_end2_cdf,
    combobox_bin_start2_cdf,
    combobox_bin_end2_cdf,
    1
  ))


tkgrid(
  tklabel(frame_cdf_tab_buttons, text = "to"),
  column = 2,
  row = 4,
  padx = c(0, 0),
  sticky = "w"
)
combobox_bin_end2_cdf <- tk2combobox(
  frame_cdf_tab_buttons,
  textvariable = tcl_bin_end2_cdf,
  state = "readonly",
  width = 3
)
tkgrid(
  combobox_bin_end2_cdf,
  column = 3,
  row = 4,
  padx = c(0, 0),
  sticky = "w"
)
tkbind(combobox_bin_end2_cdf, "<<ComboboxSelected>>", function()
  BinStartEndHelper(
    tcl_bin_start2_cdf,
    tcl_bin_end2_cdf,
    combobox_bin_start2_cdf,
    combobox_bin_end2_cdf,
    2
  ))

tkgrid(
  tk2button(
    frame_cdf_tab_buttons,
    text = "   cdf plot  ",
    command =  function()
      CumulativeDistribution()
  ),
  column = 0,
  row = 5,
  columnspan = 2,
  pady = c(5, 5),
  padx = c(10, 0)
)

tkgrid(
  tk2combobox(
    frame_cdf_tab_buttons,
    values =  kAccDec,
    textvariable = tcl_acc_dec_cdf,
    state = "readonly",
    width = 3
  ),
  sticky = "w",
  columnspan = 2,
  column = 1,
  row = 5,
  padx = c(55, 5),
  pady = c(5, 5)
)

tkgrid(
  tkcheckbutton(frame_cdf_tab_buttons, variable = tcl_checkbox_cdf_innerjoin, text = "merge lists?"),
  columnspan = 2,
  column = 3,
  row = 5
)

tkgrid(
  tklabel(frame_cdf_tab_buttons, text = "rm low hi"),
  padx = c(10, 0),
  column = 0,
  row = 6,
  sticky = "w"
)

tkgrid(
  tk2combobox(
    frame_cdf_tab_buttons,
    values =  kQuntile,
    textvariable = tcl_quntile_cdf_bottom,
    state = "readonly",
    width = 4
  ),
  sticky = "w",
  column = 1,
  row = 6,
  padx = c(0, 5),
  pady = c(5, 5)
)

tkgrid(
  tk2combobox(
    frame_cdf_tab_buttons,
    values =  kQuntile,
    textvariable = tcl_quntile_cdf_top,
    state = "readonly",
    width = 4
  ),
  sticky = "w",
  columnspan = 3,
  column = 2,
  row = 6,
  padx = c(0, 5),
  pady = c(5, 5)
)

tkgrid(frame_cdf_tab_buttons,
       columnspan = 4,
       sticky = 'nsew')

notebook_cdf <- tk2notebook(frame_cdf_tab,
                            tabs = c("list 1", "list 2",
                                     "list 3", "list 4"))
tkgrid(notebook_cdf, columnspan = 4)

notebook_cdf_list1_tab <- tk2notetab(notebook_cdf, "list 1")


frame_cdf_notbook_list1 <- tkframe(notebook_cdf_list1_tab)

tkgrid(label_cdf_list1 <-
         tklabel(frame_cdf_notbook_list1, text = "n = "),
       columnspan = 3)

tkgrid(
  listbox_gene_cdf_list1 <-
    tk2listbox(
      frame_cdf_notbook_list1,
      height = kHeight + 7,
      width = kWidth + 8,
      selectmode = "extended",
      autoscroll = 'none'
    ),
  columnspan = 3,
  pady = c(5, 5)
)
tkgrid(frame_cdf_notbook_list1)

notebook_cdf_list2_tab <- tk2notetab(notebook_cdf, "list 2")


frame_cdf_notbook_list2 <- tkframe(notebook_cdf_list2_tab)

tkgrid(label_cdf_list2 <-
         tklabel(frame_cdf_notbook_list2, text = "n = "),
       columnspan = 3)

tkgrid(
  listbox_gene_cdf_list2 <-
    tk2listbox(
      frame_cdf_notbook_list2,
      height = kHeight + 7,
      width = kWidth + 8,
      selectmode = "extended",
      autoscroll = 'none'
    ),
  columnspan = 3,
  pady = c(5, 5)
)
tkgrid(frame_cdf_notbook_list2)

notebook_cdf_list3_tab <- tk2notetab(notebook_cdf, "list 3")

frame_cdf_notbook_list3 <- tkframe(notebook_cdf_list3_tab)

tkgrid(label_cdf_list3 <-
         tklabel(frame_cdf_notbook_list3, text = "n = "),
       columnspan = 3)

tkgrid(
  listbox_gene_cdf_list3 <-
    tk2listbox(
      frame_cdf_notbook_list3,
      height = kHeight + 7,
      width = kWidth + 8,
      selectmode = "extended",
      autoscroll = 'none'
    ),
  columnspan = 3,
  pady = c(5, 5)
)
tkgrid(frame_cdf_notbook_list3)

notebook_cdf_list4_tab <- tk2notetab(notebook_cdf, "list 4")

frame_cdf_notbook_list4 <- tkframe(notebook_cdf_list4_tab)

tkgrid(label_cdf_list4 <-
         tklabel(frame_cdf_notbook_list4, text = "n = "),
       columnspan = 3)

tkgrid(
  listbox_gene_cdf_list4 <-
    tk2listbox(
      frame_cdf_notbook_list4,
      height = kHeight + 7,
      width = kWidth + 8,
      selectmode = "extended",
      autoscroll = 'none'
    ),
  columnspan = 3,
  pady = c(5, 5)
)
tkgrid(frame_cdf_notbook_list4)

tkgrid(
  tk2button(
    frame_cdf_tab,
    text = "plot list",
    command = function()
      GeneListPlotHelper("cdf"),
    width = 15
  ),
  column = 0,
  row = 7,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_cdf_tab,
    text = "plot selected",
    command = function()
      SelectGeneListPlotHelper("cdf"),
    width = 15
  ),
  column = 1,
  row = 7,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_cdf_tab,
    text = "Intersect list",
    command = function()
      IntersectGeneLists(as.character(tkget(
        ListBoxSelectHelper("cdf"), 0, 'end'
      )),
      paste("cdf-", tk2notetab.text(notebook_cdf)))
    ,
    width = 15
  ),
  column = 0,
  row = 8,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_cdf_tab,
    text = "Save list",
    command = function()
      SaveGenelist('cdf', listbox_active_cdf, "cdf"),
    width = 15
  ),
  column = 1,
  row = 8,
  pady = c(5, 5)
)

tkgrid(
  tk2button(
    frame_cdf_tab,
    text = "Search list",
    command = function()
      FindGene(entry_cdf_search, 'cdf'),
    width = 15
  ),
  column = 0,
  row = 9,
  pady = c(10, 10)
)

tkgrid(
  entry_cdf_search <-
    tk2entry(frame_cdf_tab, tip = "Enter gene name to search", width = 20),
  column = 1,
  row = 9,
  pady = c(10, 10)
)

tkgrid(frame_cdf_tab)
tkpack.configure(frame_cdf_tab, fill = 'both', expand = 1)

# TOOL grid ----

tkgrid(frame_notebook_tool, sticky = 'nsew')
tkgrid(frame_tool,
       column = 1,
       row = 0,
       sticky = 'nsew')

# end ----

tkgrid.columnconfigure(root, frame_tool, weight = 1)
tkgrid.rowconfigure(root, frame_tool, weight = 1)

try(tk2theme("keramik"), silent = TRUE)
tkraise(root)
tkbind(root, "<FocusIn>", function()
  OpenWindowControl())
# }
# expandTk()
