# load packages ----
if(require("ggplot2")){
  print("ggplot2 is loaded correctly")
} else {
  print("trying to install ggplot2")
  install.packages("ggplot2")
  if(require(ggplot2)){
    print("ggplot2 installed and loaded")
  } else {
    stop("could not install ggplot2")
  }
}

if(require("tcltk")){
  print("tcltk is loaded correctly")
} else {
  print("trying to install tcltk")
  install.packages("tcltk")
  if(require(tcltk)){
    print("tcltk installed and loaded")
  } else {
    stop("could not install tcltk")
  }
}

if(require("tcltk2")){
  print("tcltk2 is loaded correctly")
} else {
  print("trying to install tcltk2")
  install.packages("tcltk2")
  if(require(tcltk2)){
    print("tcltk2 installed and loaded")
  } else {
    stop("could not install tcltk2")
  }
}

if(require("dplyr")){
  print("dplyr is loaded correctly")
} else {
  print("trying to install dplyr")
  install.packages("dplyr")
  if(require(dplyr)){
    print("dplyr installed and loaded")
  } else {
    stop("could not install dplyr")
  }
}

#expandTk <- function() {
  

# starting values  ----
my_version_num <- 'Ben Tools tabs'
FILE_LIST <- list() # for holding table files in list
FILE_LIST_INFO <- list()
# c(full_name, NAs, Zeros)
GENE_LISTS <- list() # for holding each gene list
#[[1]] Original [[2]] common 
GENE_LIST_INFO <- list() 
# c(name, nickname, dot", line", color, plot yes/no)
# for each file in each common gene list 
STATE <- c("colorset1", 0, 0, 1) # Keep track of the state and flow control 
# [1] active color set, [2] check if load file window open, 
# [3] when busy stop user input/activity, [4] master plot check

# values for comboboxs ----
my_dotlist <- c("circle", "triangle point up", "plus", "cross", "diamond", 
                "triangle point down", "square cross", "star", "diamond plus", 
                "circle  plus", "triangles up and down","square  plus", "circle cross", 
                "square and triangle down", "filled square", "filled circle",
                "filled triangle point up", "filled diamond", "solid circle", 
                "bullet (smaller circle)", "square")
my_linelist <- c("solid line", "dashed line", "dotted line", "dot dash line", 
                 "long dash line", "two dash line", "No line")
MY_COLORS <- list("colorset1" = c("#a6cee3",  "#1f78b4",  "#b2df8a",  "#33a02c",  "#fb9a99", 
                                  "#e31a1c",  "#fdbf6f",  "#ff7f00",  "#cab2d6", "#a6cee3",  
                                  "#1f78b4",  "#b2df8a",  "#33a02c",  "#fb9a99",  "#e31a1c",  
                                  "#fdbf6f",  "#ff7f00",  "#cab2d6"),
                  "colorset2" = c("red", "orange", "purple", "yellow", "blue", "green",
                                  rep("black",10)))
my_math <- c(" mean", " sum", " median")
my_topbottom <- c(" Top%", " Bottom%")
my_topbottom_num <- c(100, 75, 50, 25, 10, 5)
MY_LISTBOXS <- c("cgonbox", "cgoffbox")
my_plot_lines <- list("543 bins 20,20,40" = c(15.5, 45.5, 20.5, 40.5),
                      "543 bins 10,10,10" = c(5.5, 25.5, 10.5, 20.5))
my_plot_ticks <- list("543 bins 20,20,40" = list('name' = c('-1450 -950 -450 +450 +950 +1450 +1950 +2450 +2950 +3450'), 'loc' = c(1, 6, 11, 50, 55, 60, 65, 70, 75, 80)),
                      "543 bins 10,10,10" = list('name' = c('-450', '+450'), 'loc' = c(1,30)))

# tcl starting values ----
start_name <- tclVar("Load File")
start_col_list <- tclVar(names(MY_COLORS)[1])
start_line_list <- tclVar(my_linelist[1])
start_topbottom <- tclVar(my_topbottom[1])
start_topbottom_num <- tclVar(my_topbottom_num[5])
start_dot_list <- tclVar(my_dotlist[1])
start_color <- tclVar(MY_COLORS[[1]][1])
start_math <- tclVar(my_math[1])
start_nom <- "numerator"
start_dnom <- "denominator"
cbbVar_nbin <- tclVar(0)
start_plot_lines <- tclVar(names(my_plot_lines)[1])
start_list_gene <- tclVar("common gene list")
Header <- tclVar('')
Txt_one <- tclVar('TSS')
Txt_two <- tclVar('PolyA')
Txt_three <- tclVar('500')
Txt_four <- tclVar('500')
Txt_five <- tclVar(my_plot_ticks[[1]][[1]])
Pos_one <- tclVar(my_plot_lines[[1]][1])
Pos_two <- tclVar(my_plot_lines[[1]][2])
Pos_three <- tclVar(my_plot_lines[[1]][3])
Pos_four <- tclVar(my_plot_lines[[1]][4])
Pos_five <- tclVar(my_plot_ticks[[1]][[2]])
cbVar_relative_frequency <- tclVar(0)
cbVar_log2 <- tclVar(0)
cbVar_colorset <- tclVar(0)

# functions ----

# test function 
onOK <- function(){
  print("this works")
}

# keeps numbers, empty string for the rest
# from https://github.com/gsk3/taRifx/blob/master/R/Rfunctions.R#L1161
destring <- function(x,keep="0-9.-") {
  return(as.numeric(gsub(paste("[^",keep,"]+",sep=""),"",x)))
}

#moves all items from one list to the other
switchLstAll<-function(onlist, offlist, direction, workinglist){
  if(direction == "on"){
    tkdelete(offlist, 0, 'end')
    tkdelete(onlist, 0, 'end')
    tkconfigure(onlist, listvariable = tclVar(names(FILE_LIST)))
    lapply(names(FILE_LIST), function(i) GENE_LIST_INFO[[workinglist]][[i]][6] <<- 1)
    }
     
  if(direction == "off"){
    tkdelete(offlist, 0, 'end')
    tkdelete(onlist, 0, 'end')
    tkconfigure(offlist, listvariable = tclVar(names(FILE_LIST)))
    lapply(names(FILE_LIST), function(i) GENE_LIST_INFO[[workinglist]][[i]][6] <<- 0)
  }
}

#moves selected items from one list to the other
switchLst<-function(onlist, offlist, workinglist){
  for(i in rev(as.integer(tkcurselection(onlist)))){
    tkinsert(offlist,"end",tclvalue(tkget(onlist,i)))
    GENE_LIST_INFO[[workinglist]][[tclvalue(tkget(onlist,i))]][6] <<- 0
    tkdelete(onlist,i)
  }
  for(i in rev(as.integer(tkcurselection(offlist)))){
    tkinsert(onlist,"end",tclvalue(tkget(offlist,i)))
    GENE_LIST_INFO[[workinglist]][[tclvalue(tkget(offlist,i))]][6] <<- 1
    tkdelete(offlist,i)
  }
}

# updates items in lists when a file is removed
UpdateLstAll<-function(onlist, offlist){
  keepon <- NULL
  keepoff <- NULL
  lapply(GENE_LIST_INFO, function(i) lapply(i, function(j) 
    ifelse(j[6] == 1, keepon <<- c(keepon,j[1]), keepoff <<- c(keepoff,j[1]))))
  tkconfigure(onlist, listvariable=tclVar(as.character(keepon)))
  tkconfigure(offlist, listvariable=tclVar(as.character(keepoff)))
}
  
# read in /remove files functions ----

# reads in file, tests, fills out info 
GetTableFile <- function() {
  if(STATE[3] == 0){
    if(is.null(names(FILE_LIST))){
      file_count <- 0
    }else{
      file_count <- length(FILE_LIST)
    }
    #tk2notetab.select(nb, "Table files") # change tab
    STATE[3] <<- 1
    if(file_count > 10){ 
      tkmessageBox(message = "I have too many files, 
                   you need to reset me or remove some files")
      STATE[3] <<- 0
      return()
    }
    tcl("wm", "attributes", root, topmost=F)
    pb <- tkProgressBar(title = "Loading file, please be patient!!", width = 300 )
    file_name <- tclvalue(tkgetOpenFile(filetypes = 
                                          "{{Table Files} {.table .tab .Table}}"))
    ld_name <- paste(strsplit(as.character(file_name), 
                              '/')[[1]][(length(strsplit(as.character(file_name), 
                                                         '/')[[1]]))])
    if(!nchar(file_name)) { ## file select test
      close(pb)
      STATE[3] <<- 0
      return()
    }else if(ld_name %in% names(FILE_LIST)){
      tkmessageBox(message = "This file has already been loaded")
      STATE[3] <<- 0
      close(pb)
      tcl("wm", "attributes", root, topmost=TRUE)
      return()
    }else{
      legend_name <- strsplit(unlist(strsplit(as.character(ld_name), '.tab')[[1]][1]),'[.]')[[1]]
      legend_name <- legend_name[floor(mean(seq_along(legend_name)))]
      first_file <- read.table(file_name, header = TRUE, stringsAsFactors= FALSE, 
                               comment.char = "")
      names(first_file)[1]<-paste("gene")
      num_bins <- dim(first_file)
      if(file_count > 0){
        if(num_bins[2] != length(FILE_LIST[[1]])){
          close(pb)
          tkmessageBox(message = "Can't load file, different number of bins")
          STATE[3] <<- 0
          tcl("wm", "attributes", root, topmost=TRUE)
          return()
        }
        gene_names <- c(GENE_LISTS$main$common, unique(first_file[,1]))
        gene_names <- gene_names[duplicated(gene_names)]
        if(length(gene_names) > 0){
          GENE_LISTS$main$common <<- gene_names
        }else{
          close(pb)
          tkmessageBox(message = "Can't load file, no genes in common or remake your 
                       table files all the same way.")
          STATE[3] <<- 0
          tcl("wm", "attributes", root, topmost=TRUE)
          return()
        }
      }else{ # first time setting it up
        
        tkconfigure(cbb_nbin, values=c(0:(num_bins[2] - 1)))
        GENE_LISTS$main$common <<- unique(first_file[,1])
      }
      file_count <- file_count + 1
      FILE_LIST_INFO[ld_name] <<- list(c(file_name, paste(" % NA's = ", 
                                                          round((sum(is.na(first_file)) / (num_bins[1] * num_bins[2])) * 100, digits = 2)), 
                                               paste(" % Zeors = ", 
                                                     round((sum(first_file == 0) / (num_bins[1] * num_bins[2])) * 100, digits = 2))))
      
      colorsafe <- file_count %% length(MY_COLORS[[STATE[1]]])
      if(colorsafe == 0 ){
        colorsafe <- file_count
      }
      
      GENE_LIST_INFO$main[ld_name] <<- list(c(ld_name, legend_name, my_dotlist[1], 
                                              my_linelist[1],
                                              MY_COLORS[[STATE[1]]][colorsafe], 
                                              1))
      FILE_LIST[ld_name] <<- list(first_file)
      tkinsert(cgonbox, 'end', ld_name)
    }
    tcl("wm", "attributes", root, topmost=TRUE)
    STATE[3] <<- 0  
    tkconfigure(cbb_file, values=sapply(GENE_LIST_INFO$main, "[[", 1))
    tkconfigure(cbb_nom_file, values = sapply(GENE_LIST_INFO$main, "[[", 1))
    tkconfigure(cbb_dnom_file, values= sapply(GENE_LIST_INFO$main, "[[", 1))
    tkset(cbb_file, last(sapply(GENE_LIST_INFO$main, "[[", 1)))
    cbb_configure()
    close(pb)
  }
}

#removes file
RemoveFile <- function(){
  if(!is.null(names(FILE_LIST)) & STATE[3] == 0 & length(names(FILE_LIST)) > 1){
    #TODO change to cbb
    #num <- tclvalue(tkget(full_name2, 0))
    FILE_LIST[[num]] <<- NULL
    FILE_LIST_INFO[[num]] <<- NULL
    gene_names <- NULL
    lapply(names(FILE_LIST), function(i) gene_names <<- c(gene_names, 
                                                          unique(FILE_LIST[[i]][,1])))
    if(length(names(FILE_LIST)) > 1){
      gene_names <- gene_names[duplicated(gene_names)]
    }
      GENE_LISTS$main$common <<- gene_names
    # update GENE_LISTS[[]]$common all but $main ... look at rapply
    sapply(names(GENE_LIST_INFO), function(i) GENE_LIST_INFO[[i]][[num]] <<-NULL)
    UpdateLstAll(cgonbox, cgoffbox)
    tkconfigure(cbb_file, values=sapply(GENE_LIST_INFO$main, "[[", 1))
    tkset(cbb_file, names(FILE_LIST)[1])
    return(cbb_configure())
  }
  # if last one ask for reset
}

#get file to add more custome color options
GetColor <- function(){
  if(STATE[3] == 0){
     STATE[3] <<- 1
     tcl("wm", "attributes", root, topmost=F)
    
    file_name <- tclvalue(tkgetOpenFile(filetypes = 
                                          "{{color.txt Files} {.txt}}"))
    if(!nchar(file_name)) { ## file select test
      STATE[3] <<- 0
      return()
    }else if(file_name %in% names(MY_COLORS)){
      tkmessageBox(message = "This file has already been loaded")
      STATE[3] <<- 0
      tcl("wm", "attributes", root, topmost=TRUE)
      return()
    }else{
      l_name <- strsplit(as.character(file_name), '/') 
      ld_name <- paste(l_name[[1]][(length(l_name[[1]]))])
      legend_name <- paste(strsplit(as.character(ld_name), '.txt')[[1]][1])
      first_file <- read.table(file_name, header = FALSE, stringsAsFactors = FALSE)
      sapply(seq_along(first_file[[1]]), function(i) 
        if(suppressWarnings(!is.na(as.numeric(substr(first_file[[1]][i],1,1))))==TRUE){ 
          kkk <- strsplit(first_file[[1]][i],",")
          first_file[[1]][i] <<- rgb(as.numeric(kkk[[1]])[1], 
                        as.numeric(kkk[[1]])[2], 
                        as.numeric(kkk[[1]])[3], 
                        maxColorValue = 255)})
      
      MY_COLORS[legend_name] <<- first_file
      tkconfigure(cbb_color_sets, values=names(MY_COLORS))
      tkset(cbb_color_sets, legend_name)
      STATE[3] <<- 0
      tcl("wm", "attributes", root, topmost=TRUE)
      return(cbb_colorsets())
    }
  }
}

# make normalized file ... devide one by the other
MakeNormFile <- function(){
  nom <- as.character(tclvalue(tkget(cbb_nom_file)))
  dnom <- as.character(tclvalue(tkget(cbb_dnom_file)))
  if(nom != start_nom & dnom != start_dnom){ ## change to more streamed lined and split and divide on len
    gene_names <- c(FILE_LIST[[nom]]["gene"], FILE_LIST[[dnom]]["gene"])
    my_list <- data.frame(inner_join(FILE_LIST[[nom]], FILE_LIST[[dnom]], by = "gene"), stringsAsFactors = FALSE)
    my_list[is.na(my_list)] <- 0
    len <- (dim(my_list)[2]-1)/2
    my_min <- min(my_list[, -1][my_list[, -1] > 0])/2
    my_list[,-1] <- as.data.frame(lapply(my_list[,-1], function(x){replace(x, x == 0, my_min)}), stringsAsFactors = FALSE)
    first_file <- data.frame(gene = my_list[,1], my_list[,2:(len+1)] / my_list[,(len+2):((len*2)+1)], stringsAsFactors = FALSE)
    ld_name <- paste(nom, dnom, sep = "/")
    legend_name <- paste(GENE_LIST_INFO$main[[nom]][2], GENE_LIST_INFO$main[[dnom]][2], sep = "/")
    FILE_LIST[[ld_name]] <<- first_file
    file_count <- length(FILE_LIST)
    FILE_LIST_INFO[[ld_name]] <<- c(ld_name, paste(" 0's, NA's now = min/2"),
                                       paste(my_min))
  
    colorsafe <- file_count %% length(MY_COLORS[[STATE[1]]])
    if(colorsafe == 0 ){
      colorsafe <- file_count
    }
    GENE_LIST_INFO$main[[ld_name]] <<- c(ld_name, legend_name, my_dotlist[1], 
                                          my_linelist[1],
                                          MY_COLORS[[STATE[1]]][colorsafe], 
                                          1)
  
    tkinsert(cgonbox, 'end', ld_name)
    tkconfigure(cbb_file, values=sapply(GENE_LIST_INFO$main, "[[", 1))
    tkconfigure(cbb_nom_file, values=sapply(GENE_LIST_INFO$main, "[[", 1))
    tkconfigure(cbb_dnom_file, values=sapply(GENE_LIST_INFO$main, "[[", 1))
    tkset(cbb_file, last(sapply(GENE_LIST_INFO$main, "[[", 1)))
    tkset(cbb_nom_file, start_nom)
    tkset(cbb_dnom_file, start_dnom)
    cbb_configure()
  }  
}

# gene list functions ----

# displays the selected gene lists genes
ShowGenes <- function(){
  if(STATE[3] == 1 | is.null(names(FILE_LIST))){
    return()
  }else{    
    list_name <- as.character(tclvalue(tkget(cbb_list_gene)))
    if(list_name != tclvalue(start_list_gene)){ # do I need to fix this?
      return() #add something like tkconfigure(genelistentry, listvariable = tclVar(as.character(GENE_LISTS[[list_name]]$common)))
    }else{
      
      tkdelete(genelistinfo, 0, 'end')
      tkdelete(genelistentry, 0 ,'end')
      tkconfigure(genelistentry, listvariable = tclVar(as.character(GENE_LISTS$main$common)))
      tkinsert(genelistinfo, 0, paste('common genes,  n = ', length(GENE_LISTS$main$common)))
    }
  }
}

#sorts active gene list contain top % signal based on selected bins and file
SortTop <- function(filelist1, filelist2, maingenelist, genelist1, genelist2, genelistcount1, 
                    genelistcount2, tablefile, startbin1, startbox1, stopbin1, stopbox1, number) {
  if(BusyTest()){
    CloseSubWindows(c(3,4))
    STATE[5] <<- 1
    pb <<- tkProgressBar(title = "be patient!!",  min = 0, max = 100, width = 300)
    file_count <- as.integer(tksize(filelist1))
    file_count2 <- as.integer(tksize(filelist2))
    sel <- NULL
    my_list2 <- NULL
    for(d in 1 : file_count2){
      my_list2 <- c(my_list2, tclvalue(tkget(filelist2, (d-1))))
    }
    for(f in 1 : file_count){
      if(my_list2[1] %in% tclvalue(tkget(filelist1, (f-1))))
        sel <- c(sel, f)
    }
    for(f in 1 : file_count){
      if(my_list2[2] %in% tclvalue(tkget(filelist1, (f-1))))
        sel <- c(sel, f)
    }
    num_bins <- length(tablefile[[1]]) - 1 
    R_start_bin1 <- as.integer(tclvalue(startbin1))
    R_stop_bin1 <- as.integer(tclvalue(stopbin1))
    R_num <- as.character(tclvalue(number))
    gene_count <- as.integer(tksize(maingenelist))
    myList <- NULL
    if(R_num == " top_25%"){
      num <-  c(1, floor(gene_count * 0.25))
    } else if (R_num == " top_50%"){
      num <- c(1, floor(gene_count * 0.5))
    } else if (R_num == " top_75%"){
      num <- c(1, floor(gene_count * 0.75))
    } else if (R_num == " top_10%"){
      num <- c(1, floor(gene_count * 0.1))
    } else if (R_num == " top_5%"){
      num <- c(1, floor(gene_count * 0.05))
    } else if (R_num == " bottom_50%"){
      num <- c(ceiling(gene_count * 0.5), gene_count)
    } else if (R_num == " bottom_25%"){
      num <- c(ceiling(gene_count * 0.75), gene_count)
    } else if (R_num == " bottom_10%"){
      num <- c(ceiling(gene_count * 0.9), gene_count)
    } else if (R_num == " bottom_5%"){
      num <- c(ceiling(gene_count * 0.95), gene_count)
    }else if (R_num == " ALL"){
      num <- c(1, gene_count)
    }
    if(is.na(R_stop_bin1) | is.na(R_start_bin1)){
      R_start_bin1 <- 0
      R_stop_bin1 <- 0
    }
    if(R_start_bin1 < 1 | R_start_bin1 > R_stop_bin1){
      R_start_bin1 <- 1
      tkdelete(startbox1, 0, 'end')
      tkinsert(startbox1, "end", 1)
    } 
    if(R_stop_bin1 < 1 | R_stop_bin1 < R_start_bin1 | R_stop_bin1 > num_bins){
      R_stop_bin1 <- num_bins
      tkdelete(stopbox1, 0, 'end')
      tkinsert(stopbox1, "end", num_bins)
    }
    for(i in 1: gene_count){
      myList <- c(myList, tclvalue(tkget(maingenelist,(i-1))))
    }
    enesg <- as.data.frame(matrix(myList), stringsAsFactors = FALSE) 
    colnames(enesg) <- "gene"
    outlist <-NULL
    lc <- 0
    for(k in sel){  
      mch <- merge(enesg, tablefile[[k]], by="gene", sort=F)
      apply_bins <- rowSums(mch[,-1][R_start_bin1:R_stop_bin1],	na.rm = T)
      ix <- sort(apply_bins, decreasing=T, index=T)$ix
      lc <- lc+1
      outlist[[lc]] <- as.character(mch[ix,1][num[1]:num[2]])
    }
    tkdelete(genelist1, 0, 'end')
    tkdelete(genelist2, 0, 'end')
    if(length(outlist[[1]]) > 0){
      tkconfigure(genelist1, listvariable = tclVar(as.character(outlist[[1]])))
    }
    if(file_count2 == 2 ){
      if(length(outlist[[2]]) > 0){
        tkconfigure(genelist2, listvariable = tclVar(as.character(outlist[[2]])))
      }
    }
    tkdelete(genelistcount1, 0, 'end')
    tkdelete(genelistcount2, 0, 'end')
    lstcount1 <- paste('n = ', (as.integer(tksize(genelist1))))
    lstcount1a <- paste(R_num , ' in ',tclvalue(tkget(filelist2,0)))
    tkinsert(genelistcount1, "end", lstcount1, lstcount1a)
    lstcount2 <- paste('n = ', (as.integer(tksize(genelist2))))
    lstcount2a <- paste(R_num , ' in ',tclvalue(tkget(filelist2,1)))
    tkinsert(genelistcount2, "end", lstcount2, lstcount2a)
    close(pb)
    STATE[5] <<- 0
    return()
  }
  return()
}	


# plotting functions ----

# Makes data frame and gathers plot settings for plotting active samples
MakeDataFrame <- function(){
  if(STATE[3] == 1 | is.null(names(FILE_LIST))){
    return()
  }else{
    use_col <- NULL
    use_dot <- NULL
    use_line <- NULL
    use_name <- NULL
    #tk2notetab.select(nb, "Plot")
    wide_list <- list()
    for(i in names(GENE_LISTS)){
      # checks to see if at least one in list is acitve
      if(sum(as.numeric(sapply(GENE_LIST_INFO[[i]], "[[", 6))) == 0){ 
        return()
      }else{
        enesg <- data.frame(gene=GENE_LISTS[[i]][[1]], stringsAsFactors = FALSE)
        lapply(names(FILE_LIST), function(k) 
          # uses only acive lists  
          if(as.numeric(GENE_LIST_INFO[[i]][[k]][6]) == 1){        
            wide_list[[k]] <<- data.frame(inner_join(enesg, FILE_LIST[[k]], by = "gene"), stringsAsFactors = FALSE)
            dot <- which(my_dotlist == GENE_LIST_INFO[[i]][[k]][3])
            if(dot > 20){
              dot <- 0
            }
            line <- which(my_linelist == GENE_LIST_INFO[[i]][[k]][4])
            if(line > 6){
              line <- 0
            }
            use_col <<- c(use_col, GENE_LIST_INFO[[i]][[k]][5])
            use_dot <<- c(use_dot, dot)
            use_line <<- c(use_line, line)
            use_name <<- c(use_name, gsub("/", " / \n", GENE_LIST_INFO[[i]][[k]][2]))
          }
        )
      } 
    }
  }
  if(!is.null(names(wide_list))){
    ApplyMath(wide_list, use_col, use_dot, use_line, use_name)
  }
}

# Applys math to long list
ApplyMath <- function(wide_list, use_col, use_dot, use_line, use_name){
  
  # math set and y lable
  R_my_math <- as.character(tclvalue(tkget(cbb_math)))
  if(R_my_math == " mean"){
    my_apply <- function(x) colMeans(x, na.rm = TRUE)
    y_lab <- "Mean of bin counts"
  } else if (R_my_math == " sum"){
    my_apply <- function(x) colSums(x, na.rm = TRUE)
    y_lab <- "Sum of bin counts"
  } else if (R_my_math == " median"){
    my_apply <- function(x) apply(x, 2, median, na.rm = TRUE)
    y_lab <- "Median of bin counts"
  } 
  
  # set normilization to relative frequency or bin number or 1 for none, update y lable
  cbValue_relative_frequency <- as.character(tclvalue(cbVar_relative_frequency))
  RValue_norm_bin <- as.numeric(tclvalue(cbbVar_nbin))
  
  if(cbValue_relative_frequency == 1 & RValue_norm_bin == 0){
    y_lab <- paste(strsplit(y_lab, split = " ")[[1]][1], "bins : Relative Frequency")
    normz <- lapply(seq_along(wide_list), function(i) sum(my_apply(wide_list[[i]][,-1])))
  } else if(RValue_norm_bin > 0){
    if(cbValue_relative_frequency == 1){
      cbVar_relative_frequency <<- tclVar(0)
      tkconfigure(cb_rf, variable = cbVar_relative_frequency)
    }
    y_lab <- paste(strsplit(y_lab, split = " ")[[1]][1], "bins : Normalize to bin ", RValue_norm_bin)
    normz <- as.numeric(lapply(seq_along(wide_list), 
                    function(i) my_apply(wide_list[[i]][,-1])[RValue_norm_bin]))
  } else {
    normz <- lapply(seq_along(wide_list) ,function(i) 1)
  }
  
  # update y lable if log2
  if(tclvalue(cbVar_log2) == 1){
    y_lab <- paste("log2(",y_lab,")", sep = "")
  }
  x_lab <- paste("bins \n n = ", length(GENE_LISTS$main$common))
  math_list <- lapply(seq_along(wide_list), function(i) 
    data.frame(bin=(seq_along(wide_list[[i]][-1])), 
               set=(as.character(use_name[i])), 
               value=my_apply(wide_list[[i]][,-1])/normz[[i]], stringsAsFactors = FALSE)) 
  long_list <- rbind_all(math_list)
  
  Pfive <- destring(unlist(strsplit(tclvalue(Pos_five), " ")))
  Tfive <- unlist(strsplit(tclvalue(Txt_five), " "))
  if(length(Tfive) != length(Pfive)){
    tkmessageBox(message = "The number of Positions and Lables are unequal")
    return()
  }
  
  
  my_xbreaks <- c(destring(tclvalue(Pos_one)), destring(tclvalue(Pos_two)), 
                  destring(tclvalue(Pos_three)), destring(tclvalue(Pos_four)),
                  Pfive)
  
  my_xlab <- c(tclvalue(Txt_one), tclvalue(Txt_two),tclvalue(Txt_three),tclvalue(Txt_four),
               Tfive)
  ltype <- c(3,3,1,1)
  my_col <- c("green", "red", "black", "black")
  my_xlab <- my_xlab[!is.na(my_xbreaks)]
  ltype <- ltype[!is.na(my_xbreaks[1:4])]
  my_col <- my_col[!is.na(my_xbreaks[1:4])]
  xbreaks <- my_xbreaks[1:4][!is.na(my_xbreaks[1:4])]
  my_xbreaks <- my_xbreaks[!is.na(my_xbreaks)]
  
  # need controls?
  my_vlines <- data.frame(xbreaks, ltype, my_col, stringsAsFactors = FALSE)
  names(use_col) <- use_name
  names(use_dot) <- use_name
  names(use_line) <- use_name
  GGplotF(long_list, use_col, use_dot, use_line, y_lab, x_lab, my_xbreaks, my_vlines, my_xlab)
}

# ggplot function
GGplotF <- function(long_list, use_col, use_dot, use_line, y_lab, x_lab, my_xbreaks, my_vlines, my_xlab){
  if(tclvalue(cbVar_log2) == 1){
    gp <- ggplot(long_list, aes(x=bin, y=log2(value), color=set, shape=set, linetype=set))
  }else{
    gp <- ggplot(long_list, aes(x=bin, y=value, color=set, shape=set, linetype=set))
  }
  gp <- gp +
   
    geom_line(size=1.5) + geom_point(size=4) +  
    scale_color_manual(name = "Sample", values=use_col)+
    scale_shape_manual(name = "Sample", values=use_dot) + 
    scale_linetype_manual(name = "Sample", values=use_line)+
    xlab(x_lab) + ylab(y_lab) + # Set axis labels
    ggtitle(tclvalue(Header)) +  # Set title
    scale_x_continuous(breaks = my_xbreaks, labels = my_xlab)+
  
    geom_vline(data = my_vlines, aes(xintercept = xbreaks), size = 2, linetype = my_vlines$ltype, color = my_vlines$my_col) +
    theme_bw() +
    theme(panel.grid.minor = element_blank())+
    theme(plot.title = element_text(size = 30, vjust = 2))+
    theme(axis.title.y = element_text(size =  20, vjust = 1.5))+
    theme(axis.title.x = element_text(size =  25, vjust = 0))+
    theme(axis.text.x = element_text(size = 10, angle = -45, hjust = .1, 
                                     vjust = .8, face = 'bold'))
 print(gp)
}

# combobox functions ----

# updates comboboxs and lists 
cbb_configure <- function(){
  num <- tclvalue(tkget(cbb_file))
  if(!is.null(names(FILE_LIST)) & STATE[3] == 0){
    #_TODO fix or remove?
#     if(num == ""){ # back to "" if needed
#       num <- tclvalue(tkget(full_name2,0))
#       tkset(cbb_file, num)
#     }else{
#       tkdelete(full_name2, 0, "end")
#       tkinsert(full_name2, 0, num)
#     }
    tkset(cbb_color, sapply(GENE_LIST_INFO$main, "[[", 5)[num])
    tkset(cbb_line, sapply(GENE_LIST_INFO$main, "[[", 4)[num])
    tkset(cbb_dot, sapply(GENE_LIST_INFO$main, "[[", 3)[num])
    tkdelete(new_name, 0, 'end')
    tkinsert(new_name,  0, sapply(GENE_LIST_INFO$main, "[[", 2)[num])
    tkconfigure(stats_list, listvariable = 
                  tclVar(as.character(FILE_LIST_INFO[[num]][2:3])))
  } 
}

# saves current seletion
cbb_setvalues <- function(){
  num <- tclvalue(tkget(cbb_file))
  if(!is.null(names(FILE_LIST)) & num != "" & STATE[3] == 0){
    GENE_LIST_INFO$main[[num]][5] <<- tclvalue(tkget(cbb_color))
    GENE_LIST_INFO$main[[num]][4] <<- tclvalue(tkget(cbb_line))
    GENE_LIST_INFO$main[[num]][3] <<- tclvalue(tkget(cbb_dot))
    GENE_LIST_INFO$main[[num]][2] <<- tclvalue(tkget(new_name))
  } 
}

# Adds ability to change color sets
cbb_colorsets <- function(){
  if(STATE[3] == 0){
    num <- as.numeric(tclvalue(tcl(cbb_file,"current")))+1
    num1 <- length(FILE_LIST)
    num2 <- length(MY_COLORS[[tclvalue(tkget(cbb_color_sets))]])
    if(!is.numeric(num2) | num2 < 1){
      return()
    }
    STATE[1] <<- tclvalue(tkget(cbb_color_sets))
    if(num < 1){
      num <- 1
    }
    
    if(num1 > 0 & as.character(tclvalue(cbVar_colorset)) == 1){
      lapply(seq_along(FILE_LIST), function(i) 
        GENE_LIST_INFO$main[[i]][5] <<- MY_COLORS[[tclvalue(tkget(cbb_color_sets))]][ifelse(i %% num2 > 0, i %% num2, i)])  
      colorsafe <- num %% num2
      if(colorsafe == 0 ){
        colorsafe <- num
      }
      tkset(cbb_color, MY_COLORS[[tclvalue(tkget(cbb_color_sets))]][colorsafe])  
    }  
    tkconfigure(cbb_color, values = MY_COLORS[[tclvalue(tkget(cbb_color_sets))]])  
  }
}

# Change plot lables with lines
PlotLines <- function(){
  num <- tclvalue(tkget(cbb_plot_lines))
  tkdelete(Posone, 0, 'end')
  tkinsert(Posone, 0, my_plot_lines[[num]][1])
  tkdelete(Postwo, 0, 'end')
  tkinsert(Postwo, 0, my_plot_lines[[num]][2])
  tkdelete(Posthree, 0, 'end')
  tkinsert(Posthree, 0, my_plot_lines[[num]][3])
  tkdelete(Posfour, 0, 'end')
  tkinsert(Posfour, 0, my_plot_lines[[num]][4])
  tkdelete(Posfive, 0, 'end')
  tkinsert(Posfive, 0, my_plot_ticks[[num]][["loc"]])
  tkdelete(Txtfive, 0, 'end')
  tkinsert(Txtfive, 0, my_plot_ticks[[num]][["name"]])
}

# GUI function ----

# set up root window ---- 

root <- tktoplevel() #container for it all
tcl("wm", "attributes", root, topmost = TRUE)
tkwm.title(root, my_version_num)

# menu setup ----
topMenu <- tk2menu(root)           # Create a menu
tkconfigure(root, menu = topMenu) # Add it to the main window
fileMenu <- tkmenu(topMenu, tearoff = FALSE)
tkadd(fileMenu, "command", label = "Load table file", command = function() 
  GetTableFile())
tkadd(fileMenu, "command", label = "Load color pallet")
tkadd(fileMenu, "command", label = "Quit", command = function() 
  tkdestroy(root))
tkadd(fileMenu, "command", label = "Restart", command = function() 
  tkdestroy(root))
tkadd(topMenu, "cascade", label = "File", menu = fileMenu)

# create main notebook ----
nb <- tk2notebook(root, tabs = c("Plot", "Genes", "Tools", "Plot Options"))
tkgrid(nb, column = 1, row = 0)

# tab for Plot Options  ----
PlotOptionstab <- tk2notetab(nb, "Plot Options")

# frame for loading and applying diffrent color groups

tab1box4 <- tkframe(PlotOptionstab, relief = 'ridge', borderwidth = 5)

tkgrid(tklistbox(tab1box4, listvariable = tclVar("colorset"), height = 1, width = 8, 
                 relief = 'flat'), padx = c(5, 0))

cbb_color_sets <- tk2combobox(tab1box4, value = names(MY_COLORS), textvariable = start_col_list, 
                              state="readonly")
tkgrid(cbb_color_sets, sticky = 'w', padx = c(0,5), column = 1, row = 0)
tkbind(cbb_color_sets, "<<ComboboxSelected>>", cbb_colorsets)

tkgrid(tkcheckbutton(tab1box4, variable = cbVar_colorset, 
                     text = "Update colors" ))

tkgrid(tkbutton(tab1box4, text = ' Load custom color ', 
                command = function() GetColor()), pady = c(15,0), columnspan =2) 

tkgrid(tklistbox(tab1box4, listvariable = tclVar("Line"), height = 1, width = 4, 
                 relief = 'flat'), padx = c(20, 0))

cbb_line <- tk2combobox(tab1box4, value = my_linelist, textvariable= start_line_list,
                        state="readonly")
tkgrid(cbb_line, sticky = "w", column = 1, row = 3, padx = c(0, 16)) 
tkbind(cbb_line, "<<ComboboxSelected>>", cbb_setvalues)

tkgrid(tklistbox(tab1box4, listvariable = tclVar("dot"), height = 1, width = 4, 
                 relief = 'flat'), padx = c(20, 0))

cbb_dot <- tk2combobox(tab1box4, value = my_dotlist, textvariable= start_dot_list,
                       state="readonly") 
tkgrid(cbb_dot, sticky = "w", column = 1, row = 4, padx = c(0, 16)) 
tkbind(cbb_dot, "<<ComboboxSelected>>", cbb_setvalues)

tkgrid(tk2entry(tab1box4, width = 20, textvariable = Header),  
       padx = c(5, 0), pady = c(0, 2), sticky = "w", columnspan = 5)
tkgrid(tklabel(tab1box4, text = "  Header"), padx = c(5,1), pady = c(0, 2))
tkgrid(tk2entry(tab1box4, width = 20, textvariable = Header),  
       padx = c(5, 0), pady = c(0, 2), sticky = "w", columnspan = 5)
tkgrid(tklabel(tab1box4, text = "  Header"), padx = c(5,1), pady = c(0, 2))
tkgrid(tk2entry(tab1box4, width = 20, textvariable = Header),  
       padx = c(5, 0), pady = c(0, 2), sticky = "w", columnspan = 5)
tkgrid(tklabel(tab1box4, text = "  Header"), padx = c(5,1), pady = c(0, 2))

# left frame for constent items ----

leftframe <- tkframe(root)

# frame for file select 
tab1box1 <- tkframe(leftframe, relief = 'ridge', borderwidth = 5)
tkgrid(tkbutton(tab1box1, font =c('bold', 20), text = " Load Table File ", 
                command =  function() GetTableFile()))

# frame for file options and info
leftbox1 <- tkframe(leftframe, relief = 'ridge', borderwidth = 5)

title_file <- tklistbox(leftbox1, height = 1, width = 30, relief = 'flat')
tkgrid(title_file, columnspan = 2)  
tkinsert(title_file, 0, "                     File options settings")

tkgrid(tklistbox(leftbox1, listvariable = tclVar("File"), height = 1, width = 5, 
                 relief = 'flat'), padx = c(5, 0))
cbb_file <- tk2combobox(leftbox1, textvariable = start_name, state = "readonly", width = 25)
tkgrid(cbb_file, column = 1, row = 1)
tkbind(cbb_file, "<<ComboboxSelected>>", cbb_configure)

tkgrid(tklistbox(leftbox1, listvariable = tclVar("nickname:"), height = 1, width = 10, 
                 relief = 'flat'), padx = c(5, 0))

new_name <- tk2entry(leftbox1, width = 30)
tkgrid(new_name, sticky = "w", column = 1, row = 2, padx = c(0, 4))
tkinsert(new_name, 0,  paste(GENE_LIST_INFO$main[1]))
tkbind(new_name, "<Leave>", cbb_setvalues)

tkgrid(tklistbox(leftbox1, listvariable = tclVar("color"), height = 1, width = 5, 
                 relief = 'flat'), padx = c(5, 0))

cbb_color <- tk2combobox(leftbox1, value = MY_COLORS[[tclvalue(tkget(cbb_color_sets))]], 
                         textvariable= start_color, state="readonly")
tkgrid(cbb_color, sticky = "w", column = 1, row = 3, padx = c(0, 16))
tkbind(cbb_color, "<<ComboboxSelected>>", cbb_setvalues)

tkgrid(tklistbox(leftbox1, listvariable = tclVar("Stats"), height = 1, width = 5, 
                 relief = 'flat'), column = 0, padx = c(5, 0))
stats_list <- tklistbox(leftbox1, height = 2, width = 30)
tkgrid(stats_list, column = 1, row = 4)

tkgrid(tkbutton(leftbox1, text = " Remove file ", command =  function() 
  RemoveFile()), columnspan = 2)

# frame for plot button

tab1box3 <- tkframe(leftframe, relief = 'ridge', borderwidth = 5)
tkgrid(tkbutton(tab1box3, font =c('bold', 23), text = '      Plot       ', 
                command = function() MakeDataFrame())) 


tkgrid(tab1box1, column = 0, row = 0)
#tkgrid(tab1box2, column = 1, row = 0)
tkgrid(tab1box3, column = 0, row = 2)
tkgrid(tab1box4, column = 1, row = 1)
tkgrid(leftbox1, column = 0, row = 1)
tkgrid(leftframe, column = 0, row = 0)

# tab plot ----
plottab <- tk2notetab(nb, "Plot")

# box for plot settings
tab2box1 <- tkframe(plottab, relief = 'ridge', borderwidth = 5)

tkgrid(tklabel(tab2box1, text = "Plot Options", width = 35))  

cbb_math <- tk2combobox(tab2box1, value = my_math, textvariable = start_math, state="readonly")
tkgrid(cbb_math, sticky = "n")

cb_rf <- tkcheckbutton(tab2box1, variable = cbVar_relative_frequency, 
                       text = "Relative Frequency" )
tkgrid(cb_rf)

cb_log2 <- tkcheckbutton(tab2box1, variable = cbVar_log2, text = "log2 transformation" )
tkgrid(cb_log2)

tkgrid(tklistbox(tab2box1, listvariable = tclVar("Norm_to_bin"), height = 1, width = 12, 
                 relief = 'flat'), padx = c(50, 0), sticky = "w")

cbb_nbin <- tk2combobox(tab2box1, textvariable = cbbVar_nbin, state="readonly", width = 3)
tkgrid(cbb_nbin, sticky = "e", column = 0, row = 5, padx = c(0, 50)) 

tkgrid(tab2box1, row = 0, sticky = "n")

#frame for bottom left

tab2box1_1 <- tkframe(plottab, relief='ridge', borderwidth = 5) 

tkgrid(tklabel(tab2box1_1, text = ' Plot Options ', width = 30), padx = c(5, 3), 
       pady = c(0, 2), columnspan = 6)


tkgrid(tklabel(tab2box1_1, text = "Plot lines and lables"), pady = c(5, 5), row = 7,
       column = 0, columnspan = 6)

cbb_plot_lines <- tk2combobox(tab2box1_1, value = names(my_plot_lines), textvariable = start_plot_lines, state="readonly", width = 20)
tkgrid(cbb_plot_lines, column = 0, columnspan = 6, row = 7) 
tkbind(cbb_plot_lines, '<<ComboboxSelected>>', PlotLines)

tkgrid(tk2entry(tab2box1_1, width = 5, textvariable = Txt_one),  
       padx = c(10, 0), pady = c(5, 0), column = 0, row = 8)
tkgrid(tklabel(tab2box1_1, text = 'Pos'), padx = c(5, 3), pady = c(5, 0), column = 1, 
       row = 8, sticky = "w")
Posone <- tk2entry(tab2box1_1, width = 4, textvariable = Pos_one)
tkgrid(Posone, column = 2, row = 8, sticky = "w", padx = c(0, 0), pady = c(5, 0))

tkgrid(tk2entry(tab2box1_1, width = 5, textvariable = Txt_two),  
       padx = c(10, 0), pady = c(3, 0), column = 0, row = 9)
tkgrid(tklabel(tab2box1_1, text = 'Pos'), padx = c(5, 3), pady = c(3, 0), column = 1,
       row = 9, sticky = "w")
Postwo <- tk2entry(tab2box1_1, width = 4, textvariable = Pos_two)
tkgrid(Postwo, column = 2 , row = 9, sticky = "w", padx = c(0, 0), pady = c(3, 0))

tkgrid(tk2entry(tab2box1_1, width = 5, textvariable = Txt_three),  
       padx = c(10, 0), pady = c(5, 0), column = 3, row = 8)
tkgrid(tklabel(tab2box1_1, text = 'Pos'), padx = c(5, 3), pady = c(5, 0), column = 4,
       row = 8, sticky = "w")
Posthree <- tk2entry(tab2box1_1, width = 4, textvariable = Pos_three)
tkgrid(Posthree, column = 5, row = 8, sticky = "w", padx = c(0, 0), pady = c(3, 0))

tkgrid(tk2entry(tab2box1_1, width = 5, textvariable = Txt_four),  
       padx = c(10, 0), pady = c(5, 0), column = 3, row = 9)
tkgrid(tklabel(tab2box1_1, text = 'Pos'), column = 4, row = 9, sticky = "w",
       padx = c(5, 3), pady = c(5, 0))
Posfour <- tk2entry(tab2box1_1, width = 4, textvariable = Pos_four)
tkgrid(Posfour, column = 5, row = 9, sticky = "w", padx = c(0, 0), pady = c(3, 0))
  

tkgrid(tklabel(tab2box1_1, text = "More Bin labels"), pady = c(4, 3), row = 11, column = 0,
       columnspan = 6)

tkgrid(tklabel(tab2box1_1, text = 'Pos'), padx = c(5, 0), column = 0,
       row = 12, sticky = "w")
Posfive <- tk2entry(tab2box1_1, width = 35, textvariable = Pos_five)
tkgrid(Posfive, column = 1, row = 12, padx = c(0, 0), columnspan = 5, sticky = "w")
tkgrid(tklabel(tab2box1_1, text = 'Lable'), padx = c(5, 0), column = 0,
       row = 13, sticky = "w")
Txtfive <- tk2entry(tab2box1_1, width = 35, textvariable = Txt_five)
tkgrid(Txtfive, column = 1, row = 13, padx = c(0, 0), columnspan = 5)
 
tkgrid(tab2box1_1)

# on/off list notebook ---- 
tab2box2 <- tkframe(plottab, relief = 'ridge', borderwidth = 5)
pnb <- tk2notebook(tab2box2, tabs =c("Common\nGenes","Gene\nlist 1","Gene\nlist 2", 
                                     "Gene\nlist 3", " Gene\nlist 4"))
tkgrid(pnb)
pttab <- tk2notetab(pnb, "Common\nGenes")
cgtabbox2 <- tkframe(pttab)
tkgrid(tklabel(cgtabbox2, text= "List of table files"), columnspan = 6)
cgonbox <- tk2listbox(cgtabbox2, width = 35, height = 8)
tkgrid(cgonbox, columnspan = 3)
tkgrid(tkbutton(cgtabbox2,text="<<Switch>>", command = function() 
  switchLst(cgonbox, cgoffbox, "main")), 
       tkbutton(cgtabbox2,text="<<All On>>", command = function() 
         switchLstAll(cgonbox, cgoffbox, "on", "main")), 
       tkbutton(cgtabbox2,text="<<All Off>>", command = function() 
         switchLstAll(cgonbox, cgoffbox, "off", "main")),
       sticky = 'we')
cgoffbox <- tk2listbox(cgtabbox2, width = 35, height = 8)
tkgrid(cgoffbox, columnspan = 3)
tkgrid(cgtabbox2)

cgtabbox1 <- tkframe(pttab)
tkgrid(cgtabbox1)
tkgrid(tab2box2, column = 1, row = 0, rowspan = 2)

# tab for display genes ----
geneliststab <- tk2notetab(nb, "Genes")

tab3box1 <- tkframe(geneliststab, relief = 'ridge', borderwidth = 5)

tkgrid(tklistbox(tab3box1, listvariable = tclVar("list:"), height = 1, width = 4, 
                 relief = 'flat'), padx = c(20, 0))
cbb_list_gene <- tk2combobox(tab3box1, textvariable = start_list_gene,
                       state="readonly") 
tkgrid(cbb_list_gene, sticky = "w", column = 1, row = 0, padx = c(0, 16)) 

tkgrid(tkbutton(tab3box1, text = " Show Genes ", command =  function() ShowGenes()), 
       column = 0, row = 1, columnspan =2)

tkgrid(tklistbox(tab3box1, listvariable = tclVar(""), height = 1, width = 12, 
                 relief = 'flat'), padx = c(20, 0), column = 0, row = 2)

tkgrid(tklistbox(tab3box1, listvariable = tclVar("compare_to:"), height = 1, width = 12, 
                 relief = 'flat'), padx = c(20, 0), column = 0, row = 3)
cbb_list_comp <- tk2combobox(tab3box1, textvariable = start_list_gene,
                             state="readonly") 
tkgrid(cbb_list_comp, sticky = "w", column = 1, row = 3, padx = c(0, 16)) 

tkgrid(tkbutton(tab3box1, text = " intersect list ", command =  function() onOK()), 
       column = 0, row = 4, columnspan =2)

tkgrid(tkbutton(tab3box1, text = " save list ", command =  function() onOK()), 
       column = 0, row = 5, columnspan =2)


tab3box2 <- tkframe(geneliststab, relief = 'ridge', borderwidth = 5)

genelistinfo <- tklistbox(tab3box2, listvariable = tclVar("load_list"), 
                 relief = 'flat', height = 1, width = 30)
tkgrid(genelistinfo, column = 0, row = 0)
genelistentry <- tk2listbox(tab3box2, height = 25, width = 30)
tkgrid(genelistentry, column = 0, row = 1, sticky = "n")

tkgrid(tab3box1, column = 0, row = 0, sticky = "n")
tkgrid(tab3box2, column = 1, row = 0, sticky = "n")

# tab for tools ----
tooltab <- tk2notetab(nb, "Tools")

# frame for file deviding 
tab1box5 <- tkframe(tooltab, relief = 'ridge', borderwidth = 5)
new_file_name <- tklistbox(tab1box5, height = 1, width = 38, relief = 'flat')
tkgrid(new_file_name, sticky = "n", columnspan = 2)  
tkinsert(new_file_name, 0, "     select samples for normalization")

num_name <- tklistbox(tab1box5, height = 1, width = 6, relief = 'flat')
tkgrid(num_name, padx = c(20, 0), pady = c(5,0), column = 0, row = 1)  
tkinsert(num_name, 0, "divid:")

cbb_nom_file <- tk2combobox(tab1box5, state = "readonly")
tkset(cbb_nom_file, start_nom)
tkgrid(cbb_nom_file, sticky = "w", column = 1, row = 1, padx = c(0, 16), pady = c(5,0)) 

dnom_name <- tklistbox(tab1box5, height = 1, width = 6, relief = 'flat')
tkgrid(dnom_name, padx = c(20, 0), pady = c(0,5), column = 0, row = 2)  
tkinsert(dnom_name, 0, "by:")

cbb_dnom_file <- tk2combobox(tab1box5, state = "readonly")
tkset(cbb_dnom_file, start_dnom)
tkgrid(cbb_dnom_file, sticky = "w", column = 1, row = 2, padx = c(0, 16), pady = c(0,5)) 

tkgrid(tkbutton(tab1box5, text = '      Create       ', 
                command = function() MakeNormFile()), columnspan = 2) 

tkgrid(tab1box5, column = 0, row = 1)

# box for plot settings
tooltabbox1 <- tkframe(tooltab, relief = 'ridge', borderwidth = 5)

tkgrid(tklabel(tooltabbox1, text = "Sort tools", width = 12)) 

tkgrid(tklistbox(tooltabbox1, listvariable = tclVar(""), height = 1, width = 1, relief = 'flat'),
       column = 0, row = 1, columnspan = 2)
cbb_topbottom <- tk2combobox(tooltabbox1, values =  my_topbottom, textvariable = start_topbottom,
                             state = "readonly", width = 10) 
tkgrid(cbb_topbottom, sticky = "w", column = 0, row = 2, padx = c(5, 0)) 

cbb_topbottom_num <- tk2combobox(tooltabbox1, values =  my_topbottom_num, 
                                 textvariable = start_topbottom_num, state = "readonly", width = 3) 
tkgrid(cbb_topbottom_num, sticky = "we", column = 1, row = 2, padx = c(0, 5), pady = c(10, 10))

tkgrid(tkbutton(tooltabbox1, text = "   Sort   ", command =  function() onOK()), 
       column = 0, row = 4, columnspan = 3, pady = c(10, 10))

tkgrid(tooltabbox1, column = 0, row = 0, sticky = 'n')



# end ----

try(tk2theme("keramik"), silent = TRUE)
#}
#expandTk()
