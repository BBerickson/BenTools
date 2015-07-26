# Created by Benjamin Erickson
version.num <- 'Ben Tools tabs'
# For plotting Bentley lab table files and generating gene list

# load packages ----
if (require("ggplot2")){
  print("ggplot2 is loaded correctly")
} else {
  print("trying to install ggplot2")
  install.packages("ggplot2")
  if (require(ggplot2)){
    print("ggplot2 installed and loaded")
  } else {
    stop("could not install ggplot2")
  }
}

if (require("tcltk")){
  print("tcltk is loaded correctly")
} else {
  print("trying to install tcltk")
  install.packages("tcltk")
  if (require(tcltk)){
    print("tcltk installed and loaded")
  } else {
    stop("could not install tcltk")
  }
}

if (require("tcltk2")){
  print("tcltk2 is loaded correctly")
} else {
  print("trying to install tcltk2")
  install.packages("tcltk2")
  if (require(tcltk2)){
    print("tcltk2 installed and loaded")
  } else {
    stop("could not install tcltk2")
  }
}

if (require("dplyr")){
  print("dplyr is loaded correctly")
} else {
  print("trying to install dplyr")
  install.packages("dplyr")
  if (require(dplyr)){
    print("dplyr installed and loaded")
  } else {
    stop("could not install dplyr")
  }
}

#expandTk <- function() {
  

# file list varibles  ----
list.tablefile <- list()  # for holding table files in list
                          # [[]] gene X1 X2 ...
list.tablefile.info <- list()  # for holding table file info, 
                               # c(full_name, NAs, Zeros)
list.genefile <- list()  # for holding gene files in a list
                         # [[1]] Original [[2]] common 
list.genefile.info <- list()  # for holding gene file info
                              # c(name, nickname, dot", line", color, plot yes/no)

# R varibles ----
control.state <- c("color.set1", 0, 0, 1)  # state of the GUI and control functions
                                          # [1] active color set, 
                                          # [2] check if load file window open, 
                                          # [3] busy? stop user input/activity, 
                                          # [4] master plot check

# values for comboboxs ----
kNbFileControlTabNames <- c("Common Genes", "Gene list 1", "Gene list 2",
                            "Gene list 3", "Gene list 4")
kDotOptions <- c("circle", "triangle point up", "plus", "cross", "diamond",
                "triangle point down", "square cross", "star", "diamond plus", 
                "circle  plus", "triangles up and down","square  plus", 
                "circle cross", "square and triangle down", "filled square",
                "filled circle", "filled triangle point up", "filled diamond", 
                "solid circle", "bullet (smaller circle)", "square")
kLineOptions <- c("solid line", "dashed line", "dotted line", "dot dash line", 
                 "long dash line", "two dash line", "No line")
kListColorSet <- list("color.set1" = c("#a6cee3", "#1f78b4", "#b2df8a",
                                       "#33a02c", "#fb9a99", "#e31a1c", 
                                       "#fdbf6f", "#ff7f00", "#b2df8a",
                                       "#cab2d6", "#a6cee3", "#1f78b4",   
                                       "#33a02c", "#fb9a99", "#e31a1c",  
                                       "#fdbf6f", "#ff7f00", "#cab2d6"),
                      "color.set2" = c("red", "orange", "purple", "yellow",
                                       "blue", "green", rep("black",10)))
kMathOptions <- c("mean", "sum", "median")
kTopBottomOptions <- c("Top%", "Bottom%") #_TODO change to inclusive exclusive 
kTopBottomNum <- c(100, 75, 50, 25, 10, 5)
# plot lines and ticks with lables
list.plot.lines <- list("543 bins 20,20,40" = c(15.5, 45.5, 20.5, 40.5),
                        "543 bins 10,10,10" = c(5.5, 25.5, 10.5, 20.5))
list.plot.ticks <- list("543 bins 20,20,40" = 
  list('name' = c('-1450 -950 -450 +450 +950 +1450 +1950 +2450 +2950 +3450'),
        'loc' = c(1, 6, 11, 50, 55, 60, 65, 70, 75, 80)),
                        "543 bins 10,10,10" = 
  list('name' = c('-450', '+450'),
         'loc' = c(1,30)))
tss.tts.options <- c('TSS', 'PolyA', '500', '500')

# tcl starting values ----
tcl.start.tablefile <- tclVar("Load File")
tcl.start.file.control.tab.names <- tclVar(kNbFileControlTabNames[1])
tcl.start.color.set <- tclVar(names(kListColorSet)[1])
tcl.start.line.option <- tclVar(kLineOptions[1])
tcl.start.dot.option <- tclVar(kDotOptions[1])
tcl.start.top.bottom.option <- tclVar(kTopBottomOptions[1])
tcl.start.top.bottom.num <- tclVar(kTopBottomNum[5])
tcl.start.color <- tclVar(kListColorSet[[1]][1])
tcl.start.math.option <- tclVar(kMathOptions[1])
tcl.start.norm.bin <- tclVar(0)
tcl.start.plot.line.name <- tclVar(names(list.plot.lines)[1])
tcl.start.file.compare.names <- tclVar(kNbFileControlTabNames[1])
tcl.header <- tclVar('')
tcl.one.tss.tts.option <- tclVar(tss.tts.options[1])
tcl.two.tss.tts.option <- tclVar(tss.tts.options[2])
tcl.three.tss.tts.option <- tclVar(tss.tts.options[3])
tcl.four.tss.tts.option <- tclVar(tss.tts.options[4])
tcl.start.lable.plot.ticks <- tclVar(list.plot.ticks[[1]][[1]])
tcl.start.pos.one.line <- tclVar(list.plot.lines[[1]][1])
tcl.start.pos.two.line <- tclVar(list.plot.lines[[1]][2])
tcl.start.pos.three.line <- tclVar(list.plot.lines[[1]][3])
tcl.start.pos.four.line <- tclVar(list.plot.lines[[1]][4])
tcl.start.pos.plot.ticks <- tclVar(list.plot.ticks[[1]][[2]])
tcl.checkbox.relative.frequency <- tclVar(0)
tcl.checkbox.log2 <- tclVar(0)
tcl.checkbox.use.colorset <- tclVar(0)

# test function ----
OnOk <- function(){
  print("this works")
}

# entry frame functions ----

# keeps numbers, empty string for the rest
# from https://github.com/gsk3/taRifx/blob/master/R/Rfunctions.R#L1161
Destring <- function(x, keep = "0-9.-") {
  return(as.numeric(gsub(paste("[^", keep, "]+", sep=""), "", x)))
}

#moves all items from one list to the other
MoveAllToOtherEntry <- function(onlist, offlist, direction, workinglist){
  if (direction == "on"){
    tkdelete(offlist, 0, 'end')
    tkdelete(onlist, 0, 'end')
    tkconfigure(onlist, listvariable = tclVar(names(list.tablefile)))
    lapply(names(list.tablefile), 
           function(i) list.genefile.info[[workinglist]][[i]][6] <<- 1)
    }
     
  if (direction == "off"){
    tkdelete(offlist, 0, 'end')
    tkdelete(onlist, 0, 'end')
    tkconfigure(offlist, listvariable = tclVar(names(list.tablefile)))
    lapply(names(list.tablefile), 
           function(i) list.genefile.info[[workinglist]][[i]][6] <<- 0)
  }
}

#moves selected items from one list to the other
MoveSelectToOtherEntry <- function(onlist, offlist, workinglist){
  for (i in rev(as.integer(tkcurselection(onlist)))){
    tkinsert(offlist, "end", tclvalue(tkget(onlist, i)))
    list.genefile.info[[workinglist]][[tclvalue(tkget(onlist, i))]][6] <<- 0
    tkdelete(onlist, i)
  }
  for (i in rev(as.integer(tkcurselection(offlist)))){
    tkinsert(onlist, "end", tclvalue(tkget(offlist, i)))
    list.genefile.info[[workinglist]][[tclvalue(tkget(offlist, i))]][6] <<- 1
    tkdelete(offlist, i)
  }
}

# updates items in lists when a file is removed
UpdateEntrys <- function(onlist, offlist){
  keep.on <- NULL
  keep.off <- NULL
  lapply(list.genefile.info, function(i) lapply(i, function(j) 
    ifelse(j[6] == 1, 
           keep.on <<- c(keep.on,j[1]), 
           keep.off <<- c(keep.off,j[1]))))
  tkconfigure(onlist, listvariable=tclVar(as.character(keep.on)))
  tkconfigure(offlist, listvariable=tclVar(as.character(keep.off)))
}
  

# read in /remove files functions ----

# reads in file, tests, fills out info 
LoadTableFile <- function() {
  if (control.state[3] == 0) {
    if (is.null(names(list.tablefile))) {
      file.count <- 0
    } else {
      file.count <- length(list.tablefile)
    }
    tk2notetab.select(notebook.on.off, "Common\nGenes") # change tab
    control.state[3] <<- 1
    if (file.count > 10) { 
      tkmessageBox(message = "I have too many files, 
                   you need to reset me or remove some files")
      control.state[3] <<- 0
      return ()
    }
    tcl("wm", "attributes", root, topmost = F)
    pb <- tkProgressBar(title = "Loading file, please be patient!!",
                        width = 300)
    full.file.name <- tclvalue(tkgetOpenFile(filetypes = 
                                        "{{Table Files} {.table .tab .Table}}"))
    file.name <- paste(strsplit(as.character(full.file.name), 
                        '/')[[1]][(length(strsplit(as.character(full.file.name), 
                        '/')[[1]]))])
    if (!nchar(full.file.name)) { ## file select test
      close(pb)
      control.state[3] <<- 0
      return ()
    } else if (file.name %in% names(list.tablefile)) {
      tkmessageBox(message = "This file has already been loaded")
      control.state[3] <<- 0
      close(pb)
      tcl("wm", "attributes", root, topmost = TRUE)
      return ()
    } else {
      legend.nickname <- strsplit(unlist(strsplit(as.character(file.name), 
                                                  '.tab')[[1]][1]), '[.]')[[1]]
      legend.nickname <- legend.nickname[floor(mean(seq_along(legend.nickname)))]
      tablefile <- read.table(full.file.name, header = TRUE, 
                              stringsAsFactors = FALSE, comment.char = "")
      names(tablefile)[1] <- paste("gene")
      num.bins <- dim(tablefile)
      if (file.count > 0) {
        if (num.bins[2] != length(list.tablefile[[1]])) {
          close(pb)
          tkmessageBox(message = "Can't load file, different number of bins")
          control.state[3] <<- 0
          tcl("wm", "attributes", root, topmost = TRUE)
          return ()
        }
        gene.names <- c(list.genefile$main$common, unique(tablefile[ ,1]))
        gene.names <- gene.names[duplicated(gene.names)]
        if (length(gene.names) > 0) {
          list.genefile$main$common <<- gene.names
        } else {
          close(pb)
          tkmessageBox(message = "Can't load file, no genes in common or 
                                  remake your table files all the same way.")
          control.state[3] <<- 0
          tcl("wm", "attributes", root, topmost = TRUE)
          return ()
        }
      } else {  # first time loading a file set up
        tkconfigure(combobox.norm.bin, values=c(0:(num.bins[2] - 1)))
        list.genefile$main$common <<- unique(tablefile[ ,1])
      }
      file.count <- file.count + 1
      list.tablefile.info[file.name] <<- list(c(full.file.name, 
              paste(" % NA's = ", round((sum(is.na(tablefile)) / 
                   (num.bins[1] * num.bins[2])) * 100, digits = 2)), 
              paste(" % Zeors = ", round((sum(tablefile == 0) / 
                    (num.bins[1] * num.bins[2])) * 100, digits = 2))))
      
      color.safe <- file.count %% length(kListColorSet[[control.state[1]]])
      if (color.safe == 0) {
        color.safe <- file.count
      }
      
      list.genefile.info$main[file.name] <<- list(c(file.name, 
              legend.nickname, kDotOptions[1], kLineOptions[1],
                kListColorSet[[control.state[1]]][color.safe], 1))
      list.tablefile[file.name] <<- list(tablefile)
      tkinsert(listbox.common.on, 'end', file.name)
    }
    tcl("wm", "attributes", root, topmost = TRUE)
    control.state[3] <<- 0  
    tkconfigure(combobox.file, values = sapply(list.genefile.info$main, "[[", 1))
    tkconfigure(combobox.numerator, 
                values = sapply(list.genefile.info$main, "[[", 1))
    tkconfigure(combobox.denominator, 
                values = sapply(list.genefile.info$main, "[[", 1))
    tkset(combobox.file, last(sapply(list.genefile.info$main, "[[", 1)))
    ComboboxsUpdate()
    close(pb)
  }
}

#removes file
RemoveFile <- function() {
  if (!is.null(names(list.tablefile)) & control.state[3] == 0 & 
      length(names(list.tablefile)) > 1) {
    list.tablefile[[num]] <<- NULL
    list.tablefile.info[[num]] <<- NULL
    gene.names <- NULL
    lapply(names(list.tablefile), function(i) gene.names <<- c(gene.names, 
                                            unique(list.tablefile[[i]][ ,1])))
    if (length(names(list.tablefile)) > 1) {
      gene.names <- gene.names[duplicated(gene.names)]
    }
      list.genefile$main$common <<- gene.names
    # update list.genefile[[]]$common all but $main ... look at rapply
    sapply(names(list.genefile.info), 
           function(i) list.genefile.info[[i]][[num]] <<-NULL)
    UpdateEntrys(listbox.common.on, listbox.common.off)
    tkconfigure(combobox.file, values = sapply(list.genefile.info$main, "[[", 1))
    tkset(combobox.file, names(list.tablefile)[1])
    return (ComboboxsUpdate())
  }
  # TODO if last one ask for reset
}

#get file to add more custome color options
GetColor <- function() {
  if (control.state[3] == 0) {
     control.state[3] <<- 1
     tcl("wm", "attributes", root, topmost = FALSE)
    
    full.file.name <- tclvalue(tkgetOpenFile(filetypes = 
                                          "{{color.txt Files} {.color.txt}}"))
    if (!nchar(full.file.name)) { ## file select test
      control.state[3] <<- 0
      return ()
    } else if (full.file.name %in% names(kListColorSet)) {
      tkmessageBox(message = "This file has already been loaded")
      control.state[3] <<- 0
      tcl("wm", "attributes", root, topmost = TRUE)
      return ()
    } else {
      split.file.name <- strsplit(as.character(full.file.name), '/') 
      file.name <- paste(split.file.name[[1]][(length(split.file.name[[1]]))])
      file.nickname <- paste(strsplit(as.character(file.name), '.txt')[[1]][1])
      color.file <- read.table(full.file.name, header = FALSE, 
                               stringsAsFactors = FALSE)
      # checks if in rgb format and converts to hex
      sapply(seq_along(color.file[[1]]), function(i) 
        if (suppressWarnings(!is.na(as.numeric(
          substr(color.file[[1]][i], 1, 1)))) == TRUE) { 
          red.green.blue <- strsplit(color.file[[1]][i], ",")
          color.file[[1]][i] <<- rgb(as.numeric(red.green.blue[[1]])[1], 
                        as.numeric(red.green.blue[[1]])[2], 
                        as.numeric(red.green.blue[[1]])[3], 
                        maxColorValue = 255)})
      
      kListColorSet[file.nickname] <<- color.file
      tkconfigure(combobox.color.sets, values = names(kListColorSet))
      tkset(combobox.color.sets, file.nickname)
      control.state[3] <<- 0
      tcl("wm", "attributes", root, topmost = TRUE)
      return (ComboboxsColorSets())
    }
  }
}

# make normalized file ... devide one by the other
MakeNormFile <- function() {
  nom <- as.character(tclvalue(tkget(combobox.numerator)))
  dnom <- as.character(tclvalue(tkget(combobox.denominator)))
  # TODO change to more streamed lined and split and divide on len
  if (nom != "numerator" & dnom != "denominator") {  
    gene.names <- c(list.tablefile[[nom]]["gene"], 
                    list.tablefile[[dnom]]["gene"])
    new.gene.list <- data.frame(inner_join(list.tablefile[[nom]], 
                                     list.tablefile[[dnom]], by = "gene"), 
                          stringsAsFactors = FALSE)
    new.gene.list[is.na(new.gene.list)] <- 0
    len <- (dim(new.gene.list)[2]-1)/2
    new.min.for.na <- min(new.gene.list[, -1][new.gene.list[ ,-1] > 0])/2
    new.gene.list[ ,-1] <- as.data.frame(lapply(new.gene.list[ ,-1], 
             function(x) {replace(x, x == 0, new.min.for.na)}), 
             stringsAsFactors = FALSE)
    new.tablefile <- data.frame(gene = new.gene.list[ ,1], 
                       new.gene.list[ ,2:(len + 1)] / 
                       new.gene.list[ ,(len + 2):((len * 2) + 1)], 
                       stringsAsFactors = FALSE)
    file.name <- paste(nom, dnom, sep = "/")
    file.nickname <- paste(list.genefile.info$main[[nom]][2], 
                           list.genefile.info$main[[dnom]][2], sep = "/")
    list.tablefile[[file.name]] <<- new.tablefile
    file.count <- length(list.tablefile)
    list.tablefile.info[[file.name]] <<- c(file.name, 
                                           paste(" 0's, NA's now = min/2"),
                                           paste(new.min.for.na))
  
    color.safe <- file.count %% length(kListColorSet[[control.state[1]]])
    if (color.safe == 0 ) {
      color.safe <- file.count
    }
    list.genefile.info$main[[file.name]] <<- c(file.name, file.nickname, 
              kDotOptions[1], kLineOptions[1], 
              kListColorSet[[control.state[1]]][color.safe], 1)
  
    tkinsert(listbox.common.on, 'end', file.name)
    tkconfigure(combobox.file, values = sapply(list.genefile.info$main, "[[", 1))
    tkconfigure(combobox.numerator, 
                values = sapply(list.genefile.info$main, "[[", 1))
    tkconfigure(combobox.denominator, 
                values = sapply(list.genefile.info$main, "[[", 1))
    tkset(combobox.file, last(sapply(list.genefile.info$main, "[[", 1)))
    tkset(combobox.numerator, "numerator")
    tkset(combobox.denominator, "denominator")
    ComboboxsUpdate()
  }  
}

# gene list functions ----

# displays the selected gene lists genes
ShowGenes <- function() {
  if(control.state[3] == 1 | is.null(names(list.tablefile))) {
    return ()
  } else {    
    combo.name.gene.file <- as.character(tclvalue(tkget(combobox.gene.compare)))
    # TODO do I need this and/orto fix this?
    if (combo.name.gene.file != tclvalue(tcl.start.file.compare.names)) { 
      # TODO add something like tkconfigure(listbox.show.genes, 
        # listvariable = tclVar(as.character(
        # list.genefile[[combo.name.gene.file]]$common)))
      return ()     
    } else {
      tkdelete(entry.title.show.genes, 0, 'end')
      tkdelete(listbox.show.genes, 0 ,'end')
      tkconfigure(listbox.show.genes, 
              listvariable = tclVar(as.character(list.genefile$main$common)))
      tkinsert(entry.title.show.genes, 0, 
              paste('common genes,  n = ', length(list.genefile$main$common)))
    }
  }
}

# TODO update and make work with new build
# sorts active gene list contain top % signal based on selected bins and file
SortTop <- function(filelist1, filelist2, maingenelist, genelist1, genelist2, 
                    genelistcount1, genelistcount2, tablefile, startbin1, 
                    startbox1, stopbin1, stopbox1, number) {
  if (BusyTest()) {
    CloseSubWindows(c(3,4))
    control.state[5] <<- 1
    pb <<- tkProgressBar(title = "be patient!!",  
                         min = 0, max = 100, width = 300)
    file.count <- as.integer(tksize(filelist1))
    file_count2 <- as.integer(tksize(filelist2))
    sel <- NULL
    my_list2 <- NULL
    for(d in 1 : file_count2){
      my_list2 <- c(my_list2, tclvalue(tkget(filelist2, (d-1))))
    }
    for(f in 1 : file.count){
      if(my_list2[1] %in% tclvalue(tkget(filelist1, (f-1))))
        sel <- c(sel, f)
    }
    for(f in 1 : file.count){
      if(my_list2[2] %in% tclvalue(tkget(filelist1, (f-1))))
        sel <- c(sel, f)
    }
    num.bins <- length(tablefile[[1]]) - 1 
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
    if(R_stop_bin1 < 1 | R_stop_bin1 < R_start_bin1 | R_stop_bin1 > num.bins){
      R_stop_bin1 <- num.bins
      tkdelete(stopbox1, 0, 'end')
      tkinsert(stopbox1, "end", num.bins)
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
    control.state[5] <<- 0
    return()
  }
  return()
}	


# plotting functions ----

# Makes data frame and gathers plot settings for plotting active samples
MakeDataFrame <- function() {
  if (control.state[3] == 1 | is.null(names(list.tablefile))) {
    return ()
  } else {
    use.col <- NULL
    use.dot <- NULL
    use.line <- NULL
    use.nickname <- NULL
    list.wide.data.frame <- list()
    for (i in names(list.genefile)) {
      # checks to see if at least one file in list is acitve
      if (sum(as.numeric(sapply(list.genefile.info[[i]], "[[", 6))) == 0) { 
        return ()
      } else {
        enesg <- data.frame(gene=list.genefile[[i]][[1]], 
                            stringsAsFactors = FALSE)
        lapply(names(list.tablefile), function(k) 
          # uses only acive lists  
          if (as.numeric(list.genefile.info[[i]][[k]][6]) == 1) {        
            list.wide.data.frame[[k]] <<- data.frame(inner_join(enesg, 
                  list.tablefile[[k]], by = "gene"), stringsAsFactors = FALSE)
            dot <- which(kDotOptions == list.genefile.info[[i]][[k]][3])
            if (dot > 20) {
              dot <- 0
            }
            line <- which(kLineOptions == list.genefile.info[[i]][[k]][4])
            if (line > 6) {
              line <- 0
            }
                 use.col <<- c(use.col, list.genefile.info[[i]][[k]][5])
                 use.dot <<- c(use.dot, dot)
                use.line <<- c(use.line, line)
            use.nickname <<- c(use.nickname, 
                          gsub("/", " / \n", list.genefile.info[[i]][[k]][2]))
          }
        )
      } 
    }
  }
  if (!is.null(names(list.wide.data.frame))) {
    ApplyMath(list.wide.data.frame, use.col, use.dot, use.line, use.nickname)
  }
}

# Applys math to long list
ApplyMath <- function(list.wide.data.frame, use.col, use.dot, use.line,
                      use.nickname) {
  
  # math set and y lable
  use.math <- as.character(tclvalue(tkget(combobox.math)))
  if (use.math == "mean") {
    use.apply <- function(x) colMeans(x, na.rm = TRUE)
    use.y.lable <- "Mean of bin counts"
  } else if (use.math == "sum") {
    use.apply <- function(x) colSums(x, na.rm = TRUE)
    use.y.lable <- "Sum of bin counts"
  } else if (use.math == "median") {
    use.apply <- function(x) apply(x, 2, median, na.rm = TRUE)
    use.y.lable <- "Median of bin counts"
  } 
  
  # set normilization to relative frequency or bin number or 1 for none, 
  # and update y lable
  checkbox.relative.frequency <- as.character(tclvalue(
    tcl.checkbox.relative.frequency))
  norm.bin <- as.numeric(tclvalue(tcl.start.norm.bin))
  
  if (checkbox.relative.frequency == 1 & norm.bin == 0) {
    use.y.lable <- paste(strsplit(use.y.lable, split = " ")[[1]][1], 
                         "bins : Relative Frequency")
    norm <- lapply(seq_along(list.wide.data.frame), 
                    function(i) sum(use.apply(list.wide.data.frame[[i]][ ,-1])))
  } else if (norm.bin > 0) {
    if (checkbox.relative.frequency == 1) {
      tcl.checkbox.relative.frequency <<- tclVar(0)
      tkconfigure(checkbox.relative.frequency, variable = tcl.checkbox.relative.frequency)
    }
    use.y.lable <- paste(strsplit(use.y.lable, split = " ")[[1]][1], 
                         "bins : Normalize to bin ", norm.bin)
    norm <- as.numeric(lapply(seq_along(list.wide.data.frame), 
              function(i) use.apply(list.wide.data.frame[[i]][ ,-1])[norm.bin]))
  } else {
    norm <- lapply(seq_along(list.wide.data.frame), function(i) 1)
  }
  
  # update y lable if log2
  if (tclvalue(tcl.checkbox.log2) == 1) {
    use.y.lable <- paste("log2(", use.y.lable, ")", sep = "")
  }
  use.x.lable <- paste("bins \n n = ", length(list.genefile$main$common))
  list.applied.math.data.frame <- lapply(seq_along(list.wide.data.frame), 
    function(i) 
      data.frame(bin = (seq_along(list.wide.data.frame[[i]][-1])), 
                 set = (as.character(use.nickname[i])), 
               value = use.apply(list.wide.data.frame[[i]][,-1]) / norm[[i]], 
               stringsAsFactors = FALSE)) 
  list.long.data.frame <- rbind_all(list.applied.math.data.frame)
  
  use.pos.plot.ticks <- Destring(unlist(strsplit(tclvalue(
    tcl.start.pos.plot.ticks), " ")))
  use.lable.plot.ticks <- unlist(strsplit(tclvalue(tcl.start.lable.plot.ticks), 
                                          " "))
  if (length(use.lable.plot.ticks) != length(use.pos.plot.ticks)) {
    tkmessageBox(message = "The number of Positions and Lables are unequal")
    return ()
  }
  
  use.plot.breaks <- c(Destring(tclvalue(tcl.start.pos.one.line)), 
                       Destring(tclvalue(tcl.start.pos.two.line)), 
                       Destring(tclvalue(tcl.start.pos.three.line)), 
                       Destring(tclvalue(tcl.start.pos.four.line)),
                       use.pos.plot.ticks)
  
  use.plot.breaks.lables <- c(tclvalue(tcl.one.tss.tts.option), 
                              tclvalue(tcl.two.tss.tts.option),
                              tclvalue(tcl.three.tss.tts.option),
                              tclvalue(tcl.four.tss.tts.option),
                              use.lable.plot.ticks)
  use.virtical.line.type <- c(3,3,1,1)  # TODO add change virtical line type
  use.virtical.line.color <- c("green", "red", "black", "black")
  use.plot.breaks.lables <- use.plot.breaks.lables[!is.na(use.plot.breaks)]
  use.virtical.line.type <- use.virtical.line.type[!is.na(use.plot.breaks[1:4])]
  use.virtical.line.color <- use.virtical.line.color[!is.na(
                             use.plot.breaks[1:4])]
  use.virtical.line <- use.plot.breaks[1:4][!is.na(use.plot.breaks[1:4])]
  use.plot.breaks <- use.plot.breaks[!is.na(use.plot.breaks)]
  
  # TODO need controls?
  virtical.line.data.frame <- data.frame(use.virtical.line, 
                                         use.virtical.line.type, 
                                         use.virtical.line.color, 
                                         stringsAsFactors = FALSE)
  names(use.col) <- use.nickname
  names(use.dot) <- use.nickname
  names(use.line) <- use.nickname
  GGplotF(list.long.data.frame, use.col, use.dot, use.line, use.y.lable, 
          use.x.lable, use.plot.breaks, virtical.line.data.frame, 
          use.plot.breaks.lables)
}

# ggplot function
GGplotF <- function(list.long.data.frame, use.col, use.dot, use.line,
                    use.y.lable, use.x.lable, use.plot.breaks, 
                    virtical.line.data.frame, use.plot.breaks.lables) {
  if (tclvalue(tcl.checkbox.log2) == 1) {
    gp <- ggplot(list.long.data.frame, aes(x = bin, y = log2(value), 
                                    color = set, shape = set, linetype = set))
  } else {
    gp <- ggplot(list.long.data.frame, aes(x = bin, y = value, color = set, 
                                           shape = set, linetype = set))
  }
  gp <- gp +
   
    geom_line(size = 1.5) + geom_point(size = 4) +  
    scale_color_manual(name = "Sample", values = use.col) +
    scale_shape_manual(name = "Sample", values = use.dot) + 
    scale_linetype_manual(name = "Sample", values = use.line) +
    xlab(use.x.lable) + ylab(use.y.lable) +  # Set axis labels
    ggtitle(tclvalue(tcl.header)) +  # Set title
    scale_x_continuous(breaks = use.plot.breaks, 
                       labels = use.plot.breaks.lables) +
  
    geom_vline(data = virtical.line.data.frame, 
           aes(xintercept = use.virtical.line), 
                     size = 2, 
                 linetype = virtical.line.data.frame$use.virtical.line.type, 
                    color = virtical.line.data.frame$use.virtical.line.color) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    theme(plot.title = element_text(size = 30, vjust = 2)) +
    theme(axis.title.y = element_text(size =  20, vjust = 1.5)) +
    theme(axis.title.x = element_text(size =  25, vjust = 0)) +
    theme(axis.text.x = element_text( size = 10,  angle = -45, hjust = .1, 
                                     vjust = .8,   face = 'bold'))
 print(gp)
}

# combobox functions ----

# updates comboboxs and lists 
ComboboxsUpdate <- function() {
  num <- tclvalue(tkget(combobox.file))
  if (!is.null(names(list.tablefile)) & control.state[3] == 0) {
    tkset(combobox.color, sapply(list.genefile.info$main, "[[", 5)[num])
    tkset(combobox.lines, sapply(list.genefile.info$main, "[[", 4)[num])
    tkset(combobox.dots, sapply(list.genefile.info$main, "[[", 3)[num])
    tkdelete(entry.nickname, 0, 'end')
    tkinsert(entry.nickname,  0, sapply(list.genefile.info$main, "[[", 2)[num])
    tkconfigure(listbox.stats, listvariable = 
                  tclVar(as.character(list.tablefile.info[[num]][2:3])))
  } 
}

#keeps genelist cbb and tabs in sync
ComboboxsGeneSet <- function() {
  num2 <- unlist(strsplit(tclvalue(tkget(combobox.gene.set)), " ")[[1]])
  num3 <- unlist(strsplit(paste(num2[1], "\n", num2[2], " ", num2[3], sep = ""),
                          " NA"))
  tk2notetab.select(notebook.on.off, num3)
  return (ComboboxsUpdate())
}

#keeps genelist cbb and tabs in sync
ComboboxsGeneSet2 <- function() {
  tkset(combobox.gene.set, paste(strsplit(tk2notetab.text(notebook.on.off), "\n")[[1]], 
                            collapse = " "))
  return (ComboboxsUpdate())
}

# saves current seletion
ComboboxsUpdateVaribles <- function() {
  num <- tclvalue(tkget(combobox.file))
  if(!is.null(names(list.tablefile)) & num != "" & control.state[3] == 0){
    list.genefile.info$main[[num]][5] <<- tclvalue(tkget(combobox.color))
    list.genefile.info$main[[num]][4] <<- tclvalue(tkget(combobox.lines))
    list.genefile.info$main[[num]][3] <<- tclvalue(tkget(combobox.dots))
    list.genefile.info$main[[num]][2] <<- tclvalue(tkget(entry.nickname))
  } 
}

# Adds ability to change color sets
ComboboxsColorSets <- function() {
  if (control.state[3] == 0) {
    num <- as.numeric(tclvalue(tcl(combobox.file, "current"))) + 1
    num1 <- length(list.tablefile)
    num2 <- length(kListColorSet[[tclvalue(tkget(combobox.color.sets))]])
    if (!is.numeric(num2) | num2 < 1){
      return ()
    }
    control.state[1] <<- tclvalue(tkget(combobox.color.sets))
    if (num < 1) {
      num <- 1
    }
    
    if (num1 > 0 & as.character(tclvalue(tcl.checkbox.use.colorset)) == 1) {
      lapply(seq_along(list.tablefile), function(i) 
        list.genefile.info$main[[i]][5] <<- kListColorSet[[tclvalue(tkget(
          combobox.color.sets))]][ifelse(i %% num2 > 0, i %% num2, i)])  
      color.safe <- num %% num2
      if (color.safe == 0 ) {
        color.safe <- num
      }
      tkset(combobox.color, kListColorSet[[tclvalue(tkget(
        combobox.color.sets))]][color.safe])  
    }  
    tkconfigure(combobox.color, values = kListColorSet[[tclvalue(tkget(
      combobox.color.sets))]])  
  }
}

# Change plot lables with lines
PlotLines <- function() {
  num <- tclvalue(tkget(combobox.plot.lines))
  tkdelete(entry.line.tick.pos.one, 0, 'end')
  tkinsert(entry.line.tick.pos.one, 0, list.plot.lines[[num]][1])
  tkdelete(entry.line.tick.pos.two, 0, 'end')
  tkinsert(entry.line.tick.pos.two, 0, list.plot.lines[[num]][2])
  tkdelete(entry.line.tick.pos.three, 0, 'end')
  tkinsert(entry.line.tick.pos.three, 0, list.plot.lines[[num]][3])
  tkdelete(entry.line.tick.pos.four, 0, 'end')
  tkinsert(entry.line.tick.pos.four, 0, list.plot.lines[[num]][4])
  tkdelete(entry.line.tick.pos.five, 0, 'end')
  tkinsert(entry.line.tick.pos.five, 0, list.plot.ticks[[num]][["loc"]])
  tkdelete(entry.line.tick.lable.five, 0, 'end')
  tkinsert(entry.line.tick.lable.five, 0, list.plot.ticks[[num]][["name"]])
}

# GUI function ----

# set up root window ---- 

root <- tktoplevel() #container for it all
tcl("wm", "attributes", root, topmost = TRUE)
tkwm.title(root, version.num)

# menu setup ----
menu.top <- tk2menu(root)           # Create a menu
tkconfigure(root, menu = menu.top)  # Add it to the main window
menu.top.file <- tkmenu(menu.top, tearoff = FALSE)
tkadd(menu.top.file, "command", label = "Load table file", command = function() 
  LoadTableFile())
tkadd(menu.top.file, "command", label = "Load color pallet")
tkadd(menu.top.file, "command", label = "Quit", command = function() 
  tkdestroy(root))
tkadd(menu.top.file, "command", label = "Restart", command = function() 
  tkdestroy(root))
tkadd(menu.top, "cascade", label = "File", menu = menu.top.file)

# create main notebook ----
notebook.main <- tk2notebook(root, tabs = c("Plot", "Genes", "Tools", 
                                            "Plot Options"))
tkgrid(notebook.main, column = 1, row = 0)

# tab for Plot Options  ----
notebook.main.plot.options.tab <- tk2notetab(notebook.main, "Plot Options")

# frame for loading and applying diffrent color groups

tab.plot.options.color.set.frame <- tkframe(notebook.main.plot.options.tab, 
                                       relief = 'ridge', borderwidth = 5)
tkgrid(tab.plot.options.color.set.frame, column = 0, row = 0)

tkgrid(tklistbox(tab.plot.options.color.set.frame, 
                 listvariable = tclVar("colorset"),
                 height = 1, width = 8, relief = 'flat'), padx = c(5, 0))

combobox.color.sets <- tk2combobox(tab.plot.options.color.set.frame, 
                                     value = names(kListColorSet), 
                              textvariable = tcl.start.color.set, 
                                     state = "readonly",
                                     width = 10)
tkgrid(combobox.color.sets, sticky = 'w', padx = c(0,5), column = 1, row = 0)
tkbind(combobox.color.sets, "<<ComboboxSelected>>", ComboboxsColorSets)

tkgrid(tkcheckbutton(tab.plot.options.color.set.frame, 
                     variable = tcl.checkbox.use.colorset, 
                         text = "Update colors" ))

tkgrid(tkbutton(tab.plot.options.color.set.frame, text = ' Load custom color ', 
                command = function() GetColor()), 
                   pady = c(15, 0), columnspan = 2) 

tkgrid(tklistbox(tab.plot.options.color.set.frame, 
                 listvariable = tclVar("Line"), height = 1, width = 4, 
                 relief = 'flat'), padx = c(20, 0))

combobox.lines <- tk2combobox(tab.plot.options.color.set.frame, 
                               value = kLineOptions, 
                        textvariable = tcl.start.line.option, state="readonly",
                        width = 10)
tkgrid(combobox.lines, sticky = "w", column = 1, row = 3, padx = c(0, 16)) 
tkbind(combobox.lines, "<<ComboboxSelected>>", ComboboxsUpdateVaribles)

tkgrid(tklistbox(tab.plot.options.color.set.frame, listvariable = tclVar("dot"),
                 height = 1, width = 4, relief = 'flat'), padx = c(20, 0))

combobox.dots <- tk2combobox(tab.plot.options.color.set.frame, 
                             value = kDotOptions, 
                       textvariable = tcl.start.dot.option, state="readonly", 
                       width = 10) 
tkgrid(combobox.dots, sticky = "w", column = 1, row = 4, padx = c(0, 16)) 
tkbind(combobox.dots, "<<ComboboxSelected>>", ComboboxsUpdateVaribles)

tkgrid(tklabel(tab.plot.options.color.set.frame, text = "Header"), 
       padx = c(5, 1), pady = c(0, 2))
tkgrid(tk2entry(tab.plot.options.color.set.frame, width = 20, 
                textvariable = tcl.header), padx = c(5, 0), pady = c(0, 2), 
       column = 1, row = 5)

tkgrid(tklabel(tab.plot.options.color.set.frame, text = "Header"), 
       padx = c(5, 1), pady = c(0, 2))
tkgrid(tk2entry(tab.plot.options.color.set.frame, width = 20, 
                textvariable = tcl.header),  padx = c(5, 0), pady = c(0, 2), 
       column = 1, row = 6)

tkgrid(tklabel(tab.plot.options.color.set.frame, text = "Header"), 
       padx = c(5, 1), pady = c(0, 2))
tkgrid(tk2entry(tab.plot.options.color.set.frame, width = 20, 
                textvariable = tcl.header), padx = c(5, 0), pady = c(0, 2), 
       column = 1, row = 7)

#frame for line and tick plot options

tab.plot.options.line.tick.frame <- tkframe(notebook.main.plot.options.tab, 
                                            relief = 'ridge', borderwidth = 5) 
tkgrid(tab.plot.options.line.tick.frame)

tkgrid(tklabel(tab.plot.options.line.tick.frame, text = ' Plot Options ', 
               width = 30), padx = c(5, 3), pady = c(0, 2), columnspan = 6)

tkgrid(tklabel(tab.plot.options.line.tick.frame, text = "Plot lines and lables"), 
       pady = c(5, 5), row = 7, column = 0, columnspan = 6)

combobox.plot.lines <- tk2combobox(tab.plot.options.line.tick.frame, 
                                   value = names(list.plot.lines), 
                                   textvariable = tcl.start.plot.line.name, 
                                   state = "readonly", width = 20)
tkgrid(combobox.plot.lines, column = 0, columnspan = 6, row = 7) 
tkbind(combobox.plot.lines, '<<ComboboxSelected>>', PlotLines)

tkgrid(tk2entry(tab.plot.options.line.tick.frame, width = 5, 
                textvariable = tcl.one.tss.tts.option),  
                padx = c(10, 0), pady = c(5, 0), column = 0, row = 8)
tkgrid(tklabel(tab.plot.options.line.tick.frame, text = 'Pos'), padx = c(5, 3), 
       pady = c(5, 0), column = 1, row = 8, sticky = "w")
entry.line.tick.pos.one <- tk2entry(tab.plot.options.line.tick.frame, width = 4, 
                   textvariable = tcl.start.pos.one.line)
tkgrid(entry.line.tick.pos.one, column = 2, row = 8, sticky = "w", 
       padx = c(0, 0), pady = c(5, 0))

tkgrid(tk2entry(tab.plot.options.line.tick.frame, width = 5, 
                textvariable = tcl.two.tss.tts.option),  
       padx = c(10, 0), pady = c(3, 0), column = 0, row = 9)
tkgrid(tklabel(tab.plot.options.line.tick.frame, text = 'Pos'), padx = c(5, 3), 
       pady = c(3, 0), column = 1, row = 9, sticky = "w")
entry.line.tick.pos.two <- tk2entry(tab.plot.options.line.tick.frame, width = 4, 
                                    textvariable = tcl.start.pos.two.line)
tkgrid(entry.line.tick.pos.two, column = 2 , row = 9, sticky = "w", 
       padx = c(0, 0), pady = c(3, 0))

tkgrid(tk2entry(tab.plot.options.line.tick.frame, width = 5, 
                textvariable = tcl.three.tss.tts.option),  
       padx = c(10, 0), pady = c(5, 0), column = 3, row = 8)
tkgrid(tklabel(tab.plot.options.line.tick.frame, text = 'Pos'), 
       padx = c(5, 3), pady = c(5, 0), column = 4, row = 8, sticky = "w")
entry.line.tick.pos.three <- tk2entry(tab.plot.options.line.tick.frame, 
                            width = 4, textvariable = tcl.start.pos.three.line)
tkgrid(entry.line.tick.pos.three, column = 5, row = 8, sticky = "w", 
       padx = c(0, 0), pady = c(3, 0))

tkgrid(tk2entry(tab.plot.options.line.tick.frame, width = 5, 
                textvariable = tcl.four.tss.tts.option),  
       padx = c(10, 0), pady = c(5, 0), column = 3, row = 9)
tkgrid(tklabel(tab.plot.options.line.tick.frame, text = 'Pos'), column = 4, 
       row = 9, sticky = "w", padx = c(5, 3), pady = c(5, 0))
entry.line.tick.pos.four <- tk2entry(tab.plot.options.line.tick.frame, 
                            width = 4, textvariable = tcl.start.pos.four.line)
tkgrid(entry.line.tick.pos.four, column = 5, row = 9, sticky = "w", 
       padx = c(0, 0), pady = c(3, 0))

tkgrid(tklabel(tab.plot.options.line.tick.frame, text = "More Bin labels"), 
       pady = c(4, 3), row = 11, column = 0, columnspan = 6)

tkgrid(tklabel(tab.plot.options.line.tick.frame, text = 'Pos'), padx = c(5, 0), 
       column = 0, row = 12, sticky = "w")
entry.line.tick.pos.five <- tk2entry(tab.plot.options.line.tick.frame, 
                            width = 35, textvariable = tcl.start.pos.plot.ticks)
tkgrid(entry.line.tick.pos.five, column = 1, row = 12, padx = c(0, 0), 
       columnspan = 5, sticky = "w")
tkgrid(tklabel(tab.plot.options.line.tick.frame, text = 'Lable'), 
       padx = c(5, 0), column = 0, row = 13, sticky = "w")
entry.line.tick.lable.five <- tk2entry(tab.plot.options.line.tick.frame, 
                          width = 35, textvariable = tcl.start.lable.plot.ticks)
tkgrid(entry.line.tick.lable.five, column = 1, row = 13, padx = c(0, 0), 
       columnspan = 5)

# frame for constent items ----

frame.common.items <- tkframe(root)


# frame for load file
frame.file.load <- tkframe(frame.common.items, relief = 'ridge', 
                             borderwidth = 5)
tkgrid(frame.file.load, column = 0, row = 0)

tkgrid(tkbutton(frame.file.load, font =c('bold', 20), 
                text = " Load Table File ", 
                command =  function() LoadTableFile()))

# frame for gene set and file options and info
frame.geneset.file.select <- tkframe(frame.common.items, relief = 'ridge', 
                                     borderwidth = 5)
tkgrid(frame.geneset.file.select, column = 0, row = 1)

title.geneset.file.select <- tklistbox(frame.geneset.file.select, height = 1, 
                                       relief = 'flat')
tkgrid(title.geneset.file.select, columnspan = 2)  
tkinsert(title.geneset.file.select, 0, "File options settings")

tkgrid(tklistbox(frame.geneset.file.select, listvariable = tclVar("List"), 
                 height = 1, width = 5, relief = 'flat'), padx = c(5, 0))
combobox.gene.set <- tk2combobox(frame.geneset.file.select, 
                                 value = kNbFileControlTabNames,
                            textvariable = tcl.start.file.control.tab.names, 
                            state = "readonly", width = 25)
tkgrid(combobox.gene.set, column = 1, row = 1)
tkbind(combobox.gene.set, "<<ComboboxSelected>>", ComboboxsGeneSet)

tkgrid(tklistbox(frame.geneset.file.select, listvariable = tclVar("File"), 
                 height = 1, width = 5, relief = 'flat'), padx = c(5, 0))
combobox.file <- tk2combobox(frame.geneset.file.select, 
            textvariable = tcl.start.tablefile, state = "readonly", width = 25)
tkgrid(combobox.file, column = 1, row = 2)
tkbind(combobox.file, "<<ComboboxSelected>>", ComboboxsUpdate)

tkgrid(tklistbox(frame.geneset.file.select, listvariable = tclVar("nickname:"),
                 height = 1, width = 10, relief = 'flat'), padx = c(5, 0))

entry.nickname <- tk2entry(frame.geneset.file.select, width = 30)
tkgrid(entry.nickname, sticky = "w", column = 1, row = 3, padx = c(0, 4))
tkinsert(entry.nickname, 0,  paste(list.genefile.info$main[1]))

tkgrid(tklistbox(frame.geneset.file.select, listvariable = tclVar("color"), 
                 height = 1, width = 5, relief = 'flat'), padx = c(5, 0))

combobox.color <- tk2combobox(frame.geneset.file.select, 
                  value = kListColorSet[[tclvalue(tkget(combobox.color.sets))]], 
                         textvariable= tcl.start.color, state="readonly")
tkgrid(combobox.color, sticky = "w", column = 1, row = 4, padx = c(0, 16))
tkbind(combobox.color, "<<ComboboxSelected>>", ComboboxsUpdateVaribles)

tkgrid(tklistbox(frame.geneset.file.select, listvariable = tclVar("Stats"),
                 height = 1, width = 5, 
                 relief = 'flat'), column = 0, padx = c(5, 0))
listbox.stats <- tklistbox(frame.geneset.file.select, height = 2, width = 30)
tkgrid(listbox.stats, column = 1, row = 5)

tkgrid(tkbutton(frame.geneset.file.select, text = " Remove file ", 
                command =  function() RemoveFile()), columnspan = 2)

# frame for plot button

frame.plot.button <- tkframe(frame.common.items, relief = 'ridge', 
                             borderwidth = 5)
tkgrid(frame.plot.button, column = 0, row = 2)

tkgrid(tkbutton(frame.plot.button, font =c('bold', 23), 
                text = '      Plot       ', 
                command = function() MakeDataFrame())) 
tkgrid(frame.common.items, column = 0, row = 0)

# tab plot ----
notebook.main.plot.tab <- tk2notetab(notebook.main, "Plot")

# box for plot settings
frame.plot.math.settings <- tkframe(notebook.main.plot.tab, relief = 'ridge',
                                    borderwidth = 5)

tkgrid(tklabel(frame.plot.math.settings, text = "Plot Options", width = 35))  

combobox.math <- tk2combobox(frame.plot.math.settings, 
                             value = kMathOptions, 
                             textvariable = tcl.start.math.option, 
                             state = "readonly")
tkgrid(combobox.math, sticky = "n")

checkbox.relative.frequency <- tkcheckbutton(frame.plot.math.settings, 
                   variable = tcl.checkbox.relative.frequency, 
                       text = "Relative Frequency" )
tkgrid(checkbox.relative.frequency)

checkbox.log2 <- tkcheckbutton(frame.plot.math.settings, 
                  variable = tcl.checkbox.log2, text = "log2 transformation")
tkgrid(checkbox.log2)

tkgrid(tklistbox(frame.plot.math.settings, listvariable = tclVar("Norm_to_bin"),
                 height = 1, width = 12, 
                 relief = 'flat'), padx = c(50, 0), sticky = "w")

combobox.norm.bin <- tk2combobox(frame.plot.math.settings,
                textvariable = tcl.start.norm.bin, state="readonly", width = 3)
tkgrid(combobox.norm.bin, sticky = "e", column = 0, row = 5, padx = c(0, 50)) 
tkgrid(frame.plot.math.settings, row = 0, sticky = "n")

# on/off list notebook ---- 
frame.on.off <- tkframe(notebook.main.plot.tab, relief = 'ridge', borderwidth = 5)

notebook.on.off <- tk2notebook(frame.on.off, tabs = c("Common\nGenes", 
                                                      "Gene\nlist 1", 
                                                      "Gene\nlist 2", 
                                                      "Gene\nlist 3",
                                                      "Gene\nlist 4"))
tkgrid(notebook.on.off)

notebook.on.off.common.tab <- tk2notetab(notebook.on.off, "Common\nGenes")
tkbind(notebook.on.off.common.tab, "<Visibility>", ComboboxsGeneSet2)

frame.common.tab <- tkframe(notebook.on.off.common.tab)
tkgrid(frame.common.tab)

tkgrid(tklabel(frame.common.tab, text = "List of table files"), columnspan = 6)
listbox.common.on <- tk2listbox(frame.common.tab, width = 35, height = 8)
tkgrid(listbox.common.on, columnspan = 3)
tkgrid(tkbutton(frame.common.tab, text = "<<Switch>>", command = function() 
  MoveSelectToOtherEntry(listbox.common.on, listbox.common.off, "main")), 
    tkbutton(frame.common.tab, text = "<<All On>>", command = function() 
     MoveAllToOtherEntry(listbox.common.on, listbox.common.off, "on", "main")), 
    tkbutton(frame.common.tab, text = "<<All Off>>", command = function() 
     MoveAllToOtherEntry(listbox.common.on, listbox.common.off, "off", "main")),
       sticky = 'we')
listbox.common.off <- tk2listbox(frame.common.tab, width = 35, height = 8)
tkgrid(listbox.common.off, columnspan = 3)

notebook.on.off.gene1.tab <- tk2notetab(notebook.on.off, "Gene\nlist 1")
tkbind(notebook.on.off.gene1.tab, "<Visibility>", ComboboxsGeneSet2)

notebook.on.off.gene2.tab <- tk2notetab(notebook.on.off, "Gene\nlist 2")
tkbind(notebook.on.off.gene2.tab, "<Visibility>", ComboboxsGeneSet2)

notebook.on.off.gene3.tab <- tk2notetab(notebook.on.off, "Gene\nlist 3")
tkbind(notebook.on.off.gene3.tab, "<Visibility>", ComboboxsGeneSet2)

notebook.on.off.gene4.tab <- tk2notetab(notebook.on.off, "Gene\nlist 4")
tkbind(notebook.on.off.gene4.tab, "<Visibility>", ComboboxsGeneSet2)

tkgrid(frame.on.off, column = 0, row = 1, rowspan = 2)

# tab for display genes ----
notebook.main.genes.tab <- tk2notetab(notebook.main, "Genes")

frame.genes.tab <- tkframe(notebook.main.genes.tab, relief = 'ridge', 
                           borderwidth = 5)
tkgrid(frame.genes.tab, column = 0, row = 0, sticky = "n")

tkgrid(tklistbox(frame.genes.tab, listvariable = tclVar("list:"), height = 1, 
                 width = 4, relief = 'flat'), padx = c(20, 0))
combobox.gene.compare <- tk2combobox(frame.genes.tab, 
                       textvariable = tcl.start.file.compare.names,
                       state="readonly") 
tkgrid(combobox.gene.compare, sticky = "w", column = 1, row = 0, 
       padx = c(0, 16)) 

tkgrid(tkbutton(frame.genes.tab, text = " Show Genes ", 
                command =  function() ShowGenes()), 
       column = 0, row = 1, columnspan =2)

tkgrid(tklistbox(frame.genes.tab, listvariable = tclVar(""), height = 1, 
                 width = 12, 
                 relief = 'flat'), padx = c(20, 0), column = 0, row = 2)

tkgrid(tklistbox(frame.genes.tab, listvariable = tclVar("compare_to:"), 
                 height = 1, width = 12, 
                 relief = 'flat'), padx = c(20, 0), column = 0, row = 3)
combobox.gene.compare2 <- tk2combobox(frame.genes.tab, 
                             textvariable = tcl.start.file.compare.names,
                             state="readonly") 
tkgrid(combobox.gene.compare2, sticky = "w", column = 1, row = 3, 
       padx = c(0, 16)) 

tkgrid(tkbutton(frame.genes.tab, text = " intersect list ", 
                command =  function() OnOk()), 
       column = 0, row = 4, columnspan =2)

tkgrid(tkbutton(frame.genes.tab, text = " save list ", 
                command =  function() OnOk()), 
       column = 0, row = 5, columnspan =2)

frame.show.genes <- tkframe(notebook.main.genes.tab, relief = 'ridge', 
                            borderwidth = 5)
tkgrid(frame.show.genes, column = 0, row = 1, sticky = "n")

entry.title.show.genes <- tklistbox(frame.show.genes, 
                                    listvariable = tclVar("load_list"), 
                 relief = 'flat', height = 1, width = 30)
tkgrid(entry.title.show.genes, column = 0, row = 0)

listbox.show.genes <- tk2listbox(frame.show.genes, height = 20, width = 30)
tkgrid(listbox.show.genes, column = 0, row = 1, sticky = "n")

# tab for tools ----
notebook.main.tools.tab <- tk2notetab(notebook.main, "Tools")

# frame for file deviding 
frame.tools.tab <- tkframe(notebook.main.tools.tab, relief = 'ridge',
                           borderwidth = 5)
tkgrid(frame.tools.tab, column = 0, row = 1)

listbox.title.norm <- tklistbox(frame.tools.tab, height = 1, width = 38, 
                                relief = 'flat')
tkgrid(listbox.title.norm, sticky = "n", columnspan = 2)  
tkinsert(listbox.title.norm, 0, "      select samples for normalization")

listbox.title.div <- tklistbox(frame.tools.tab, height = 1, width = 7, 
                               relief = 'flat')
tkgrid(listbox.title.div, padx = c(20, 0), pady = c(5, 0), column = 0, row = 1)  
tkinsert(listbox.title.div, 0, "divide:")

combobox.numerator <- tk2combobox(frame.tools.tab, state = "readonly")
tkset(combobox.numerator, "numerator")
tkgrid(combobox.numerator, sticky = "w", column = 1, row = 1, padx = c(0, 16), 
       pady = c(5, 0)) 

listbox.title.by <- tklistbox(frame.tools.tab, height = 1, width = 6, 
                              relief = 'flat')
tkgrid(listbox.title.by, padx = c(20, 0), pady = c(0, 5), column = 0, row = 2)  
tkinsert(listbox.title.by, 0, "by:")

combobox.denominator <- tk2combobox(frame.tools.tab, state = "readonly")
tkset(combobox.denominator, "denominator")
tkgrid(combobox.denominator, sticky = "w", column = 1, row = 2, padx = c(0, 16),
       pady = c(0, 5)) 

tkgrid(tkbutton(frame.tools.tab, text = '      Create       ', 
                command = function() MakeNormFile()), columnspan = 2) 

# box for plot settings
frame.sort.tools.tab <- tkframe(notebook.main.tools.tab, relief = 'ridge', 
                                borderwidth = 5)

tkgrid(tklabel(frame.sort.tools.tab, text = "Sort tools", width = 12)) 

tkgrid(tklistbox(frame.sort.tools.tab, listvariable = tclVar(""), 
                 height = 1, width = 1, relief = 'flat'),
       column = 0, row = 1, columnspan = 2)
combobox.top.bottom <- tk2combobox(frame.sort.tools.tab, 
                                   values =  kTopBottomOptions, 
                                   textvariable = tcl.start.top.bottom.option,
                             state = "readonly", width = 10) 
tkgrid(combobox.top.bottom, sticky = "w", column = 0, row = 2, padx = c(5, 0)) 

combobox.top.bottom.num <- tk2combobox(frame.sort.tools.tab, 
                                       values =  kTopBottomNum, 
                                 textvariable = tcl.start.top.bottom.num, 
                                 state = "readonly", width = 3) 
tkgrid(combobox.top.bottom.num, sticky = "we", column = 1, row = 2, 
       padx = c(0, 5), pady = c(10, 10))

tkgrid(tkbutton(frame.sort.tools.tab, text = "   Sort   ", 
                command =  function() OnOk()), 
       column = 0, row = 4, columnspan = 3, pady = c(10, 10))

tkgrid(frame.sort.tools.tab, column = 0, row = 0, sticky = 'n')



# end ----

try(tk2theme("keramik"), silent = TRUE)
#}
#expandTk()
