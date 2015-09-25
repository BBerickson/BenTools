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
# for holding table files in list
list.data <- list(table.file = list(),  # [[]] gene X1 X2 ...
                  gene.file = list(),  # holds common genes from files and gene files
                  gene.info = list())  # for holding gene file info in a list of lists
                                          # c(name, nickname, dot", line", color,
                                          # plot?, stat1, stat2, colorset, #common      

# R varibles ----
# TODO do i use all of these ?
# state of the GUI and control functions
# [1] active color set, 
# [2] allows nickname update, 
# [3] busy? stop user input/activity, 
# [4] number of bins
control.state <- c("color.set1", 0, 0, 0)  

# values for comboboxs ----

kNbFileControlTabNames <- c("Common Genes", "Gene list 1", "Gene list 2",
                            "Gene list 3", "Gene list 4", "Sort tools list", 
                            "Intersect list", "Compare regions", "Compare ratios")
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
kTopBottomOptions <- c("Top%", "Bottom%") 
kInOutOptions <- c("inclusive", "exclusive") 
kTopBottomNum <- c(1, seq(5, 100, by = 5))
kFoldList <- seq(1, 3, by = 0.1)
# plot lines and ticks with labels
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
tcl.tablefile <- tclVar("Load File")
tcl.file.control.tab.names <- tclVar(kNbFileControlTabNames[1])
tcl.file.compare.names1 <- tclVar("Load File")
tcl.file.compare.names2 <- tclVar("Load File")
tcl.file1.regions <- tclVar("Load File")
tcl.file2.regions <- tclVar("Load File")
tcl.color.set <- tclVar(names(kListColorSet)[1])
tcl.line.option <- tclVar(kLineOptions[1])
tcl.dot.option <- tclVar(kDotOptions[1])
tcl.top.bottom.option <- tclVar(kTopBottomOptions[1])
tcl.top.bottom.num <- tclVar(kTopBottomNum[6])
tcl.in.out.option.regions <- tclVar(kInOutOptions[1])
tcl.bin.fold.regions <- tclVar(kFoldList[11])
tcl.color <- tclVar(kListColorSet[[1]][1])
tcl.math.option <- tclVar(kMathOptions[1])
tcl.norm.bin <- tclVar(0)
tcl.bin.start <- tclVar(1)
tcl.bin.end <- tclVar(1)
tcl.bin.start.regions <- tclVar(1)
tcl.bin.end.regions <- tclVar(1)
tcl.plot.line.name <- tclVar(names(list.plot.lines)[1])
tcl.header <- tclVar('')
tcl.one.tss.tts.option <- tclVar(tss.tts.options[1])
tcl.two.tss.tts.option <- tclVar(tss.tts.options[2])
tcl.three.tss.tts.option <- tclVar(tss.tts.options[3])
tcl.four.tss.tts.option <- tclVar(tss.tts.options[4])
tcl.label.plot.ticks <- tclVar(list.plot.ticks[[1]][[1]])
tcl.pos.one.line <- tclVar(list.plot.lines[[1]][1])
tcl.pos.two.line <- tclVar(list.plot.lines[[1]][2])
tcl.pos.three.line <- tclVar(list.plot.lines[[1]][3])
tcl.pos.four.line <- tclVar(list.plot.lines[[1]][4])
tcl.pos.plot.ticks <- tclVar(list.plot.ticks[[1]][[2]])
tcl.checkbox.relative.frequency <- tclVar(0)
tcl.checkbox.log2 <- tclVar(0)

# test function ----
OnOk <- function(){
  print("this works")
}

# entry frame functions ----

#moves all items from one list to the other
MoveAllToOtherEntry <- function(onlist, offlist, direction){
  file.name <- ComboboxSelectionHelper()
  if (file.name == "list of table files") {
    return ()
  }
  
  if (direction == "on"){
    tkdelete(offlist, 0, 'end')
    tkdelete(onlist, 0, 'end')
    tkconfigure(onlist, listvariable = tclVar(names(list.data$table.file)))
    lapply(names(list.data$table.file), 
           function(i) list.data$gene.info[[file.name]][[i]][6] <<- 1)
    }
     
  if (direction == "off"){
    tkdelete(offlist, 0, 'end')
    tkdelete(onlist, 0, 'end')
    tkconfigure(offlist, listvariable = tclVar(names(list.data$table.file)))
    lapply(names(list.data$table.file), 
           function(i) list.data$gene.info[[file.name]][[i]][6] <<- 0)
  }
}

#moves selected items from one list to the other
MoveSelectToOtherEntry <- function(onlist, offlist){
  file.name <- ComboboxSelectionHelper()
  if (file.name == "list of table files") {
    return ()
  }
  
  for (i in rev(as.integer(tkcurselection(onlist)))){
    tkinsert(offlist, "end", tclvalue(tkget(onlist, i)))
    list.data$gene.info[[file.name]][[tclvalue(tkget(onlist, i))]][6] <<- 0
    tkdelete(onlist, i)
  }
  for (i in rev(as.integer(tkcurselection(offlist)))){
    tkinsert(onlist, "end", tclvalue(tkget(offlist, i)))
    list.data$gene.info[[file.name]][[tclvalue(tkget(offlist, i))]][6] <<- 1
    tkdelete(offlist, i)
  }
}

# updates items in lists when tab is displaid
UpdateEntrys <- function(list.item){
  onlist <- get(paste("listbox." , list.item, ".on", sep = "")) 
  offlist <- get(paste("listbox." , list.item, ".off", sep = ""))
  if (list.item != "common" && list.item != "sort" && list.item != "intersect"){
    genelist <- tclvalue(tkcget(get(paste("label." , list.item, ".file", 
                            sep = "")), text = NULL))
  } else {
    genelist <- list.item
  }
  genelength <- get(paste("label." , list.item, ".length", sep = ""))
  
  keep.on <- NULL
  keep.off <- NULL
  tkdelete(onlist, 0, "end")
  tkdelete(offlist, 0, "end")
  lapply(list.data$gene.info[[genelist]], function(j) 
    ifelse(j[6] == 1, 
           keep.on <<- c(keep.on,j[1]), 
           keep.off <<- c(keep.off,j[1])))
  tkconfigure(genelength, 
              text = paste("n = ", length(list.data$gene.file[[list.item]])))
  tkconfigure(onlist, listvariable=tclVar(as.character(keep.on)))
  tkconfigure(offlist, listvariable=tclVar(as.character(keep.off)))
}
  

# helper functions ----

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

# helper for getting selected set 
ComboboxSelectionHelper <- function() {
  num <- as.numeric(tclvalue(tcl(combobox.gene.set, "current")))
  if (num == 0) {
    file.name <- "common"
  } else if (num == 1) {
    file.name <- tclvalue(tkcget(label.gene1.file, text = NULL))
  } else if (num == 2) {
    file.name <- tclvalue(tkcget(label.gene2.file, text = NULL))
  } else if (num == 3) {
    file.name <- tclvalue(tkcget(label.gene3.file, text = NULL))
  } else if (num == 4) {
    file.name <- tclvalue(tkcget(label.gene4.file, text = NULL))
  } else if (num == 5) {
    file.name <- "sort"
  } else if ( num == 6) {
    file.name <- "intersect"
  } else if (num == 7) {
    file.name <- "regions"
  } else if (num == 8) {
    file.name <- "ratios"
  }
  return (file.name)
}

# helper for keeping start and end bins in check
BinStartEndHelper <- function(start.bin, end.bin, combo.start, combo.end, 
                              num) {
  start <- as.numeric(tclvalue(start.bin))
  end <- as.numeric(tclvalue(end.bin))
  if (num == 1 && start > end) {
    tkset(combo.end, start)
  } else if (num == 2 && start > end) {
    tkset(combo.start, end)
  }
}

# helper for ploting selected gene list
SelectGeneListPlotHelper <- function(){
  num <- as.integer(tkcurselection(listbox.show.genes))
  sel.list <- NULL
  for(i in num){
    sel.list <- c(sel.list, tclvalue(tkget(listbox.show.genes,(i))))
  }
  MakeDataFrame(sel.list)
}

# read in /remove files functions ----

# reads in file, tests, fills out info 
LoadTableFile <- function() {
  if (control.state[3] == 0) {
    if (is.null(names(list.data$table.file))) {
      file.count <- 0
    } else {
      file.count <- length(list.data$table.file)
    }
    tk2notetab.select(notebook.on.off, "Common\nGenes") # change tab
    control.state[3] <<- 1
    if (file.count > 10) { 
      tkmessageBox(message = "I have too many files, 
                   you need to reset me or remove some files")
      control.state[3] <<- 0
      return ()
    }
    tcl("wm", "attributes", root, topmost = FALSE)
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
      tcl("wm", "attributes", root, topmost = TRUE)
      return ()
    } else if (file.name %in% names(list.data$table.file)) {
      tkmessageBox(message = "This file has already been loaded")
      control.state[3] <<- 0
      close(pb)
      tcl("wm", "attributes", root, topmost = TRUE)
      return ()
    } else {
      legend.nickname <- strsplit(unlist(strsplit(as.character(file.name), 
                                                  '.tab')[[1]][1]), '[!]')[[1]]
      legend.nickname <- legend.nickname[floor(mean(
        seq_along(legend.nickname)))]
      tablefile <- read.table(full.file.name, header = TRUE, 
                              stringsAsFactors = FALSE, comment.char = "")
      names(tablefile)[1] <- paste("gene")
      num.bins <- dim(tablefile)
      if (file.count > 0) {
        if (num.bins[2] != length(list.data$table.file[[1]])) {
          close(pb)
          tkmessageBox(message = "Can't load file, different number of bins")
          control.state[3] <<- 0
          tcl("wm", "attributes", root, topmost = TRUE)
          return ()
        }
        gene.names <- c(list.data$gene.file$common, unique(tablefile[ ,1]))
        gene.names <- gene.names[duplicated(gene.names)]
        if (length(gene.names) > 0) {
          list.data$gene.file$common <<- gene.names
        } else {
          close(pb)
          tkmessageBox(message = "Can't load file, no genes in common or 
                                  remake your table files all the same way.")
          control.state[3] <<- 0
          tcl("wm", "attributes", root, topmost = TRUE)
          return ()
        }
      } else {  # first time loading a file set up
        tkconfigure(entry.nickname, state = "normal")
        tkconfigure(combobox.norm.bin, values = c(0:(num.bins[2] - 1)))
        list.data$gene.file$common <<- unique(tablefile[ ,1])
        tkconfigure(combobox.bin.start, values = c(1:(num.bins[2] - 1)))
        tkconfigure(combobox.bin.end, values = c(1:(num.bins[2] - 1)))
        tkset(combobox.compare1, "Need_2_gene_sets")
        tkset(combobox.compare2, "Need_2_gene_sets")
        tkset(combobox.regions.file1, "Need_2_files")
        tkset(combobox.regions.file2, "Need_2_files")
        tkset(combobox.bin.end, num.bins[2] - 1)
      }
      file.count <- file.count + 1
      color.safe <- file.count %% 
        length(kListColorSet[[tclvalue(tkget(combobox.color.sets))]])
      if (color.safe == 0) {
        color.safe <- 1
      }
      
      if (legend.nickname %in% sapply(names(list.data$gene.info), function(g)
        sapply(list.data$gene.info[[g]], "[[", 2))){
        legend.nickname <- paste(legend.nickname, "rep?")
      }
       
      lapply(names(list.data$gene.file), function(k) {
        if (k != "common"){
          legend.nickname2 <- strsplit(k, '[!]')[[1]]
          legend.nickname <- paste(legend.nickname, 
                                   legend.nickname2[floor(mean(
                                     seq_along(legend.nickname2)))], sep = "-")
        }
        enesg <- data.frame(gene = unique(list.data$gene.file$common), 
                            stringsAsFactors = FALSE)
        enesg2 <- data.frame(gene = list.data$gene.file[[k]], 
                             stringsAsFactors = FALSE)
        genelist.count2 <- length(unlist(inner_join(enesg, enesg2, by = "gene")))
        list.data$gene.info[[k]][file.name] <<- list(c(file.name, 
              legend.nickname, kDotOptions[1], kLineOptions[1],
              kListColorSet[[tclvalue(tkget(combobox.color.sets))]][color.safe], 
              1, 
              paste(" % NA's = ", round((sum(is.na(tablefile)) / 
                            (num.bins[1] * num.bins[2])) * 100, digits = 2)), 
              paste(" % Zeors = ", round((sum(tablefile == 0) / 
                            (num.bins[1] * num.bins[2])) * 100, digits = 2)),
              tclvalue(tkget(combobox.color.sets)), genelist.count2))
      })
      
      list.data$table.file[file.name] <<- list(tablefile)
    }
    tcl("wm", "attributes", root, topmost = TRUE)
    control.state[3] <<- 0  
    tkconfigure(combobox.file, 
                values = sapply(list.data$gene.info$common, "[[", 1))
    tkconfigure(combobox.numerator, 
                values = sapply(list.data$gene.info$common, "[[", 1))
    tkconfigure(combobox.denominator, 
                values = sapply(list.data$gene.info$common, "[[", 1))
    tkconfigure(combobox.denominator, 
                values = sapply(list.data$gene.info$common, "[[", 1))
    tkconfigure(combobox.regions.file1, 
                values = sapply(list.data$gene.info$common, "[[", 1))
    tkconfigure(combobox.regions.file2, 
                values = sapply(list.data$gene.info$common, "[[", 1))
    tkset(combobox.file, file.name)
    tkset(combobox.regions.file1, "file_1")
    tkset(combobox.regions.file2, "file_2")
    tkset(combobox.color, 
          kListColorSet[[tclvalue(tkget(combobox.color.sets))]][color.safe])
    tkdelete(entry.nickname, 0, 'end')
    tkinsert(entry.nickname, 0, legend.nickname)
    UpdateEntrys("common")
    ComboboxsUpdate()
    
    close(pb)
  }
}

# reads in gene list files
LoadGeneFile <- function(on.listbox, off.listbox, label.file, label.count) { 
  if (control.state[3] == 0) {
    file.count <- length(list.data$table.file)
    if (file.count < 1) {
     return ()
    }
    control.state[3] <<- 1
    tcl("wm", "attributes", root, topmost = FALSE)
    full.file.name <- tclvalue(tkgetOpenFile(filetypes = 
                                               "{{Gene List} {.txt}}"))
    if (!nchar(full.file.name)) {
      control.state[3] <<- 0
      tcl("wm", "attributes", root, topmost = TRUE)
      return ()
      # _TODO
      # remove items from lists and globals?
    } else {
      file.name <- paste(strsplit(as.character(full.file.name), 
                       '/')[[1]][(length(strsplit(as.character(full.file.name), 
                       '/')[[1]]))])
      if (file.name %in% names(list.data$gene.file)) {
        tkmessageBox(message = "This file has already been loaded")
        control.state[3] <<- 0
        tcl("wm", "attributes", root, topmost = TRUE)
        return ()
      }
      old.file.name <- ComboboxSelectionHelper()
      if (old.file.name != "list of table files") {
        list.data$gene.file[old.file.name] <<- NULL
        list.data$gene.info[old.file.name] <<- NULL
      }
      genefile <- read.table(full.file.name, stringsAsFactors = FALSE,
                               header = FALSE)
      legend.name <- strsplit(as.character(file.name), '.txt')[[1]][1]
      legend.nickname <- strsplit(unlist(legend.name), '[!]')[[1]]
      legend.nickname <- legend.nickname[floor(mean(
        seq_along(legend.nickname)))]
      enesg <- data.frame(gene = unique(genefile[ , 1]), 
                          stringsAsFactors = FALSE)
      enesg2 <- data.frame(gene = list.data$gene.file$common, 
                           stringsAsFactors = FALSE)
      genelist.count2 <- length(unlist(inner_join(enesg, enesg2, by = "gene")))
      if (genelist.count2 == 0) {
        tkmessageBox(message = "No genes in common with loaded files")
        control.state[3] <<- 0
        tcl("wm", "attributes", root, topmost = TRUE)
        tkconfigure(label.file, text = "list of table files")
        tkconfigure(label.count, text = "n = 0")
        tkdelete(on.listbox, 0, 'end')
        tkdelete(off.listbox, 0, 'end')
        tkconfigure(combobox.compare1, values = names(list.data$gene.file))
        tkconfigure(combobox.compare2, values = names(list.data$gene.file))
        tkset(combobox.compare1, "Pick_file_1")
        tkset(combobox.compare2, "Pick_file_2")
        return (ComboboxsUpdate())
      }
      list.data$gene.file[legend.name] <<- list(unique(genefile[ , 1]))
      genelist.count <- length(list.data$gene.file[[legend.name]])
      list.data$gene.info[legend.name] <<- list(lapply(
        list.data$gene.info$common, function(i) c(i[[1]], paste(i[[2]], 
                            legend.nickname , sep = "-"), 
                            i[[3]], i[[4]], i[[5]], i[[6]], 
                            paste("genes in file = " , genelist.count), 
                            paste("genes in common n = ", genelist.count2),
                            i[9], genelist.count2)))
      tkconfigure(label.file, text = legend.name)
      tkconfigure(combobox.compare1, values = names(list.data$gene.file))
      tkconfigure(combobox.compare2, values = names(list.data$gene.file))
      tkset(combobox.compare1, "Pick_file_1")
      tkset(combobox.compare2, "Pick_file_2")
      tab <- paste(strsplit(tk2notetab.text(notebook.on.off), 
                            "\n")[[1]], collapse = " ")
      tmp <- strsplit(tab, " " )[[1]][3]
      UpdateEntrys(paste("gene", tmp, sep = ""))
      }
    tcl("wm", "attributes", root, topmost = TRUE)
    control.state[3] <<- 0
    return (ComboboxsUpdate())
  }
}

#removes file
RemoveFile <- function() {
  if (control.state[3] == 0 && length(names(list.data$table.file)) > 0) {
    for (i in as.integer(tkcurselection(listbox.common.on))) {
      filename <- tclvalue(tkget(listbox.common.on, i))
      list.data$table.file[[filename]] <<- NULL
      sapply(names(list.data$gene.info),
             function(k) list.data$gene.info[[k]][[filename]] <<-NULL)
    }
    for (i in as.integer(tkcurselection(listbox.common.off))) {
      filename <- tclvalue(tkget(listbox.common.off, i))
      list.data$table.file[[filename]] <<- NULL
      sapply(names(list.data$gene.info),
             function(k) list.data$gene.info[[k]][[filename]] <<-NULL)
    }
    if (length(names(list.data$table.file)) > 1) {
    gene.names <- c(sapply(names(list.data$table.file), function(i)  
                                             unique(list.data$table.file[[i]][ ,1])))
    
      gene.names <- gene.names[duplicated(gene.names)]
    } else if (legth(names(list.data$table.file)) == 1) {
      gene.names <- unique(list.data$table.file[[1]][ ,1])
    } else {
      # TODO add reset function
      tkmessageBox(message = " have not made a restart funciton yet!!! ")
      tkdestroy(root)
    }
    list.data$gene.file$common <<- gene.names
    sapply(names(list.data$gene.info), function(i) {
      if (i != "common"){
        gene.names2 <- c(gene.names, list.data$gene.file[[i]])
        gene.num <- length(gene.names2[duplicated(gene.names2)])
        sapply(names(list.data$gene.info[[i]]), function(k){
          list.data$gene.info[[i]][[k]][10] <<- gene.num
        })
      }
    })
    UpdateEntrys("common")
    tkconfigure(combobox.file, values = names(list.data$table.file))
    tkset(combobox.file, last(names(list.data$table.file)))
    return (ComboboxsUpdate())
  }
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
      tcl("wm", "attributes", root, topmost = TRUE)
      return ()
    } 
    
    split.file.name <- strsplit(as.character(full.file.name), '/') 
    file.name <- paste(split.file.name[[1]][(length(split.file.name[[1]]))])
    file.nickname <- paste(strsplit(as.character(file.name), '.txt')[[1]][1])
    # fix
    if (full.file.name %in% names(kListColorSet)) {
      tkmessageBox(message = "This file has already been loaded")
      control.state[3] <<- 0
      tcl("wm", "attributes", root, topmost = TRUE)
      return ()
    } else {
      
      color.file <- read.table(full.file.name, header = FALSE, 
                               stringsAsFactors = FALSE)
      # checks if in rgb format and converts to hex and if is a color
      sapply(seq_along(color.file[[1]]), function(i) {
        if (suppressWarnings(!is.na(as.numeric(
          substr(color.file[[1]][i], 1, 1)))) == TRUE) { 
          red.green.blue <- strsplit(color.file[[1]][i], ",")
          if (length(red.green.blue[[1]]) == 3) {
            color.file[[1]][i] <<- rgb(as.numeric(red.green.blue[[1]])[1], 
                        as.numeric(red.green.blue[[1]])[2], 
                        as.numeric(red.green.blue[[1]])[3], 
                        maxColorValue = 255)
          } else {
            color.file[[1]][i] <<- "black"
            }
        }
        if (!isColor(color.file[[1]][i])){
          color.file[[1]][i] <<- "black"
        }
      })
       
      
      kListColorSet[file.nickname] <<-  color.file
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
  nom <- tclvalue(tkget(combobox.numerator))
  dnom <- tclvalue(tkget(combobox.denominator))
  if (nom != "numerator" && dnom != "denominator") {  
    new.gene.list <- data.frame(inner_join(list.data$table.file[[nom]], 
                                     list.data$table.file[[dnom]], by = "gene"), 
                          stringsAsFactors = FALSE)
    new.gene.list[is.na(new.gene.list)] <- 0  # replace NAs with 0
    len <- (dim(new.gene.list)[2] - 1) / 2  # find number of bins
    # find min value /2 to replace 0s 
    new.min.for.na <- min(new.gene.list[, -1][new.gene.list[ ,-1] > 0])/2
    # replace 0's with min/2
    new.gene.list[ , -1] <- 
      as.data.frame(lapply(new.gene.list[ , -1], 
             function(x) {replace(x, x == 0, new.min.for.na)}), 
             stringsAsFactors = FALSE)
    new.tablefile <- data.frame(gene = new.gene.list[ ,1], 
                       new.gene.list[ ,2:(len + 1)] / 
                       new.gene.list[ ,(len + 2):((len * 2) + 1)], 
                       stringsAsFactors = FALSE)
    file.name <- paste(nom, dnom, sep = "/")
    file.nickname <- paste(list.data$gene.info$common[[nom]][2], 
                           list.data$gene.info$common[[dnom]][2], sep = "/")
    list.data$table.file[[file.name]] <<- new.tablefile
    file.count <- length(list.data$table.file)
   
    color.safe <- file.count %% 
      length(kListColorSet[[tclvalue(tkget(combobox.color.sets))]])
    if (color.safe == 0 ) {
      color.safe <- 1
    }
    list.data$gene.info$common[[file.name]] <<- c(file.name, file.nickname, 
            kDotOptions[1], kLineOptions[1], 
            kListColorSet[[tclvalue(tkget(combobox.color.sets))]][color.safe],
            1, 
            paste(" 0's, NA's now = min/2"),
            paste(new.min.for.na), 
            tclvalue(tkget(combobox.color.sets)), 
            list.data$gene.info$common[[1]][10])
  
    tkinsert(listbox.common.on, 'end', file.name)
    tkconfigure(combobox.file, 
                values = sapply(list.data$gene.info$common, "[[", 1))
    tkconfigure(combobox.numerator, 
                values = sapply(list.data$gene.info$common, "[[", 1))
    tkconfigure(combobox.denominator, 
                values = sapply(list.data$gene.info$common, "[[", 1))
    tkset(combobox.file, file.name)
    tkset(combobox.numerator, "numerator")
    tkset(combobox.denominator, "denominator")
    ComboboxsUpdate()
    UpdateEntrys("common")
  }  
}

# gene list functions ----

# displays the selected gene lists genes
ShowGenes <- function() {
  if(control.state[3] == 1 || is.null(names(list.data$table.file))) {
    return()
  } else {    
    file.name <- ComboboxSelectionHelper()
    if (file.name == "list of table files"|| 
        !file.name %in% names(list.data$gene.info)) {
      tkmessageBox(message = "No genes in this list")
      return()
    } 
    tkdelete(listbox.title.show.genes, 0, 'end')
    tkdelete(listbox.show.genes, 0 ,'end')
    tkconfigure(listbox.show.genes, 
              listvariable = tclVar(list.data$gene.file[[file.name]]))
      tkinsert(listbox.title.show.genes, 0, 
              paste(file.name, ' genes,  n = ', 
                    length(list.data$gene.file[[file.name]])))
  }
}

# sorts active gene list contain top % signal based on selected bins and file
SortTop <- function(on.listbox, off.listbox) { 
  if (control.state[3] == 0) {
    if (length(list.data$table.file) < 1) {
      return ()
    }
    control.state[3] <<- 1
    tcl("wm", "attributes", root, topmost = FALSE)
    pb <<- tkProgressBar(title = "be patient!!",  
                         min = 0, max = 100, width = 300)
    setTkProgressBar(pb, 100, label =  "sorting genes")
    R.start.bin <- as.integer(tclvalue(tcl.bin.start))
    R.end.bin <- as.integer(tclvalue(tcl.bin.end))
    R.num <- as.integer(tclvalue(tcl.top.bottom.num))
    R.file.control <- ComboboxSelectionHelper()
    R.tablefile <- tclvalue(tcl.tablefile)
    R.option <- tclvalue(tcl.top.bottom.option)
    enesg <- data.frame(gene = 
            list.data$gene.file[[R.file.control]],
            stringsAsFactors = FALSE)
    if (nrow(enesg) == 0) {
      tkmessageBox(message = "no items in that gene list set")
      close(pb)
      control.state[3] <<- 0
      tcl("wm", "attributes", root, topmost = TRUE)
      return()
    }
    my.data.frame <- data.frame(inner_join(enesg, 
            list.data$table.file[[R.tablefile]], by = "gene"), 
            stringsAsFactors = FALSE)
    gene.count <- nrow(my.data.frame)
    if(R.option == "Top%") {
      num <- c(1, ceiling(gene.count * (R.num/100)))
    } else {
      num <- c(ceiling(gene.count * (R.num/100)), gene.count)
    }
    apply.bins <- rowSums(my.data.frame[,-1][R.start.bin:R.end.bin],	na.rm = T)
    ix <- sort(apply.bins, decreasing=T, index=T)$ix
    # TODO check if I need to NULL $sort if already exists
    list.data$gene.file$sort <<- as.character(my.data.frame[ix,1][num[1]:num[2]])
    gene.count <- length(list.data$gene.file$sort)
    lapply(list.data$gene.info[[R.file.control]], function(i) 
      list.data$gene.info$sort[[i[[1]]]] <<- c(i[[1]], 
                                              paste(i[[2]], "sort", sep = "-"), 
                                                i[[3]], i[[4]], i[[5]], i[[6]], 
                                                paste(R.option, R.num , "from",
                                                      R.tablefile), 
                                                paste("genes in sort n = ", 
                                                      gene.count), i[9]))
    legend.nickname <- paste(tclvalue(tkget(entry.nickname)), "sort", sep = "-")
    tkdelete(entry.nickname, 0, 'end')
    tkinsert(entry.nickname, 'end', legend.nickname)
    tkset(combobox.gene.set, kNbFileControlTabNames[6])
    UpdateEntrys("sort")
    ComboboxsUpdate()
    tkconfigure(combobox.compare1, values = names(list.data$gene.file))
    tkconfigure(combobox.compare2, values = names(list.data$gene.file))
    tkset(combobox.compare1, "Pick_file_1")
    tkset(combobox.compare2, "Pick_file_2")
    close(pb)
    control.state[3] <<- 0
    tcl("wm", "attributes", root, topmost = TRUE)
    return(ComboboxsUpdate())
  }
  return()
}	

# a[1]/a[2] make gene list
CompareRegions <- function(on.listbox, off.listbox) { 
  R.tablefile1 <- tclvalue(tcl.file1.regions)
  R.tablefile2 <- tclvalue(tcl.file2.regions)
  if (control.state[3] == 0 && length(list.data$table.file) > 0 && 
      R.tablefile1 != R.tablefile2) {
    
    control.state[3] <<- 1
    tcl("wm", "attributes", root, topmost = FALSE)
    pb <<- tkProgressBar(title = "be patient!!",  
                         min = 0, max = 100, width = 300)
    setTkProgressBar(pb, 100, label =  "Comparing genes")
    R.start.bin <- as.integer(tclvalue(tcl.bin.start.regions))
    R.end.bin <- as.integer(tclvalue(tcl.bin.end.regions))
    R.num <- as.integer(tclvalue(tcl.bin.fold.regions))
    R.option <- tclvalue(tcl.in.out.option.regions)
    new.gene.list <- data.frame(inner_join(list.data$table.file[[R.tablefile1]], 
                                           list.data$table.file[[R.tablefile2]], 
                                           by = "gene"), 
                                stringsAsFactors = FALSE)
    new.gene.list[is.na(new.gene.list)] <- 0  # replace NAs with 0
    len <- (dim(new.gene.list)[2] - 1) / 2  # find number of bins
    # find min value /2 to replace 0s 
    new.min.for.na <- min(new.gene.list[, -1][new.gene.list[ , -1] > 0]) / 2
    
    new.tablefile <- data.frame(gene = new.gene.list[ ,1], 
                   rowSums(new.gene.list[ ,2:(len + 1)][R.start.bin:R.end.bin], 
                           na.rm = T), 
                   rowSums(new.gene.list[ ,(len + 2):((len * 2) + 
                                                    1)][R.start.bin:R.end.bin], 
                           na.rm = T), 
                                stringsAsFactors = FALSE)
    # replace 0's with min/2
    new.tablefile[ , -1] <- 
      as.data.frame(lapply(new.tablefile[ , -1], 
                           function(x) {replace(x, x == 0, new.min.for.na)}), 
                    stringsAsFactors = FALSE)
    new.tablefile[ , 4] <- new.tablefile[ , 2] / new.tablefile[ , 3]
    
    ix <- sort(new.tablefile[ , 4], decreasing=T, index=T)$ix
    
    gene.count <- nrow(new.tablefile)
    if(R.option == kInOutOptions[1]) {
      test1 <- new.tablefile[ , 4][ix] > R.num
    } else {
      test1 <- new.tablefile[ , 4][ix] < R.num
    }
    list.data$gene.file$regions <<- new.tablefile$gene[ix][test1]
    gene.count <- length(list.data$gene.file$regions)
    lapply(list.data$gene.info$common, function(i) 
      list.data$gene.info$regions[[i[[1]]]] <<- c(i[[1]], 
                                               paste(i[[2]], "regions", sep = "-"), 
                                               i[[3]], i[[4]], i[[5]], i[[6]], 
                                               paste(R.option, R.num , "fold",
                                                     R.tablefile, R.start.bin, 
                                                     "to", R.end.bin), 
                                               paste(R.option, R.num , "fold, 
                                                     n = ", gene.count), i[9],
                                               gene.count))
    tkset(combobox.gene.set, kNbFileControlTabNames[8])
    UpdateEntrys("regions")
    tkset(combobox.regions.file1, "Pick_file_1")
    tkset(combobox.regions.file2, "Pick_file_2")
    close(pb)
    control.state[3] <<- 0
    tcl("wm", "attributes", root, topmost = TRUE)
    return(ComboboxsUpdate())
  }
  return()
}	

# intersect gene lists creating a new one
IntersectGeneLists <- function() { 
  compare1 <- tclvalue(tcl.file.compare.names1)
  compare2 <- tclvalue(tcl.file.compare.names2)
  if (compare1 != compare2 && control.state[3] != 1) {
    gene.names <- c(unique(list.data$gene.file[[compare1]]),
                       unique(list.data$gene.file[[compare2]]),
                    list.data$gene.file$common)
    gene.names <- gene.names[duplicated(gene.names)]
    if (length(gene.names) > 0) {
      file.nickname <- paste(compare1, "intersect with", compare2, sep = " ")
      list.data$gene.file$intersect <<- gene.names
      gene.count <- length(gene.names)
      lapply(list.data$gene.info[[compare1]], function(i) 
        list.data$gene.info$intersect[[i[[1]]]] <<- c(i[[1]],
                                    paste(i[[2]], "intersect", sep = "-"), 
                                    i[[3]], i[[4]], i[[5]], i[[6]], 
                                    file.nickname, 
                                    paste("n = ", gene.count),
                                    i[9], gene.count))
      tkset(combobox.gene.set, kNbFileControlTabNames[7])
      tkconfigure(combobox.compare1, values = names(list.data$gene.file))
      tkconfigure(combobox.compare2, values = names(list.data$gene.file))
      tkset(combobox.compare1, "Pick_file_1")
      tkset(combobox.compare2, "Pick_file_2")
      UpdateEntrys("intersect")
      return(ComboboxsUpdate())
    } else {
      tkmessageBox(message = "no genes in common")
      return(ComboboxsUpdate())
    }
  }
  tkmessageBox(message = "Can't compare to self")
  return()
}


# plotting functions ----

# Makes data frame and gathers plot settings for plotting active samples
MakeDataFrame <- function(sel.list = NULL) {
  if (control.state[3] == 1 || is.null(names(list.data$table.file))) {
    return ()
  } else {
    file.name <- ComboboxSelectionHelper()
    if (file.name == "list of table files") {
      file.name <- "common"
    }
    ComboboxsUpdateVaribles(file.name) # TODO is there any use for the retuned name?
    use.col <- NULL
    use.dot <- NULL
    use.line <- NULL
    use.nickname <- NULL
    use.x.label <- NULL
    list.wide.data.frame <- list()
    for (i in names(list.data$gene.file)) {
      # checks to see if at least one file in list is acitve
      if (sum(as.numeric(sapply(list.data$gene.info[[i]], "[[", 6))) == 0) { 
        next
      } else {
        enesg <- data.frame(gene = list.data$gene.file[[i]], 
                            stringsAsFactors = FALSE)
        if (!is.null(sel.list)) {
          enesg2 <- c(enesg$gene, sel.list)
          enesg <- data.frame(gene = enesg2[duplicated(enesg2)], 
                              stringsAsFactors = FALSE)
          use.x.label <- paste(use.x.label, paste("select ", i, "n = ", 
                                                length(enesg[[1]])), sep = '\n') 
          
        } else {
          # TODO change to looking at info[10] to speed things up 
          enesg2 <- c(enesg$gene, list.data$gene.file$common)
          
          use.x.label <- paste(use.x.label, paste(i, "n = ", 
                                length(enesg2[duplicated(enesg2)])), sep = '\n') 
        }
        lapply(names(list.data$table.file), function(k) 
          # uses only acive lists  
          if (as.numeric(list.data$gene.info[[i]][[k]][6]) == 1) {
            
            list.wide.data.frame[[list.data$gene.info[[i]][[k]][2]]] <<- 
              data.frame(inner_join(enesg, list.data$table.file[[k]], 
                                    by = "gene"), stringsAsFactors = FALSE)
            dot <- which(kDotOptions == list.data$gene.info[[i]][[k]][3])
            if (dot > 20) {
              dot <- 0
            }
            line <- which(kLineOptions == list.data$gene.info[[i]][[k]][4])
            if (line > 6) {
              line <- 0
            }
    
            use.col <<- c(use.col, list.data$gene.info[[i]][[k]][5])
            use.dot <<- c(use.dot, dot)
            use.line <<- c(use.line, line)
            use.nickname <<- c(use.nickname, 
                          gsub("/", " / \n", list.data$gene.info[[i]][[k]][2]))
          }
        )
      } 
    }
  }
  if (!is.null(names(list.wide.data.frame))) {
    ApplyMath(list.wide.data.frame, use.col, use.dot, use.line, use.nickname,
              use.x.label)
  }
}

# Applys math to long list
ApplyMath <- function(list.wide.data.frame, use.col, use.dot, use.line,
                      use.nickname, use.x.label) {
  
  # math set and y label
  use.math <- as.character(tclvalue(tcl.math.option))
  if (use.math == "mean") {
    use.apply <- function(x) colMeans(x, na.rm = TRUE)
    use.y.label <- "Mean of bin counts"
  } else if (use.math == "sum") {
    use.apply <- function(x) colSums(x, na.rm = TRUE)
    use.y.label <- "Sum of bin counts"
  } else if (use.math == "median") {
    use.apply <- function(x) apply(x, 2, median, na.rm = TRUE)
    use.y.label <- "Median of bin counts"
  } 
  
  # set normilization to relative frequency or bin number or 1 for none, 
  # and update y label
  r.checkbox.relative.frequency <- as.character(tclvalue(
    tcl.checkbox.relative.frequency))
  norm.bin <- as.numeric(tclvalue(tcl.norm.bin))
  
  if (r.checkbox.relative.frequency == 1 && norm.bin == 0) {
    use.y.label <- paste(strsplit(use.y.label, split = " ")[[1]][1], 
                         "bins : Relative Frequency")
    norm <- lapply(seq_along(list.wide.data.frame), 
                    function(i) 
                      sum(use.apply(list.wide.data.frame[[i]][ ,-1])))
  } else if (norm.bin > 0) {
    if (r.checkbox.relative.frequency == 1) {
      tkconfigure(checkbox.relative.frequency, 
                  variable = tclVar(0))
    }
    use.y.label <- paste(strsplit(use.y.label, split = " ")[[1]][1], 
                         "bins : Normalize to bin ", norm.bin)
    norm <- as.numeric(lapply(seq_along(list.wide.data.frame), 
              function(i) 
                use.apply(list.wide.data.frame[[i]][ ,-1])[norm.bin]))
  } else {
    norm <- lapply(seq_along(list.wide.data.frame), function(i) 1)
  }
  
  # update y label if log2
  if (tclvalue(tcl.checkbox.log2) == 1) {
    use.y.label <- paste("log2(", use.y.label, ")", sep = "")
  }
  # TODO add bin control?
  list.applied.math.data.frame <- lapply(seq_along(list.wide.data.frame), 
    function(i) 
      data.frame(bin = (seq_along(list.wide.data.frame[[i]][-1])), 
                 set = (as.character(use.nickname[i])), 
               value = use.apply(list.wide.data.frame[[i]][,-1]) / norm[[i]], 
               stringsAsFactors = FALSE)) 
  list.long.data.frame <- rbind_all(list.applied.math.data.frame)
  
  use.pos.plot.ticks <- Destring(unlist(strsplit(tclvalue(
    tcl.pos.plot.ticks), " ")))
  use.label.plot.ticks <- unlist(strsplit(tclvalue(tcl.label.plot.ticks), 
                                          " "))
  if (length(use.label.plot.ticks) != length(use.pos.plot.ticks)) {
    tkmessageBox(message = "The number of Positions and labels are unequal")
    return ()
  }
  
  use.plot.breaks <- c(Destring(tclvalue(tcl.pos.one.line)), 
                       Destring(tclvalue(tcl.pos.two.line)), 
                       Destring(tclvalue(tcl.pos.three.line)), 
                       Destring(tclvalue(tcl.pos.four.line)),
                       use.pos.plot.ticks)
  
  use.plot.breaks.labels <- c(tclvalue(tcl.one.tss.tts.option), 
                              tclvalue(tcl.two.tss.tts.option),
                              tclvalue(tcl.three.tss.tts.option),
                              tclvalue(tcl.four.tss.tts.option),
                              use.label.plot.ticks)
  use.virtical.line.type <- c(3,3,1,1)  # TODO add change virtical line type
  use.virtical.line.color <- c("green", "red", "black", "black")
  use.plot.breaks.labels <- use.plot.breaks.labels[!is.na(use.plot.breaks)]
  use.virtical.line.type <- use.virtical.line.type[!is.na(
                             use.plot.breaks[1:4])]
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
  GGplotF(list.long.data.frame, use.col, use.dot, use.line, use.y.label, 
          use.x.label, use.plot.breaks, virtical.line.data.frame, 
          use.plot.breaks.labels)
}

# ggplot function
GGplotF <- function(list.long.data.frame, use.col, use.dot, use.line,
                    use.y.label, use.x.label, use.plot.breaks, 
                    virtical.line.data.frame, use.plot.breaks.labels) {
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
    xlab(use.x.label) + ylab(use.y.label) +  # Set axis labels
    ggtitle(tclvalue(tcl.header)) +  # Set title
    scale_x_continuous(breaks = use.plot.breaks, 
                       labels = use.plot.breaks.labels) +
  
    geom_vline(data = virtical.line.data.frame, 
           aes(xintercept = use.virtical.line), 
                     size = 2, 
                 linetype = virtical.line.data.frame$use.virtical.line.type, 
                    color = virtical.line.data.frame$use.virtical.line.color) +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    theme(plot.title = element_text(size = 30, vjust = 2)) +
    theme(axis.title.y = element_text(size =  20, vjust = 1.5)) +
    theme(axis.title.x = element_text(size =  (25 / 
                                  length(names(list.data$gene.file))), vjust = 0)) +
    theme(axis.text.x = element_text( size = 10,  angle = -45, hjust = .1, 
                                     vjust = .8,   face = 'bold')) 
    
  
 print(gp)
}

# combobox functions ----

# updates comboboxs and lists 
ComboboxsUpdate <- function() {
  file.name <- ComboboxSelectionHelper()
  if (file.name == "list of table files"|| !file.name %in% 
      names(list.data$gene.info)) {
    file.name <- "common"
  }
  ComboboxsUpdateVaribles(file.name)
  if (!is.null(names(list.data$table.file)) && control.state[3] == 0) {
    cfile <- tclvalue(tkget(combobox.file))
    tkset(combobox.color, list.data$gene.info[[file.name]][[cfile]][5])
    tkset(combobox.lines, list.data$gene.info[[file.name]][[cfile]][4])
    tkset(combobox.dots, list.data$gene.info[[file.name]][[cfile]][3])
    tkdelete(entry.nickname, 0, 'end')
    tkinsert(entry.nickname,  0, list.data$gene.info[[file.name]][[cfile]][2])
    tkconfigure(listbox.stats, listvariable = 
          tclVar(as.character(list.data$gene.info[[file.name]][[cfile]][7:8])))
    tkset(combobox.color.sets, list.data$gene.info[[file.name]][[cfile]][9])
  } 
}

# keeps genelist cbb and tabs in sync
# change in combobox saves vaues and changes tab
ComboboxsGeneSet <- function() {
  file.name <- ComboboxSelectionHelper()
  if (file.name == "list of table files") {
    file.name <- "common"
  }
  if (file.name %in% names(list.data$gene.info)) {
    ComboboxsUpdateVaribles(file.name)
  }
  combo.name <- tclvalue(tkget(combobox.gene.set))
  if (combo.name == kNbFileControlTabNames[6] || 
      combo.name == kNbFileControlTabNames[7]) {
    tk2notetab.select(notebook.on.off.tools1, combo.name)
  } else if (combo.name == kNbFileControlTabNames[8] || 
             combo.name == kNbFileControlTabNames[9]){
    tk2notetab.select(notebook.on.off.tools2, combo.name)
  } else {
    tmp <- strsplit(combo.name, " " )[[1]]
    tmp2 <- unlist(strsplit(paste(tmp[1], "\n", tmp[2], " ", 
                                tmp[3], sep = ""), " NA"))
    tk2notetab.select(notebook.on.off, tmp2)
  }
}

# keeps genelist cbb and tabs in sync
# change tab changes combobox and updates on/off lists
ComboboxsGeneSet2 <- function(notebook) {
  file.name <- ComboboxSelectionHelper()
  if (file.name == "list of table files") {
    file.name <- "common"
  }
  if (file.name %in% names(list.data$gene.info)) {
    ComboboxsUpdateVaribles(file.name)
  }
  tab <- paste(strsplit(tk2notetab.text(notebook), 
                        "\n")[[1]], collapse = " ")
  tmp <- strsplit(tab, " " )[[1]]
  # if sort list don't change combobox.gene.set
  if (file.name == "Sort") {
    tkset(combobox.gene.set, tab)
  } 
  UpdateEntrys(file.name)
  ComboboxsUpdate()
}

# saves current seletion
ComboboxsUpdateVaribles <- function(file.name) {
  if (file.name == "list of table files" || !file.name %in%
      names(list.data$gene.info)) {
    return ()
  }
  
  cfile <- tclvalue(tkget(combobox.file))
  if (!is.null(names(list.data$table.file)) && control.state[3] == 0) {
    list.data$gene.info[[file.name]][[cfile]][5] <<- 
      tclvalue(tkget(combobox.color))
    list.data$gene.info[[file.name]][[cfile]][4] <<- 
      tclvalue(tkget(combobox.lines))
    list.data$gene.info[[file.name]][[cfile]][3] <<- 
      tclvalue(tkget(combobox.dots))
    list.data$gene.info[[file.name]][[cfile]][2] <<- 
      tclvalue(tkget(entry.nickname))
    
    if (sum(duplicated(sapply(names(list.data$gene.info), function(g)
                            sapply(list.data$gene.info[[g]], "[[", 2)))) > 0) {
      list.data$gene.info[[file.name]][[cfile]][2] <<- paste(tclvalue(
        tkget(entry.nickname)), "rep?")
    }
    return ()
  } 
}

# Adds ability to change color sets
ComboboxsColorSets <- function() {
  file.name <- ComboboxSelectionHelper()
  if (file.name == "list of table files") {
    file.name <- "common"
  }
  ComboboxsUpdateVaribles(file.name)
  lapply(seq_along(list.data$table.file), function(i) {
    listname <- names(list.data$table.file)[i]
    list.data$gene.info[[file.name]][[listname]][9] <<- 
      tclvalue(tkget(combobox.color.sets))
        
    color.safe <- i %% 
      length(kListColorSet[[tclvalue(tkget(combobox.color.sets))]])
    if (color.safe == 0) {
      color.safe <- 1
    }
      
    list.data$gene.info[[file.name]][[listname]][5] <<- 
      kListColorSet[[tclvalue(tkget(combobox.color.sets))]][color.safe]
  })
  tkconfigure(combobox.color, 
              values = kListColorSet[[tclvalue(tkget(combobox.color.sets))]])
  ComboboxsUpdate()
}


# Change plot axis labels + lines 
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
  tkdelete(entry.line.tick.label.five, 0, 'end')
  tkinsert(entry.line.tick.label.five, 0, list.plot.ticks[[num]][["name"]])
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

# frame for constent items ----
frame.common.items <- tkframe(root)

# frame for notebook, math, line dot plot options
frame.notebook.math.options <- tkframe(frame.common.items, relief = 'ridge',
                                       borderwidth = 6) 
notebook.math.options <- tk2notebook(frame.notebook.math.options, 
                             tabs = c("Math", 
                                      "Lines & Lables", "Norm Files",
                                      "Extra Options"))
tkgrid(notebook.math.options)

# tab for Math Plot Options ----
notebook.math.tab <- tk2notetab(notebook.math.options, "Math")
frame.plot.math.settings <- tkframe(notebook.math.tab, 
                                    relief = 'ridge', borderwidth = 5)

tkgrid(tklabel(frame.plot.math.settings, text = "Math", width = 35))  

tkgrid(tk2combobox(frame.plot.math.settings,
                   value = kMathOptions,
                   textvariable = tcl.math.option, 
                   state = "readonly"), sticky = "n")

checkbox.relative.frequency <- tkcheckbutton(frame.plot.math.settings, 
                                  variable = tcl.checkbox.relative.frequency, 
                                  text = "Relative Frequency" )
tkgrid(checkbox.relative.frequency)

tkgrid(tkcheckbutton(frame.plot.math.settings, 
                    variable = tcl.checkbox.log2, text = "log2 transformation"))

tkgrid(tklistbox(frame.plot.math.settings, 
                 listvariable = tclVar("Norm_to_bin"),
                 height = 1, width = 12, 
                 relief = 'flat'), padx = c(50, 0), sticky = "w")

tkgrid(combobox.norm.bin <- tk2combobox(frame.plot.math.settings,
                textvariable = tcl.norm.bin, state="readonly", width = 3),
       sticky = "e", column = 0, row = 4, padx = c(0, 50))

tkgrid(frame.plot.math.settings)

# tab for line and tick plot options ---- 
notebook.lines.lables.tab <- tk2notetab(notebook.math.options, 
                                        "Lines & Lables")

tab.plot.options.line.tick.frame <- tkframe(notebook.lines.lables.tab, 
                                            relief = 'ridge', borderwidth = 5) 
tkgrid(tab.plot.options.line.tick.frame)

tkgrid(tklabel(tab.plot.options.line.tick.frame, text = ' Plot Options '),
       columnspan = 6)

tkgrid(tklabel(tab.plot.options.line.tick.frame, 
               text = "Plot lines and labels"), 
       pady = c(5, 5), row = 7, column = 0, columnspan = 6)

combobox.plot.lines <- tk2combobox(tab.plot.options.line.tick.frame, 
                                   value = names(list.plot.lines), 
                                   textvariable = tcl.plot.line.name, 
                                   state = "readonly", width = 20)
tkgrid(combobox.plot.lines, column = 0, columnspan = 6, row = 7) 
tkbind(combobox.plot.lines, '<<ComboboxSelected>>', PlotLines)

tkgrid(tk2entry(tab.plot.options.line.tick.frame, width = 5, 
                textvariable = tcl.one.tss.tts.option),  
       padx = c(10, 0), pady = c(5, 0), column = 0, row = 8)
tkgrid(tklabel(tab.plot.options.line.tick.frame, text = 'Pos'), padx = c(5, 3), 
       pady = c(5, 0), column = 1, row = 8, sticky = "w")
entry.line.tick.pos.one <- tk2entry(tab.plot.options.line.tick.frame, 
                                    width = 4, 
                                    textvariable = tcl.pos.one.line)
tkgrid(entry.line.tick.pos.one, column = 2, row = 8, sticky = "w", 
       padx = c(0, 0), pady = c(5, 0))

tkgrid(tk2entry(tab.plot.options.line.tick.frame, width = 5, 
                textvariable = tcl.two.tss.tts.option),  
       padx = c(10, 0), pady = c(3, 0), column = 0, row = 9)
tkgrid(tklabel(tab.plot.options.line.tick.frame, text = 'Pos'), padx = c(5, 3), 
       pady = c(3, 0), column = 1, row = 9, sticky = "w")
entry.line.tick.pos.two <- tk2entry(tab.plot.options.line.tick.frame, 
                                    width = 4, 
                                    textvariable = tcl.pos.two.line)
tkgrid(entry.line.tick.pos.two, column = 2 , row = 9, sticky = "w", 
       padx = c(0, 0), pady = c(3, 0))

tkgrid(tk2entry(tab.plot.options.line.tick.frame, width = 5, 
                textvariable = tcl.three.tss.tts.option),  
       padx = c(10, 0), pady = c(5, 0), column = 3, row = 8)
tkgrid(tklabel(tab.plot.options.line.tick.frame, text = 'Pos'), 
       padx = c(5, 3), pady = c(5, 0), column = 4, row = 8, sticky = "w")
entry.line.tick.pos.three <- tk2entry(tab.plot.options.line.tick.frame, 
                                      width = 4, 
                                      textvariable = tcl.pos.three.line)
tkgrid(entry.line.tick.pos.three, column = 5, row = 8, sticky = "w", 
       padx = c(0, 0), pady = c(3, 0))

tkgrid(tk2entry(tab.plot.options.line.tick.frame, width = 5, 
                textvariable = tcl.four.tss.tts.option),  
       padx = c(10, 0), pady = c(5, 0), column = 3, row = 9)
tkgrid(tklabel(tab.plot.options.line.tick.frame, text = 'Pos'), column = 4, 
       row = 9, sticky = "w", padx = c(5, 3), pady = c(5, 0))
entry.line.tick.pos.four <- tk2entry(tab.plot.options.line.tick.frame, 
                                     width = 4, 
                                     textvariable = tcl.pos.four.line)
tkgrid(entry.line.tick.pos.four, column = 5, row = 9, sticky = "w", 
       padx = c(0, 0), pady = c(3, 0))

tkgrid(tklabel(tab.plot.options.line.tick.frame, text = "More Bin labels"), 
       row = 11, columnspan = 6)

tkgrid(tklabel(tab.plot.options.line.tick.frame, text = 'Pos'), padx = c(5, 0), 
       column = 0, row = 12, sticky = "w")
entry.line.tick.pos.five <- tk2entry(tab.plot.options.line.tick.frame, 
                                     width = 35,
                                     textvariable = tcl.pos.plot.ticks)
tkgrid(entry.line.tick.pos.five, column = 1, row = 12, padx = c(0, 0), 
       columnspan = 5, sticky = "w")
tkgrid(tklabel(tab.plot.options.line.tick.frame, text = 'label'), 
       padx = c(5, 0), column = 0, row = 13, sticky = "w")
entry.line.tick.label.five <- tk2entry(tab.plot.options.line.tick.frame, 
                                    width = 35, 
                                    textvariable = tcl.label.plot.ticks)
tkgrid(entry.line.tick.label.five, column = 1, row = 13, padx = c(0, 0), 
       columnspan = 5)


tkgrid(frame.notebook.math.options, column = 0, row = 0, padx = c(5,5), 
       pady = c(0,5),sticky = "n")

# tab for making norm file ----
notebook.norm.file.tab <- tk2notetab(notebook.math.options, 
                                             "Norm Files")
frame.norm.files.tab <- tkframe(notebook.norm.file.tab, relief = 'ridge',
                           borderwidth = 5)

tkgrid(tklabel(frame.norm.files.tab, text = "select samples for normalization"), 
       columnspan = 2)

listbox.title.div <- tklistbox(frame.norm.files.tab, height = 1, width = 7, 
                               relief = 'flat')
tkgrid(listbox.title.div, padx = c(20, 0), pady = c(5, 0), column = 0, row = 1)  
tkinsert(listbox.title.div, 0, "divide:")

combobox.numerator <- tk2combobox(frame.norm.files.tab, state = "readonly")
tkset(combobox.numerator, "numerator")
tkgrid(combobox.numerator, sticky = "w", column = 1, row = 1, padx = c(0, 16), 
       pady = c(5, 0)) 

listbox.title.by <- tklistbox(frame.norm.files.tab, height = 1, width = 6, 
                              relief = 'flat')
tkgrid(listbox.title.by, padx = c(20, 0), pady = c(0, 5), column = 0, row = 2)  
tkinsert(listbox.title.by, 0, "by:")

combobox.denominator <- tk2combobox(frame.norm.files.tab, state = "readonly")
tkset(combobox.denominator, "denominator")
tkgrid(combobox.denominator, sticky = "w", column = 1, row = 2, 
       padx = c(0, 16), pady = c(0, 5)) 

tkgrid(tkbutton(frame.norm.files.tab, text = '      Create       ', 
                command = function() MakeNormFile()), columnspan = 2) 
tkgrid(frame.norm.files.tab)

# tab for extra plot options ----
notebook.math.plot.options.tab <- tk2notetab(notebook.math.options, 
                                             "Extra Options")

# frame for loading and applying diffrent color.tet files
tab.plot.options.color.set.frame <- tkframe(notebook.math.plot.options.tab, 
                                            relief = 'ridge', borderwidth = 5)
tkgrid(tab.plot.options.color.set.frame, column = 0, row = 0)

tkgrid(tkbutton(tab.plot.options.color.set.frame, text = ' Load custom color ', 
                command = function() GetColor()), 
       pady = c(15, 0), columnspan = 2) 

tkgrid(tklistbox(tab.plot.options.color.set.frame, 
                 listvariable = tclVar("Line"), height = 1, width = 4, 
                 relief = 'flat'), padx = c(20, 0))

combobox.lines <- tk2combobox(tab.plot.options.color.set.frame, 
                              value = kLineOptions, 
                              textvariable = tcl.line.option, state="readonly",
                              width = 10)
tkgrid(combobox.lines, sticky = "w", column = 1, row = 1, padx = c(0, 16)) 
tkbind(combobox.lines, "<<ComboboxSelected>>", ComboboxsUpdate)

tkgrid(tklistbox(tab.plot.options.color.set.frame, 
                 listvariable = tclVar("dot"),
                 height = 1, width = 4, relief = 'flat'), padx = c(20, 0))

combobox.dots <- tk2combobox(tab.plot.options.color.set.frame, 
                             value = kDotOptions, 
                             textvariable = tcl.dot.option, state="readonly", 
                             width = 10) 
tkgrid(combobox.dots, sticky = "w", column = 1, row = 2, padx = c(0, 16)) 
tkbind(combobox.dots, "<<ComboboxSelected>>", ComboboxsUpdate)

tkgrid(tklabel(tab.plot.options.color.set.frame, text = "Header"), 
       padx = c(5, 1), pady = c(0, 2))
tkgrid(tk2entry(tab.plot.options.color.set.frame, width = 20, 
                textvariable = tcl.header), padx = c(5, 0), pady = c(0, 2), 
       column = 1, row = 3)


# frame for gene set and file options and info ----
frame.geneset.file.select <- tkframe(frame.common.items, relief = 'ridge', 
                                     borderwidth = 5)
tkgrid(frame.geneset.file.select, column = 0, row = 1)

tkgrid(tklabel(frame.geneset.file.select, text = "File options settings"), 
       columnspan = 2)

tkgrid(tklistbox(frame.geneset.file.select, listvariable = tclVar("List"), 
                 height = 1, width = 5, relief = 'flat'), padx = c(5, 0))
combobox.gene.set <- tk2combobox(frame.geneset.file.select, 
                                 value = kNbFileControlTabNames,
                            textvariable = tcl.file.control.tab.names, 
                            state = "readonly", width = 25)
tkgrid(combobox.gene.set, column = 1, row = 1)
tkbind(combobox.gene.set, "<<ComboboxSelected>>", ComboboxsGeneSet)

tkgrid(tklistbox(frame.geneset.file.select, listvariable = tclVar("File"), 
                 height = 1, width = 5, relief = 'flat'), padx = c(5, 0))
combobox.file <- tk2combobox(frame.geneset.file.select, 
            textvariable = tcl.tablefile, state = "readonly", width = 25)
tkgrid(combobox.file, column = 1, row = 2)
tkbind(combobox.file, "<<ComboboxSelected>>", ComboboxsUpdate)
#TODO fix
tkbind(combobox.file, "<Double-Button-1>", LoadTableFile)

tkgrid(tklistbox(frame.geneset.file.select, listvariable = tclVar("nickname:"),
                 height = 1, width = 10, relief = 'flat'), padx = c(5, 0))

entry.nickname <- tk2entry(frame.geneset.file.select, width = 30,
                           state = "disabled")
tkgrid(entry.nickname, sticky = "w", column = 1, row = 3, padx = c(0, 4))

tkgrid(tklistbox(frame.geneset.file.select, listvariable = tclVar("color"), 
                 height = 1, width = 5, relief = 'flat'), padx = c(5, 0))

combobox.color.sets <- tk2combobox(frame.geneset.file.select, 
                                   value = names(kListColorSet), 
                                   textvariable = tcl.color.set, 
                                   state = "readonly",
                                   width = 10)
combobox.color <- tk2combobox(frame.geneset.file.select, 
                 value = kListColorSet[[tclvalue(tkget(combobox.color.sets))]], 
                         textvariable= tcl.color, state = "readonly")
tkgrid(combobox.color, sticky = "w", column = 1, row = 4, padx = c(0, 16))
tkbind(combobox.color, "<<ComboboxSelected>>", ComboboxsUpdate)

tkgrid(tklistbox(frame.geneset.file.select, 
                 listvariable = tclVar("colorset"),
                 height = 1, width = 8, relief = 'flat'), padx = c(5, 0))


tkgrid(combobox.color.sets, sticky = 'w', padx = c(0,5), column = 1, row = 5)
tkbind(combobox.color.sets, "<<ComboboxSelected>>", ComboboxsColorSets)

tkgrid(tklistbox(frame.geneset.file.select, listvariable = tclVar("Stats"),
                 height = 1, width = 5, 
                 relief = 'flat'), column = 0, padx = c(5, 0))
listbox.stats <- tklistbox(frame.geneset.file.select, height = 2, width = 30)
tkgrid(listbox.stats, column = 1, row = 6)

# frame for plot button

frame.plot.button <- tkframe(frame.common.items, relief = 'ridge', 
                             borderwidth = 5)
tkgrid(frame.plot.button, column = 0, row = 2)

tkgrid(tkbutton(frame.plot.button, font = c('bold', 23), 
                text = '      Plot       ', 
                command = function() MakeDataFrame())) 
tkgrid(frame.common.items, column = 0, row = 0)

# create main notebook ----
notebook.main <- tk2notebook(root, tabs = c("On/Off", "Tools1", "Tools2" , 
                                            "Show Genes", "Tools"))
tkgrid(notebook.main, column = 1, row = 0)

# on/off main tab ----
notebook.main.plot.tab <- tk2notetab(notebook.main, "On/Off")

frame.on.off <- tkframe(notebook.main.plot.tab, relief = 'ridge', 
                        borderwidth = 5)

notebook.on.off <- tk2notebook(frame.on.off, tabs = c("Common\nGenes", 
                                                      "Gene\nlist 1", 
                                                      "Gene\nlist 2", 
                                                      "Gene\nlist 3",
                                                      "Gene\nlist 4"))
tkgrid(notebook.on.off)

notebook.on.off.common.tab <- tk2notetab(notebook.on.off, "Common\nGenes")
tkbind(notebook.on.off.common.tab, "<Visibility>", function() 
  ComboboxsGeneSet2(notebook.on.off))

frame.common.tab <- tkframe(notebook.on.off.common.tab)

label.common.file <- tklabel(frame.common.tab, text = "list of table files")
tkgrid(label.common.file, columnspan = 3)
label.common.length <- tklabel(frame.common.tab, text = "n = ")
tkgrid(label.common.length, columnspan = 3)

listbox.common.on <- tk2listbox(frame.common.tab, width = 35, height = 5)
tkgrid(listbox.common.on, columnspan = 3)
tkgrid(tklabel(frame.common.tab, text ="Plot list"), columnspan = 3)
tkgrid(tkbutton(frame.common.tab, text = "<<Switch>>", command = function() 
  MoveSelectToOtherEntry(listbox.common.on, listbox.common.off)), 
    tkbutton(frame.common.tab, text = "<<All On>>", command = function() 
     MoveAllToOtherEntry(listbox.common.on, listbox.common.off, "on")), 
    tkbutton(frame.common.tab, text = "<<All Off>>", command = function() 
     MoveAllToOtherEntry(listbox.common.on, listbox.common.off, "off")), 
  sticky = 'we')
tkgrid(tklabel(frame.common.tab, text ="Don't plot list"), columnspan = 3)
listbox.common.off <- tk2listbox(frame.common.tab, width = 35, height = 5)
tkgrid(listbox.common.off, columnspan = 3)

tkgrid(tkbutton(frame.common.tab, text = " Load table file ", 
                font =c('bold', 15),
               command =  function() LoadTableFile()), row = 7, 
       columnspan = 3)
tkgrid(tkbutton(frame.common.tab, text = " Remove selected file(s) ",
                font =c('bold', 10),
               command =  function() RemoveFile()), row = 8, 
               columnspan = 3)

tkgrid(frame.common.tab)

notebook.on.off.gene1.tab <- tk2notetab(notebook.on.off, "Gene\nlist 1")
tkbind(notebook.on.off.gene1.tab, "<Visibility>", function() 
  ComboboxsGeneSet2(notebook.on.off))

frame.gene1.tab <- tkframe(notebook.on.off.gene1.tab)

label.gene1.file <- tklabel(frame.gene1.tab, text = "list of table files")
tkgrid(label.gene1.file, columnspan = 3)
label.gene1.length <- tklabel(frame.gene1.tab, text = "n = ")
tkgrid(label.gene1.length, columnspan = 3)

listbox.gene1.on <- tk2listbox(frame.gene1.tab, width = 35, height = 5)
tkgrid(listbox.gene1.on, columnspan = 3)
tkgrid(tklabel(frame.gene1.tab, text ="Plot list"), columnspan = 3)
tkgrid(tkbutton(frame.gene1.tab, text = "<<Switch>>", command = function() 
  MoveSelectToOtherEntry(listbox.gene1.on, listbox.gene1.off)), 
  tkbutton(frame.gene1.tab, text = "<<All On>>", command = function() 
    MoveAllToOtherEntry(listbox.gene1.on, listbox.gene1.off, "on")), 
  tkbutton(frame.gene1.tab, text = "<<All Off>>", command = function() 
    MoveAllToOtherEntry(listbox.gene1.on, listbox.gene1.off, "off"
                        )), sticky = 'we')
tkgrid(tklabel(frame.gene1.tab, text ="Don't plot list"), columnspan = 3)
listbox.gene1.off <- tk2listbox(frame.gene1.tab, width = 35, height = 5)
tkgrid(listbox.gene1.off, columnspan = 3)
tkgrid(tkbutton(frame.gene1.tab, text = " Load gene list ", 
                command =  function() LoadGeneFile(listbox.gene1.on,
                                                   listbox.gene1.off,
                                                   label.gene1.file,
                                                   label.gene1.length)), 
       row = 7, 
       columnspan = 2, sticky = 'w', padx = c(3, 0))
tkgrid(tkbutton(frame.gene1.tab, text = " place holder ",
                command =  function() OnOk()), row = 7, 
       column = 1, columnspan = 2, sticky = 'e', padx = c(0, 3))
tkgrid(frame.gene1.tab)

notebook.on.off.gene2.tab <- tk2notetab(notebook.on.off, "Gene\nlist 2")
tkbind(notebook.on.off.gene2.tab, "<Visibility>", function() 
  ComboboxsGeneSet2(notebook.on.off))

frame.gene2.tab <- tkframe(notebook.on.off.gene2.tab)

label.gene2.file <- tklabel(frame.gene2.tab, text = "list of table files")
tkgrid(label.gene2.file, columnspan = 3)
label.gene2.length <- tklabel(frame.gene2.tab, text = "n = ")
tkgrid(label.gene2.length, columnspan = 3)

listbox.gene2.on <- tk2listbox(frame.gene2.tab, width = 35, height = 5)
tkgrid(listbox.gene2.on, columnspan = 3)
tkgrid(tklabel(frame.gene2.tab, text ="Plot list"), columnspan = 3)
tkgrid(tkbutton(frame.gene2.tab, text = "<<Switch>>", command = function() 
  MoveSelectToOtherEntry(listbox.gene2.on, listbox.gene2.off)), 
  tkbutton(frame.gene2.tab, text = "<<All On>>", command = function() 
    MoveAllToOtherEntry(listbox.gene2.on, listbox.gene2.off, "on")), 
  tkbutton(frame.gene2.tab, text = "<<All Off>>", command = function() 
    MoveAllToOtherEntry(listbox.gene2.on, listbox.gene2.off, "off")), 
  sticky = 'we')
tkgrid(tklabel(frame.gene2.tab, text ="Don't plot list"), columnspan = 3)
listbox.gene2.off <- tk2listbox(frame.gene2.tab, width = 35, height = 5)
tkgrid(listbox.gene2.off, columnspan = 3)
tkgrid(tkbutton(frame.gene2.tab, text = " Load gene list ", 
                command =  function() LoadGeneFile(listbox.gene2.on,
                                                   listbox.gene2.off,
                                                   label.gene2.file,
                                                   label.gene2.length)), 
       row = 7, 
       columnspan = 2, sticky = 'w', padx = c(3, 0))
tkgrid(tkbutton(frame.gene2.tab, text = " place holder ",
                command =  function() OnOk()), row = 7, 
       column = 1, columnspan = 2, sticky = 'e', padx = c(0, 3))
tkgrid(frame.gene2.tab)

notebook.on.off.gene3.tab <- tk2notetab(notebook.on.off, "Gene\nlist 3")
tkbind(notebook.on.off.gene3.tab, "<Visibility>", function() 
  ComboboxsGeneSet2(notebook.on.off))

frame.gene3.tab <- tkframe(notebook.on.off.gene3.tab)

label.gene3.file <- tklabel(frame.gene3.tab, text = "list of table files")
tkgrid(label.gene3.file, columnspan = 3)
label.gene3.length <- tklabel(frame.gene3.tab, text = "n = ")
tkgrid(label.gene3.length, columnspan = 3)

listbox.gene3.on <- tk2listbox(frame.gene3.tab, width = 35, height = 5)
tkgrid(listbox.gene3.on, columnspan = 3)
tkgrid(tklabel(frame.gene3.tab, text ="Plot list"), columnspan = 3)
tkgrid(tkbutton(frame.gene3.tab, text = "<<Switch>>", command = function() 
  MoveSelectToOtherEntry(listbox.gene3.on, listbox.gene3.off)), 
  tkbutton(frame.gene3.tab, text = "<<All On>>", command = function() 
    MoveAllToOtherEntry(listbox.gene3.on, listbox.gene3.off, "on")), 
  tkbutton(frame.gene3.tab, text = "<<All Off>>", command = function() 
    MoveAllToOtherEntry(listbox.gene3.on, listbox.gene3.off, "off")), 
  sticky = 'we')
tkgrid(tklabel(frame.gene3.tab, text ="Don't plot list"), columnspan = 3)
listbox.gene3.off <- tk2listbox(frame.gene3.tab, width = 35, height = 5)
tkgrid(listbox.gene3.off, columnspan = 3)
tkgrid(tkbutton(frame.gene3.tab, text = " Load gene list ", 
                command =  function() LoadGeneFile(listbox.gene3.on,
                                                   listbox.gene3.off,
                                                   label.gene3.file,
                                                   label.gene3.length)), 
       row = 7, 
       columnspan = 2, sticky = 'w', padx = c(3, 0))
tkgrid(tkbutton(frame.gene3.tab, text = " place holder ",
                command =  function() OnOk()), row = 7, 
       column = 1, columnspan = 2, sticky = 'e', padx = c(0, 3))
tkgrid(frame.gene3.tab)

notebook.on.off.gene4.tab <- tk2notetab(notebook.on.off, "Gene\nlist 4")
tkbind(notebook.on.off.gene4.tab, "<Visibility>", function() 
  ComboboxsGeneSet2(notebook.on.off))

frame.gene4.tab <- tkframe(notebook.on.off.gene4.tab)

label.gene4.file <- tklabel(frame.gene4.tab, text = "list of table files")
tkgrid(label.gene4.file, columnspan = 3)
label.gene4.length <- tklabel(frame.gene4.tab, text = "n = ")
tkgrid(label.gene4.length, columnspan = 3)

listbox.gene4.on <- tk2listbox(frame.gene4.tab, width = 35, height = 5)
tkgrid(listbox.gene4.on, columnspan = 3)
tkgrid(tklabel(frame.gene4.tab, text ="Plot list"), columnspan = 3)
tkgrid(tkbutton(frame.gene4.tab, text = "<<Switch>>", command = function() 
  MoveSelectToOtherEntry(listbox.gene4.on, listbox.gene4.off)), 
  tkbutton(frame.gene4.tab, text = "<<All On>>", command = function() 
    MoveAllToOtherEntry(listbox.gene4.on, listbox.gene4.off, "on")), 
  tkbutton(frame.gene4.tab, text = "<<All Off>>", command = function() 
    MoveAllToOtherEntry(listbox.gene4.on, listbox.gene4.off, "off")),
  sticky = 'we')
tkgrid(tklabel(frame.gene4.tab, text ="Don't plot list"), columnspan = 3)
listbox.gene4.off <- tk2listbox(frame.gene4.tab, width = 35, height = 5)
tkgrid(listbox.gene4.off, columnspan = 3)
tkgrid(tkbutton(frame.gene4.tab, text = " Load gene list ", 
                command =  function() LoadGeneFile(listbox.gene4.on,
                                                   listbox.gene4.off,
                                                   label.gene4.file,
                                                   label.gene4.length)),
       row = 7, 
       columnspan = 2, sticky = 'w', padx = c(3, 0))
tkgrid(tkbutton(frame.gene4.tab, text = " place holder ",
                command =  function() OnOk()), row = 7, 
       column = 1, columnspan = 2, sticky = 'e', padx = c(0, 3))
tkgrid(frame.gene4.tab)

tkgrid(frame.on.off, column = 0, row = 1, rowspan = 2)

# tools1 main tab ----
notebook.main.tools1.tab <- tk2notetab(notebook.main, "Tools1")

frame.tools1 <- tkframe(notebook.main.tools1.tab, relief = 'ridge', 
                        borderwidth = 5)



notebook.on.off.tools1 <- tk2notebook(frame.tools1, tabs = c("Sort tools list", 
                                                      "Intersect list"))
tkgrid(notebook.on.off.tools1)

notebook.on.off.sort.tab <- tk2notetab(notebook.on.off.tools1, 
                                         "Sort tools list")
tkbind(notebook.on.off.sort.tab, "<Visibility>", function() 
  ComboboxsGeneSet2(notebook.on.off.tools1))

# box for sort tool
frame.sort.tools.tab <- tkframe(notebook.on.off.sort.tab, relief = 'ridge', 
                                borderwidth = 5)

tkgrid(tklabel(frame.sort.tools.tab, text = "Sort tools"), columnspan = 2) 

tkgrid(tk2combobox(frame.sort.tools.tab,
                   values =  kTopBottomOptions, 
                   textvariable = tcl.top.bottom.option,
                   state = "readonly", width = 8), sticky = "w", 
       columnspan = 2, column = 0, row = 1, padx = c(5, 0)) 

tkgrid(tk2combobox(frame.sort.tools.tab,
                   values =  kTopBottomNum, 
                   textvariable = tcl.top.bottom.num, 
                   state = "readonly", width = 3), sticky = "we", 
       columnspan = 2, column = 2, row = 1, padx = c(0, 5), pady = c(10, 10))

tkgrid(tklistbox(frame.sort.tools.tab, listvariable = tclVar("bins"), 
                 height = 1, width = 4, relief = 'flat'), padx = c(5, 0), 
       column = 0, row = 3, sticky ="e")
combobox.bin.start <- tk2combobox(frame.sort.tools.tab, 
                                     textvariable = tcl.bin.start,
                                     state = "readonly", width = 3) 
tkgrid(combobox.bin.start, padx = c(0, 0), column = 1, row = 3, sticky ="w")
tkbind(combobox.bin.start, "<<ComboboxSelected>>", function()
       BinStartEndHelper(tcl.bin.start, tcl.bin.end, 
                         combobox.bin.start, combobox.bin.end, 1))
tkgrid(tklistbox(frame.sort.tools.tab, listvariable = tclVar("to"), 
                 height = 1, width = 2, relief = 'flat'), column = 2, row = 3, 
       padx = c(0, 0), sticky ="w")
combobox.bin.end <- tk2combobox(frame.sort.tools.tab, 
                                  textvariable = tcl.bin.end,
                                  state = "readonly", width = 3) 
tkgrid(combobox.bin.end, column = 3, row = 3, padx = c(0, 10), sticky ="w")
tkbind(combobox.bin.end, "<<ComboboxSelected>>", function()
       BinStartEndHelper(tcl.bin.start, tcl.bin.end, 
                         combobox.bin.start, combobox.bin.end, 2))

tkgrid(tkbutton(frame.sort.tools.tab, text = "   Sort   ", 
                command =  function() SortTop(listbox.sort.on, 
                                              listbox.sort.off)), 
       column = 0, row = 4, columnspan = 4, pady = c(10, 10))

tkgrid(frame.sort.tools.tab, column = 0, row = 0, sticky = 'n')

# on off tab for sort
frame.sort.tab <- tkframe(notebook.on.off.sort.tab)

label.sort.file <- tklabel(frame.sort.tab, text = "list of table files")
tkgrid(label.sort.file, columnspan = 3)
label.sort.length <- tklabel(frame.sort.tab, text = "n = ")
tkgrid(label.sort.length, columnspan = 3)

listbox.sort.on <- tk2listbox(frame.sort.tab, width = 35, height = 5)
tkgrid(listbox.sort.on, columnspan = 3)
tkgrid(tklabel(frame.sort.tab, text ="Plot list"), columnspan = 3)
tkgrid(tkbutton(frame.sort.tab, text = "<<Switch>>", command = function() 
  MoveSelectToOtherEntry(listbox.sort.on, listbox.sort.off)), 
  tkbutton(frame.sort.tab, text = "<<All On>>", command = function() 
    MoveAllToOtherEntry(listbox.sort.on, listbox.sort.off, "on")), 
  tkbutton(frame.sort.tab, text = "<<All Off>>", command = function() 
    MoveAllToOtherEntry(listbox.sort.on, listbox.sort.off, "off")), 
  sticky = 'we')
tkgrid(tklabel(frame.sort.tab, text ="Don't plot list"), columnspan = 3)
listbox.sort.off <- tk2listbox(frame.sort.tab, width = 35, height = 5)
tkgrid(listbox.sort.off, columnspan = 3)
# TODO save function
tkgrid(tkbutton(frame.sort.tab, text = " Save gene list ", 
                font =c('bold', 15),
                command =  function() OnOk()), row = 7, 
       columnspan = 3)

tkgrid(frame.sort.tab)

# on off tab for intersect tool
notebook.on.off.intersect.tab <- tk2notetab(notebook.on.off.tools1, "Intersect list")
tkbind(notebook.on.off.intersect.tab, "<Visibility>", function() 
  ComboboxsGeneSet2(notebook.on.off.tools1))

#frame for intersect tools 
frame.intersect.tools.tab <- tkframe(notebook.on.off.intersect.tab, relief = 'ridge', 
                                     borderwidth = 5)

tkgrid(tklistbox(frame.intersect.tools.tab, listvariable = tclVar("list:"), 
                 height = 1, width = 4, relief = 'flat'))
tkgrid(combobox.compare1 <- tk2combobox(frame.intersect.tools.tab,
                   textvariable = tcl.file.compare.names1,
                   state = "readonly") , sticky = "w", column = 1, row = 0)

tkgrid(tklistbox(frame.intersect.tools.tab, 
                 listvariable = tclVar("compare_to:"), 
                 height = 1, width = 12, 
                 relief = 'flat'), column = 0, row = 3)
tkgrid(combobox.compare2 <- tk2combobox(frame.intersect.tools.tab, 
                   textvariable = tcl.file.compare.names2,
                   state="readonly"), sticky = "w", column = 1, row = 3) 

tkgrid(tkbutton(frame.intersect.tools.tab, text = " intersect list ", 
                command =  function() IntersectGeneLists()), 
       column = 0, row = 4, columnspan =2)

tkgrid(frame.intersect.tools.tab)

frame.intersect.tab <- tkframe(notebook.on.off.intersect.tab)

label.intersect.file <- tklabel(frame.intersect.tab, text = "list of table files")
tkgrid(label.intersect.file, columnspan = 3)
label.intersect.length <- tklabel(frame.intersect.tab, text = "n = ")
tkgrid(label.intersect.length, columnspan = 3)

listbox.intersect.on <- tk2listbox(frame.intersect.tab, width = 35, height = 5)
tkgrid(listbox.intersect.on, columnspan = 3)
tkgrid(tklabel(frame.intersect.tab, text ="Plot list"), columnspan = 3)
tkgrid(tkbutton(frame.intersect.tab, text = "<<Switch>>", command = function() 
  MoveSelectToOtherEntry(listbox.intersect.on, listbox.intersect.off)), 
  tkbutton(frame.intersect.tab, text = "<<All On>>", command = function() 
    MoveAllToOtherEntry(listbox.intersect.on, listbox.intersect.off, "on")), 
  tkbutton(frame.intersect.tab, text = "<<All Off>>", command = function() 
    MoveAllToOtherEntry(listbox.intersect.on, listbox.intersect.off, "off"
    )), sticky = 'we')
tkgrid(tklabel(frame.intersect.tab, text ="Don't plot list"), columnspan = 3)
listbox.intersect.off <- tk2listbox(frame.intersect.tab, width = 35, height = 5)
tkgrid(listbox.intersect.off, columnspan = 3)
tkgrid(tkbutton(frame.intersect.tab, text = " Save gene list ", 
                font =c('bold', 15),
                command =  function() OnOk()), 
       row = 7, columnspan = 3)

tkgrid(frame.intersect.tab)

tkgrid(frame.tools1)

# tools2 main tab ----
notebook.main.tools2.tab <- tk2notetab(notebook.main, "Tools2")

frame.tools2 <- tkframe(notebook.main.tools2.tab, relief = 'ridge', 
                        borderwidth = 5)

notebook.on.off.tools2 <- tk2notebook(frame.tools2, tabs = c("Compare regions", 
                                                             "Compare ratios"))
tkgrid(notebook.on.off.tools2)

notebook.on.off.regions.tab <- tk2notetab(notebook.on.off.tools2, 
                                       "Compare regions")
tkbind(notebook.on.off.regions.tab, "<Visibility>", function() 
  ComboboxsGeneSet2(notebook.on.off.tools2))

# box for sort tool
frame.regions.tools.tab <- tkframe(notebook.on.off.regions.tab, relief = 'ridge', 
                                borderwidth = 5)

tkgrid(tklabel(frame.regions.tools.tab, text = "Compare regions"), 
       columnspan = 3) 

tkgrid(combobox.regions.file1 <- tk2combobox(frame.regions.tools.tab,
                                             textvariable = tcl.file1.regions,
                                             state = "readonly"), 
       columnspan = 2, sticky = "w", column = 0, row = 1)
tkgrid(combobox.regions.file2 <- tk2combobox(frame.regions.tools.tab,
                                        textvariable = tcl.file2.regions,
                                        state = "readonly"), 
       columnspan = 2, sticky = "e", column = 1, row = 1)
tkgrid(tklabel(frame.regions.tools.tab, text = "Fold +"), 
       column = 0, row = 2)
tkgrid(combobox.bin.fold.regions <- tk2combobox(frame.regions.tools.tab, 
                                                textvariable = tcl.bin.fold.regions,
                                                value = kFoldList,
                                                state = "readonly", width = 4),
       column = 1, row = 2)
tkgrid(tk2combobox(frame.regions.tools.tab,
                   values =  kInOutOptions, 
                   textvariable = tcl.in.out.option.regions,
                   state = "readonly", width = 8), sticky = "w", 
                   column = 2, row = 2) 

tkgrid(tklabel(frame.regions.tools.tab, text = "bins"), 
       column = 0, row = 3)
combobox.bin.start.regions <- tk2combobox(frame.regions.tools.tab, 
                                  textvariable = tcl.bin.start.regions,
                                  state = "readonly", width = 3) 
tkgrid(combobox.bin.start.regions, column = 1, row = 3)
tkbind(combobox.bin.start.regions, "<<ComboboxSelected>>", function()
  BinStartEndHelper(tcl.bin.start.regions, tcl.bin.end.regions, 
                    combobox.bin.start.regions, combobox.bin.end.regions, 1))
tkgrid(tklabel(frame.regions.tools.tab, text = ":"), 
       column = 2, row = 3, sticky = "w", padx = c(4,0))
combobox.bin.end.regions <- tk2combobox(frame.regions.tools.tab, 
                                textvariable = tcl.bin.end.regions,
                                state = "readonly", width = 3) 
tkgrid(combobox.bin.end.regions, column = 2, row = 3, sticky = "e", padx = c(0,5))
tkbind(combobox.bin.end.regions, "<<ComboboxSelected>>", function()
  BinStartEndHelper(tcl.bin.start.regions, tcl.bin.end.regions, 
                    combobox.bin.start.regions, combobox.bin.end.regions, 2))

tkgrid(tkbutton(frame.regions.tools.tab, text = "Compare regions", 
                command =  function() OnOk()), 
       column = 0, row = 4, columnspan = 4, pady = c(10, 10))

tkgrid(frame.regions.tools.tab, column = 0, row = 0, sticky = 'n')

# on off tab for regions
frame.regions.tab <- tkframe(notebook.on.off.regions.tab)

label.regions.file <- tklabel(frame.regions.tab, text = "list of table files")
tkgrid(label.regions.file, columnspan = 3)
label.regions.length <- tklabel(frame.regions.tab, text = "n = ")
tkgrid(label.regions.length, columnspan = 3)

listbox.regions.on <- tk2listbox(frame.regions.tab, width = 35, height = 5)
tkgrid(listbox.regions.on, columnspan = 3)
tkgrid(tklabel(frame.regions.tab, text ="Plot list"), columnspan = 3)
tkgrid(tkbutton(frame.regions.tab, text = "<<Switch>>", command = function() 
  MoveSelectToOtherEntry(listbox.regions.on, listbox.regions.off)), 
  tkbutton(frame.regions.tab, text = "<<All On>>", command = function() 
    MoveAllToOtherEntry(listbox.regions.on, listbox.regions.off, "on")), 
  tkbutton(frame.regions.tab, text = "<<All Off>>", command = function() 
    MoveAllToOtherEntry(listbox.regions.on, listbox.regions.off, "off")), 
  sticky = 'we')
tkgrid(tklabel(frame.regions.tab, text ="Don't plot list"), columnspan = 3)
listbox.regions.off <- tk2listbox(frame.regions.tab, width = 35, height = 5)
tkgrid(listbox.regions.off, columnspan = 3)
# TODO save function
tkgrid(tkbutton(frame.regions.tab, text = " Save gene list ", 
                font =c('bold', 15),
                command =  function() OnOk()), row = 7, 
       columnspan = 3)

tkgrid(frame.regions.tab)

# on off tab for ratios tool
notebook.on.off.ratios.tab <- tk2notetab(notebook.on.off.tools2, 
                                         "Compare ratios")
tkbind(notebook.on.off.ratios.tab, "<Visibility>", function() 
  ComboboxsGeneSet2(notebook.on.off.tools2))

#frame for ratios tools 
frame.ratios.tools.tab <- tkframe(notebook.on.off.ratios.tab, relief = 'ridge', 
                                     borderwidth = 5)

tkgrid(frame.ratios.tools.tab)

frame.ratios.tab <- tkframe(notebook.on.off.ratios.tab)

label.ratios.file <- tklabel(frame.ratios.tab, text = "list of table files")
tkgrid(label.ratios.file, columnspan = 3)
label.ratios.length <- tklabel(frame.ratios.tab, text = "n = ")
tkgrid(label.ratios.length, columnspan = 3)

listbox.ratios.on <- tk2listbox(frame.ratios.tab, width = 35, height = 5)
tkgrid(listbox.ratios.on, columnspan = 3)
tkgrid(tklabel(frame.ratios.tab, text ="Plot list"), columnspan = 3)
tkgrid(tkbutton(frame.ratios.tab, text = "<<Switch>>", command = function() 
  MoveSelectToOtherEntry(listbox.ratios.on, listbox.ratios.off)), 
  tkbutton(frame.ratios.tab, text = "<<All On>>", command = function() 
    MoveAllToOtherEntry(listbox.ratios.on, listbox.ratios.off, "on")), 
  tkbutton(frame.ratios.tab, text = "<<All Off>>", command = function() 
    MoveAllToOtherEntry(listbox.ratios.on, listbox.ratios.off, "off"
    )), sticky = 'we')
tkgrid(tklabel(frame.ratios.tab, text ="Don't plot list"), columnspan = 3)
listbox.ratios.off <- tk2listbox(frame.ratios.tab, width = 35, height = 5)
tkgrid(listbox.ratios.off, columnspan = 3)
tkgrid(tkbutton(frame.ratios.tab, text = " Save gene list ", 
                font =c('bold', 15),
                command =  function() OnOk()), 
       row = 7, columnspan = 3)

tkgrid(frame.ratios.tab)
tkgrid(frame.tools2)

# tab for display genes ----
notebook.main.genes.tab <- tk2notetab(notebook.main, "Show Genes")

frame.genes.tab <- tkframe(notebook.main.genes.tab, relief = 'ridge', 
                           borderwidth = 5)

tkgrid(tkbutton(frame.genes.tab, text = " Show Genes ", font = c("bold", 15), 
                command =  function() ShowGenes()), 
       column = 0, row = 1, columnspan = 2)


frame.show.genes <- tkframe(notebook.main.genes.tab, relief = 'ridge', 
                            borderwidth = 5)
tkgrid(frame.show.genes, column = 0, row = 1, sticky = "n")

listbox.title.show.genes <- tklistbox(frame.show.genes, 
                                      listvariable = tclVar("load_list"), 
                                      relief = 'flat', height = 1, width = 30)
tkgrid(listbox.title.show.genes, column = 0, row = 0)

listbox.show.genes <- tk2listbox(frame.show.genes, height = 20, width = 30)
tkgrid(listbox.show.genes, column = 0, row = 1, sticky = "n")

tkgrid(tkbutton(frame.show.genes, text = " Plot select ", font = c("bold", 15), 
                command =  function() SelectGeneListPlotHelper()), 
       column = 0, row = 3, columnspan = 2, sticky = "ew")
tkgrid(frame.genes.tab, column = 0, row = 0, sticky = "n")

# end ----

try(tk2theme("keramik"), silent = TRUE)
#}
#expandTk()
