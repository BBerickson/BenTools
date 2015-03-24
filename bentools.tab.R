
require(tcltk)
require(tcltk2)
require(dplyr)

#expandTk <- function() {
  
# starting values  ----
my_version_num <- 'Ben Tools tabs'
FILE_LIST <- list(NULL) # for holding table files in list
FILE_LIST_INFO <- list(NULL)
# c(full_name, NAs, Zeros)
GENE_LISTS <- list(list(NULL,NULL)) # for holding each gene list
#[[1]] Original [[2]] common 
GENE_LIST_INFO <- list(list("NA")) 
# c(name, nickname, dot", line", color, plot?)
# for each file in each common gene list 
STATE <- c(0, 0, 0) # Keep track of the state and flow control 
# [1] active tab number, [2] check if load file window open, 
# [3] when busy stop user input/activity

# value lists ----
my_dotlist <- c("circle", "triangle point up", "plus", "cross", "diamond", 
                "triangle point down", "square cross", "star", "diamond plus", 
                "circle  plus", "triangles up and down","square  plus", "circle cross", 
                "square and triangle down", "filled square", "filled circle",
                "filled triangle point up", "filled diamond", "solid circle", 
                "bullet (smaller circle)", "square")
my_linelist <- c("solid line", "dashed line", "dotted line", "dot dash line", 
                 "long dash line", "two dash line", "split line")
MY_COLORS <- list("colorset1" = c("yellow", "blue", "green", "red", "orange", "purple", rep("black",10)),
                  "colorset2" = c("red", "orange", "purple", "yellow", "blue", "green", rep("black",10)))


# tcl starting values ----

sf <- tclVar("Load File")
cs <- tclVar(names(MY_COLORS)[1])
ll <- tclVar(my_linelist[1])
dl <- tclVar(my_dotlist[1])
cl <- tclVar(MY_COLORS[[1]][1])

# functions ----

# reads in file, tests, fills out info 
GetTableFile <- function(...) {
  if(STATE[3] == 0){
    if(is.null(FILE_LIST[[1]])){
      file_count <- 0
    }else{
      file_count <- length(FILE_LIST)
    }
    tk2notetab.select(nb, "Table files")
    STATE[3] <<- 1
    if(file_count > 10){ 
      tkmessageBox(message = "I have too many files, 
                   you need to reset me or remove some files")
      STATE[3] <<- 0
      return(FALSE)
    }
    tcl("wm", "attributes", root, topmost=F)
    pb <- tkProgressBar(title = "Loading file, please be patient!!", width = 300 )
    file_name <- tclvalue(tkgetOpenFile(filetypes = 
                                          "{{Table Files} {.table .tab .Table}}"))
    if(!nchar(file_name)) { ## file select test
      close(pb)
      STATE[3] <<- 0
      return(FALSE)
    }else if(file_name %in% sapply(FILE_LIST_INFO, "[[", 1)){
      tkmessageBox(message = "This file has already been loaded")
      STATE[3] <<- 0
      close(pb)
      tcl("wm", "attributes", root, topmost=TRUE)
      return(FALSE)
    }else{
      l_name <- strsplit(as.character(file_name), '/') 
      ld_name <- paste(l_name[[1]][(length(l_name[[1]]))])
      legend_name <- paste(strsplit(as.character(ld_name), '.tab')[[1]][1])
      first_file <- read.table(file_name, header = TRUE, stringsAsFactors= FALSE, 
                               comment.char = "")
      names(first_file)[1]<-paste("gene")
      num_bins <- dim(first_file)
      gene_names <- c(GENE_LISTS[[1]][[1]], unique(first_file[,1]))
      if(file_count > 0){
        if(num_bins[2] - 1 != length(FILE_LIST[[1]]) - 1){
          close(pb)
          tkmessageBox(message = "Can't load file, different number of bins")
          STATE[3] <<- 0
          tcl("wm", "attributes", root, topmost=TRUE)
          return(FALSE)
        }
        gene_names <- gene_names[duplicated(gene_names)]
        if(length(gene_names) > 0){
          GENE_LISTS[[1]][[1]] <<- gene_names
        }else{
          close(pb)
          tkmessageBox(message = "Can't load file, no genes in common or remake your 
                       table files all the same way.")
          STATE[3] <<- 0
          tcl("wm", "attributes", root, topmost=TRUE)
          return(FALSE)
        }
      }else{
        #tkdelete(stop_box, 0, 'end')
        #tkinsert(stop_box, "end", num_bins[2] - 1)
        GENE_LISTS[[1]][[1]] <<- gene_names
      }
      file_count <- file_count + 1
      FILE_LIST_INFO[[file_count]] <<- c(file_name, paste(" NA's = ", 
                                                          sum(is.na(first_file))), 
                                               paste(" Zeors = ", 
                                                     sum(first_file==0, na.rm = TRUE)))
      GENE_LIST_INFO[[1]][[file_count]] <<- c(ld_name, legend_name,
                                                    my_dotlist[1], my_linelist[1], 
                                                    MY_COLORS[[tclvalue(tkget(color_sets))]][file_count], 1)
      first_file[is.na(first_file)] <- 0
      FILE_LIST[[file_count]] <<- first_file
    }
    tcl("wm", "attributes", root, topmost=TRUE)
    STATE[3] <<- 0  
    close(pb)
    return(TRUE)
  }
}

# After file is loaded comboboxes are added to and set
GetTableFileHelper <- function(...){
  if(GetTableFile()){
    tkconfigure(cbb_file, values=sapply(GENE_LIST_INFO[[1]], "[[", 1), state="active")
    tkconfigure(cbb_file, textvariable=tclVar(last(sapply(GENE_LIST_INFO[[1]], "[[", 1))))
    tkdelete(full_name2, 0, "end")
    tkinsert(full_name2, 0, tkget(cbb_file))
    cbb_configure()
  }
}

# sets comboboxs to correct values
cbb_configure <- function(){
  num <- as.numeric(tclvalue(tcl(cbb_file,"current")))+1
  if(!is.null(FILE_LIST[[1]]) & num > 0 & STATE[3] == 0){
    tkconfigure(cbb_color, textvariable=tclVar(sapply(GENE_LIST_INFO[[1]], "[[", 5)[num]))
    tkconfigure(cbb_line, textvariable=tclVar((sapply(GENE_LIST_INFO[[1]], "[[", 4)[num])))
    tkconfigure(cbb_dot, textvariable=tclVar(sapply(GENE_LIST_INFO[[1]], "[[", 3)[num]))
    tkconfigure(new_name, textvariable=tclVar(sapply(GENE_LIST_INFO[[1]], "[[", 2)[num]))
    tkconfigure(stats_list, listvariable = 
                  tclVar(as.character(FILE_LIST_INFO[[num]][2:3])))
  } 
}

# saves current seletion
cbb_setvalues <- function(){
  num <- as.numeric(tclvalue(tcl(cbb_file,"current")))+1
  if(!is.null(FILE_LIST[[1]]) & num > 0 & STATE[3] == 0){
    GENE_LIST_INFO[[1]][[num]][5] <<- tclvalue(tkget(cbb_color))
    GENE_LIST_INFO[[1]][[num]][4] <<- tclvalue(tkget(cbb_line))
    GENE_LIST_INFO[[1]][[num]][3] <<- tclvalue(tkget(cbb_dot))
    GENE_LIST_INFO[[1]][[num]][2] <<- tclvalue(tkget(new_name))
    
  } 
}

# Adds ability to change color sets
cbb_colorsets <- function(){
  num <- as.numeric(tclvalue(tcl(cbb_file,"current")))+1
  if(num < 1){
    num <- 1
  }
  if(STATE[3] == 0){
    tkconfigure(cbb_color, 
                textvariable=tclVar(MY_COLORS[[tclvalue(tkget(color_sets))]][num]))
    GENE_LIST_INFO[[1]][[num]][5] <<- tclvalue(tkget(cbb_color))
  }
}


# GUI function ----

# set up root window  
root <- tktoplevel() #container for it all
tcl("wm", "attributes", root, topmost = TRUE)
tkwm.title(root, my_version_num)

# menu setup
topMenu <- tkmenu(root)           # Create a menu
tkconfigure(root, menu = topMenu) # Add it to the main window
fileMenu <- tkmenu(topMenu, tearoff = FALSE)
tkadd(fileMenu, "command", label = "Load table file", command = function() 
  GetTableFileHelper())
tkadd(fileMenu, "command", label = "Load color pallet")
tkadd(fileMenu, "command", label = "Quit", command = function() 
  tkdestroy(root))
tkadd(fileMenu, "command", label = "Restart", command = function() 
  tkdestroy(root))
tkadd(topMenu, "cascade", label = "File", menu = fileMenu)

# notebook 
nb <- tk2notebook(root, tabs = c("Table files", "Gene lists", "Plot", 
                                        "tools"))
tkgrid(nb)

# tab1 table files ----
filetab <- tk2notetab(nb, "Table files")

# frame for file select 
tab1box1 <- tkframe(filetab, relief = 'ridge', borderwidth = 5)

main_list <- tklistbox(tab1box1, height = 1, width = 38, relief = 'flat', 
                       background = 'gray93')
tkgrid(main_list, sticky = "n", row = 0)	
tkinsert(main_list, 0, "                       List of table files")

cbb_file <- tk2combobox(tab1box1, state="readonly", width=35)
tkgrid(cbb_file, sticky = "n")
tkbind(cbb_file, "<<ComboboxSelected>>", cbb_configure)
tkconfigure(cbb_file, textvariable = sf, state = "disable")

color_sets <- tk2combobox(tab1box1, value = names(MY_COLORS), textvariable = cs, 
                          state="readonly")
tkgrid(color_sets, sticky = "s")
tkbind(color_sets, "<<ComboboxSelected>>", cbb_colorsets)

tkgrid(tkbutton(tab1box1, text = " Load Table File ", command =  function() 
  GetTableFileHelper()))
stats_list <- tklistbox(tab1box1)
tkgrid(stats_list)
tkconfigure(stats_list, height = 2, width = 30)

tkgrid(tab1box1, row = 0, sticky = "n")

# frame for file options and info
tab1box2 <- tkframe(filetab, relief = 'ridge', borderwidth = 5)
 
title_file <- tklistbox(tab1box2, height = 1, width = 38, relief = 'flat', 
                      background = 'gray93')
tkgrid(title_file, columnspan = 2)  
tkinsert(title_file, 0, "                     File options settings")

full_name1 <- tklistbox(tab1box2, height = 1, width = 6, relief = 'flat', 
                      background = 'gray93')
tkgrid(full_name1, padx = c(20, 0))  
tkinsert(full_name1, 0, "File:")
full_name2 <- tklistbox(tab1box2, height = 1, width = 35, relief = 'flat', 
                         background = 'gray93')
tkgrid(full_name2, column = 1, row = 1, sticky = "w", padx = c(0, 4)) 
tkinsert(full_name2, 0, paste(GENE_LIST_INFO[[1]][1]))
 
tkgrid(tklistbox(tab1box2, listvariable = tclVar("nickname:"), height = 1, width = 10, 
                 relief = 'flat', background = 'gray93'), padx = c(20, 0))

new_name <- tk2entry(tab1box2, width = 35)
tkgrid(new_name, sticky = "w", column = 1, row = 2, padx = c(0, 4))
tkinsert(new_name, 0,  paste(GENE_LIST_INFO[[1]][1]))
tkbind(new_name, "<Leave>", cbb_setvalues)

tkgrid(tklistbox(tab1box2, listvariable = tclVar("Line"), height = 1, width = 4, 
                 relief = 'flat', background = 'gray93'), padx = c(20, 0))

cbb_line <- tk2combobox(tab1box2, value = my_linelist, textvariable= ll,
                        state="readonly")
tkgrid(cbb_line, sticky = "w", column = 1, row = 3, padx = c(0, 16)) 
tkbind(cbb_line, "<<ComboboxSelected>>", cbb_setvalues)

tkgrid(tklistbox(tab1box2, listvariable = tclVar("dot"), height = 1, width = 4, 
                 relief = 'flat', background = 'gray93'), padx = c(20, 0))

cbb_dot <- tk2combobox(tab1box2, value = my_dotlist, textvariable= dl,
                       state="readonly") 
tkgrid(cbb_dot, sticky = "w", column = 1, row = 4, padx = c(0, 16)) 
tkbind(cbb_dot, "<<ComboboxSelected>>", cbb_setvalues)

tkgrid(tklistbox(tab1box2, listvariable = tclVar("color"), height = 1, width = 5, 
                 relief = 'flat', background = 'gray93'), padx = c(20, 0))

cbb_color <- tk2combobox(tab1box2, value = MY_COLORS[[tclvalue(tkget(color_sets))]], 
                         textvariable= cl, state="readonly")
tkgrid(cbb_color, sticky = "w", column = 1, row = 5, padx = c(0, 16)) 
tkbind(cbb_color, "<<ComboboxSelected>>", cbb_setvalues)


# cb <- tkcheckbutton(tab1box2, variable = tclVar(1), 
#                     text = tclvalue(sf))
# tkgrid(cb, sticky = "w")
# 
# tk helper functions tab1 ----
# tkbind(cbb_file, "<<ComboboxSelected>>", 
#        cb_configure(cb, tclvalue(sf)))
# 
# 
tkgrid(tab1box2, column = 1, row = 0)

#}

#expandTk()
