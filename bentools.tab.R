
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
STATE <- c(0, 0, 0, 1) # Keep track of the state and flow control 
# [1] active tab number, [2] check if load file window open, 
# [3] when busy stop user input/activity, [4] master plot check

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
my_math <- c(" mean", " sum", " median")
Legend_loc <- c("outside", "topright", "top", "topleft", "bottomright", "bottom", "bottomleft", "right", "left", "No legend")
plot_lines <- c("543 bins 20,20,40", "5 1.5k-2k 70 bins", "543 bins 10,10,10", "543 bins 20,20,20", "5 .5k-1.5k")

# tcl starting values ----

sf <- tclVar("Load File")
cs <- tclVar(names(MY_COLORS)[1])
ll <- tclVar(my_linelist[1])
dl <- tclVar(my_dotlist[1])
cl <- tclVar(MY_COLORS[[1]][1])
mm <- tclVar(my_math[1])
nbin <- tclVar("0")
cbvalue_rf <- tclVar(0)
Lloc <- tclVar(Legend_loc[4])
Legend_size <- tclVar(0.8)
Header <- tclVar('Header')
Header_size <- tclVar(2.0)
ylable <- tclVar('Y Label')
xlable <- tclVar('X Label')
ylable_size <- tclVar(1.7)
xlable_size <- tclVar(1.7)
Txt_one <- tclVar('TSS')
Txt_two <- tclVar('PolyA')
Txt_three <- tclVar('500')
Txt_four <- tclVar('500')
Txt_five <- tclVar('-1450')
Txt_six <- tclVar('+3450')
Txt_seven <- tclVar('')
Txt_eight <- tclVar('')
Pos_one <- tclVar(15.5)
Pos_two <- tclVar(45.5)
Pos_three <- tclVar(20.5)
Pos_four <- tclVar(40.5)
Pos_five <- tclVar(1)
Pos_six <- tclVar(80)
Pos_seven <- tclVar(0)
Pos_eight <- tclVar(0)
cbValue_ylim <- tclVar("0")
cbValue_lable <- tclVar("1")
max_ylim <- tclVar('')
min_ylim <- tclVar('')

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
                                                    MY_COLORS[[tclvalue(tkget(color_sets))]][file_count], paste("X",ld_name,sep=""))
      first_file[is.na(first_file)] <- 0
      FILE_LIST[[file_count]] <<- first_file
    }
    tcl("wm", "attributes", root, topmost=TRUE)
    STATE[3] <<- 0  
    close(pb)
    return(TRUE)
  }
}

# remove tips
  # tkdesroy(name)
  # rm(list=name) and Xname
  # then remove the lists 3x 

# After file is loaded comboboxes are added to and set
GetTableFileHelper <- function(...){
  if(GetTableFile()){
    tkconfigure(cbb_file, values=sapply(GENE_LIST_INFO[[1]], "[[", 1), state="active")
    tkconfigure(cbb_file, textvariable=tclVar(last(sapply(GENE_LIST_INFO[[1]], "[[", 1))))
    tkdelete(full_name2, 0, "end")
    tkinsert(full_name2, 0, tkget(cbb_file))
    assign(last(sapply(GENE_LIST_INFO[[1]], "[[", 6)),tclVar("1"),pos=1)
    assign(last(sapply(GENE_LIST_INFO[[1]], "[[", 1)), 
             tkcheckbutton(cgtabbox1,text= last(sapply(GENE_LIST_INFO[[1]], "[[", 1)),
                           variable=get(last(sapply(GENE_LIST_INFO[[1]], "[[", 6)))),
                           pos=1)
    tkgrid(get(last(sapply(GENE_LIST_INFO[[1]], "[[", 1))))
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

# notebook ----
nb <- tk2notebook(root, tabs = c("Table files", "Plot", "Gene lists", "tools"))
tkgrid(nb)

# tab loading table files ----
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
tkgrid(tab1box2, column = 1, row = 0)

# tab plot ----
tk2notetab.select(nb, "Plot") #remove when done

# legends, ylim, ...

plottab <- tk2notetab(nb, "Plot")

# box for plot settings
tab2box1 <- tkframe(plottab, relief = 'ridge', borderwidth = 5)

tkgrid(tklabel(tab2box1, text = "Plot Options", width = 38))  

tkgrid(tkbutton(tab2box1, font =c('bold', 23), text = '      Plot       '
                )) 

cbb_math <- tk2combobox(tab2box1, value = my_math, state="readonly")
tkgrid(cbb_math, sticky = "n")
#tkbind(cbb_math, "<<ComboboxSelected>>", cbb_configure)
tkconfigure(cbb_math, textvariable = mm)

cb_rf <- tkcheckbutton(tab2box1, text = "Relative Frequency" )
tkgrid(cb_rf)

cb_log2 <- tkcheckbutton(tab2box1, text = "log2 transformation" )
tkgrid(cb_log2)

tkgrid(tklistbox(tab2box1, listvariable = tclVar("Norm_to_bin"), height = 1, width = 12, 
                 relief = 'flat', background = 'gray93'), padx = c(50, 0), sticky = "w")

cbb_nb <- tk2combobox(tab2box1, textvariable = nbin, state="readonly", width = 3)
tkgrid(cbb_nb, sticky = "e", column = 0, row = 5, padx = c(0, 50)) 
#tkbind(cbb_nb, "<<ComboboxSelected>>", )

tkgrid(tab2box1, row = 0, sticky = "n")

#frame for bottom left

tab2box1_1 <- tkframe(plottab, relief='ridge', borderwidth = 5) 

tkgrid(tklabel(tab2box1_1, text = ' Plot Options ', width = 30), padx = c(5, 3), 
       pady = c(0, 2), columnspan = 6)

tkgrid(tk2combobox(tab2box1_1, state = "readonly", textvariable = Lloc, width = 17, 
                   values = Legend_loc), padx = c(5, 0), pady = c(0, 2), sticky = "e", 
       columnspan = 3)
tkgrid(tklabel(tab2box1_1, text = 'Size '), padx = c(5, 3), pady = c(0, 2), 
       column = 3, row = 1, sticky = "w")
tkgrid(tkwidget(tab2box1_1, type ="spinbox", from =0.1, to = 1, inc=0.1,
                width = 3, textvariable = Legend_size), padx = c(0, 10), 
       pady = c(0, 2), column = 4, row = 1, sticky = "w")

tkgrid(tk2entry(tab2box1_1, width = 20, textvariable = Header),  
       padx = c(5, 0), pady = c(0, 2), column = 0, row = 2, sticky = "e", columnspan = 3)
tkgrid(tklabel(tab2box1_1, text = 'Size '), padx = c(5, 3), pady = c(0, 2), 
       column = 3, row = 2, sticky = "w")
tkgrid(tkwidget(tab2box1_1, type ="spinbox", from =1, to = 4, inc=0.5, width = 3,
                textvariable = Header_size), padx = c(0, 10), pady = c(0, 2), column = 4, 
       row = 2, sticky =  "w")

tkgrid(tk2entry(tab2box1_1, width = 20, textvariable = xlable), padx = c(5, 0), 
       pady = c(0, 2), column = 0, row = 3, sticky = "e", columnspan = 3)
tkgrid(tklabel(tab2box1_1, text = 'Size '), padx = c(5, 3), pady = c(0, 2), 
       column = 3, row = 3, sticky = "w")
tkgrid(tkwidget(tab2box1_1, type ="spinbox", from =0.1, to = 3, inc=0.2, width = 3,
                textvariable = xlable_size), padx = c(0, 10), pady = c(0, 0), column = 4, 
       row = 3, sticky ="w")

tkgrid(tk2entry(tab2box1_1, width = 20, textvariable = ylable),  
       padx = c(5, 0), pady = c(0, 2), column = 0, row = 4, sticky = "e", columnspan = 3)
tkgrid(tklabel(tab2box1_1, text = 'Size '), padx = c(5, 3), pady = c(0, 2), 
       column = 3, row = 4, sticky = "w")
tkgrid(tkwidget(tab2box1_1, type ="spinbox", from =0.1, to = 3, inc=0.2, width = 3, 
                textvariable = ylable_size), padx = c(0, 10), pady = c(0, 2), column = 4,
       row =  4, sticky = "w")

cb_ylim <- tkcheckbutton(tab2box1_1)
tkconfigure(cb_ylim, variable = cbValue_ylim)
tkgrid(tklabel(tab2box1_1, text = 'Set Y axis?'), padx = c(5, 0), pady = c(0, 0), 
       column = 1, row = 5, sticky = "e", rowspan = 2)
tkgrid(cb_ylim, padx = c(5, 0), pady = c(0, 0), column = 2, row = 5, rowspan = 2, 
       sticky = "w")
max_ylim_box <- tk2entry(tab2box1_1, width = 6, textvariable = max_ylim)
tkgrid(max_ylim_box, padx = c(5, 0), pady = c(0, 0), column = 3, row = 5, sticky = "ws")
min_ylim_box <- tk2entry(tab2box1_1, width = 6, textvariable = min_ylim)
tkgrid(min_ylim_box, padx = c(5, 0), pady = c(0, 5), column = 3, row = 6, sticky = "nw")

tkgrid(tklabel(tab2box1_1, text = "Plot lines and lables"), pady = c(5, 5), 
       column = 0, columnspan = 3)

tkgrid(tk2entry(tab2box1_1, width = 5, textvariable = Txt_one),  
       padx = c(10, 0), pady = c(5, 0))
tkgrid(tklabel(tab2box1_1, text = 'Pos'), padx = c(5, 3), pady = c(5, 0), column = 1, 
       row = 8, sticky = "w")
tkgrid(tk2entry(tab2box1_1, width = 4, textvariable = Pos_one), column = 2, row = 8,
       sticky = "w", padx = c(0, 10), pady = c(5, 0))

tkgrid(tk2entry(tab2box1_1, width = 5, textvariable = Txt_two),  
       padx = c(10, 0), pady = c(3, 0))
tkgrid(tklabel(tab2box1_1, text = 'Pos'), padx = c(5, 3), pady = c(3, 0), column = 1,
       row = 9, sticky = "w")
tkgrid(tkentry(tab2box1_1, width = 4, textvariable = Pos_two), column = 2 , row = 9, 
       sticky = "w", padx = c(0, 10), pady = c(3, 0))

tkgrid(tk2entry(tab2box1_1, width = 5, textvariable = Txt_three),  
       padx = c(10, 0), pady = c(3, 0))
tkgrid(tklabel(tab2box1_1, text = 'Pos'), padx = c(5, 3), pady = c(3, 0), column = 1,
       row = 10, sticky = "w")
tkgrid(tk2entry(tab2box1_1, width = 4, textvariable = Pos_three), column = 2, row = 10,
       sticky = "w", padx = c(0, 10), pady = c(3, 0))

tkgrid(tk2entry(tab2box1_1, width = 5, textvariable = Txt_four),  
       padx = c(10, 0), pady = c(3, 4))
tkgrid(tklabel(tab2box1_1, text = 'Pos'), column = 1, row =11, sticky = "w",
       padx = c(5, 3), pady = c(3, 4))
tkgrid(tk2entry(tab2box1_1, width = 4, textvariable = Pos_four), column = 2, row = 11,
       sticky = "w", padx = c(0, 10), pady = c(3, 4))
  

tkgrid(tklabel(tab2box1_1, text = "More Bin labels"), pady = c(4, 3), row = 7, column = 3,
       columnspan = 3)

tkgrid(tk2entry(tab2box1_1, width = 5, textvariable = Txt_five),  
       padx = c(10, 0), pady = c(5, 0), column = 3, row = 8)
tkgrid(tklabel(tab2box1_1, text = 'Pos'), padx = c(5, 3), pady = c(5, 0), column = 4,
       row = 8, sticky = "w")
tkgrid(tk2entry(tab2box1_1, width = 3, textvariable = Pos_five), column = 5, row = 8, 
       sticky = "w", padx = c(0, 10), pady = c(5, 0))

tkgrid(tk2entry(tab2box1_1, width = 5, textvariable = Txt_six), 
       padx = c(10, 0), pady = c(5, 0), column = 3, row = 9)
tkgrid(tklabel(tab2box1_1, text = 'Pos'), padx = c(5, 3), pady = c(5, 0), column = 4, 
       row = 9, sticky = "w")
tkgrid(tk2entry(tab2box1_1, width = 3, textvariable = Pos_six), column = 5, row = 9, 
       sticky = "w", padx = c(0, 10), pady = c(5, 0))

tkgrid(tk2entry(tab2box1_1, width = 5, textvariable = Txt_seven), 
       padx = c(10, 0), pady = c(5, 0), column = 3, row = 10)
tkgrid(tklabel(tab2box1_1, text = 'Pos'), padx = c(5, 3), pady = c(5, 0), column = 4,
       row = 10, sticky = "w")
tkgrid(tk2entry(tab2box1_1, width = 3, textvariable = Pos_seven), column = 5, row = 10, 
       sticky = "w", padx = c(0, 10), pady = c(5, 0))

tkgrid(tk2entry(tab2box1_1, width = 5, textvariable = Txt_eight), 
       padx = c(10, 0), pady = c(5, 5), column = 3, row = 11)
tkgrid(tklabel(tab2box1_1, text = 'Pos'), padx = c(5, 3), pady = c(5, 5), column = 4,
       row = 11, sticky = "w")
tkgrid(tk2entry(tab2box1_1, width = 3, textvariable = Pos_eight), column = 5, row = 11, 
       sticky = "w", padx = c(0, 10), pady = c(5, 5))
 
tkgrid(tab2box1_1)

# plot checkboxs for each file and master checkbox, tabs for each list 

tab2box2 <- tkframe(plottab, relief = 'ridge', borderwidth = 5)
pnb <- tk2notebook(tab2box2, tabs =c("Common Genes","Gene list 1","Gene list 2", 
                                     "Gene list 3", " Gene list 4"))
tkgrid(pnb)
pttab <- tk2notetab(pnb, "Common Genes")
cgtabbox2 <- tkframe(pttab)

tkgrid(tklabel(cgtabbox2, text= "List of table files"), columnspan = 6)
tkgrid(tkbutton(cgtabbox2, text = "on"))
tkgrid(tkbutton(cgtabbox2, text = "off"), column = 1, row = 1)
tkgrid(cgtabbox2)

cgtabbox1 <- tkframe(pttab)
tkgrid(cgtabbox1)
tkgrid(tab2box2, column = 1, row = 0)




# tab  ----
#plottab <- tk2notetab(nb, "Plot")

#tab2box1 <- tkframe(plottab, relief = 'ridge', borderwidth = 5)
#tkgrid(tab2box1, row = 0, sticky = "n")

# frame for file options and info

#tab2box2 <- tkframe(plottab, relief = 'ridge', borderwidth = 5)
#tkgrid(tab2box2, column = 1, row = 0)

 # end ----
#}

#expandTk()
