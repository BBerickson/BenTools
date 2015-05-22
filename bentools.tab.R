require(ggplot2)
require(tcltk)
require(tcltk2)
require(dplyr)

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
                 "long dash line", "two dash line", "No line")
MY_COLORS <- list("colorset1" = c("blue", "green", "red", "orange", "purple", "yellow", rep("black",10)),
                  "colorset2" = c("red", "orange", "purple", "yellow", "blue", "green", rep("black",10)))
my_math <- c(" mean", " sum", " median")
plot_lines <- c("543 bins 20,20,40", "5 1.5k-2k 70 bins", "543 bins 10,10,10", "543 bins 20,20,20", "5 .5k-1.5k")

# tcl starting values ----

start_name <- tclVar("Load File")
start_col_list <- tclVar(names(MY_COLORS)[1])
start_line_list <- tclVar(my_linelist[1])
start_dot_list <- tclVar(my_dotlist[1])
start_color <- tclVar(MY_COLORS[[1]][1])
start_math <- tclVar(my_math[1])
nbin <- tclVar("0")
cbvalue_rf <- tclVar(0)
Header <- tclVar('')
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

# test function 
onOK <- function(){
  print("this works")
}

# reads in file, tests, fills out info 
GetTableFile <- function(...) {
  if(STATE[3] == 0){
    if(is.null(names(FILE_LIST))){
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
      if(file_count > 0){
        if(num_bins[2] - 1 != length(FILE_LIST[[1]]) - 1){
          close(pb)
          tkmessageBox(message = "Can't load file, different number of bins")
          STATE[3] <<- 0
          tcl("wm", "attributes", root, topmost=TRUE)
          return(FALSE)
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
          return(FALSE)
        }
      }else{ # first time setting it up
        
        #tkdelete(stop_box, 0, 'end')
        #tkinsert(stop_box, "end", num_bins[2] - 1)
        GENE_LISTS$main$common <<- unique(first_file[,1])
      }
      file_count <- file_count + 1
      FILE_LIST_INFO[ld_name] <<- list(c(file_name, paste(" NA's = ", 
                                                          sum(is.na(first_file))), 
                                               paste(" Zeors = ", 
                                                     sum(first_file==0, na.rm = TRUE))))
      GENE_LIST_INFO$main[ld_name] <<- list(c(ld_name, legend_name, my_dotlist[1], 
                                              my_linelist[1],
                                              MY_COLORS[[tclvalue(tkget(color_sets))]][file_count], 
                                              1))
      first_file[is.na(first_file)] <- 0
      FILE_LIST[ld_name] <<- list(first_file)
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
    tkconfigure(cbb_file, values=sapply(GENE_LIST_INFO$main, "[[", 1), state="active")
    tkconfigure(cbb_file, textvariable=tclVar(last(sapply(GENE_LIST_INFO$main, "[[", 1))))
    cbb_configure()
  }
}

# plotting chunck ----

# Makes data frame and gathers plot settings for plotting active samples
MakeDataFrame <- function(){
  if(STATE[3] == 1 | is.null(names(FILE_LIST))){
    return()
  }else{
    tk2notetab.select(nb, "Plot")
    use_col <- NULL
    use_dot <- NULL
    use_line <- NULL
    use_name <- NULL
    wide_list <- list()
    for(i in names(GENE_LISTS)){
      if(sum(as.numeric(sapply(GENE_LIST_INFO[[i]], "[[", 6))) == 0){ # checks to see if at least one in list is acitve
        return()
      }else{
        enesg <- data.frame(gene=GENE_LISTS[[i]][[1]])
        lapply(names(FILE_LIST), function(k) 
          if(as.numeric(GENE_LIST_INFO[[i]][[k]][6]) == 1){  # uses only acive lists        
            wide_list[[k]] <<- data.frame(inner_join(enesg, FILE_LIST[[k]], by = "gene"))
            dot <- which(my_dotlist == GENE_LIST_INFO[[i]][[k]][3])
            if(dot == 21){
              dot <- 0
            }
            line <- which(my_linelist == GENE_LIST_INFO[[i]][[k]][4])
            if(line > 6){
              line <- 0
            }
            use_col <<- c(use_col, GENE_LIST_INFO[[i]][[k]][5])
            use_dot <<- c(use_dot, dot)
            use_line <<- c(use_line, line)
            use_name <<- c(use_name, GENE_LIST_INFO[[i]][[k]][2])
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
  math_list <- lapply(seq_along(wide_list), function(i) 
    data.frame(bin=(seq_along(wide_list[[i]][-1])), 
               set=(as.character(use_name[i])), 
               value=my_apply(wide_list[[i]][,-1]), stringsAsFactors = FALSE)) 
  long_list <- rbind_all(math_list)
  GGplotF(long_list, use_col, use_dot, use_line, y_lab)
}

# ggplot function
GGplotF <- function(long_list, use_col, use_dot, use_line, y_lab){
  gp <- ggplot(long_list, aes(x=bin, y=value, group=set, color=set, shape=set, linetype=set)) +
  # Use larger points, fill with white
    geom_line(size=1.5) + geom_point(size=4) +  
    #ylim(0, 0.0009) +             # Set y range
    scale_color_manual(name="Sample", values=use_col)+
    scale_shape_manual(name="Sample", values=use_dot) + 
    scale_linetype_manual(name="Sample", values=use_line)+
    xlab("Bins") + ylab(y_lab) + # Set axis labels
    ggtitle(tclvalue(Header)) +  # Set title
    scale_x_continuous( breaks=c(10,20,30,70),
                        minor_breaks=seq(50,60,by=2) )+
    theme_bw() +
    theme(plot.title = element_text(size = 30, vjust = 2))+
    theme(axis.title.y = element_text(size =  20, vjust = 1.5))+
    theme(axis.title.x = element_text(size =  25, vjust = 0))+
    theme(axis.text.x = element_text(size = 15, hjust = .5, vjust = 1, face = 'bold'))
 print(gp)
  
}

# translates dot and line chr to number
#TODO

# combobox functions ----

# sets comboboxs to correct values
cbb_configure <- function(){
  num <- tclvalue(tkget(cbb_file))
  if(!is.null(names(FILE_LIST)) & STATE[3] == 0){
    if(num == ""){
      num <- tclvalue(tkget(full_name2,0))
      tkconfigure(cbb_file, textvariable=tclVar(num))
    }else{
      tkdelete(full_name2, 0, "end")
      tkinsert(full_name2, 0, num)
    }
    tkconfigure(cbb_color, textvariable=tclVar(sapply(GENE_LIST_INFO$main, "[[", 5)[num]))
    tkconfigure(cbb_line, textvariable=tclVar(sapply(GENE_LIST_INFO$main, "[[", 4)[num]))
    tkconfigure(cbb_dot, textvariable=tclVar(sapply(GENE_LIST_INFO$main, "[[", 3)[num]))
    tkconfigure(new_name, textvariable=tclVar(sapply(GENE_LIST_INFO$main, "[[", 2)[num]))
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
  num <- as.numeric(tclvalue(tcl(cbb_file,"current")))+1
  if(num < 1){
    num <- 1
  }
  if(STATE[3] == 0){
    tkconfigure(cbb_color, 
                textvariable=tclVar(MY_COLORS[[tclvalue(tkget(color_sets))]][num]))
    GENE_LIST_INFO$main[[num]][5] <<- tclvalue(tkget(cbb_color))
  }
}


# GUI function ----

# set up root window ---- 

root <- tktoplevel() #container for it all
tcl("wm", "attributes", root, topmost = TRUE)
tkwm.title(root, my_version_num)

# menu setup ----
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
# needed to add so that when switching tabs things get updated right
tkbind(filetab, "<Visibility>", cbb_configure)

# frame for file select 
tab1box1 <- tkframe(filetab, relief = 'ridge', borderwidth = 5)

main_list <- tklistbox(tab1box1, height = 1, width = 38, relief = 'flat', 
                       background = 'gray93')
tkgrid(main_list, sticky = "n", row = 0)	
tkinsert(main_list, 0, "                       List of table files")

cbb_file <- tk2combobox(tab1box1, state="readonly", width=35)
tkgrid(cbb_file, sticky = "n")
tkbind(cbb_file, "<<ComboboxSelected>>", cbb_configure)
tkconfigure(cbb_file, textvariable = start_name, state = "disable")

color_sets <- tk2combobox(tab1box1, value = names(MY_COLORS), textvariable = start_col_list, 
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
tkinsert(full_name2, 0, paste(GENE_LIST_INFO$main[1]))
 
tkgrid(tklistbox(tab1box2, listvariable = tclVar("nickname:"), height = 1, width = 10, 
                 relief = 'flat', background = 'gray93'), padx = c(20, 0))

new_name <- tk2entry(tab1box2, width = 35)
tkgrid(new_name, sticky = "w", column = 1, row = 2, padx = c(0, 4))
tkinsert(new_name, 0,  paste(GENE_LIST_INFO$main[1]))
tkbind(new_name, "<Leave>", cbb_setvalues)

tkgrid(tklistbox(tab1box2, listvariable = tclVar("Line"), height = 1, width = 4, 
                 relief = 'flat', background = 'gray93'), padx = c(20, 0))

cbb_line <- tk2combobox(tab1box2, value = my_linelist, textvariable= start_line_list,
                        state="readonly")
tkgrid(cbb_line, sticky = "w", column = 1, row = 3, padx = c(0, 16)) 
tkbind(cbb_line, "<<ComboboxSelected>>", cbb_setvalues)

tkgrid(tklistbox(tab1box2, listvariable = tclVar("dot"), height = 1, width = 4, 
                 relief = 'flat', background = 'gray93'), padx = c(20, 0))

cbb_dot <- tk2combobox(tab1box2, value = my_dotlist, textvariable= start_dot_list,
                       state="readonly") 
tkgrid(cbb_dot, sticky = "w", column = 1, row = 4, padx = c(0, 16)) 
tkbind(cbb_dot, "<<ComboboxSelected>>", cbb_setvalues)

tkgrid(tklistbox(tab1box2, listvariable = tclVar("color"), height = 1, width = 5, 
                 relief = 'flat', background = 'gray93'), padx = c(20, 0))

cbb_color <- tk2combobox(tab1box2, value = MY_COLORS[[tclvalue(tkget(color_sets))]], 
                         textvariable= start_color, state="readonly")
tkgrid(cbb_color, sticky = "w", column = 1, row = 5, padx = c(0, 16)) 
tkbind(cbb_color, "<<ComboboxSelected>>", cbb_setvalues)
tkgrid(tab1box2, column = 1, row = 0)

# frame for plot button to switch to plot tab and plot

tab1box3 <- tkframe(filetab, relief = 'ridge', borderwidth = 5)
tkgrid(tkbutton(tab1box3, font =c('bold', 23), text = '      Plot       ', 
                command = function() MakeDataFrame())) 
tkgrid(tab1box3, column = 0, row = 1)

# tab plot ----


# legends, ylim, ...

plottab <- tk2notetab(nb, "Plot")

# box for plot settings
tab2box1 <- tkframe(plottab, relief = 'ridge', borderwidth = 5)

tkgrid(tklabel(tab2box1, text = "Plot Options", width = 38))  

tkgrid(tkbutton(tab2box1, font =c('bold', 23), text = '      Plot       ', 
                command = function() MakeDataFrame())) 


cbb_math <- tk2combobox(tab2box1, value = my_math, state="readonly")
tkgrid(cbb_math, sticky = "n")
tkconfigure(cbb_math, textvariable = start_math)

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


tkgrid(tk2entry(tab2box1_1, width = 20, textvariable = Header),  
       padx = c(5, 0), pady = c(0, 2), column = 0, row = 2, sticky = "e", columnspan = 3)

tkgrid(tklabel(tab2box1_1, text = "Plot lines and lables"), pady = c(5, 5), row = 7,
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
cgonbox <- tk2listbox(cgtabbox2, width = 40, height = 13)
tkgrid(cgonbox, columnspan = 3)
tkgrid(tkbutton(cgtabbox2,text="<<Switch>>"), 
       tkbutton(cgtabbox2,text="<<All On>>"), tkbutton(cgtabbox2,text="<<All Off>>"),
       sticky = 'we')
cgoffbox <- tk2listbox(cgtabbox2, width = 40, height = 13)
tkgrid(cgoffbox, columnspan = 3)
tkgrid(cgtabbox2)

cgtabbox1 <- tkframe(pttab)
tkgrid(cgtabbox1)
tkgrid(tab2box2, column = 1, row = 0, rowspan = 2)




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
