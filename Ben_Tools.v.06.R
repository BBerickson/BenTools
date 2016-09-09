my_version_num <- 'Ben Tools v.06b1'

# fix cancel load file if there is no file already loaded
# fix if all bins are NA/0 ploting
# add plot all gene lists button
# fix end bin box in gene tools to be independent like start box
# laod file auto plot
# add stat plot button
# ablility to not have dots
# remove start line and bin defalut box and add ablitiy in plot options
# 
# update and alow laoding of color files
# pass created gene list to loaded list

##### new ########
# add bins to norm to gene check box
# add intersect top/bottom beween the two lists to 3ed
# add incusive gene list to 3ed on on region and ratio
# norm file no longer log2, now log2 checkbox on plotting
# disabled stats plot ... need to make button
# made per gene RF
# made clustering
# removed the header, ylable and xlable defalut on plot
# thicked up lines and asis tiks and text
# outside legend now on top of plot ... needs more work
# norm file ok closes all sub windows

# load packages ----
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

if(require("flashClust")){
  print("flashClust is loaded correctly")
} else {
  print("trying to install flashClust")
  install.packages("flashClust")
  if(require(flashClust)){
    print("flashClust installed and loaded")
  } else {
    stop("could not install flashClust")
  }
}

#Globals ----
my_colors <- c("#a6cee3",  "#1f78b4",  "#b2df8a",  "#33a02c",  "#fb9a99",  "#e31a1c",  "#fdbf6f",  "#ff7f00",  "#cab2d6", "#a6cee3",  "#1f78b4",  "#b2df8a",  "#33a02c",  "#fb9a99",  "#e31a1c",  "#fdbf6f",  "#ff7f00",  "#cab2d6","black",  "red",  "green",  "blue",  "grey",  "pink",  "purple",  "burgundy",  "orange", "brown",  "fuchsia",  "cyan",  "#33a02c",  "#fb9a99",  "#e31a1c",  "#fdbf6f",  "#ff7f00",  "#cab2d6")
FILE_LIST <- list(NULL) # for holding table files in list
my_list <- list(NULL, NULL, NULL) # [[1]] all common genes, [[2]] list name and all common genes, [[3]] name (do I use this one on loaded gene lists?)
GENE_LIST <- list(list(NULL, "Genes in common",NULL, NULL, NULL, NULL, NULL,NULL), my_list, my_list, my_list, my_list) 
	# [[1]] all common genes, [[2]] list name and all common genes, [[3]] name, 
	# [[4]] full file name, [[5]] number of genes, [[6]] number of observations, [[7]] number of 0's, [[8]] number of NA's, [[9]] for line segments  
COLOR_LIST <- list(my_colors, my_colors, my_colors, my_colors, my_colors) # list for plot colors
CHECK_LIST <- list(1, 0, 0, 0, 0)	# c(master check, sub checks)
OPTIONS_LIST <- list(list(NULL),list(NULL),list(NULL),list(NULL),list(NULL)) # [[1]] dot [[2]] line [[3]] log2 transformed record
FRAME_NAMES <- c("mainframe", "lineOptionsframe", "SampleOptionsFrame", "SubOptionsFrame")
LIST_NAMES <- c("G1_main", "file_list", "gene_list0",  "gene_list1", "gene_list2", 
	"gene_list3", "gene_main_count", "G1C_main", "gene_list0_count",	"gene_list1_count", "gene_list2_count", "gene_list3_count")

STATE <- c(0, 0, 0, 0, 0, 0, 0) # Keep track of the state and flow control 
  # [1] check for state of gene/file list, [2] check if plot option window open, 
	# [3] check if sample option window open, [4] check if sub option window open, 
	# [5] 1 when running functions, [6] active list, [7] active list number 

#tclvar ----
Legend_loc <- c("outside", "topright", "top", "topleft", "bottomright", "bottom", "bottomleft", "right", "left", "No legend")
Legend_loc2 <- tclVar(Legend_loc[4])
Legend_size <- tclVar(0.8)
Header <- tclVar('')
Header_size <- tclVar(2.0)
ylable <- tclVar('')
xlable <- tclVar('')
ylable_size <- tclVar(1.7)
xlable_size <- tclVar(1.7)
Txt_one <- tclVar('TSS')
Txt_two <- tclVar('TTS')
Txt_three <- tclVar('500')
Txt_four <- tclVar('500')
Txt_five <- tclVar('-1450 3450')
Pos_one <- tclVar(15.5)
Pos_two <- tclVar(45.5)
Pos_three <- tclVar(20.5)
Pos_four <- tclVar(40.5)
Pos_five <- tclVar("1 80")
cbValue_ylim <- tclVar("0")
cbValue_lable <- tclVar("1")
cbValue_relative_frequency <- tclVar("0")
cbValue_gene_relative_frequency <- tclVar("0")
cbValue_gene_cluster_relative_frequency <- tclVar("1")
cbValue_log2 <- tclVar("0")
cbValue_gene_norm <- tclVar("0")
start_bin <- tclVar(1) 
stop_bin <- tclVar(1)
start_bin_norm <- tclVar(1)
stop_bin_norm <- tclVar(1)
max_ylim <- tclVar('')
min_ylim <- tclVar('')
my_math <- c(" mean", " sum", " median")
my_math2 <- tclVar(' mean')
my_Pos_Txt <- c("543 bins 20,20,40", "5 1.5k-2k 70 bins", "543 bins 10,10,10", "543 bins 20,20,20", "5 .5k-1.5k")
my_Pos_Txt2 <- tclVar(my_Pos_Txt[1])
my_percent  <- c(" top_25%", " top_50%", " top_75%", " ALL", " bottom_50%", " bottom_25%", 
	" bottom_10%", " bottom_5%", " ALL", " top_5%", " top_10%")
my_dotlist <- c("circle", "triangle point up", "plus", "cross", "diamond", 
				"triangle point down", "square cross", "star", "diamond plus", "circle  plus", "triangles up and down", 
				"square  plus", "circle cross", "square and triangle down", "filled square", "filled circle", 
				"filled triangle point up", "filled diamond", "solid circle", "bullet (smaller circle)", "square")
my_linelist <- c("solid line", "dashed line", "dotted line", "dot dash line", "long dash line", 
				"two dash line", "split line")
my_percent2  <- tclVar(' top_25')
norm_bin <- tclVar(0)
one_diff <- tclVar(2)
fold_diff <- tclVar(2)
start_bin1 <- tclVar(1)
one_bin1 <- tclVar(1) 
start_bin_cluster <- tclVar(1)

# code ----

# keeps numbers, empty string for the rest
# from https://github.com/gsk3/taRifx/blob/master/R/Rfunctions.R#L1161
Destring <- function(x, keep = "0-9.-") {
  return(as.numeric(gsub(paste("[^", keep, "]+", sep=""), "", x)))
}

#reads in file and pass on to fill out genes in lists ... STATS, GENE_LIST 
GetTableFile <- function(biglist, genelist, GM, G1, G2, G3, G4, GMC, G1gC, G1C, G2C, G3C, G4C) {
	if(BusyTest()){ 
		file_count <- as.integer(tksize(GM))
		DActLst(active_list, active_lable, c(3,4)) # closes open name/color/line/dot window and sub window, and resets gene tools
		STATE[5] <<- 1
		if(file_count > 10){ 
			tkmessageBox(message = "I have too many files, you need to reset me or remove some files")
			STATE[5] <<- 0
			return()
		}
		tcl("wm", "attributes", mainframe, topmost=F)
		pb <<- tkProgressBar(title = "Loading file, please be patient!!", width = 300 ) # move progress for every step
		file_name <- tclvalue(tkgetOpenFile(filetypes = "{{Table Files} {.table .tab .Table}}"))
		if(!nchar(file_name)) { ## file select test
			StatsPlot()
			close(pb)
			STATE[5] <<- 0
			return()
		}else if(file_name %in% GENE_LIST[[1]][[4]]){
			tkmessageBox(message = "This file has already been loaded")
			#StatsPlot()
			STATE[5] <<- 0
			close(pb)
			tcl("wm", "attributes", mainframe, topmost=TRUE)
			return()
		}else{
			l_name <- strsplit(as.character(file_name), '/')
			ld_name <- paste(l_name[[1]][(length(l_name[[1]]))])
			legend_name <- paste(strsplit(as.character(ld_name), '.ta')[[1]][1])
			first_file <- read.table(file_name, header = TRUE, stringsAsFactors= FALSE, comment.char = "")
			names(first_file)[1]<-paste("gene")
			
 			first_file[,-1][is.na(first_file[,-1])] <- 0 # need to test for speed
			first_file[,-1] <- as.numeric((as.matrix(first_file[,-1])))
			
			num_bins <- dim(first_file)
			gene_names <- c(GENE_LIST[[1]][[2]][-1], unique(first_file[,1])) # change to test
			if(file_count > 0){
				if(num_bins[2] - 1 != length(FILE_LIST[[1]]) - 1){
					close(pb)
					tkmessageBox(message = "Can't load file, different number of bins")
					STATE[5] <<- 0
					tcl("wm", "attributes", mainframe, topmost=TRUE)
					#return()
				}
				gene_names <- gene_names[duplicated(gene_names)] # pass to handle gene list
				if(length(gene_names) > 0){
					GENE_LIST[[1]][[2]] <<- c(GENE_LIST[[1]][[2]][1], gene_names)
				}else{
					close(pb)
					tkmessageBox(message = "Can't load file, no genes in common or remake your table files all the same way.")
					STATE[5] <<- 0
					tcl("wm", "attributes", mainframe, topmost=TRUE)
					return()
				}
			}else{
				tkdelete(stop_box, 0, 'end')
				tkinsert(stop_box, "end", num_bins[2] - 1)
				stop_bin_norm <<- tclVar(num_bins[2] - 1)
				GENE_LIST[[1]][[2]] <<- c(GENE_LIST[[1]][[2]][1], gene_names)
			}
			file_count <- file_count + 1
			tkinsert(GM, "end", legend_name) 
			GENE_LIST[[1]][[3]][file_count] <<- legend_name 
			GENE_LIST[[1]][[4]][file_count] <<- file_name
			GENE_LIST[[1]][[5]][file_count] <<- num_bins[1]
			GENE_LIST[[1]][[6]] <<-  c(GENE_LIST[[1]][[6]], (sum(first_file==0) / (num_bins[1] * num_bins[2])) * 100, 
				(sum(is.na(first_file)) / (num_bins[1] * num_bins[2])) * 100)
			GENE_LIST[[1]][[7]] <<- c(GENE_LIST[[1]][[7]], summary(colMeans(first_file[,-1]))[1:6]) #summary[7] holds NA's if they exist  
			UpdateFileLists(file_count, gene_names, GM, G1, G2, G3, G4, GMC, G1C, G2C, G3C, G4C, c(0:4), TRUE, 0) # change c(1:4) option in the function, only run if number of common genes changes
			FILE_LIST[[file_count]] <<- first_file
		}
		#StatsPlot()
		STATE[5] <<- 0	
		close(pb)
	}
  tcl("wm", "attributes", mainframe, topmost = TRUE)

}

#build GENE_LIST
BuildGeneList <- function(){

}

#Plot stats on loaded file
StatsPlot <- function(){ # update pb, # move legend to outside and adjust margins 
  #need to add gene number to plot
  cc <- NULL 
  count <-length(GENE_LIST[[1]][[3]])
	for(i in 1 : count){
			cc <- c(cc, COLOR_LIST[[1]][i])
		}
		mm2 <- matrix(GENE_LIST[[1]][[7]],ncol=6,byrow=T)
		mm <- matrix(GENE_LIST[[1]][[6]],ncol=2,byrow=T)	
    mm2[is.na(mm2)] <- 0
    mm[is.na(mm)] <- 0
    par(mfrow=c(2,1))
    par(mar=c(2.1, 4.1, 2.1, 13.1))
    bplt <- barplot(mm, col = cc, beside=T, ylim = c(0,100), names.arg=c("% 0's", "% NA's"))
    text(x=bplt , y= mm + 6, font =2 ,col = cc, labels=paste(as.character(signif(mm,2)),"%",sep=""), cex = 0.8)
    par(xpd=TRUE)# works on outside location
    legend((2+2*count),95, legend=paste(GENE_LIST[[1]][[3]]), bty = "o",cex=.8, text.col = cc) 
    title(main = "Bins with no signal")
    par(mar=c(3, 4, 2, 2) + 0.1)
    bplt2 <- barplot(mm2, col = cc, beside=T, names.arg=c("Min", "1st Qu", "Median", "Mean", "3rd Qu", "Max"))
    text(x=bplt2 , y= mm2 + max(mm2)*.1 , col = cc, labels=paste(as.character(signif(mm2,1))),cex=.6)
    title(main = "Mean row summary")
    par(mfrow=c(1,1), mar=c(5, 4, 4, 2) + 0.1, xpd=FALSE)
}

#Busy test ... STATE[5]
BusyTest <- function (){ #need to move tkfocus(bp)
	if(STATE[5] == 0){
		return(TRUE)
	}else{
		#tkfocus(bp) #need to move
		return(FALSE)
	}
}

#test for running or some open sub windows... STATE, FRAME_NAMES 
CloseSubWindows <- function(nums){
	for(i in nums){
		if(i < 5 & STATE[i] > 0){
			STATE[i] <<- 0
			tkdestroy(get(FRAME_NAMES[i]))
		}
	}
}

#update file lists and check box state, keep pb?
UpdateFileLists <- function(file_count, gene_names, GM, G1, G2, G3, G4, GMC, G1C, G2C, G3C, G4C, num, test, mylog){
	pb <- tkProgressBar(title = "be patient!! ",  min = 0, max = 100, width = 300 )
		for(i in num){
			setTkProgressBar(pb, i, label = paste(round(i/4*100, 0), "remaking gene lists"))
			
			if(i == 0){
				GENE_LIST[[1]][[1]] <<- HandleGeneList(GM, file_count, GENE_LIST[[1]][[2]][-1], gene_names, GENE_LIST[[1]][[2]][1], GM, GMC)
				CHECK_LIST[[1]] <<- SetMasterCheck(file_count, CHECK_LIST[[1]], COLOR_LIST[[1]], cb_mainlist, GM, test)
				if(length(OPTIONS_LIST[[1]])-1 < file_count){
					OPTIONS_LIST[[1]][[file_count]] <<- c("filled circle", "solid line", mylog)
				}
			}else if(i == 1){
				GENE_LIST[[2]][[1]] <<- HandleGeneList(GM, file_count, GENE_LIST[[2]][[2]][-1], GENE_LIST[[1]][[1]], GENE_LIST[[2]][[2]][1], G1, G1C)
				CHECK_LIST[[2]] <<- SetMasterCheck(file_count, CHECK_LIST[[2]], COLOR_LIST[[2]], cb_mainlist1, G1, test)
				if(length(OPTIONS_LIST[[2]])-1 < file_count){
					OPTIONS_LIST[[2]][[file_count]] <<- c("filled circle", "solid line", mylog)
				}
			}else if(i == 2){
				GENE_LIST[[3]][[1]] <<- HandleGeneList(GM, file_count, GENE_LIST[[3]][[2]][-1], GENE_LIST[[1]][[1]], GENE_LIST[[3]][[2]][1], G2, G2C)
				CHECK_LIST[[3]] <<- SetMasterCheck(file_count, CHECK_LIST[[3]], COLOR_LIST[[3]], cb_mainlist2, G2, test)
				if(length(OPTIONS_LIST[[3]])-1 < file_count){
					OPTIONS_LIST[[3]][[file_count]] <<- c("filled circle", "solid line", mylog)
				}
			}else if(i == 3){
				GENE_LIST[[4]][[1]] <<- HandleGeneList(GM, file_count, GENE_LIST[[4]][[2]][-1], GENE_LIST[[1]][[1]], GENE_LIST[[4]][[2]][1], G3, G3C)
				CHECK_LIST[[4]] <<- SetMasterCheck(file_count, CHECK_LIST[[4]], COLOR_LIST[[4]], cb_mainlist3, G3, test)
				if(length(OPTIONS_LIST[[4]])-1 < file_count){
					OPTIONS_LIST[[4]][[file_count]] <<- c("filled circle", "solid line", mylog)
				}
			}else{
				GENE_LIST[[5]][[1]] <<- HandleGeneList(GM, file_count, GENE_LIST[[5]][[2]][-1], GENE_LIST[[1]][[1]], GENE_LIST[[5]][[2]][1], G4, G4C)
				CHECK_LIST[[5]] <<- SetMasterCheck(file_count, CHECK_LIST[[5]], COLOR_LIST[[5]], cb_mainlist4, G4, test)
				if(length(OPTIONS_LIST[[5]])-1 < file_count){
					OPTIONS_LIST[[5]][[file_count]] <<- c("filled circle", "solid line", mylog)
				}
			}			
		}
	close(pb)
	return()
}

#turn off master check list if active list is turned off 
SetMasterCheck <- function(file_num, active_list, color_list, cbgene, temp, test){
	if(file_num > 0){
		if(active_list[1] == "0"){
			tkdeselect(cbgene)
			for(i in 1 : file_num){
				if(test){
					active_list[i + 1] <- "0"
				}
			}
		}else{
			tkselect(cbgene)
			for(i in 1 : file_num){
				if(test){
					active_list[1 + i] <- "1"
				}
			}
		}
		for(i in 1 : file_num){
			if(active_list[i+1] == 0 & as.integer(tksize(temp)) == file_num){
				tkitemconfigure(temp, (i - 1),foreground = "gray")
			}else	if(active_list[i+1] == 1 & as.integer(tksize(temp)) == file_num){
				tkitemconfigure(temp, (i - 1), foreground = color_list[i])
			}
		}
	return(active_list)
	}
	return(active_list)
}

#master check box R and tcltk conversion
HandleMasterCheck <- function(file_num, main_list, color_list, cbgene, temp){
	if(main_list[1] == "0"){	
		main_list[1] <- "1"
	}else{
		main_list[1] <- "0"
	}
	main_list <- SetMasterCheck(file_num, main_list, color_list, cbgene, temp, TRUE)
	return(main_list)
}

#sorts gene lists to active lists based on what is in common and update name and count
HandleGeneList <- function(list1, len, gene_list, list_in, list_name, gg, ggc){ 
	if(length(gene_list) > 1 & length(list_in) > 0){ # need list in size check?
		lmod <- as.integer(tksize(gg))
		if(len != lmod){
			for(i in (1 + lmod) : len){
				temp <-  paste(tkget(list1, (i-1),(i-1)))
				tkinsert(gg, "end", temp)
			}
		}
		tkdelete(ggc, 0, "end")
		list_m <- c(gene_list, list_in)
		list_out <- list_m[duplicated(list_m)]
		lst <- paste('  n = ', length(list_out), ' : ', list_name)
		tkinsert(ggc, "end", lst)
		return(list_out)		
	}
	return("")
}

# filling out Pos and Txt entry boxes
Pos_Txt <- function(my_Pos_Txt2){
  my_name <- tclvalue(my_Pos_Txt2)
  if(my_name == "5 .5k-1.5k"){
    tkdelete(Box_Pos_one, 0, 'end')
    tkinsert(Box_Pos_one, 0, '10.5')
    tkdelete(Box_Pos_two, 0, 'end')
    tkinsert(Box_Pos_two, 0, '0')
    tkdelete(Box_Pos_three, 0, 'end')
    tkinsert(Box_Pos_three, 0, '0')
    tkdelete(Box_Pos_four, 0, 'end')
    tkinsert(Box_Pos_four, 0, '0')
    tkdelete(Box_Pos_five, 0, 'end')
    tkinsert(Box_Pos_five, 0, '1 40')
    tkdelete(Box_Txt_one, 0, 'end')
    tkinsert(Box_Txt_one, 0, 'TSS')
    tkdelete(Box_Txt_two, 0, 'end')
    tkinsert(Box_Txt_two, 0, '')
    tkdelete(Box_Txt_three, 0, 'end')
    tkinsert(Box_Txt_three, 0, '')
    tkdelete(Box_Txt_four, 0, 'end')
    tkinsert(Box_Txt_four, 0, '')
    tkdelete(Box_Txt_five, 0, 'end')
    tkinsert(Box_Txt_five, 0, '-450 1450')
  } else if(my_name == "543 bins 10,10,10") {
    tkdelete(Box_Pos_one, 0, 'end')
    tkinsert(Box_Pos_one, 0, '5.5')
    tkdelete(Box_Pos_two, 0, 'end')
    tkinsert(Box_Pos_two, 0, '25.5')
    tkdelete(Box_Pos_three, 0, 'end')
    tkinsert(Box_Pos_three, 0, '10.5')
    tkdelete(Box_Pos_four, 0, 'end')
    tkinsert(Box_Pos_four, 0, '20.5')
    tkdelete(Box_Pos_five, 0, 'end')
    tkinsert(Box_Pos_five, 0, '1 30')
    tkdelete(Box_Txt_one, 0, 'end')
    tkinsert(Box_Txt_one, 0, 'TSS')
    tkdelete(Box_Txt_two, 0, 'end')
    tkinsert(Box_Txt_two, 0, 'TTS')
    tkdelete(Box_Txt_three, 0, 'end')
    tkinsert(Box_Txt_three, 0, '500')
    tkdelete(Box_Txt_four, 0, 'end')
    tkinsert(Box_Txt_four, 0, '500')
    tkdelete(Box_Txt_five, 0, 'end')
    tkinsert(Box_Txt_five, 0, '-450 1450')
  } else if(my_name == "543 bins 20,20,20"){
    tkdelete(Box_Pos_one, 0, 'end')
    tkinsert(Box_Pos_one, 0, '10.5')
    tkdelete(Box_Pos_two, 0, 'end')
    tkinsert(Box_Pos_two, 0, '50.5')
    tkdelete(Box_Pos_three, 0, 'end')
    tkinsert(Box_Pos_three, 0, '20.5')
    tkdelete(Box_Pos_four, 0, 'end')
    tkinsert(Box_Pos_four, 0, '40.5')
    tkdelete(Box_Pos_five, 0, 'end')
    tkinsert(Box_Pos_five, 0, '1 60')
    tkdelete(Box_Txt_one, 0, 'end')
    tkinsert(Box_Txt_one, 0, 'TSS')
    tkdelete(Box_Txt_two, 0, 'end')
    tkinsert(Box_Txt_two, 0, 'TTS')
    tkdelete(Box_Txt_three, 0, 'end')
    tkinsert(Box_Txt_three, 0, '500')
    tkdelete(Box_Txt_four, 0, 'end')
    tkinsert(Box_Txt_four, 0, '500')
    tkdelete(Box_Txt_five, 0, 'end')
    tkinsert(Box_Txt_five, 0, '-450 1450')
  } else if(my_name == "5 1.5k-2k 70 bins"){
    tkdelete(Box_Pos_one, 0, 'end')
    tkinsert(Box_Pos_one, 0, '30.5')
    tkdelete(Box_Pos_two, 0, 'end')
    tkinsert(Box_Pos_two, 0, '0')
    tkdelete(Box_Pos_three, 0, 'end')
    tkinsert(Box_Pos_three, 0, '0')
    tkdelete(Box_Pos_four, 0, 'end')
    tkinsert(Box_Pos_four, 0, '0')
    tkdelete(Box_Pos_five, 0, 'end')
    tkinsert(Box_Pos_five, 0, '1 70')
    tkdelete(Box_Txt_one, 0, 'end')
    tkinsert(Box_Txt_one, 0, 'TSS')
    tkdelete(Box_Txt_two, 0, 'end')
    tkinsert(Box_Txt_two, 0, '')
    tkdelete(Box_Txt_three, 0, 'end')
    tkinsert(Box_Txt_three, 0, '')
    tkdelete(Box_Txt_four, 0, 'end')
    tkinsert(Box_Txt_four, 0, '')
    tkdelete(Box_Txt_five, 0, 'end')
    tkinsert(Box_Txt_five, 0, '-1450 1950')
  } else {
    tkdelete(Box_Pos_one, 0, 'end')
    tkinsert(Box_Pos_one, 0, '15.5')
    tkdelete(Box_Pos_two, 0, 'end')
    tkinsert(Box_Pos_two, 0, '45.5')
    tkdelete(Box_Pos_three, 0, 'end')
    tkinsert(Box_Pos_three, 0, '20.5')
    tkdelete(Box_Pos_four, 0, 'end')
    tkinsert(Box_Pos_four, 0, '40.5')
    tkdelete(Box_Pos_five, 0, 'end')
    tkinsert(Box_Pos_five, 0, '1 80')
    tkdelete(Box_Txt_one, 0, 'end')
    tkinsert(Box_Txt_one, 0, 'TSS')
    tkdelete(Box_Txt_two, 0, 'end')
    tkinsert(Box_Txt_two, 0, 'TTS')
    tkdelete(Box_Txt_three, 0, 'end')
    tkinsert(Box_Txt_three, 0, '500')
    tkdelete(Box_Txt_four, 0, 'end')
    tkinsert(Box_Txt_four, 0, '500')
    tkdelete(Box_Txt_five, 0, 'end')
    tkinsert(Box_Txt_five, 0, '-1450 3450')
  }
}

#reads in gene list files
GetGenelist <- function(GM, num, genelistID, genelistcount, cbgene, biglist){ #set pb?
	file_count <- as.integer(tksize(GM))
	if(file_count < 1){
		return(biglist)
	}
	if(BusyTest()){
		CloseSubWindows(c(3,4))
		STATE[5] <<- 1
		tcl("wm", "attributes", mainframe, topmost=F)
		file_name <- tclvalue(tkgetOpenFile(filetypes = "{{Gene List} {.txt}}"))
		if(!nchar(file_name)) {
			biglist[[2]] <- ""
			biglist[[1]] <- ""
			CHECK_LIST[[num]] <- 0
			tkdelete(genelistID, 0, "end")
			tkdelete(genelistcount, 0, "end")
		}else{
			l_name <- strsplit(as.character(file_name), '/')
			ld_name <- paste(l_name[[1]][(length(l_name[[1]]))])
			first_file <- read.table(file_name, header = FALSE)
			CHECK_LIST[[num]] <- 1
			GENE_LIST[[num]][[3]] <<- GENE_LIST[[1]][[3]]
			biglist[[2]] <- c(paste(strsplit(as.character(ld_name), '.txt')[[1]][1]),  paste(unique(first_file$V1)))
			biglist[[1]] <- HandleGeneList(GM, file_count, biglist[[2]][-1], GENE_LIST[[1]][[1]],	biglist[[2]][1], genelistID, genelistcount)
		}
		CHECK_LIST[[num]] <<- SetMasterCheck(file_count, CHECK_LIST[[num]], COLOR_LIST[[num]], cbgene, genelistID, TRUE)
		tcl("wm", "attributes", mainframe, topmost=TRUE)
		STATE[5] <<- 0
		return(biglist)
	}
	return(biglist)
}

#adds selected item(s) from list1 to list2 and states what list it cam from, max of 2 items
ActLst <- function(lst1, lst2, list_name, lst2C, num){
	if(BusyTest()){
		CloseSubWindows(c(3,4))
		my_list2 <- as.character(tclvalue(tkget(lst2C, 0)))
		my_list1 <- as.character(list_name)
		sel <- as.integer(tclvalue(tkcurselection(lst1)))
		if(is.na(sel)){
			return()
		}
		if(STATE[6] != num[1]){
			if(my_list2 == "                     No Active table files" & my_list1[1] != ""){
				tkdelete(lst2C, 0, "end")
				tkinsert(lst2, 0, tclvalue(tkget(lst1, sel)))
				tkinsert(lst2C, 0, my_list1)
				tkconfigure(G1_main, listvariable = tclVar(GENE_LIST[[num[1]]][[1]]))
				lst <- paste('  n = ', as.integer(tksize(G1_main)))
				tkdelete(G1C_main, 0)
				tkinsert(G1C_main, 0, lst)
				STATE[6:7] <<- num				
			}else{
				return()
			}
		}else{
			if(as.integer(tksize(lst2)) == 2){
				tkdelete(lst2, "end")
			}
			if(tclvalue(tkget(lst1, sel)) != tclvalue(tkget(lst2, 0))){
				tkinsert(lst2, 0, tclvalue(tkget(lst1, sel)))
			}
		}
	}
	return()
}

#removes selected item from list
DActLst<-function(lst1, lst1C, wnum){
	if(BusyTest()){
		CloseSubWindows(c(wnum))
		num <- (as.integer(tkcurselection(lst1)))
		if(length(num) < 1){
			num <- 0 : (as.integer(tksize(lst1)) - 1)
		}
		counter <- 0
		for(i in num){
			tkdelete(lst1,(i-counter))
			counter <- counter + 1
		}
		if(as.integer(tksize(lst1)) == 0 & as.integer(tksize(file_list)) > 0){
			tkdelete(lst1C, 0, "end")
			tkdelete(G1C_main, 0)
			tkdelete(G1_main, 0, "end")
			tkinsert(lst1C, 0, "                     No Active table files")
			GENE_LIST[[1]][[1]] <<- GENE_LIST[[1]][[2]][-1]
			if(STATE[1] == 1){
				swapFrame()	
			}
			STATE[6] <<- 0
		}
	}
}

#sorts main gene list to active and inactive lists based on selected file 
SortGene <- function(GM, GC, biglist){ # set state 5
	file_count <- as.integer(tksize(GM))
	if(STATE[6] != 0){
		if(BusyTest()){
			CloseSubWindows(c(3,4))
			if(STATE[1] == 1){
				STATE[1] <<- 0
				tkdestroy(box2g)
				makeListFrame()	
			}
			tcl("wm", "attributes", mainframe, topmost=F)
			file_name <- tclvalue(tkgetOpenFile(filetypes = "{{Gene List} {.txt}}"))
			pb <<- tkProgressBar(title = "be patient!!",  min = 0, max = 100, width = 300 )
			if (!nchar(file_name)) {
				tkdelete(GC, 0, "end")
				tkdelete(G1_main, 0, "end")
				tkconfigure(G1_main, listvariable = tclVar(as.character(biglist[[STATE[6]]][[1]])))
					cgng5 <- paste(' n  = ', as.integer(tksize(G1_main)), ' :  in ', biglist[[STATE[6]]][[2]][1])
					tkinsert(GC, 0, cgng5)
				tcl("wm", "attributes", mainframe, topmost=TRUE)
				close(pb)
				return()
			}else{
				l_name <- strsplit(as.character(file_name), '/')
				ld_name <- paste(l_name[[1]][(length(l_name[[1]]))])  
				legend_name <- paste(strsplit(as.character(ld_name), '.txt')[[1]][1])
				first_file <- read.table(file_name, header = FALSE, comment.char = "")
				list_m <- c(biglist[[STATE[6]]][[2]][-1],  paste(unique(first_file$V1)))
				list_out <- list_m[duplicated(list_m)]
				setTkProgressBar(pb, 100, label =  "sorting genes")
				if(length(list_out) > 0){
					tkdelete(GC,0,"end")
					tkconfigure(G1_main, listvariable = tclVar(list_out))
					cgng5 <- paste(' n  = ', as.integer(tksize(G1_main)), ' : intersect', legend_name)
					cgng4 <- paste(' and ', biglist[[STATE[6]]][[2]][1])
					tkinsert(GC, 0, cgng5, cgng4)
					close(pb)
				}else{
					tkmessageBox(message = "no genes left after sorting")
					tcl("wm", "attributes", mainframe, topmost=TRUE)
					close(pb)
					return()
				}
				tcl("wm", "attributes", mainframe, topmost=TRUE)
				close(pb)
				return()
			}
		}
		return()
	}
tkmessageBox(message = "You need to activate a list first")
	return()
}

#sorts active gene list contain top % signal based on selected bins and file
SortTop <- function(filelist1, filelist2, maingenelist, genelist1, genelist2, genelist3, genelistcount1, 
	genelistcount2, genelistcount3, tablefile, startbin1, startbox1, stopbin1, stopbox1, number) {
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
		enesg <- as.data.frame(matrix(myList)) 
		colnames(enesg) <- "gene"
		outlist <-NULL
		lc <- 0
		for(k in sel){  
			mch <- merge(enesg, tablefile[[k]], by="gene", sort=F)
			apply_bins <- rowSums(mch[,-1][R_start_bin1:R_stop_bin1],	na.rm = T)
			ix <- sort(apply_bins, decreasing=T, index=T)$ix
			lc <- lc+1
			outlist[[lc]] <- as.character(mch[ix,1][num[1]:num[2]])
			if(lc == 2){
			  test3 <- c(outlist[[1]], outlist[[2]])
			  outlist[[3]] <- test3[duplicated(test3)]
			}
		}
		tkdelete(genelist1, 0, 'end')
		tkdelete(genelist2, 0, 'end')
		tkdelete(genelist3, 0, 'end')
		if(length(outlist[[1]]) > 0){
			tkconfigure(genelist1, listvariable = tclVar(as.character(outlist[[1]])))
		}
		if(file_count2 == 2 ){
			if(length(outlist[[2]]) > 0){
				tkconfigure(genelist2, listvariable = tclVar(as.character(outlist[[2]])))
			}
		  if(length(outlist[[3]]) > 0){
		    tkconfigure(genelist3, listvariable = tclVar(as.character(outlist[[3]])))
		  }
		}
		tkdelete(genelistcount1, 0, 'end')
		tkdelete(genelistcount2, 0, 'end')
		tkdelete(genelistcount3, 0, 'end')
		lstcount1 <- paste('n = ', (as.integer(tksize(genelist1))))
		lstcount1a <- paste(R_num , ' in ',tclvalue(tkget(filelist2,0)))
		tkinsert(genelistcount1, "end", lstcount1, lstcount1a)
		lstcount2 <- paste('n = ', (as.integer(tksize(genelist2))))
		lstcount2a <- paste(R_num , ' in ',tclvalue(tkget(filelist2,1)))
		tkinsert(genelistcount2, "end", lstcount2, lstcount2a)
		lstcount3 <- paste('n = ', (as.integer(tksize(genelist3))))
		lstcount3a <- paste(R_num , ' in both ')
		tkinsert(genelistcount3, "end", lstcount3, lstcount3a)
		close(pb)
		STATE[5] <<- 0
		return()
	}
	return()
}	

#takes two reigns of genes and compares the signal, generates gene lists outside of fold diff and
	# makes a scatter plot
plotSOne <- function(filelist1, filelist2, maingenelist, genelist1, genelist2, genelist3, genelistcount1, 
	genelistcount2, genelistcount3, tablefile, up_bin1, up_box1, up_bin2, up_box2, number, num_box) {
	if(BusyTest()){
		CloseSubWindows(c(3,4))
		STATE[5] <<- 1
		file_count <- as.integer(tksize(filelist1))
		file_count2 <- as.integer(tksize(filelist2))
		gene_count <- as.integer(tksize(maingenelist))
		myList <- NULL
		if(file_count2 == 2){ # adjust to pull in another file or norm to self
			sel <- NULL
			my_list2 <- NULL
			for(d in 1 : file_count2){
				my_list2 <- c(my_list2, tclvalue(tkget(filelist2,(d-1))))
			}
			for(f in 1 : file_count){
			 if(my_list2[1] %in% tclvalue(tkget(filelist1,(f-1))))
				sel <- c(sel, f)
			}
			for(f in 1 : file_count){
			 if(my_list2[2] %in% tclvalue(tkget(filelist1,(f-1))))
				sel <- c(sel, f)
			}
			num_bins <- length(tablefile[[1]]) - 1 
			R_up_bin1 <- as.integer(tclvalue(up_bin1))
			R_up_bin2 <- as.integer(tclvalue(up_bin2))
			R_num <- as.numeric(tclvalue(number))
			if(is.na(R_num) | R_num < 1){
				R_num <-  2
				tkdelete(num_box, 0, 'end')
				tkinsert(num_box, "end", 2)
				}
			if(is.na(R_up_bin1) | is.na(R_up_bin2)){
				R_up_bin1 <- 0
				R_up_bin2 <- 0
			}
			if(R_up_bin1 < 1 | R_up_bin1 > R_up_bin2){
				R_up_bin1 <- 1
				tkdelete(up_box1, 0, 'end')
				tkinsert(up_box1, "end", 1)
			} 
			if(R_up_bin2 < R_up_bin1 | R_up_bin2 > num_bins){
				R_up_bin2 <- num_bins
				tkdelete(up_box2, 0, 'end')
				tkinsert(up_box2, "end", num_bins)
			}
			for(i in 1: gene_count){
				myList <- c(myList, tclvalue(tkget(maingenelist,(i-1))))
			}
			enesg <- as.data.frame(matrix(myList)) 
			colnames(enesg) <- "gene"
			##  replacing 0 with small number
			# bbb <- NULL
			# for(i in sel)){
				# bb <- tablefile[[i]][,-1] > 0
				# bbb <- min(tablefile[[i]][,-1][bb], bbb)
			# }
			tablefile[[sel[1]]][,-1] <- rowSums(tablefile[[sel[1]]][,-1][R_up_bin1:R_up_bin2], na.rm = T)
			tablefile[[sel[2]]][,-1] <- rowSums(tablefile[[sel[2]]][,-1][R_up_bin1:R_up_bin2], na.rm = T)
			tablefile[[sel[1]]][,-1] <- as.data.frame(lapply(tablefile[[sel[1]]][,-1], function(x){replace(x, x == 0, NA)}))
			tablefile[[sel[1]]] <- tablefile[[sel[1]]][complete.cases(tablefile[[sel[1]]]),]
			tablefile[[sel[2]]][,-1] <- as.data.frame(lapply(tablefile[[sel[2]]][,-1], function(x){replace(x, x == 0, NA)}))
			tablefile[[sel[2]]] <- tablefile[[sel[2]]][complete.cases(tablefile[[sel[2]]]),]
			mch <- merge(enesg, tablefile[[sel[1]]][,1:2], by="gene", sort=F)
			mch <- merge(mch, tablefile[[sel[2]]][,1:2], by="gene", sort=F)
			out <- mch[,2]
			out0 <- mch[,3]
			myout <- out/out0
			myout1 <- sort(myout, decreasing=T, index=T)$ix
			myout2 <- sort(myout, decreasing=F, index=T)$ix
			test1 <- myout[myout1] > R_num
			test2 <- myout[myout2] < 1/R_num
			test3 <- myout <= R_num & myout >= 1/R_num
			outlist1 <- mch$gene[myout1][test1]
			outlist2 <- mch$gene[myout2][test2]
			outlist3 <- mch$gene[test3]
			xlab = paste(my_list2[2])
			ylab = paste(my_list2[1])
			par(mar=c(5.1, 5.5, 4.1, 8.1))
			plot(out0,out,xlab=xlab,ylab=ylab)
			points(out0[myout1][test1],out[myout1][test1],col='blue')
			points(out0[myout2][test2],out[myout2][test2],col='blue')
			R_Header <- (as.character(tclvalue(Header)))
			R_Header_size <- as.numeric(tclvalue(Header_size))
			if(!is.na(R_Header_size)){
				title(main = R_Header, cex.main = R_Header_size)
				}
			mtext("\nRegion plot",side=3)	
			abline(0,1,col="orange")
			#abline(0,(R_num),col="blue")
			#abline(0,(1/R_num),col="blue")
			#abline(lm(out~out0),col="red")
			title(sub=paste("Bins",R_up_bin1,"to",R_up_bin2,": cut off of",R_num,sep=" "))
			pb <- tkProgressBar(title = "be patient!!",  min = 0, max = 100, width = 300 )
			tkdelete(genelist1, 0, 'end')
			tkdelete(genelist2, 0, 'end')
			tkdelete(genelist3, 0, 'end')
			tkdelete(genelistcount1, 0, 'end')
			tkdelete(genelistcount2, 0, 'end')
			tkdelete(genelistcount3, 0, 'end')
			if(length(outlist1) > 0){
				tkconfigure(genelist1, listvariable = tclVar(as.character(outlist1)))
			}
			if(length(outlist2) > 0){
				tkconfigure(genelist2, listvariable = tclVar(as.character(outlist2)))
			}
			if(length(outlist3) > 0){
			  tkconfigure(genelist3, listvariable = tclVar(as.character(outlist3)))
			}
				lstcount1 <- paste('n = ', (as.integer(tksize(genelist1))), "up ", R_num, "fold")
				lstcount1a <- paste(my_list2[1])
				tkinsert(genelistcount1, "end", lstcount1, lstcount1a)
				lstcount2 <- paste('n = ', (as.integer(tksize(genelist2))), "down ", R_num, "fold")
				lstcount2a <- paste(my_list2[2])
				tkinsert(genelistcount2, "end", lstcount2, lstcount2a)
				lstcount3 <- paste('n = ', (as.integer(tksize(genelist3))))
				lstcount3a <- paste("less then", R_num, "fold change")
				tkinsert(genelistcount3, "end", lstcount3, lstcount3a)
				close(pb)
				# mtext(c(paste("n = ",length(mch[,1])),"\n\nliner regression line", "\n\n\n\nline through 0",paste("\n\n\n\n\n\nupper cut off, n = ",as.integer(tksize(genelist1))), paste("\n\n\n\n\n\n\n\nlower cut off, n = ",as.integer(tksize(genelist2)))),las=1,side = 4, line = 1, bty = "n", cex = .8, col = c("black","red","orange","blue","blue"))
				mtext(c(paste("n = ", length(mch[,1])), "\n\n\n\nline through 0",paste("\n\n\n\n\n\nupper cut off, n = ",as.integer(tksize(genelist1))), paste("\n\n\n\n\n\n\n\nlower cut off, n = ",as.integer(tksize(genelist2)))),las=1,side = 4, line = 1, bty = "n", cex = .8, col = c("black","orange","blue","blue"))
				
				par(mar=c(5.1, 5.5, 4.1, 4.1), xpd=FALSE)
		}else{
			tkmessageBox(message = "You need to have two samples to use this tool.")
		}
		STATE[5] <<- 0
		return()
	}
	return()	
}

#takes two reigns of genes and compares the log2 signal, generates gene lists outside of fold diff and
	# makes a scatter plot
plotEI <- function(filelist1, filelist2, maingenelist, genelist1, genelist2, genelist3, genelistcount1, 
	genelistcount2, genelistcount3, tablefile, up_bin1, up_box1, up_bin2, up_box2, down_bin1, down_box1, down_bin2, 
		down_box2, number, num_box) {
	if(BusyTest()){
		CloseSubWindows(c(3,4))
		STATE[5] <<- 1
		file_count <- as.integer(tksize(filelist1))
		file_count2 <- as.integer(tksize(filelist2))
		if(file_count2 == 2){ # same and plotone
			sel <- NULL
			my_list2 <- NULL
			for(d in 1 : file_count2){
				my_list2 <- c(my_list2, tclvalue(tkget(filelist2,(d-1))))
			}
			for(f in 1 : file_count){
			 if(my_list2[1] %in% tclvalue(tkget(filelist1,(f-1))))
				sel <- c(sel, f)
			}
			for(f in 1 : file_count){
			 if(my_list2[2] %in% tclvalue(tkget(filelist1,(f-1))))
				sel <- c(sel, f)
			}
			num_bins <- length(tablefile[[1]]) - 1 
			R_up_bin1 <- as.integer(tclvalue(up_bin1))
			R_up_bin2 <- as.integer(tclvalue(up_bin2))
			R_down_bin1 <- as.integer(tclvalue(down_bin1))
			R_down_bin2 <- as.integer(tclvalue(down_bin2))
			R_num <- as.numeric(tclvalue(number))
			gene_count <- as.integer(tksize(maingenelist))
			myList <- NULL
			if(is.na(R_num)){
				R_num <-  2
				tkdelete(num_box, 0, 'end')
				tkinsert(num_box, "end", 2)
			}
			if(is.na(R_up_bin1) | is.na(R_up_bin2)){
				R_up_bin1 <- 0
				R_up_bin2 <- 0
			}
			if(is.na(R_down_bin1) | is.na(R_down_bin2)){
				R_down_bin1 <- 0
				R_down_bin2 <- 0
			}
			if(R_up_bin1 < 1 | R_up_bin1 > R_up_bin2 | R_up_bin2 > num_bins){
				R_up_bin1 <- 1
				tkdelete(up_box1, 0, 'end')
				tkinsert(up_box1, "end", 1)
				R_up_bin2 <- as.integer(num_bins/2)
				tkdelete(up_box2, 0, 'end')
				tkinsert(up_box2, "end", as.integer(num_bins/2))
			} 
			if(R_down_bin1 < 1 | R_down_bin1 > R_down_bin2 | R_down_bin2 > num_bins){
				R_down_bin1 <- as.integer(num_bins/2)
				tkdelete(down_box1, 0, 'end')
				tkinsert(down_box1, "end", as.integer(num_bins/2))
				R_down_bin2 <- num_bins
				tkdelete(down_box2, 0, 'end')
				tkinsert(down_box2, "end", num_bins)
			} 
			for(i in 1: gene_count){
				myList <- c(myList, tclvalue(tkget(maingenelist,(i-1))))
			}
			enesg <- as.data.frame(matrix(myList)) 
			colnames(enesg) <- "gene"
			tablefile[[sel[1]]][,-1] <- c(rowSums(tablefile[[sel[1]]][,-1][R_up_bin1:R_up_bin2], na.rm = T), 
				rowSums(tablefile[[sel[1]]][,-1][R_down_bin1:R_down_bin2], na.rm = T))
			tablefile[[sel[2]]][,-1] <- c(rowSums(tablefile[[sel[2]]][,-1][R_up_bin1:R_up_bin2], na.rm = T), 
				rowSums(tablefile[[sel[2]]][,-1][R_down_bin1:R_down_bin2], na.rm = T))
			# remove genes with no signal
			tablefile[[sel[1]]][,-1] <- as.data.frame(lapply(tablefile[[sel[1]]][,-1], function(x){replace(x, x == 0, NA)}))
			tablefile[[sel[1]]] <- tablefile[[sel[1]]][complete.cases(tablefile[[sel[1]]]),]
			tablefile[[sel[2]]][,-1] <- as.data.frame(lapply(tablefile[[sel[2]]][,-1], function(x){replace(x, x == 0, NA)}))
			tablefile[[sel[2]]] <- tablefile[[sel[2]]][complete.cases(tablefile[[sel[2]]]),]
			# have min signal in gene body
			tablefile[[sel[1]]] <- tablefile[[sel[1]]][tablefile[[sel[1]]][,2] >= quantile(tablefile[[sel[1]]][,2], probs = c(.01)), ]
			tablefile[[sel[2]]] <- tablefile[[sel[2]]][tablefile[[sel[2]]][,2] >= quantile(tablefile[[sel[2]]][,2], probs = c(.01)), ]
			# keep only genes in common
			mch <- merge(enesg, tablefile[[sel[1]]][,1:3], by="gene", sort=F)
			mch <- merge(mch, tablefile[[sel[2]]][,1:3], by="gene", sort=F)
			# saves table
						# file_name <- paste(paste(my_list2[1]),".txt",sep="")
						# write.table(mch[,1:3], file_name, col.names = FALSE, row.names=FALSE, quote=FALSE)
						# file_name <- paste(paste(my_list2[2]),".txt",sep="")
						# write.table(mch[,-c(2:3)], file_name, col.names = FALSE, row.names=FALSE, quote=FALSE)
			# 
			# body-promoter
			out <- log2(mch[,2])-log2(mch[,3])
			out0 <- log2(mch[,4])-log2(mch[,5])
			# compare 1st with 2ed
			myout <- out-out0
			myout1 <- sort(myout, decreasing=T, index=T)$ix
			myout2 <- sort(myout, decreasing=F, index=T)$ix
			test1 <- myout[myout1] > R_num
			test2 <- myout[myout2] < -R_num
			test3 <- myout <= R_num & myout >= -R_num
			outlist1 <- mch$gene[myout1][test1]
			outlist2 <- mch$gene[myout2][test2]
			outlist3 <- mch$gene[test3]
			xlab = paste(my_list2[2])
			ylab = paste(my_list2[1])
			par(mar=c(5.1, 5.5, 4.1, 9.1))
			plot(out0,out,xlab=xlab,ylab=ylab)
			points(out0[myout1][test1],out[myout1][test1],col='blue')
			points(out0[myout2][test2],out[myout2][test2],col='green')
			abline(0,1,col="orange")
			#abline(0,(R_num),col="blue")
			#abline(0,(1/R_num),col="green")
			#abline(lm(out~out0),col="red")
			R_Header <- (as.character(tclvalue(Header)))
			R_Header_size <- as.numeric(tclvalue(Header_size))
			if(!is.na(R_Header_size)){
				title(main = R_Header, cex.main = R_Header_size)
			}
			mtext("\nLog2 Ratio plot (3'/5')/(3'/5')",side=3)
			title(sub=paste("Bins",R_up_bin1,"-",R_up_bin2,"/ Bins",R_down_bin1,"-",R_down_bin2,": cut off of",R_num,sep=" "))
			tkdelete(genelist1, 0, 'end')
			tkdelete(genelist2, 0, 'end')
			tkdelete(genelist3, 0, 'end')
			tkdelete(genelistcount1, 0, 'end')
			tkdelete(genelistcount2, 0, 'end')
			tkdelete(genelistcount3, 0, 'end')
			pb <- tkProgressBar(title = "be patient!!",  min = 0, max = 100, width = 300 )
			if(length(outlist1)> 0){
				tkconfigure(genelist1, listvariable = tclVar(as.character(outlist1)))
			}
			if(length(outlist2)> 0){
				tkconfigure(genelist2, listvariable = tclVar(as.character(outlist2)))
			}
			if(length(outlist3) > 0){
			  tkconfigure(genelist3, listvariable = tclVar(as.character(outlist3)))
			}
			lstcount1 <- paste('n = ', (as.integer(tksize(genelist1))), "down in 3'/5' ratio")
			lstcount1a <- paste(my_list2[2])
			tkinsert(genelistcount1, "end", lstcount1, lstcount1a)
			lstcount2 <- paste('n = ', (as.integer(tksize(genelist2))), "down in 3'/5' ratio")
			lstcount2a <- paste(my_list2[1])
			tkinsert(genelistcount2, "end", lstcount2, lstcount2a)
			lstcount3 <- paste('n = ', (as.integer(tksize(genelist3))))
			lstcount3a <- paste("less then", R_num, "fold change")
			tkinsert(genelistcount3, "end", lstcount3, lstcount3a)
			close(pb)
			mtext(c(paste("n = ", length(mch[,1])), "\n\n\n\nline through 0",paste("\n\n\n\n\n\nupper cut off, n = ",as.integer(tksize(genelist1))), paste("\n\n\n\n\n\n\n\nlower cut off, n = ",as.integer(tksize(genelist2)))),las=1,side = 4, line = 1, bty = "n", cex = .8, col = c("black","orange","blue","green"))
			par(mar=c(5.1, 5.5, 4.1, 4.1), xpd=FALSE)
		}else{
			tkmessageBox(message = "You need to have two samples to use this tool.")
		}
	STATE[5] <<- 0
	return()
	}
	return()	
}

# finds 3 clusers from the one active file, plotting the 3 patterns and displaying the gene lists
FindClustersSetUpTest <- function(master,filelist1, filelist2, maingenelist, 
                                  startbin, startbox, stopbin, stopbox, startbin1, 
                                  startbox1, stopbin1, stopbox1, mymath, 
                                  minylim, maxylim, minylimbox, maxylimbox, cbylim,
                                  genelist1, genelist2, genelist3, genelistcount1,
                                  genelistcount2, genelistcount3) {
  if(BusyTest()){
    CloseSubWindows(c(3,4))
    file_count <- as.integer(tksize(filelist1))
    file_count2 <- as.integer(tksize(filelist2))
    if(file_count2 == 1){ # same and plotone
      sel <- NULL
      my_list2 <- tclvalue(tkget(filelist2,0))
      
      for(f in 1 : file_count){
        if(my_list2 %in% tclvalue(tkget(filelist1,(f-1))))
          sel <- f
      }
      
      num_bins <- ncol(master[[1]]) - 1 
      R_start_bin_cluster <- as.integer(tclvalue(startbin))
      R_stop_bin_cluster <- as.integer(tclvalue(stopbin))
      if(is.na(R_stop_bin_cluster) | is.na(R_start_bin_cluster)){
        R_start_bin_cluster <- 0
        R_stop_bin_cluster <- 0
      }			
      if(R_start_bin_cluster < 1 | R_start_bin_cluster > R_stop_bin_cluster){
        startbin_cluster <- tclVar(1)
        tkdelete(startbox, 0, 'end')
        tkinsert(startbox, "end", 1)
      } 
      if(R_stop_bin_cluster < 1 | R_stop_bin_cluster < R_start_bin_cluster | R_stop_bin_cluster > num_bins){
        stopbin_cluster <- tclVar(num_bins)
        tkdelete(stopbox, 0, 'end')
        tkinsert(stopbox, "end", num_bins)
      }
      R_start_bin <- as.integer(tclvalue(startbin1))
      R_stop_bin <- as.integer(tclvalue(stopbin1))
      if(is.na(R_stop_bin) | is.na(R_start_bin)){
        R_start_bin <- 0
        R_stop_bin <- 0
      }			
      if(R_start_bin < 1 | R_start_bin > R_stop_bin){
        startbin1 <- tclVar(1)
        tkdelete(startbox1, 0, 'end')
        tkinsert(startbox1, "end", 1)
      } 
      if(R_stop_bin < 1 | R_stop_bin < R_start_bin | R_stop_bin > num_bins){
        stopbin1 <- tclVar(num_bins)
        tkdelete(stopbox1, 0, 'end')
        tkinsert(stopbox1, "end", num_bins)
      }
      R_my_math <- as.character(tclvalue(mymath))# test noquote()
      if(R_my_math == " mean"){
        my_apply <- function(x) colMeans(x, na.rm = T)
      } else if (R_my_math == " sum"){
        my_apply <- function(x) colSums(x, na.rm = T)
      } else if (R_my_math == " median"){
        my_apply <- function(x) apply(x, 2, median, na.rm = T)
      } 
      ylim <- c(as.numeric(tclvalue(minylim)),as.numeric(tclvalue(maxylim)))
      ylimt <- c(minylim, maxylim, as.character(tclvalue(cbylim)))
      cbVal_gene_cluster <- as.character(tclvalue(cbValue_gene_cluster_relative_frequency)) 
      cbVal_gene_relative_frequency <- as.character(tclvalue(cbValue_gene_relative_frequency)) 
      myList <- NULL
      clist <- list(NULL)
      use_col <- NULL
      tmp <- NULL 
      subtmp <- list(NULL,NULL)
      mydots <- NULL
      mylines <- NULL
      mylog <- NULL
      apply_bins <- NULL
      get_color <- COLOR_LIST[[STATE[6]]]
      tkdelete(genelist1, 0, 'end')
      tkdelete(genelist2, 0, 'end')
      tkdelete(genelist3, 0, 'end')
      tkdelete(genelistcount1, 0, 'end')
      tkdelete(genelistcount2, 0, 'end')
      tkdelete(genelistcount3, 0, 'end')
      myList <-paste(tkget(maingenelist,0,"end"))
      enesg <- as.data.frame(matrix(myList)) 
      colnames(enesg) <- "gene"
      mch <- merge(enesg, master[[sel]], 
                   by="gene", sort=FALSE)
      #samp <- nrow(mch[ , -1])/10
      clusters.num <- 3
      mch2 <- mch
      mch2[is.na(mch2)] <- 0
      bbb <- min(mch2[,-1][mch2[,-1]>0])/2
      mch2[,-1] <- as.data.frame(lapply(mch2[,-1], function(x){replace(x, x == 0, bbb)}))
      #cm <- clara(mch[ ,c(1, (R_start_bin_cluster:R_stop_bin_cluster) + 1)], clusters.num, samples=samp, pamLike = TRUE, metric="manhattan")
      if(cbVal_gene_cluster == 1){
        mch2[ , -1] <- mch2[ , -1]/rowSums(mch2[ , -1], na.rm = TRUE)
      }
      cm <- hclust(dist(mch2[ ,c((R_start_bin_cluster:R_stop_bin_cluster) + 1)]), method = "ward")
      for( i in 1 : clusters.num){
        #clist[[i]] <- mch[cm$clustering == i,]
        clist[[i]] <- mch[cutree(cm, clusters.num) == i,]
        if(cbVal_gene_relative_frequency == 1){
          clist[[i]][ , -1] <- clist[[i]][ , -1]/rowSums(clist[[i]][ , -1], na.rm = TRUE)
        }
        use_col <- c(use_col, get_color[i])
        tmpnum <- paste( "n = ", nrow(clist[[i]]))  
        tmpnum <- paste(tmpnum, ": list", i, sep=" ")
        tmptot <- paste(my_list2, "list", i, sep=" : ")
        tmp <- c(tmp, tmptot)
        subtmp[[1]] <- paste(subtmp[[1]], tmpnum, sep='    ')
        my_data <- my_apply(clist[[i]][,-1])
        apply_bins <- c(apply_bins, my_data)
        num <- NULL
        setdot <- OPTIONS_LIST[[STATE[6]]][[sel]][1]
        num <- which(my_dotlist == setdot)
        if(num == 21){
          num <- 0
        }
        num2 <- NULL
        setline <- OPTIONS_LIST[[STATE[6]]][[sel]][2]
        num2 <- which(my_linelist == setline)
        if(num2 > 6){
          num2 <- 0
        }
        mydots <- c(mydots, num)
        mylines <- c(mylines, num2)
        mylog <- c(mylog, OPTIONS_LIST[[STATE[6]]][[sel]][3])
      }
      subtmp[[2]] <- R_my_math
      subtmp[[1]] <- paste("bins ", R_start_bin_cluster, ":", R_stop_bin_cluster, "  ", subtmp[[1]])
      if(nrow(clist[[1]]) > 0){
        tkconfigure(genelist1, listvariable = tclVar(as.character(clist[[1]][,1])))
      }
      if(nrow(clist[[2]]) > 0){
        tkconfigure(genelist2, listvariable = tclVar(as.character(clist[[2]][,1])))
      }
      if(nrow(clist[[3]]) > 0){
        tkconfigure(genelist3, listvariable = tclVar(as.character(clist[[3]][,1])))
      }
      
      lstcount1 <- paste('n = ', (as.integer(tksize(genelist1))), "list 1")
      lstcount1a <- paste(my_list2)
      tkinsert(genelistcount1, "end", lstcount1, lstcount1a)
      lstcount2 <- paste('n = ', (as.integer(tksize(genelist2))), "list 2")
      lstcount2a <- paste(my_list2)
      tkinsert(genelistcount2, "end", lstcount2, lstcount2a)
      lstcount3 <- paste('n = ', (as.integer(tksize(genelist3))), "list 3")
      lstcount3a <- paste(my_list2)
      tkinsert(genelistcount3, "end", lstcount3, lstcount3a)
      HandlePlotMods(clusters.num, apply_bins, num_bins, norm_bin, 
                     tmp, subtmp, use_col, startbin1, stopbin1, ylim, 
                     ylimt, minylimbox, maxylimbox, mydots, mylines, mylog)
    }else{
      tkmessageBox(message = "You can have only one active file to use this tool.")
    }
  }
}

#quits program and closes plot window
quit_Program <- function() {
	if(BusyTest()){
		CloseSubWindows(c(4:2))
		tkdestroy(mainframe)
		#tkdestroy(tt)
		dev.off()
	}else{
		return()
	}
}

#removes all files and lists 
ClearTableFile <- function(...) { 
	if(BusyTest()){
		tt <- tkmessageBox(message = "Do you want to restart?",
			icon = "question", type = "yesno", default = "yes")
		if (tclvalue(tt) == 'yes'){
		DActLst(active_list, active_lable, c(2:4)) # closes open name/color/line/dot window and sub window, and resets gene tools
		FILE_LIST <<- list(NULL) 
		my_list <- list(NULL,NULL,NULL)
		GENE_LIST <<- list(list(NULL, "Genes in common", NULL, NULL, NULL, NULL, NULL), my_list, my_list, my_list, my_list) # contains common gene list and loaded gene lists 
		CHECK_LIST <<- list(1, 0, 0, 0, 0)	
		OPTIONS_LIST <<- list(list(NULL),list(NULL),list(NULL),list(NULL),list(NULL))
		tkdelete(file_list, 0, "end")
		tkdelete(gene_main_count, 0, "end")
		tkdelete(active_list, 0, "end")
		tkdelete(active_lable, 0, "end")
		tkinsert(active_lable, 0, "                     No Active table files")
		STATE[-1] <<- c(0, 0, 0, 0, 0, 0) 
		tkdelete(G1_main, 0, "end")
		tkdelete(G1C_main, 0, "end")
		UpdateFileLists(as.integer(tksize(file_list)), NULL, file_list, gene_list0,	gene_list1, gene_list2, gene_list3, 
			gene_main_count, gene_list0_count,	gene_list1_count, gene_list2_count, gene_list3_count, c(0:4), TRUE, 0)
		if(STATE[1] == 1){
			swapFrame()	
		}
		dev.off()
		}else{
		return()
		}
		}else{
		return()
		}
	}

#change color, name, and activation of file(s), along with other plot options 
handleColorSel<-function(lst, list_num){#add ok plot cancel button
	if(BusyTest()){
		CloseSubWindows(c(3,4))
		SampleOptionsFrame <<-tktoplevel()
		STATE[3] <<- 1
		tkbind(SampleOptionsFrame, "<Destroy>", function() STATE[3] <<- 0)
		OnOK <- function(){
			if(nchar(tclvalue(Name))>0){
				tkdelete(lst,sel) 
				tkinsert(lst, sel, tclvalue(Name))
				CHECK_LIST[[list_num]][sel+2] <<- tclvalue(cbValue_active)
				mydot <- paste(as.character(tkget(mylist1,0)), collapse = " ")
				myline <- paste(as.character(tkget(mylist2,0)), collapse = " ")
				OPTIONS_LIST[[list_num]][[sel+1]][1] <<- mydot
				OPTIONS_LIST[[list_num]][[sel+1]][2] <<- myline
				if (CHECK_LIST[[list_num]][sel+2] == "0"){ 
					tkitemconfigure(lst,sel,foreground="gray")
				}else{
					tkitemconfigure(lst,sel,foreground=COLOR_LIST[[list_num]][sel+1])
				}
				if(GENE_LIST[[list_num]][[3]][sel+1] != paste(tclvalue(Name))){
					GENE_LIST[[list_num]][[3]][sel+1] <<- paste(tclvalue(Name))
					DActLst(active_list, active_lable, 5)
				}
			}
			STATE[3] <<- 0 # to keep window open make state 0 before plotting
			Setup_Test(FILE_LIST, GENE_LIST, c(1:5), CHECK_LIST, file_list, gene_list0, gene_list1, gene_list2, 
				gene_list3, gene_main_count, gene_list0_count, gene_list1_count, gene_list2_count, 
					gene_list3_count, start_bin, start_box, stop_bin, stop_box, min_ylim, max_ylim, min_ylim_box, 
						max_ylim_box, cbValue_ylim, my_math2)
			tcl("wm", "attributes", mainframe, topmost=T)
			tkdestroy(SampleOptionsFrame)
			}
			remove_file <- function(...){ # need to pull out to own function
				if(as.integer(tksize(lst)) > 1){ # if last file restart
					FILE_LIST <<- FILE_LIST[-(sel+1)]
					tkdelete(lst,sel)
					tkdelete(gene_list0,sel)
					tkdelete(gene_list1,sel)
					tkdelete(gene_list2,sel)
					tkdelete(gene_list3,sel)
					gene_names <- FILE_LIST[[1]][,1]
					for(i in 1: length(FILE_LIST)){ # fix so it does not compare to itself
						gene_names <- c(unique(FILE_LIST[[i]][,1]), gene_names)
						gene_names <- gene_names[duplicated(gene_names)]
					}
					
					GENE_LIST[[1]][[2]] <<- c(GENE_LIST[[1]][[2]][1], gene_names)
					GENE_LIST[[1]][[3]] <<- GENE_LIST[[1]][[3]][-(sel+1)]
					GENE_LIST[[1]][[4]] <<- GENE_LIST[[1]][[4]][-(sel+1)]
					GENE_LIST[[1]][[5]] <<- GENE_LIST[[1]][[5]][-(sel+1)]
					GENE_LIST[[1]][[6]] <<- GENE_LIST[[1]][[6]][-(((sel*2)+1):((sel*2)+2))]
					GENE_LIST[[1]][[7]] <<- GENE_LIST[[1]][[7]][-(((sel*6)+1):((sel*6)+6))]
					COLOR_LIST[[1]] <<- c(COLOR_LIST[[1]][-(sel+1)], COLOR_LIST[[1]][(sel+1)])
					COLOR_LIST[[2]] <<- c(COLOR_LIST[[2]][-(sel+1)], COLOR_LIST[[2]][(sel+1)])
					COLOR_LIST[[3]] <<- c(COLOR_LIST[[3]][-(sel+1)], COLOR_LIST[[3]][(sel+1)])
					COLOR_LIST[[4]] <<- c(COLOR_LIST[[4]][-(sel+1)], COLOR_LIST[[4]][(sel+1)])
					COLOR_LIST[[5]] <<- c(COLOR_LIST[[5]][-(sel+1)], COLOR_LIST[[5]][(sel+1)])
					CHECK_LIST[[1]] <<- CHECK_LIST[[1]][-(sel+1)]
					CHECK_LIST[[2]] <<- CHECK_LIST[[2]][-(sel+1)]
					CHECK_LIST[[3]] <<- CHECK_LIST[[3]][-(sel+1)]
					CHECK_LIST[[4]] <<- CHECK_LIST[[4]][-(sel+1)]
					CHECK_LIST[[5]] <<- CHECK_LIST[[5]][-(sel+1)]
					OPTIONS_LIST[[1]] <<- OPTIONS_LIST[[1]][-(sel+1)]
					UpdateFileLists(as.integer(tksize(lst)), gene_names, lst, gene_list0,	gene_list1, gene_list2, gene_list3, 
						gene_main_count, gene_list0_count,	gene_list1_count, gene_list2_count, gene_list3_count, c(0:4), FALSE, 0) 
					DActLst(active_list, active_lable, c(3,4))
					tcl("wm", "attributes", mainframe, topmost=T)
				}
				return() #change to call restart
			}
			OnCOL <- function(...) {
				tmp<-tclvalue(tcl("tk_chooseColor",title="Choose color"))
				if(nchar(tmp)>0){
					COLOR_LIST[[list_num]][sel+1] <<- tmp
				}
			}
			len <- as.integer(tksize(lst))
			tkpack(tklabel(SampleOptionsFrame,text="Set new name and color"))
			COL.but <- tkbutton(SampleOptionsFrame,text="Set Color",command= function() OnCOL())
			tkpack(COL.but, pady = c(10,10))
			sel <-as.integer(tkcurselection(lst))
			Name <- tclVar(tkget(lst,sel))
			mydot <- OPTIONS_LIST[[list_num]][[sel+1]][1]
			myline <- OPTIONS_LIST[[list_num]][[sel+1]][2]
			mysample <- "select sample for normalization"
			mylist <- (tklistbox(SampleOptionsFrame, height = 1,width = 50, selectmode="single", 
				relief='solid', borderwidth = 5, background = "white"))
			tkinsert(mylist, 0, mysample)
			OK.but <- tkbutton(SampleOptionsFrame,text="   OK   ",command=OnOK)
			RM_button <- tkbutton(SampleOptionsFrame,text="  Remove file  ",command=remove_file)
			tkpack(tkentry(SampleOptionsFrame, width=50, textvariable=Name), pady = c(10, 10), padx = c(10, 10))
			mylist1 <- (tklistbox(SampleOptionsFrame, height = 1,width = 50, selectmode="single", 
				relief='solid', borderwidth = 5, background = "white"))
			tkinsert(mylist1, 0, mydot)
			tkpack(mylist1, pady = c(10, 10), padx = c(10, 10))
			mylist2 <- (tklistbox(SampleOptionsFrame, height = 1,width = 50, selectmode="single", 
				relief='solid', borderwidth = 5, background = "white"))
			tkinsert(mylist2, 0, myline)
			tkpack(mylist2, pady = c(10, 10), padx = c(10, 10))
			tkpack(OK.but)
			if(list_num ==1){
				tkpack(RM_button)
				tkpack(mylist, pady = c(10, 10), padx = c(10, 10))
			}
			mydot <- tkbind(mylist1,"<Double-ButtonPress-1>",function() HandleDotPlot(mylist1))
			myline <- tkbind(mylist2,"<Double-ButtonPress-1>",function() HandleLinePlot(mylist2))
			mysample <- tkbind(mylist,"<Double-ButtonPress-1>",function() HandleSamplePlot(mylist, list_num, sel, lst))
			cbValue_active <- tclVar(CHECK_LIST[[list_num]][sel+2])
			cb_active <- tkcheckbutton(SampleOptionsFrame)
			tkconfigure(cb_active, variable = cbValue_active)
			tkpack(tklabel(SampleOptionsFrame, text = '                                           Plot?'), 
				cb_active, side = 'left', padx = c(5, 0), pady = c(0, 0))
			tcl("wm", "attributes", mainframe, topmost=F)
	}else{
		tkmessageBox(message = "You need to close 'option' window(s) first.")
		return()
	}
}

#handles dots for plotting
HandleDotPlot <- function(lst) { 
	#opens frame with select what type of dot to use
		if(BusyTest()){
			CloseSubWindows(4)
			SubOptionsFrame <<- tktoplevel()
			STATE[4] <<- 1
			tkbind(SubOptionsFrame, "<Destroy>", function() STATE[4] <<- 0)
			OnOK <- function(){
				j<-as.integer(tkcurselection(mydotlist))
				Name <- tclVar(tkget(mydotlist,j))
				if(nchar(tclvalue(Name))>0){
					tkdelete(lst,i) 
					tkinsert(lst, i, tclvalue(Name))
				}
				STATE[4] <<- 0
				tkdestroy(SubOptionsFrame)
				}
			tkpack(tklabel(SubOptionsFrame,text="Set icon to plot"))
			i<-as.integer(tkcurselection(lst))
			OK_but <- tkbutton(SubOptionsFrame,text="   OK   ",command=OnOK)
			tkpack(OK_but, side = "top")
			yscr <- tkscrollbar(SubOptionsFrame, repeatinterval = 5, command = function(...)
				tkyview(mydotlist, ...))
			mydotlist <- (tklistbox(SubOptionsFrame, height = 10,width = 50, listvariable = tclVar(my_dotlist), selectmode="single", 
				background = "white", yscrollcommand = function(...) tkset(yscr, ...)))
			tkpack(mydotlist, pady = c(10, 10), padx = c(10, 10), side = "left")
			tkselection.set(mydotlist,0)
			tkpack(yscr, fill = 'y', side = "left")
			tcl("wm", "attributes", SubOptionsFrame, topmost=T)
		}else{
			return()
		}
	}

#handles lines for plotting
HandleLinePlot <<- function(lst) { 
	#opens frame with select what type of dot to use
		if(BusyTest()){
			CloseSubWindows(4)
			SubOptionsFrame <<- tktoplevel()
				STATE[4] <<- 1
				tkbind(SubOptionsFrame, "<Destroy>", function() STATE[4] <<- 0)
				 OnOK <- function(){
					j<-as.integer(tkcurselection(mylinelist))
					Name <- tclVar(tkget(mylinelist,j))
					if(nchar(tclvalue(Name))>0){
					tkdelete(lst,i) 
				tkinsert(lst, i, tclvalue(Name))
				}
				STATE[4] <<- 0
				tkdestroy(SubOptionsFrame)
				}
			tkpack(tklabel(SubOptionsFrame,text="Set line type to plot"))
			i<-as.integer(tkcurselection(lst))
			OK_but <- tkbutton(SubOptionsFrame,text="   OK   ",command=OnOK)
			tkpack(OK_but, side = "top")
			yscr <- tkscrollbar(SubOptionsFrame, repeatinterval = 5, command = function(...)
				tkyview(mylinelist, ...))
			mylinelist <- (tklistbox(SubOptionsFrame, height = 10,width = 50, listvariable = tclVar(my_linelist), selectmode="single", 
				background = "white", yscrollcommand = function(...) tkset(yscr, ...)))
			tkpack(mylinelist, pady = c(10, 10), padx = c(10, 10), side = "left")
			tkselection.set(mylinelist,0)
			tkpack(yscr, fill = 'y', side = "left")
			tcl("wm", "attributes", SubOptionsFrame, topmost=T)
		}else{
			return()
		}
	}

# handles sample norm for plotting
HandleSamplePlot <- function(lst1, lmod, sel, lst) { 
	#opens frame with select what sample to normalize to to use
	if(BusyTest()){
		CloseSubWindows(4)
		if(as.integer(tksize(file_list)) > 20){ 
			tkmessageBox(message = "I have too many files, you need to reset me or remove some files first")
			return()
		}
	  if(STATE[1] == 1){
	    swapFrame()	
	  }
			SubOptionsFrame <<-tktoplevel()
			STATE[4] <<- 1	
			tkbind(SubOptionsFrame, "<Destroy>", function() STATE[4] <<- 0)
			OnOK <- function(){
				j<-as.integer(tkcurselection(mysamplelist))
				Name <- tclVar(tkget(mysamplelist,j))
				if(nchar(tclvalue(Name))>0){
					tkdelete(lst1,0) 
					tkinsert(lst1, 0, tclvalue(Name))
					if(j>0){
					  R_start_bin <- as.integer(tclvalue(start_bin_norm))
					  R_stop_bin <- as.integer(tclvalue(stop_bin_norm))
					  if(is.na(R_stop_bin) | is.na(R_start_bin)){
					    tkmessageBox(message = "please use bins that will work with this data")
					  }			
					  if(R_start_bin < 1 | R_start_bin > R_stop_bin){
					    tkmessageBox(message = "please use bins that will work with this data")
					  } 
					  if(R_stop_bin < 1 | R_stop_bin < R_start_bin | R_stop_bin > (ncol(FILE_LIST[[1]]))){
					    tkmessageBox(message = "please use bins that will work with this data")
					  }
						mch <- merge(FILE_LIST[[(sel+1)]], FILE_LIST[[j]], by="gene")
						mch[is.na(mch)] <- 0
						bbb <- min(mch[,-1][mch[,-1]>0])/2
						mch[,-1] <- as.data.frame(lapply(mch[,-1], function(x){replace(x, x == 0, bbb)}))
						len <- (length(mch[,-1])/2)
						if(tclvalue(cbValue_gene_norm) == "0"){
						  FILE_LIST[[as.integer(tksize(lst))+1]] <<- data.frame(mch[, 1], 
						                                    mch[, 2 : (len + 1)] / mch[, (len + 2) : ((len * 2) + 1)])
						  newname <- paste(as.character(tclvalue(tkget(lst,sel))),"/",as.character(tclvalue(Name)))
						} else {
						  FILE_LIST[[as.integer(tksize(lst))+1]] <<- data.frame(mch[,1], 
						      mch[, 2 : (len + 1)] / rowSums(mch[, (len + 2) : ((len * 2) + 1)][R_start_bin : R_stop_bin]))
						  newname <- paste(as.character(tclvalue(tkget(lst,sel))),"/sum(",as.character(tclvalue(Name)),")bins ", 
						                   R_start_bin, ":", R_stop_bin)
						}
						tkinsert(lst,"end", newname)
						GENE_LIST[[1]][[3]] <<- c(GENE_LIST[[1]][[3]], newname)
						GENE_LIST[[1]][[4]] <<- c(GENE_LIST[[1]][[4]], newname) 
						GENE_LIST[[1]][[5]] <<- c(GENE_LIST[[1]][[5]], length(GENE_LIST[[1]][[1]])) 
						GENE_LIST[[1]][[6]] <<- c(GENE_LIST[[1]][[6]], 0,0) 
						GENE_LIST[[1]][[7]] <<- c(GENE_LIST[[1]][[7]], 0,0,0,0,0,0) 
						gene_names <- c(GENE_LIST[[1]][[2]][-1], mch[,1])
						gene_names <- gene_names[duplicated(gene_names)]
						names(FILE_LIST[[as.integer(tksize(lst))]])[1] <<- paste("gene")
						UpdateFileLists(as.integer(tksize(lst)), gene_names, lst, gene_list0,	gene_list1, gene_list2, gene_list3, 
					gene_main_count, gene_list0_count,	gene_list1_count, gene_list2_count, gene_list3_count, c(0:4), TRUE, 0)
					}
				}
				CloseSubWindows(c(4,3))
			}
			tkgrid(tklabel(SubOptionsFrame, text = "Sample to normalize to"), columnspan = 6, row = 0)
			tkgrid(OK_but <- tkbutton(SubOptionsFrame,text="   OK   ",command = OnOK), columnspan = 6, row = 1)
			tkgrid(tkcheckbutton(SubOptionsFrame, variable = cbValue_gene_norm, text = "norm to gene sum"),
			       column = 0, row = 2)
			
			tkgrid(tklabel(SubOptionsFrame, text = ' Bins to sum'), columnspan = 3, column = 1, row = 2)
			tkgrid(tk2entry(SubOptionsFrame, width = 2, textvariable = start_bin_norm), columnspan = 3, column = 2, row = 2)
			tkgrid(tklabel(SubOptionsFrame, text = '- '), columnspan = 2, column = 3, row = 2, padx = c(0, 0))			
			tkgrid(tk2entry(SubOptionsFrame, width = 2, textvariable = stop_bin_norm), column = 4, row = 2)
			tkgrid(mysamplelist <- tk2listbox(SubOptionsFrame, height = 10, width = 50, selectmode="single", 
				background = "white"), columnspan = 6, row = 3,pady = c(15, 15), padx = c(15, 15))
			tkinsert(mysamplelist, 0, "select sample for normalization")
			for(i in 1 : as.integer(tksize(lst))){
					tkinsert(mysamplelist, i, tclvalue(tkget(lst,(i-1))))
			}
			tkselection.set(mysamplelist,0)
			tcl("wm", "attributes", SubOptionsFrame, topmost=T)
	}else{
		return()
	}
}

# gathers all needed info and checks if it has everything needed to plot  
Setup_Test <- function(master, gene_list, list_count, check_list, GM, G1, G2, G3, G4, GMC, G1C, G2C, G3C, G4C, startbin, startbox, 
	stopbin, stopbox, minylim, maxylim, minylimbox, maxylimbox, cbylim, mymath) { 
		if(BusyTest()){   
			CloseSubWindows(c(3,4))
			files <- as.integer(tksize(GM))
			num_bins <- length(master[[1]]) - 1
			if ( !files > 0){
				return()
			} else {
				R_start_bin <- as.integer(tclvalue(startbin))
				R_stop_bin <- as.integer(tclvalue(stopbin))
				if(is.na(R_stop_bin) | is.na(R_start_bin)){
					R_start_bin <- 0
					R_stop_bin <- 0
				}			
				if(R_start_bin < 1 | R_start_bin > R_stop_bin){
					startbin <- tclVar(1)
					tkdelete(startbox, 0, 'end')
					tkinsert(startbox, "end", 1)
				} 
				if(R_stop_bin < 1 | R_stop_bin < R_start_bin | R_stop_bin > num_bins){
					stopbin <- tclVar(num_bins)
					tkdelete(stopbox, 0, 'end')
					tkinsert(stopbox, "end", num_bins)
				}
				R_my_math <- as.character(tclvalue(mymath))# test noquote()
				if(R_my_math == " mean"){
					my_apply <- function(x) colMeans(x, na.rm = T)
				} else if (R_my_math == " sum"){
					my_apply <- function(x) colSums(x, na.rm = T)
				} else if (R_my_math == " median"){
					my_apply <- function(x) apply(x, 2, median, na.rm = T)
				} 
				ylim <- c(as.numeric(tclvalue(minylim)),as.numeric(tclvalue(maxylim)))
				ylimt <- c(minylim, maxylim, as.character(tclvalue(cbylim))) 
		apply_bins <- NULL 
		apply_count <- 0 
		use_col <- NULL
		tmp <- NULL 
		subtmp <- list(NULL,NULL)
		mydots <- NULL
		mylines <- NULL
		mylog <- NULL
		file_count <- as.integer(tksize(GM))
		for(i in list_count){
			get_color <- COLOR_LIST[[i]]
			if("1" %in% check_list[[i]]){
				if(i == 1){#need and?
					tmpnum <- paste(tkget(GMC, 0,0))
					tmpnum <- paste(tmpnum, tkget(GMC, 1,1), sep=" ")
					tmptot <- paste(tkget(GM, 0, "end"))
					subtmp[[1]] <- paste(subtmp[[1]], tmpnum, sep='  ')
				}else{
					tmptot <- paste(gene_list[[i]][[3]], gene_list[[i]][[2]][1], sep="-")
					tmpnum <- paste("n = ", length(gene_list[[i]][[1]]), " : ", gene_list[[i]][[2]][1])
					subtmp[[1]] <- paste(subtmp[[1]], tmpnum, sep='  ')
				}
				subtmp[[2]] <- as.character(tclvalue(my_math2))
				enesg <- as.data.frame(matrix(gene_list[[i]][[1]])) 
				colnames(enesg) <- "gene"
				cbVal_gene_relative_frequency <- as.character(tclvalue(cbValue_gene_relative_frequency))
				for(j in 1 : file_count){
					if(check_list[[i]][j+1] == 1){
						use_col <- c(use_col, get_color[j])
						mch <- merge(enesg, master[[j]], by="gene", sort=FALSE)
						if(cbVal_gene_relative_frequency == 1){
						  mch[ , -1] <- mch[ , -1]/rowSums(mch[ , -1], na.rm = TRUE)
						}
						my_data <- my_apply(mch[,-1])
						if(as.character(tclvalue(cbValue_log2)) == 1){
						  mylog <- c(mylog, 1)
						  mch[is.na(mch)] <- 0
						  bbb <- min(mch[,-1][mch[,-1]>0])/2
						  my_data <- (sapply(my_data, function(x){replace(x, x == 0, bbb)}))
						  apply_bins <- c(apply_bins, log2(my_data))
						}  else {
						  mylog <- c(mylog, 0)
						  apply_bins <- c(apply_bins, my_data)
						}
						
						num <- NULL
						setdot <- OPTIONS_LIST[[i]][[j]][1]
						num <- which(my_dotlist == setdot)
						if(num == 21){
							num <- 0
						}
						num2 <- NULL
						setline <- OPTIONS_LIST[[i]][[j]][2]
						num2 <- which(my_linelist == setline)
						if(num2 > 6){
							num2 <- 0
						}
						mydots <- c(mydots, num)
						mylines <- c(mylines, num2)
						tmp <- c(tmp, tmptot[j])
						apply_count <- apply_count + 1
					}
				}
			}
		}
		if(apply_count == 0){
			return()
		}
		HandlePlotMods(apply_count, apply_bins, num_bins, norm_bin, 
			tmp, subtmp, use_col, startbin, stopbin, ylim, ylimt, minylimbox, 
			maxylimbox, mydots, mylines, mylog)
	}
		}else{
		  return()
		}
}

# handle what is going to be plotted and set ylim
HandlePlotMods <- function(apply_count, apply_bins, num_bins, nbin,  
	tmp, subtmp, Colors, startbin, stopbin, ylim, ylimt, minylimbox, maxylimbox, mydots, mylines, mylog){ 
		R_start_bin <- as.integer(tclvalue(startbin))
		R_stop_bin <- as.integer(tclvalue(stopbin))
		cbVal_relative_frequency <- as.character(tclvalue(cbValue_relative_frequency))
		cbVal_gene_relative_frequency <- as.character(tclvalue(cbValue_gene_relative_frequency))
		cbVal_log2 <- as.character(tclvalue(cbValue_log2))
		R_norm_bin <- as.numeric(tclvalue(nbin))
		normz <- 1
		ylim_set <- NULL
		apply_plot <- NULL
		apply_sample <- NULL
		use_bins <- R_stop_bin - R_start_bin + 1
		my_mult <- 1.2
		if(cbVal_relative_frequency == 1){
			subtmp[[2]] <- paste(subtmp[[2]], "Relative Frequency", sep=" - ")
		}
		if(cbVal_gene_relative_frequency == 1){
		  subtmp[[2]] <- paste(subtmp[[2]], "Gene RF", sep=" - ")
		}
		if(cbVal_log2 == 1){
		  subtmp[[2]] <- paste(subtmp[[2]], "Log2", sep=" - ")
		  my_mult <- 0.8
		}
		if(!is.na(R_norm_bin) & R_norm_bin > 0 & R_norm_bin <= num_bins){
			subtmp[[2]] <- paste(subtmp[[2]], " - Normalized to bin ", R_norm_bin)
		}
		for(i in 1:apply_count){ 
			if(cbVal_relative_frequency == 1){
				normz <- sum(abs(apply_bins[(num_bins*(i-1)+1):(num_bins*i)]))
			}
			if(!is.na(R_norm_bin) & R_norm_bin > 0 & R_norm_bin <= num_bins){
				normz <-(apply_bins[((num_bins * (i-1)) + floor(R_norm_bin))] + apply_bins[((num_bins * (i-1)) + ceiling(R_norm_bin))])/2
			}
			if(normz == 0){
			  normz <- 1
			}
			temp <- (apply_bins[(num_bins*(i-1)+1):(num_bins*i)][R_start_bin:R_stop_bin]) / normz
			apply_plot <- c(apply_plot, temp)
		}
		if(length(apply_plot) < 1){
			print('oops')
			return()
		}
		if(ylimt[3] == 0 | is.na(ylim[1]) | is.na(ylim[2])){
			ymin <- round(min(apply_plot),digits=5) 
			ymax <- round(max(apply_plot)*my_mult, digits=5)
			ylim_set  <- c(ymin,ymax)
			ylimt[1] <- tclVar(ymin)
			ylimt[2] <- tclVar(ymax)
			if(STATE[2] == 1){
				tkdelete(minylimbox, 0, 'end')
				tkinsert(minylimbox, "end", ymin)
				tkdelete(maxylimbox, 0, 'end')
				tkinsert(maxylimbox, "end", ymax)
			}
		} else {
			ylim_set  <- ylim
		}
		LineDotPlot(apply_plot, use_bins, ylim_set, Colors, tmp, subtmp, apply_count, R_start_bin, R_stop_bin, mydots, mylines)
	}

# line and dot plot function
LineDotPlot <- function(apply_plot, use_bins, ylim, Colors, use_legend, subtmp, pcount, 
	R_start_bin, R_stop_bin, mydots, mylines) {
		par(mar=c(5.1, 5.5, 4.1, 4.1), xpd=FALSE)  
		L_loc <- as.character(tclvalue(Legend_loc2))
		if(L_loc == "outside"){
		  par(mar=c(5.1, 5.5, 4.1, 4.1)) # par(mar=c(5.1, 5.5, (4.1 + pcount), 4.1))
		}
		plot(1, type="n", axes=F, ylim=ylim, xlim=c(1, use_bins), ann=FALSE)
		### plot points 
		for(i in 1:pcount){ 
		  my_score <- apply_plot[(use_bins*(i-1)+1):(use_bins*i)]
		  # change to type="l" for not dots
			points(my_score, type="o",col=Colors[i] ,pch=mydots[i], 
				cex = 1.2, lwd=5, lty=mylines[i])
			if(mylines[i]==0){
        
			  #GENE_LIST[[1]][[8]] <<- c(GENE_LIST[[1]][[8]], apply_plot[(use_bins*(i-1)+1):(use_bins*i)])
			  segments(c(1:use_bins)[-tail(use_bins,1)], my_score[c(1:use_bins)[-tail(use_bins,1)]], c(2:(use_bins/2), use_bins/2, ((use_bins/2)+2):use_bins), my_score[c(2:(use_bins/2), use_bins/2, ((use_bins/2)+2):use_bins)], col=Colors[i], lwd=3)
			  #print(c(2:(use_bins/2), use_bins/2, ((use_bins/2)+2):use_bins))

			}
		}
    
		cex <- as.numeric(tclvalue(Legend_size))
		title(sub=subtmp[[1]], cex.sub = cex)
		mtext(subtmp[[2]], side = 2, line = 4.5)
		R_Header <- as.character(tclvalue(Header))
		R_Header_size <- as.numeric(tclvalue(Header_size))
		if(!is.na(R_Header_size)){
			title(main = R_Header, cex.main = R_Header_size)
		}
		R_ylable <- as.character(tclvalue(ylable))
		R_ylable_size <- as.numeric(tclvalue(ylable_size))
		if(!is.na(R_ylable_size)){  
			title(ylab = R_ylable, cex.lab = R_ylable_size, line = 2.5) ## line = 2.5
		}
		R_xlable <- as.character(tclvalue(xlable))
		R_xlable_size <- as.numeric(tclvalue(xlable_size))
		if(!is.na(R_xlable_size)){
			title(xlab = R_xlable, cex.lab = R_xlable_size)  ## remove line?
		}
		axis(2, lwd.ticks = 2)
		box()
		cex <- as.numeric(tclvalue(Legend_size))
		if(L_loc == "outside"){
			par(xpd=TRUE)# works on outside location
		  legend(.5,(ylim[2] + ylim[2]*.05+(ylim[2]*.05*pcount)), use_legend, bty = "n", cex = cex, pch = mydots, col = Colors, text.col = Colors) 
		} else {
			legend(L_loc, use_legend, bty = "n", cex = cex, pch = mydots, col = Colors, text.col = Colors) 
		}
		par(mar=c(5.1, 5.5, 4.1, 4.1), xpd=FALSE)
		PlotFormat(R_start_bin, R_stop_bin)
	}
	 
# plot finishing touches
PlotFormat <- function(R_start_bin, R_stop_bin){
		start_bin_norm <- R_start_bin - 1
		mystop <- R_stop_bin + .2
		R_Txt_one <- as.character(tclvalue(Txt_one))
		R_Pos_one <- as.numeric(tclvalue(Pos_one))
		if (R_Pos_one > 0 & !is.na(R_Pos_one) & R_Pos_one < mystop) {
		axis(1, lab = F, at = (-start_bin_norm + R_Pos_one), lwd.ticks = 5, col = "green")
		mtext(R_Txt_one, side = 1, at = (-start_bin_norm + R_Pos_one), line = 1.2, cex = 1.5, col = "green")
			abline(col="green", v=(-start_bin_norm + R_Pos_one), lwd = 5, lty = 3)
		}
		R_Txt_two <- as.character(tclvalue(Txt_two))
		R_Pos_two <- as.numeric(tclvalue(Pos_two))
		if (R_Pos_two > 0 & !is.na(R_Pos_two) & R_Pos_two < mystop) {
		axis(1, lab = F, at = (-start_bin_norm + R_Pos_two), lwd.ticks = 5, col = "red")
		mtext(R_Txt_two, side = 1, at = (-start_bin_norm + R_Pos_two), line = 1.2, cex = 1.5,col = "red")
				abline(col="red", v=(-start_bin_norm + R_Pos_two), lwd = 5, lty = 3)
		}
		R_Txt_three <- as.character(tclvalue(Txt_three))
		R_Pos_three <- as.numeric(tclvalue(Pos_three))
		if (R_Pos_three > 0 & !is.na(R_Pos_three)& R_Pos_three < mystop) {
		axis(1, lab = F, at = (-start_bin_norm + R_Pos_three), lwd.ticks = 5, col = "black")
		mtext(R_Txt_three, side = 1, cex = 1.5, at = (-start_bin_norm + R_Pos_three), 
			line = 1.2, col = "black")
				abline(col="black", v=(-start_bin_norm + R_Pos_three), lwd = 5, lty = 3)
		}
		R_Txt_four <- as.character(tclvalue(Txt_four))
		R_Pos_four <- as.numeric(tclvalue(Pos_four))
		if (R_Pos_four > 0 & !is.na(R_Pos_four)& R_Pos_four < mystop) {
		axis(1, lab = F, at = (-start_bin_norm + R_Pos_four), lwd.ticks = 5, col = "black")
		mtext(R_Txt_four, side=1, at = (-start_bin_norm + R_Pos_four), line = 1.2, cex = 1.5, col = "black")
				abline(col="black", v=(-start_bin_norm + R_Pos_four), lwd = 5, lty = 3)
		}
		use.pos.plot.ticks <- Destring(unlist(strsplit(tclvalue(Pos_five), " ")))
		use.pos.plot.ticks[is.na(use.pos.plot.ticks)] <- 0
		use.label.plot.ticks <- unlist(strsplit(tclvalue(Txt_five), " "))
		use.label.plot.ticks <- use.label.plot.ticks[use.pos.plot.ticks > start_bin_norm]
		use.pos.plot.ticks <- use.pos.plot.ticks[use.pos.plot.ticks > start_bin_norm]
		if (length(use.label.plot.ticks) != length(use.pos.plot.ticks)) {
		  tkmessageBox(message = "The number of Positions and labels are unequal")
		  return ()
		}
		
		axis(1, lab = F, at = (-start_bin_norm + use.pos.plot.ticks), lwd.ticks = 5, col = "black")
		mtext(use.label.plot.ticks, side=1, at = (-start_bin_norm + use.pos.plot.ticks), line = 1.2, cex = 1.5, col = "black")
	}

# sets up Master list to plot from new list	
handleGenePlot <- function(MGL, MGLC){ # add state5
	if(as.integer(tksize(MGL)) > 0){
		gene_list <- list(list(),list(),list(),list(),list())
		check_list <- list(list(),list(),list(),list(),list())
		myList <- paste((tkget(MGL,0,"end")))
		gene_list[[STATE[6]]] <- list(myList,GENE_LIST[[STATE[6]]][[2]][1],GENE_LIST[[STATE[6]]][[3]])
		check_list[[STATE[6]]] <- CHECK_LIST[[STATE[6]]]
			Setup_Test(FILE_LIST, gene_list, STATE[6], check_list, file_list, file_list, file_list, file_list, 
						file_list, MGLC, MGLC, MGLC, MGLC, MGLC, start_bin, start_box, stop_bin, stop_box, min_ylim, max_ylim, min_ylim_box, 
								max_ylim_box, cbValue_ylim, my_math2)
			}
		return()
		}
		
# sets up Master list from selected genes to plot from new list		
handleGenePlot2 <- function(MGL, MGLC){ #add state5
			GL0 <- tklistbox(mainframe, height = 4, width = 35, selectmode = "single",  background = "white")
			num <- (as.integer(tkcurselection(MGL)))
			gene_list <- list(list(),list(),list(),list(),list())
			check_list <- list(list(),list(),list(),list(),list())
			if(as.integer(tksize(MGL)) > 0 & length(num) > 0){
		myList <- NULL
		for(i in num){
			myList <- c(myList, tclvalue(tkget(MGL,(i))))
		}
		gene_list[[STATE[6]]] <- list(myList,GENE_LIST[[STATE[6]]][[2]][1],GENE_LIST[[STATE[6]]][[3]])
		check_list[[STATE[6]]] <- CHECK_LIST[[STATE[6]]]
		bb <- NULL
		for(i in 1:length(myList)){
					bb[i] <- (strsplit(as.character(myList[i]), ';')[[1]][2])
				} 
		tkinsert(GL0,0,paste("n = ",length(myList)," "),bb)
		Setup_Test(FILE_LIST, gene_list, STATE[6], check_list, file_list, file_list, file_list, file_list, 
						file_list, GL0, GL0, GL0, GL0, GL0, start_bin, start_box, stop_bin, stop_box, min_ylim, max_ylim, min_ylim_box, 
								max_ylim_box, cbValue_ylim, my_math2)
			}
			return()
		}
		
# saves selected gene list to a file
SaveGenelist <- function(genelist){
		gene_count <- as.integer(tksize(genelist))
		if(gene_count > 0 & BusyTest()){
			tcl("wm", "attributes", mainframe, topmost=F)
			master <- NULL
			for(i in 1 : gene_count){
				master <- c(master, tclvalue(tkget(genelist,(i-1))))
			}
			file_name <- tclvalue(tkgetSaveFile(filetypes = "{{Gene txt Files} {.txt}}"))
			if (!nchar(file_name)) { ## file save test
				tcl("wm", "attributes", mainframe, topmost=T)
				return()
			}else{
				if(!grepl(".txt",file_name)){
					file_name <- paste(file_name,".txt",sep="")
				}
				write.table(master, file_name, col.names = FALSE, row.names=FALSE, quote=FALSE)
				tcl("wm", "attributes", mainframe, topmost=T)
			}
		}
		return()
	}

# removes selected item(s) from list
RemoveLst<-function(lst1,lst1c){ #do I use this?
	num <- (as.integer(tkcurselection(lst1)))
	if(length(num) < 1){
		num <- 0:(as.integer(tksize(lst1))-1)
	}
	counter <- 0
	for(i in num){
		tkdelete(lst1,(i-counter))
		counter <- counter + 1
	}
	tkdelete(lst1c, 0, 'end')
	lstcount <- paste('  N = ', (as.integer(tksize(lst1))))
	tkinsert(lst1c, "end", lstcount)
}

mainframe <- tktoplevel() #container for it all
	tcl("wm", "attributes", mainframe, topmost = TRUE)
	# tcl('wm', "resizable", mainframe, F, F)
	tkwm.title(mainframe, my_version_num)
	box1 <- tkframe(mainframe, relief='ridge', borderwidth = 5) #frame for top half
		box1a <- tkframe(box1, relief='ridge', borderwidth = 5) #frame for top right 1/2
			b1a1 <- tkframe(box1a) #frame for file list and scroll bars
			main_list <- tklistbox(b1a1, height = 1, width = 38, relief = 'flat', background = 'gray93')
					tkpack(main_list)	
					tkinsert(main_list, 0, "                     List of table files")
				b1a1a <- tkframe(b1a1) 
					yscrb1a1a <- tkscrollbar(b1a1a, repeatinterval = 5, command = function(...) 
						tkyview(file_list, ...))
					file_list <- tklistbox(b1a1a, height = 5,width = 38, selectmode = "single",
						yscrollcommand = function(...) tkset(yscrb1a1a, ...),background = "white")
					tkpack(file_list, side = "left")
					tkbind(file_list,"<Double-ButtonPress-1>",function() handleColorSel(file_list, 1))
					tkpack(yscrb1a1a, fill = 'y', side = "left")
				tkpack(b1a1a, side = 'left')
			tkpack(b1a1, padx = c(0, 0), pady = c(7, 0))
			b1a3 <- tkframe(box1a) #frame for buttons 
				button_SortGeneList <- tkbutton(b1a3, text = " Activate file ", command = function() 
					ActLst(file_list, active_list, ' genes in common ', active_lable, c(1, 0)))
				button_TopGeneList <- tkbutton(b1a3, text = " plot labels ", command = function() linePlotOptions())
				tkpack(button_SortGeneList, button_TopGeneList, side = 'left', padx = c(5, 0), pady = c(0, 0))
				cb_mainlist <- tkcheckbutton(b1a3)
				tkconfigure(cb_mainlist, variable = tclVar(CHECK_LIST[[1]][1]), command=function() 
					CHECK_LIST[[1]] <<- HandleMasterCheck(as.integer(tksize(file_list)), CHECK_LIST[[1]], COLOR_LIST[[1]], 
						cb_mainlist, file_list))
				tkpack(tklabel(b1a3, text = 'on/off'), side = 'left',	padx = c(5, 0), pady = c(0, 0))
				tkpack(cb_mainlist,  side = 'left', padx = c(5, 0), pady = c(0, 0))
			tkpack(b1a3) 
			b1a3a <- tkframe(box1a, relief='ridge', borderwidth = 5)# frame for gene count
					gene_main_count <- tklistbox(b1a3a, height = 2, width = 40, relief = 'flat',
						background = 'gray93')
					tkpack(gene_main_count)				
			tkpack(b1a3a,  side = 'top', padx = c(0, 0), pady = c(0, 0))
			b1a4 <- tkframe(box1a) #frame for buttons
				button_GetTableFile <- tkbutton(b1a4, text = "  Load Table File(s)  ", command=function() 
					GetTableFile(FILE_LIST, G1_main, file_list, gene_list0,	gene_list1, gene_list2, 
					gene_list3, gene_main_count, G1C_main, gene_list0_count,	gene_list1_count, gene_list2_count, gene_list3_count))
				button_ClearTableFile <- tkbutton(b1a4, text = "  Restart   ", command=function() ClearTableFile())
				tkpack(button_GetTableFile, button_ClearTableFile, padx = c(0, 0), side = 'left')
				tkpack(tkbutton(b1a4, text = '  Quit  ', command=function() quit_Program()))
			tkpack(b1a4, pady = c(0, 5))
			b1a5 <- tkframe(box1a) #frame for buttons
				tkpack(tkbutton(b1a5, font =c('bold', 23), text = '      Plot       ',
					command = function()  Setup_Test(FILE_LIST, GENE_LIST, c(1:5), CHECK_LIST, file_list, gene_list0, gene_list1, gene_list2, 
						gene_list3, gene_main_count, gene_list0_count, gene_list1_count, gene_list2_count, 
							gene_list3_count, start_bin, start_box, stop_bin, stop_box, min_ylim, max_ylim, min_ylim_box, 
								max_ylim_box, cbValue_ylim, my_math2)))	
			tkpack(b1a5, pady = c(2, 2))
			b1a6 <- tkframe(box1a) #frame for plotting options and math
				b1a6a <- tkframe(b1a6, relief='ridge', borderwidth = 5)
					b1a6a1 <- tkframe(b1a6a)
						tkpack(tkwidget(b1a6a1, type ="spinbox", state = "readonly", wrap = TRUE, width = 7, 
							textvariable = my_math2, values = my_math), side = 'left', padx = c(10, 15)
								, pady = c(3, 3))
						cb_relative_frequency <- tkcheckbutton(b1a6a1)
						cb_gene_relative_frequency <- tkcheckbutton(b1a6a1)
						tkconfigure(cb_relative_frequency, variable = cbValue_relative_frequency)
						tkconfigure(cb_gene_relative_frequency, variable = cbValue_gene_relative_frequency)
						tkpack(tklabel(b1a6a1, text = 'Relatve Frequency: Bin, Gene'), cb_relative_frequency,	
						       cb_gene_relative_frequency, side = 'left', padx = c(0, 0))
					tkpack(b1a6a1, padx = c(0, 0), pady = c(0, 0), side = 'top')
					b1a6a2 <- tkframe(b1a6a)
					  tkpack(cb_log2 <- tkcheckbutton(b1a6a ,text = "log2"), side = 'left')
					  tkconfigure(cb_log2, variable = cbValue_log2)
						tkpack(tklabel(b1a6a2, text = 'Norm bin'), side = 'left',padx = c(5, 0))
						tkpack(tkentry(b1a6a2, width = 2, textvariable = norm_bin), side = 'left', 
							padx = c(0, 3), pady = c(0, 0))
						tkpack(tklabel(b1a6a2, text = ' Bins to plot'),side = 'left', padx = c(20, 3), 
							pady = c(0, 0))
						start_box <- tkentry(b1a6a2, width = 2, textvariable = start_bin)
						tkpack(start_box, side = 'left', padx = c(0, 0), pady = c(0, 0))
						tkpack(tklabel(b1a6a2, text = '- '),side = 'left', padx = c(0, 0), pady = c(5, 0))			
						stop_box <- tkentry(b1a6a2, width = 2, textvariable = stop_bin)
						tkpack(stop_box, side = 'left', padx = c(0, 5), pady = c(0, 0))
					tkpack(b1a6a2, pady = c(0, 0), side = 'top')
				tkpack(b1a6a, side = 'left', padx = c(0, 0), pady = c(0, 0))
			tkpack(b1a6)
		tkpack(box1a, side = 'left')
		box1b <- tkframe(box1) #frame for top left 1/2
			b1b1 <- tkframe(box1b)
				b1b1a <- tkframe(b1b1)  
					b1b1a1 <- tkframe(b1b1a, relief='ridge', borderwidth = 5)
						b1b1a1a <- tkframe(b1b1a1) # frame for lists and scroll bars
							active_lable <- tklistbox(b1b1a1, height = 1, width = 38, relief = 'flat', 
								background = 'grey93')
							tkinsert(active_lable,0,"                     No Active table files")
							active_list <- tklistbox(b1b1a1a, height = 2, width = 40, selectmode = "single",  
								background = "white")
							tkpack(active_lable, side = "top", pady = c(0, 0), padx = c(0, 0))
							tkpack(active_list, side = "left", pady = c(0, 0), padx = c(0, 0))
						tkpack(b1b1a1a, pady = c(0, 0), side = 'top')
						b1b1a1b <- tkframe(b1b1a1) # frame for activate table list button  
							deact_button <- tkbutton(b1b1a1b,text="  Deactivate file(s)  ", 
								command=function() DActLst(active_list, active_lable, 5))
							genetool_button <- tkbutton(b1b1a1b,text="  Gene tools  ", command = function() swapFrame())
							tkpack(deact_button, genetool_button, padx = c(10, 0), side = 'left')
						tkpack(b1b1a1b, pady = c(0, 0), side = 'top')
					tkpack(b1b1a1, pady = c(0, 0), side = 'left')      
				tkpack(b1b1a)    
			tkpack(b1b1)
			b1b2 <- tkframe(box1b)
				b1b2a <- tkframe(b1b2, relief='ridge', borderwidth = 5)
					b1b2a0 <- tkframe(b1b2a)
						tkpack(tkbutton(b1b2a0,text="  plot list  ", command = function() handleGenePlot(G1_main, G1C_main)), side='left', pady = c(5, 5), padx = c(5, 0))
						tkpack(tkbutton(b1b2a0,text="  plot selected  ", command = function() handleGenePlot2(G1_main, G1C_main)), side='left', pady = c(0, 0), padx = c(5, 5))
					tkpack(b1b2a0)
					b1b2a1 <- tkframe(b1b2a)
						b2g1b1 <- tkframe(b1b2a1)
							yscrb2g1b1 <- tkscrollbar(b2g1b1, repeatinterval = 5, command = function(...)
								tkyview(G1_main, ...))
							G1_main <- tklistbox(b2g1b1, height = 8, width = 38, 
								selectmode = "extended", xscrollcommand = function(...) tkset(xscrb2g1b1, ...), 
								yscrollcommand = function(...) tkset(yscrb2g1b1, ...), background = "white")
							G1C_main <- tklistbox(b1b2a1, height = 2, width = 38, relief = 'flat', 
								background = 'gray93')
							tkpack(G1C_main, pady = c(0, 0), padx = c(0, 0))
							tkpack(G1_main, side = "left", pady = c(0, 0), padx = c(0, 0))
							tkpack(yscrb2g1b1, fill = 'y', side = "left")
							xscrb2g1b1 <- tkscrollbar(b1b2a1, repeatinterval = 5, orient = "horizontal",
								command = function(...) tkxview(G1_main, ...))
							tkpack(xscrb2g1b1, fill = 'both', side = 'bottom')	
						tkpack(b2g1b1, pady = c(0, 3), side = 'left') 
					tkpack(b1b2a1)
					b1b2a2 <- tkframe(b1b2a)
						tkpack(tkbutton(b1b2a2,text="  Intersect gene list  ",command = function() 
							SortGene(file_list, G1C_main, GENE_LIST)), tkbutton(b1b2a2,text="  Save list  ", command = function() SaveGenelist(G1_main)), 
										side='left', pady = c(0, 5), padx = c(10, 0))
					tkpack(b1b2a2)
				tkpack(b1b2a, side='left')  
			tkpack(b1b2, side = 'top')
		tkpack(box1b, side = 'right', padx = c(0, 0))
	tkpack(box1)
			
		#bottom half frame for all the gene table files
		makeListFrame <- function(...){
			box2l <<- tkframe(mainframe, relief='ridge', borderwidth = 5)
				box2la <- tkframe(box2l)		
					b2la1 <- tkframe(box2la, relief='ridge', borderwidth = 5)
						b2la1a <- tkframe(b2la1)
							b2la1a1 <- tkframe(b2la1a)
								yscrb2la1a1 <- tkscrollbar(b2la1a1, repeatinterval = 5, command = function(...)
									tkyview(gene_list0, ...))
								gene_list0 <<- tklistbox(b2la1a1, height = 5, width = 38, selectmode = "extended",  
									yscrollcommand = function(...) tkset(yscrb2la1a1, ...), background = "white")
								gene_list0_count <<- tklistbox(b2la1, height = 1, width = 35, relief = 'flat', 
									background = 'gray93')
								tkpack(gene_list0_count, pady = c(0, 0), padx = c(0, 0))
								tkpack(gene_list0, side = "left", pady = c(0, 0), padx = c(0, 0))
								tkbind(gene_list0,"<Double-ButtonPress-1>",function() handleColorSel(gene_list0, 2))
								tkpack(yscrb2la1a1, fill = 'y', side = "left")
							tkpack(b2la1a1, pady = c(0, 3), side = 'left') 
						tkpack(b2la1a)
						b2la1b <- tkframe(b2la1)
							tkpack(tkbutton(b2la1b,text=" load gene list  ", command = function()	GENE_LIST[[2]] <<- 
								GetGenelist(file_list, 2, gene_list0,	gene_list0_count, cb_mainlist1, GENE_LIST[[2]])), 
									side = "left", padx = c(0, 0))
							tkpack(tkbutton(b2la1b, text = " Activate file ", command = function() ActLst(gene_list0, 
								active_list, tkget(gene_list0_count,0), active_lable, c(2, 1))),	side = 'left', padx = c(0, 0))
							cb_mainlist1 <<- tkcheckbutton(b2la1b)							
							tkpack(tklabel(b2la1b, text = '  on/off'), side = 'left', padx = c(0, 0), 
								pady = c(0, 0))
							tkpack(cb_mainlist1,  side = 'left', padx = c(5, 0), pady = c(0, 0))
						tkpack(b2la1b)
					tkpack(b2la1, padx = c(0, 0), side='left')
					b2la2 <- tkframe(box2la, relief='ridge', borderwidth = 5)
						b2la2a <- tkframe(b2la2)
							b2la2a1 <- tkframe(b2la2a)
								yscrb2la2a1 <- tkscrollbar(b2la2a1, repeatinterval = 5, command = function(...)
									tkyview(gene_list1, ...))
								gene_list1 <<- tklistbox(b2la2a1, height = 5, width = 38, selectmode = "extended",  
									yscrollcommand = function(...) tkset(yscrb2la2a1, ...), background = "white")
								gene_list1_count <<- tklistbox(b2la2, height = 1, width = 35, relief = 'flat', 
									background = 'gray93')
								tkpack(gene_list1_count, pady = c(0, 0), padx = c(0, 0))
								tkpack(gene_list1, side = "left", pady = c(0, 0), padx = c(0, 0))
								tkbind(gene_list1,"<Double-ButtonPress-1>",function() handleColorSel(gene_list1, 3))
								tkpack(yscrb2la2a1, fill = 'y', side = "left")
							tkpack(b2la2a1, pady = c(0, 3), side = 'left') 
						tkpack(b2la2a)
						b2la2b <- tkframe(b2la2)
							tkpack(tkbutton(b2la2b,text=" load gene list  ", command = function()	GENE_LIST[[3]] <<- 
								GetGenelist(file_list, 3, gene_list1,	gene_list1_count, cb_mainlist2, GENE_LIST[[3]])), 
									side = "left", padx = c(0, 0))
							tkpack(tkbutton(b2la2b, text = " Activate file ", command = function() ActLst(gene_list1, 
								active_list, tkget(gene_list1_count,0), active_lable, c(3, 2))),	side = 'left', padx = c(0, 0))
							cb_mainlist2 <<- tkcheckbutton(b2la2b)
							tkpack(tklabel(b2la2b, text = '  on/off'), side = 'left', padx = c(0, 0), 
								pady = c(0, 0))
							tkpack(cb_mainlist2,  side = 'left', padx = c(5, 0), pady = c(0, 0))
						tkpack(b2la2b)
					tkpack(b2la2,  padx = c(0, 0), side='right')
				box2lb <- tkframe(box2l, relief='ridge', borderwidth = 5)
					b2lb1 <- tkframe(box2lb)
						b2lb1a <- tkframe(b2lb1)
							yscrb2lb1a <- tkscrollbar(b2lb1a, repeatinterval = 5, command = function(...)
								tkyview(gene_list2, ...))
							gene_list2 <<- tklistbox(b2lb1a, height = 5, width = 38, selectmode = "extended",  
								yscrollcommand = function(...) tkset(yscrb2lb1a, ...), background = "white")
							gene_list2_count <<- tklistbox(box2lb, height = 1, width = 35, relief = 'flat', 
								background = 'gray93')
							tkpack(gene_list2_count, pady = c(0, 0), padx = c(0, 0))
							tkpack(gene_list2, side = "left", pady = c(0, 0), padx = c(0, 0))
							tkbind(gene_list2,"<Double-ButtonPress-1>",function() handleColorSel(gene_list2, 4))
							tkpack(yscrb2lb1a, fill = 'y', side = "left")
						tkpack(b2lb1a, pady = c(0, 3), side = 'bottom') 
					tkpack(b2lb1)
				tkpack(box2la)
				b2lb2 <- tkframe(box2lb)
					tkpack(tkbutton(b2lb2,text=" load gene list  ", command = function()	GENE_LIST[[4]] <<- 
								GetGenelist(file_list, 4, gene_list2,	gene_list2_count, cb_mainlist3, GENE_LIST[[4]])),
									side = "left", padx = c(0, 0))
					tkpack(tkbutton(b2lb2, text = " Activate file ", command = function() ActLst(gene_list2, 
								active_list, tkget(gene_list2_count,0), active_lable, c(4, 3))),	side = 'left', padx = c(0, 0))
					cb_mainlist3 <<- tkcheckbutton(b2lb2)
					tkpack(tklabel(b2lb2, text = '  on/off'), side = 'left', padx = c(0, 0), pady = c(0, 0))
					tkpack(cb_mainlist3,  side = 'left', padx = c(5, 0), pady = c(0, 0))
				tkpack(b2lb2)
			tkpack(box2lb, padx = c(0, 0), side='right')
			box2lc <- tkframe(box2l, relief='ridge', borderwidth = 5)
				b2lc1 <- tkframe(box2lc)
					b2lc1a <- tkframe(b2lc1)
						yscrb2lc1a <- tkscrollbar(b2lc1a, repeatinterval = 5, command = function(...)
							tkyview(gene_list3, ...))
						gene_list3 <<- tklistbox(b2lc1a, height = 5, width = 38, selectmode = "extended",  
							yscrollcommand = function(...) tkset(yscrb2lc1a, ...), background = "white")
						gene_list3_count <<- tklistbox(box2lc, height = 1, width = 35, relief = 'flat', 
							background = 'gray93')
						tkpack(gene_list3_count, pady = c(0, 0), padx = c(0, 0))
						tkpack(gene_list3, side = "left", pady = c(0, 0), padx = c(0, 0))
						tkbind(gene_list3,"<Double-ButtonPress-1>",function() handleColorSel(gene_list3, 5))
						tkpack(yscrb2lc1a, fill = 'y', side = "left")
					tkpack(b2lc1a, pady = c(0, 3), side = 'left') 
				tkpack(b2lc1)
				b2lc2 <- tkframe(box2lc)
					tkpack(tkbutton(b2lc2,text=" load gene list  ", command = function()	GENE_LIST[[5]] <<- 
								GetGenelist(file_list, 5, gene_list3,	gene_list3_count, cb_mainlist4, GENE_LIST[[5]])),
									side = "left", padx = c(0, 0))
					tkpack(tkbutton(b2lc2, text = " Activate file ", command = function() ActLst(gene_list3, 
								active_list, tkget(gene_list3_count,0), active_lable, c(5, 4))),	side = 'left', padx = c(0, 0))
					cb_mainlist4 <<- tkcheckbutton(b2lc2)
					tkpack(tklabel(b2lc2, text = '  on/off'), side = 'left', padx = c(0, 0), pady = c(0, 0))
					tkpack(cb_mainlist4,  side = 'left', padx = c(5, 0), pady = c(0, 0))
				tkpack(b2lc2)
			tkpack(box2lc,  padx = c(0, 0), side='bottom')
		tkpack(box2l)
		tkconfigure(cb_mainlist1, variable = tclVar(CHECK_LIST[[2]][1]), command=function() 
			CHECK_LIST[[2]] <<- HandleMasterCheck(as.integer(tksize(file_list)), CHECK_LIST[[2]], COLOR_LIST[[2]], 
				cb_mainlist1, gene_list0))
		tkconfigure(cb_mainlist2, variable = tclVar(CHECK_LIST[[3]][1]), command=function() 
			CHECK_LIST[[3]] <<- HandleMasterCheck(as.integer(tksize(file_list)), CHECK_LIST[[3]], COLOR_LIST[[3]], 
				cb_mainlist2, gene_list1))		
		tkconfigure(cb_mainlist3, variable = tclVar(CHECK_LIST[[4]][1]), command=function() 
			CHECK_LIST[[4]] <<- HandleMasterCheck(as.integer(tksize(file_list)), CHECK_LIST[[4]], COLOR_LIST[[4]], 
				cb_mainlist3, gene_list2))
		tkconfigure(cb_mainlist4, variable = tclVar(CHECK_LIST[[5]][1]), command=function() 
			CHECK_LIST[[5]] <<- HandleMasterCheck(as.integer(tksize(file_list)), CHECK_LIST[[5]], COLOR_LIST[[5]], 
				cb_mainlist4, gene_list3))
		UpdateFileLists(as.integer(tksize(file_list)), NULL, file_list, gene_list0,	gene_list1, gene_list2, gene_list3, 
		                gene_main_count, gene_list0_count,	gene_list1_count, gene_list2_count, gene_list3_count, c(1:4), FALSE, 0)
		}
		
		#bottom half frame for all the Gene tools
		makeGeneFrame <- function(...){
			stop_bin1 <<-  tclVar(as.integer(tclvalue(stop_bin)))
			one_bin2 <<- tclVar(as.integer(tclvalue(stop_bin)))
			down_bin1 <<- tclVar(as.integer(tclvalue(Pos_one))-5)
			down_bin2 <<- tclVar(as.integer(tclvalue(Pos_one))+5)
			up_bin1 <<- tclVar(as.integer(tclvalue(Pos_one))+6)
			up_bin2 <<- tclVar(as.integer(tclvalue(Pos_four)))
			box2g <<- tkframe(mainframe, relief='ridge', borderwidth = 5)
			  b2g1 <- tkframe(box2g)
					b2g1a <- tkframe(b2g1, relief='ridge', borderwidth = 5)
						tkpack(tklabel(b2g1a, text = "Get sorted gene list(s)"),pady = c(0, 0))
						b2g1a0 <- tkframe(b2g1a)
							tkpack(tklabel(b2g1a0),side = 'left', padx = c(0, 0), pady = c(0, 0))
							tkpack(tkwidget(b2g1a0, type ="spinbox", state = "readonly", wrap = TRUE, 
								width = 11, textvariable = my_percent2, values = my_percent), side = 'left',
									padx = c(0, 0), pady = c(0, 0))
							tkpack(tklabel(b2g1a0, text = ' Bins'),side = 'left', padx = c(0, 0), pady = c(0, 0))
							start_box1 <<- tkentry(b2g1a0, width = 2, textvariable = start_bin1)
							tkpack(start_box1, side = 'left', padx = c(0, 0), pady = c(0, 0))
							tkpack(tklabel(b2g1a0, text = '- '),side = 'left', padx = c(0, 0), pady = c(0, 0))			
							stop_box1 <<- tkentry(b2g1a0, width = 2, textvariable = stop_bin1)
							tkpack(stop_box1,side = 'left', padx = c(0, 0), pady = c(0, 0))
						b2g1a1 <- tkframe(b2g1a)
							tkpack(tkbutton(b2g1a1,text="  Get gene list(s)   ", command =  function() 
								SortTop(file_list, active_list, G1_main, G2, G3, G4, G2C, G3C, G4C, FILE_LIST, start_bin1,
									start_box1, stop_bin1, stop_box1, my_percent2)), pady = c(0, 0), side = 'bottom')	
						tkpack(b2g1a0,b2g1a1, padx = c(0, 0), pady = c(0, 0))
					tkpack(b2g1a, pady = c(0, 0), side = 'top')
				b2g1b <- tkframe(b2g1, relief='ridge', borderwidth = 5) #frame for region tool 
					tkpack(tklabel(b2g1b, text = "Compare regions"),pady = c(0, 3))
					b2g1b1 <- tkframe(b2g1b)
						tkpack(tklabel(b2g1b1, text = '    Bin region'),side = 'left', padx = c(0, 0), 
							pady = c(0, 0))
						one_box1 <<- tkentry(b2g1b1, width = 2, textvariable = one_bin1)
						tkpack(one_box1, side = 'left', padx = c(0, 0), pady = c(0, 0))
						tkpack(tklabel(b2g1b1, text = ' - '),side = 'left', padx = c(0, 0), pady = c(0, 0))			
						one_box2 <<- tkentry(b2g1b1, width = 2, textvariable = one_bin2)
						tkpack(one_box2, side = 'left', padx = c(0, 0), pady = c(0, 0))
					tkpack(b2g1b1, padx = c(0, 10), pady = c(0, 0))
					b2g1b2 <- tkframe(b2g1b)
						tkpack(tklabel(b2g1b2, text = 'Fold cut off  '),side = 'left', padx = c(0, 0), 
							pady = c(0, 0))
						one_box <<-tkentry(b2g1b2, width = 2, textvariable = one_diff)
						tkpack(one_box, side = 'left',padx = c(0, 0), pady = c(0, 0))
					tkpack(b2g1b2, padx = c(0, 0), pady = c(0, 0))
					b2g1b3 <- tkframe(b2g1b)
						tkpack(tkbutton(b2g1b3,text="  Scatter plot   ", command = function() 
							plotSOne(file_list, active_list, G1_main, G2, G3, G4, G2C, G3C, G4C, FILE_LIST, 
								one_bin1, one_box1, one_bin2, one_box2, one_diff, one_box)),pady = c(0, 0), side = 'bottom')
					tkpack(b2g1b3, padx = c(0, 0), pady = c(0, 0))
				tkpack(b2g1b, pady = c(0, 0), side = 'top')
				
				b2g1c <- tkframe(b2g1, relief='ridge', borderwidth = 5)  #frame ratio tool
					tkpack(tklabel(b2g1c, text = "Compare ratios"), pady = c(0, 3))
				b2g1c1 <- tkframe(b2g1c)
					tkpack(tklabel(b2g1c1, text = '   Bins  '), side = 'left', padx = c(0, 0), pady = c(0, 0))
					up_box1 <<- tkentry(b2g1c1, width = 2, textvariable = up_bin1)
					tkpack(up_box1, side = 'left', padx = c(0, 0), pady = c(0, 0))
					tkpack(tklabel(b2g1c1, text = ' - '), side = 'left', padx = c(0, 0), pady = c(0, 0))			
					up_box2 <<- tkentry(b2g1c1, width = 2, textvariable = up_bin2)
					tkpack(up_box2, side = 'left', padx = c(0, 0), pady = c(0, 0))
					tkpack(tklabel(b2g1c1, text = '/'), side = 'left', padx = c(0, 0), pady = c(0, 0))
					down_box1 <<- tkentry(b2g1c1, width = 2, textvariable = down_bin1)
					tkpack(down_box1, side = 'left', padx = c(0, 0), pady = c(0, 0))
					tkpack(tklabel(b2g1c1, text = ' - '),side = 'left', padx = c(0, 0), pady = c(0, 0))			
					down_box2 <<- tkentry(b2g1c1, width = 2, textvariable = down_bin2)
					tkpack(down_box2, side = 'left', padx = c(0, 8), pady = c(0, 0))
				tkpack(b2g1c1, padx = c(0, 0), pady = c(0, 0))
				b2g1c3 <- tkframe(b2g1c)
					tkpack(tklabel(b2g1c3, text = 'Fold diff cut off  '),side = 'left', padx = c(0, 0), 
						pady = c(0, 0))
					fold_box <<-tkentry(b2g1c3, width = 2, textvariable = fold_diff)
					tkpack(fold_box, side = 'left',padx = c(0, 0), pady = c(0, 0))
				tkpack(b2g1c3, padx = c(0, 0), pady = c(0, 0))
				b2g1c4 <- tkframe(b2g1c)
					tkpack(tkbutton(b2g1c4,text="  Scatter plot   ", command = function() 
						plotEI(file_list, active_list, G1_main, G2, G3, G4, G2C, G3C, G4C, FILE_LIST, 
							up_bin1, up_box1, up_bin2, up_box2, down_bin1, down_box1, down_bin2, 
								down_box2, fold_diff, fold_box)),pady = c(0, 0), side = 'bottom')
				tkpack(b2g1c4, padx = c(0, 0), pady = c(0, 0))
				tkpack(b2g1c, pady = c(0, 0), side = 'top')
				
				b2g1d <- tkframe(b2g1, relief='ridge', borderwidth = 5)
				tkpack(tklabel(b2g1d, text = "Get clusters"),pady = c(0, 0))
				b2g1d0 <- tkframe(b2g1d)
				tkpack(tklabel(b2g1d0),side = 'left', padx = c(0, 0), pady = c(0, 0))
				cb_gene_relative_frequency2 <<- tkcheckbutton(b2g1d0)
				tkpack(tklabel(b2g1d0, text = 'Rel Freq: Gene'), cb_gene_relative_frequency2,
				       side = 'left', padx = c(0, 0), pady = c(0, 0))
				tkconfigure(cb_gene_relative_frequency2, variable = cbValue_gene_cluster_relative_frequency)
				tkpack(tklabel(b2g1d0, text = ' Bins'),side = 'left', padx = c(0, 0), pady = c(0, 0))
				start_cluster1 <<- tkentry(b2g1d0, width = 2, textvariable = start_bin_cluster)
				tkpack(start_cluster1, side = 'left', padx = c(0, 0), pady = c(0, 0))
				tkpack(tklabel(b2g1d0, text = '- '),side = 'left', padx = c(0, 0), pady = c(0, 0))			
				stop_cluster1 <<- tkentry(b2g1d0, width = 2, textvariable = stop_bin1)
				tkpack(stop_cluster1, side = 'left', padx = c(0, 0), pady = c(0, 0))
				b2g1d1 <- tkframe(b2g1d)
				tkpack(tkbutton(b2g1d1,text="  Get 3 clusters   ", command =  function() 
				  FindClustersSetUpTest(FILE_LIST, file_list, active_list, G1_main, start_bin_cluster, start_cluster1, 
				                        stop_bin1, stop_cluster1, start_bin, start_box, stop_bin, stop_box, 
				                        my_math2, min_ylim, max_ylim, min_ylim_box, 
				                        max_ylim_box, cbValue_ylim, G2, G3, G4, G2C, G3C, G4C)), 
				  pady = c(0, 0), side = 'bottom')	
				tkpack(b2g1d0,b2g1d1, padx = c(0, 0), pady = c(0, 0))
				tkpack(b2g1d, pady = c(0, 0), side = 'bottom')
				
			tkpack(b2g1, side = 'left', padx = c(0, 0), pady = c(0, 0))
# 			b2g5 <- tkframe(box2g)
# 			tkpack(tkbutton(b2g5,text="Plot all lists"))
# 			tkpack(b2g5, side = "bottom")
				b2g2 <- tkframe(box2g, relief='ridge', borderwidth = 5)
					b2g2a <- tkframe(b2g2)
						tkpack(tkbutton(b2g2a,text="plot list", command = function() handleGenePlot(G2, G2C)), side='left', pady = c(5, 5), padx = c(5, 0))
						tkpack(tkbutton(b2g2a,text="plot selected", command = function() handleGenePlot2(G2, G2C)), side='left', pady = c(5, 5), padx = c(0, 5))
					tkpack(b2g2a)
					b2g2b <- tkframe(b2g2)
						b2g2b1 <- tkframe(b2g2b)
							yscrb2g2b1 <- tkscrollbar(b2g2b1, repeatinterval = 5, command = function(...)
								tkyview(G2, ...))
							G2 <<- tklistbox(b2g2b1, height = 9, width = 20, selectmode = "extended",
								xscrollcommand = function(...) tkset(xscrb2g2b1, ...), 
								yscrollcommand = function(...) tkset(yscrb2g2b1, ...), background = "white")
							G2C <<- tklistbox(b2g2, height = 2, width = 20, relief = 'flat', 
								background = 'gray93')
								tkpack(G2C, pady = c(0, 0), padx = c(0, 0))
								tkpack(G2, side = "left", pady = c(0, 0), padx = c(0, 0))
								tkpack(yscrb2g2b1, fill = 'y', side = "left")
								xscrb2g2b1 <- tkscrollbar(b2g2b, repeatinterval = 5, orient = "horizontal",
									command = function(...) tkxview(G2, ...))
								tkpack(xscrb2g2b1, fill = 'both', side = 'bottom')	
							tkpack(b2g2b1, pady = c(0, 3), side = 'left') 
						tkpack(b2g2b)
						b2g2c <- tkframe(b2g2)
							tkpack(tkbutton(b2g2c,text="Remove", command = function() RemoveLst(G2, G2C)),	
							       tkbutton(b2g2c,text="Save list", command = function() SaveGenelist(G2)), 
								side='left', pady = c(5, 5), padx = c(0, 0))
						tkpack(b2g2c)
					tkpack(b2g2,  padx = c(0, 0), side='left')
					b2g3 <- tkframe(box2g, relief='ridge', borderwidth = 5)
						b2g3a <- tkframe(b2g3)
							tkpack(tkbutton(b2g3a,text="plot list", command = function() handleGenePlot(G3, G3C)), side='left', pady = c(5, 5), padx = c(5, 0))
							tkpack(tkbutton(b2g3a,text="plot selected", command = function() handleGenePlot2(G3, G3C)), side='left', pady = c(5, 5), 
								padx = c(0, 5))
						tkpack(b2g3a)
						b2g3b <- tkframe(b2g3)
							b2g3b1 <- tkframe(b2g3b)
								yscrb2g3b1 <- tkscrollbar(b2g3b1, repeatinterval = 5, command = function(...)
									tkyview(G3, ...))
								G3 <<- tklistbox(b2g3b1, height = 9, width = 20, selectmode = "extended", 
									xscrollcommand = function(...) tkset(xscrb2g3b1, ...),
									yscrollcommand = function(...) tkset(yscrb2g3b1, ...), background = "white")
								G3C <<- tklistbox(b2g3, height = 2, width = 20, relief = 'flat', background = 'gray93')
								tkpack(G3C, pady = c(0, 0), padx = c(0, 0))
								tkpack(G3, side = "left", pady = c(0, 0), padx = c(0, 0))
								tkpack(yscrb2g3b1, fill = 'y', side = "left")
								xscrb2g3b1 <- tkscrollbar(b2g3b, repeatinterval = 5, orient = "horizontal",
									command = function(...) tkxview(G3, ...))
								tkpack(xscrb2g3b1, fill = 'both', side = 'bottom')	
							tkpack(b2g3b1, pady = c(0, 3), side = 'left') 
						tkpack(b2g3b)
						b2g3c <- tkframe(b2g3)
							tkpack(tkbutton(b2g3c,text="Remove", command = function() RemoveLst(G3, G3C)),	
							       tkbutton(b2g3c,text="Save list", command = function() SaveGenelist(G3)), 
								side='left', pady = c(5, 5), padx = c(0, 0))
						tkpack(b2g3c)
					tkpack(b2g3,  padx = c(0, 0), side='left')
					b2g4 <- tkframe(box2g, relief='ridge', borderwidth = 5)
					b2g4a <- tkframe(b2g4)
					tkpack(tkbutton(b2g4a,text="plot list", command = function() handleGenePlot(G4, G4C)), 
					       side='left', pady = c(5, 5), padx = c(5, 0))
					tkpack(tkbutton(b2g4a,text="plot selected", command = function() handleGenePlot2(G4, G4C)), 
					       side='left', pady = c(5, 5), 
					       padx = c(0, 5))
					tkpack(b2g4a)
					b2g4b <- tkframe(b2g4)
					b2g4b1 <- tkframe(b2g4b)
					yscrb2g4b1 <- tkscrollbar(b2g4b1, repeatinterval = 5, command = function(...)
					  tkyview(G4, ...))
					G4 <<- tklistbox(b2g4b1, height = 9, width = 20, selectmode = "extended", 
					                 xscrollcommand = function(...) tkset(xscrb2g4b1, ...),
					                 yscrollcommand = function(...) tkset(yscrb2g4b1, ...), background = "white")
					G4C <<- tklistbox(b2g4, height = 2, width = 20, relief = 'flat', background = 'gray93')
					tkpack(G4C, pady = c(0, 0), padx = c(0, 0))
					tkpack(G4, side = "left", pady = c(0, 0), padx = c(0, 0))
					tkpack(yscrb2g4b1, fill = 'y', side = "left")
					xscrb2g4b1 <- tkscrollbar(b2g4b, repeatinterval = 5, orient = "horizontal",
					                          command = function(...) tkxview(G4, ...))
					tkpack(xscrb2g4b1, fill = 'both', side = 'bottom')	
					tkpack(b2g4b1, pady = c(0, 3), side = 'left') 
					tkpack(b2g4b)
					b2g4c <- tkframe(b2g4)
					tkpack(tkbutton(b2g4c,text="Remove", command = function() RemoveLst(G4, G4C)),	
					       tkbutton(b2g4c,text="Save list", command = function() SaveGenelist(G4)), 
					       side='left', pady = c(5, 5), padx = c(0, 0))
					tkpack(b2g4c)
					
					tkpack(b2g4,  padx = c(0, 0), side='left')
				tkpack(box2g)
		}
		
		#opens options for settings on line plot
		linePlotOptions <- function(...){ #needs fixing
			if(BusyTest()){
				CloseSubWindows(c(2))
				lineOptionsframe <<- tktoplevel() #container for it all
				STATE[2] <<- 1
				tkbind(lineOptionsframe, "<Destroy>", function() STATE[2] <<- 0)
			Legend_loc <<- unique(c(as.character(tclvalue(Legend_loc2)),Legend_loc))
		 tcl("wm", "attributes", lineOptionsframe, topmost=TRUE)
		 tcl('wm', "resizable", lineOptionsframe, F, F)
			box1c <- tkframe(lineOptionsframe) #frame for top left
			b1c1 <- tkframe(box1c)
				b1c1a <- tkframe(b1c1, relief='ridge', borderwidth = 5) #box for plot customizations 
				b1c1a0 <- tkframe(b1c1a)
				tkpack(tkwidget(b1c1a0, type ="spinbox", state = "readonly", wrap = TRUE, width = 11, textvariable = Legend_loc2, values = Legend_loc), side = 'left',  padx = c(5, 0), pady = c(0, 2))
					tkpack(tklabel(b1c1a0, text = 'Size '),side = 'left', 
					padx = c(1, 4), pady = c(0, 2))
					tkpack(tkwidget(b1c1a0, type ="spinbox", from =0.1, to = 1, inc=0.1, width = 3, textvariable = Legend_size),side = 'left',
					padx = c(0, 10), pady = c(0, 2))
				tkpack(b1c1a0, side = 'top')
				b1c1a1 <- tkframe(b1c1a)
					tkpack(tkentry(b1c1a1, width = 12, textvariable = Header), side = 'left', 
					padx = c(5, 0), pady = c(0, 2))
					tkpack(tklabel(b1c1a1, text = 'Size '),side = 'left', 
					padx = c(5, 3), pady = c(0, 2))
					tkpack(tkwidget(b1c1a1, type ="spinbox", from =1, to = 4, inc=0.5, width = 3, textvariable = Header_size),side = 'left',
					padx = c(0, 10), pady = c(0, 2))
				tkpack(b1c1a1, side = 'top')
				b1c1a2 <- tkframe(b1c1a)
					tkpack(tkentry(b1c1a2, width = 12, textvariable = xlable), side = 'left', 
					padx = c(5, 0), pady = c(0, 0))
					tkpack(tklabel(b1c1a2, text = 'Size '),side = 'left', 
					padx = c(5, 3), pady = c(0, 0))
					tkpack(tkwidget(b1c1a2, type ="spinbox", from =0.1, to = 3, inc=0.2, width = 3, textvariable = xlable_size),side = 'left',
					padx = c(0, 10), pady = c(0, 0))
				tkpack(b1c1a2, side = 'top')
				b1c1a3 <- tkframe(b1c1a)
					tkpack(tkentry(b1c1a3, width = 12, textvariable = ylable), side = 'left', 
					padx = c(5, 0), pady = c(0, 2))
					tkpack(tklabel(b1c1a3, text = 'Size '),side = 'left', 
					padx = c(5, 3), pady = c(0, 2))
					tkpack(tkwidget(b1c1a3, type ="spinbox", from =0.1, to = 3, inc=0.2, width = 3, textvariable = ylable_size),side = 'left',
					padx = c(0, 10), pady = c(0, 2))
				tkpack(b1c1a3, side = 'top')
				b1c1a4 <- tkframe(b1c1a)
					cb_ylim <<- tkcheckbutton(b1c1a4)
					tkconfigure(cb_ylim, variable = cbValue_ylim)
					tkpack(tklabel(b1c1a4, text = 'Set Y axis?'), side = 'left', 
					padx = c(5, 0), pady = c(0, 0))
				tkpack(cb_ylim,  side = 'left', padx = c(5, 0), pady = c(0, 0))
				max_ylim_box <<- tkentry(b1c1a4, width = 4, textvariable = max_ylim)
				tkpack(max_ylim_box, side = 'top', padx = c(5, 0), pady = c(0, 0))
					min_ylim_box <<- tkentry(b1c1a4, width = 4, textvariable = min_ylim)
				tkpack(min_ylim_box, side = 'bottom', padx = c(5, 0), pady = c(0, 5))
				tkpack(b1c1a4, side = 'top')
			tkpack(b1c1a, side = 'left', padx = c(0, 0), pady = c(0, 0))
				b1c1b <- tkframe(b1c1, relief='ridge', borderwidth = 5) #box for plot lines and labels
					tkpack(tklabel(b1c1b, text = "Plot lines"), pady = c(5, 5))
				b1c1b1 <- tkframe(b1c1b)
					tkpack(Box_Txt_one <<- tkentry(b1c1b1, width = 5, textvariable = Txt_one), side = 'left', 
					padx = c(10, 0), pady = c(5, 0))
					tkpack(tklabel(b1c1b1, text = 'Pos'), side = 'left', 
					padx = c(5, 3), pady = c(5, 0))
					tkpack(Box_Pos_one <<- tkentry(b1c1b1, width = 4, textvariable = Pos_one), side = 'left',
					padx = c(0, 10), pady = c(5, 0))
				tkpack(b1c1b1, side = 'top')
				b1c1b2 <- tkframe(b1c1b)
					tkpack(Box_Txt_two <<- tkentry(b1c1b2, width = 5, textvariable = Txt_two), side = 'left', 
					padx = c(10, 0), pady = c(3, 0))
					tkpack(tklabel(b1c1b2, text = 'Pos'),side = 'left', 
					padx = c(5, 3), pady = c(3, 0))
					tkpack(Box_Pos_two <<- tkentry(b1c1b2, width = 4, textvariable = Pos_two),side = 'left',
					padx = c(0, 10), pady = c(3, 0))
				tkpack(b1c1b2, side = 'top')
				b1c1b3 <- tkframe(b1c1b)
					tkpack(Box_Txt_three <<- tkentry(b1c1b3, width = 5, textvariable = Txt_three), side = 'left', 
					padx = c(10, 0), pady = c(3, 0))
					tkpack(tklabel(b1c1b3, text = 'Pos'), side = 'left', padx = c(5, 3), pady = c(3, 0))
					tkpack(Box_Pos_three <<- tkentry(b1c1b3, width = 4, textvariable = Pos_three),side = 'left',
					padx = c(0, 10), pady = c(3, 0))
				tkpack(b1c1b3, side = 'top')
				b1c1b4 <- tkframe(b1c1b)
					tkpack(Box_Txt_four <<- tkentry(b1c1b4, width = 5, textvariable = Txt_four), side = 'left', 
					padx = c(10, 0), pady = c(3, 4))
					tkpack(tklabel(b1c1b4, text = 'Pos'),side = 'left', 
					padx = c(5, 3), pady = c(3, 4))
					tkpack(Box_Pos_four <<- tkentry(b1c1b4, width = 4, textvariable = Pos_four),side = 'left',
					padx = c(0, 10), pady = c(3, 4))
				tkpack(b1c1b4, side = 'top')
				tkpack(b1c1b, side = 'left', padx = c(0, 0), pady = c(0, 0))
				tkpack(b1c1, side = 'top')  
			b1c2 <- tkframe(box1c) # plotting math frame
				b1c2b <- tkframe(b1c2, relief='ridge', borderwidth = 5) #box for more bin labels
					tkpack(tklabel(b1c2b, text = "Bin labels"), pady = c(4, 3))
					b1c2b1 <- tkframe(b1c2b)
					tkgrid(tkwidget(b1c2b1, type ="spinbox", state = "readonly", wrap = TRUE, width = 20, 
					                textvariable = my_Pos_Txt2, values = my_Pos_Txt, 
					                command = function() Pos_Txt(my_Pos_Txt2)), row = 0, 
					       columnspan = 2, pady = c(5, 10))
					  tkgrid(tklabel(b1c2b1, text = '  text  '), column = 0, row = 1)
						tkgrid(Box_Txt_five <<- tkentry(b1c2b1, width = 35, textvariable = Txt_five), column = 1, row = 1)
						tkgrid(tklabel(b1c2b1, text = '  Pos  '), row = 2)
						tkgrid(Box_Pos_five <<- tkentry(b1c2b1, width = 35, textvariable = Pos_five), column = 1, row = 2)
					tkpack(b1c2b1, side = 'top')
				tkpack(b1c2b, side = 'left', padx = c(0, 0), pady = c(0, 0))	  
				tkpack(b1c2, side = 'top') 
				tkpack(tkbutton(box1c,text="  Plot  ", command = function() Setup_Test(FILE_LIST, GENE_LIST, c(1:5), CHECK_LIST, file_list, gene_list0, gene_list1, gene_list2, 
						gene_list3, gene_main_count, gene_list0_count, gene_list1_count, gene_list2_count, 
							gene_list3_count, start_bin, start_box, stop_bin, stop_box, min_ylim, max_ylim, min_ylim_box, 
								max_ylim_box, cbValue_ylim, my_math2)))
				tkpack(box1c, side = 'right', padx = c(0, 0))
			}else{
				return()
			}
		}			


# tkpack(tklabel(tt,text="Set Starting state"))
# OK_but <- tkbutton(tt,text="   OK   ",command=OnOK)
# tkpack(OK_but, side = "bottom")
# yscr <- tkscrollbar(tt, repeatinterval = 5, command = function(...)	tkyview(mylist, ...))
# mylist <- tklistbox(tt, height = 5,width = 30, selectmode="single",	background = "white", 
# 	yscrollcommand = function(...) tkset(yscr, ...))
# tkinsert(mylist, 0, "543 bins 20,20,40", "5 1.5k-2k 70 bins", "543 bins 10,10,10", "543 bins 20,20,20", "5 .5k-1.5k")
# tkpack(mylist, pady = c(10, 10), padx = c(10, 10), side = "left")
# tkpack(yscr, fill = 'y', side = "left")


swapFrame <- function(){ 
	if(STATE[1] == 0 & STATE[5] == 0){
		if(as.integer(tksize(active_list)) == 0){
			#return()
		}
		STATE[1] <<- 1
		#tkconfigure(cb_mainlist, state = "disabled")
		tkdestroy(box2l)
		return(makeGeneFrame())
		#makeGeneFrame(STATE[6])
	}else if (STATE[1] == 1 & STATE[5] == 0){
		STATE[1] <<- 0
		tkdestroy(box2g)
		#tkconfigure(cb_mainlist, state = "normal")
		return(makeListFrame())
	}
}

makeListFrame()
