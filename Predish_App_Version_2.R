#MODULES
repository="http://cran.us.r-project.org"

#user interface handling
if (!require(shiny)) install.packages('shiny',repos = repository)
library(shiny)

#apply a theme to the ui
if (!require(bslib)) install.packages('bslib',repos = repository)
library(bslib)

#for the heatmap colors
if (!require(RColorBrewer)) install.packages('RColorBrewer',repos = repository)
library(RColorBrewer)

#for all the disable/enable (toggleState) parts
if (!require(shinyjs)) install.packages('shinyjs',repos = repository)
library(shinyjs)

#read the excel files
if (!require(readxl)) install.packages('readxl',repos = repository)
library(readxl)

#read the csv files
if (!require(readr)) install.packages('readr',repos = repository)
library(readr)

#write the excel files
if (!require(openxlsx)) install.packages('openxlsx',repos = repository)
library(openxlsx)

#I had to use shinyDirChoose from shinyFiles : there's a bug on windows 10 where choose_dir() doesn't work if choose.files() haven't been called before.
if (!require(shinyFiles)) install.packages('shinyFiles',repos = repository)
library(shinyFiles)

#species Name comparison
if (!require(stringdist)) install.packages('stringdist',repos = repository)
library(stringdist)

#for graphical output
if (!require(ggplot2)) install.packages('ggplot2',repos = repository)
library(ggplot2)

#used to know file extentions
if (!require(tools)) install.packages('tools',repos = repository)
library("tools")

#CONSTANTS

#Input files names
regressionFileName="regression.RData"
toffDataFileName="dataFromTOFF.RData"

#Output files names
predRangeFileName="predationRange.xlsx"
predMatrixFileName="predationProbabilityMatrix.xlsx"
predListFileName="predationProbabilityList.xlsx"
heatMapFileName="predationProbabiltyHeatMap.pdf"

#FUNCTIONS

#check the xlsx file measure type :1=min+max 2=mean size+SD, 0=error
checkMeasureType =function(namesOfColumns,criticalErrorMsg){
	measureType=0
	if (namesOfColumns[3]=="Min Total length (mm)"){
		measureType=1
	}
	else if (namesOfColumns[3]=="Average Mean Size (Total length mm)"){
		measureType=2
	}
	else{
		criticalErrorMsg=paste0(criticalErrorMsg,"Cell A3: The column name must be \"Min Total length (mm)\" or \"Average Mean Size (Total length mm)\". Instead it was found to be : \"",namesOfColumns[3],"\"<br/>")		
	}
	
	if (namesOfColumns[4]=="Max Total length (mm)"){
		#measureType=1
	}
	else if (namesOfColumns[4]=="Standard deviation"){
		#measureType=2
	}
	else{
		criticalErrorMsg=paste0(criticalErrorMsg,"Cell A4: The column name must be \"Max Total length (mm)\" or \"Standard deviation\". Instead it was found to be : \"",namesOfColumns[4],"\"<br/>")		
	}
	
	return(list("measureType" = measureType, "criticalErrorMsg"=criticalErrorMsg))
}

#return an error if the developmentType isn't Unknown/Juvenile/Adult
checkDevelopmentType =function(developmentType,criticalErrorMsg,current_line_str,column){
	if((developmentType!="Unknown")&&(developmentType!="Juvenile")&&(developmentType!="Adult")){
		criticalErrorMsg <- paste0(criticalErrorMsg,"Cell ",column,current_line_str,": Development type must be Unknown, Juvenile or Adult. Instead it was found to be : \"",developmentType,"\"<br/>")		
	}
	return(criticalErrorMsg)
}

#return an error if the value isn't numeric or empty
checkValues = function(value, criticalErrorMsg, current_line_str,column){
	if (!is.na(value)){
		if (is.na(as.numeric(value))){
			criticalErrorMsg <- paste0(criticalErrorMsg,"Cell ",column,current_line_str,": Values must be empty or numerical. Instead it was found to be : \"",value,"\"<br/>")		
		}
	}
	return(criticalErrorMsg)
}

#return an error if 1/2 values are filled
checkHalfValues = function(value1, value2, criticalErrorMsg, current_line_str,column1, column2){
	if (!is.na(value1)&&(is.na(value2))){
		criticalErrorMsg <- paste0(criticalErrorMsg,"Cell ",column1,current_line_str,": is filled while Cell ",column2,current_line_str," is empty. Both cells must be filled or both cells must be empty.<br/>")		
	}

	if (is.na(value1)&&(!is.na(value2))){
		criticalErrorMsg <- paste0(criticalErrorMsg,"Cell ",column1,current_line_str,": is empty while Cell ",column2,current_line_str," is filled. Both cells must be filled or both cells must be empty.<br/>")		
	}
	return(criticalErrorMsg)
}

#return an error if the Piscivore information isn't Unknown/Yes/No
checkPiscivoreInfo = function(piscivoreInfo,criticalErrorMsg,current_line_str,column){
	if((piscivoreInfo!="Unknown")&&(piscivoreInfo!="Yes")&&(piscivoreInfo!="No")){
		criticalErrorMsg <- paste0(criticalErrorMsg,"Cell ",column,current_line_str,": Piscivore information must be Unknown, Yes or No. Instead it was found to be : \"",piscivoreInfo,"\"<br/>")		
	}
	return(criticalErrorMsg)
}

#replace mean and SD by a calculated min (mean-SD) and max (mean+SD)
calculateMinMaxfromAverageSD = function(inputDataFrame){
	for (row in 1:nrow(inputDataFrame)) {
		average <- inputDataFrame[[row, 3]]
		SD <- inputDataFrame[[row, 4]]
		if (!is.na(average)&&!is.na(SD)){
			newMin=average-SD
			newMax=average+SD
			inputDataFrame[row, 3] <- newMin
			inputDataFrame[row, 4] <- newMax
		}
	}
	colnames(inputDataFrame)[3] <- "Min Size"
	colnames(inputDataFrame)[4] <- "Max Size"
	return(inputDataFrame)
}

#check if the species (and genus) is in TOFF. If it's not, return the closest species in the warning msg
checkSpeciesNames = function(species,speciesTOFFDataframe,warningsErrorMsg,current_line_str,column){
	minDistance=10000
	minSpecies=""
	speIdrows=which(speciesTOFFDataframe[2]==species)
	if (length(speIdrows)==0){
		for (row in 1:nrow(speciesTOFFDataframe)) {
			speciesTOFF <- speciesTOFFDataframe[[row, 2]]
			distance=stringdist(species,speciesTOFF)
			if (distance==0){
				#print(species)
				#print(speciesTOFF)
				break
			}
			if (distance<minDistance){
				minDistance=distance
				minSpecies=speciesTOFF
			}
		}
		if (distance!=0){
			warningsErrorMsg <- paste0(warningsErrorMsg,"Cell ",column,current_line_str,": The species \"",species,"\" wasn't found in TOFF. Did you mean : \"",minSpecies,"\"?<br/>")		
		}
	}
	return(warningsErrorMsg)
}

# Function to calculate overlap proportion
calculate_overlap <- function(min_prey, max_prey, min_size, max_size) {
  overlap_min <- max(min_prey, min_size)
  overlap_max <- min(max_prey, max_size)
  if (overlap_min < overlap_max) {
    return((overlap_max - overlap_min) / (max_size - min_size))
  }
  else {
    return(0)
  }
}

#Change the style of first cols and/or first row in italic, except the 1:1 tile
add_italics_excel <- function(file_path, do_first_row, do_first_col) {
	wb <- loadWorkbook(file_path)
	x = read_xlsx(file_path)
	ncols=ncol(x)
	nrows=nrow(x)+1

	italicStyle <- createStyle(textDecoration = "Italic")

	if (do_first_row == TRUE){
		addStyle(wb, 1, style = italicStyle, rows = 1, cols = 2:ncols, gridExpand = TRUE)
	}
	if (do_first_col == TRUE){
		addStyle(wb, 1, style = italicStyle, rows = 2:nrows, cols = 1, gridExpand = TRUE)
	}

	saveWorkbook(wb, file_path, overwrite = TRUE)
}

#Change the style of the two first cols in italics, except the first row
add_italics_excel_list <- function(file_path){
wb <- loadWorkbook(file_path)
	x = read_xlsx(file_path)
	ncols=ncol(x)
	nrows=nrow(x)+1	
	italicStyle <- createStyle(textDecoration = "Italic")
	addStyle(wb, 1, style = italicStyle, rows = 2:nrows, cols = 1:2, gridExpand = TRUE)
	saveWorkbook(wb, file_path, overwrite = TRUE)
}

#USER INTERFACE (=THE BUTTONS)
ui <- fixedPage(
	useShinyjs(),  # Used for all the disable/enable (toggleState) parts
	theme = bs_theme(preset = "yeti"), #theme definition
	card(
		fileInput("XlsxFileInput", "Choose xlsx or csv Input File", accept = c(".xlsx",".xls",".csv"))
	),
	card(
		disabled(actionButton("UseTOFFButton", "Use TOFF to Fill missing Data",class = "btn-primary"))
	),
	card(
		disabled(checkboxInput("predationCutoffCheckbox", "Remove all results in the predationProbabilityList file equal or over a threshold", FALSE)),
		disabled(textInput('predationCutoffPanel', 'Predation risk threshold [0,1]', value=1)),
		disabled(shinyDirButton('OutputFolderButton', 'Select the Output folder', 'Please select a folder', FALSE,class = "btn-primary")),
		textOutput("OutputFolderText"),
		disabled(actionButton("PredationButton", 'GO !',class = "btn-primary"))		
	),
	card(
		htmlOutput("InfoText")
	)
)

#SERVER (=WHAT THE BUTTONS DO)
server <- function(input, output, session) {
	#uncomment the next line to choose the graphical theme of the user interface (UI) on the fly
	#bs_themer()
	
	#some reactive variables, which can be imported and exported in multiple parts of the UI
	#the xlsx file measure type :min+max=1 mean size+SD=2, error=0
	reactiveMeasureType <- reactiveValues(measureType=0)
	#state of the excel import, which disable or not some part of the UI
	reactiveToggleValues <- reactiveValues(correctImportToggle = FALSE, correctOutputDirPath = FALSE)
	#the main dataframe of the values
	reactiveDataFrames <- reactiveValues(dataFrame=data.frame())

	#as I had to use a different function from an other library (shinyDirChoose from shinyFiles), the usage differs from the rest. I have to defined shinyDirChoose here to use it later. 
	shinyDirChoose(input, 'OutputFolderButton', roots=c(wd='.'), filetypes=c('', 'txt'))
	
	#clean session stop for standalone app. Without it, if you close the app, you can't restart it without restarting the computer first.
	session$onSessionEnded(function() {
		stopApp()
	})
	
	#CHOOSE EXCEL FILE BUTTON (import and check the excel file contents)
	observeEvent(input$XlsxFileInput, {

		infoMsg=""
		warningsErrorMsg=""
		criticalErrorMsg=""

		file_type=file_ext(input$XlsxFileInput$datapath)
		
		if ((file_type=="xlsx")||(file_type=="xls")){
			inputDataFrame <- read_excel(input$XlsxFileInput$datapath)
		}
		else if (file_type=="csv"){
			inputDataFrame <- read_csv(input$XlsxFileInput$datapath,local = locale(encoding = "UTF-8"),show_col_types = FALSE)
		}

		#Replace all empty developmentType by Unknown
		inputDataFrame[2][is.na(inputDataFrame[2])] <- "Unknown"
		#Replace all empty Piscivore bu Unknown
		inputDataFrame[5][is.na(inputDataFrame[5])] <- "Unknown"

		columnsNames=colnames(inputDataFrame)
		
		#check the input file measure type :min+max=1 mean size+SD=2, error=0
		results <- checkMeasureType(columnsNames,criticalErrorMsg)
		measureType = results$measureType
		criticalErrorMsg = results$criticalErrorMsg
		
		#verify the input file content
		for (row in 1:nrow(inputDataFrame)) {
			realRow=as.character(row+1)
			developmentType <- inputDataFrame[[row, 2]]
			value1 <- inputDataFrame[[row, 3]]
			value2 <- inputDataFrame[[row, 4]]
			piscivoreInfo <- inputDataFrame[[row, 5]]
			criticalErrorMsg <- checkDevelopmentType(developmentType,criticalErrorMsg,realRow,"B")
			criticalErrorMsg <- checkValues(value1,criticalErrorMsg,realRow,"C")
			criticalErrorMsg <- checkValues(value2,criticalErrorMsg,realRow,"D")
			criticalErrorMsg <- checkHalfValues(value1, value2, criticalErrorMsg,realRow,"C", "D")
			criticalErrorMsg <- checkPiscivoreInfo(piscivoreInfo,criticalErrorMsg,realRow,"E")
		}
		
		#if mean and SD : transform mean and SD in min and max (i.e: average-SD,average+SD)
		#In the first version of this program, this transformation was in the calculation part, since TOFF extraction could retrieve mean+SD or min+max data. Now the extraction only retrieve min+max, so this transformation has to be done before.
		if(measureType==2){
			inputDataFrame <- calculateMinMaxfromAverageSD(inputDataFrame)
		}
		
		#error handling: If there is at least one critical error disable the rest of the UI
		if (criticalErrorMsg!=""){
			infoMsg=paste0(infoMsg,"There are critical errors. Please fix them and reimport the file.<br/>CRITICAL ERRORS:<br/>",criticalErrorMsg)
			reactiveToggleValues$correctImportToggle <- FALSE
		}
		else{
			reactiveToggleValues$correctImportToggle <- TRUE
			reactiveDataFrames$dataFrame<-inputDataFrame
			reactiveMeasureType$measureType <-measureType
		}
		if (warningsErrorMsg!=""){
			infoMsg=paste0(infoMsg,"<br/>WARNINGS:<br/>",warningsErrorMsg)
		}

		toggleState("UseTOFFButton", condition = reactiveToggleValues$correctImportToggle)
		toggleState("predationCutoffCheckbox", condition = input$correctImportToggle)	
		toggleState("OutputFolderButton", condition = reactiveToggleValues$correctImportToggle)

		output$InfoText <- renderUI({HTML(infoMsg)})
    })
	#USE TOFF DATA BUTTON
	observeEvent(input$UseTOFFButton, {
		infoMsg=""
		criticalErrorMsg=""
		warningsErrorMsg=""

		inputDataFrame <-reactiveDataFrames$dataFrame
		measureType <-reactiveMeasureType$measureType
		
		#return an error if program can't find the TOFF Data file
		if(!file.exists(toffDataFileName)){
			criticalErrorMsg <- paste0(criticalErrorMsg,"I can't find the file ",toffDataFileName,".<br/>")
			criticalErrorMsg <- paste0(criticalErrorMsg,"Please put it in the directory : ",getwd())
		}
		else{
			#load dataFromTOFF.RData file. It will import 3 dataframes : 
			#speciesTOFFDataframe : speciesId,speciesName
			#medianDataframe : speciesName,developmentStage,minMedian,maxMedian
			#dietDataframe : speciesName,developmentStage,is_piscivore_number_of_lines,isnt_piscivore_number_of_lines,number_of_different_environnements
			load(toffDataFileName)
			
			#check if species names are in TOFF and suggest replacement if they aren't
			for (row in 1:nrow(inputDataFrame)) {
				realRow=as.character(row+1)
				species <- inputDataFrame[[row, 1]]
				warningsErrorMsg <- checkSpeciesNames(species,speciesTOFFDataframe,warningsErrorMsg,realRow,"A")
			}

			#adding missing lengths from TOFF, if able

			#temporary dataframe. Used for adding Juvenile and Adult data when the development type is Unknown 
			addedJuvenilesAndAdultsDataframe <- data.frame(matrix(ncol = 5, nrow = 0))
			colnames(addedJuvenilesAndAdultsDataframe) <- colnames(inputDataFrame)
			
			for (row in 1:nrow(inputDataFrame)) {
				realRow=as.character(row+1)
				species <- inputDataFrame[[row, 1]]
				developmentType <- inputDataFrame[[row, 2]]
				value1 <- inputDataFrame[[row, 3]]
				value2 <- inputDataFrame[[row, 4]]
				piscivoreInfo <- inputDataFrame[[row, 5]]
				
				#if the length is missing and the development type is missing, then try to find length data for all dev types and copy the piscivory info given by user
				if (is.na(value1)&&is.na(value2) && developmentType=="Unknown"){
					idRows=which(medianDataframe$speciesName==species & medianDataframe$developmentStage=="Unknown")
					if (length(idRows)==1){
						newMin=as.numeric(medianDataframe$minMedian[idRows])
						newMax=as.numeric(medianDataframe$maxMedian[idRows])
						inputDataFrame[row, 3] <- newMin
						inputDataFrame[row, 4] <- newMax
						infoMsg <- paste0(infoMsg,"Cell A",realRow,": Some size data have been found for the species \"",species,"\" for the Unknown development stage<br/>")
					}
					idRows=which(medianDataframe$speciesName==species & medianDataframe$developmentStage=="Juvenile")
					if (length(idRows)==1){
						newMin=as.numeric(medianDataframe$minMedian[idRows])
						newMax=as.numeric(medianDataframe$maxMedian[idRows])
						tempDataframe <- data.frame(matrix(ncol = 5, nrow = 0))
						colnames(tempDataframe) <- colnames(inputDataFrame)
						tempDataframe[1,1] <- species
						tempDataframe[1,2] <- "Juvenile"
						tempDataframe[1,3] <- newMin
						tempDataframe[1,4] <- newMax
						tempDataframe[1,5] <- piscivoreInfo
						addedJuvenilesAndAdultsDataframe<-rbind(addedJuvenilesAndAdultsDataframe, tempDataframe)
						infoMsg <- paste0(infoMsg,"Cell A",realRow,": Some size data have been found for the species \"",species,"\" for the Juvenile development stage<br/>")
					}
					idRows=which(medianDataframe$speciesName==species & medianDataframe$developmentStage=="Adult")
					if (length(idRows)==1){
						newMin=as.numeric(medianDataframe$minMedian[idRows])
						newMax=as.numeric(medianDataframe$maxMedian[idRows])
						tempDataframe <- data.frame(matrix(ncol = 5, nrow = 0))
						colnames(tempDataframe) <- colnames(inputDataFrame)
						tempDataframe[1,1] <- species
						tempDataframe[1,2] <- "Adult"
						tempDataframe[1,3] <- newMin
						tempDataframe[1,4] <- newMax
						tempDataframe[1,5] <- piscivoreInfo
						addedJuvenilesAndAdultsDataframe<-rbind(addedJuvenilesAndAdultsDataframe, tempDataframe)
						infoMsg <- paste0(infoMsg,"Cell A",realRow,": Some size data have been found for the species \"",species,"\" for the Adult development stage<br/>")
					}
				}
				#if only the length is missing, add the length according to development stage, if able
				if (is.na(value1)&&is.na(value2) && developmentType!="Unknown"){
					idRows=which(medianDataframe$speciesName==species & medianDataframe$developmentStage==developmentType)
					if (length(idRows)==1){
						newMin=as.numeric(medianDataframe$minMedian[idRows])
						newMax=as.numeric(medianDataframe$maxMedian[idRows])
						inputDataFrame[row, 3] <- newMin
						inputDataFrame[row, 4] <- newMax
						infoMsg <- paste0(infoMsg,"Cell A",realRow,": Some size data have been found for the species \"",species,"\" (",developmentType," development stage)<br/>")
					}
				}
			}
			inputDataFrame<-rbind(inputDataFrame, addedJuvenilesAndAdultsDataframe)
			
			#adding missing piscivory data from TOFF, if able
			for (row in 1:nrow(inputDataFrame)) {
				realRow=as.character(row+1)
				species <- inputDataFrame[[row, 1]]
				developmentType <- inputDataFrame[[row, 2]]
				value1 <- inputDataFrame[[row, 3]]
				value2 <- inputDataFrame[[row, 4]]
				piscivoreInfo <- inputDataFrame[[row, 5]]
				
				if (piscivoreInfo=="Unknown"){
					idRows=which(dietDataframe$speciesName==species & dietDataframe$developmentStage==developmentType)
					if (length(idRows)==1){
						nbYesPiscivore=dietDataframe[[idRows, 3]]
						nbNoPiscivore=dietDataframe[[idRows, 4]]
						nbDietEnv=dietDataframe[[idRows, 5]]
						if (nbDietEnv==0){
							warningsErrorMsg <- paste0(warningsErrorMsg,"Cell A",realRow,": The species \"",species,"\" (",developmentType," development stage) doesn't have Diet informations in TOFF<br/>")
						}
						else if (nbYesPiscivore>0){
							inputDataFrame[row, 5] <- "Yes"
							infoMsg <- paste0(infoMsg,"Cell A",realRow,": Some piscivore data have been found for the species \"",species,"\" (",developmentType," development stage) : The species is piscivore (",nbYesPiscivore," \"Yes\" piscivore lines for ",nbDietEnv," environnments)<br/>")
						}
						else if (nbNoPiscivore>0){
							inputDataFrame[row, 5] <- "No"
							infoMsg <- paste0(infoMsg,"Cell A",realRow,": Some piscivore data have been found for the species \"",species,"\" (",developmentType," development stage) The species isn't piscivore (",nbNoPiscivore," \"No\" piscivore lines for ",nbDietEnv," environnments)<br/>")
						}
						else{
							inputDataFrame[row, 5] <- "Unknown"
							warningsErrorMsg <- paste0(warningsErrorMsg,"Cell A",realRow,": No piscivore data have been found for the species \"",species,"\" (",developmentType," development stage) for ",nbDietEnv," environnements<br/>")
						}
					}
				}
			}
		}
		#error handling: if there are errors, add them to the info msg
		if (criticalErrorMsg!=""){
			infoMsg=paste0(infoMsg,"There are critical errors. Please fix them.<br/>CRITICAL ERRORS:<br/>",criticalErrorMsg)
		}
		else{
			reactiveDataFrames$dataFrame<-inputDataFrame
		}
		if (warningsErrorMsg!=""){
			infoMsg=paste0(infoMsg,"<br/>WARNINGS:<br/>",warningsErrorMsg)
		}
		output$InfoText <- renderUI({HTML(infoMsg)})
	})
	#UNABLE THE CUTOFF PANEL WHEN CLICKING ON CHECKBOX
	observeEvent(input$predationCutoffCheckbox, {
		toggleState("predationCutoffPanel", condition = input$predationCutoffCheckbox)	
	})
	#CHOOSE OUTPUT FOLDER BUTTON
	observeEvent(input$OutputFolderButton, {

		#if an outputDirPath has been chosen, unlock the PredationButton
		outputDirPath=parseDirPath(roots=c(wd='.'),input$OutputFolderButton)
		if (length(outputDirPath)>0){
			reactiveToggleValues$correctOutputDirPath <- TRUE
			output$OutputFolderText <- renderText({outputDirPath})
		}
		toggleState("PredationButton", condition = reactiveToggleValues$correctOutputDirPath)
    })
	#"GO" BUTTON (CALCULATE PREDATION BUTTON)
	observeEvent(input$PredationButton, {
	
		infoMsg=""
		criticalErrorMsg=""
		warningsErrorMsg=""

		inputDataFrame <-reactiveDataFrames$dataFrame

		#return an error if the predation cutoff is not a number or not between 0 and 1
		predationCutoffThreshold=gsub(",", ".", input$predationCutoffPanel)
		predationCutoffThreshold=as.numeric(predationCutoffThreshold)
		if (is.na(predationCutoffThreshold)){
			criticalErrorMsg <- paste0(criticalErrorMsg,"The predation cutoff is not a number : ",input$predationCutoffPanel,".<br/>")
		}else if ((predationCutoffThreshold<0)||(predationCutoffThreshold>1)){
			criticalErrorMsg <- paste0(criticalErrorMsg,"The predation cutoff should be between 0 and 1 : ",input$predationCutoffPanel,".<br/>")			
		}
		
		#return an error if output dir doesn't exist
		outputDirPath=parseDirPath(roots=c(wd='.'),input$OutputFolderButton)
		if (!dir.exists(outputDirPath)){
			criticalErrorMsg <- paste0(criticalErrorMsg,"I can't find the specified output dir ",outputDirPath,".<br/>")
		}
		
		#return an error if full non-piscivore
		piscivoreYesRows=which(inputDataFrame[5]=="Yes")
		nbYesPiscivore=length(piscivoreYesRows)

		if(nbYesPiscivore==0){
			criticalErrorMsg <- paste0(criticalErrorMsg,"There are no piscivore. Please change your input file and load it again.<br/>")
		}
		#R can't do heatmap without at least 2 rows and 2 columns :
		#return a warning if there is exactly 1 piscivore
		if(nbYesPiscivore==1){
			warningsErrorMsg <- paste0(warningsErrorMsg,"There are only one piscivore. This is not enough to produce a heatmap.<br/>")
		}
		
		#return an error if program can't find the Regression File
		if(!file.exists(regressionFileName)){
			criticalErrorMsg <- paste0(criticalErrorMsg,"I can't find the file ",regressionFileName,".<br/>")
			criticalErrorMsg <- paste0(criticalErrorMsg,"Please put it in the directory : ",getwd())
		}

		#return an error if output files already exists
		#(The UI crashed if the pdf was opened while it tried to rewrite it, and I have no way to check if the pdf is open so that's the best solution I had)
		predRangeFilePath=file.path(outputDirPath,predRangeFileName)
		predListFilePath=file.path(outputDirPath,predListFileName)
		predMatrixFilePath=file.path(outputDirPath,predMatrixFileName)
		heatMapFilePath=file.path(outputDirPath,heatMapFileName)

		if(file.exists(predRangeFilePath)){
			criticalErrorMsg <- paste0(criticalErrorMsg,"The file ",predRangeFilePath," already exist. Please rename or move it.<br/>")
		}
		if(file.exists(predListFilePath)){
			criticalErrorMsg <- paste0(criticalErrorMsg,"The file ",predListFilePath," already exist. Please rename or move it.<br/>")
		}
		if(file.exists(predMatrixFilePath)){
			criticalErrorMsg <- paste0(criticalErrorMsg,"The file ",predMatrixFilePath," already exist. Please rename or move it.<br/>")
		}
		if(file.exists(heatMapFilePath)){
			criticalErrorMsg <- paste0(criticalErrorMsg,"The file ",heatMapFilePath," already exist. Please rename or move it.<br/>")
		}
		
		#continue if no errors
		if (criticalErrorMsg==""){

			#load regression.RData file. It will fill Param_regvert with parameters for the prediction
			load(regressionFileName)
			qrsup = Param_regvert[[1]]
			qrinf = Param_regvert[[2]]
			
			#create empty Matrix for the list output
			listMatrix<-matrix(nrow = 0, ncol = 3)
			colnames(listMatrix) <- c("Prey species","Predator species","Predation risk")
						
			#sort the data by species name
			inputDataFrame <- inputDataFrame[order(inputDataFrame[[1]]),]
					
			#remove rows without values
			inputDataFrame <- na.omit(inputDataFrame)
								
			#create empty columns
			predRangeAllDataFrame <- inputDataFrame
			predRangeAllDataFrame['Min_prey'] <- NA
			predRangeAllDataFrame['Max_prey'] <- NA

			#log10 transform to prepare for pred range calculation
			predRangeAllDataFrame[,3:4]<-log10(predRangeAllDataFrame[,3:4])

			#calculate pred range
			for(row in 1:nrow(predRangeAllDataFrame)){
				minSize <- predRangeAllDataFrame[[row, 3]]
				maxSize <- predRangeAllDataFrame[[row, 4]]
				predRangeAllDataFrame[[row, 6]] <- qrinf[1] + qrinf[2]*minSize
				predRangeAllDataFrame[[row, 7]] <- qrsup[1] + qrsup[2]*maxSize
			}

			#return to normal size
			predRangeAllDataFrame[,3:4]<-10^(predRangeAllDataFrame[,3:4])
			predRangeAllDataFrame[,6:7]<-10^(predRangeAllDataFrame[,6:7])
					
			#piscivores subset
			predRangePiscivoresDataFrame <- subset(predRangeAllDataFrame, predRangeAllDataFrame[5] == "Yes")

			#paste the names with their dev stages
			namesWithDevStageAllDataFrame = predRangeAllDataFrame
			namesWithDevStagePiscivoresDataFrame = predRangePiscivoresDataFrame

			namesWithDevStageAllDataFrame[1] <- paste(predRangeAllDataFrame[[1]],predRangeAllDataFrame[[2]],sep=" ")
			namesWithDevStagePiscivoresDataFrame[1] <- paste(predRangePiscivoresDataFrame[[1]],predRangePiscivoresDataFrame[[2]],sep=" ")

			#intialize an empty matrix
			result_matrix <- matrix(0, nrow = nrow(predRangeAllDataFrame), ncol = nrow(predRangePiscivoresDataFrame))
					
			#replace the names of the rows and cols
			rownames(result_matrix) <- namesWithDevStageAllDataFrame[[1]]
			colnames(result_matrix) <- namesWithDevStagePiscivoresDataFrame[[1]]
					
			#calculate pred score
			for(allRow in 1:nrow(predRangeAllDataFrame)){
				speciesNameAndDev=namesWithDevStageAllDataFrame[[allRow, 1]]
				speciesMinSize=predRangeAllDataFrame[[allRow, 3]]
				speciesMaxSize=predRangeAllDataFrame[[allRow, 4]]
				#speciesMinPreySize=predRangeAllDataFrame[[allRow, 6]]
				#speciesMaxPreySize=predRangeAllDataFrame[[allRow, 7]]
				for(piscivoreRow in 1:nrow(predRangePiscivoresDataFrame)){
					piscivoreNameAndDev=namesWithDevStagePiscivoresDataFrame[[piscivoreRow, 1]]
					#piscivoreMinSize=predRangePiscivoresDataFrame[[piscivoreRow, 3]]
					#piscivoreMaxSize=predRangePiscivoresDataFrame[[piscivoreRow, 4]]
					piscivoreMinPreySize=predRangePiscivoresDataFrame[[piscivoreRow, 6]]
					piscivoreMaxPreySize=predRangePiscivoresDataFrame[[piscivoreRow, 7]]
					#for the piscivore against himself, the score is 0
					if (speciesNameAndDev==piscivoreNameAndDev){
						score=0
						result_matrix[allRow, piscivoreRow] <- score
						if (score<=predationCutoffThreshold){
							listMatrix<-rbind(listMatrix,c(speciesNameAndDev,piscivoreNameAndDev,score))
						}
					}
					else{
						score=calculate_overlap(piscivoreMinPreySize, piscivoreMaxPreySize, speciesMinSize, speciesMaxSize)
						result_matrix[allRow, piscivoreRow] <- score
						#If the checkbox is checked, filter all scores that are higher or equal to the cutoff
						#only exception is 0, which will always be kept
						if(input$predationCutoffCheckbox){
							if (score<predationCutoffThreshold|score==0){
								listMatrix<-rbind(listMatrix,c(speciesNameAndDev,piscivoreNameAndDev,score))
							}
						}
						else{
							listMatrix<-rbind(listMatrix,c(speciesNameAndDev,piscivoreNameAndDev,score))
						}
					}
				}
			}
			
			predMatrixDataFrame=as.data.frame(result_matrix)
			predMatrixDataFrame <- cbind(AllSpecies_Piscivores = namesWithDevStageAllDataFrame[[1]], predMatrixDataFrame)
			predListDataFrame=as.data.frame(listMatrix)
			
			#output pred range
			write.xlsx(predRangeAllDataFrame, predRangeFilePath,colWidths = "auto")
			add_italics_excel(predRangeFilePath,FALSE,TRUE)
			infoMsg <- paste0(infoMsg,"Prediction of predation range data outputed in ",predRangeFilePath,"<br/>")

			#output list matrix
			write.xlsx(predListDataFrame, predListFilePath,colWidths = "auto")
			add_italics_excel_list(predListFilePath)
			infoMsg <- paste0(infoMsg,"Prediction of predation probabilties list outputed in ",predListFilePath,"<br/>")

			#output pred matrix
			write.xlsx(predMatrixDataFrame, predMatrixFilePath,colWidths = "auto")
			add_italics_excel(predMatrixFilePath,TRUE,TRUE)
			infoMsg <- paste0(infoMsg,"Prediction of predation probabilties matrix outputed in ",predMatrixFilePath,"<br/>")

			#the heatmap can't be produced without at least 2 rows and 2 columns, which means 2 pisicvores
			if(nbYesPiscivore!=1){
			  
				nbTotalSpecies=nrow(inputDataFrame)
				widthPdf=3+0.60*nbYesPiscivore
				heightPdf=3+0.25*nbTotalSpecies
				
				if(widthPdf<10){
					widthPdf=10
				}
				if(heightPdf<10){
					heightPdf=10
				}
			  
				#output heatmap
				#pdf(file=heatMapFilePath, width =10 , height =10)
				pdf(file=heatMapFilePath, width =widthPdf , height =heightPdf)
				#pdf(file=heatMapFilePath)
				#colorPalette <- colorRampPalette(brewer.pal(11, "RdYlGn"))(100)
				#Palette for color-blind persons
				colorPalette <- colorRampPalette(brewer.pal(11, "RdYlBu"))(100)
				inverted_result_matrix=1-result_matrix
				#without clustering
				#heatmap(inverted_result_matrix,main="Predation probabilty heatmap",xlab="Piscivores", Colv=NA, Rowv=NA, ylab="All species",result_matrix,margins = c(30, 25),col=colorPalette,scale="none")
				heatmap(inverted_result_matrix,main="Predation probabilty heatmap",xlab="Piscivores", Colv=NA, Rowv=NA, ylab="All species",result_matrix,margins = c(30, 25),col=colorPalette,scale="none", labCol=as.expression(lapply(colnames(inverted_result_matrix), function(a) bquote(italic(.(a))))), labRow=as.expression(lapply(rownames(inverted_result_matrix), function(a) bquote(italic(.(a))))) )

				legend(x = "bottomright", title="Predation risk",  legend = c("1","0.9","0.8","0.7","0.6","0.5","0.4","0.3","0.2","0.1","0"), cex = 0.8, fill = colorRampPalette(brewer.pal(11, "RdYlBu"))(11))					
				dev.off()
				infoMsg <- paste0(infoMsg,"Heatmap of predation probabilties outputed in ",heatMapFilePath,"<br/>")
			}
		}
		if (criticalErrorMsg!=""){
			infoMsg=paste0(infoMsg,"There are critical errors. Please fix them.<br/>CRITICAL ERRORS:<br/>",criticalErrorMsg)
		}
		if (warningsErrorMsg!=""){
			infoMsg=paste0(infoMsg,"<br/>WARNINGS:<br/>",warningsErrorMsg)
		}
		output$InfoText <- renderUI({HTML(infoMsg)})
	})
}


shinyApp(ui = ui, server = server)
