# Use to get indexes for the matrices to use as stimuli
trial_index_fun <- function(number_trials, dif_levels){
  #Takes two arguments and returns a shuffled vector of integers by sampling x elements (depending on number_trials) at random from intervals of 100 
  #with total vector length equal to dif_levels times number_trials 
    ##number_trials = an integer (must be less than 100) determining the amount of indexes to select and the total returned vector length
    ##dif_levels = an integer given by the number of dificulty levels (i.e. the number of proportions)
  
  #Array length (number of 3rd dimension elements)
  total_3dim <- dif_levels * 100
  
  #Intervals to sample from for each level of difficulty (it goes from 100 to 100)
  sampling_intervals <- c(seq(1,total_3dim,100), total_3dim)
  
  #Vector to store results from the for loop
  res <- c()
  
  #Iterate over each sampling interval to sample array indexes in the 3rd dimension
  for(i in 1:(length(sampling_intervals) - 1)){
    res <- append(
      res,
      sample(sampling_intervals[i]:sampling_intervals[i+1], number_trials)
    )
  }
  #Shuffle the indexes in the result vector to randomize stimulus order or appearance 
  sample(res, number_trials * dif_levels)
}



# use js script to detect device type
mobileDetect = function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}

loadScripts=function () {
  returnList = list()
  #global css
  returnList[["globalStyle"]] = includeCSS("www/flatly.css") 
  #specific css for response boxes
  returnList[["myStyle"]] = includeCSS("www/myStyle.css")
  # js file with all functions used to collect responses across trials
  # response keys, time-out deadline and message are set there.
  returnList[["trial"]] = includeScript("www/trial.js")
  
  returnList
}

makePageList=function (fileName = "Example",globId = fileName) 
{ 
  dir = fileName
  df = read.table(dir, header = TRUE, sep = "\t", encoding = "UTF-8",
                  stringsAsFactors = FALSE)
  
  choicesList = df$choices
  if (any(!is.na(df$choices))) {
    choicesList = strsplit(as.character(df$choices), split = ",", 
                           fixed = TRUE)
    choiceNames = strsplit(as.character(df$choiceNames), 
                           split = ",", fixed = TRUE)
    choiceNames = lapply(choiceNames, revArgs, pattern = "NA", 
                         replacement = NA)
    choicesList[!is.na(choicesList)] = lapply(choicesList[!is.na(choicesList)], 
                                              as.numeric)
    for (i in seq_along(df$id)) {
      if (any(is.na(choiceNames[[i]]))) {
        (next)()
      }
      else {
        names(choicesList[[i]]) = choiceNames[[i]]
      }
    }
  }
  df$placeholder[is.na(df$placeholder)] = ""
  df$inline[df$inline != 1] = FALSE
  df$inline[df$inline == 1] = TRUE
  #randomize
  df$page[df$randomize == 1] = sample(df$page[df$randomize ==1])
  
  id.order = df$id[order(df$page)]
  id.order = paste(id.order[!is.na(id.order)], collapse = ";")
  textOrQuestionnaireList = list(text = df$text, reverse = df$reverse, 
                                 choices = choicesList, page = df$page, type = df$type, 
                                 min = df$min, max = df$max, placeholder = df$placeholder, 
                                 id = paste0(globId, "_", df$id), globId = as.character(globId), 
                                 disabled = df$disabled, width = df$width, height = df$height, 
                                 inline = df$inline, checkType = df$checkType,
                                 id.order = id.order)
  
  ind = substr(textOrQuestionnaireList$id, start = nchar(textOrQuestionnaireList$id) - 
                 1, stop = nchar(textOrQuestionnaireList$id)) != "NA"
  textOrQuestionnaireList$obIds = textOrQuestionnaireList$id[ind]
  textOrQuestionnaireList
}

# helper for makePageList
revArgs = function(x, pattern, replacement){
  # reverse input arguments for use in apply functions
  gsub(pattern = pattern, replacement = replacement, x = x)
}

#Inicializa currVal (se llama una sola vez)
makeCtrlList= function (firstPage, globIds){
  
  #firstPage = string con el indice de la primer pagina para mostrar que es el primer valor que va a tomar currVal
  #globIds = vector de strings con los indices de las paginas que van a generarse
  
  nameVec = paste0(globIds, ".num") #Pega la string '.num' a cada string de la lista globIds 'ejemplo.num'
  ctrlList = reactiveValues(page = firstPage, proceed = 0) #Genera un objeto reactivo 
  for (nam in seq_along(nameVec)) { #Itera por la cantidad de elementos en nameVec
    ctrlList[[nameVec[nam]]] = 1 } #En cada iteracion asigna un 1 a un nuevo elemento de la variable ctrlList que lleva el nombre de un elemento en nameVec
  #trial number count
  ctrlList[["trngTrial"]] = 1 #Genera un nuevo elemento de la variable ctrlList para almacenar los trials de practica
  ctrlList[["expTrial"]] = 1 #Genera un nuevo elemento de la variable ctrlList para almacenar los trials de experimento
  ctrlList #Devuelve un objeto reactivo como una lista con los elementos generados anteriormente
}

nextPage=function (pageId, ctrlVals, nextPageId, pageList, globId)
{
  if (ctrlVals$page == pageId) {
    tempIndex = paste0(globId, ".num")
    ctrlVals[[tempIndex]] = ctrlVals[[tempIndex]] + ctrlVals$proceed
    ctrlVals$proceed = 0
    if (ctrlVals[[tempIndex]] > max(pageList$page, na.rm = TRUE)) {
      ctrlVals$page = nextPageId
    }
  }
}


onInputEnable=function (pageId, ctrlVals, pageList, globId, inputList, charNum = NULL) 
{
  if (ctrlVals$page == pageId) {
    checkInput = pageList$id[pageList$page == ctrlVals[[paste0(globId,".num")]] 
                             & pageList$id %in% pageList$obIds 
                             &pageList$disabled == 1]
    checkTypeTemp = pageList$checkType[pageList$page == 
                                         ctrlVals[[paste0(globId, ".num")]] 
                                       & pageList$id %in%pageList$obIds 
                                       & pageList$disabled == 1]
    if (length(checkTypeTemp > 0) && any(!is.na(checkTypeTemp))) {
      if (mean(unlist(lapply(seq_along(checkInput), checkInputFn, 
                             inList = inputList, checkType = checkTypeTemp, 
                             charNum = charNum, checkInput = checkInput)), 
               na.rm = TRUE) == 1) {
        shinyjs::enable(paste0(globId, "_next"))
      }
    }
  }
}

# helper for onInputEnable
checkInputFn = function(Index, inList, checkType, charNum, checkInput){
  
  if (checkType[Index] == "isTRUE"){
    
    checkTemp = !is.null(inList[[checkInput[Index]]]) &&
      isTRUE(inList[[checkInput[Index]]])
    
  } else if (checkType[Index] == "is.null"){
    
    checkTemp = !is.null(inList[[checkInput[Index]]])
  } else if (checkType[Index] == "is.num"){
    
    checkTemp = !is.null(inList[[checkInput[Index]]]) &&
      is.numeric(inList[[checkInput[Index]]])
    
  } else if (checkType[Index] == "nchar"){
    
    checkTemp = !is.null(inList[[checkInput[Index]]]) &&
      nchar(inList[[checkInput[Index]]]) >= charNum
    
  } else {
    
    stop(paste(checkType[Index],
               "is no valid checkType. Use one of \"isTRUE\", \"is.null\", \"nchar\""))
  }
  checkTemp
}



appendTrngValues=function (ctrlVals, input, trngData, container,
                           afterTrialPage = "ConfTrng") {
  trngData$time = c(trngData$time, input$respTime[length(input$respTime)])
  trngData$resp = c(trngData$resp, input$selected[length(input$selected)])
    ctrlVals$page = afterTrialPage
    ctrlVals$trngTrial = ctrlVals$trngTrial + 1
}

appendConfTrngValues=function (ctrlVals, input, trngConf, container,
                           afterTrialPage = "Trng", afterLastTrialPage = "endTrng") {
  trngConf$time = c(trngConf$time, input$respTime[length(input$respTime)])
  trngConf$resp = c(trngConf$resp, input$selected[length(input$selected)])
  ctrlVals$page = afterTrialPage
  # if last trial  
  if(ctrlVals$trngTrial==nrow(container)) {
    ctrlVals$page = afterLastTrialPage
  }
}


appendExpValues=function (ctrlVals, input, expData, container, afterTrialPage = "ConfExp") {
  expData$time = c(expData$time, input$respTime[length(input$respTime)])
  expData$resp = c(expData$resp, input$selected[length(input$selected)])
  # update num trial 
    ctrlVals$page = afterTrialPage
    
}

appendExpConfValues=function (ctrlVals, input, expConf, container,
                          afterTrialPage = "ITIexp", afterLastTrialPage = "Demog") {
  expConf$time = c(expConf$time, input$respTime[length(input$respTime)])
  expConf$resp = c(expConf$resp, input$selected[length(input$selected)])
  ctrlVals$page = afterTrialPage
  ctrlVals$expTrial = ctrlVals$expTrial + 1
  # if last trial
  if(ctrlVals$expTrial==nrow(container)) { 
    ctrlVals$page = afterLastTrialPage
  }
}

saveData= function (data, partId, checkNull = TRUE, 
                    suffix = "_s", outputDir = NULL) 
{
  if (checkNull) {
    data.new = lapply(data, convertNull)
  }
  data.df = as.data.frame(data.new)
  incProgress(0.5)
  idu=round(as.numeric(Sys.time()) )
  
  DatafileName = paste0(partId, "_" ,idu, 
                        suffix, ".csv")
  DatafilePath = file.path(outputDir, DatafileName)
  write.table(data.df, DatafilePath, row.names = FALSE, 
              quote = TRUE, sep = ",")
}
# helper for saveData function
convertNull = function(x){
  if (is.null(x)){
    val = NA
  } else if (length(x) == 0){
    val = NA
  } else {
    val = x
  }
  val
}


# js fn to detect OS & browser type/version
browserdet="'use strict';

var module = {
options: [],
header: [navigator.platform, navigator.userAgent, navigator.appVersion, navigator.vendor, window.opera],
dataos: [
{ name: 'Windows Phone', value: 'Windows Phone', version: 'OS' },
{ name: 'Windows', value: 'Win', version: 'NT' },
{ name: 'iPhone', value: 'iPhone', version: 'OS' },
{ name: 'iPad', value: 'iPad', version: 'OS' },
{ name: 'Kindle', value: 'Silk', version: 'Silk' },
{ name: 'Android', value: 'Android', version: 'Android' },
{ name: 'PlayBook', value: 'PlayBook', version: 'OS' },
{ name: 'BlackBerry', value: 'BlackBerry', version: '/' },
{ name: 'Macintosh', value: 'Mac', version: 'OS X' },
{ name: 'Linux', value: 'Linux', version: 'rv' },
{ name: 'Palm', value: 'Palm', version: 'PalmOS' }
],
databrowser: [
{ name: 'Chrome', value: 'Chrome', version: 'Chrome' },
{ name: 'Firefox', value: 'Firefox', version: 'Firefox' },
{ name: 'Safari', value: 'Safari', version: 'Version' },
{ name: 'Internet Explorer', value: 'MSIE', version: 'MSIE' },
{ name: 'Opera', value: 'Opera', version: 'Opera' },
{ name: 'BlackBerry', value: 'CLDC', version: 'CLDC' },
{ name: 'Mozilla', value: 'Mozilla', version: 'Mozilla' }
],
init: function () {
var agent = this.header.join(' '),
os = this.matchItem(agent, this.dataos),
browser = this.matchItem(agent, this.databrowser);

return { os: os, browser: browser };
},
matchItem: function (string, data) {
var i = 0,
j = 0,
html = '',
regex,
regexv,
match,
matches,
version;

for (i = 0; i < data.length; i += 1) {
regex = new RegExp(data[i].value, 'i');
match = regex.test(string);
if (match) {
regexv = new RegExp(data[i].version + '[- /:;]([\\d._]+)', 'i');
matches = string.match(regexv);
version = '';
if (matches) { if (matches[1]) { matches = matches[1]; } }
if (matches) {
matches = matches.split(/[._]+/);
for (j = 0; j < matches.length; j += 1) {
if (j === 0) {
version += matches[j] + '.';
} else {
version += matches[j];
}
}
} else {
version = '0';
}
return {
name: data[i].name,
version: parseFloat(version)
};
}
}
return { name: 'unknown', version: 0 };
}
};

var e = module.init(),
debug = '';

debug += 'os.name = ' + e.os.name + '<br/>';
debug += 'os.version = ' + e.os.version + '<br/>';
debug += 'browser.name = ' + e.browser.name + '<br/>';
debug += 'browser.version = ' + e.browser.version + '<br/>';

debug += '<br/>';
debug += 'navigator.userAgent = ' + navigator.userAgent + '<br/>';
debug += 'navigator.appVersion = ' + navigator.appVersion + '<br/>';
debug += 'navigator.platform = ' + navigator.platform + '<br/>';
debug += 'navigator.vendor = ' + navigator.vendor + '<br/>';


Shiny.onInputChange('osbrow',e);"