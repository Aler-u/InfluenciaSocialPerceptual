# functions for all page types

# simple questionnaire/text page ----
makePage=function (pageList, pageNumber, globId, ctrlVals, continueButton = TRUE) 
{  
  index = which(.subset2(pageList, "page") %in% c(0,pageNumber))
  thisPage = lapply(index, callTag, pageList = pageList)
  thisPage = list (thisPage,tag("script", "document.body.style.cursor = 'default';")) # show pointer
  
  if (isTRUE(continueButton)) {
    if (any(.subset2(pageList, "disabled")[index] ==1)) {
      thisPage = list(
        thisPage, 
        br(), 
        shinyjs::disabled(
          actionButton(
            inputId =  paste0(globId,"_next"),
            label = "Continuar",
            class = "btn-primary"
          )),
        br(),
        br()  
      ) 
    }else {
      thisPage = list(
        thisPage,
        br(),
        actionButton(
          inputId =  paste0(globId,"_next"),
          label = "Continuar",
          class = "btn-primary"),
        br(),
        br()
      )
    }
  }else {  thisPage = list(thisPage)  }
  ctrlVals$proceed = 1
  thisPage
}

# helper for page parsing
callTag = function(index, pageList){
  # identify tag and call it with the appropriate arguments
  # define lists of possible inputs
  tagList = c("h1", "h2", "h3", "h4", "h5", "h6", "p")
  multiList = c("checkboxGroupInput", "radioButtons")
  textList = c("passwordInput", "textInput")
  nusliList = c("sliderInput", "numericInput")
  
  # check which function is matched to ensure correct use of arguments
  if (any(tagList == pageList$type[index])){
    if (substr(pageList$id[index], nchar(pageList$id[index]) - 1,
               nchar(pageList$id[index])) == "NA"){
      tempid = NULL
    } else {
      tempid = pageList$id[index]
    }
    
    # prints text, such as headers and paragraphs
    getExportedValue("shiny", pageList$type[index])(pageList$text[index],
                                                    width = pageList$width[index],
                                                    id = tempid)
    
  } else if (any(multiList == pageList$type[index])){
    # creates input objects such as radio buttons and multi check boxes.
    getExportedValue("shiny", pageList$type[index])(inputId = pageList$id[index],
                                                    choices = pageList$choices[[index]],
                                                    label = pageList$text[index],
                                                    selected = character(0),
                                                    width = pageList$width[index],
                                                    inline = pageList$inline[index])
    
  } else if (any(textList == pageList$type[index])){
    # creates input objects such as password or text input.
    getExportedValue("shiny", pageList$type[index])(pageList$id[index],
                                                    label = pageList$text[index],
                                                    placeholder = pageList$placeholder[index],
                                                    width = pageList$width[index])
    
  } else if (any(nusliList == pageList$type[index])){
    # creates input objects such as numeric input or slider
    getExportedValue("shiny", pageList$type[index])(pageList$id[index],
                                                    label = pageList$text[index],
                                                    min = pageList$min[index],
                                                    max = pageList$max[index],
                                                    value = pageList$choices[index],
                                                    width = pageList$width[index])
    
  } else if (pageList$type[index] == "img"){
    # post an image from a given source
    if (isTRUE(pageList$defaultList)){
      addResourcePath('pictures', system.file('extdata', package='ShinyPsych'))
    }
    if (substr(pageList$id[index], nchar(pageList$id[index]) - 1,
               nchar(pageList$id[index])) == "NA"){
      tempid = NULL
    } else {
      tempid = pageList$id[index]
    }
    getExportedValue("shiny", pageList$type[index])(src = pageList$text[index],
                                                    width = pageList$width[index],
                                                    height = pageList$height[index],
                                                    id = tempid)
    
  }else if (pageList$type[index] == "HTML"){
    
    # this is apropriate if the text is actually written html code
    HTML(pageList$text[index])
    
  } else if (pageList$type[index] == "checkboxInput"){
    # creates a checkbox that yields FALSE if unchecked and TRUE if checked
    getExportedValue("shiny", pageList$type[index])(pageList$id[index],
                                                    label = pageList$text[index],
                                                    width = pageList$width[index])
    
  } else if (pageList$type[index] == "selectInput"){
    # creates a dropdown list from which an input can be selected
    getExportedValue("shiny", pageList$type[index])(inputId = pageList$id[index],
                                                    choices = pageList$choices[[index]],
                                                    label = pageList$text[index],
                                                    selected = character(0),
                                                    width = pageList$width[index],
                                                    multiple = pageList$inline[index])
    
  } else if (pageList$type[index] == "textAreaInput"){
    getExportedValue("shiny", pageList$type[index])(pageList$id[index],
                                                    label = pageList$text[index],
                                                    placeholder = pageList$placeholder[index],
                                                    width = pageList$width[index],
                                                    height = pageList$height[index])
    
  } else if (pageList$type[index] == "dateInput"){
    val_temp = if(is.na(pageList$placeholder[index])) NULL else pageList$placeholder[index]
    min_temp = if(is.na(pageList$min[index])) NULL else pageList$min[index]
    max_temp = if(is.na(pageList$max[index])) NULL else pageList$max[index]
    time_int = c("month", "year", "decade")
    startview_temp = if(is.na(pageList$choices[index])){
      "month"
    } else {
      time_int[pageList$choices[[index]]]
    }
    getExportedValue("shiny", pageList$type[index])(pageList$id[index],
                                                    label = pageList$text[index],
                                                    value = val_temp,
                                                    width = pageList$width[index],
                                                    min = min_temp,
                                                    max = max_temp,
                                                    startview = startview_temp)
    
  } else {
    # give exact value that raised the error
    stop("Couldn't identify function. See documentation for valid inputs. Note that spelling must match shiny functions!")
  }
}


# InterTrialInterval (ITI) page ----
itiPage=function (ctrlVals, trials) {
  # device type (determines some visual params)
  mobi = ctrlVals$mobile 
  # training or exp
  if (ctrlVals$page=="ITItrng"){ 
    dInd = ctrlVals$trngTrial
    tipoITI=1
  } else{
    dInd = ctrlVals$expTrial
    tipoITI=2
  }
  if (dInd==1){
    # for 1st trial give plenty of time to put hands on keyboard
    ITI=1200 
  }else{
    ITI=trials$ITI[dInd]
  }
  # HTMLize js fn to wait the ITI (in ms)
  jsc=fixedRow(tag("script", paste0("waiter(",dInd,",",ITI,",",tipoITI,");") ) )  
  
  # ajusto par visuales segun dispositivo
  if (mobi==T){
    wd= "75px"
  }else{
    wd="100px"
  }
  list(fixedRow(br()),
       fixedRow(column(12, 
                                     align = "center",
                                     # fixation cross
                                     h2(style = paste0("font-size:",wd,"; text-align:center;"),"+") ) 
       ),
       br(), 
       br(),
       jsc  
  )
}

# training trial page (almost identical to exp page, intended to allow different trial logic)----

trngPage=function (ctrlVals, trials) {
  dInd = ctrlVals$trngTrial
  mobi = ctrlVals$mobile 
  
  # adjust visual pars according to device type
  if (mobi==T){
    estilo="width: 100%"
    wd= "150px"
    caj="box2"
    op="options2"
    
    # set HTML/css for left and right boxes and HTMLize js functions that are called onClick (i.e. phone tap)
    cajita1=paste0("<div class = \"",
                   caj,"\"><div class = \"",
                   op,"\" id=\"opt1\"\n onmousedown=\"onOptionClick('opt1', 1, t, respTime,
                   clickEnabled,\n trialNr,tipo);\"><table>","NARANJA", "</table></div></div>")
    cajita2=paste0("<div class = \"",
                   caj,"\"><div class = \"",
                   op,"\" id=\"opt3\"\n onmousedown=\"onOptionClick('opt3', 3, t, respTime,
                   clickEnabled,\n trialNr,tipo);\"><table>","AZUL", "</table></div></div>")
    
  }else{ 
    estilo="width: 700px"
    wd="300px"
    caj="box"
    op="options"
    
    cajita1=paste0("<div class = \"",
                   caj,"\"><div class = \"",
                   op,"\" id=\"opt1\"\n \"><table>","NARANJA", "</table></div></div>")
    cajita2=paste0("<div class = \"",
                   caj,"\"><div class = \"",
                   op,"\" id=\"opt3\"\n \"><table>","AZUL", "</table></div></div>")
  }
  
  list(
    fixedRow(column(12, 
                    plotOutput('colors_matrix')
                    ) 
    ),
    br(), 
    br(),
    fixedRow(column(12,align = "center",
                                  # remove double-tap delay
                                  HTML('<meta name="viewport" content= "width=device-width, user-scalable=no">'),
                                  # initialize js vars for this trial
                                  tag("script", paste0("var TO \n TRIALpr(",dInd,");")),
                                  # listen to keyboard for responses in js
                                  tag("script", "document.onkeydown;"),
                                  column(2, h1("")),
                                  # call js fns for both boxes
                                  column(5, HTML(cajita1)) ,
                                  column(5, HTML(cajita2)) ,
                                  column(2, h1(""))
    )
    )
  ) 
}

trngConfPage=function (ctrlVals) {
  dInd = ctrlVals$trngTrial
  mobi = ctrlVals$mobile 
  
  # adjust visual pars according to device type
  if (mobi==T){
    estilo="width: 100%"
    wd= "50px"
    caj="box2"
    op="options2"
    
    # set HTML/css for left and right boxes and HTMLize js functions that are called onClick (i.e. phone tap)
    cajita1=paste0("<div class = \"",
                   caj,"\"><div class = \"",
                   op,"\" id=\"opt1\"\n onmousedown=\"onOptionClick('opt1', 1, t, respTime,
                   clickEnabled,\n trialNr,tipo);\"><table>","BAJA", "</table></div></div>")
    cajita2=paste0("<div class = \"",
                   caj,"\"><div class = \"",
                   op,"\" id=\"opt2\"\n onmousedown=\"onOptionClick('opt2', 2, t, respTime,
                   clickEnabled,\n trialNr,tipo);\"><table>","MEDIA", "</table></div></div>")
    cajita3=paste0("<div class = \"",
                   caj,"\"><div class = \"",
                   op,"\" id=\"opt3\"\n onmousedown=\"onOptionClick('opt3', 3, t, respTime,
                   clickEnabled,\n trialNr,tipo);\"><table>","ALTA", "</table></div></div>")
    
  }else{ 
    estilo="width: 700px"
    wd="75px"
    caj="box"
    op="options"
    
    cajita1=paste0("<div class = \"",
                   caj,"\"><div class = \"",
                   op,"\" id=\"opt1\"\n \"><table>","BAJA", "</table></div></div>")
    cajita2=paste0("<div class = \"",
                   caj,"\"><div class = \"",
                   op,"\" id=\"opt2\"\n \"><table>","MEDIA", "</table></div></div>")
    cajita3=paste0("<div class = \"",
                   caj,"\"><div class = \"",
                   op,"\" id=\"opt3\"\n \"><table>","ALTA", "</table></div></div>")
  }
  
  list(
    fixedRow(
      column(12, 
             align = "center",
             #show number
             style = paste0("font-size:",wd,"; text-align:center;"),
                'Indica que tanta confianza tenes en tu respuesta') 
    ),
    br(), 
    br(),
    fixedRow(column(12, align = "center",
                    # remove double-tap delay
                    HTML('<meta name="viewport" content= "width=device-width, user-scalable=no">'),
                    # initialize js vars for this trial
                    tag("script", paste0("var TO \n TRIALprConf(",dInd,");")),
                    # listen to keyboard for responses in js
                    tag("script", "document.onkeydown_conf;"),
                    #column(4, h1("")),
                    # call js fns for both boxes
                    column(4, HTML(cajita1)) ,
                    column(4, HTML(cajita2)) ,
                    column(4, HTML(cajita3)) ,
                    #column(2, h1(""))
    )
    )
  ) 
}

# experimental trial page ----
expPage=function (ctrlVals, trials) {
  dInd = ctrlVals$expTrial
  mobi = ctrlVals$mobile 
  
  if (mobi==T){
    estilo="width: 100%"
    wd= "150px"
    caj="box2"
    op="options2"
    
    cajita1=paste0("<div class = \"",
                   caj,"\"><div class = \"",
                   op,"\" id=\"opt1\"\n onmousedown=\"onOptionClick('opt1', 1, t, respTime,
                   clickEnabled,\n trialNr,tipo)\"><table>","NARANJA", "</table></div></div>")
    cajita2=paste0("<div class = \"",
                   caj,"\"><div class = \"",
                   op,"\" id=\"opt3\"\n onmousedown=\"onOptionClick('opt3', 3, t, respTime,
                   clickEnabled,\n trialNr,tipo)\"><table>","AZUL", "</table></div></div>")
  }else{
    estilo="width: 500px"
    wd="220px"
    caj="box"
    op="options"
    
    cajita1=paste0("<div class = \"",
                   caj,"\"><div class = \"",
                   op,"\" id=\"opt1\"\n \"><table>","NARANJA", "</table></div></div>")
    cajita2=paste0("<div class = \"",
                   caj,"\"><div class = \"",
                   op,"\" id=\"opt3\"\n \"><table>","AZUL", "</table></div></div>")
  }
  
  list(
    fixedRow(
      column(12, 
             plotOutput('colors_matrix') 
             ) 
    ),
    br(), 
    br(),
    fixedRow(
      column(12,align = "center",
                    HTML('<meta name="viewport" content= "width=device-width, user-scalable=no">'),
                    tag("script", paste0("var TO \n TRIAL(",dInd,");")),
                    tag("script", "document.onkeydown;"),
                    column(2, h1("")),
                    column(5, HTML(cajita1)) ,
                    column(5, HTML(cajita2)) ,
                    column(2, h1("")) 
      )
    )
  ) 
}

expConfPage=function (ctrlVals) {
  dInd = ctrlVals$expTrial
  mobi = ctrlVals$mobile 
  
  if (mobi==T){
    estilo="width: 100%"
    wd= "50px"
    caj="box2"
    op="options2"
    
    cajita1=paste0("<div class = \"",
                   caj,"\"><div class = \"",
                   op,"\" id=\"opt1\"\n onmousedown=\"onOptionClick('opt1', 1, t, respTime,
                   clickEnabled,\n trialNr,tipo)\"><table>","BAJA", "</table></div></div>")
    cajita2=paste0("<div class = \"",
                   caj,"\"><div class = \"",
                   op,"\" id=\"opt2\"\n onmousedown=\"onOptionClick('opt2', 2, t, respTime,
                   clickEnabled,\n trialNr,tipo)\"><table>","MEDIA", "</table></div></div>")
    cajita3=paste0("<div class = \"",
                   caj,"\"><div class = \"",
                   op,"\" id=\"opt2\"\n onmousedown=\"onOptionClick('opt3', 3, t, respTime,
                   clickEnabled,\n trialNr,tipo)\"><table>","ALTA", "</table></div></div>")
  }else{
    estilo="width: 500px"
    wd="75px"
    caj="box"
    op="options"
    
    cajita1=paste0("<div class = \"",
                   caj,"\"><div class = \"",
                   op,"\" id=\"opt1\"\n \"><table>","BAJA", "</table></div></div>")
    cajita2=paste0("<div class = \"",
                   caj,"\"><div class = \"",
                   op,"\" id=\"opt2\"\n \"><table>","MEDIA", "</table></div></div>")
    cajita3=paste0("<div class = \"",
                   caj,"\"><div class = \"",
                   op,"\" id=\"opt3\"\n \"><table>","ALTA", "</table></div></div>")
  }
  
  list(
    fixedRow(
      column(12, 
             align = "center",
             #show number
             h3(style = paste0("font-size:",wd,"; text-align:center;"),
                "Indica que tanta confianza tenes en tu respuesta") ) 
    ),
    br(), 
    br(),
    fixedRow(
      column(12,align = "center",
             HTML('<meta name="viewport" content= "width=device-width, user-scalable=no">'),
             tag("script", paste0("var TO \n TRIALConf(",dInd,");")),
             tag("script", "document.onkeydown;"),
             #column(4, h1("")),
             column(4, HTML(cajita1)) ,
             column(4, HTML(cajita2)) ,
             column(4, HTML(cajita3)) ,
             #column(2, h1("")) 
      )
    )
  ) 
}

# goodbye (debrief) page ----
gbPage=function (pageList , pageNumber, globId = "Goodbye", ctrlVals = CurrentValues) {
  index = which(.subset2(pageList, "page") %in% c(0,pageNumber))
  thisPage = lapply(index, callTag, pageList = pageList)
  thisPage = list(
    tags$div( tags$h2(
      # show score
      HTML(paste0("Tu puntaje fue: ",
                  tags$span(style="color:blue", as.character(ctrlVals$score) ),
                  ' de ',2*ctrlVals$expTrial,'.')) )),
    tags$hr(),
    thisPage, 
    br()
    )
}