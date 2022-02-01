# This app is *largely* based on the ShinyPsych package [cite]
# Nonetheless, this package does not have a strong focus on RT tasks.
# Here, we adapted some of its functionality and aesthetics for a numerical comparison task based on Dehaene, 1990, JEP:HPP

# Load pkgs, fns and num lists ====================================================
library(shiny);library(shinyjs);library(shinyWidgets)

# source pipeline and page layout fns
source("helper_fns_v2.R"); source("page_fns_v2.R")

# load training stimulus
# training stimulus consists of a 3d array with dimensions 100x100xN where the 3rd dimensions (N) corresponds to the number of training trials

training_stimuli <-  readRDS('training_stimulus.rds')

# sets the number of epxerimental trials you want to use to test the app (must be bigger than 4)
# The number ntr will get multiplied by p_levels (5) to account for the five different levels of difficulty
ntr <-  5
p_levels <- 5

# set save dir (locally). You should have 4 subdirs within the save_dir: sessions, trng, exp & demog
save_dir=paste0(getwd(),"/data/")

# Load app pages' lists ====================================================
lists_dir="www/lists/"

# translate simple pages to HTML
instru.list = makePageList(fileName = paste0(lists_dir,"Instructions.txt"), globId = "Instructions")
consent.list =makePageList(fileName = paste0(lists_dir,"Consent.txt"), globId = "Consent")
endTrng.list = makePageList(fileName = paste0(lists_dir,"EndTraining.txt"), globId = "endTrng")
demog.list =  makePageList(fileName = paste0(lists_dir,"Demographics.txt"),  globId = "Demog")
goodbye.list =makePageList(fileName = paste0(lists_dir,"Goodbye.txt"),  globId = "Goodbye")

# Set pages IDs
pageIDs = c("Instructions",
            "Consent",
            "ITItrng",
            "ITIexp",
            "endTrng",
            "Trng",
            "Exp",
            "ConfTrng",
            "ConfExp",
            "Demog",
            "Goodbye")

# Shiny UI ====================================================
ui = fixedPage(
  # App title (appears on browser window tag)
  title = "Percepcion Visual",      
  # progress bar throughout the whole app-flow
  progressBar(id = "pb1", value = 0, display_pct = FALSE,status = "primary"), 
  # for Shinyjs functions
  useShinyjs(),
  # include custom css and js scripts
  loadScripts(), 
  # to detect the device type (might be redundant with os/browser data)
  mobileDetect('isMobile'),
  # render reactive HTML from server
  uiOutput("MainAction")
)

# Shiny server ====================================================
server = function(input, output, session) {
  # get os and browser data
  shinyjs::runjs(browserdet) 
  
  
  # everything (HTML) is output in "MainAction"
  output$MainAction = renderUI( {
    PageLayouts()
  })
  
  # pick a random pre-ordered number-list and set ITIs
  set.seed(round( as.numeric(Sys.time() ) ) )
  
  # set trng and exp ITIs and color truth
  trng_trials=data.frame("ITI"=runif(4,.7,1)*1000)
  
  exp_trials=data.frame("ITI"= runif(ntr * p_levels,.7,1)*1000,
                        'color_truth' = sample(c('orange','blue'), ntr * p_levels, replace = TRUE),
                        'trial_matrix' = trial_index_fun(ntr, p_levels),
                        stringsAsFactors = FALSE
                        )
  #load a subset of experimental stimulus given by the trial_matrix column (thus the experimental stimulus is loaded after trial_matrix has been defined)
  ## experimental stimulus consists of a named list, each element representing a different proportion of 0s and 1s (see stimulus_construcion.R for details)
  experimental_array <- readRDS('experimental_stimulus.rds')[,,exp_trials$trial_matrix]
  
  #Generate the first
  output$colors_matrix <- renderPlot({
    heatmap(new_stim_list[,,1], Rowv = NA, Colv = NA, labRow = '', labCol = '', col = c('orange','blue'))
  })
  
  # set a 3-letter random workerID
  wid =  paste0(as.numeric(Sys.time()), 
               paste(sample(letters,3),collapse = '') 
  )
  
  # Reactives to store data during game
  trngData = reactiveValues(time = c(), resp=c() )
  expData  = reactiveValues(time = c(), resp=c() )
  trngConf = reactiveValues(time = c(), resp=c() )
  expConf = reactiveValues(time = c(), resp=c() )
  
  # inicializo conteos de pags
  currVal = makeCtrlList(firstPage = "Instructions",globIds = pageIDs)  
  # HTML page layouts
  PageLayouts = reactive({
    #intro
    if (currVal$page == "Instructions") {  
        return(makePage(pageList = instru.list, pageNumber = currVal$Instructions.num,
                            globId = "Instructions", ctrlVals = currVal))   }
    if (currVal$page == "Consent"){
      return(makePage(pageList = consent.list, pageNumber = currVal$Consent.num,
                    globId = "Consent", ctrlVals = currVal)  )}
    
    #trng
    if (currVal$page == "ITItrng") {
      return(itiPage(ctrlVals = currVal, trng_trials))}
    
    if(currVal$page == 'ConfTrng'){
      return(trngConfPage(ctrlVals = currVal))
    }
    
    if (currVal$page == "Trng") {
      output$colors_matrix <- renderPlot({
        heatmap(training_stimuli[,,currVal$trngTrial], Rowv = NA, Colv = NA, labRow = '', labCol = '', col = c('orange','blue'))
      })
      return(trngPage(ctrlVals = currVal, trials = trng_stim_list))}
    
    # fin prac
    if (currVal$page == "endTrng") {
      return( makePage(pageList = endTrng.list, pageNumber = currVal$endTrng.num,
                          globId = "endTrng", ctrlVals = currVal)) }
    
    #exp
    if (currVal$page == "ITIexp") {
      return(itiPage(ctrlVals = currVal, exp_trials))}
    
    if (currVal$page == 'ConfExp'){
      return(expConfPage(ctrlVals = currVal))
    }
    
    if (currVal$page == "Exp") {
      output$colors_matrix <- renderPlot({
        heatmap(
          #Matrix of data for stimulus 
          experimental_array[,#all first dimension rows
                             ,#all second dimension columns
                             currVal$expTrial #3rd dimension according to index in exp_trials dataframe
                             ], 
                #Parameters to devoid plot from annotations, etc.
                Rowv = NA, Colv = NA, labRow = '', labCol = '', 
                col = c( #Color determination according to ground truth
                  ifelse(exp_trials$color_truth[currVal$expTrial] == 'orange', 'blue', 'orange'),
                  exp_trials$color_truth[currVal$expTrial]
                  )
              )
        #(first color is 0 and second color is 1, ground truth is always 1)
      })
      return(expPage(ctrlVals = currVal, exp_trials))}
    

    #outro
    if (currVal$page == "Demog"){
      return(makePage(pageList = demog.list, pageNumber = currVal$Demog.num,
                    globId = "Demog", ctrlVals = currVal))}
    
    if (currVal$page == "Goodbye") {
      return(gbPage(pageList = goodbye.list, pageNumber = currVal$Goodbye.num,
                    globId = "Goodbye", ctrlVals = currVal))}
    
    if (currVal$page=="end"){
      shiny::stopApp()}
  })
  
  # Observe events ======================================
  observeEvent(input[["isMobile"]],{currVal$mobile=input$isMobile}) # get device type
  
  # Page Navigation ----------------------
  observeEvent(input[["Instructions_next"]],{
      nextPage(pageId = "Instructions", ctrlVals = currVal, nextPageId ="Consent" ,
                pageList = instru.list, globId = "Instructions")
  })
  
  observeEvent(input[["Consent_next"]],{
    shinyjs::runjs("document.body.style.cursor = 'none';") # no muestro mouse
    
    nextPage(pageId = "Consent", ctrlVals = currVal, nextPageId ="ITItrng" ,
              pageList = consent.list, globId = "Consent")
  })
  
  # TRNG ITI page complete
  observeEvent(input[["waitTrng"]], { currVal$page="Trng" })
  
  # TRNG trial page complete
  observeEvent(input[["trialNrpr"]], {
    updateProgressBar(session = session,id = "pb1", value = 100*(currVal$trngTrial/nrow(trng_trials) ) )
    appendTrngValues(ctrlVals = currVal, container = trng_trials,
                    input = input, trngData = trngData,
                    afterTrialPage = "ConfTrng")
    
  })
  
  # CONF TRNG trial page complete
  observeEvent(input[["trialConfNrpr"]], {
    updateProgressBar(session = session,id = "pb1", value = 100*(currVal$trngTrial/nrow(trng_trials) ) )
    appendConfTrngValues(ctrlVals = currVal, container = trng_trials,
                     input = input, trngConf = trngConf,
                     afterTrialPage = "ITItrng")
    
  })
  
  # last TRNG trial page complete
  observeEvent(input[["endTrng_next"]],{
    updateProgressBar(session = session,id = "pb1", value = 0 )
    shinyjs::runjs("document.body.style.cursor = 'none';") # no muestro mouse
    
    nextPage(pageId = "endTrng", ctrlVals = currVal, nextPageId ="ITIexp" ,
              pageList = endTrng.list, globId = "endTrng")
  })
  
  
  # EXP ITI page complete
  observeEvent(input[["waitExp"]], { currVal$page="Exp"
  output$colors_matrix <- renderPlot({
    heatmap(new_stim_list[,,currVal$expTrial], Rowv = NA, Colv = NA, labRow = '', labCol = '', col = c('orange','blue'))
  })})
  # EXP trial page complete
  observeEvent(input[["trialExpNr"]], {
      updateProgressBar(session = session,id = "pb1", value = 100*(currVal$expTrial/nrow(exp_trials) ) )
      appendExpValues(ctrlVals = currVal, container = exp_trials, 
                      input = input, expData = expData,
                      afterTrialPage = "ConfExp")
      
  }) 
  observeEvent(input[["trialConfNr"]], {
    if(currVal$expTrial<nrow(exp_trials)){ 
    updateProgressBar(session = session,id = "pb1", value = 100*(currVal$expTrial/nrow(exp_trials) ) )
    appendExpConfValues(ctrlVals = currVal, container = exp_trials,
                         input = input, expConf = expConf,
                         afterTrialPage = "ITIexp")
    }
    
    # if last trial
    else if(currVal$expTrial==nrow(exp_trials)) {
      updateProgressBar(session = session,id = "pb1", value = 100*(currVal$expTrial/nrow(exp_trials) ) )
      appendExpConfValues(ctrlVals = currVal, container = exp_trials, 
                      input = input, expConf = expConf,
                      afterTrialPage = "ITIexp", afterLastTrialPage = "Demog")
      # save all data to local dir
      withProgress(message = "Guardando...", value = 0, {
        incProgress(.25)
        
        data.trng = list(  
          "id" = wid,
          "mobile"= input$isMobile,
          
          "resp" = trngData$resp, # 1 es MEnor, 2 es Mayor
          "RT" = trngData$time # 
        )
        
        data.exp = list(  
          "id" = wid,
          "mobile"= input$isMobile,
          
          "resp" = expData$resp, # 1 es Menor, 2 es Mayor
          'conf_resp' = expConf$resp, #Confidence answer
          
          "RT" = expData$time, # Reaction Time
          'conf_RT' = expConf$time, #Confidence reaction time
          
          "num" = exp_trials$trial_matrix, # indice de la matrix usada 
          "truth" = exp_trials$color_truth, #color ground truth (ie. correct answer)
          "ITI" = exp_trials$ITI #Iti
        )
        data.session=list(reactiveValuesToList(session$clientData),input$osbrow)
        
       # Uncomment this to save data
        saveData(data.session,
                  outputDir= paste0(save_dir,"sessions"),
                  partId = data.exp$id, suffix = "_s")

        saveData(data.trng,
                  outputDir= paste0(save_dir,"trng"),
                  partId = data.exp$id, suffix = "_t")

       saveData(data.exp,
                 outputDir= paste0(save_dir,"exp"),
                 partId = data.exp$id, suffix = "_x")
      })}
    
  })

    
  
  # Save demog data ===================================
  # Demog page complete
  observeEvent(input[["Demog_next"]], {(
      
    withProgress(message = "Guardando...", value = 0, {
        incProgress(.25)
        data.demo=list(
          "id" = wid,
          "edu" = input$Demog_edu,
          "age" = input$Demog_age,
          "sex" = input$Demog_sex
          )
        # Uncomment this to save data
        saveData(data.demo,
                  outputDir= paste0(save_dir,"demog"),
                  partId = data.demo$id, suffix = "_d")

        # Compute final score for display (Correct: +2. Error/timeout: -1)
        currVal$score = round(runif(1,ntr,ntr * 5))
          
          # 2*(sum(expData$resp==as.numeric(exp_trials$truth) ) ) -
          # 1*(sum(expData$resp!=as.numeric(exp_trials$truth) ) )
        
        currVal$page = "Goodbye"
    
      })
    
  )} )
  # Check inputs ----------------------
  observeEvent(reactiveValuesToList(input),{

    onInputEnable(pageId = "Consent", ctrlVals = currVal,
                   pageList = consent.list, globId = "Consent",
                   inputList = input)

    onInputEnable(pageId = "Demog", ctrlVals = currVal,
                   pageList = demog.list, globId = "Demog",
                   inputList = input)
    
  })
}

# Create app
shinyApp(ui = ui, server = server)