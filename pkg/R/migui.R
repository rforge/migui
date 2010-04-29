#library (tcltk)
#library(tkrplot)
#library (mi)


migui <- function(){
  # initialize miEnv
  miEnv()
  
# This is from Rcmdr file-menu.R
  .setWd <- function(){
    wd <- tclvalue(tkchooseDirectory(initialdir = getwd()))
    if (wd != ""){
      setwd (wd)
    }
  }



  .exit <- function(){
    exitValue <- tkmessageBox(message="Are you sure you want to exit MI?",
      icon="question",
      type="okcancel",
      default="cancel")
    if(tclvalue(exitValue) == "ok"){
      tkdestroy(gui)
      detach("miEnv")
   }
  }



  ## Create a gui with the title "MI"
  gui <- tktoplevel(width=500, height=50) #backgound
  tktitle(gui) <- "MI: Missing Data Imputation and Diagnostics"
  current <- options("Mi")[[1]]
  #setOption("crisp.dialogs", TRUE, global=TRUE)
  
  ## Create a Main Menu
  main.menu      <- tkmenu(gui)
  main.menu.file <- tkmenu(main.menu, tearoff=0) 
  #main.menu.edit <- tkmenu(main.menu, tearoff=0)
  #main.menu.data <- tkmenu(main.menu, tearoff=0) 
  main.menu.setup <- tkmenu(main.menu, tearoff=0)
  main.menu.impute <- tkmenu(main.menu, tearoff=0)
  main.menu.analysis <- tkmenu(main.menu, tearoff=0)
  main.menu.validation <- tkmenu(main.menu, tearoff=0)
  main.menu.help <- tkmenu(main.menu, tearoff=0)
  # tearoff: Without it, each of your menus (on Windows and X11) will start with 
  # what looks like a dashed line, and allows you to "tear off" the menu so it 
  # appears in its own window.
  
  ### File
  #===="Changing Working Directory"
  tkadd(main.menu.file, "command", label="Change working directory", command = .setWd)
  #===="Open Script File"
  #tkadd(main.menu.file, "command", label="Open script file", command = function() NULL)
  #===="Save Script File"
  #tkadd(main.menu.file, "command", label="Save script file", command = function() NULL)
  #===="Save Output File"
  #tkadd(main.menu.file, "command", label="Save output file", command = function() NULL)
  #===="Save R Workspace"
  tkadd(main.menu.file, "command", label="Save session", command = saveWorkspace)
  #===="Exit"
  tkadd(main.menu.file, "command", label="Exit", command = .exit)
  
  
  #### Edit
  ##===="Copy"
  #tkadd(main.menu.edit, "command", label="Copy", command = function() NULL)
  ##===="Paste"
  #tkadd(main.menu.edit, "command", label="Paste", command = function() NULL)
  ##===="Select All"
  #tkadd(main.menu.edit, "command", label="Select all", command = function() NULL)
  ##===="Undo"
  #tkadd(main.menu.edit, "command", label="Undo", command = function() NULL)
  ##===="Redo"
  #tkadd(main.menu.edit, "command", label="Redo", command = function() NULL)
  ##===="Clear Console"
  #tkadd(main.menu.edit, "command", label="Clear Console", command = function() NULL)
  
  
  
  ### Setup
  #===="Load Data Set"
  tkadd(main.menu.setup, "command", label="Load session", command = loadSession)
  #===="Import data from"
  tkadd(main.menu.setup, "command", label="Import data from", command = importDataFrom)
  #===="Diaply Missing Data Pattern"
  tkadd(main.menu.setup, "command", label="Display missing data patterns", command = displayMissingDataPattern)
  #===="Specify Inputation Information"
  tkadd(main.menu.setup, "command", label="Specify imputation information", command = specifyImputationInformation)
  
  ### Imputation
  #===="Run"
  tkadd(main.menu.impute, "command", label="Run imputation", command = runImputation)
  #===="Checking the fit of conditional models"
  tkadd(main.menu.impute, "command", label="Check the fit of conditional models", command = checkModelFit)
  #===="Checking the convergence of the imputation"
  #tkadd(main.menu.impute, "command", label="Check the convergence of the imputation", command = function() NULL)
  
  ### Analysis
  #===="Obstain imputed data"
  tkadd(main.menu.analysis, "command", label="Save imputed data sets", command = saveImputedData)
  #===="Pool Regression Estimates"
  tkadd(main.menu.analysis, "command", label="Pool regression estimates", command = poolRegressionEstimates)
  
  ### Validation
  #===="Validation"
  tkadd(main.menu.validation, "command", label="Sensitivity analysis", command = function() NULL, state="disabled")
  #===="Pooling Regression Estimates"
  tkadd(main.menu.validation, "command", label="Cross validation", command = function() NULL, state="disabled")
  
  ### Help
  #===="Documentation"
  tkadd(main.menu.help, "command", label="Documentation", command = .doc)
  tkadd(main.menu.help, "command", label="Bugs Report",command = .bugsReport)
  #===="Pooling Regression Estimates"
  tkadd(main.menu.help, "command", label="About",command = .about)
  
  
  #### put "File" on the main menu
  tkadd(main.menu,"cascade", label="File", menu=main.menu.file)
  #tkadd(main.menu,"cascade", label="Edit", menu=main.menu.edit)
  #tkadd(main.menu,"cascade", label="Data", menu=main.menu.data)
  tkadd(main.menu,"cascade", label="Setup", menu=main.menu.setup)
  tkadd(main.menu,"cascade", label="Imputation", menu=main.menu.impute)
  tkadd(main.menu,"cascade", label="Analysis", menu=main.menu.analysis)
  tkadd(main.menu,"cascade", label="Validation", menu=main.menu.validation)
  tkadd(main.menu,"cascade", label="Help", menu=main.menu.help)
  tkconfigure(gui,menu=main.menu)
  tkfocus(gui)
}
