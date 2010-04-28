## ASSUME data is in a variable called "data"
loadSession <- function () {
  onOkButton <- function () {
    if (tclvalue (data.variable) == "") {
      if (existsMi (data)) {
        rmMi (data)    
      }
    }
    if (tclvalue (info.variable) == "") {
      if (existsMi (info)) {
        rmMi (info)    
      }
    }
    if (tclvalue (mi.variable) == "") {
      if (existsMi (IMP)) {
        rmMi (IMP)    
      }
    }
    
    if (tclvalue (data.variable) != "") {
      data.name <- as.name (strsplit (tclvalue (data.variable), " ")[[1]][1])
      if (class (get (as.character (data.name), envir=vars.env)) == "mi") {
        putMi (data, eval(data.name, envir=vars.env)@data)
      } else {
        putMi (data, eval(data.name, envir=vars.env))  
      }
      if (tclvalue (info.variable) == "") {
        putMi (info, mi.info(getMi(data)))
      }
    }
    
    if (tclvalue(info.variable) != "") {
      if (existsMi (data)) {
        info.name <- as.name (strsplit (tclvalue (info.variable), " ")[[1]][1])
        
        if (class (get (as.character (info.name), envir=vars.env)) == "mi") {
          putMi (info, eval (info.name, envir=vars.env)@mi.info)
        } else {
          putMi (info, eval (info.name, envir=vars.env))  
        }
      }
    }
    
    if (tclvalue (mi.variable) != "") {
      mi.name <- as.name (strsplit (tclvalue (mi.variable), " ")[[1]][1])
      IMP <- eval (mi.name, envir=vars.env)
      data <- IMP@data
      info <- IMP@mi.info
      putMi (IMP)
      putMi (data)
      putMi (info)
    }
    tkdestroy (this.gui)
  }   
  
  loadFile <- function () {
    fileName <- tclvalue(tkgetOpenFile(filetypes="{{All files} *} {{R Data files} {.Rdata}}"))
    if (nchar(fileName)) {
      rm (list=ls(envir=vars.env), envir=vars.env)
      load (fileName, envir=vars.env)
    }
    useTmpWorkspace()
  }
  
  useTmpWorkspace <- function () {
    names <- ls(envir=vars.env)
    classes <- lapply (names, as.name)
    classes <- lapply (classes, eval, envir=vars.env)
    classes <- lapply (classes, class)
    classes <- lapply (classes, "[[", 1)
    classes <- unlist (classes)
    
    vars <- cbind (names, classes)
    
    subset <- vars[, "classes"] == "data.frame" | vars[, "classes"] == "mi" 
    if (sum (subset) == 0) {
      data.variables <- c ("")
    } else {
      data.variables <- c("", paste (vars[subset, 1], "  (", vars[subset, 2], ")", sep=""))
    }
    
    subset <- vars[, "classes"] == "mi.info"
    if (sum (subset) == 0) {
      info.variables <- c ("")    
    } else {
      info.variables <- c("", paste (vars[subset, 1], "  (", vars[subset, 2], ")", sep=""))
    }
    
    subset <- vars[, "classes"] == "mi"
    if (sum (subset) == 0) {
      mi.variables <- c ("")    
    } else {
      mi.variables <- c("", paste (vars[subset, 1], "  (", vars[subset, 2], ")", sep=""))
    } 
    assign ("data.variables", data.variables, envir = loadSession.env)
    assign ("info.variables", info.variables, envir = loadSession.env)
    assign ("mi.variables"  , mi.variables,   envir = loadSession.env)
    redrawComboBoxes()
  }
  
  redrawComboBoxes <- function () {
    data.variable.comboBox <- ttkcombobox (frameLeft, values=data.variables, textvariable=data.variable, width=25)
    info.variable.comboBox <- ttkcombobox (frameLeft, values=info.variables, textvariable=info.variable, width=25)
    mi.variable.comboBox   <- ttkcombobox (frameLeft, values=mi.variables, textvariable=mi.variable, width=25)
    
    tkgrid (data.variable.comboBox, row=1, column=0)
    tkgrid (info.variable.comboBox, row=3, column=0)
    tkgrid (mi.variable.comboBox, row=5, column=0)
  }
  
  
  this.gui <- tktoplevel()
  tktitle(this.gui) <- "Load Previously Saved Session"
  frameOverall <- tkframe(this.gui)
  frameLeft <- tkframe(frameOverall, relief="groove", borderwidth=4)
  frameUpperRight <- tkframe(frameOverall, relief="groove", borderwidth=4)
  frameBottomRight <- tkframe(frameOverall, relief="groove", borderwidth=4)
  tkgrid(frameOverall)
  tkgrid(frameLeft, row=0, column=0, rowspan=6)  
  tkgrid(frameUpperRight, row=0, column=1, rowspan=1)     
  tkgrid(frameBottomRight, row=2, column=1, rowspan=5)     
  
  loadFile.but <- tkbutton (frameUpperRight, text="Load File", command=loadFile, width=15)    
  ok.but <- tkbutton (frameBottomRight, text="OK", command=onOkButton, width=15)
  exit.but <- tkbutton (frameBottomRight, text="Exit", command=function() tkdestroy(this.gui), width=15)
  #tkgrid(tklabel(frameRight, text = ""), row = 0, column = 1, rowspan=2)
  tkgrid(loadFile.but, row=0, column=1)
  tkgrid (ok.but, row=1, column=1)
  tkgrid (exit.but, row=2, column=1)
  
  data.variables <- c("")
  info.variables <- c("")
  mi.variables <- c("")
  
  data.variable <- tclVar ("")
  mi.variable <- tclVar ("")
  info.variable <- tclVar ("")
  
  #tkgrid (tklabel (this.gui, text=" "), row=0, column=0)
  #tkgrid (loadFile.but, row=1, column=0)
  tkgrid (tklabel (frameLeft, text="Data"), row=0, column=0, sticky="w")
  tkgrid (tklabel (frameLeft, text="Imputation Info"), row=2, column=0, sticky="w")
  tkgrid (tklabel (frameLeft, text="Imputation (class: mi)"), row=4, column=0, sticky="w")
  
  loadSession.env <- environment ()
  vars.env <- new.env()
  for (varName in ls(envir=globalenv())) {
    assign (varName, get(varName, envir=globalenv()), envir=vars.env)
  }
  useTmpWorkspace()
  tkfocus (this.gui)
} 
