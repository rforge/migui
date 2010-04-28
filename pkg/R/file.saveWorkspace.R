## ASSUME mi object is in a variable called "IMP"
saveWorkspace <- function () {
  onSaveFile <- function () {
    file <- tclvalue (filename)
    if (grepl (pattern=".Rdata", x=file, ignore.case=TRUE) == FALSE) {
      file <- paste (file, ".Rdata", sep="")
    }
    
    local.env <- new.env()
    if (tclvalue (data.name) != "") {
      assign (tclvalue(data.name), getMi(data), envir=local.env)      
    }
    if (tclvalue (info.name) != "") {
      assign (tclvalue(info.name), getMi(info), envir=local.env)      
    }
    if (tclvalue (imp.name) != "") {
      assign (tclvalue(imp.name), getMi(IMP), envir=local.env)      
    }
    
    save(list = ls(envir=local.env, all=TRUE), file = file, envir = local.env)
    tkdestroy (this.gui)
  }
  
  onSaveGlobalEnvironment <- function () {
    if (tclvalue (data.name) != "") {
      assign (tclvalue(data.name), getMi(data), envir=.GlobalEnv)      
    }
    if (tclvalue (info.name) != "") {
      assign (tclvalue(info.name), getMi(info), envir=.GlobalEnv)      
    }
    if (tclvalue (imp.name) != "") {
      assign (tclvalue(imp.name), getMi(IMP), envir=.GlobalEnv)      
    }
    tkdestroy (this.gui)
  }
  
  this.gui <- tktoplevel(width=500, height=750)
  tktitle(this.gui) <- "Save R Workspace"
  
  ## data -> data (data.frame) 
  ## info -> info (mi.info)
  ## IMP  -> IMP  (mi)
  
  tkgrid (tklabel (this.gui, text="Variable"), row=0, column=0)
  tkgrid (tklabel (this.gui, text="Save As"), row=0, column=1)

  state <- ""

  data.name <- tclVar("")
  if (existsMi(data)) {
    state <- "normal"
    tclvalue (data.name) <- "data"
  } else {
    state <- "disabled"
    tclvalue (data.name) <- ""
  }
  tkgrid (tklabel (this.gui, text="data: ", width=15, state=state), row=1, column=0, sticky="e")
  data.name.entry <- tkentry (this.gui, width=15, textvariable=data.name, state=state)
  tkgrid (data.name.entry, row=1, column=1)

  info.name <- tclVar ("")
  if (existsMi(info)) {
    state <- "normal"
    tclvalue (info.name) <- "info"
  } else {
    state <- "disabled"
    tclvalue <- tclVar ("")
  }
  tkgrid (tklabel (this.gui, text="info: ", width=15, state=state), row=2, column=0, sticky="e")
  info.name.entry <- tkentry (this.gui, width=15, textvariable=info.name, state=state)
  tkgrid (info.name.entry, row=2, column=1)

  imp.name <- tclVar ("")
  if (existsMi(IMP)) {
    state <- "normal"
    tclvalue (imp.name) <- "IMP"
  } else {
    state <- "disabled"
    tclvalue (imp.name) <- ""
  }
  tkgrid (tklabel (this.gui, text="IMP: ", width=15, state=state), row=3, column=0, sticky="en")
  imp.name.entry <- tkentry (this.gui, width=15, textvariable=imp.name, state=state)
  tkgrid (imp.name.entry, row=3, column=1, sticky="n")
  
  
  tkgrid (tklabel (this.gui, text="File name:"), row=1, column=2)
  filename <- tclVar ("")
  #tkgrid (tklabel (this.gui, text="  "), row=1, column=1)
  filename.entry <- tkentry (this.gui, width=15, textvariable=filename)
  tkgrid (filename.entry, row=1, column=3)
  
  tkgrid (tkbutton (this.gui, text="Save File", command=onSaveFile, width=20), row=1, column=4)#, sticky="e")
  tkgrid (tkbutton (this.gui, text="Save to\nGlobal Environment", command=onSaveGlobalEnvironment, width=20), row=3, column=4)#, sticky="e")
  
  
  
  tkfocus(filename.entry)
}
