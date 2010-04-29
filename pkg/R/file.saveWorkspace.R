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
  tktitle(this.gui) <- "Save session"
  frameOverall <- tkframe(this.gui)
  frameLeft <- tkframe(frameOverall, relief="groove", borderwidth=4)
  frameRight <- tkframe(frameOverall, relief="groove", borderwidth=4) 
  frameCenter <- tkframe(frameOverall, relief="groove", borderwidth=4)  

  tkgrid(frameOverall)
  tkgrid(frameLeft, row=0, column=0, rowspan=4, columnspan=2)
  tkgrid(frameCenter, row=0, column=2, rowspan=4, columnspan=2)
  tkgrid(frameRight, row=0, column=4, rowspan=4, columnspan=1)




  ## data -> data (data.frame) 
  ## info -> info (mi.info)
  ## IMP  -> IMP  (mi)
  
  tkgrid (tklabel (frameLeft, text="Object", font = c("Arial", "10")), row=0, column=0)
  tkgrid (tklabel (frameLeft, text="Rename as...", font = c("Arial", "10")), row=0, column=1)

  state <- ""

  data.name <- tclVar("")
  if (existsMi(data)) {
    state <- "normal"
    tclvalue (data.name) <- "data"
  } else {
    state <- "disabled"
    tclvalue (data.name) <- ""
  }
  tkgrid (tklabel (frameLeft, text="dataframe: ", state=state), row=1, column=0, sticky="w")
  data.name.entry <- tkentry (frameLeft, width=20, textvariable=data.name, state=state, border=2)
  tkgrid (data.name.entry, row=1, column=1)

  info.name <- tclVar ("")
  if (existsMi(info)) {
    state <- "normal"
    tclvalue (info.name) <- "info"
  } else {
    state <- "disabled"
    tclvalue <- tclVar ("")
  }
  tkgrid (tklabel (frameLeft, text="mi information: ", state=state), row=2, column=0, sticky="w")
  info.name.entry <- tkentry (frameLeft, width=20, textvariable=info.name, state=state, border=2)
  tkgrid (info.name.entry, row=2, column=1)

  imp.name <- tclVar ("")
  if (existsMi(IMP)) {
    state <- "normal"
    tclvalue (imp.name) <- "IMP"
  } else {
    state <- "disabled"
    tclvalue (imp.name) <- ""
  }
  tkgrid (tklabel (frameLeft, text="mi object: ", state=state), row=3, column=0, sticky="w")
  imp.name.entry <- tkentry (frameLeft, width=20, textvariable=imp.name, state=state, border=2)
  tkgrid (imp.name.entry, row=3, column=1, sticky="n")
 
  tkgrid (tklabel (frameCenter, text="Output as an R workspace", font=c("Arial", "10")), row=0, column=2, columnspan=2)
  tkgrid (tklabel (frameCenter, text="File name:"), row=2, column=2, sticky="w")
  filename <- tclVar ("")
  #tkgrid (tklabel (this.gui, text="  "), row=1, column=1)
  filename.entry <- tkentry (frameCenter, width=20, textvariable=filename, border=2)
  tkgrid (filename.entry, row=2, column=3)
  tkgrid (tklabel(frameCenter, text=""), row=1, column=3)
  tkgrid (tklabel(frameCenter, text=""), row=3, column=3)
  
  
  
  tkgrid (tkbutton (frameRight, text="Save to global environment", command=onSaveGlobalEnvironment, width=24), row=2, column=4)
  tkgrid (tkbutton (frameRight, text="Save to file", command=onSaveFile, width=24), row=3, column=4)
  tkgrid (tkbutton (frameRight, text="Exit", command=function() tkdestroy(this.gui), width=24), row=4, column=4)

  
  tkfocus(this.gui)
}
