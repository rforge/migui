## ASSUME mi object is in a variable called "IMP"
saveWorkspace <- function () {
  onSave <- function () {
    file <- tclvalue (filename)
    if (grepl (pattern=".Rdata", x=file, ignore.case=TRUE) == FALSE) {
      file <- paste (file, ".Rdata", sep="")
    }
    save(list = lsMi(), file = file, envir = miEnv())
    tkdestroy (this.gui)
  }
  
  this.gui <- tktoplevel(width=500, height=750)
  tktitle(this.gui) <- "Save R Workspace"
  
  tkgrid (tklabel (this.gui, text="  "), row=0, column=0)
  
  tkgrid (tklabel (this.gui, text="File name:"), row=1, column=1)
  tkgrid (tklabel (this.gui, text="(.Rdata)"), row=2, column=1)
  filename <- tclVar ("")
  #tkgrid (tklabel (this.gui, text="  "), row=1, column=1)
  filename.entry <- tkentry (this.gui, width=15, textvariable=filename)
  tkgrid (filename.entry, row=1, column=2)
  
  tkgrid (tklabel (this.gui, text="  "), row=3, column=0)
  tkgrid (tkbutton (this.gui, text="Save", command=onSave, width=10), row=4, column=1)#, sticky="e")
  tkgrid (tkbutton (this.gui, text="Exit", command=function() tkdestroy(this.gui), width=10), row=4, column=2)#, sticky="e")    
  #tkgrid (tklabel (this.gui, text="  "), row=4, column=0)
  
  tkbind (this.gui, "<Return>", function() onSave())
  
  tkfocus(filename.entry)
}
