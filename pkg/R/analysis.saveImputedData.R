## ASSUME data is in a variable called "data"
## ASSUME mi.info(data) is in a variable called "info"
saveImputedData <- function () {
  onSaveButton <- function () {
    IMP <- getMi (IMP)
    write.mi (IMP, format=as.character(tclvalue (format)))
    
    tkmessageBox(title="Save Imputed Data",message="Data saved.",icon="info",type="ok")
    tkdestroy (this.gui)
  }
  
  this.gui <- tktoplevel(width=500, height=750, border=4)
  tktitle (this.gui) <- "Save Imputed Data..."
  formats <- c("csv", "dta", "table")
  #formats <- c("CSV", "Stata", "Tab-delimited")
  format <- tclVar (formats[1])
  format.comboBox <- ttkcombobox (this.gui, values=formats, textvariable=format, width=16)
  tkgrid (tklabel (this.gui, text="      Output format: "), row=0, column=0, sticky="e")
  tkgrid (format.comboBox, row=0, column=1, columnspan=2)
  save.but <- tkbutton (this.gui, text="Save", command=onSaveButton, width=8)
  exit.but <- tkbutton (this.gui, text="Exit", command=function() tkdestroy(this.gui), width=8)
  tkgrid (save.but, row=1, column=1)
  tkgrid (exit.but, row=1, column=2)
  tkfocus (this.gui)
}
