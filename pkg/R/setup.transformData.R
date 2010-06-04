transformData <- function () {

  if (requireData() == FALSE) {
    return (NULL)
  }
  
  onVarChange <- function () {
    var <- tclvalue(current.var)
    tclvalue (type)    <- getMi(info)$type[[var]]
  }
  
  onApplyButton <- function () {
    var <- tclvalue(current.var)
    tmp <- list()
    tmp[[var]] <- tclvalue (type)
    info <- update (info, "type", tmp)    
    putMi (info)
    onVarChange()
  }
  
  this.gui <- tktoplevel()
  tktitle (this.gui) <- "Transforming the data"
  frameOverall <- tkframe(this.gui)
  frameUpper <- tkframe(frameOverall, relief="groove",borderwidth=4)
  frameBottom <- tkframe(frameOverall, relief="groove",borderwidth=4)
  frameRight <- tkframe(frameOverall, relief="groove",borderwidth=4)
  tkgrid(frameOverall)
  tkgrid(frameUpper, row=0, column=0, columnspan=2, rowspan=1)  
  tkgrid(frameBottom, row=1, column=0, columnspan=2, rowspan=2)  
  tkgrid(frameRight, row=0, column=2, columnspan=1, rowspan=3)  

  preprocess <- tclVar("0")
  preprocess.chk <- tkcheckbutton(frameUpper)
  tkconfigure(preprocess.chk, variable=preprocess)
  preprocess.label <- tklabel (frameUpper, text="Preprocess the data?")#, state="disable")
  tkgrid (preprocess.label, row=0, column=0, sticky="w")
  tkgrid (preprocess.chk, row=0, column=1)
  
  
  ## Variable
  states <- c("disable", "normal")
  states <- states[(as.numeric(tclvalue(preprocess))+1)]
  
  all.vars <- getMi(info)$name
  current.var <- tclVar (getMi(info)$name[[1]])
  current.var.comboBox <- ttkcombobox (frameBottom, values=all.vars, textvariable=current.var)
  tkgrid (tklabel (frameBottom, text="Variable"), row=2, column=0, sticky="w")
  tkgrid (current.var.comboBox, row=3, column=0, sticky="w")
  tkbind (current.var.comboBox, "<<ComboboxSelected>>", onVarChange)
  
  ## type
  types <- c ("fixed", "binary", "unordered-categorical", "ordered-categorical", 
      "continuous", "proportion", "ordered-categorical", "nonnegative", "positive-continuous",
      "count", "predictive-mean-matching")
  type <- tclVar (getMi(info)$type[[1]])
  type.comboBox <- ttkcombobox (frameBottom, values=types, textvariable=type, width=30)
  tkgrid (tklabel (frameBottom, text="Variable type"), row=2, column=1, sticky="w")
  tkgrid (type.comboBox, row=3, column=1, sticky="w")
    
  ## button
  apply.but <- tkbutton (frameRight, text="Apply", command=onApplyButton, width=10)
  ok.but <- tkbutton (frameRight, text="Ok", command=function() tkdestroy(this.gui), width=10)
  exit.but <- tkbutton (frameRight, text="Exit", command=function() tkdestroy(this.gui), width=10)
  tkgrid (apply.but, row=0, column=2)
  tkgrid (ok.but, row=1, column=2)
  tkgrid (exit.but, row=2, column=2)
  tkfocus (this.gui)
}
