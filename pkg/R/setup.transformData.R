transformData <- function () {



  if (requireData() == FALSE) {
    return (NULL)
  }
  preprocess <- tclVar ("FALSE")
  states <- c("disable", "normal")
  states <- states[as.numeric(as.logical(tclvalue(preprocess))+1)]



  
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
  frameCenter <- tkframe(frameOverall, relief="groove",borderwidth=4)
  frameBottom <- tkframe(frameOverall, relief="groove",borderwidth=4)
  tkgrid(frameOverall)
  tkgrid(frameUpper, row=0, column=0, columnspan=3, rowspan=1)  
  tkgrid(frameCenter, row=1, column=0, columnspan=3, rowspan=2)  
  tkgrid(frameBottom, row=4, column=0, columnspan=3, rowspan=2)  

   preprocess.rb1 <- tkradiobutton (frameUpper, text="Yes")
   tkconfigure(preprocess.rb1,variable=preprocess, value="TRUE")
   preprocess.rb2 <- tkradiobutton (frameUpper, text="No")
   tkconfigure(preprocess.rb2, variable=preprocess, value="FALSE")
   preprocess.label <- tklabel (frameUpper, text="Preprocess the data?", width=50)
   tkgrid (preprocess.label, row=0, column=0)
   tkgrid (preprocess.rb1, row=0, column=1)#, sticky="w")
   tkgrid (preprocess.rb2, row=0, column=2)#, sticky="w")
  
  
  ## Variable
  
  all.vars <- getMi(info)$name
  current.var <- tclVar (getMi(info)$name[[1]])
  current.var.comboBox <- ttkcombobox (frameCenter, values=all.vars, textvariable=current.var)
  tkgrid (tklabel (frameCenter, text="Variable"), row=2, column=0, sticky="w")
  tkgrid (current.var.comboBox, row=3, column=0, sticky="w")
  tkbind (current.var.comboBox, "<<ComboboxSelected>>", onVarChange)
  
  ## type
  types <- c ("fixed", "binary", "unordered-categorical", "ordered-categorical", 
      "continuous", "proportion", "ordered-categorical", "nonnegative", "positive-continuous",
      "count", "predictive-mean-matching")
  type <- tclVar (getMi(info)$type[[1]])
  type.comboBox <- ttkcombobox (frameCenter, values=types, textvariable=type, width=26)
  tkgrid (tklabel (frameCenter, text="Variable type"), row=2, column=1, sticky="w")
  tkgrid (type.comboBox, row=3, column=1, sticky="w")
  
  apply.but <- tkbutton (frameCenter, text="Apply", command=onApplyButton, width=10, state=states)
  tkgrid (apply.but, row=3, column=2)
  
  preprocessData <- function(){ 
    putMi(preprocess.flg, as.logical(tclvalue(preprocess)))
    if(as.logical(tclvalue(preprocess))){
      data.new <- mi.preprocess(data=getMi(data), info=getMi(info))
      putMi(info, data.new@mi.info)
      putMi(data, data.new@data)
    }
    tkdestroy(this.gui)
  }

  ## button
  ok.but <- tkbutton (frameBottom, text="Ok", command=preprocessData, width=15)
  exit.but <- tkbutton (frameBottom, text="Exit", command=function() tkdestroy(this.gui), width=15)
  tkgrid (ok.but, row=5, column=1)
  tkgrid (exit.but, row=5, column=2)
  tkfocus (this.gui)
}
