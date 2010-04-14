specifyImputationInformation <- function () {
  if (requireData() == FALSE) {
    return (NULL)
  }
  
  onVarChange <- function () {
    var <- tclvalue(current.var)
    
    tclvalue (formula) <- getMi(info)$imp.formula[[var]]
    tclvalue (type)    <- getMi(info)$type[[var]]
    tclvalue (include) <- as.character (getMi(info)$include[[var]])
    tclvalue (is.ID)   <- as.character (getMi(info)$is.ID[[var]])
  }
  
  onApplyButton <- function () {
    var <- tclvalue(current.var)
    tmp <- list()
    tmp[[var]] <- tclvalue (formula)
    info <- getMi(info)
    info <- update (info, "imp.formula", tmp)
    
    tmp[[var]] <- tclvalue (type)
    info <- update (info, "type", tmp)
    
    tmp[[var]] <- as.logical (tclvalue (include))
    info <- update (info, "include", tmp)
    
    tmp[[var]] <- as.logical (tclvalue (is.ID))
    info <- update (info, "is.ID", tmp)
    
    putMi (info)
    onVarChange()
  }
  
  this.gui <- tktoplevel(border=4)
  tktitle (this.gui) <- "Specify Conditional Models"
  ## Variable
  all.vars <- getMi(info)$name
  current.var <- tclVar (getMi(info)$name[[1]])
  current.var.comboBox <- ttkcombobox (this.gui, values=all.vars, textvariable=current.var)
  tkgrid (tklabel (this.gui, text="Variable"), row=0, column=0, sticky="w")
  tkgrid (current.var.comboBox, row=1, column=0, sticky="w")
  tkbind (current.var.comboBox, "<<ComboboxSelected>>", onVarChange)
  
  ## type
  types <- c ("fixed", "binary", "unordered-categorical", "ordered-categorical", 
      "continuous", "proportion", "ordered-categorical", "nonnegative", "positive-continuous",
      "count", "predictive-mean-matching")
  type <- tclVar (getMi(info)$type[[1]])
  type.comboBox <- ttkcombobox (this.gui, values=types, textvariable=type)
  tkgrid (tklabel (this.gui, text="Variable type"), row=0, column=1, sticky="w")
  tkgrid (type.comboBox, row=1, column=1, sticky="w")
  
  ## include
  include <- tclVar (as.character (getMi(info)$include[[1]]))
  
  include.rb1 <- tkradiobutton (this.gui, text="Yes")
  tkconfigure(include.rb1, variable=include, value="TRUE")
  include.rb2 <- tkradiobutton (this.gui, text="No")
  tkconfigure(include.rb2, variable=include, value="FALSE")
  
  tkgrid (tklabel (this.gui, text="Include this variable?"), row=2, column=1, sticky="w")
  tkgrid (include.rb1, row=3, column=1, sticky="w")
  tkgrid (include.rb2, row=4, column=1, sticky="w")
  
  
  ## is.ID
  is.ID <- tclVar (as.character (getMi(info)$is.ID[[1]]))
  
  is.ID.rb1 <- tkradiobutton (this.gui, text="Yes")
  tkconfigure(is.ID.rb1, variable=is.ID, value="TRUE")
  is.ID.rb2 <- tkradiobutton (this.gui, text="No")
  tkconfigure(is.ID.rb2, variable=is.ID, value="FALSE")
  
  tkgrid (tklabel (this.gui, text="Is this an identification variable?"), row=5, column=1, sticky="w")
  tkgrid (is.ID.rb1, row=6, column=1, sticky="w")
  tkgrid (is.ID.rb2, row=7, column=1, sticky="w")
  
  
  ## formula
  formula <- tclVar (getMi(info)$imp.formula[[1]])
  formula.entry <- tkentry (this.gui, width=80, textvariable=formula)
  
  tkgrid (tklabel (this.gui, text="Formula for the conditional model"), row=8, column=1, columnspan=2, sticky="w")
  tkgrid (formula.entry, row=9, column=1, columnspan=2)
  
  
  
  apply.but <- tkbutton (this.gui, text="Apply", command=onApplyButton, width=10)
  exit.but <- tkbutton (this.gui, text="Exit", command=function() tkdestroy(this.gui), width=10)
  tkgrid (apply.but, row=7, column=3)
  tkgrid (exit.but, row=9, column=3)
  tkfocus (this.gui)
}
