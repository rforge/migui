poolRegressionEstimates <- function () {
  if (requireIMP() == FALSE) {
    return (NULL)
  }
  
  onOutcomeVarChange <- function () {
    var <- strsplit (tclvalue(outcomeVariable), " ")[[1]][1]
    
    tclvalue (formula) <- getMi(info)$imp.formula[[var]]
    
    ## TODO: Want to shade accordingly here
    
    ## tclvalue (type)    <- getMi(info)$type[[var]]
    ## tclvalue (include) <- as.character (getMi(info)$include[[var]])
    ## tclvalue (is.ID)   <- as.character (getMi(info)$is.ID[[var]])
  }

  onSubmitButton <- function () {
    var <- strsplit (tclvalue(outcomeVariable), " ")[[1]][1]
    
    model <- tclvalue (regressionModel) 
    
    fit <- NA
    if (model == regressionModels[1]) { # lm
      fit <- lm.mi (tclvalue (formula), IMP)   
    }
    if (model == regressionModels[2]) { # glm
      family <- ""
      if (tclvalue (regressionModel.glm.link) == "Gaussian") {
        family <- gaussian()
      } else if (tclvalue (regressionModel.glm.link) == "logit") {
        family <- binomial(link="logit")
      } else if (tclvalue (regressionModel.glm.link) == "quasi-Poisson") {
        family <- quasipoisson()
      } else if (tclvalue (regressionModel.glm.link) == "probit") {
        family <- binomial(link="probit")
      } 
        
      fit <- glm.mi (tclvalue (formula), IMP, family=family)
    }
    if (model == regressionModels[3]) { # polr
      fit <- polr.mi (tclvalue (formula), IMP)
    }
    if (model == regressionModels[4]) { # bayesglm
      family <- ""
      if (tclvalue (regressionModel.glm.link) == "Gaussian") {
        family <- gaussian()
      } else if (tclvalue (regressionModel.glm.link) == "logit") {
        family <- binomial(link="logit")
      } else if (tclvalue (regressionModel.glm.link) == "quasi-Poisson") {
        family <- quasipoisson()
      } else if (tclvalue (regressionModel.glm.link) == "probit") {
        family <- binomial(link="probit")
      } 
      fit <- bayesglm.mi (tclvalue (formula), IMP, family=family)
    }
    if (model == regressionModels[5]) { # bayespolr
      fit <- bayespolr.mi (tclvalue (formula), IMP)
    }
    display (fit)
  }
  
  
  this.gui <- tktoplevel(border=4)
  tktitle (this.gui) <- "Pool Regression Estimates"
  frameOverall <- tkframe(this.gui)
  frameUpper <- tkframe(frameOverall, relief="groove", borderwidth=4)
  frameLeft <- tkframe(frameOverall, relief="groove", borderwidth=4)
  frameRight <- tkframe(frameOverall, relief="groove", borderwidth=4)  


  tkgrid(frameOverall)
  tkgrid(frameUpper, row=0, column=0, rowspan=2, columnspan=3)
  tkgrid(frameLeft, row=2, column=0, rowspan=6, columnspan=2)
  tkgrid(frameRight, row=2, column=2, rowspan=6, columnspan=1)


  ## Outcome Variable
  all.vars <- paste (getMi(info)$name, "  (", getMi(info)$type, ")", sep="")
  outcomeVariable <- tclVar (all.vars[1])
  outcomeVariable.comboBox <- ttkcombobox (frameUpper, values=all.vars, textvariable=outcomeVariable, width=30)
  
  tkgrid (tklabel (frameUpper, text="Outcome Variable"), row=0, column=0, sticky="w")
  tkgrid (outcomeVariable.comboBox, row=1, column=0, sticky="w")
  tkbind (outcomeVariable.comboBox, "<<ComboboxSelected>>", onOutcomeVarChange)
  
  ## Regression formula
  formula <- tclVar (getMi(info)$imp.formula[[1]])
  formula.entry <- tkentry (frameUpper, width=75, textvariable=formula)
  
  tkgrid (tklabel (frameUpper, text="Regression Formula"), row=0, column=1, columnspan=3, sticky="w")
  tkgrid (formula.entry, row=1, column=1, columnspan=3)
  
  ## Regression model
  regressionModels <- c ("lm", "glm", "polr", "bayesglm", "bayespolr")
  links <- c ("Gaussian", "logit", "quasi-Poisson", "probit")
  regressionModel <- tclVar (regressionModels[1])
  
  regressionModel.lm <- tkradiobutton (frameLeft, text="Linear Regression")
  tkconfigure(regressionModel.lm, variable=regressionModel, value=regressionModels[1])
  regressionModel.glm <- tkradiobutton (frameLeft, text="Generalized Linear Regression, Link = ")
  tkconfigure(regressionModel.glm, variable=regressionModel, value=regressionModels[2])
  regressionModel.glm.link <- tclVar (links[1])
  regressionModel.glm.link.combobox <- ttkcombobox (frameLeft, values=links, textvariable=regressionModel.glm.link)
  
  regressionModel.polr <- tkradiobutton (frameLeft, text="Ordered Logistic Regression")
  tkconfigure(regressionModel.polr, variable=regressionModel, value=regressionModels[3])
  regressionModel.bayesglm <- tkradiobutton (frameLeft, text="Bayes Generalized Linear Regression, Link = ")
  tkconfigure(regressionModel.bayesglm, variable=regressionModel, value=regressionModels[4])
  regressionModel.bayesglm.link <- tclVar (links[1])
  regressionModel.bayesglm.link.combobox <- ttkcombobox (frameLeft, values=links, textvariable=regressionModel.bayesglm.link)
  
  
  regressionModel.bayespolr <- tkradiobutton (frameLeft, text="Bayesian Ordered Logistic Regression")
  tkconfigure(regressionModel.bayespolr, variable=regressionModel, value=regressionModels[5])
  
  
  tkgrid (tklabel (frameLeft, text="Regression Models"), row=2, column=0, columnspan=4, sticky="w")
  tkgrid (regressionModel.lm, row=3, column=0, sticky="w")
  tkgrid (regressionModel.glm, row=4, column=0, sticky="w")
  tkgrid (regressionModel.glm.link.combobox, row=4, column=1, sticky="w")
  tkgrid (regressionModel.polr, row=5, column=0, sticky="w")
  tkgrid (regressionModel.bayesglm, row=6, column=0, sticky="w")
  tkgrid (regressionModel.bayesglm.link.combobox, row=6, column=1, sticky="w")
  tkgrid (regressionModel.bayespolr, row=7, column=0, sticky="w")
   
  onOutcomeVarChange()
  submit.but <- tkbutton (frameRight, text="Submit", command=onSubmitButton, width=15)
  exit.but <- tkbutton (frameRight, text="Exit", command=function() tkdestroy(this.gui), width=15)
  tkgrid (submit.but, row=6, column=3)
  tkgrid (exit.but, row=7, column=3)
  tkfocus (frameOverall)
}
