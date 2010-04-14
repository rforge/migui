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

  ## Outcome Variable
  all.vars <- paste (getMi(info)$name, "  (", getMi(info)$type, ")", sep="")
  outcomeVariable <- tclVar (all.vars[1])
  outcomeVariable.comboBox <- ttkcombobox (this.gui, values=all.vars, textvariable=outcomeVariable, width=30)
  
  tkgrid (tklabel (this.gui, text="Outcome"), row=0, column=0, sticky="w")
  tkgrid (outcomeVariable.comboBox, row=1, column=0, sticky="w")
  tkbind (outcomeVariable.comboBox, "<<ComboboxSelected>>", onOutcomeVarChange)
  
  ## Regression formula
  formula <- tclVar (getMi(info)$imp.formula[[1]])
  formula.entry <- tkentry (this.gui, width=80, textvariable=formula)
  
  tkgrid (tklabel (this.gui, text="Regression formula"), row=0, column=1, columnspan=2, sticky="w")
  tkgrid (formula.entry, row=1, column=1, columnspan=2)
  
  ## Regression model
  regressionModels <- c ("lm", "glm", "polr", "bayesglm", "bayespolr")
  links <- c ("Gaussian", "logit", "quasi-Poisson", "probit")
  regressionModel <- tclVar (regressionModels[1])
  
  regressionModel.lm <- tkradiobutton (this.gui, text="Linear Regression")
  tkconfigure(regressionModel.lm, variable=regressionModel, value=regressionModels[1])
  regressionModel.glm <- tkradiobutton (this.gui, text="Generalized Linear Regression")
  tkconfigure(regressionModel.glm, variable=regressionModel, value=regressionModels[2])
  regressionModel.glm.link <- tclVar (links[1])
  regressionModel.glm.link.combobox <- ttkcombobox (this.gui, values=links, textvariable=regressionModel.glm.link)
  
  regressionModel.polr <- tkradiobutton (this.gui, text="Penalized Ordered Logistic Regression")
  tkconfigure(regressionModel.polr, variable=regressionModel, value=regressionModels[4])
  regressionModel.bayesglm <- tkradiobutton (this.gui, text="Bayes Generalized Linear Regression")
  tkconfigure(regressionModel.bayesglm, variable=regressionModel, value=regressionModels[5])
  regressionModel.bayesglm.link <- tclVar (links[1])
  regressionModel.bayesglm.link.combobox <- ttkcombobox (this.gui, values=links, textvariable=regressionModel.bayesglm.link)
  
  
  regressionModel.bayespolr <- tkradiobutton (this.gui, text="Bayesian Penalized Ordered Logistic Regression")
  tkconfigure(regressionModel.bayespolr, variable=regressionModel, value=regressionModels[7])

  tkgrid (tklabel (this.gui, text="Regression Models"), row=2, column=0, columnspan=2)
  row <- 3
  tkgrid (regressionModel.lm, row=row, column=0, columnspan=2, sticky="w"); row <- row+1
  tkgrid (regressionModel.glm, row=row, column=0, columnspan=2, sticky="w");
  tkgrid (tklabel (this.gui, text="Link = "), row=row, column=2, sticky="e")
  tkgrid (regressionModel.glm.link.combobox, row=row, column=3, sticky="w"); row <- row + 1 
  tkgrid (regressionModel.polr, row=row, column=0, columnspan=2, sticky="w"); row <- row+1
  tkgrid (regressionModel.bayesglm, row=row, column=0, columnspan=2, sticky="w");
  tkgrid (tklabel (this.gui, text="Link = "), row=row, column=2, sticky="e") 
  tkgrid (regressionModel.bayesglm.link.combobox, row=row, column=3, sticky="w"); row <- row + 1
  tkgrid (regressionModel.bayespolr, row=row, column=0, columnspan=2, sticky="w"); row <- row+1
   
  onOutcomeVarChange()
  submit.but <- tkbutton (this.gui, text="Submit", command=onSubmitButton, width=10)
  tkgrid (submit.but, row=9, column=3)
  tkfocus (this.gui)
}