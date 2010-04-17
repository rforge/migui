runImputation <- function () {
  if (requireDataOrImp() == FALSE) {
    return (NULL)
  }

  states <- c("normal", "disabled")
  
  render <- function () {
        
    runState <- 1
    if (existsMi (IMP)) {
      runState <- 2
      conv <- getMi(IMP)@converged
      check.conv <- getMi(IMP)@coef.converged
      coef.mcmc <- getMi(IMP)@coef.mcmc
      if(is.null(coef.mcmc)){
        conv <- conv
      }else{
        conv <- conv & check.conv
      }
      if (conv) {
        tclvalue(converged.message) <- "MI CONVERGED!"
        tkconfigure (converged.entry, disabledforeground="white", disabledbackground="black")
      } else {
        tclvalue(converged.message) <- "MI IS NOT CONVERGED!"
        tkconfigure (converged.entry, disabledforeground="red", disabledbackground="yellow")
      }
    } else{
      tclvalue(converged.message) <- "MI NOT RUN!"
      tkconfigure (converged.entry, disabledforeground="gray", disabledbackground="white")
    }
    
    tkconfigure (n.imp.label, state=states[runState])
    tkconfigure (n.imp.entry, state=states[runState])
    
    tkconfigure (preprocess.label, state=states[runState])   
    tkconfigure (preprocess.rb1, state=states[runState])
    tkconfigure (preprocess.rb2, state=states[runState])
    
    tkconfigure (check.coef.convergence.label, state=states[runState])   
    tkconfigure (check.coef.convergence.chk, state=states[runState])
    
    tkconfigure (add.noise.label, state=states[runState])   
    tkconfigure (add.noise.rb1, state=states[runState])
    tkconfigure (add.noise.rb2, state=states[runState])
    
    tkconfigure (add.noise.post.run.iter.label, state=states[runState])
    tkconfigure (add.noise.post.run.iter.entry, state=states[runState])
    
    tkconfigure (add.noise.method.label, state=states[runState])
    tkconfigure (add.noise.method.rb1, state=states[runState])
    tkconfigure (add.noise.method.rb2, state=states[runState])
    
    tkconfigure (add.noise.K.label, state=states[runState])
    tkconfigure (add.noise.K.entry, state=states[runState])
    
    tkconfigure (add.noise.pct.aug.label, state=states[runState])
    tkconfigure (add.noise.pct.aug.entry, state=states[runState])
    
    tkconfigure (run.but, state=states[runState])
    tkconfigure (continue.but, state=states[3-runState])
    tkconfigure (clear.but, state=states[3-runState])
    
    renderNoise()
  }
  
  renderNoise <- function () {
    if (existsMi (IMP) == FALSE) {
      noiseState <- 2
      if (as.logical (tclvalue (add.noise))) {
        noiseState <- 1
      }
      tkconfigure (add.noise.post.run.iter.entry, state=states[noiseState])
      tkconfigure (add.noise.post.run.iter.label, state=states[noiseState])
      
      tkconfigure (add.noise.method.label, state=states[noiseState])
      tkconfigure (add.noise.method.rb1, state=states[noiseState])
      tkconfigure (add.noise.method.rb2, state=states[noiseState])
      
      tkconfigure (add.noise.K.label, state=states[noiseState])       
      tkconfigure (add.noise.K.entry, state=states[noiseState])
      
      tkconfigure (add.noise.pct.aug.label, state=states[noiseState])       
      tkconfigure (add.noise.pct.aug.entry, state=states[noiseState])
      
      renderNoiseMethod ()
    }
  }
  
  renderNoiseMethod <- function () {
    if (existsMi (IMP) == FALSE) {
      if (as.logical (tclvalue (add.noise))) {
        reshufflingState <- 2
        if (tclvalue (add.noise.method) == "reshuffling") {
          reshufflingState <- 1
        }
        tkconfigure (add.noise.K.entry, state=states[reshufflingState])
        tkconfigure (add.noise.K.label, state=states[reshufflingState])
        
        tkconfigure (add.noise.pct.aug.entry, state=states[3-reshufflingState])
        tkconfigure (add.noise.pct.aug.label, state=states[3-reshufflingState])
      }
    }
  }
  
  
  onClearButton <- function () {
    rmMi(IMP)
    render()
  }
  
  onContinueButton <- function () {
    IMP <- mi (getMi (IMP),
        n.iter          = as.numeric (tclvalue (n.iter)),
        R.hat           = as.numeric (tclvalue (R.hat)),
        max.minutes     = as.numeric (tclvalue (max.minutes)), 
        rand.imp.method     = as.character(tclvalue (rand.imp.method)), 
        run.past.convergence  = as.logical((tclvalue (run.past.convergence))),
        seed          = as.numeric (tclvalue (seed)))
    putMi(IMP)
    ## TODO: OK button saying convergence
    render()
  }
  
  onRunButton <- function () {
    if (as.logical (tclvalue (add.noise))) {
      noise <- noise.control (
          method        = as.character(tclvalue (add.noise.method)), 
          pct.aug       = as.numeric (tclvalue (add.noise.pct.aug)),
          K             = as.numeric (tclvalue (add.noise.K)),
          post.run.iter = as.numeric (tclvalue (add.noise.post.run.iter)))
    } else {
      noise <- FALSE
    }
    
    IMP <- mi (getMi(data), getMi (info), 
        n.imp           = as.numeric (tclvalue (n.imp)),
        n.iter          = as.numeric (tclvalue (n.iter)),
        R.hat           = as.numeric (tclvalue (R.hat)),
        max.minutes     = as.numeric (tclvalue (max.minutes)), 
        rand.imp.method     = as.character(tclvalue (rand.imp.method)), 
        preprocess        = as.logical (tclvalue (preprocess)), 
        run.past.convergence  = as.logical (tclvalue (run.past.convergence)),
        seed          = as.numeric (tclvalue (seed)),
        check.coef.convergence  = as.logical (as.numeric(tclvalue (check.coef.convergence))), 
        add.noise         = noise)
    putMi(IMP)
    ## TODO: OK button saying convergence
    render()
  }
  
  
  
  this.gui <- tktoplevel()
  tktitle (this.gui) <- "Run Imputation"
  frameOverall <- tkframe(this.gui)
  frameLeft <- tkframe(frameOverall, width=500, height=500, relief="groove",borderwidth=4)
  frameCenter <- tkframe(frameOverall, width=400, height=500, relief="groove",borderwidth=4)
  frameBottom <- tkframe(frameOverall, width=100, height=500, relief="groove",borderwidth=4)
  frameConv <- tkframe(frameOverall, relief="groove",borderwidth=4)

  #frameBottom <- tkframe(frameOverall, width=840, height=40, relief="groove",borderwidth=2)  
  tkgrid(frameOverall)
  tkgrid(frameLeft, row=0, column=0, columnspan=2, rowspan=11)  
  tkgrid(frameCenter, row=0, column=3, columnspan=2, rowspan=7)
  tkgrid(frameBottom, row=9, column=3, columnspan=2, rowspan=3)  
  tkgrid(frameConv, row=12, column=0, columnspan=4, rowspan=1)  

  #tkgrid(frameRight, row=0, column=3, columnspan=2)

  


  ## options:
  ## Left Frame
  tkgrid(tklabel(frameLeft, text = "Basic Setup", font = c("Arial", "11")), row = 0,
    column = 0, columnspan = 2, sticky = "w")
  ## n.imp = 3
  n.imp <- tclVar (3)
  n.imp.label <- tklabel (frameLeft, text="# Imputation Chains")
  n.imp.entry <- tkentry (frameLeft, width=10, textvariable=n.imp)
  tkgrid (n.imp.label, row=1, column=0, sticky="w")
  tkgrid (n.imp.entry, row=1, column=1, sticky="w")
  
  ## n.iter = 20
  n.iter <- tclVar (20)
  n.iter.label <- tklabel (frameLeft, text="# Iterations")
  n.iter.entry <- tkentry (frameLeft, width=10, textvariable=n.iter)
  tkgrid (n.iter.label, row=2, column=0, sticky="w")
  tkgrid (n.iter.entry, row=2, column=1, sticky="w")
    
  ## max.minutes = 20
  max.minutes <- tclVar (20)
  max.minutes.label <- tklabel (frameLeft, text="Maximum minutes")
  max.minutes.entry <- tkentry (frameLeft, width=10, textvariable=max.minutes)
  tkgrid (max.minutes.label, row=3, column=0, sticky="w")
  tkgrid (max.minutes.entry, row=3, column=1, sticky="w")

  ## seed = ""
  seed <- tclVar ("")
  seed.label <- tklabel (frameLeft, text="Random Seed")
  seed.entry <- tkentry (frameLeft, width=10, textvariable=seed)
  tkgrid (seed.label, row=4, column=0, sticky="w")
  tkgrid (seed.entry, row=4, column=1, sticky="w")
  
  ## R.hat = 1.1
  R.hat <- tclVar (1.1)
  R.hat.label <- tklabel (frameLeft, text="R.hat convergence criterion")
  R.hat.entry <- tkentry (frameLeft, width=10, textvariable=R.hat)
  tkgrid (R.hat.label, row=5, column=0, sticky="w")
  tkgrid (R.hat.entry, row=5, column=1, sticky="w")
  
  ## preprocess = TRUE
  preprocess <- tclVar ("TRUE")
  preprocess.rb1 <- tkradiobutton (frameLeft, text="Yes")
  tkconfigure(preprocess.rb1,variable=preprocess, value="TRUE")
  preprocess.rb2 <- tkradiobutton (frameLeft, text="No")
  tkconfigure(preprocess.rb2, variable=preprocess, value="FALSE")
  preprocess.label <- tklabel (frameLeft, text="Transform the data?")
  tkgrid (preprocess.label, row=6, column=0, sticky="w")
  tkgrid (preprocess.rb1, row=6, column=1, sticky="w")
  tkgrid (preprocess.rb2, row=7, column=1, sticky="w")

  
  ## check.coef.convergence = FALSE
  check.convergence <- tclVar("1")
  check.coef.convergence <- tclVar ("0")
  check.convergence.chk <- tkcheckbutton (frameLeft)
  tkconfigure(check.convergence.chk, variable=check.convergence, state="disable")
  check.coef.convergence.chk <- tkcheckbutton (frameLeft)
  tkconfigure(check.coef.convergence.chk,variable=check.coef.convergence)
  check.convergence.label <- tklabel (frameLeft, text="Check convergence of imputed values", state="disable")
  check.coef.convergence.label <- tklabel (frameLeft, text="Check convergence of coefficients")
  tkgrid (check.convergence.label, row=8, column=0, sticky="w")
  tkgrid (check.coef.convergence.label, row=9, column=0, sticky="w")
  tkgrid (check.convergence.chk, row=8, column=1, sticky="w")
  tkgrid (check.coef.convergence.chk, row=9, column=1, sticky="w")

  ## run.past.convergence = FALSE
  run.past.convergence <- tclVar ("FALSE")
  run.past.convergence.rb1 <- tkradiobutton (frameLeft, text="Yes")
  tkconfigure(run.past.convergence.rb1,variable=run.past.convergence,value="TRUE")
  run.past.convergence.rb2 <- tkradiobutton (frameLeft, text="No")
  tkconfigure(run.past.convergence.rb2,variable=run.past.convergence,value="FALSE")
  run.past.convergence.label <- tklabel (frameLeft, text="Run past convergence?")
  tkgrid (run.past.convergence.label, row=10, column=0, sticky="w")
  tkgrid (run.past.convergence.rb1, row=10, column=1, sticky="w")
  tkgrid (run.past.convergence.rb2, row=11, column=1, sticky="w")
  
  

  ## rand.imp.method = bootstrap
#  rand.imp.methods <- c ("bootstrap")
#  rand.imp.method <- tclVar ("bootstrap")
#  rand.imp.method.comboBox <- ttkcombobox (frameLeft, values=rand.imp.methods, textvariable=rand.imp.method)
#  tkgrid (tklabel (this.gui, text="Random Imputation Method"), row=0, column=1)
#  tkgrid (rand.imp.method.comboBox, row=1, column=1)
  
  ## center frame 
  
  tkgrid(tklabel(frameCenter, text = "Methods to deal with Collinearity", font = c("Arial", "11")), row = 0,
    column = 3, columnspan = 2, sticky = "w")
 
  ## add.noise = TRUE
  add.noise <- tclVar ("TRUE")
  add.noise.rb1 <- tkradiobutton (frameCenter, text="Yes", command=renderNoise)
  tkconfigure(add.noise.rb1,variable=add.noise,value="TRUE")
  add.noise.rb2 <- tkradiobutton (frameCenter, text="No", command=renderNoise)
  tkconfigure(add.noise.rb2,variable=add.noise,value="FALSE")
  add.noise.label <- tklabel (frameCenter, text="Add noise") 
  tkgrid (add.noise.label, row=1, column=3, sticky="w")
  tkgrid (add.noise.rb1, row=1, column=4, sticky="w")
  tkgrid (add.noise.rb2, row=2, column=4, sticky="w")


  ## add.noise.method = "reshuffling", add.noise.K = 1, add.noise.post.run.iter = 20, add.noise.pct.aug=10
  add.noise.method.label <- tklabel (frameCenter, text="Add noise methods")
  add.noise.method <- tclVar ("reshuffling")
  add.noise.method.rb1 <- tkradiobutton (frameCenter, text="reshuffling")
  tkconfigure(add.noise.method.rb1,variable=add.noise.method,value="reshuffling", command=renderNoiseMethod)
  add.noise.method.rb2 <- tkradiobutton (frameCenter, text="fading")
  tkconfigure(add.noise.method.rb2,variable=add.noise.method,value="fading", command=renderNoiseMethod)
  tkgrid (add.noise.method.label, row=3, column=3, sticky="w", columnspan=2)
  tkgrid (add.noise.method.rb1, row=4, column=3, sticky="w")
  tkgrid (add.noise.method.rb2, row=4, column=4, sticky="w")

  # cooling
  add.noise.K <- tclVar (1)
  add.noise.K.entry <- tkentry (frameCenter, width=10, textvariable=add.noise.K)
  add.noise.K.label <- tklabel (frameCenter, text="Cooling parameter")
  tkgrid (add.noise.K.label, row=5, column=3, sticky="w")
  tkgrid (add.noise.K.entry, row=6, column=3, sticky="w")

  # augmentation  
  add.noise.pct.aug <- tclVar (10)
  add.noise.pct.aug.entry <- tkentry (frameCenter, width=10, textvariable=add.noise.pct.aug)
  add.noise.pct.aug.label <- tklabel (frameCenter, text="Percent augmented")
  tkgrid (add.noise.pct.aug.label, row=5, column=4, sticky="w")
  tkgrid (add.noise.pct.aug.entry, row=6, column=4, sticky="w")

  # postrun
  add.noise.post.run.iter <- tclVar (20)
  add.noise.post.run.iter.entry <- tkentry (frameCenter, width=10, textvariable=add.noise.post.run.iter)
  add.noise.post.run.iter.label <- tklabel (frameCenter, text="# Iterations after imputation with noise")
  tkgrid(tklabel(frameCenter, text="        "), row = 7, column = 3, columnspan=2)
  tkgrid (add.noise.post.run.iter.label, row=8, column=3, sticky="w")
  tkgrid (add.noise.post.run.iter.entry, row=8, column=4, sticky="w")

  # buttons
  run.but    <- tkbutton (frameBottom, text="Run", command=onRunButton, width=15)
  continue.but <- tkbutton (frameBottom, text="Continue", command=onContinueButton, width=15)
  clear.but    <- tkbutton (frameBottom, text="Clear", command=onClearButton, width=15)
  exit.but    <- tkbutton (frameBottom, text="Exit", command=function() tkdestroy(this.gui), width=15)
  tkgrid (run.but, row=9, column=3)#, sticky="w")
  tkgrid (continue.but, row=9, column=4)#, sticky="w")
  tkgrid (clear.but, row=10, column=3)#, sticky="w")
  tkgrid (exit.but, row=10, column=4)#, sticky="w")

  # converged message
  converged.message <- tclVar ("")
  converged.entry <- tkentry (this.gui, width=92, textvariable=converged.message, state="disabled")
  tkgrid(converged.entry, row=12)
  
  tkfocus (this.gui)
  render()
}
