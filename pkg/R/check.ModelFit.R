## ASSUME mi object is in a variable called "IMP"
checkModelFit <- function () {
  IMP <- getMi("IMP")
  onSettingChange <- function () {
    # collect information for plot
    thisChain <- as.numeric(tclvalue(whichChain))
    grayScale <- as.logical(tclvalue(gray.scale))
    currentVar <- as.character(tclvalue(current.var)) 
    
    mi.object <- imp(IMP, thisChain)
    if(IMP@preprocess){
      IMP@data <- mi.preprocess(IMP@data, info=IMP@mi.info)$data
    }
    outcome.var <- as.data.frame(IMP@data[ , names(mi.object)])
    names(outcome.var) <- names(mi.object)        
    
    className <- class(mi.object[[currentVar]])
    if(className == "mi.binary"){
      X   <- mi.object[[currentVar]]
      fit <- as.numeric(fitted(X))
      Y   <- as.numeric(outcome.var[[currentVar]])
      res     <- residuals(X, Y)
      sigma   <- sigma.hat(X)
      vrb.obs <- Y
      vrb.imp <- imputed(X, Y)
      img01 <- tkrplot(frameLeft, hscale=.8, vscale=.8, fun=function(){ par(bg="white", mar=c(3,3,4,1), mgp=c(1.5,0.2,0), tcl=-0.2);mi.hist (X, Yobs=vrb.obs, xlab = "", main = currentVar, gray.scale = grayScale )})
      img02 <- tkrplot(frameLeft, hscale=.8, vscale=.8, fun=function(){ par(bg="white", mar=c(3,3,4,1), mgp=c(1.5,0.2,0), tcl=-0.2);binnedplot ( fit[ !is.na(Y)], res[ !is.na(Y)], nclass = sqrt( length( fit[  !is.na(Y)])), main = currentVar )})
      img03 <- tkrplot(frameLeft, hscale=.8, vscale=.8, fun=function(){ par(bg="white", mar=c(3,3,4,1), mgp=c(1.5,0.2,0), tcl=-0.2);mi.scatterplot( Yobs=vrb.obs, vrb.imp, fit, xlab = "Predicted", ylab = "", main = currentVar, gray.scale = grayScale )})
      img04 <- tkrplot(frameLeft, hscale=.8, vscale=.8, fun=function(){ par(bg="white", mar=c(3,3,4,1), mgp=c(1.5,0.2,0), tcl=-0.2);plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )})
    }
    else if(className == "mi.categorical"){
      X   <- mi.object[[currentVar]]
      fit <- as.numeric(fitted(X))
      Y   <- as.numeric(outcome.var[[currentVar]])
      res     <- residuals(X, Y)
      sigma   <- sigma.hat(X)
      vrb.imp <- as.numeric(imputed(X, Y))
      vrb.obs <- Y
      img01 <- tkrplot(frameLeft, hscale=.8, vscale=.8, fun=function(){ par(bg="white", mar=c(3,3,4,1), mgp=c(1.5,0.2,0), tcl=-0.2); mi.hist(X, Y, type = "Categorical", xlab = "", main = currentVar, gray.scale = grayScale)})
      img02 <- tkrplot(frameLeft, hscale=.8, vscale=.8, fun=function(){ par(bg="white", mar=c(3,3,4,1), mgp=c(1.5,0.2,0), tcl=-0.2); mi.scatterplot(Yobs = vrb.obs, vrb.imp, fit, xlab = "predicted", ylab = "", main = currentVar, gray.scale = grayScale)})
      img03 <- tkrplot(frameLeft, hscale=.8, vscale=.8, fun=function(){ par(bg="white", mar=c(3,3,4,1), mgp=c(1.5,0.2,0), tcl=-0.2); plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )})
      img04 <- tkrplot(frameLeft, hscale=.8, vscale=.8, fun=function(){ par(bg="white", mar=c(3,3,4,1), mgp=c(1.5,0.2,0), tcl=-0.2); plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )})
    }
    else if(className == "mi.polr"){
      X   <- mi.object[[currentVar]]
      Y   <- outcome.var[[currentVar]]
      fit     <- mi:::.factor2num(fitted(X))
      res     <- mi:::.factor2num(residuals(X, Y))
      sigma   <- mi:::.factor2num(sigma.hat(X))
      vrb.obs <- mi:::.factor2num(Y)
      vrb.imp <- mi:::.factor2num(imputed(X, Y))
      img01 <- tkrplot(frameLeft, hscale=.8, vscale=.8, fun=function(){ par(bg="white", mar=c(3,3,4,1), mgp=c(1.5,0.2,0), tcl=-0.2); mi.hist(X, Y, xlab = "", main = currentVar, gray.scale = grayScale)})
      img02 <- tkrplot(frameLeft, hscale=.8, vscale=.8, fun=function(){ par(bg="white", mar=c(3,3,4,1), mgp=c(1.5,0.2,0), tcl=-0.2); mi.scatterplot(vrb.obs, vrb.imp, fit, xlab = "predicted", ylab = "", main = currentVar, gray.scale = grayScale)})
      img03 <- tkrplot(frameLeft, hscale=.8, vscale=.8, fun=function(){ par(bg="white", mar=c(3,3,4,1), mgp=c(1.5,0.2,0), tcl=-0.2); plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )})
      img04 <- tkrplot(frameLeft, hscale=.8, vscale=.8, fun=function(){ par(bg="white", mar=c(3,3,4,1), mgp=c(1.5,0.2,0), tcl=-0.2); plot( 0, 0, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", frame.plot = FALSE )})
    }
    else{
      X     <- mi.object[[currentVar]]
      fit   <- fitted(X)
      Y     <- outcome.var[[currentVar]]
      res   <- residuals(X, Y)
      sigma <- sigma.hat(X)
      vrb.obs <- Y
      vrb.imp <- imputed(X, Y)
   
      img01 <- tkrplot(frameLeft, hscale=.8, vscale=.8, fun=function(){ par(bg="white", mar=c(3,3,4,1), mgp=c(1.5,0.2,0), tcl=-0.2); mi.hist(X, vrb.obs, xlab="", main = currentVar, gray.scale = grayScale)})
      img02 <- tkrplot(frameLeft, hscale=.8, vscale=.8, fun=function(){ par(bg="white", mar=c(3,3,4,1), mgp=c(1.5,0.2,0), tcl=-0.2); residual.plot( fit, res, sigma, main = currentVar, gray.scale = grayScale)})
      img03 <- tkrplot(frameLeft, hscale=.8, vscale=.8, fun=function(){ par(bg="white", mar=c(3,3,4,1), mgp=c(1.5,0.2,0), tcl=-0.2); binnedplot ( fit[ !is.na(Y)], res[ !is.na(Y)], nclass = sqrt( length( fit[  !is.na(Y)] ) ), main = currentVar)})
      img04 <- tkrplot(frameLeft, hscale=.8, vscale=.8, fun=function(){ par(bg="white", mar=c(3,3,4,1), mgp=c(1.5,0.2,0), tcl=-0.2); mi.scatterplot( vrb.obs, vrb.imp, fit, xlab = "predicted", ylab = "",  main = currentVar, gray.scale = grayScale)})
    }
    tkgrid(img01, "in"=frameLeft, row=0, column=0, columnspan=5, rowspan=5)
    tkgrid(img02, "in"=frameLeft, row=0, column=5, columnspan=5, rowspan=5)
    tkgrid(img03, "in"=frameLeft, row=5, column=0, columnspan=5, rowspan=5)
    tkgrid(img04, "in"=frameLeft, row=5, column=5, columnspan=5, rowspan=5)
  }
  
  
  this.gui <- tktoplevel()
  tktitle(this.gui) <- "Check Model Fit via Plots"
  frameOverall <- tkframe(this.gui, relief="groove",borderwidth=4)
  frameLeft <- tkframe(frameOverall, relief="groove",borderwidth=4)
  frameRight <- tkframe(frameOverall, relief="groove",borderwidth=4)
  frameBottom <- tkframe(frameOverall, relief="groove",borderwidth=4)
  tkgrid(frameOverall)
  tkgrid(frameLeft, row=0, column=0, rowspan=10, columnspan=10)
  tkgrid(frameRight, row=0, column=10, rowspan=8, columnspan=2)
  tkgrid(frameBottom, row=8, column=10, rowspan=2, columnspan=2)
  
  #### options ####    
  # which variable    
  
  #tkgrid(tklabel(frameLeft, text = ""), row = 0,
  #  column = 3, columnspan = 2, rowspan=10, sticky = "w")
  
  
  all.vars <- names (imp(IMP, m=1))
  current.var <- tclVar (all.vars[1])
  current.var.comboBox <- ttkcombobox (frameRight, values=all.vars, textvariable=current.var, width=25)
  tkgrid (tklabel (frameRight, text="Variable"), row=0, column=3, sticky="w", columnspan=2)
  tkgrid (current.var.comboBox, row=1, column=3, sticky="w", columnspan=2)
  tkbind (current.var.comboBox, "<<ComboboxSelected>>", onSettingChange)
  
  # which chain
  numberOfChain <- m(IMP)
  whichChain <- tclVar ("1")
  whichChain.comboBox <- ttkcombobox(frameRight, values=1:numberOfChain, textvariable=whichChain, width=25)    
  tkgrid (tklabel (frameRight, text="Plot which chain of impution?"), row=2, column=3, sticky="w", columnspan=2)
  tkgrid (whichChain.comboBox, row=3, column=3, sticky="w", columnspan=2)
  tkbind (whichChain.comboBox, "<<ComboboxSelected>>", onSettingChange)
  
  # gray.scale
  #col <- col + 1
  gray.scale <- tclVar ("FALSE")
  gray.scale.rb1 <- tkradiobutton (frameRight, text="Yes ")
  tkconfigure(gray.scale.rb1, variable=gray.scale, value="TRUE", command=onSettingChange)
  gray.scale.rb2 <- tkradiobutton (frameRight, text="No")
  tkconfigure(gray.scale.rb2, variable=gray.scale, value="FALSE", command=onSettingChange)
  tkgrid (tklabel (frameRight, text="Gray Scale"), row=4, column=3, sticky="w")
  tkgrid (gray.scale.rb1, row=5, column=3, sticky="w")
  tkgrid (gray.scale.rb2, row=5, column=4, sticky="w")
  
  
  exit.but  <- tkbutton (frameBottom, text="Exit", command=function() tkdestroy(this.gui), width=15)
  tkgrid (exit.but, row=10, column=4, stick="se")
  
  onSettingChange()
  tkfocus(this.gui) 
}
