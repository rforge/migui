displayMissingDataPattern <- function () {
  
  this.gui <- tktoplevel()
  tktitle(this.gui) <- "Display Missing Data Plot"
  frameOverall <- tkframe(this.gui)
  frameLeft <- tkframe(frameOverall, relief="groove", borderwidth=4)
  frameRight <- tkframe(frameOverall, relief="groove", borderwidth=4)  
  frameBottom <- tkframe(frameOverall, relief="groove", borderwidth=4)  
  tkgrid(frameOverall)
  tkgrid(frameLeft, row=0, column=0, columnspan=9, rowspan=15)
  tkgrid(frameRight, row=0, column=9, rowspan=14)
  tkgrid(frameBottom, row=14, column=9)

  #### options ####
  # y.order
  y.order <- tclVar ("FALSE")
  y.order.rb1 <- tkradiobutton (frameRight, text="true")
  tkconfigure(y.order.rb1,variable=y.order,value="TRUE")
  y.order.rb2 <- tkradiobutton (frameRight, text="false")
  tkconfigure(y.order.rb2,variable=y.order,value="FALSE")
  tkgrid (tklabel (frameRight, text="Order y-axis by missing rate"), row=0, column=9, columnspan=2, sticky="w")
  tkgrid (y.order.rb1, row=1, column=9)
  tkgrid (y.order.rb2, row=1, column=10)
  
  # x.order
  x.order <- tclVar ("FALSE")
  x.order.rb1 <- tkradiobutton (frameRight, text="true")
  tkconfigure(x.order.rb1,variable=x.order, value="TRUE")
  x.order.rb2 <- tkradiobutton (frameRight, text="false")
  tkconfigure(x.order.rb2,variable=x.order, value="FALSE")
  tkgrid (tklabel (frameRight, text="Order x-axis by missing rate"), row=2, column=9, columnspan=2, sticky="w")
  tkgrid (x.order.rb1, row=3, column=9)
  tkgrid (x.order.rb2, row=3, column=10)
  
  # xlab
  xlab <- tclVar ("Index")
  xlab.entry <- tkentry (frameRight, width=25, textvariable=xlab)
  tkgrid (tklabel (frameRight, text="Label for x-axis"), row=4, column=9, columnspan=2, stick="w")
  tkgrid (xlab.entry, row=5, column=9, columnspan=2)
  
  # ylab
  ylab <- tclVar ("Variable")
  ylab.entry <- tkentry (frameRight, width=25, textvariable=ylab)
  tkgrid (tklabel (frameRight, text="Label for y-axis"), row=6, column=9, columnspan=2, stick="w")
  tkgrid (ylab.entry, row=7, column=9, columnspan=2)
  
  # main: title
#  main <- tclVar ("")
#  main.entry <- tkentry (frameRight, width=10, textvariable=main)
#  
#  tkgrid (tklabel (frameRight, text="Title"), column=col, row=1)
#  tkgrid (main.entry, column=col, row=2)
  
  # gray.scale
#  col <- col + 1
  gray.scale <- tclVar ("FALSE")
  gray.scale.rb1 <- tkradiobutton (frameRight, text="true")
  tkconfigure(gray.scale.rb1, variable=gray.scale, value="TRUE")
  gray.scale.rb2 <- tkradiobutton (frameRight, text="false")
  tkconfigure(gray.scale.rb2,variable=gray.scale,value="FALSE")
  tkgrid (tklabel (frameRight, text="Gray Scale"), row=8, column=9, columnspan=2, sticky="w")
  tkgrid (gray.scale.rb1, row=9, column=9)
  tkgrid (gray.scale.rb2, row=9, column=10)
  
  
  colors.simple <- c ("black", "white", "red", "violet", "blue", "green", "yellow", "orange")
  # obs.col
  obs.col <- tclVar ()
  tclvalue (obs.col) <- "blue"
  obs.col.comboBox <- ttkcombobox(frameRight, values=colors.simple, textvariable=obs.col, width=24)
  
  tkgrid (tklabel (frameRight, text="Colors for the observed"), row=10, column=9, columnspan=2, sticky="w")
  tkgrid (obs.col.comboBox, row=11, column=9, columnspan=2)
  
  # mis.col
  mis.col <- tclVar ()
  tclvalue (mis.col) <- "red" 
  mis.col.comboBox <- ttkcombobox(frameRight, values=colors.simple, textvariable=mis.col, width=24)
  tkgrid (tklabel (frameRight, text="Colors for the missing"), row=12, column=9, columnspan=2, sticky="w")
  tkgrid (mis.col.comboBox, row=13, column=9, columnspan=2)
  
  onPlotButton <- function () {
    options <- list()
    options$y.order <- as.logical (tclvalue(y.order))
    options$x.order <- as.logical (tclvalue(x.order))
    options$xlab <- as.character (tclvalue (xlab))
    options$ylab <- as.character (tclvalue (ylab))
 #  options$main <- as.character (tclvalue (main))
    options$gray.scale <- as.logical (tclvalue(gray.scale))
    options$obs.col <- as.character (tclvalue (obs.col))
    options$mis.col <- as.character (tclvalue (mis.col))
    
    missing.pattern <- tkrplot(frameLeft, fun=plotFunctionCreator(options), hscale=1.5)
    tkgrid(missing.pattern, "in"=frameLeft, row=0, column=0, columnspan=9, rowspan=14)
    tkfocus(this.gui) 
  }
  
  
  onPlotButton()
  plot.but <- tkbutton(frameBottom,text="Plot",command=onPlotButton, width=12)
  exit.but <- tkbutton(frameBottom,text="Exit",command=function() tkdestroy(this.gui), width=12)
  tkgrid(plot.but, row=14, column=9)
  tkgrid(exit.but, row=14, column=10)
  tkfocus(this.gui)
} 

plotFunctionCreator <- function (options) {
  return (function () {
        par( mar = c(1, 8, 1, 1 ))
        par( mgp = c( 1.5, .25, 0 ), oma=c(2, 2, 2, 0))
        par( bg="white", tcl=-0.2) 
        missing.pattern.plot (getMi(data), y.order=options$y.order, 
          x.order=options$x.order, 
          xlab="", ylab="", main="",#options$main, 
          gray.scale=options$gray.scale, 
          obs.col=options$obs.col, 
          mis.col=options$mis.col)
        mtext(options$ylab, side=2, outer=TRUE)
        mtext(options$xlab, side=1, outer=TRUE)
        mtext("Missing Pattern Plot", side=3, outer=TRUE, font=2, cex=1.2)
      })
}
