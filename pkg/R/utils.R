#setOption <- function(option, default, global=TRUE) {
#    opt <- if (is.null(current[option][[1]])) default else current[option][[1]]
#    if (global) putMi(option, opt)
#    else opt
#}

#info <- NULL
#IMP <- NULL
#gui <- NULL


## the following functions have been imported from Rcmdr
miEnv <- function(){
  pos <- match("miEnv", search())
  if (is.na(pos)) { # Must create it
    miEnv <- list()
    attach(miEnv, pos = length(search()) - 1)
    rm(miEnv)
    pos <- match("miEnv", search())
  }
  return(pos.to.env(pos))
}

putMi <- function(x, value) {
  if (missing (value)) {
    value <- x
  }
  assign (as.character (substitute (x)), value, envir = miEnv())
}

getMi <- function(x, mode="any"){
  get(as.character (substitute (x)), envir = miEnv(), mode = mode, inherits = FALSE)
}

lsMi <- function () {
  ls (envir = miEnv(), all=TRUE)
}

existsMi <- function (x, ...) {
  exists (as.character (substitute (x)), envir = miEnv(), ...)
}

rmMi <- function (x, ...) {
  rm (list=as.character (substitute (x)), envir = miEnv(), ...)    
}

removeMi <- function (x, ...) {
  rmMi (x, ...)
}

miTclSet <- function(name, value) {
  name <- ls(unclass(getMi(name))$env)
  tcl("set", name, value)
}



#mainWindow <- function() getMi("mainWindow")
#
#scriptWindow <- function() getMi("scriptWindow")
#
#outputWindow <- function() getMi("outputWindow")
#
#messagesWindow <- function() getMi("messagesWindow")
#





# for internationalization

#gettextMi <- function(...){
#    gettext(..., domain="R-mi")
#}

# functions for building dialog boxes
#
#GrabFocus <- function(value){
#    if (missing(value)){
#        getMi("grab.focus")
#    }
#    else{
#        putMi("grab.focus", value)
#    }
#}
