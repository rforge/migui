.onAttach <- function(...) {
  mylib <- dirname(system.file(package = "migui"))
  ver <- packageDescription("migui", lib.loc = mylib)$Version
  builddate <- packageDescription("migui", lib.loc = mylib)$Date
  #cat(paste("\nmigui (Version ", ver, ", built: ", builddate, ")\n", sep = ""))
  packageStartupMessage(paste("\nmigui (Version ", ver, ", built: ", builddate, ")\n", sep = ""))
#  if(!any(search()=="package:tcltk"))
#    require(tcltk)
#  if(!any(search()=="package:tkrplot"))
#    require(tkrplot)
#  if(!any(search()=="package:mi"))
#    require(mi)
#  #migui()
}

info <- NULL
IMP <- NULL
preprocess.flg <- NULL
rand.imp.method <- NULL
