.onAttach <- function(...) {
  mylib <- dirname(system.file(package = "migui"))
  ver <- packageDescription("migui", lib = mylib)$Version
  builddate <- packageDescription("migui", lib = mylib)$Date
  cat(paste("\nmigui (Version ", ver, ", built: ", builddate, ")\n", sep = ""))
  if(!any(search()=="package:tcltk"))
    require(tcltk)
  if(!any(search()=="package:tkrplot"))
    require(tkrplot)
  if(!any(search()=="package:mi"))
    require(mi)
  #migui()
}
