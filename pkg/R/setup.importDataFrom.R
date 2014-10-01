importDataFrom <- function(){
  availableDataFormat <- c("CSV", "Tab-Delimited", "Stata", "SPSS")
  # Available Data Format
  allDataFormat <- c("{{All files} *} {{Comma-delimited files} {.csv}}",
      "{{All files} *} {{Tab Delimited} {.txt}}",
      "{{All files} *} {{Stata files} {.dta}}",
      "{{All files} *} {{SPSS} {.sav}}")
  names (allDataFormat) <- availableDataFormat

  readFunctions <- c( function (dataFile) { read.csv (dataFile, header=TRUE) },
      function (dataFile) { read.table(dataFile, header=TRUE) },
      function (dataFile) { read.dta(dataFile, convert.factors=FALSE) },
      function (dataFile) { read.spss(dataFile, use.value.labels=TRUE, to.data.frame=TRUE) })
  names (readFunctions) <- availableDataFormat

  onChange <- function () {
    getDataButton <- tkbutton(this.gui, text="Select a Data File", command=generateReadCommand(as.character(tclvalue(dataFormat))))

    tkgrid(getDataButton, row=1, column=1)
  }

  generateReadCommand <- function (choice) {
    return (function () {
          dataFile <- tclvalue(tkgetOpenFile(filetypes=allDataFormat[choice]))
          if (dataFile == "") {
            return (NULL)
          }
          if (existsMi (data)) {
            rmMi (data)
          }
          if (existsMi (info)) {
            rmMi (info)
          }
          data <- readFunctions[[choice]](dataFile)
          info <- mi.info(data)
          putMi (data)
          putMi(info)
          tkdestroy (this.gui)
        })
  }

  this.gui <- tktoplevel(width=400, height=50)
  tktitle (this.gui) <- "Import Data From..."

  # combobox for data format
  dataFormat <- tclVar(availableDataFormat[1])
  dataFormat.comboBox <- ttkcombobox(this.gui, values=availableDataFormat, textvariable=dataFormat, width=15)
  tkgrid (tklabel(this.gui, text="Choose a Data Format", font=c("Arial", 10)), row=0, column=0)
  tkgrid (dataFormat.comboBox, row=1, column=0, sticky="e")
  tkbind (dataFormat.comboBox, "<<ComboboxSelected>>", onChange)
  exitButton <- tkbutton(this.gui, text="Exit", command=function() tkdestroy(this.gui), width=10)
  tkgrid(exitButton, row=1, column=2)
  onChange()
  tkfocus(this.gui)
}
