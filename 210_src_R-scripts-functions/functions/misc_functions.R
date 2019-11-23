#Misc functions

write2clip <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,file="clipboard-4056",sep="\t",row.names=row.names,col.names=col.names,...)
}



load_packages <- function(pkg_names) {
  #invisible lapply console output
  invisible(lapply(pkg_names
                   ,FUN=function(x) {
                     if(!require(x,character.only=TRUE,quietly = TRUE))
                     {
                       install.packages(x);
                       library(x,character.only=TRUE,quietly = TRUE)
                     }
                   }))

  }


.onAttach <- function(libname, pkgname) {
#Source : https://stackoverflow.com/questions/36699272/why-is-message-a-better-choice-than-print-in-r-for-writing-a-package

  vec <- lsf.str("package:cutlery")

#  packageStartupMessage(vec)
  cat("Functions in the package:\n")
  cat(ls.str("package:cutlery"),sep = "\n")
  #warning(vec)

}



#Source : https://github.com/tidyverse/lubridate/issues/682
#Function to assign fiscal year to a date
#Below fixes the issue with lubridate::quarter
fiscal_year <- function(x, with_year = FALSE, fiscal_start = 1) {
  fs <- fiscal_start - 1
  shifted <- seq(fs, 11 + fs) %% 12 + 1
  m <- lubridate::month(x)
  quarters <- rep(1:4, each = 3)
  s <- match(m, shifted)
  q <- quarters[s]
  if (with_year) {
    uq <- quarters[m]
    inc_year <- (m >= fiscal_start) * (fiscal_start != 1)
    lubridate::year(x) + inc_year + q/10
  }
  else q
}



chr2fact <- function(DT) {
  data.table::setDT(DT)

  changeCols<- names(Filter(is.character, head(DT,2)))
  DT[, (changeCols) := lapply(.SD, as.factor),.SDcols = changeCols]
  return(DT)

}



int2fact <- function(DT) {
  data.table::setDT(DT)

  changeCols<- names(Filter(is.integer, head(DT,2)))
  DT[, (changeCols) := lapply(.SD, as.factor),.SDcols = changeCols]
  return(DT)

}



num2fact <- function(DT) {
  data.table::setDT(DT)

  changeCols<- names(Filter(is.numeric, head(DT,2)))
  DT[, (changeCols) := lapply(.SD, as.factor),.SDcols = changeCols]
  return(DT)

}




blankchr2NA <- function(DT) {
  data.table::setDT(DT)

  changeCols<- names(Filter(is.character, head(DT,2)))
  DT[, (changeCols) := lapply(.SD, function(x) ifelse(x=="",NA,x))
     ,.SDcols = changeCols]
  return(DT)

}



