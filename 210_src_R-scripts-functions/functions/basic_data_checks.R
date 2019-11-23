#' @import data.table

#https://stackoverflow.com/questions/43688380/data-table-aggregate-function-doesnt-work-when-build-in-package

dupes_per_keycol <- function(DT,keycols,pct=TRUE) {
  data.table::setDT(DT)
  # dupe_counts <- DT[,
  #                   lapply(.SD,function(x) ifelse(length(unique(x)) >1,1,0)),
  #                   by=keycols][,lapply(.SD,sum),.SDcols=!keycols]

  if(pct == TRUE) {
#    return(dupe_counts/DT[,.N])

    tot_recs <-  DT[,.N]
    DT_out <- melt(DT[,
                      lapply(.SD,function(x) ifelse(length(unique(x)) >1,1,0)),
                      by=keycols][,lapply(.SD,sum),.SDcols=!keycols]
                   ,variable.name = "column"
                   ,value.name = "Dupe_count")
    DT_out$Total <- tot_recs
    DT_out[,Dupe_pct:= round(100*Dupe_count/Total,2)]
    setcolorder(DT_out, c("column", "Total", "Dupe_count","Dupe_pct"))
    return(DT_out)

  }
  else {
    return <- NULL
  }

}



NAs_per_col <- function(DT,pct=TRUE,NA_char = NA) {
  data.table::setDT(DT)

  if(!is.na(NA_char)) {
    #TO DO : Handle NA character other than NA
  }
  else {

  }

  if(pct == TRUE) {
    tot_recs <-  DT[,.N]
    DT_out <- melt(DT[,lapply(.SD, function(x) sum(is.na(x)))]
                   ,variable.name = "column"
                   ,value.name = "NA_count")
    DT_out$Total <- tot_recs
    DT_out[,NA_pct:= round(100*NA_count/Total,2)]
    setcolorder(DT_out, c("column", "Total", "NA_count","NA_pct"))
    return(DT_out)

  }
  else {
    #not required
  }
}



top_n_values_per_col <- function(DT,n=10)
{
  data.table::setDT(DT)
  tot_recs<-DT[,.N]

  for(var in names(DT))
  {
    temp_lol <- list(DT[,.N,by=var][order(-N)][1:n][,pct:=round(100*N/tot_recs,2)])
    names(temp_lol[[1]])[2] <- paste0(var,"_count")
    names(temp_lol[[1]])[3] <- paste0(var,"_pct")

    if(!exists("lol")) {
      lol <- temp_lol
      maxN <- length(lol[[1]][[1]])
    }
    else {
      lol <- append(lol,temp_lol)
      tempmaxN <-length(lol[[1]][[1]])
      if(tempmaxN > maxN) {
        maxN <- tempmaxN
      }
    }
  }

  return(setDT(data.frame(lol)))
}



