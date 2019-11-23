#getting this error, if library()
# Error: 'dbDriver' is not an exported object from 'namespace:RPostgreSQL'


get_sql_data <- function(sql_file_name=NA,password= NA,text=NA,username = NA)
{

if(!require("pacman")) { install.packages("pacman") ;library(pacman) } 
pacman::p_load(RPostgreSQL,getPass,DBI,svDialogs)
    
  drv <- DBI::dbDriver("PostgreSQL")

  if(is.na(password) == TRUE) {
  con <- RPostgreSQL::dbConnect(drv, dbname = "gdwprod",
                   host = "gdwpdbreporting", port = 5432,
                   user = svDialogs::dlg_input(message = "Enter DWH username")$res, password = getPass::getPass())
  } else {
  con <- RPostgreSQL::dbConnect(drv, dbname = "gdwprod",
                                  host = "gdwpdbreporting", port = 5432,
                                  user = username, password = password)
  }


  if(is.na(text) == TRUE) {
    qry <- .getSQL(sql_file_name)
    dataset <- DBI::dbGetQuery(con, qry)
  } else {
    dataset <- DBI::dbGetQuery(con, text)
  }


  dbDisconnect(con)
  return(dataset)

}

.getSQL <- function(filepath){
  con = file(filepath, "r")
  sql.string <- ""

  while (TRUE){
    line <- readLines(con, n = 1)

    if ( length(line) == 0 ){
      break
    }

    line <- gsub("\\t", " ", line)

    if(grepl("--",line) == TRUE){
      line <- paste(sub("--","/*",line),"*/")
    }

    sql.string <- paste(sql.string, line)
  }

  close(con)
  return(sql.string)
}






