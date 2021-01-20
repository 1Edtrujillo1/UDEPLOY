# MONGO DB ----------------------------------------------------------------

#' mongo_Listread_or_write
#'
#' Read or pull data in a cluster database
#'
#' This function allows you to pull a json file to the database o read information in the database
#'
#' @param collection your cluster
#' @param database name of database inside the \code{cluster}
#' @param host host of the \code{cluster}
#' @param username Your username to enter to the \code{cluster}
#' @param password your password to enter to the \code{cluster}
#' @param do \code{read} database in the cluster OR \code{write} information in the database
#' @param json_to_write if \code{do} is \code{write} then this parameter represent the information
#'
#' @author Eduardo Trujillo
#'
#' @import mongolite
#' @importFROM jsonlite toJSON fromJSON
#'
#' @return This function returns two possible results depend on \code{do} parameter:
#' \itemize{
#' if \code{do} = read then we return the database information as a list as a global variable
#' if \code{do} = write then write a JSON file (or other) to the database
#' }
#' @export
#'
#' @note Add this function inside a UI in your shiny app.
#'
#' @example
#' \dontrun{
#' udeploy::mongo_Listread_or_write(do = "write",
#' json_to_write = jsonlite::fromJSON("file.json"))
#' }
#'
mongo_Listread_or_write <- function(collection = config$collection,
                                    database = config$database,
                                    host = config$host,
                                    username = config$username,
                                    password = config$password,
                                    do = c("read", "write"),
                                    json_to_write = NULL){

  mongo_connection <- mongo(
    collection = collection,
    url = str_glue("mongodb+srv://{username}:{password}@{host}/{database}")
  )

  if(do == "read"){
    json_information <<- mongo_connection$find() %>% toJSON() %>%
      str_remove_all(pattern = "^\\[|\\]$") %>% fromJSON()

  }else if(do == "write"){
    json_to_write %>% mongo_connection$insert()
  }
  mongo_connection$disconnect()
}
