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

# MERGES ------------------------------------------------------------------

#' iterative_merge
#'
#' Merge a list of datasets
#'
#' This function allows you to make any type of merge of different datasets withing a list.
#'
#' @param dfs_list list of datasets
#' @param key join ID
#' @param ... additional parameters of merge
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#'
#' @return This function returns a merged dataset of all the datasets from \code{dfs_list}
#'
#' @export
#'
#' @example
#'iterative_merge(dfs_list = list(data.table(a=rep(1:2,each=3), b=1:6, key="a,b"),
#'                                data.table(a=0:1, bb=10:11, key="a")),
#'                 key = "a")
#'
iterative_merge <- function(dfs_list, key, ...){
  Reduce(function(x,y) merge(x, y, by = key, ...),
         x = dfs_list)
}
