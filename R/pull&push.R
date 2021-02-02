# MONGO DB ----------------------------------------------------------------

#' mongo_manipulation
#'
#' Read or pull or modify data in a cluster database
#'
#' This function allows you to push, pull or modify a database
#'
#' @param collection your cluster
#' @param database name of database inside the \code{cluster}
#' @param host host of the \code{cluster}
#' @param username Your username to enter to the \code{cluster}
#' @param password your password to enter to the \code{cluster}
#' @param mongo_choice choice that we want to do with a database in a collection
#' @param push_record dataset we want to push to the collection
#' @param id_filter variable of the dataset that we want to filter when we pull this.
#' @param element_id_filter observation of \code{id_variable} that will allow to pull an specific table.
#' @param id_update variable of the dataset that we want to update.
#' @param element_id_update values to update for the variable \code{id_update}
#'
#' @author Eduardo Trujillo
#'
#' @import mongolite
#' @import data.table
#' @importFROM stringr str_glue str_remove_all str_c
#' @importFROM jsonlite toJSON
#'
#' @return
#' #' "This function *different results* based on \code{mongo_choice} variable:"
#' \itemize{
#'   \item If \code{mongo_choice = "push"} then you only need to use the variable \code{push_record} to push the table with the information to your MongoDB collection.
#'   \item If \code{mongo_choice = "pull"} is not necessary to use other variable, because you are going to pull the dataset from the config file.
#'   \item If \code{mongo_choice = "pull"} and use the variables \code{id_filter} and \code{element_id_filter} to pull a table from the dataset.
#'   \item If \code{mongo_choice = "update_record"} and use the variables \code{id_filter}, \code{element_id_filter}, \code{id_update}, \code{element_id_update} to update a record from a table from the dataset.
#'   \item If \code{mongo_choice = "delete_record"} and use the variables \code{id_filter} and \code{element_id_filter} to delete a table from the dataset.
#' }
#'
#' @export
#'
#' @note You can use this function inside your shiny app.
#'
#' @example
#' \dontrun{
#' *1. push*
#' mongo_manipulation(mongo_choice = "push",
#'                    push_record = data.table(
#'                       user           = c("user1", "user2"),
#'                       password       = c("pass1", "pass2"),
#'                       permissions    = c("admin", "standard"),
#'                       name           = c("User One", "User Two"),
#'                       favorites      = list(c("AAPL", "GOOG", "NFLX"), c("MA", "V", "FB")),
#'                       last_symbol    = c("GOOG", "NFLX"),
#'                       user_settings  = list(tibble(mavg_short = 20, mavg_long = 50, time_window = 180),
#'                       tibble(mavg_short = 30, mavg_long = 90, time_window = 365)),
#'                       account_created = c(ymd_hms("2019-05-12 12:31:09"), ymd_hms("2019-06-04 06:18:02"))
#'                    ))
#'
#' *2. Pull dataset*
#' mongo_manipulation(mongo_choice = "pull")
#'
#' *3. Pull table from dataset*
#' mongo_manipulation(mongo_choice = "pull",
#'                    id_filter = "user",
#'                    element_id_filter = "user1")
#'
#' *4. Update record from table from dataset*
#' mongo_manipulation(push_record = pull_info,
#'                    mongo_choice = "update_record",
#'                    id_filter = "user",
#'                    element_id_filter = "user1",
#'                    id_update = "user_settings",
#'                    element_id_update = list(data.table(
#'                        mavg_short = 12,
#'                        mavg_long = 70,
#'                        time_window = 710
#'                    )))
#'
#' *5. Delete table from dataset*
#' mongo_manipulation(mongo_choice = "delete_record",
#'                    id_filter = "user",
#'                    element_id_filter = "user1")
#' }
#'
mongo_manipulation <- function(collection = config$collection,
                               database = config$database,
                               host = config$host,
                               username = config$username,
                               password = config$password,
                               mongo_choice = c("push", "pull",
                                                "update_record", "delete_record"),
                               push_record = NULL,
                               id_filter = NULL,
                               element_id_filter = NULL,
                               id_update = NULL,
                               element_id_update = NULL){

  mongo_connection <- mongo(
    collection = collection,
    url = str_glue("mongodb+srv://{username}:{password}@{host}/{database}")
  )

  if(is.null(id_filter) && is.null(element_id_filter)){
    filter_query <- "{}"
  }else{
    filter_query <- data.table(element_id_filter) %>%
      setnames(id_filter) %>% toJSON(POSIXt = "mongo") %>%
      str_remove_all(pattern = "^\\[|\\]$")
  }

  if(mongo_choice == "push"){
    push_record %>% mongo_connection$insert()

  }else if(mongo_choice == "pull"){
    pull_info <<- mongo_connection$find(query = filter_query) %>% as.data.table()

  }else if(mongo_choice == "update_record"){

    filter_df <- as.data.table(push_record)[eval(parse(text = id_filter)) == element_id_filter,]
    filter_df[[id_update]] <- element_id_update

    update_query <- filter_df %>% toJSON(POSIXt = "mongo") %>%
      str_remove_all(pattern = "^\\[|\\]$")

    mongo_connection$update(
      query = filter_query,
      update = str_c('{"$set" : ', update_query, '}')
    )

  }else if(mongo_choice == "delete_record"){
    mongo_connection$remove(query = filter_query)
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
