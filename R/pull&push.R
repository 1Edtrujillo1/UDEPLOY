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
#' "This function returns *different results* based on \code{mongo_choice} variable:"
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
#' @note
#'\itemize{
#'   \item You can use this function inside your shiny app.
#'   \item If \code{mongo_choice = "pull"} then the variable \code{pull_info} is going to be saved in your global environment with the pull information. So is not necessary to assign this function.
#' }
#'
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


#' pull_from_environment
#'
#' particular pull data
#'
#' This function allows you to pull a database similar with the function *mongo_manipulation*
#'
#' @param config_file config file that confain the database information
#' @param id_filter variable of the dataset that we want to filter when we pull this.
#' @param element_id_filter observation of \code{id_variable} that will allow to pull an specific table.
#'
#' @author Eduardo Trujillo
#'
#' @return
#' "This function allows you to pull dataset without overwriting the previous pulled."
#' \itemize{
#'   \item If \code{mongo_choice = "pull"} is not necessary to use other variable, because you are going to pull the dataset from the config file.
#'   \item If \code{mongo_choice = "pull"} and use the variables \code{id_filter} and \code{element_id_filter} to pull a table from the dataset.
#' }
#'
#' @export
#'
#' @note
#'\itemize{
#'   \item You can use this function inside your shiny app.
#'   \item Since the function  *mongo_manipulation* pull a variable \code{pull_info} in the global environment, and if we want to pull from different *cofig* files. Then use this functions to not overwrite the variable \code{pull_info}, since now is written in son environment.
#' }
#'
#' @example
#' \dontrun{
#' Sys.setenv(R_CONFIG_ACTIVE_0 = "default")
#' Sys.setenv(R_CONFIG_ACTIVE_1 = "USERS")
#' config1 <- config::get(file = "config.yml", config = Sys.getenv("R_CONFIG_ACTIVE_1"))
#' config2 <- config::get(file = "config.yml", config = Sys.getenv("R_CONFIG_ACTIVE_2"))
#'
#' *1. Pull dataset*
#' pull_from_environment(config_file = "config1")
#'
#' *2. Pull table from dataset*
#' pull_from_environment(config_file = "config2",
#'                    id_filter = "user",
#'                    element_id_filter = "user1")
#' }
#'
pull_from_environment <- function(config_file,
                                  id_filter = NULL, element_id_filter = NULL){

  mongo_manipulation(mongo_choice = "pull",
                     collection = config_file$collection,
                     database = config_file$database,
                     host = config_file$host,
                     username = config_file$username,
                     password = config_file$password,
                     id_filter = id_filter,
                     element_id_filter = element_id_filter)

  # 1.0 Create new environment.
  NEW_environment <- new.env()

  # 2.0 Add element to the environment.
  NEW_environment$NEW_element <- pull_info

  # 3.0 Delete variable in the global environment.
  rm(pull_info, envir = .GlobalEnv)

  # 4.0 Extract value from the environment.
  base::get("NEW_element", envir = NEW_environment) %>%
    return()
}


# S3 AWS  -----------------------------------------------------------------

#' RDs_and_S3
#'
#' Read an S3 bucket or push, pull or delete an RDs data from the S3 bucket.
#'
#' This function allows you to read an S3 bucket or manipulate an RDs data with S3 AWS
#'
#' @param dfs dataset or a list of datasets
#' @param objects vector of files in S3 (it could be folder/file path)
#' @param bucket name of your S3 bucket
#' @param choice choice that we want to do with the RDs data
#'
#' @author Eduardo Trujillo
#'
#' @importFROM aws.s3 bucketlist get_bucket s3saveRDS s3readRDS delete_object
#' @importFROM stringr str_c str_glue
#' @importFROM purrr walk2 map walk
#'
#' @return
#' "This function returns *different results* based on \code{choice} variable:"
#' \itemize{
#'   \item If \code{choice = "watch_bucket"} then you only need to use the variable \code{bucket} to see what is inside the AWS S3 bucket.
#'   \item If \code{choice = "push"} then you need to use the variable \code{dfs} representing the dataset (or a list of datasets) in your R session, the  \code{objects} representing the path on AWS S3. and the name of your \code{bucket}.
#'   \item If \code{choice = "pull"} then you need to use  the variable \code{objects} representing a vector of path of your RDs data on AWS S3. and the name of your \code{bucket} to pull in our R-session.
#'   \item If \code{choice = "delete"} then you need to use  the variable \code{objects} representing a vector of path of your RDs data on AWS S3. and the name of your \code{bucket} to delete in AWS S3.
#' }
#'
#' @export
#'
#' @note
#'\itemize{
#'   \item You can use this function inside your shiny app.
#'   \item If \code{mongo_choice = "push"} then in AWS S3 is going to be an RDs data (s).
#' }
#'
#' @example
#' \dontrun{
#' df <- datasets::iris
#' Sys.setenv("AWS_ACCESS_KEY_ID" = "ACCESS KEY",
#'            "AWS_SECRET_ACCESS_KEY" = "SECRET ACCESS KEY",
#'            "AWS_DEFAULT_REGION" = "region")
#'
#' *0.watch bucket*
#' RDs_and_S3(bucket = "aimagination-files",choice = "watch_bucket")
#'
#' *1. push RDs data*
#' *1.1 One object*
#' RDs_and_S3(bucket = "aimagination-files",
#'            dfs = df,
#'            objects = "file/example",
#'            choice = "push")
#' *1.2 Multiple Objects*
#'RDs_and_S3(bucket = "aimagination-files",
#'           dfs = list(df,df,df),
#'           objects = c("file/example1", "example2", "file2/example3"), #are the names that I wanted.
#'           choice = "push")
#'
#' *2. Pull RDs data*
#' *2.1 One object*
#' RDs_and_S3(bucket = "aimagination-files",
#'            objects = "file/example",
#'            choice = "pull")
#' *2.2 Multiple objects*
#' RDs_and_S3(bucket = "aimagination-files",
#'            objects = c("file/example1", "example2","file2/example3"),
#'            choice = "pull")
#'
#' *3. Delete RDs data on AWS S3*
#' *3.1 One object*
#'  RDs_and_S3(bucket = "aimagination-files",
#'             objects = "file/example",
#'             choice = "delete")
#' *3.2 Multiple objects*
#'  RDs_and_S3(bucket = "aimagination-files",
#'             objects = c("file/example1", "example2", "file2/example3"),
#'             choice = "delete")
#' }
#'
RDs_and_S3 <- function(dfs = NULL, objects = NULL, bucket,
                       choice = c("watch_bucket", "push", "pull", "delete")){

  bucketlist()

  if(choice == "watch_bucket"){

    get_bucket(bucket) %>% return()

  }else if(choice == "push"){
    if(!is.null(dfs %>% dim())){
      s3saveRDS(x = dfs, object = str_c(objects, "rds", sep = "."), bucket = bucket)
    }else{
      walk2(dfs, objects,
            ~ s3saveRDS(x = ..1, object = str_c(..2, "rds", sep = "."),
                        bucket = bucket)
      )
    }
  }else if(choice == "pull"){
    map(objects,
        ~s3readRDS(object = str_glue("s3://{bucket}/{.x}.rds"),
                   bucket = bucket)) %>% return()

  }else if(choice == "delete"){
    walk(objects,
         ~ delete_object(object = str_c(..1, "rds", sep = "."),
                         bucket = bucket))
  }
}

# SQL DB ------------------------------------------------------------------

#' sql_manipulation
#'
#' push or pull or watch the information of the database
#'
#' This function allows you to push or pull or watch the information of the database
#'
#' @param dsn our \code{server} if is local
#' @param server our \code{server} if is not local
#' @param database database in the \code{server}
#' @param uid Your username to enter to the \code{server}
#' @param pwd your password to enter to the \code{server}
#' @param sql_choice choice that we want to do with a table in a database
#' @param df_identifier string name that identify a table in a database.
#' @param table_name name of the table
#' @param df dataset that we want to pull to the database
#' @param attributes variables to select from the table
#'
#' @author Eduardo Trujillo
#'
#' @importFROM DBI dbConnect dbWriteTable dbGetQuery dbListTables dbListFields dbDisconnect
#' @importFROM odbc odbc
#' @importFROM stringr str_glue str_c str_subset
#' @importFROM purrr map set_names
#'
#' @return
#' "This function returns *different results* based on \code{mongo_choice} variable:"
#' \itemize{
#'   \item If \code{sql_choice = "check_connection"} then returns the SQL connection
#'   \item If \code{sql_choice = "push"} then you need to use the parameters  \code{df_identifier}, \code{table_name}, \code{df} and if you want to select specific variables tu push in the table use the parameter \code{attributes}. All of these to push a table to the database.
#'   \item If \code{sql_choice = "pull"} then  you need to use the parameters  \code{df_identifier} and  \code{table_name} (or use only \code{table_name} indicating the  \code{df_identifier} and  \code{table_name} together) and if you want to select specific variables tu pul from the table use the parameter \code{attributes}. All of these to pull to our R-studio session the table from the database.
#'   \item If \code{sql_choice = "tables_info"} then you need to use the parameter \code{df_identifier} to bring a list of all the tables names (with its variables).
#' }
#'
#' @export
#'
#' @note If there is something wrong with the connection will return a message string.
#'
#' @example
#' \dontrun{
#' *0. Check Connection*
#' sql_manipulation(server = "server,
#'                  database = "database name",
#'                  uid = "user name",
#'                  pwd = "pwd",
#'                  sql_choice = "check_connection")
#' *1. push*
#' library(datasets)
#' *1.1 specific variables*
#' sql_manipulation(server = "server",
#'                  database = "database name",
#'                  uid = "user name",
#'                  pwd = "password",
#'                  sql_choice = "push",
#'                  df_identifier = "PERSONAL",
#'                  table_name = "iris_df",
#'                  df = iris,
#'                  attributes = c("Petal.Width", "Species"))
#' *1.2 all variables*
#' sql_manipulation(server = "server",
#'                  database = "database name",
#'                  uid = "user name",
#'                  pwd = "password",
#'                  sql_choice = "push",
#'                  df_identifier = "PERSONAL",
#'                  table_name = "iris_df",
#'                  df = iris)
#' *2. pull*
#' *2.1 specific variables*
#' *2.1.1 with df_identifier*
#' sql_manipulation(server = "server",
#'                  database = "database name",
#'                  uid = "user name",
#'                  pwd = "password",
#'                  sql_choice = "pull",
#'                  df_identifier = "PERSONAL",
#'                  table_name = "TABLE1",
#'                  attributes = c("TICKET", "DATE"))
#'
#' *2.1.2 without df_identifier*
#'sql_manipulation(server = "server",
#'                 database = "database name",
#'                 uid = "user name",
#'                 pwd = "password",
#'                 sql_choice = "pull",
#'                 table_name = "PERSONAL_TABLE1",
#'                 attributes = c("TICKET", "DATE"))
#' *2.2 all variables*
#' *2.2.1 with df_identifier*
#'sql_manipulation(server = "server",
#'                 database = "database name",
#'                 uid = "user name",
#'                 pwd = "password",
#'                 sql_choice = "pull",
#'                 df_identifier = "PERSONAL",
#'                 table_name = "TABLE1")
#' *2.2.2 without df_identifier*
#'sql_manipulation(server = "server",
#'                 database = "database name",
#'                 uid = "user name",
#'                 pwd = "password",
#'                 sql_choice = "pull",
#'                 table_name = "PERSONAL_TABLE1")
#' *3. tables info*
#'info <- sql_manipulation(server = "server",
#'                         database = "database name",
#'                         uid = "user name",
#'                         pwd = "password",
#'                         sql_choice = "tables_info",
#'                         df_identifier = "PERSONAL")
#' *4. pull al variables from all datasets*
#'map2(names(info), info,
#'~ sql_manipulation(server = "server",
#'                   database = "database name",
#'                   uid = "user name",
#'                   pwd = "password",
#'                   sql_choice = "pull",
#'                   table_name = .x,
#'                  attributes = .y)) %>%
#'set_names(names(info))
#'
#' }
#'
sql_manipulation <- function(dsn = NULL, server = NULL,
                             database, uid, pwd,
                             sql_choice = c("check_connection", "push", "pull", "tables_info"),
                             df_identifier = NULL, table_name = NULL,
                             df = NULL, attributes = NULL){

  options(warn = -1) #warn = -1 hide warnings

  SQL_connection <- tryCatch({
    dbConnect(drv = odbc(),
              driver = "SQL Server",
              dsn = dsn,
              server = server,
              database = database,
              uid = uid,
              pwd = pwd,
              port = 1433)
  }, error=function(e) "Incorrect connection")

  if(sql_choice == "check_connection"){
    result <- SQL_connection

  }else{
    if(!is.character(SQL_connection)){

      if(sql_choice == "push"){
        dbWriteTable(conn = SQL_connection,
                     name = str_glue("{df_identifier}_{table_name}"),
                     value = df %>% clean_df(col_select = attributes),
                     overwrite = TRUE)

        result <- str_glue("pushed dataset {df_identifier}_{table_name}")

      }else if(sql_choice == "pull"){
        query_variables <- ifelse(is.null(attributes), "*",
                                  str_c("[",attributes,"]", collapse = ","))

        table <- ifelse(is.null(df_identifier), table_name,
                        str_glue("{df_identifier}_{table_name}"))

        result <- dbGetQuery(conn = SQL_connection,
                             statement = str_glue("SELECT {query_variables} FROM [{table}]")) %>%
          clean_df()

      }else if(sql_choice == "tables_info"){
        tables <- str_subset(string = dbListTables(conn = SQL_connection),
                             pattern = obtain_regex(pattern = df_identifier,
                                                    return_regex = "starts_with_pattern"))

        result <- map(tables, ~ dbListFields(conn = SQL_connection, name =.x)) %>%
          set_names(tables)
      }
      dbDisconnect(SQL_connection)

    }else{result <- SQL_connection}
  }
  result
}

# MERGES ------------------------------------------------------------------

#' iterative_merge
#'
#' Merge a list of datasets
#'
#' This function allows you to make any type of merge of different datasets withing a list unifying the ID variable and cleaning duplicated variables.
#'
#' @param dfs_list list of datasets
#' @param key join IDs
#' @param keep argument that indicate if you want to get .x variables or .y variables
#' @param ... additional parameters of merge
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFROM purrr map2
#'
#' @return
#' "This function returns a merged dataset of all datasets from \code{dfs_list} with *different results* based on \code{keep} argument:"
#' \itemize{
#'   \item If \code{keep is NULL} return the merged dataset without cleaning the duplicated variables after merging.
#'   \item If \code{keep = "x"} return the dataset with the no duplicated variables and the duplicated old variables without the extension .x
#'   \item If \code{keep = "y"} return the dataset with the no duplicated variables and the duplicated new variables without the extension .y
#' }
#' @export
#'
#' @note
#'\itemize{
#'   \item The {keys argument} ids of each dataset is going to be renamed as _ID_, with the intention that the merge works with the variables with name _ID_
#'   \item If you select the \code{keep} argument and there is no duplicated variables, then it will ignore that argument.
#' }
#'
#' @example
#'df1 <- data.table(id = c(1, 5, 7, 5),
#'                  num = c(501, 502, 503, 504),
#'                  llave = c(1, 2, 3, 4))
#'df2 <- data.table(reference = c(1, 3, 1, 5, 5),
#'                  marca = c("Audi", "BMW", "MAZDA", "DODGE", "FERRARI"),
#'                  llave = c(1, 4, 3, 2, 5))
#'
#' # *final dataset with duplicated variables*
#' iterative_merge(dfs_list = list(df1, df2), key = c("id", "reference"))
#' # *keep x*
#' iterative_merge(dfs_list = list(df1, df2), key = c("id", "reference"), keep = "x")
#' # *keep y*
#' iterative_merge(dfs_list = list(df1, df2), key = c("id", "reference"),keep = "y")
#' # *unique key id*
#' iterative_merge(dfs_list = list(data.table(a=rep(1:2,each=3), b=1:6, key="a,b"),
#'                                 data.table(a=0:1, bb=10:11, key="a")),
#'                 key = "a")
#'
iterative_merge <- function(dfs_list, key, keep = NULL, ...){

  dfs_list <- map2(copy(dfs_list), key, ~ setnames(x = .x, old = .y, new = "ID"))

  df_merged <- Reduce(function(x,y) merge.data.table(x, y, by = "ID", ...),
                      x = dfs_list)

  if(is.null(keep)){
    result <- df_merged
  }else{
    check <- clean_merged_table(df_merged = df_merged, keep = keep)
    if(is.character(check)){
      result <- check
    }else{result <- clean_merged_table(df_merged = df_merged)}
  }
  result
}

#' clean_merged_table
#'
#' Unify duplicated columns.
#'
#' This function allows you after *merging* datasets, unify duplicated columns (this is the extension .x and .y)
#'
#' @param df_merged dataset with duplicated columns
#' @param keep argument that indicate if you want to get .x variables or .y variables
#'
#' @author Eduardo Trujillo
#'
#' @import data.table
#' @importFROM purrr map set_names pluck
#' @importFROM stringr str_subset str_remove_all
#'
#' @return
#' "This function returns *different results* based on \code{keep} argument:"
#' \itemize{
#'   \item If \code{keep = "x"} return the dataset with the no duplicated variables and the duplicated old variables without the extension .x
#'   \item If \code{keep = "y"} return the dataset with the no duplicated variables and the duplicated new variables without the extension .y
#' }
#'
#' @note If there is no duplicated variables will show an string message.
#'
#' @example
#' \dontrun{
#'df1 <- data.table(id = c(1, 5, 7, 5),
#'                  num = c(501, 502, 503, 504),
#'                  check_result = c(1, 2, 3, 4))
#'df2 <- data.table(id = c(1, 3, 1, 5, 5),
#'                  brand = c("Audi", "BMW", "MAZDA", "DODGE", "FERRARI"),
#'                  check_result = c(1, 4, 3, 2, 5))
#'
#'df <- udeploy::iterative_merge(dfs_list = list(df1, df2), key = "id")
#'
#' *keep x*
#' df <- clean_merged_table(df_merged = df, keep = "x")
#'
#' *keep y*
#' df <- clean_merged_table(df_merged = df, keep = "y")
#' }
#'
clean_merged_table <- function(df_merged, keep = c("x", "y")){

  merged_pattern <- map(c("ends_with_pattern", "extract_pattern"),
                        ~ obtain_regex(pattern =
                                         obtain_regex(pattern = c("x", "y"),
                                                      return_regex = "or"),
                                       return_regex = .x))

  variables_pattern <- map(c("x", "y"),
                           ~ obtain_regex(pattern = .x,
                                          return_regex = "contains_pattern")
  ) %>% set_names("x", "y")

  duplicated <- str_subset(string = names(df_merged),
                           pattern = pluck(merged_pattern, 1))

  if(length(duplicated) > 0){
    not_duplicated <-
      str_subset(string = names(df_merged),
                 pattern = obtain_regex(pattern = pluck(merged_pattern, 1),
                                        return_regex = "not_contains_pattern")
      )
    variables <- map(c("x", "y"),
                     ~str_subset(string = duplicated,
                                 pattern = pluck(variables_pattern, ..1))) %>%
      set_names(c("x", "y")) %>% pluck(keep)

    clean_variables <- str_remove_all(string = variables,
                                      pattern = pluck(merged_pattern, 2))

    result <- df_merged[,not_duplicated %>% append(variables), with = FALSE]
    result <- result %>% setnames(old = variables,
                                  new = str_remove_all(string = variables,
                                                       pattern = pluck(merged_pattern, 2)))
  }else result <- "The merged dataset does not have duplicated variables."
  result
}
