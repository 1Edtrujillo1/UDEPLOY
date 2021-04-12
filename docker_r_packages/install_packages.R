for(i in c("devtools", "purrr")){
  install.packages(i)
}

# PACKAGE NAME AND VERSION ------------------------------------------------
packages <- c("shinydashboard", "shinydashboardPlus", "shinyjs",
              "shinythemes", "shinyWidgets", "htmltools", "config",
              "sodium", "waiter", "jsonlite", "mongolite", "data.table",
              "haven", "readxl", "lubridate", "DT", "DBI", "odbc", 
              "aws.s3", "htmlwidgets", "sparkline", "formattable")

versions <- c("0.7.1", "0.7.5", "2.0.0", "1.1.2", "0.5.4", "0.5.1.1",
              "0.3.1", "1.1", "0.2.0", "1.7.2", "2.2.1", "1.13.6", "2.3.1", 
              "1.3.1", "1.7.9.2", "0.17", "1.1.0", "1.3.0", "0.3.3",
              "1.5.3", "2.0", "0.2.1")

packages_github <- c("nik01010/dashboardthemes", "1Edtrujillo1/udeploy")

# INSTALLING PACKAGE ------------------------------------------------------
purrr::map2(packages, versions, function(i, j){
  if(! (i %in% rownames(installed.packages()))){
    devtools::install_version(package = i, version = j, upgrade = "never")
  }
})

purrr::map(packages_github, 
           ~ devtools::install_github(.x, dependencies = FALSE))


# map(packages, packageVersion) %>% 
#   set_names(packages)
