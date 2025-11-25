rm(list=ls())

library(DBI)
library(dbplyr)
library(tidyverse)




# Create database ---------------------------------------------------------


con <- DBI::dbConnect(
   duckdb::duckdb(),
   dbdir = "VSA_database"
   )





# Create population data --------------------------------------------------

n <- 100000

data_register <- tibble(
   ID=sprintf("ID%08f",1:n),
   sex=sample(c('M','F'),n,replace=TRUE),
   age=runif(n,0,90) |> floor()
   )

rm(n)



# Create survey data ------------------------------------------------------

n <- 3000

data_survey <- tibble(
   ID=sprintf("Resp%04.f",1:n),
   gndr=sample(c('Man','Vrouw'),n,replace=TRUE,prob=c(4,6)),
   trustgov=sample(1:5,n,replace=TRUE,prob=c(1,2,4,5,3)),
   )

rm(n)













# -------------------------------------------------------------------------
# DATAMODEL ---------------------------------------------------------------
# -------------------------------------------------------------------------



# Codelists ---------------------------------------------------------------

codelists <- list()

codelists[['sex']] <- tribble(
   ~value  ,~valuen,~label_nl,~version_register,~version_survey,
   'man'   , 1     ,'man'    , TRUE            ,TRUE           ,
   'vrouw' , 2     ,'vrouw'  , TRUE            ,TRUE           ,
   'ander' , 3     ,'ander'  , FALSE           ,TRUE           ,
   )

codelists[['agecat']] <- tribble(
   ~value  ,~valuen,~label_nl         ,~version_all,
   '0-18'  , 1     ,'0-18 jaar'       , TRUE       ,
   '19-35' , 2     ,'19-35 jaar'      , TRUE       ,
   '36-64' , 3     ,'36-64 jaar'      , TRUE       ,
   '65+'   , 4     ,'65 jaar of meer' , TRUE       ,
   )

codelists[['many']] <- tribble(
   ~value                   ,~valuen,~label_nl               ,~version_all,~version_substantial,
   'zeer weinig'            , 1     ,'zeer weinig'           , TRUE       , TRUE               ,
   'weinig'                 , 2     ,'weinig'                , TRUE       , TRUE               ,
   'niet veel, niet weinig' , 3     ,'niet veel, niet weinig', TRUE       , TRUE               ,
   'veel'                   , 4     ,'veel'                  , TRUE       , TRUE               ,
   'zeer veel'              , 5     ,'zeer veel'             , TRUE       , TRUE               ,
   'geen mening'            ,98     ,'geen mening'           , TRUE       , FALSE              ,
   'nvt'                    ,99     ,'niet van toepassing'   , TRUE       , FALSE              ,
   )


# Data Structure Definition -----------------------------------------------

get_codelist_versions <- function(codelist) {
   codelists |>
   magrittr::extract2(codelist) |>
   names() |>
   str_subset('^version_') |>
   str_remove('^version_')
   }

DSD <- tribble(
   ~concept  ,~label_nl                  ,~format    ,~codelist  ,~allow_na,~min,~max,
   'ID'      ,'Identificatie'            ,'character',NA         ,FALSE    ,NA  ,NA,
   'sex'     ,'Geslacht'                 ,'codelist' ,'sex'      ,FALSE    ,NA  ,NA,
   'age'     ,'Leeftijd'                 ,'numeric'  ,NA         ,FALSE    ,0   ,99,
   'agecat'  ,'Leeftijd'                 ,'codelist' ,'agecat'   ,FALSE    ,NA  ,NA,
   'trustgov','Vertrouwen in de overheid','codelist' ,'many'     ,FALSE    ,NA  ,NA,
   ) |>
   mutate(
      codelist_versions=map(codelist,get_codelist_versions),
      .after='codelist'
      )






# Add codelists to database -----------------------------------------------

write_codelist_version <- function(codelist,version) {
   codelists |>
   magrittr::extract2(codelist) |>
   filter(!!sym(str_c('version_',version))) |>
   select(any_of(c('value','valuen')),starts_with('label_')) |>
   dbWriteTable(
      conn=con,
      name=str_c('cl_',codelist,'_',version),
      overwrite = TRUE
      )
   }

write_codelist_all_versions <- function(codelist) {
   codelists |>
   magrittr::extract2(codelist) |>
   names() |>
   str_subset('^version_') |>
   str_remove('^version_') |>
   walk(~write_codelist_version(codelist,.x))
   }

codelists |>
   names() |>
   walk(write_codelist_all_versions)



dbListTables(con)
dbReadTable(con,"cl_many_substantial") |> as_tibble()












# -------------------------------------------------------------------------
# Disconnect database -----------------------------------------------------
# -------------------------------------------------------------------------


dbDisconnect(con)