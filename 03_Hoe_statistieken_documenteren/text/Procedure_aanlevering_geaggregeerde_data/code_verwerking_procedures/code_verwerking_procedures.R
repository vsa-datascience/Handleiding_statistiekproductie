
rm(list=ls())

library(tidyverse)





read_procedure <- function(file) {
  tibble(text=read_lines(file)) |>
  filter(str_detect(text,'\\S')) |>
  mutate(
    part=case_when(
      str_detect(text,'^#[:space:]') ~ text,
      TRUE ~ NA
      ),
    question=case_when(
      str_detect(text,'^#[:space:]') ~ '---',
      str_detect(text,'^##[:space:]') ~ text,
      TRUE ~ NA
      ),
    ) |>
  fill(part,question) |>
  filter(text!=part,text!=question) |>
  summarize(info=str_c(text,collapse='\n'),.by=c(part,question)) |>
  mutate(
    part=str_replace(part,'^#[:space:]',''),
    question=str_replace(question,'^##[:space:]',''),
    )
  }



files <- 
  r'{C:\Users\vanniejo\OneDrive - Vlaamse overheid - Office 365\Datawarehouse\Procedure_aanlevering_geaggregeerde_data\Procesbeschrijvingen}' |>
  list.files(pattern='[.]md$',full.names=TRUE) |>
  as_tibble() |>
  mutate(
    aanlevering = str_replace(value,'.+_([A-Z]+)[.]md$','\\1'),
    data = map(value,read_procedure),
    .keep='none'
    ) |>
  unnest(data)


files2 <- files |>
  pivot_wider(id_cols='aanlevering',names_from='question',values_from='info')



