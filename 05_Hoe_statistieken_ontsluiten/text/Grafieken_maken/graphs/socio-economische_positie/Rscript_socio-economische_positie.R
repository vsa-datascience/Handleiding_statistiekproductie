library(tidyverse)


data_admin <- "Bevolking naar socio-economische positie op basis van administratieve data.csv" |>
   read_csv2() |>
   pivot_longer(-Jaartal,names_to='year') |>
   rename(status=Jaartal) |>
   mutate(value=100*value/sum(value),.by=year) |>
   mutate(
      year=as.numeric(year),
      yearcode=(year-1999)/(2024-1999),
      code=sprintf("(%.4f,%.4f)",yearcode,value)
      ) |>
   summarize(code=str_c(code,collapse=' -- '),.by=status) |>
   mutate(print=walk2(status,code,~cat(.x,'\n',.y,'\n')))


data_EAK <- "Bevolking naar socio-economische positie op basis van EAK-enquête.csv" |>
   read_csv2() |>
   pivot_longer(-X.1,names_to='year') |>
   rename(status=X.1) |>
   mutate(
      year=as.numeric(year),
      yearcode=(year-1999)/(2024-1999),
      code=sprintf("(%.4f,%.4f)",yearcode,value)
      ) |>
   summarize(code=str_c(code,collapse=' -- '),.by=status) |>
   mutate(print=walk2(status,code,~cat(.x,'\n',.y,'\n')))

