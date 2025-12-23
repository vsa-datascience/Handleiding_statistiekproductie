library(tidyverse)

set.seed(6238)

standardize <- function(x,minx=min(x),maxx=max(x)) {
   (x-minx)/(maxx-minx)
   }



data <- "data-C6rq6.csv" |>
   read_csv2(col_types=cols(.default="numeric")) |>
   mutate(
      year= (X.1-min(X.1))/20,
      onedef = coalesce(`Woonhuizen obv statistisch formulier bouwvergunning`,`Woonhuizen obv statistisch formulier bouwvergunning1`),
      twodef1 = `Woonhuizen obv statistisch formulier bouwvergunning`,
      twodef2 = `Woonhuizen obv statistisch formulier bouwvergunning1`,
      twodef2overlap = coalesce(
         `Woonhuizen obv statistisch formulier bouwvergunning1`,
         sample(na.omit(`Woonhuizen obv statistisch formulier bouwvergunning1`),n(),replace=TRUE)+rnorm(n(),0,3)
         ),
      .keep='none'
      ) |>
   pivot_longer(-year) |>
   mutate(year=standardize(year),value=standardize(value,0,350)) |>
   pivot_wider()

miny <- data |> select(-year) |> pivot_longer(everything()) |> pull(value) |> min(na.rm=TRUE)
maxy <- data |> select(-year) |> pivot_longer(everything()) |> pull(value) |> max(na.rm=TRUE)
plot(data$year,data$twodef1,type = "l",col="blue",ylim=c(miny,maxy))
lines(data$year,data$twodef2overlap,col="red")


#yaxis ticks
seq(0, 350, by = 50) |>
  (function(x){sprintf("%.3f/%i", standardize(x, 0, 350), x)})() |>
  paste(collapse = ",") |>
  cat()


#xaxis ticks
seq(2005,2024, by = 5) |>
  (function(x){sprintf("%.3f/%i", standardize(x, 2005, 2024), x)})() |>
  paste(collapse = ",") |>
  cat()


#dezelfde operationele definitie
sprintf("(%.3f,%.3f)",data$year,data$onedef) |> paste(collapse = " -- ") |> cat()

#dezelfde conceptuele definitie
sprintf("(%.3f,%.3f)",data$year,data$twodef1) |> paste(collapse = " -- ") |> cat()
sprintf("(%.3f,%.3f)",data$year,data$twodef2overlap) |> paste(collapse = " -- ") |> cat()
