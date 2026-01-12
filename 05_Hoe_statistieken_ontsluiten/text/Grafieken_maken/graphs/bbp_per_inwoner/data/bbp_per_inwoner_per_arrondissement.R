library(tidyverse)

data <- "data-kCqZG.csv" |>
   read_csv2() |>
   rename(year=X.1) |>
   pivot_longer(-year) |>
   pivot_wider(names_from=year, values_from=value) |>
   mutate(
      log2003 = log(`2003`/100),
      log2023 = log(`2023`/100),
   )

data |>
   pivot_longer(c(`2003`,`2023`), names_to='year', values_to='value') |>
   ggplot(aes(x=year,y=value,group=name,color=name)) +
   geom_line()

data |>
   pivot_longer(c(log2003,log2023), names_to='year', values_to='value') |>
   ggplot(aes(x=year,y=value,group=name,color=name)) +
   geom_line()

summary(data)

x=seq(0,10,.01)
plot(x,log(x))


fmt <- function(x){x |> round(1) |> format(decimal.mark = ",") |> trimws()}


yaxis <- data |>
   filter(`2003`<`2023`) |>
   mutate(code=sprintf(r"{\node[smallaxislabel,anchor=east,tealgreen,yshift= 0pt] at (0,%.1f) {%s (%s$\rightarrow$%s)};}",`2003`,name,fmt(`2003`),fmt(`2023`))) |>
   pull(code) |>
   cat(sep='\n')


yaxisb <- data |>
   filter(`2003`>`2023`) |>
   mutate(code=sprintf(r"{\node[smallaxislabel,anchor=west,warmorange,yshift= 0pt] at (0,%.1f) {%s (%s$\rightarrow$%s)};}",`2023`,name,fmt(`2003`),fmt(`2023`))) |>
   pull(code) |>
   cat(sep='\n')

body_minmax <- data |>
   summarize(
      min = min(`2003`,`2023`),
      max = max(`2003`,`2023`),
      ) |>
   mutate(
      code = sprintf("(0,%.1f) -- (0,,%.1f) (1,%.1f) -- (1,%.1f)",min,max,min,max),
      ) |>
   pull(code) |>
   cat(sep='\n')


body_slopes <- data |>
   mutate(
      color = if_else(`2003`<`2023`,'tealgreen','warmorange'),
      code=sprintf(r"{\filldraw[line width=2pt,%s] (0,%.1f) circle(1pt) -- (1,%.1f) circle(1pt); %% %s}",color,`2003`,`2023`,name),
      ) |>
   pull(code) |>
   cat(sep='\n')







dumbbell <- data |>
   arrange(desc(`2023`)) |>
   mutate(
      min = min(`2003`,`2023`),
      max = max(`2003`,`2023`),
      scale2003 = (`2003`-min)/(max-min),
      scale2023 = (`2023`-min)/(max-min),
      code=sprintf(r"{%s & %s & %s & \drawdumbbell{%.4f}{%.4f} \\}",name,fmt(`2003`),fmt(`2023`),scale2003,scale2023)
      ) |>
   pull(code) |>
   cat(sep='\n')

dumbbell <- data |>
   summarize(
      min = min(`2003`,`2023`),
      max = max(`2003`,`2023`),
      ) |>
   mutate(code=(100-min)/(max-min)) |>
   pull(code) |>
   cat(sep='\n')





