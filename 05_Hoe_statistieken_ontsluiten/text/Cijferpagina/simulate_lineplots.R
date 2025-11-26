x=2005:2025

set.seed(23376)

x=(0:20)/20
y1=.6-.6*x+0.6*x^2 + rnorm(21,0,.05)
y2=y1-.1 + rnorm(21,0,.1)

plot(x,y1,xlim=c(0,1),ylim=c(0,1),type = "l",col="blue")
lines(x,y2,col="red")

#dezelfde operationele definitie
sprintf("(%.3f,%.3f)",x,y1) |> paste(collapse = " -- ") |> cat()

#dezelfde conceptuele definitie
sprintf("(%.3f,%.3f)",x,y1) |> magrittr::extract(1:15) |> paste(collapse = " -- ") |> cat()
sprintf("(%.3f,%.3f)",x,y2) |> magrittr::extract(12:21) |> paste(collapse = " -- ") |> cat()
