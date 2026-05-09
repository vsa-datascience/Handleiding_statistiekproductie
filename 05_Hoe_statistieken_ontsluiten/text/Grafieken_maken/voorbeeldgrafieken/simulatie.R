setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

library(tidyverse)

set.seed(123)










### groupplots


m=38
se=12

x=seq(0,100,.1)

y=dnorm(x,m,se)
y=y/max(y)
y2=sin(4*x)*y

plot(x,y2,type='l',ylim=c(-1,1))

sprintf('(%.1f,%.4f)',x,y2) %>% str_c(collapse=' -- ') %>% cat()



	18-34 jarigen       & $\pm$ 32,0\% & \graph{32}{8}\\
	35-49 jarigen       & $\pm$ 30,0\% & \graph{30}{3}\\
	50-64 jarigen       & $\pm$ 49,0\% & \graph{49}{4}\\
	65 jarigen en ouder & $\pm$ 38,0\% & \graph{38}{12}\\








### trendplots



data.short <- tibble(
    x=0:10
   ,y1= 50 - 2*x + runif(11)*4
   ,se1=3*runif(11)+4
   ,ll1 = y1-se1
   ,ul1 = y1+se1
   ,y2= 55 +   x + runif(11)*4
   ,se2=3*runif(11)+4
   ,ll2 = y2-se2
   ,ul2 = y2+se2
   )

attach(data.short)
plot(x,y1,type='l',ylim=c(0,100),col='blue',lwd=3)
lines(x,ll1,col='blue')
lines(x,ul1,col='blue')
lines(x,y2,col='red',lwd=3)
lines(x,ll2,col='red')
lines(x,ul2,col='red')
detach(data.short)


data.long <- tibble(
    x = seq(0,10,.01)
   ,y1 = approx(data.short$x,data.short$y1,xout=x)$y
   ,se1 = approx(data.short$x,data.short$se1,xout=x)$y
   ,line1 = y1+sin(x*30)*se1
   ,y2 = approx(data.short$x,data.short$y2,xout=x)$y
   ,se2 = approx(data.short$x,data.short$se2,xout=x)$y
   ,line2 = y2+sin(x*30+90)*se2
   )


attach(data.long)
plot(x,y1,type='l',ylim=c(0,100),col='blue')
lines(x,line1,col='blue')
lines(x,y2,col='red')
lines(x,line2,col='red')
detach(data.long)

attach(data.long)
plot(x,line1,type='l',ylim=c(0,100),col='blue')
lines(x,line2,col='red')
detach(data.long)











#### DOTPLOT

tmp <- ( data.short
   %>% filter(x<=3)
   )

#### LINEPLOT

( data.short
   %>% arrange(-x)
   %>% transmute(code=sprintf('(%i,%.4f)',x,ul1))
   %>% summarize(code=str_c(code,collapse=' -- '))
   %>% pull(code)
   %>% cat()
   )
   
( data.short
   %>% transmute(code=sprintf('(%i,%.4f)',x,y2))
   %>% summarize(code=str_c(code,collapse=' -- '))
   %>% pull(code)
   %>% cat()
   )


### SINUSPLOT

( data.long
   %>% transmute(code=sprintf('(%.2f,%.4f)',x,line2))
   %>% summarize(code=str_c(code,collapse=' -- '))
   %>% pull(code)
   %>% cat()
   )
