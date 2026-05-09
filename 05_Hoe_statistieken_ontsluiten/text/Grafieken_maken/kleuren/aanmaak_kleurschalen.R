library(colorblindcheck)

cols=c(
   'maincol1'='#4B6C7A','maincol2'='#71A3B8','maincol3'='#8ED2EE','textcol'='#34444A',
   'graycol1'='#EDEDED','graycol2'='#E5E5E5','graycol3'='#D5D5D5','graycol4'='#989898',
   'pal1col1'='#4E7E71','pal1col2'='#75C4AF','pal1col3'='#79D9BF',
   'pal2col1'='#795032','pal2col2'='#A66E45','pal2col3'='#D18955',
   'pal3col1'='#767344','pal3col2'='#A5A059','pal3col3'='#C1BB67')



tom <- c("#DC8648","#A5A04D","#1F8782","#355A60","#64A5BB","#C11F4C")
palette_check(c(cols[1],tom),plot = TRUE)



okabe_ito <- c(
  "#000000", # Black
  "#E69F00", # Orange
  "#56B4E9", # Sky Blue
  "#009E73", # Bluish Green
  "#F0E442", # Yellow
  "#0072B2", # Blue
  "#D55E00", # Vermillion
  "#CC79A7"  # Reddish Purple
)
palette_check(c(cols[1],okabe_ito),plot = TRUE)













palette_check(cols, plot = TRUE)
warnings()


ordinalscale <- c(cols$maincol1,cols$graycol1,cols$pal3col1) |> (\(x) colorRampPalette(x)(7))()
barplot(rep(1,7), col = ordinalscale)
palette_check(ordinalscale, plot = TRUE)



ordinalscale <- colorRampPalette(c("#DBE1E4",cols$maincol1))(5)
barplot(rep(1,5), col = ordinalscale)


palette.colors(palette = "Okabe-Ito")

nominalscale <- c( '#CC6677', '#332288', '#DDCC77', '#117733', '#88CCEE', '#882255', '#44AA99', '#999933', '#AA4499','#DDDDDD')
palette_check(nominalscale, plot = TRUE)




nominalscale <- c('#4B6C7A','maincol2'='#71A3B8','maincol3'='#8ED2EE','#2F5D9B','#1FBAD6','#2FA84F','#F0B323','#E36A2E','#7A4FB3','#C94A8A')
palette_check(nominalscale, plot = TRUE)


#
# % 1. Diep blauw (anker)
# \definecolor{nomBlue}{HTML}{2F5D9B}
#
# % 2. Cyaan
# \definecolor{nomCyan}{HTML}{1FBAD6}
#
# % 3. Groen
# \definecolor{nomGreen}{HTML}{2FA84F}
#
# % 4. Geel / amber
# \definecolor{nomAmber}{HTML}{F0B323}
#
# % 5. Oranje
# \definecolor{nomOrange}{HTML}{E36A2E}
#
# % 6. Paars
# \definecolor{nomPurple}{HTML}{7A4FB3}
#
# % 7. Magenta / roze (niet rood)
# \definecolor{nomMagenta}{HTML}{C94A8A}