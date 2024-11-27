library(maps)
library(dplyr)
library(geosphere)
library(basicPlotteR)

par(mar=c(0,0,0,0))

# World map
map('world',
    col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,
    mar=rep(0,4),border=0, ylim=c(-80,80) 
)

#Endemic
South_africa <- c(22.937506,-30.559482)
Zimbabwe <- c(29.154857,-19.015438)
Lesotho <- c(28.233608,-29.609988)
Algeria <- c(1.659626,28.033886)
Azerbaijan <- c(47.576927,40.143105)
Belarus <- c(27.953389,53.709807)
Bolivia <- c(-63.588653,-16.290154)
Brazil <- c(-51.92528,-14.235004)
Chad <- c(18.732207,15.454166)
China <- c(104.195397,35.86166)
Egypt <- c(30.802498,26.820553)
Gambia <- c(-15.310139,13.443182)
India <- c(78.96288,20.593684)
Indonesia <- c(113.921327,-0.789275)
Iran <- c(53.688046,32.427908)
Iraq <- c(43.679291,33.223191)
Israel <- c(34.851612,31.046051)
Malaysia <- c(101.975766,4.210484)
Mexico <- c(-102.552784, 23.634501)
Mongolia <- c(103.846656, 46.862496)
Morocco <- c(-7.09262,31.791702)
Nepal <- c(84.124008,28.394857)
Peru <- c(-75.015152,-9.189967)
Philippines <- c(121.774017,12.879721)
Russia <- c(105.318756,61.52401)
Sri_Lanka <- c(80.771797,7.873054)
Tanzania <- c(34.888822,-6.369028)
Thailand <- c(100.992541,15.870032)
Turkey <- c(35.243322,38.963745)
Ukraine <- c(31.16558,48.379433)
Senegal <- c(-14.452362,14.497401)
Afghanistan <- c(67.709953,33.93911)

#Controlled
United_kingdom <- c(-3.435973,55.378051)
USA <- c(-95.712891,37.09024)
Switzerland <- c(8.227512,46.818188)
Serbia <- c(21.005859,44.016521)
Slovakia <- c(19.699024,48.669026)
Slovenia <- c(14.995463,46.151241)
Spain <- c(-3.74922,40.463667)
Belgium <- c(4.469936,50.503887)
Bhutan <- c(90.433601,27.514162)
Austria <- c(14.550072,47.516231)
Albania <- c(20.168331,41.153332)
Bosnia_Herzegovina <- c(17.679076,43.915886)
Canada <- c(-106.346771,56.130366)
Croatia <- c(15.2,45.1)
Estonia <- c(25.013607,58.595272)
Ethiopia <- c(40.489673,9.145)
Finland <- c(25.748151,61.92411)
France <- c(2.213749,46.227638)
Germany <- c(10.451526,51.165691)
Greece <- c(21.824312,39.074208)
Greenland <- c(-42.604303,71.706936)
Italy <- c(12.56738,41.87194)
Kosovo <- c(20.902977,42.602636)
Netherlands <- c(-69.060087,12.226079)
Macedonia <- c(21.745275,41.608635)
Norway <- c(8.468946,60.472024)
Poland <- c(19.145136,51.919438)
Romania <- c(24.96676,45.943161)
South_korea <- c(127.766922,35.907757)
Portugal <- c(-8.224454,39.399872)
Bulgaria <- c(25.48583,42.733883)

#Midpoints
RuMo <- c(104.5827,54.19325)
(103.846656+105.318756)/2
(46.862496+61.52401)/2
AlMo <- c(16.72566,27.21569)
(1.659626+31.791702)/2 
(61.52401+-7.09262)/2
BeUk <- c(29.55948,51.04462)
(27.953389+31.16558)/2
(53.709807+48.379433)/2
RuUk <- c(68.24217,61.95172)
(105.318756+31.16558)/2
(61.52401+48.379433)/2


data <- rbind(South_africa, Zimbabwe, Lesotho, Algeria, Azerbaijan, Belarus, Bolivia, Brazil, Chad, China, Egypt, Ethiopia, Gambia, India, Indonesia, Iran, Iraq, Israel, Malaysia, Mexico, Mongolia, Morocco, Nepal, Peru, Philippines, Russia, Sri_Lanka, Tanzania, Thailand, Turkey, Ukraine, Senegal, Afghanistan) %>% 
  as.data.frame()
colnames(data) <- c("long","lat")

datac <- rbind(South_korea, Albania, Austria, Belgium, Bhutan, Bosnia_Herzegovina, Canada, Croatia, Estonia, Finland, France, Germany, Greece, Greenland, Italy, Kosovo, Netherlands, Macedonia, Norway, Poland, Romania, Serbia, Slovakia, Slovenia, Spain, Switzerland, United_kingdom, USA, Portugal) %>% 
  as.data.frame()
colnames(datac) <- c("long","lat")

datam <- rbind(RuMo, AlMo, BeUk, RuUk) %>% 
  as.data.frame()
colnames(datam) <- c("long","lat")

# Show the cities on the map
map('world',
    col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,
    mar=rep(0,4),border=0, ylim=c(-80,80) 
)
points(x=data$long, y=data$lat, col="#ff0000", cex=1, pch=20)
points(x=datac$long, y=datac$lat, col="#00a68c", cex=1, pch=20)
points(x=datam$long, y=datam$lat, col="#00a68c", cex=0, pch=20)

text(rownames(data), x=data$long, y=data$lat,  col="slateblue", cex=1, pos=4)

inter <- gcIntermediate(South_africa, Zimbabwe, n=50, addStartEnd=TRUE, breakAtDateLine=F)
SA_L <- gcIntermediate(South_africa, Lesotho, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Ne_Ir <- gcIntermediate(Nepal, Iran, n=50, addStartEnd=TRUE, breakAtDateLine=F)
In_Bh <- gcIntermediate(India, Bhutan, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Mo_Ru <- gcIntermediate(Mongolia, Russia, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Ch_RuMo <- gcIntermediate(China, RuMo, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Th_Ma <- gcIntermediate(Thailand, Malaysia, n=50, addStartEnd=TRUE, breakAtDateLine=F)
In_Ma <- gcIntermediate(Indonesia, Malaysia, n=50, addStartEnd=TRUE, breakAtDateLine=F)
#2 legs
Mo_Sp <- gcIntermediate(Morocco, Spain, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Sp_Fr <- gcIntermediate(France, Spain, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Ko_Al <- gcIntermediate(Kosovo, Albania, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Se_Au <- gcIntermediate(Serbia, Austria, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Ge_Az <- gcIntermediate(Germany, Azerbaijan, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Ne_Ge <- gcIntermediate(Germany, Nepal, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Sl_Au <- gcIntermediate(Slovenia, Austria, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Mo_Al <- gcIntermediate(Morocco, Algeria, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Almo_Sw <- gcIntermediate(AlMo, Switzerland, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Fi_Es <- gcIntermediate(Finland, Estonia, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Mo_Ge <- gcIntermediate(Morocco, Germany, n=50, addStartEnd=TRUE, breakAtDateLine=F)
#3 legs
Sp_Po <- gcIntermediate(Spain, Portugal, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Fr_Po <- gcIntermediate(France, Portugal, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Mo_Be <- gcIntermediate(Morocco, Belgium, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Ma_Ko <- gcIntermediate(Macedonia, Kosovo, n=50, addStartEnd=TRUE, breakAtDateLine=F)
In_Fi <- gcIntermediate(India, Finland, n=50, addStartEnd=TRUE, breakAtDateLine=F)
#3 legs
Ga_Se <- gcIntermediate(Gambia, Senegal, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Se_Be <- gcIntermediate(Senegal, Belgium, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Be_Fr <- gcIntermediate(Belgium, France, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Sr_Uk <- gcIntermediate(Sri_Lanka, United_kingdom, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Cr_Ge <- gcIntermediate(Croatia, Germany, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Sl_It <- gcIntermediate(Slovenia, Italy, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Af_Fr <- gcIntermediate(Afghanistan, France, n=50, addStartEnd=TRUE, breakAtDateLine=F)
#2-leg
Bo_Sl <- gcIntermediate(Bosnia_Herzegovina, Slovenia, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Ge_Sl <- gcIntermediate(Germany, Slovenia, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Beuk_Po <- gcIntermediate(BeUk, Poland, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Ruuk_Po <- gcIntermediate(RuUk, Poland, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Be_Uk <- gcIntermediate(Belarus, Ukraine, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Ru_Uk <- gcIntermediate(Russia, Ukraine, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Cr_Ro <- gcIntermediate(Croatia, Romania, n=50, addStartEnd=TRUE, breakAtDateLine=F)
No_Ru <- gcIntermediate(Norway, Russia, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Po_Ru <- gcIntermediate(Poland, Russia, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Net_Sp <- gcIntermediate(Netherlands, Spain, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Ma_Gr <- gcIntermediate(Macedonia, Greece, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Mo_Fr <- gcIntermediate(Morocco, France, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Sl_Po <- gcIntermediate(Slovakia, Poland, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Fr_Al <- gcIntermediate(France, Algeria, n=50, addStartEnd=TRUE, breakAtDateLine=F)
#2-leg
Ru_It <- gcIntermediate(Russia, Italy, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Uk_It <- gcIntermediate(United_kingdom, Italy, n=50, addStartEnd=TRUE, breakAtDateLine=F)
#2-leg
Tu_Bu <- gcIntermediate(Turkey, Bulgaria, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Bu_Ge <- gcIntermediate(Bulgaria, United_kingdom, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Gr_Ca <- gcIntermediate(Greenland, Canada, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Me_Us <- gcIntermediate(Mexico, USA, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Th_Us <- gcIntermediate(Thailand, USA, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Ca_Us <- gcIntermediate(Canada, USA, n=50, addStartEnd=TRUE, breakAtDateLine=F)
In_Us <- gcIntermediate(India, USA, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Ir_Us <- gcIntermediate(Iraq, USA, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Eg_Us <- gcIntermediate(Egypt, USA, n=50, addStartEnd=TRUE, breakAtDateLine=F)
#2-leg
Eg_Ca <- gcIntermediate(Egypt, Canada, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Az_Us <- gcIntermediate(Azerbaijan, USA, n=50, addStartEnd=TRUE, breakAtDateLine=F)
#2-leg
Ir_Ge <- gcIntermediate(Iran, Germany, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Ca_Ge <- gcIntermediate(Canada, Germany, n=50, addStartEnd=TRUE, breakAtDateLine=F)
Bo_Br <- gcIntermediate(Bolivia, Brazil, n=50, addStartEnd=TRUE, breakAtDateLine=F)

# Show this connection
lines(inter, col="#36bed9", lwd=2) #domestic domestic
lines(SA_L, col="#36bed9", lwd=2) #domestic domestic
lines(Ne_Ir, col="#36bed9", lwd=1) #domestic
lines(In_Bh, col="#36bed9", lwd=2) #domestic domesetic
lines(Mo_Ru, col="#ff7c00", lwd=2) #wild wild
lines(Ch_RuMo, col="#ff7c00", lwd=2) #wild wild
lines(Th_Ma, col="#36bed9", lwd=2) #domestic domestic
lines(In_Ma, col="#36bed9", lwd=1) #domestic
lines(Mo_Sp, col="#36bed9", lwd=3) #domestic x 13
lines(Sp_Fr, col="#36bed9", lwd=3) #domestic x 10
lines(Ko_Al, col="#36bed9", lwd=1) #domestic
lines(Se_Au, col="#36bed9", lwd=1) #domestic
lines(Ge_Az, col="#36bed9", lwd=1) #domestic
lines(Ne_Ge, col="#36bed9", lwd=1) #domestic
lines(Sl_Au, col="#ff7c00", lwd=1) #wild
lines(Almo_Sw, col="#36bed9", lwd=1) #domestic
lines(Mo_Al, col="#36bed9", lwd=1) #domestic
lines(Fi_Es, col="#36bed9", lwd=1) #domestic
lines(Mo_Ge, col="#36bed9", lwd=2) #domestic
lines(Sp_Po, col="#36bed9", lwd=1) #domestic
lines(Fr_Po, col="#36bed9", lwd=1) #domestic
lines(Mo_Be, col="#36bed9", lwd=1) #domestic
lines(Ma_Ko, col="#ff7c00", lwd=1) #wild
lines(In_Fi, col="#36bed9", lwd=1) #dm
lines(Ga_Se, col="#36bed9", lwd=1) #dm
lines(Se_Be, col="#36bed9", lwd=1) #dm
lines(Be_Fr, col="#36bed9", lwd=1) #dm
lines(Sr_Uk, col="#36bed9", lwd=1) #dm
lines(Cr_Ge, col="#36bed9", lwd=2) #dm dm
lines(Sl_It, col="#ff7c00", lwd=1) #w
lines(Af_Fr, col="#36bed9", lwd=1) #dm
lines(Bo_Sl, col="#36bed9", lwd=1) #dm
lines(Ge_Sl, col="#36bed9", lwd=1) #dm
lines(Beuk_Po, col="#ff7c00", lwd=3) #w w w
lines(Ruuk_Po, col="#ff7c00", lwd=1) #w
lines(Ru_Uk, col="#ff7c00", lwd=1) #w
lines(Be_Uk, col="#ff7c00", lwd=1) #w
lines(Cr_Ro, col="#36bed9", lwd=1) #dm
lines(No_Ru, col="#ff7c00", lwd=1) #w
lines(Po_Ru, col="#ff7c00", lwd=1) #w
lines(Net_Sp, col="#36bed9", lwd=1) #dm
lines(Ma_Gr, col="#ff7c00", lwd=1) #w
lines(Mo_Fr, col="#36bed9", lwd=3) #dm dm dm
lines(Sl_Po, col="#fbad00", lwd=3) #dm, w, w
lines(Fr_Al, col="#36bed9", lwd=1) #dm
lines(Ru_It, col="#36bed9", lwd=1) #dm
lines(Uk_It, col="#36bed9", lwd=1) #dm
lines(Tu_Bu, col="#36bed9", lwd=1) #dm
lines(Bu_Ge, col="#36bed9", lwd=1) #dm
lines(Gr_Ca, col="#ff7c00", lwd=1) #w
lines(Me_Us, col="#36bed9", lwd=2) #dm dm
#lines(Th_Us, col="#36bed9", lwd=1) #dm
lines(Ca_Us, col="#fbad00", lwd=3) #w w w w dm
lines(In_Us, col="#36bed9", lwd=1) #dm
lines(Ir_Us, col="#36bed9", lwd=1) #dm
lines(Eg_Us, col="#36bed9", lwd=2) #dm dm
lines(Eg_Ca, col="#36bed9", lwd=1) #dm
lines(Az_Us, col="#36bed9", lwd=1) #dm
lines(Ir_Ge, col="#36bed9", lwd=1) #dm
lines(Ca_Ge, col="#36bed9", lwd=1) #dm
lines(Bo_Br, col="#36bed9", lwd=2) #dm

plot_my_connection=function( dep_lon, dep_lat, arr_lon, arr_lat, ...){
  inter <- gcIntermediate(c(dep_lon, dep_lat), c(arr_lon, arr_lat), n=50, addStartEnd=TRUE, breakAtDateLine=F)             
  inter=data.frame(inter)
  diff_of_lon=abs(dep_lon) + abs(arr_lon)
  if(diff_of_lon > 180){
    lines(subset(inter, lon>=0), ...)
    lines(subset(inter, lon<0), ...)
  }else{
    lines(inter, ...)
  }
}

map('world',col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05,mar=rep(0,4),border=0, ylim=c(-80,80) )

# Circles for cities
points(x=data$long, y=data$lat, col="#ff0000", cex=1, pch=20)
points(x=datac$long, y=datac$lat, col="#00a68c", cex=1, pch=20)
points(x=datam$long, y=datam$lat, col="slateblue", cex=0, pch=20)

# Connections
plot_my_connection(South_africa[1], Zimbabwe[1], col="#36bed9", lwd=2)
plot_my_connection(USA[1], Thailand[1], col="#36bed9", lwd=2)