#set files location
setwd("C:/Users/rache/OneDrive - University of Cambridge/Thailand/Explanatory Data/Land Use & Vegetation")

library(rgdal)
library(raster)

#R pulls the names of all the files in a directory, with a certain ending (.tif files)
names<-data.frame(name=list.files("./My Folder/",pattern = glob2rx('*.tif')))

#change your wd to be in the same folder as your files (if you weren't already)
setwd("./My Folder")
#brick is a function to put a bunch of .tif files together, but for more general
#files you could use something like list I think
#each object in the list would be assigned the name
files <- sapply(names,function(x){brick(x)})



######################################################################### NDVI
#wtf is the file name
nms<-data.frame(name=list.files("./Vegetation Indices/",pattern = glob2rx('*.tif')))
nnms<-nms[grep("NDVI",nms$name),]

#dataframe of dates
sixday<-as.numeric(sapply(nnms,function(x){substr(x,39,41)}))
sixyear<-sapply(nnms,function(x){substr(x,35,38)})
sixfirst<-sapply(sixyear,function(x){paste(x,"01-01",sep="-")})
sixdate<-mapply(function(x,y){as.Date(x)+y},sixfirst,sixday)
sixdt<-as.Date(sixdate,"1970-01-01")

#MODIS NDVI data
setwd("./Vegetation Indices")
#convert to brick files
files <- sapply(nnms,function(x){brick(x)})
#scale factor
scaled<-sapply(files,function(x){x*0.0001})
#save list of brick files
save(files,file="NDVIrasters.RData")
#plots of data
for(i in 1:365) {
    png(paste("plots/", sixdt[i], ".png", sep = ""))
    temp <- plot(scaled[[i]],xlab="Longitude",
                 ylab="Latitude",main=sixdt[i])
    print(temp)
    dev.off()
}
######################################################################### EVI
#wtf is the file name
nms<-data.frame(name=list.files("./Vegetation Indices/",pattern = glob2rx('*.tif')))
enms<-nms[grep("EVI",nms$name),]

#dataframe of dates
sixday<-as.numeric(sapply(enms,function(x){substr(x,38,40)}))
sixyear<-sapply(enms,function(x){substr(x,34,37)})
sixfirst<-sapply(sixyear,function(x){paste(x,"01-01",sep="-")})
sixdate<-mapply(function(x,y){as.Date(x)+y},sixfirst,sixday)
sixdt<-as.Date(sixdate,"1970-01-01")

#MODIS EVI data
setwd("./Vegetation Indices")
#convert to brick files
files <- sapply(enms,function(x){brick(x)})
#scale factor
scaled<-sapply(files,function(x){x*0.0001})
#save list of brick files
save(files,file="EVIrasters.RData")
#plots of data
for(i in 1:365) {
    png(paste("EVI plots/", sixdt[i], ".png", sep = ""))
    temp <- plot(scaled[[i]],xlab="Longitude",
                 ylab="Latitude",main=sixdt[i])
    print(temp)
    dev.off()
}


############################################################################ LC
setwd("C:/Users/rache/OneDrive - University of Cambridge/Thailand/Explanatory Data/Land Use & Vegetation")

#wtf is the file name
vms<-data.frame(name=list.files("./Vegetation Continuous Fields/",pattern = glob2rx('*.tif')))
ntvms<-vms[grep("NonTree",vms$name),]
nvms<-vms[grep("NonVegetated_d",vms$name),]
tvms<-vms[grep("Tree_Cover_d",vms$name),]
vvms<-c(ntvms,nvms,tvms)

#dataframe of dates
ntday<-as.numeric(sapply(ntvms,function(x){substr(x,46,48)}))
ntyear<-sapply(ntvms,function(x){substr(x,42,45)})
ntfirst<-sapply(ntyear,function(x){paste(x,"01-01",sep="-")})
ntdate<-mapply(function(x,y){as.Date(x)+y},ntfirst,ntday)
ntdt<-as.Date(ntdate,"1970-01-01")

nday<-as.numeric(sapply(nvms,function(x){substr(x,40,42)}))
nyear<-sapply(nvms,function(x){substr(x,36,39)})
nfirst<-sapply(nyear,function(x){paste(x,"01-01",sep="-")})
ndate<-mapply(function(x,y){as.Date(x)+y},nfirst,nday)
ndt<-as.Date(ndate,"1970-01-01")

tday<-as.numeric(sapply(tvms,function(x){substr(x,38,40)}))
tyear<-sapply(tvms,function(x){substr(x,34,37)})
tfirst<-sapply(tyear,function(x){paste(x,"01-01",sep="-")})
tdate<-mapply(function(x,y){as.Date(x)+y},tfirst,tday)
tdt<-as.Date(tdate,"1970-01-01")

vdates<-c(ntdt,ndt,tdt)
types<-c(rep("NonTree",length(ntdt)),rep("Bare",length(ndt)),rep("Tree",length(tdt)))

#MODIS VCF data
setwd("./Vegetation Continuous Fields")
#convert to brick files
vfiles <- sapply(vvms,function(x){brick(x)})
#save list of brick files
save(vfiles,file="VCFrasters.RData")

#plots of data
library(RColorBrewer)
ntcols<-c(brewer.pal(8,"PuBuGn"),"#2171b5")
ncols<-c(brewer.pal(8,"YlOrBr"),"#2171b5")
tcols<-c(brewer.pal(8,"YlGn"),"#2171b5")

bks<-c(0,12.5,25,37.5,50,62.5,75,87.5,100,201)
lntcols<-rep(list(ntcols),length(ntdt))
lncols<-rep(list(ncols),length(ndt))
ltcols<-rep(list(tcols),length(tdt))
colors<-c(lntcols,lncols,ltcols)

for(i in 1:44) {
    png(paste("plots/",types[i],vdates[i],".png",sep=""))
    temp <- plot(vfiles[[i]],xlab="Longitude",
                 ylab="Latitude",breaks=bks,col=colors[[i]],
                 main=paste0(types[i],vdates[i],sep=" "))
    print(temp)
    dev.off()
}


#############################################################################LST
setwd("C:/Users/rache/OneDrive - University of Cambridge/Thailand/Explanatory Data/Climate")

library(rgdal)
library(raster)

#wtf is the file name
lsts<-data.frame(name=list.files("./LST/",pattern = glob2rx('*.tif')))
ldata<-lsts[-grep("QC",lsts$name),]
lstd<-ldata[grep("Day",ldata)]
lstn<-ldata[grep("Night",ldata)]
lst<-c(lstd,lstn)

#dataframe of dates
nday<-as.numeric(sapply(lstn,function(x){substr(x,34,36)}))
nyear<-sapply(lstn,function(x){substr(x,30,33)})
nfirst<-sapply(nyear,function(x){paste(x,"01-01",sep="-")})
ndate<-mapply(function(x,y){as.Date(x)+y},nfirst,nday)
ndt<-as.Date(ndate,"1970-01-01")

dday<-as.numeric(sapply(lstd,function(x){substr(x,32,34)}))
dyear<-sapply(lstd,function(x){substr(x,28,31)})
dfirst<-sapply(dyear,function(x){paste(x,"01-01",sep="-")})
ddate<-mapply(function(x,y){as.Date(x)+y},dfirst,dday)
ddt<-as.Date(ddate,"1970-01-01")

ddates<-c(ndt,ddt)
types<-c(rep("Night",length(ndt)),rep("Day",length(ddt)))

#MODIS LST data
setwd("./LST")
#convert to brick files
lfiles <- sapply(lst,function(x){brick(x)})
#save list of brick files
save(lfiles,file="LSTrasters.RData")
#convert K to F
converted<-sapply(lfiles,function(x){(x*0.02)-273.15})
save(converted,file="LST_F_rasters.RData")

#plots of data
library(RColorBrewer)
dcols<-brewer.pal(8,"OrRd")
ncols<-brewer.pal(8,"RdPu")

#bks<-c(0,12.5,25,37.5,50,62.5,75,87.5,100,201)
lncols<-rep(list(ncols),length(ndt))
ldcols<-rep(list(dcols),length(ddt))
colors<-c(lncols,ldcols)

for(i in 1:length(converted)) {
    png(paste("plots/",types[i],ddates[i],".png",sep=""))
    temp <- plot(converted[[i]],xlab="Longitude",
                 ylab="Latitude",col=colors[[i]],
                 main=paste0(types[i],ddates[i],sep=" "))
    print(temp)
    dev.off()
}

###########################################################################MORE LST
setwd("C:/Users/rache/OneDrive - University of Cambridge/Thailand/Explanatory Data/Climate")

library(rgdal)
library(raster)

#wtf is the file name
lsts<-data.frame(name=list.files("./LST/",pattern = glob2rx('*.tif')))
ldata<-lsts[-grep("QC",lsts$name),]
lstd<-ldata[grep("Day",ldata)]

#dataframe of dates
dday<-as.numeric(sapply(lstd,function(x){substr(x,32,34)}))
dyear<-sapply(lstd,function(x){substr(x,28,31)})
dfirst<-sapply(dyear,function(x){paste(x,"01-01",sep="-")})
ddate<-mapply(function(x,y){as.Date(x)+y},dfirst,dday)
ddt<-as.Date(ddate,"1970-01-01")

types<-rep("Day",length(ddt))

#MODIS LST data
setwd("./LST")
#convert to brick files
lfiles <- sapply(lstd,function(x){brick(x)})
#save list of brick files
save(lfiles,file="more_LSTrasters.RData")
#convert K to F
m_converted<-sapply(lfiles,function(x){(x*0.02)-273.15})
save(m_converted,file="more_LST_F_rasters.RData")

#plots of data
library(RColorBrewer)
dcols<-brewer.pal(8,"OrRd")

#bks<-c(0,12.5,25,37.5,50,62.5,75,87.5,100,201)
ldcols<-rep(list(dcols),length(ddt))

for(i in 1:length(m_converted)) {
    png(paste("plots/",types[i],ddt[i],".png",sep=""))
    temp <- plot(m_converted[[i]],xlab="Longitude",
                 ylab="Latitude",col=ldcols[[i]],
                 main=paste0(types[i],ddt[i],sep=" "))
    print(temp)
    dev.off()
}











#LST data
load("~/Aim2/MODIS/LSTrasters.RData")#converted

modis.0911d <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Day_1km_doy2016255_aid0001.tif")
modis.1027d <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Day_1km_doy2016301_aid0001.tif")
modis.1121d <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Day_1km_doy2016326_aid0001.tif")
modis.1223d <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Day_1km_doy2016358_aid0001.tif")
modis.0101d <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Day_1km_doy2017001_aid0001.tif")
modis.0122d <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Day_1km_doy2017022_aid0001.tif")
modis.0131d <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Day_1km_doy2017031_aid0001.tif")
modis.0205d <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Day_1km_doy2017036_aid0001.tif")
modis.0209d <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Day_1km_doy2017040_aid0001.tif")
modis.0211d <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Day_1km_doy2017042_aid0001.tif")
modis.0221d <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Day_1km_doy2017052_aid0001.tif")
modis.0223d <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Day_1km_doy2017054_aid0001.tif")
modis.0302d <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Day_1km_doy2017063_aid0001.tif")
modis.0310d <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Day_1km_doy2017069_aid0001.tif")
modis.0316d <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Day_1km_doy2017075_aid0001.tif")
modis.0904n <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Night_1km_doy2016248_aid0001.tif")
modis.0916n <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Night_1km_doy2016260_aid0001.tif")
modis.1006n <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Night_1km_doy2016280_aid0001.tif")
modis.1029n <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Night_1km_doy2016303_aid0001.tif")
modis.1104n <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Night_1km_doy2016309_aid0001.tif")
modis.1122n <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Night_1km_doy2016327_aid0001.tif")
modis.1123n <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Night_1km_doy2016328_aid0001.tif")
modis.1220n <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Night_1km_doy2016355_aid0001.tif")
modis.1231n <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Night_1km_doy2016366_aid0001.tif")
modis.0118n <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Night_1km_doy2017018_aid0001.tif")
modis.0122n <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Night_1km_doy2017022_aid0001.tif")
modis.0125n <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Night_1km_doy2017025_aid0001.tif")
plot(modis.1231n)
lines(b2)

#rasters
septofeb<-list(modis.0911d,modis.1027d,modis.1121d,
               modis.1223d,modis.0101d,modis.0122d,
               modis.0131d,modis.0205d,modis.0209d,
               modis.0211d,modis.0221d,modis.0223d,
               modis.0904n,modis.0916n,modis.1006n,
               modis.1029n,modis.1104n,modis.1122n,
               modis.1123n,modis.1220n,modis.1231n,
               modis.0118n,modis.0122n,modis.0125n)
#convert to f
sepfeb<-sapply(septofeb,function(x){(x*0.02)-273.15})
#stacks of monthly data ( days and nights)
sepd.r<-stack(sepfeb[[1]])
octd.r<-stack(sepfeb[[2]])
novd.r<-stack(sepfeb[[3]])
decd.r<-stack(sepfeb[[4]])
jand.r<-stack(sepfeb[[5]],sepfeb[[6]],sepfeb[[7]])
febd.r<-stack(sepfeb[[8]],sepfeb[[9]],sepfeb[[10]],sepfeb[[11]],sepfeb[[12]])
sepn.r<-stack(sepfeb[[13]],sepfeb[[14]])
octn.r<-stack(sepfeb[[15]],sepfeb[[16]])
novn.r<-stack(sepfeb[[17]],sepfeb[[18]],sepfeb[[19]])
decn.r<-stack(sepfeb[[20]],sepfeb[[21]])
jann.r<-stack(sepfeb[[22]],sepfeb[[23]],sepfeb[[24]])

sep.max<-max(sepd.r,na.rm=T)
oct.max<-max(octd.r,na.rm=T)
nov.max<-max(novd.r,na.rm=T)
dec.max<-max(dacd.r,na.rm=T)
jan.max<-max(jand.r,na.rm=T)
feb.max<-max(febd.r,na.rm=T)
sep.max<-max(sepd.r,na.rm=T)
oct.mean<-overlay(oct.r,fun=mean)
nov.mean<-overlay(nov.r,fun=mean)
dec.mean<-overlay(dec.r,fun=mean)
jan.mean<-overlay(jan.r,fun=mean)
feb.mean<-overlay(feb.r,fun=mean)
values(sep.max)[values(sep.max)== 0] = NA

modis.1121d<-(modis.1121d*0.02)-273.15
plot(modis.1121d)
lines(b2)
r2 <- raster(b2, nrow=3,ncol=4)#empty raster of city
cr <- crop(modis.1029n, extent(b2), snap="out")
res(r2)<-res(cr)
fr <- rasterize(b2, cr)   
fr1<-crop(fr,extent(b2))
lr <- mask(x=cr, mask=fr)
plot(lr)
lr1 <- cover(x=cr, mask=fr1)
plot(lr1)

values(r2)<-as.vector(cr)
plot(r2)
lines(b2)


#################8day avgs
setwd("~/Aim2/modis8")
load("~/Aim2/modis8/LST8rasters.RData")#converted

#wtf is the file name
nms<-data.frame(list.files(pattern = glob2rx('*.tif')))

#dataframe of dates
day<-head(nms,50)
six<-head(day,15)
seven<-tail(day,35)
sixday<-as.numeric(sapply(six,function(x){substr(x,32,34)}))-1
sevenday<-as.numeric(sapply(seven,function(x){substr(x,32,34)}))
night<-data.frame(nms[51:100,])
sixn<-head(night,15)
sevenn<-tail(night,35)
sixnight<-as.numeric(sapply(sixn,function(x){substr(x,34,36)}))-1
sevennight<-as.numeric(sapply(sevenn,function(x){substr(x,34,36)}))
sixdate<-data.frame(lapply(sixday,function(x){as.Date(x,origin=as.Date("2016-01-01"))}))
sevendate<-data.frame(lapply(sevenday,function(x){as.Date(x,origin=as.Date("2017-01-01"))}))
sixdate2<-data.frame(lapply(sixnight,function(x){as.Date(x,origin=as.Date("2016-01-01"))}))
sevendate2<-data.frame(lapply(sevennight,function(x){as.Date(x,origin=as.Date("2017-01-01"))}))

day3<-data.frame(nms[101:150,])
six3<-head(day3,15)
seven3<-tail(day3,35)
sixday3<-as.numeric(sapply(six3,function(x){substr(x,32,34)}))-1
sevenday3<-as.numeric(sapply(seven3,function(x){substr(x,32,34)}))
night3<-data.frame(nms[151:200,])
sixn3<-head(night3,15)
sevenn3<-tail(night3,35)
sixnight3<-as.numeric(sapply(sixn3,function(x){substr(x,34,36)}))-1
sevennight3<-as.numeric(sapply(sevenn3,function(x){substr(x,34,36)}))
sixdate3<-data.frame(lapply(sixday3,function(x){as.Date(x,origin=as.Date("2016-01-01"))}))
sevendate3<-data.frame(lapply(sevenday3,function(x){as.Date(x,origin=as.Date("2017-01-01"))}))
sixdate23<-data.frame(lapply(sixnight3,function(x){as.Date(x,origin=as.Date("2016-01-01"))}))
sevendate23<-data.frame(lapply(sevennight3,function(x){as.Date(x,origin=as.Date("2017-01-01"))}))

datelist<-t(data.frame(c(sixdate,sevendate,sixdate2,sevendate2,sixdate3,
                         sevendate3,sixdate23,sevendate23)))
attributes(datelist)<-NULL

#data frame of times
times<-sapply(day,function(x){substr(x,17,19)})
times2<-sapply(night,function(x){substr(x,17,21)})
times3<-sapply(day3,function(x){substr(x,17,19)})
times4<-sapply(night3,function(x){substr(x,17,21)})

timelist<-as.vector(c(times,times2,times3,times4))
i <- sapply(timelist, is.factor)
timelist[i] <- lapply(timelist[i], as.character)
#list of titles for plots
sats<-c(rep("MOD",100),rep("MYD",100))
titles<-paste(datelist,timelist,sats,sep=" ")

#MODIS LST data
tiflist<-as.character(list.files(pattern = glob2rx('*.tif')))
#convert to brick files
files <- sapply(tiflist,function(x){raster(x)})
#convert K to F
converted<-sapply(files,function(x){(x*0.02)-273.15})
#save list of brick files
save(converted,file="LST8rasters.RData")

#plots of data
for(i in 1:length(tiflist)) {
    png(paste("plots/", titles[i], ".png", sep = ""))
    temp <- plot(converted[[i]],xlab="Longitude",
                 ylab="Latitude",main=titles[i])
    print(temp)
    lines(b2)
    dev.off()
}

modis1 <- raster("~/Aim2/modis8/MOD11A2.006_LST_Night_1km_doy2017233_aid0001.tif")
plot(modis1)
lines(b2)

#stacks of monthly data ( days and nights)
modis.0911d <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Day_1km_doy2016255_aid0001.tif")
modis.1223d <- raster("~/Aim2/MODIS/MOD11A1.005_LST_Day_1km_doy2016358_aid0001.tif")

sepd.r<-(modis.0911d*0.02)-273.15
decd.r<-(modis.1223d*0.02)-273.15

octd.r<-converted[[108]]
novd.r<-stack(converted[[10]],converted[[110]])
jand.r<-stack(converted[[18]],converted[[118]])
febd.r<-stack(converted[[20]],converted[[123]])
mard.r<-converted[[27]]
aprd.r<-converted[[29]]
mayd.r<-stack(converted[[31]],converted[[32]])
jund.r<-converted[[135]]
juld.r<-stack(converted[[39]],converted[[40]],converted[[41]])
augd.r<-stack(converted[[43]],converted[[44]])
sep2d.r<-converted[[147]]

sepn.r<-stack(sepfeb[[13]],sepfeb[[14]])#?
decn.r<-stack(converted[[20]],converted[[21]])#?
augn.r<-stack(converted[[20]],converted[[21]])#?
sep2n.r<-stack(converted[[20]],converted[[21]])#?

octn.r<-stack(converted[[58]],converted[[155]],converted[[158]])
novn.r<-converted[[60]]
jann.r<-stack(converted[[68]],converted[[69]],converted[[168]])
febn.r<-converted[[170]]
marn.r<-converted[[177]]
aprn.r<-converted[[79]]
mayn.r<-converted[[183]]
junn.r<-stack(converted[[185]],converted[[187]])
juln.r<-stack(converted[[189]],converted[[92]],converted[[191]],converted[[192]])

#mean rasters
oct.mean<-octd.r
nov.mean<-mean(novd.r,na.rm=T)
jan.mean<-mean(jand.r,na.rm=T)
feb.mean<-mean(febd.r,na.rm=T)
mar.mean<-mard.r
apr.mean<-aprd.r
may.mean<-mean(mayd.r,na.rm=T)
jun.mean<-jund.r
jul.mean<-mean(juld.r,na.rm=T)
aug.mean<-mean(augd.r,na.rm=T)
sep2.mean<-sep2d.r

sep.mean<-sepd.r
dec.mean<-decd.r

##feb and apr and sep2 (partials)
plot(converted[[20]])
plot(converted[[123]])
plot(feb.mean)
febc.mean<-feb.mean
febc.mean[4,7]<-mean(febc.mean[3,6],febc.mean[3,7],febc.mean[3,8],febc.mean[4,6],
                     febc.mean[4,8],febc.mean[5,6],febc.mean[5,7],febc.mean[5,8])
plot(apr.mean)
aprc.mean<-apr.mean
aprc.mean[4,7]<-mean(aprc.mean[5,6],aprc.mean[5,7])
aprc.mean[4,8]<-mean(aprc.mean[5,7])
aprc.mean[5,8]<-mean(aprc.mean[5,7],aprc.mean[6,7],aprc.mean[6,8])
aprc.mean[5,9]<-mean(aprc.mean[6,8])
aprc.mean[6,9]<-mean(aprc.mean[6,8],aprc.mean[7,8],aprc.mean[7,9])

plot(sep2.mean)
sep2c.mean<-sep2.mean
sep2c.mean[5,9]<-mean(sep2c.mean[5,8],sep2c.mean[6,8],sep2c.mean[6,9])
plot(sep2c.mean)
lines(b2)

##sep and dec
plot(sep.mean)
sepc.mean<-sep.mean
sepc.mean[5,6]<-mean(sepc.mean[6,7])
sepc.mean[5,7]<-mean(sepc.mean[6,7],sepc.mean[6,8])
sepc.mean[5,8]<-mean(sepc.mean[6,7],sepc.mean[6,8],sepc.mean[6,9],sepc.mean[5,9])
sepc.mean[4,7]<-mean(sepc.mean[5,6],sepc.mean[5,7],sepc.mean[5,8])
sepc.mean[4,8]<-mean(sepc.mean[5,7],sepc.mean[5,8],sepc.mean[5,9])
plot(sepc.mean)
lines(b2)

plot(dec.mean)
decc.mean<-dec.mean
decc.mean[6,7]<-mean(decc.mean[6,6],decc.mean[7,6],decc.mean[7,7],decc.mean[7,8],
                     decc.mean[6,8],decc.mean[5,7])
decc.mean[5,6]<-mean(decc.mean[6,6],decc.mean[5,7])
decc.mean[4,7]<-mean(decc.mean[5,7])
decc.mean[4,8]<-mean(decc.mean[5,7])
decc.mean[5,8]<-mean(decc.mean[5,7],decc.mean[6,8])
decc.mean[5,9]<-mean(decc.mean[6,8],decc.mean[6,9])
plot(decc.mean)
lines(b2)

#city-sized rasters
o.mod <- crop(oct.mean, extent(b2), snap="out")
o.mod[1,1] <- NA
o.mod[1,4] <- NA
o.mod[3,1] <- NA
n.mod <- crop(nov.mean, extent(b2), snap="out")
n.mod[1,1] <- NA
n.mod[1,4] <- NA
n.mod[3,1] <- NA
j.mod <- crop(jan.mean, extent(b2), snap="out")
j.mod[1,1] <- NA
j.mod[1,4] <- NA
j.mod[3,1] <- NA
f.mod <- crop(feb.mean, extent(b2), snap="out")
f.mod[1,1] <- NA
f.mod[1,4] <- NA
f.mod[3,1] <- NA
r.mod <- crop(mar.mean, extent(b2), snap="out")
r.mod[1,1] <- NA
r.mod[1,4] <- NA
r.mod[3,1] <- NA
a.mod <- crop(apr.mean, extent(b2), snap="out")
a.mod[1,1] <- NA
a.mod[1,4] <- NA
a.mod[3,1] <- NA
m.mod <- crop(may.mean, extent(b2), snap="out")
m.mod[1,1] <- NA
m.mod[1,4] <- NA
m.mod[3,1] <- NA
u.mod <- crop(jun.mean, extent(b2), snap="out")
u.mod[1,1] <- NA
u.mod[1,4] <- NA
u.mod[3,1] <- NA
l.mod <- crop(jul.mean, extent(b2), snap="out")
l.mod[1,1] <- NA
l.mod[1,4] <- NA
l.mod[3,1] <- NA
g.mod <- crop(aug.mean, extent(b2), snap="out")
g.mod[1,1] <- NA
g.mod[1,4] <- NA
g.mod[3,1] <- NA

s2.mod <- crop(sep2c.mean, extent(b2), snap="out")
s2.mod[1,1] <- NA
s2.mod[1,4] <- NA
s2.mod[3,1] <- NA
f.mod <- crop(febc.mean, extent(b2), snap="out")
f.mod[1,1] <- NA
f.mod[1,4] <- NA
f.mod[3,1] <- NA
a.mod <- crop(aprc.mean, extent(b2), snap="out")
a.mod[1,1] <- NA
a.mod[1,4] <- NA
a.mod[3,1] <- NA
s.mod <- crop(sepc.mean, extent(b2), snap="out")
s.mod[1,1] <- NA
s.mod[1,4] <- NA
s.mod[3,1] <- NA
d.mod <- crop(decc.mean, extent(b2), snap="out")
d.mod[1,1] <- NA
d.mod[1,4] <- NA
d.mod[3,1] <- NA

mod.st<-stack(s.mod,o.mod,n.mod,d.mod,j.mod,f.mod,r.mod,a.mod,m.mod,u.mod,
              l.mod,g.mod,s2.mod)
save(mod.st,file="modisrasters.RData")

#modis for clim data
load("~/Aim3/allclim.RData")#converted
coord<-data.frame(lon=allclim$lon,lat=allclim$lat)
allclim.sp<-SpatialPointsDataFrame(coords=coord,data=allclim,proj4string = CRS("+init=epsg:4326"))

plot(s.mod)
lines(b2)
plot(allclim.sp,add=T)

all

r<-raster(nrow=3,ncol=4,extent(b2))
values(r)<-c(1:12)
plot(r)
plot(allclim.sp[282:283,],pch=20,add=T)

allclim$modis<-1
#
allclim[1:16,21]<-24.19#sept
allclim[17:18,21]<-24.19#sep
allclim[51:62,21]<-26.43#dect
allclim[201:216,21]<-27.85#augt
allclim[c(217:254,257:275),21]<-27.85#aug
allclim[255,21]<-27.64#aug
allclim[c(256,277),21]<-27.29#aug
allclim[276,21]<-27.01#aug
allclim[35:50,21]<-28.71#novt
allclim[95:110,21]<-28.13#mart
allclim[63:78,21]<-32.97#jant
allclim[160:175,21]<-27.14#jult
allclim[176:200,21]<-26.64#jul
allclim[156:159,21]<-32.45#jun
allclim[140:151,21]<-32.97#junt
allclim[152:155,21]<-32.45#junt
allclim[127:136,21]<-27.52#mayt
allclim[137:139,21]<-27.98#mayt
allclim[111:122,21]<-27.07#aprt
allclim[123:126,21]<-25.93#aprt
allclim[79:94,21]<-28.83#febt
allclim[23:34,21]<-31.39#octt
allclim[19:22,21]<-31.63#octt
allclim[278:281,21]<-27.14#jult
allclim[282:283,21]<-26.43#dect

save(allclim,file="climwmodis.RData")


#save min max values and retreive
doy2016255<-setMinMax(doy2016255)
cellStats(doy2016255,min)
cellStats(doy2016255,max)
cellStats(doy2016255,range)

#raster values
hist(doy2016255, main="Distribution of LST values", 
     col= "purple")

hist(converted$MOD11A1.005_LST_Day_1km_doy2016249_aid0001.tif,
     main="Distribution of LST Values", col='purple')

mins[1]
