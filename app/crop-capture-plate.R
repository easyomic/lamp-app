library(imager)

lf=as.list(list.files(path = "www/lampcurva-padrao-27-10-2022",
              pattern="*.png", all.files=FALSE,
              full.names=F, include.dirs = F))
lf
lf[[1]]

for (photo in lf) {
picnames=paste0("www/lampcurva-padrao-27-10-2022/",photo)
picfiles=paste0("crop/crop-",photo)
png(filename = picfiles, width = 925, height = 275)
pic=imager::load.image(picnames)
par(mar=c(0,0,0,0))
imsub(pic,x %inr% c(100,285),y %inr% c(110,165)) %>% plot
dev.off()
}

for (photo in lf) {
picnames=paste0("www/lampcurva-padrao-27-10-2022/",photo)
print(picnames)
}
