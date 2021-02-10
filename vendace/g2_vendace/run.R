library(Rgadget)
library(ggplot2)
library(grid)
library(gridExtra)

## rm(list=ls())

source("~/../valerio/Share/Gadget/Rscripts/ggdata_coverage.R")
source("~/../valerio/Share/Gadget/Rscripts/add_captionModel.R")
source("~/../valerio/Share/Gadget/Rscripts/ggplot_AgeLenDistributionStock.R")

setwd(gd$dir)

tmp <- gadget.iterative(rew.sI=TRUE,
                        grouping=list(
                            ind=c('si.ven.cpue','si.ven.aco')),
                        cv.floor=0.01,
                        params.file='params.in',
                        wgts="WGTS",
                        main='main')

fit <- gadget.fit(main="WGTS/main.final",
                  f.age.range=data.frame(stock=c("venimm","venmat"),age.min=1,age.max=3))

## setwd("vendace01")
## load("WGTS/WGTS.Rdata")
## fit <- out

# ------------------------------------------
# standard plots
dirFigs <- "out"
if(sum(match(list.files(),dirFigs), na.rm=T)==1){
    print(paste("folder",dirFigs,"exists"))
} else {system(paste("mkdir",dirFigs))}

## cairo_ps(paste(dirFigs,"sidat_dir.ps", sep="/"))
  plot(fit, data = "sidat", type = "direct") +
    ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"sidat_dir.ps", sep="/"), device="ps")
## dev.off()
## cairo_ps(paste(dirFigs,"sidat_xy.ps", sep="/"))
  plot(fit, data = "sidat", type = "x-y") +
  ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"sidat_xy.ps", sep="/"), device="ps")
## dev.off()
## cairo_ps(paste(dirFigs,"summary.ps", sep="/"))
  plot(fit, data='summary') +
  ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"summary.ps", sep="/"), device="ps")
## dev.off()
## cairo_ps(paste(dirFigs,"summary_wgt.ps", sep="/"))
  plot(fit, data='summary',type = 'weighted') +
  ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"summary_wgt.ps", sep="/"), device="ps")
## dev.off()
## cairo_ps(paste(dirFigs,"summary_pie.ps", sep="/"))
  plot(fit, data='summary',type='pie') +
  ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"summary_pie.ps", sep="/"), device="ps")
## dev.off()
## cairo_ps(paste(dirFigs,"catch.ps", sep="/"))
  plot(fit, data="res.by.year", type="catch") + facet_wrap(~stock) +
  ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"catch.ps", sep="/"), device="ps")
## dev.off()
## cairo_ps(paste(dirFigs,"F.ps", sep="/"))
  plot(fit, data="res.by.year", type="F") + facet_wrap(~stock) +
  ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"f.ps", sep="/"), device="ps")
## dev.off()
## cairo_ps(paste(dirFigs,"total.ps", sep="/"))
  plot(fit, data="res.by.year", type="total") + facet_wrap(~stock) +
  ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"total.ps", sep="/"), device="ps")
## dev.off()
## cairo_ps(paste(dirFigs,"rec.ps", sep="/"))
  plot(fit, data="res.by.year", type="rec") + facet_wrap(~stock) +
  ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"rec.ps", sep="/"), device="ps")
## dev.off()
## cairo_ps(paste(dirFigs,"stock_demo.ps", sep="/"))
##   plot(fit, data='stock.std')# + facet_wrap(~stock) +
##   ggRunName() + ggRunNameSize(6)
## dev.off()
## cairo_ps(paste(dirFigs,"stock_demo_venmat.ps", sep="/"))
    ggAgeDistStk2(fit, stkName="venmat", ageVec=1:10, plusGroup=10) +
    ggRunName() + ggRunNameSize(6) +
    ggsave(paste(dirFigs,"stock_demo_venmat.ps", sep="/"), device="ps")
## dev.off()
## cairo_ps(paste(dirFigs,"suitability.ps", sep="/"))
  plot(fit, data='suitability') +
      ggRunName() + ggRunNameSize(6) +
      ggsave(paste(dirFigs,"suitability.ps", sep="/"), device="ps")
## dev.off()
tmp <- plot(fit,data = 'catchdist.fleets')
  names(tmp)
  ## cairo_ps(paste(dirFigs,"ldist_com.ps", sep="/"))
    tmp$ldist.ven.com +
        ggRunName() + ggRunNameSize(6) +
        ggsave(paste(dirFigs,"ldist_com.ps", sep="/"), device="ps")
  ## dev.off()
  ## cairo_ps(paste(dirFigs,"alk_com.ps", sep="/"))
    tmp$alk.ven.com +
        ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"alk_com.ps", sep="/"), device="ps")
## dev.off()
  ## cairo_ps(paste(dirFigs,"ldist_aco.ps", sep="/"))
    tmp$ldist.ven.aco +
  ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"ldist_aco.ps", sep="/"), device="ps")
  ## dev.off()
  ## cairo_ps(paste(dirFigs,"alk_aco.ps", sep="/"))
  tmp$alk.ven.aco +
    ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"alk_aco.ps", sep="/"), device="ps")
  ## dev.off()
  ## cairo_ps(paste(dirFigs,"adist_ref.ps", sep="/"))
  ##   tmp$adist.ven.cpue +
  ## ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"adist_ref.ps", sep="/"), device="ps")
  ## dev.off()
  ## cairo_ps(paste(dirFigs,"adist_seal.ps", sep="/"))
  ##   tmp$adist.ven.seal +
  ## ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"adist_seal.ps", sep="/"), device="ps")
  ## dev.off()
bubbles <- plot(fit,data = 'catchdist.fleets',type='bubble')
  names(bubbles)
  ## cairo_ps(paste(dirFigs,"bubble_ldist.ps", sep="/"))
    bubbles$ldist +
  ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"bubble_ldist.ps", sep="/"), device="ps")
  ## dev.off()
  ## cairo_ps(paste(dirFigs,"bubble_adist.ps", sep="/"))
    bubbles$aldist +
  ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"bubble_adist.ps", sep="/"), device="ps")
  ## dev.off()
grplot <- plot(fit,data = 'catchdist.fleets',type='growth')
  names(grplot)
  ## cairo_ps(paste(dirFigs,"growth_com.ps", sep="/"))
    grplot$alk.ven.com +
  ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"growth_com.ps", sep="/"), device="ps")
  ## dev.off()
  ## cairo_ps(paste(dirFigs,"growth_aco.ps", sep="/"))
  grplot$alk.ven.aco +
    ggRunName() + ggRunNameSize(6)
  ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"growth_aco.ps", sep="/"), device="ps")
  ## dev.off()
  tmp <- plot(fit,data = 'stockdist')
  names(tmp)
  ## cairo_ps(paste(dirFigs,"mat_com.ps", sep="/"))
    tmp$mat.ven.com + xlim(9,18) +
  ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"mat_com.ps", sep="/"), device="ps")
  ## dev.off()
  ## cairo_ps(paste(dirFigs,"mat_aco.ps", sep="/"))
    tmp$mat.ven.aco + xlim(9,18) +
  ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"mat_aco.ps", sep="/"), device="ps")
  ## dev.off()
  ## cairo_ps(paste(dirFigs,"data_coverage.ps", sep="/"))
    ggDataCoverage(fit) +
  ggRunName() + ggRunNameSize(6) + ggsave(paste(dirFigs,"data_coverage.ps", sep="/"), device="ps")
  ## dev.off()
# standard advice plot (catch, SSB, F, R)
tmp <- fit$res.by.year %>% group_by(year,area) %>% summarise(catch=sum(catch))
  catch.out <- ggplot(tmp) + geom_bar(aes(year,catch), stat="identity") +
  ggRunName() + ggRunNameSize(6)
tmp <- fit$res.by.year %>% filter(stock=="venmat")
  ssb.out <- ggplot(tmp, aes(year,total.biomass)) + geom_line() + geom_point() +
        ylim(0,max(tmp$total.biomass)) + ylab("SSB") + ggRunName() + ggRunNameSize(6)
tmp <- fit$res.by.year %>% filter(stock=="venimm")
  rec.out <- ggplot(tmp, aes(year,total.number)) + geom_line() + geom_point() +
        ylim(0,max(tmp$total.number)) + ylab("Number immature") + ggRunName() + ggRunNameSize(6)
tmp <- fit$res.by.year %>% filter(stock=="venmat")
  fbar.out <- ggplot(tmp, aes(year,F)) + geom_line() + geom_point() +
        ylim(0,max(tmp$F)) + ggRunName() + ggRunNameSize(6)
## cairo_ps(paste(dirFigs,"standard_plot.ps", sep="/"))
  grid.arrange(catch.out,ssb.out,rec.out,fbar.out,ncol=2) + ggsave(paste(dirFigs,"standard_plot.ps", sep="/"), device="ps")
## dev.off()
# comparison with SS3 output
ss3.ssb <- read.table("~/../valerio/Share/Gadget/vendace/SS3_output/SSB.csv", header=T, sep=",")
ss3.rec <- read.table("~/../valerio/Share/Gadget/vendace/SS3_output/R.csv", header=T, sep=",")
ss3.fbar <- read.table("~/../valerio/Share/Gadget/vendace/SS3_output/Fbar.csv", header=T, sep=",")
ssbcomp <- ssb.out +
           geom_point(data=ss3.ssb,aes(Years,SSB*1000),pch=2) +
           geom_line(data=ss3.ssb,aes(Years,SSB*1000),lty=2) +
           ylim(0,max(c(ss3.ssb$SSB*1000))) +
           ggRunName() + ggRunNameSize(6)
fbarcomp <- fbar.out +
            geom_point(data=ss3.fbar %>% filter(Yr<=2017),aes(Yr,fbar),pch=2) +
            geom_line(data=ss3.fbar %>% filter(Yr<=2017),aes(Yr,fbar),lty=2) +
            ## ylim(0,max(c(ss3.fbar$fbar))) +
            ggRunName() + ggRunNameSize(6)
## reccomp <- rec.out +
##            geom_point(data=ss3.rec,aes(Years,R*500),pch=2) +
##            geom_line(data=ss3.rec,aes(Years,R*500),lty=2) +
##            ylim(0,max(c(ss3.rec$R*500))) +
##            ggRunName() + ggRunNameSize(6)

# Retrospective analysis
gadget.retro(params.file="params.in", main.file="main",
             grouping=list(ind=c('si.ven.cpue','si.ven.aco')),
             cv.floor=0.01,
             num.years=5, iterative=T)

fit1 <- gadget.fit(main="RETRO/WGTS.1/main.final",
                   f.age.range=data.frame(stock=c("venimm","venmat"),age.min=1,age.max=3),
                   fit.folder ="RETRO/WGTS.1", params.file="RETRO/WGTS.1/params.final")
fit2 <- gadget.fit(main="RETRO/WGTS.2/main.final",
                   f.age.range=data.frame(stock=c("venimm","venmat"),age.min=1,age.max=3),
                   fit.folder ="RETRO/WGTS.2", params.file="RETRO/WGTS.2/params.final")
fit3 <- gadget.fit(main="RETRO/WGTS.3/main.final",
                   f.age.range=data.frame(stock=c("venimm","venmat"),age.min=1,age.max=3),
                   fit.folder ="RETRO/WGTS.3", params.file="RETRO/WGTS.3/params.final")
fit4 <- gadget.fit(main="RETRO/WGTS.4/main.final",
                   f.age.range=data.frame(stock=c("venimm","venmat"),age.min=1,age.max=3),
                   fit.folder ="RETRO/WGTS.4", params.file="RETRO/WGTS.4/params.final")
fit5 <- gadget.fit(main="RETRO/WGTS.5/main.final",
                   f.age.range=data.frame(stock=c("venimm","venmat"),age.min=1,age.max=3),
                   fit.folder ="RETRO/WGTS.5", params.file="RETRO/WGTS.5/params.final")
fitL <- bind.gadget.fit(r1=fit1,r2=fit2,r3=fit3,r4=fit4,r5=fit5)

tmp <- fitL$res.by.year %>% filter(stock=="venmat")
ssb.retro <- ggplot(tmp, aes(year,total.biomass,col=model,group=model)) +
             geom_line() + geom_point() +
             ylim(0,max(tmp$total.biomass)) + ylab("SSB") +
             ggRunName() + ggRunNameSize(6)
tmp <- fitL$res.by.year %>% filter(stock=="venimm")
rec.retro <- ggplot(tmp, aes(year,total.number,col=model,group=model)) +
             geom_line() + geom_point() +
             ylim(0,max(tmp$total.number)) + ylab("Number immature") +
             ggRunName() + ggRunNameSize(6)
tmp <- fitL$res.by.year %>% filter(stock=="venmat")
fbar.retro <- ggplot(tmp, aes(year,F,col=model,group=model)) +
              geom_line() + geom_point() +
              ylim(0,max(tmp$F)) + ylab("F13") +
              ggRunName() + ggRunNameSize(6)
cairo_ps(paste(dirFigs,"retro_plot.ps", sep="/"))
  grid.arrange(ssb.retro,rec.retro,fbar.retro,ncol=3)
dev.off()



mohns.rho <- function(...){}


# merge into a PDF
system(paste("psmerge $(ls ",dirFigs,"/*.ps) > ",dirFigs,"/figs_all.ps ; ps2pdf ",dirFigs,"/figs_all.ps ",dirFigs,"/figs_all.pdf ; rm ",dirFigs,"/figs_all.ps",sep=""))
