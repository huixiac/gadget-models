library(Rgadget)
library(tidyverse)
library(grid)
library(gridExtra)

## rm(list=ls())

source("~/../valerio/Share/Gadget/Rscripts/ggdata_coverage.R")
source("~/../valerio/Share/Gadget/Rscripts/add_captionModel.R")
source("~/../valerio/Share/Gadget/Rscripts/ggplot_AgeLenDistributionStock.R")

setwd(gd$dir)

tmp <- gadget.iterative(rew.sI=TRUE,
                        grouping=list(
                          sind=c('si0.spr.rct3','si1.spr.bias','si2.spr.bias','si3.spr.bias','si4.spr.bias','si5.spr.bias','si6.spr.bias','si7.spr.bias','si8.spr.bias')),                       
                        params.file='params.in',
                        wgts='WGTS')

fit <- gadget.fit(main="WGTS/main.final",
                  f.age.range=data.frame(stock=c("sprimm","sprmat"),age.min=1,age.max=3))

## load("WGTS/WGTS.Rdata")
## fit <- out

# ------------------------------------------
# standard plots
dirFigs <- "out"
if(sum(match(list.files(),dirFigs), na.rm=T)==1){
    print(paste("folder",dirFigs,"exists"))
} else {system(paste("mkdir",dirFigs))}

figName <- "sidat_dir.ps"
  plot(fit, data = "sidat", type = "direct") +
  ggRunName() + ggRunNameSize(6) +
  ggsave(paste(dirFigs,figName, sep="/"), device="ps")

figName <- "sidat_xy.ps"
cairo_ps(paste(dirFigs,, sep="/"))
  plot(fit, data = "sidat", type = "x-y") +
  ggRunName() + ggRunNameSize(6) +
  ggsave(paste(dirFigs,figName, sep="/"), device="ps")

figName <- "summary.ps"
cairo_ps(paste(dirFigs,, sep="/"))
  plot(fit, data='summary') +
  ggRunName() + ggRunNameSize(6) +
  ggsave(paste(dirFigs,figName, sep="/"), device="ps")

figName <- "summary_wgt.ps"
cairo_ps(paste(dirFigs,"summary_wgt.ps", sep="/"))
  plot(fit, data='summary',type = 'weighted') +
  ggRunName() + ggRunNameSize(6) +
  ggsave(paste(dirFigs,figName, sep="/"), device="ps")

figName <- "summary_pie.ps"
cairo_ps(paste(dirFigs,"summary_pie.ps", sep="/"))
  plot(fit, data='summary',type='pie') +
  ggRunName() + ggRunNameSize(6) +
  ggsave(paste(dirFigs,figName, sep="/"), device="ps")

figName <- "catch.ps"
cairo_ps(paste(dirFigs,"catch.ps", sep="/"))
  plot(fit, data="res.by.year", type="catch") + facet_wrap(~stock) +
  ggRunName() + ggRunNameSize(6) +
  ggsave(paste(dirFigs,figName, sep="/"), device="ps")

figName <- "F.ps"
cairo_ps(paste(dirFigs,"F.ps", sep="/"))
  plot(fit, data="res.by.year", type="F") + facet_wrap(~stock) +
  ggRunName() + ggRunNameSize(6) +
  ggsave(paste(dirFigs,figName, sep="/"), device="ps")

figName <- "total.ps"
cairo_ps(paste(dirFigs,"total.ps", sep="/"))
  plot(fit, data="res.by.year", type="total") + facet_wrap(~stock) +
  ggRunName() + ggRunNameSize(6) +
  ggsave(paste(dirFigs,figName, sep="/"), device="ps")

figName <- "rec.ps"
cairo_ps(paste(dirFigs,"rec.ps", sep="/"))
  plot(fit, data="res.by.year", type="rec") + facet_wrap(~stock) +
  ggRunName() + ggRunNameSize(6) +
  ggsave(paste(dirFigs,figName, sep="/"), device="ps")

figName <- "adist_stk_sprmat.ps"
  ## plot(fit, data='stock.std')# + facet_wrap(~stock) +
  ## ggRunName() + ggRunNameSize(6) +
  ## ggsave(paste(dirFigs,figName, sep="/"), device="ps")
ggAgeDistStk2(fit, stkName="sprmat", ageVec=1:8, plusGroup=8) +
  ggRunName() + ggRunNameSize(6) +
  ggsave(paste(dirFigs,figName, sep="/"), device="ps")

figName <- "suitability.ps"
##   plot(fit, data='suitability') +
##   ggRunName() + ggRunNameSize(6) +
##   ggsave(paste(dirFigs,figName, sep="/"), device="ps")
ggplot(fit$suitability %>% filter(stock == "sprmat" & step == 4 & year == 2000)) +
    geom_line(aes(length,suit,col=fleet)) +
    xlim(8,15) +
    facet_wrap(~year) +
    ggRunName() + ggRunNameSize(6) +
    ggsave(paste(dirFigs,figName, sep="/"), device="ps")

tmp <- plot(fit,data = 'catchdist.fleets')
  names(tmp)

figName <- "ldist_spr_bias.ps"
cairo_ps(paste(dirFigs,"ldist_spr_bias.ps", sep="/"))
  tmp$ldist.spr.bias +
  ggRunName() + ggRunNameSize(6) +
  ggsave(paste(dirFigs,figName, sep="/"), device="ps")
  
figName <- "alk_spr_bias.ps"
  tmp$alk.spr.bias +
  ggRunName() + ggRunNameSize(6) +
  ggsave(paste(dirFigs,figName, sep="/"), device="ps")

figName <- "adist_spr_com1.ps"
  tmp$adist.spr.com1 +
  ggRunName() + ggRunNameSize(6) +
  ggsave(paste(dirFigs,figName, sep="/"), device="ps")
  
figName <- "adist_spr_com2.ps"
  tmp$adist.spr.com2 +
  ggRunName() + ggRunNameSize(6) +
  ggsave(paste(dirFigs,figName, sep="/"), device="ps")
  
## figName <- 
## cairo_ps(paste(dirFigs,"adist_seal.ps", sep="/"))
##   tmp$adist.ven.seal +
##   ggRunName() + ggRunNameSize(6) +
##   ggsave(paste(dirFigs,figName, sep="/"), device="ps")
  
bubbles <- plot(fit,data = 'catchdist.fleets',type='bubble')
  names(bubbles)

figName <- "bubble_ldist_spr_bias.ps"
  bubbles$ldist +
  ggRunName() + ggRunNameSize(6) +
  ggsave(paste(dirFigs,figName, sep="/"), device="ps")
  
figName <- "bubble_aldist_spr.ps"
  bubbles$aldist +
  ggRunName() + ggRunNameSize(6) +
  ggsave(paste(dirFigs,figName, sep="/"), device="ps")
  
grplot <- plot(fit,data = 'catchdist.fleets',type='growth')
  names(grplot)

figName <- "growth_spr_bias.ps"
  grplot$alk.spr.bias +
  ggRunName() + ggRunNameSize(6) +
  ggsave(paste(dirFigs,figName, sep="/"), device="ps")

## tmp <- plot(fit,data = 'stockdist')
##   names(tmp)

## figName <- "mat_com.ps"
##     tmp$mat.ven.com + xlim(9,18) +
##   ggRunName() + ggRunNameSize(6) +
   ## ggsave(paste(dirFigs,figName, sep="/"), device="ps")

figName <- "data_coverage.ps"
  ggDataCoverage(fit) +
  ggRunName() + ggRunNameSize(6) +
  ggsave(paste(dirFigs,figName, sep="/"), device="ps")
  
# merge into a PDF
system(paste("psmerge $(ls ",dirFigs,"/*.ps) > ",dirFigs,"/figs_all.ps ; ps2pdf ",dirFigs,"/figs_all.ps ",dirFigs,"/figs_all.pdf ; rm ",dirFigs,"/figs_all.ps",sep=""))
## system(paste("convert -density 120x120 -quality 100 $(ls ",dirFigs,"/*.ps) ",dirFigs,"/figs_all.pdf",sep=""))
