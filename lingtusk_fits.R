library(Rgadget)
setwd('/net/hafkaldi/export/home/haf/pamela/fishvice/gadget-models/08-tusk/01-new_ass')
setwd('/net/hafkaldi/export/home/haf/pamela/fishvice/gadget-models/06-ling/12-new_ass')
setwd('/home/pamela/Documents/Hafro/fishvice/gadget-models/08-tusk/02-growth_rest_run1')
setwd('/home/pamela/Documents/Hafro/fishvice/gadget-models/08-tusk/02-growth_rest')
setwd('/home/pamela/Documents/Hafro/fishvice/gadget-models/08-tusk/03-2017')
setwd('/home/pamela/Documents/Hafro/fishvice/gadget-models/08-tusk/04-2017noage')
setwd('/home/pamela/Documents/Hafro/fishvice/gadget-models/08-tusk/05-2017noage_growth_rest')
setwd('/home/pamela/Documents/Hafro/fishvice/gadget-models/06-ling/12-new_ass_2017')
setwd('/home/pamela/Documents/Hafro/fishvice/gadget-models/06-ling/12-new_ass')
setwd('/net/hafkaldi/export/home/haf/pamela/fishvice/gadget-models/2018_lin.27.5a/13-new_ass_preprogn')
setwd('/net/hafkaldi/export/home/haf/pamela/fishvice/gadget-models/08-tusk/01-new_ass_run1')
setwd('/net/hafkaldi/export/home/haf/pamela/fishvice/gadget-models/08-tusk/02-growth_rest')
setwd('/net/hafkaldi/export/home/haf/pamela/fishvice/gadget-models/08-tusk/02-growth_rest_hafbjarmi')
setwd('/net/hafkaldi/export/home/haf/pamela/fishvice/gadget-models/08-tusk/02-growth_rest_run1')
setwd('/net/hafkaldi/export/home/haf/pamela/fishvice/gadget-models/08-tusk/02-growth_rest_hafbjarmi_run1')
setwd('/net/hafkaldi/export/home/haf/pamela/fishvice/gadget-models/')
setwd('/net/hafkaldi/export/home/haf/pamela/fishvice/gadget-models/06-ling/02-growth_rest')


fit$resTable %>% View()
#fit <- gadget.fit(params.file = 'WGTS/params.matp.ins1', wgts = NULL)

fit<- gadget.fit()
fit$res.by.year %>% tail(.)
#tuskmat 2018. area1     5077989.     14016525.    13274362. 

gplot(fit)
plot(fit, data = "summary")

plot(fit,data='suitability')#should not work now - plot by hand
View(fit$suitability)

tmp <- plot(fit,data='catchdist.fleets')
tmp$ldist.igfs
tmp$ldist.lln
tmp$ldist.gil
tmp$ldist.bmt
tmp$ldist.comm 
tmp$ldist.foreign 

fit$stock.std %>% 
  filter(age %in% c(4,5,6,7,8,9,10,11,12,13,14,15)) %>%
  ggplot(aes(year,mean.length, 
             color = as.character(age), 
             linetype = stock, 
             size = as.character(step))) + geom_line() 


lik.out <- read.gadget.lik.out('WGTS/lik.final')



fit$stock.std
plot(fit,data='stock.std')


plot(fit,data='res.by.year',type='total')
plot(fit,data='res.by.year',type='F')
plot(fit,data='res.by.year',type='rec')


fit$stockdist %>%
  filter(stock=='lingmat',name == 'matp.igfs') %>% 
  ggplot(aes(length,obs.ratio, color = as.character(step))) + geom_line() + 
  geom_point(aes(y=pred.ratio))+
  facet_wrap(~year) + theme_minimal() + 
  labs(y='Prop. mature',x='Length')
#maturity
1/(1-1/(1+exp((-21)*(1 - 1.9))))
1/(1-1/(1+exp((-0.001*200)*(60 - 75))))
#suitability
1/(1+exp(-0.001*10000*(0.5-1)))

SS <- read.gadget.lik.out('WGTS/lik.final')
fit$catchdist.fleets %>% 
  left_join(SS$weights %>% rename(name=Component)) %>% 
  filter(!grepl('aldist',name)) %>% 
  ggplot(aes(year+(step-1)/4,avg.length,size=abs((observed-predicted)*sqrt(Weight)), 
             col=as.factor(sign((observed-predicted))))) + 
  geom_point() + facet_wrap(~name)  + 
  theme_light() + scale_color_manual(values=c('darkblue','red','black')) + 
  scale_size_area() + theme(legend.position = 'none') + 
  labs(x='Year',y='Length')



#not updated here down

#Stock-Recruitment

fit$res.by.year %>% 
  filter(stock=='shmat') %>%
  ungroup() %>% 
  select(year, SSB = total.biomass) %>% 
  left_join(fit$res.by.year %>% 
              filter(stock=='shimm') %>% 
              ungroup() %>% 
              select(year, recruitment)) %>% 
  filter(year > 1987) %>% 
  mutate(rec_l1 = lead(recruitment)/100000, SSB = SSB/1000) -> SR.dat

SR.dat %>% 
  ggplot(aes(SSB, rec_l1,
             color = as.numeric(year))) + geom_point()

Ricker_NLL <- function(pars, dat = SR.dat){
  a<-pars[1]; b<-pars[2]; sigma<-pars[3]
  pred <- a*dat$SSB*exp(-b*dat$SSB)
  logres <- log(dat$rec_l1/pred)
  NLL <- -sum(log(dnorm(logres, sigma)), na.rm = T)
  return(NLL)
    }
Ricker_NLL(c(28, 0.0007, 2))

#Whelk.nls<-nls(II ~ ProductionFunction(rr, KK, qq, PP0, II, CC), data = whelk.data, start = c(rr = 0.25, KK = 9000, qq = 0.01, PP0 = 15000))

Rick.opt <- optim(c(28, 0.0007, 2), Ricker_NLL, method = "L-BFGS-B", lower = c(5, 0.00001, 0.1), upper = c(60, 0.005, 10))
Rick.opt

Ricker_NLL(Rick.opt$par)

Ricker <- function(pars, dat = SR.dat){
  a<-pars[1]; b<-pars[2]; sigma<-pars[3]
  pred <- a*dat$SSB*exp(-b*dat$SSB)
  logres <- log(dat$rec_l1/pred)
  return(data.frame(dat,pred))
}


pred.dat<-Ricker(c(10000,Rick.opt$par[2:3]))
SSB_ser<-0:6000
pred.dat<-Ricker(Rick.opt$par, dat = data.frame(SSB = SSB_ser, rec_l1 = rep(1000, length(SSB_ser))))
#pred.dat<-Ricker(c(Rick.opt$par[1]*5, Rick.opt$par[2]*2, Rick.opt$par[3]), dat = data.frame(SSB = SSB_ser, rec_l1 = rep(1000, length(SSB_ser))))


plot(SR.dat$rec_l1~SR.dat$SSB, bty = 'l', xlim = c(0, 6000), ylim = c(0, 50000), ylab = '', xlab = '',main = 'Arnarfjörður', pch = 16, col = c(rep(1, 17), rep(2, 12)), xaxt = 'n', yaxt = 'n')
axis(1, at = seq(0,6000, 1000), labels = seq(0,6000, 1000)/1000)
axis(2, at = seq(0,50000,10000), labels = seq(0,50000,10000)/10)
mtext('Recruitment
(No. millions)', 2,line = 2)
mtext('SSB ("þús. tonnes")', 1, 2)
lines(pred.dat$pred~pred.dat$SSB)
lines(pred.dat$pred~pred.dat$SSB)
legend('topright', pch = 16, col = c(1,2), legend = c('year < 2005', 'year => 2005'), bty = 'n')


barplot(fit$res.by.year$catch/1000, names.arg = fit$res.by.year$year, ylab = 'Catch (tonnes)')


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

dirlist<-list('_I1.R1', '_I1.R2', '_I1.R3', '_I2.R1', '_I2.R2', '_I2.R3', '_I3.R1', '_I3.R2', '_I3.R3')
biomlist<-NULL
Flist<-NULL
reclist<-NULL
fitlist <- NULL
for(i in 1:9){
  setwd(paste0('/net/hafkaldi/export/home/haf/pamela/gadget-models/41-shrimp', dirlist[[i]], '/01-firsttry'))
    fit<- gadget.fit()
    fitlist[[i]] <- fit
    biomlist[[i]] <- plot(fit,data='res.by.year',type='total') + ggtitle(dirlist[[i]]) + scale_x_continuous(limits = c(1990, 2017)) + scale_y_continuous(limits = c(0, 40))
    Flist[[i]] <- plot(fit,data='res.by.year',type='F') + ggtitle(dirlist[[i]])
    reclist[[i]]  <- plot(fit,data='res.by.year',type='rec') + ggtitle(dirlist[[i]])
    closeAllConnections()
    print(paste0('Done ', dirlist[[i]]))
    }

multiplot(plotlist = biomlist, cols = 3)
multiplot(plotlist = Flist, cols = 3)
multiplot(plotlist = reclist, cols = 3)


names(fitlist) <- dirlist

#all_fits<-bind.gadget.fit(fitlist)


all_fits<-bind.gadget.fit(fitlist[[1]],
                fitlist[[2]],
                fitlist[[3]],
                fitlist[[4]],
                fitlist[[5]],
                fitlist[[6]],
                fitlist[[7]],
                fitlist[[8]],
                fitlist[[9]])

plot(all_fits,data='res.by.year',type='total') + 
  facet_wrap(~model,scales = 'free_y')
