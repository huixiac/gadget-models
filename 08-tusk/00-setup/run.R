library(Rgadget)
tmp <- gadget.iterative(rew.sI=TRUE,
                        main='main',
                        grouping=list(sind=c('si.10-20','si.20-30','si.30-40','si.40-50','si.50-60',
                                             'si.60-70','si.70-110')),
                        #cv.floor = 0.05,
                        wgts='WGTS',
                        params.file = 'params.init')



print('Running analytical retro')

try(gadget.retro(mainfile = 'WGTS/main.final',params.file = 'params.init',iterative = TRUE,
             rew.sI=TRUE,
             grouping=list(sind=c('si.10-20','si.20-30','si.30-40','si.40-50','si.50-60',
                                  'si.60-70','si.70-110'))), silent = T)
if(FALSE){
R1 <- gadget.fit(wgts = 'RETRO/WGTS.1',main.file = 'RETRO/WGTS.1/main.final')
R2 <- gadget.fit(wgts = 'RETRO/WGTS.2',main.file = 'RETRO/WGTS.2/main.final')
R3 <- gadget.fit(wgts = 'RETRO/WGTS.3',main.file = 'RETRO/WGTS.3/main.final')
R4 <- gadget.fit(wgts = 'RETRO/WGTS.4',main.file = 'RETRO/WGTS.4/main.final')
R5 <- gadget.fit(wgts = 'RETRO/WGTS.5',main.file = 'RETRO/WGTS.5/main.final')

retro.fit <- bind.gadget.fit(R1,R2,R3,R4,R5)
save(retro.fit,file='retroFit.Rdata')
}