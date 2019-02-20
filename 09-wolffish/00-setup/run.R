library(Rgadget)

print('Begin  Iterative')
timestamp()

setwd('/net/hafkaldi.hafro.is/export/home/haf/pamela/gadget-models/09-wolffish/01-firsttry')


tmp <- gadget.iterative(rew.sI=TRUE,
                        main='main',
                        grouping=list(sind1.1=c('si.05-19','si.20-29'),
                                      sind1.2=c('si.30-55','si.56-74','si.75-110'),
                                      sind2=c('si.05-19.aut','si.20-29.aut','si.30-55.aut','si.56-74.aut','si.75-110.aut'),
                                      #sind2.2=c(),
                                      comm=c('ldist.gil','ldist.bmt','ldist.lln',
                                             #'aldist.gil',
                                             'aldist.bmt','aldist.lln')),
                        #cv.floor = 0.05,
                        params.file = 'params.init',
                        wgts='WGTS')
print('End Iterative')
timestamp()

#print('Running analytical retro')

#try(gadget.retro(mainfile = 'WGTS/main.final',params.file = 'params.init'), silent = T)
