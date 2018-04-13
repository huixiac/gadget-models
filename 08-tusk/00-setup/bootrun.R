library(Rgadget)
mclapply(1:100,
         function(x){
           tmp <- gadget.iterative(rew.sI=TRUE,
                                   main=sprintf('BS.WGTS/BS.%s/main',x),
                                   grouping=list(sind=c('si.10-20','si.20-30','si.30-40','si.40-50','si.50-60',
                                                        'si.60-70','si.70-110')),
                                   wgts=sprintf('BS.WGTS/BS.%s/WGTS',x),
                                   run.serial = TRUE)
           fit <- gadget.fit(main.file = sprintf('BS.WGTS/BS.%s/main',x),
                             wgts = sprintf('BS.WGTS/BS.%s/WGTS',x))
         })