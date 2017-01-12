library(Rgadget)
mclapply(1:100,
         function(x){
           tmp <- gadget.iterative(rew.sI=TRUE,
                                   main=sprintf('BS.WGTS/BS.%s/main',x),
                                   grouping=list(sind=c('si.20-50','si.50-70','si.70-180')),
                                   wgts=sprintf('BS.WGTS/BS.%s/WGTS',x))
         })
         
         