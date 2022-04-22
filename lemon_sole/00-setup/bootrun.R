setwd('~/')



library(Rgadget)
base_dir <- 'lemon gadget'
vers <- c('45-base')
gd <- gadget.variant.dir(sprintf(paste0("%s/",vers),base_dir))

#setwd('../../..')
old <- getwd()
setwd(paste0('gadget project/',gd))

range<- 1
# range<- 1:50
# range <- 51:100
#range <- c(75,78)
run_bs <- 
  function(x){
    
        
        tmp <- gadget.iterative(rew.sI=TRUE,
                                main=sprintf('BS.WGTS/BS.%s/main', x),
                                grouping=list(si.igfs=c('si.10-20.igfs','si.20-25.igfs','si.25-30.igfs','si.30-35.igfs','si.35-40.igfs','si.40-45.igfs','si.45-60.igfs'),
                                  #'si.45-50.igfs','si.50-60.igfs'),
                                  #'si.45-50.igfs'),
                                  #si.aut=c('si.10-25.aut','si.25-30.aut','si.30-35.aut','si.35-40.aut','si.40-45.aut','si.45-60.aut')
                                  #'si.45-50.aut','si.50-60.aut')),
                                  #'si.45-50.aut')
                                  ),
                                #comp = c('aldist.igfs', 'ldist.igfs', 'matp.igfs'),
                                #inverse = TRUE,
                                #cv.floor = 0.05,
                                run.serial = TRUE,
                                wgts=sprintf('BS.WGTS/BS.%s/WGTS', x),
                                params.file = 'params.init')
      
 
  }

run_bs2 <-
  function(x){
    
    if(file.exists(sprintf('BS.WGTS/BS.%s/WGTS/main.final',x))){
      fit <- gadget.fit(main.file = sprintf('BS.WGTS/BS.%s/WGTS/main.final',x),
                        wgts = sprintf('BS.WGTS/BS.%s/WGTS',x),
                        fit.folder = sprintf('BS.WGTS/BS.%s/FIT',x),
                        recruitment_step_age	= tibble(stock = 'lemimm', age = 1, step = 1))
                         
    } else {
      print(x)
    }
    
  }

parallel::mclapply(range,
         run_bs,
         mc.cores = parallel::detectCores(logical = TRUE))

parallel::mclapply(1:100,
                   run_bs2,
                   mc.cores = parallel::detectCores(logical = TRUE))

#from report 

tmp <-
  list.dirs('BS.WGTS',recursive = FALSE) %>%
  purrr::map(function(x){
    if(file.exists(sprintf('%s/WGTS/WGTS.Rdata',x))){
      load(sprintf('%s/WGTS/WGTS.Rdata',x))
      out
    } else {
      print(x)
    }
  }) %>% 
  keep(function(x) length(x) > 1)
bootfit <- 
  list.dirs('BS.WGTS',recursive = FALSE) %>% 
  purrr::map(function(x){
    if(file.exists(sprintf('%s/WGTS/WGTS.Rdata',x))){
      load(sprintf('%s/WGTS/WGTS.Rdata',x))
      out
    } else {
      print(x)
    }
  }) %>% 
  keep(function(x) length(x) > 1) %>% 
  purrr::transpose() %>% 
  purrr::map(~purrr::map_if(.,is.null,data.frame)) %>% 
  purrr::map(~purrr::map_if(.,is.factor,as.character)) %>% 
  purrr::map(purrr::safely(~dplyr::bind_rows(.,.id='model') %>% mutate(model = as.numeric(model)))) %>% 
  purrr::map('result') 
class(bootfit) <- c('gadget.fit',class(bootfit))

bootfit$likelihoodsummary <- tmp %>% purrr::map('likelihoodsummary') %>% 
  purrr::map(. %>% mutate(area=1)) %>% bind_rows(.id='model') 

save(bootfit,file='bootfit.Rdata')  
