library(Rgadget)
library(mfdb)


## input parameters

path <- '08-tusk/03-fixes/'
mainfile <- 'main'
num.years <- 300
num.iter <- 10
params.file <- 'WGTS/params.final'
pre <- 'PRE'
imm.stock <- 'tuskimm'
mat.stock <- 'tuskmat'
options(stringsAsFactors = FALSE)
block.size <- 5
rec.step <- 1
ref.fleet <- "comm"
harvest.rate <- seq(0,1,by=0.01)
asserr.cv <- 0.2
asserr.rho <- 0.8
ref.cm <- 40

## read function
readoutput <- function(x){
  tmp <- readLines(x)
  preamble <- tmp[grepl(';',tmp)]
  body <- tmp[!grepl(';',tmp)]
  header <- preamble[grepl('year-step-area',preamble)] %>% 
    gsub('; (*)','\\1',.) %>% 
    str_split('-') %>% 
    unlist() %>% 
    gsub(' ','_',.)
  body %>% 
    paste(collapse='\n') %>% 
    read.table(text=.,
               col.names = header) %>% 
    dplyr::mutate(trial=cut(1:length(year),c(0,which(diff(year)<0),1e9),labels = FALSE)-1) %>% 
    tibble::as_tibble() 
}

# retro.fit$res.by.year %>% 
#   left_join(fit$res.by.year %>% 
#               select(year,h0=harv.biomass )) %>% 
#   group_by(year,model) %>% 
#   dplyr::summarise(harv.biomass=sum(harv.biomass),h0=sum(h0)) %>% 
#   ungroup() %>% mutate(dh=(harv.biomass-h0)/h0,d0=year+as.numeric(model)) %>% 
#   filter(d0==2016) %>% select(year,model,dh) %>%select(dh) %>% unlist() %>% cor(.,lag(.),use='complete.obs')

pre.fleets <- data_frame(fleet_name = c('igfs','comm'))


ref.points <- 
  data_frame(switch = c('tuskmat.btrigger','tuskmat.blim','comm.hr.low','comm.hr.high','fleet.prop.predict.igfs','fleet.prop.predict.comm'),
             value = c(0,3.53e6,0,0.2,0,1),
             lower = 0.5*value,
             upper = pmax(1.5*value,1),
             optimise = 0)
  


# append_recruitment <- function(st){
#   recfile <- grep('file|number',st$doesrenew %>% names(),value = TRUE)
#   comp <- st$doesrenew[[recfile]][[1]]
#   for (i in seq_len(ncol(comp))) {
#     if (length(comp[,i]) == 0) {
#       # Nothing to convert
#       next
#     } else if (is.call(comp[1, i][[1]])) {
#       # Convert formulae column to character
#       comp[, i] <- vapply(comp[, i], to.gadget.formulae, "")
#     }
#   }
#   if(is.null(ref.year)){  
#     pre.rec <-
#       comp %>% 
#       dplyr::filter(year == max(year))
#   } else {
#     pre.rec <- 
#       comp %>% 
#       dplyr::filter(year == ref.year)
#   }
#   pre.rec %>% 
#     mutate(year=)
#   
#   st %>% 
#     gadget_update
# }

## Prediction fleets are specified as a data.frame
## these borrow selection functions from the model fleets
## if base_fleet is not defined it is assumed that the fleet_name column
## is the base_fleet. Also if not specified the fleet will be a totalfleet



pre.fleets$base_fleet <- pre.fleets$base_fleet%||%pre.fleets$fleet_name
pre.fleets$fleet_type <- pre.fleets$fleet_type%||%'totalfleet'

params <- read.gadget.parameters(file=sprintf('%s/%s',path,params.file))

## Read in the model:
run.dir <- gadget.variant.dir(path = path,variant_dir = pre)

main <- read.gadget.file(path,mainfile,file_type = 'main')
stocks <- 
  main$stock$stockfiles %>% 
  purrr::map(~gadgetstock(.,path)) %>% 
  set_names(.,map(.,~.[[1]]$stockname) %>% unlist())

## collect the model fleets
fleets <-
  main$fleet$fleetfiles %>% 
  purrr::map(~gadgetfleet(.,path))

## check if the projection fleets are defined 
tmp <- 
  fleets %>% 
  purrr::flatten() %>% 
  purrr::map(~.[[1]]) %>% 
  unlist() %>% 
  setdiff(pre.fleets$base_fleet,.) %>% 
  map(~stop(sprintf('Projection fleet %s not found',.)))
  



## set the projection period
class(main[[1]]$timefile) <- c('gadgettime',class(main[[1]]$timefile))
attr(main[[1]]$timefile,'file_config')$file_type <- 'time'
attr(main[[1]]$timefile,'file_config')$mainfile_section <- 'timefile'

main[[1]]$timefile %>%  
  gadget_update(lastyear = .[[1]]$lastyear + num.years) %>% 
  write.gadget.file(run.dir)


## create a timing schedule for the fleet operations
schedule <- 
  expand.grid(year = seq(main[[1]]$timefile[[1]]$lastyear,
                         by=1,
                         length.out = num.years+1),
              step = 1:main[[1]]$timefile[[1]]$notimesteps[1],
              area = main[[1]]$areafile[[1]]$areas) %>% 
  dplyr::filter(!(year==main[[1]]$timefile[[1]]$lastyear & 
                    step <= main[[1]]$timefile[[1]]$laststep),
                !(year==max(year) & step > main[[1]]$timefile[[1]]$laststep)) %>% 
  dplyr::arrange(year,step,area)

## update the area file to include temperature data in the projection
attr(main[[1]]$areafile,'file_config')$file_type <- 'area'
attr(main[[1]]$areafile,'file_config')$mainfile_section <- 'areafile'


main[[1]]$areafile[[2]] <- 
  main[[1]]$areafile[[2]] %>% 
  bind_rows(schedule %>% 
              mutate(mean=3))   ## this bit could be user defined

main[[1]]$areafile %>% 
  write.gadget.file(run.dir) 


## setup hockey stick spawning function
attr(stocks[[mat.stock]],'file_config')$mainfile_overwrite = TRUE
stocks[[mat.stock]] %>% 
  gadget_update('doesspawn',
                spawnfile = gadgetfile('hockeyspawnfile',
                                       component=list(list(spawnsteps = stocks[[imm.stock]]$doesrenew$normalparamfile[[1]]$step %>% unique(), 
                                                           spawnareas = .[[1]]$livesonareas,
                                                           firstspawnyear = min(schedule$year),
                                                           lastspawnyear = max(schedule$year),
                                                           spawnstocksandratios = paste('hockeystock',1),
                                                           proportionfunction = 'constant 1',
                                                           mortalityfunction = 'constant 0',
                                                           weightlossfunction = 'constant 0',
                                                           recruitment = 'hockeystick',
                                                           gadgetfile('hockeyrec',
                                                                      file_type = 'timevariable',
                                                                      components=list(list('hockeyrec',
                                                                                           data = 
                                                                                             tibble::data_frame(year = main[[1]]$timefile[[1]]$firstyear,
                                                                                                             step = main[[1]]$timefile[[1]]$firststep,
                                                                                                             value = "0") %>% 
                                                                                             dplyr::bind_rows(expand.grid(year = unique(schedule$year),
                                                                                                                          step = stocks[[imm.stock]]$doesrenew$normalparamfile[[1]]$step %>% unique()) %>% 
                                                                                                         dplyr::mutate(value = paste0('#',imm.stock,'.rec.',year),
                                                                                                                       year = year - stocks[[imm.stock]][[1]]$minage)) %>% 
                                                                                                         dplyr::select(year,step,value) %>%
                                                                                             as.data.frame()))),
                                                           sprintf('#%s.blim',mat.stock),
                                                           stockparameters =  stocks[[imm.stock]]$doesrenew$normalparamfile[[1]] %>% ## what about other types of recruitment files?
                                                             tail(1) %>% 
                                                             select(mean:beta)%>% 
                                                             unlist() %>% 
                                                             map(to.gadget.formulae) %>% 
                                                             unlist()
                                       )))) %>% 
    write.gadget.file(run.dir)

stocks[[imm.stock]] %>% 
  write.gadget.file(run.dir)


## setup hockeystick recruitment

gadgetstock('hockeystock',run.dir,missingOkay = TRUE) %>% 
  gadget_update('refweight',data=stocks[[imm.stock]][[1]]$refweightfile[[1]]) %>% 
  gadget_update('stock',
                minage=0,
                maxage=stocks[imm.stock][[1]][[1]]$minage,
                minlength = stocks[imm.stock][[1]][[1]]$minlength,
                maxlength = stocks[imm.stock][[1]][[1]]$maxlength,
                dl = stocks[imm.stock][[1]][[1]]$dl,
                livesonareas = stocks[imm.stock][[1]][[1]]$livesonareas) %>%
  gadget_update('naturalmortality',rep(0,.[[1]]$maxage+1)) %>% 
  gadget_update('doesgrow',0) %>% 
  ## to get multiannual recruitment you will need to define multiple hockeystocks or use straying (some other day)
  gadget_update('doesmove',
                transitionstocksandratios = sprintf('%s 1',imm.stock),
                transitionstep = stocks[[imm.stock]]$doesrenew$normalparamfile[[1]]$step %>% unique() %>% head(1)) %>% 
  gadget_update('initialconditions',
                normalparam = data_frame(age = .[[1]]$minage:.[[1]]$maxage,
                                         area = .[[1]]$livesonareas,
                                         age.factor = 0,   
                                         area.factor = 0,
                                         mean = .[[1]]$minlength,
                                         stddev = 1,
                                         alpha = 0.00001, ## never used 
                                         beta = 3)) %>% 
  write.gadget.file(run.dir)


suits <- 
  ## can define multiple fleets with the same selection
  setNames(pre.fleets$base_fleet%||%pre.fleets$fleet_name, 
           pre.fleets$fleet_name) %>% 
  map(function(x){
    fleets %>% 
      purrr::map(~purrr::keep(.,~.[[1]]==x)) %>% 
      purrr::flatten() %>% 
      purrr::map(c('suitability')) %>%
      purrr::flatten() 
  }) %>% 
  purrr::map(~bind_rows(.)) %>% 
  dplyr::bind_rows(.id='fleet') %>% 
  tidyr::gather(stock,val,-c(1)) %>% 
  dplyr::group_by(fleet,stock) %>% 
  dplyr::mutate(n=paste0('n',1:n())) %>% 
  tidyr::spread(n,val,fill= '') %>% 
  dplyr::ungroup()


## define fleet amounts that are parametrised by year, step, area 
fleet.amount <- 
  pre.fleets$fleet_name %>% 
  setNames(.,.) %>% 
  purrr::map(function(x) 
    schedule %>% 
      dplyr::mutate(number= 1) %>% #paste('#fleet',x,year,step,area,sep='.')) %>% 
      structure(area_group = lift(mfdb_group)(main[[1]]$areafile[[1]]$areas %>% 
                                                setNames(.,.))))

## define the projection fleets
fleet.tmp <- 
  gadgetfleet('fleet.predict',path,missingOkay = TRUE) 

for(i in seq_along(pre.fleets$fleet_name)){
  fleet.tmp <- 
    fleet.tmp %>% 
    gadget_update(pre.fleets$fleet_type[i],
                  name = paste0('predict.',pre.fleets$fleet_name[i]),
                  suitability = 
                    suits %>% 
                    dplyr::filter(fleet == pre.fleets$fleet_name[i]) %>% 
                    tidyr::unite(col,-matches('fleet'),sep='\t') %>% 
                    dplyr::select(-fleet) %>% 
                    unlist() %>% 
                    paste(collapse="\n") %>% 
                    sprintf('\n%s',.),
                  data = fleet.amount[[pre.fleets$fleet_name[i]]])
}

fleet.tmp %>% 
  write.gadget.file(run.dir)


## Define the prognosis likelihood
pre.fleet.names <- 
  fleet.tmp %>% 
  map(~.[[1]]) %>% 
  unlist() %>% 
  as.character()

## assumes expL50 as that is the only thing allowed
biocoeffs <- 
  suits %>% 
  dplyr::filter(fleet == ref.fleet,tolower(n2) == 'exponentiall50') %>%
  dplyr::select(-c(1:4)) %>% 
  tidyr::unite(col,everything(),sep='\t') %>% 
  dplyr::mutate(col=paste(col,1,sep='\t')) %>% 
  unlist() %>% 
  paste(collapse="\n")

progn <- 
  progn <- 
  gadgetfile('fleet.likelihood',
             file_type = 'likelihood',
             components = list(list('[component]',name = 'prognosis',
                                    weight = 1,
                                    type = "proglikelihood",
                                    fleetnames = pre.fleet.names,
                                    stocknames = c(imm.stock,mat.stock),
                                    biocoeffs = c(0,ref.cm,1,0,ref.cm,1),## biocoeffs, this doesn't work atm 
                                    ## only want the mature biomass as the trigger biomass
                                    triggercoeffs=c(0,0,0,0,0,1), 
                                    triggervalues=c(0, sprintf('#%s.btrigger',mat.stock)),
                                    harvestrates = sprintf(c('#%s.hr.low','#%s.hr.high'),ref.fleet),
                                    quotasteps = c(3, 4, 5, 6),
                                    ## assume equal fishing intensity for now
                                    quotaproportions = c(0.25, 0.25, 0.25, 0.25),
                                    fleetproportions= sprintf('#fleet.prop.%s',pre.fleet.names),
                                    weightoflastyearstac=0,
                                    maxchange=4,
                                    functionnumber=1,
                                    firsttacyear=min(schedule$year),
                                    assessmentstep=2,
                                    asserr=gadgetfile('asserr',
                                                      file_type = 'timevariable',
                                                      components=list(list('asserr',
                                                                           data = data_frame(year = main[[1]]$timefile[[1]]$firstyear,
                                                                                             step = main[[1]]$timefile[[1]]$firststep,
                                                                                             value = "0") %>% 
                                                                             bind_rows(expand.grid(year = unique(schedule$year),
                                                                                                   step = 2) %>% 
                                                                                         dplyr::mutate(value = paste0('#',mat.stock,'.asserr.',year))) %>% 
                                                                             as.data.frame())))))) 

attr(progn,'file_config')$mainfile_overwrite = TRUE
progn %>% 
  write.gadget.file(run.dir)

# 

## projections (stochastic)

rep.params <- function(params){ 
  expand.grid(iter=1:num.iter,harvest.rate=harvest.rate) %>% 
    dplyr::mutate(run.id=1:n()) %>% 
    split(.$run.id) %>% 
    parallel::mclapply(function(x){
      block.rec <-
        imm.stock %>% 
        purrr::set_names(.,.) %>% 
        purrr::map(~stocks[[.]]$doesrenew$normalparamfile[[1]] %>%
                     dplyr::filter(year < min(schedule$year)) %>% 
                     dplyr::mutate(number = number %>% 
                                     unlist() %>% 
                                     map(~eval(.,setNames( nm=params$switch,params$value) %>% 
                                                 as.list())) %>% 
                                     unlist())) %>%
        dplyr::bind_rows(.id='stock') %>%
        dplyr::group_by(stock) %>% 
        dplyr::mutate(block = round(1:n()/block.size)) %>% 
        dplyr::inner_join(data_frame(block=sample(round(.$block),size=ceiling(num.years/block.size)*2,replace=TRUE),
                                     order=1:ceiling(2*num.years/block.size)),
                          by='block') %>%
        dplyr::arrange(order) %>% 
        dplyr::slice(1:num.years) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(year=schedule$year %>% unique()) %>% 
        dplyr::transmute(switch=sprintf('%s.rec.%s',stock,year),
                         value = number,
                         upper=1.5*number,
                         lower=0.5*number,
                         optimise = 0)
      
      
      
      asserr <- 
        mat.stock %>% 
        purrr::set_names(.,.) %>%
        purrr::map(~schedule %>%  
                     dplyr::select(year) %>% 
                     dplyr::distinct() %>% 
                     dplyr::mutate(number = arima.sim(list(ar = asserr.rho), n = n(),
                                                      rand.gen = function(x) rnorm(x,sd=asserr.cv*sqrt(1-asserr.rho^2))) %>% 
                                     exp())) %>% 
        dplyr::bind_rows(.id='stock') %>% 
        dplyr::ungroup() %>% 
        dplyr::transmute(switch=sprintf('%s.asserr.%s',stock,year),
                         value = number,
                         upper=1.5*number,
                         lower=0.5*number,
                         optimise = 0)
      params %>% 
        bind_rows(asserr,block.rec,ref.points) %>% 
        dplyr::mutate(value = ifelse(grepl('hr.high',switch),x$harvest.rate,value))
    }, mc.cores = parallel::detectCores(logical = TRUE))
}

#params.pre <- rep.params(params)
params.pre <-
  bootfit$params %>% 
  split(.$model) %>% 
  map(~select(.,-model)) %>% 
  map(rep.params) %>% 
  bind_rows(.id='repl') %>% 
  tidyr::gather(model,col) %>% 
  tidyr::unnest(col)



## define the output, needs to be very compact
 
print <- 
  gadgetfile('pre.print',
             components = list(list('[component]',
                                    type = 'stockprinter',
                                    stocknames = mat.stock,
                                    areaaggfile = gadgetdata(sprintf('Aggfiles/%s.area.agg',mat.stock),
                                                             data = data.frame(name = sprintf('area%s',stocks[[mat.stock]][[1]]$livesonareas),
                                                                               value = stocks[[mat.stock]][[1]]$livesonareas)),
                                    ageaggfile = gadgetdata(sprintf('Aggfiles/%s.ssb.age.agg',mat.stock),
                                                            data = data.frame(name = 'allages',
                                                                              value = paste(stocks[[mat.stock]][[1]]$minage:stocks[[mat.stock]][[1]]$maxage,
                                                                                            collapse = ' '))),
                                    lenaggfile =   gadgetdata(paste0('Aggfiles/', stocks[[mat.stock]][[1]]$stockname, '.stock.len.agg'),
                                                              data = data.frame(name = 'alllen',
                                                                                value = paste(stocks[[mat.stock]][[1]]$minlength,stocks[[mat.stock]][[1]]$maxlength,
                                                                                              sep = ' '))),
                                    printfile = gadgetfile(sprintf('out/%s.ssb',mat.stock)),
                                    printatstart = 1,
                                    yearsandsteps = 'all 1'),
                               list('[component]',
                                    type = 'stockprinter',
                                    stocknames = imm.stock,
                                    areaaggfile = gadgetdata(sprintf('Aggfiles/%s.area.agg',imm.stock),
                                                             data = data.frame(name = sprintf('area%s',stocks[[imm.stock]][[1]]$livesonareas),
                                                                               value = stocks[[imm.stock]][[1]]$livesonareas)),
                                    ageaggfile = gadgetdata(sprintf('Aggfiles/%s.age.agg',imm.stock),
                                                            data = data.frame(name = stocks[[imm.stock]][[1]]$minage,
                                                                              value = stocks[[imm.stock]][[1]]$minage)),
                                    lenaggfile =   gadgetdata(paste0('Aggfiles/', stocks[[imm.stock]][[1]]$stockname, '.stock.len.agg'),
                                                              data = data.frame(name = 'alllen',
                                                                                value = paste(stocks[[imm.stock]][[1]]$minlength,stocks[[imm.stock]][[1]]$maxlength,
                                                                                              sep = ' '))),
                                    printfile = gadgetfile(sprintf('out/%s.rec',imm.stock)),
                                    printatstart = 1,
                                    yearsandsteps = 'all 2'),
                               list('[component]',
                                    type = 'predatorpreyprinter',
                                    predatornames = pre.fleet.names,
                                    preynames = c(imm.stock,mat.stock),
                                    areaaggfile = gadgetdata(sprintf('Aggfiles/%s.area.agg',mat.stock),
                                                             data = data.frame(name = sprintf('area%s',stocks[[mat.stock]][[1]]$livesonareas),
                                                                               value = stocks[[mat.stock]][[1]]$livesonareas)),
                                    ageaggfile = gadgetdata(sprintf('Aggfiles/%s.stock.allage.agg',mat.stock),
                                                            data = tibble::data_frame(value = paste(stocks[[imm.stock]][[1]]$minage:stocks[[mat.stock]][[1]]$maxage,
                                                                                                    collapse = ' '),
                                                                                            name = 'allages') %>% 
                                                              dplyr::select(name,value) %>% 
                                                              as.data.frame()),
                                    lenaggfile = gadgetdata(paste0('Aggfiles/', stocks[[mat.stock]][[1]]$stockname, '.stock.len.agg'),
                                                            data = data.frame(name = 'alllen',
                                                                              value = paste(stocks[[imm.stock]][[1]]$minlength,stocks[[mat.stock]][[1]]$maxlength,
                                                                                            sep = ' '))),
                                    printfile = gadgetfile('out/catch.lw'),
                                    yearsandsteps = 'all all'),
                               list('[component]',
                                    type = 'predatorpreyprinter',
                                    predatornames = pre.fleet.names,
                                    preynames = c(imm.stock,mat.stock),
                                    areaaggfile = gadgetdata(sprintf('Aggfiles/%s.area.agg',mat.stock),
                                                             data = data.frame(name = sprintf('area%s',stocks[[mat.stock]][[1]]$livesonareas),
                                                                               value = stocks[[mat.stock]][[1]]$livesonareas)),
                                    ageaggfile = gadgetdata(sprintf('Aggfiles/%s.stock.Fage.agg',mat.stock),
                                                            data = tibble::data_frame(value = stocks[[mat.stock]][[1]]$maxage,
                                                                                      name = 'allages') %>% 
                                                              dplyr::select(name,value) %>% 
                                                              as.data.frame()),
                                    lenaggfile = gadgetdata(paste0('Aggfiles/', stocks[[mat.stock]][[1]]$stockname, '.stock.len.agg'),
                                                            data = data.frame(name = 'alllen',
                                                                              value = paste(stocks[[imm.stock]][[1]]$minlength,stocks[[mat.stock]][[1]]$maxlength,
                                                                                            sep = ' '))),
                                    printfile = gadgetfile('out/catch.F'),
                                    yearsandsteps = 'all all')))
             
attr(print,'file_config')$mainfile_section <- 'printfiles'
#write.gadget.file(print,run.dir)

Sys.setenv(GADGET_WORKING_DIR=normalizePath(path))
run.func <- function(x){
  curr.dir <- sprintf('%s/run%s',tempdir(),attr(x,'run_id'))
  dir.create(curr.dir)
  dir.create(paste(curr.dir,'out',sep='/'))
  vd <- gadget.variant.dir(path,variant_dir = curr.dir,mainfile = 'main')
  vd[[1]] <- ''
  read.gadget.file(run.dir,'main',recursive = FALSE) %>% 
    write.gadget.file(vd,recursive=FALSE)
  print %>% write.gadget.file(vd)
  write.gadget.parameters(x,file=paste(curr.dir,'params.pre',sep='/'),columns = FALSE)
  
  callGadget(s=1,
             main=attr(vd,'mainfile'),
             i=paste(curr.dir,'params.pre',sep='/'),
             ignore.stderr = FALSE,
             gadget.exe = '~/bin/gadget')
  list.files(paste(curr.dir,'out',sep='/'),full.names = TRUE) %>%
    purrr::set_names(.,.) %>% 
    purrr::map(readoutput) 
}
tmp <- 
params.pre %>% 
  mutate(repl = ceiling(1:n()/690)) %>% 
  split(.$repl) %>% 
  purrr::map(~dplyr::select(.,switch,value) %>% 
               tidyr::spread(switch,value)) %>% 
  dplyr::bind_rows() %>% 
  split(.$comm.hr.high) 

tmp2 <- 
  harvest.rate %>% 
  purrr::set_names(as.character(.),.) %>% 
  purrr::map(function(x){ attr(tmp[[x]],'run_id')<-x;tmp[[x]]})
  
res <- mclapply(tmp2,run.func,mc.cores = detectCores(logical = TRUE))

reruns <- as.character(c(3,4,15,16,17,18,19,20,35,37,39,38,44,55,56,57,58,59,60)/100)
res.rerun <- mclapply(tmp2[reruns],run.func,mc.cores = detectCores(logical = TRUE))
for(i in seq_along(reruns)){
  res[[reruns[i]]] <- res.rerun[[i]]
}

res_adv <- 
  names(res) %>% 
  set_names(.,.) %>% 
  discard(~class(res[[.]])!='list')%>% 
  mclapply(.,function(x){
    mF <- 
      res[[x]][[1]] %>% 
      dplyr::filter(year>2060) %>% 
      dplyr::group_by(trial) %>% 
      dplyr::summarise(mF=mean(mortality))
    mLND <- 
      res[[x]][[2]] %>% 
      dplyr::filter(year > 2060) %>% 
      group_by(year,trial) %>% 
      dplyr::summarise(lnd=sum(biomass_consumed)/1e6) %>% 
      group_by(trial) %>% 
      dplyr::summarise(ML=mean(lnd))
    rec <- 
      res[[x]][[3]] %>% 
      dplyr::filter(year > 2060) %>% 
      group_by(trial) %>% 
      dplyr::summarise(rec=mean(number))
    mSSB <-     
      res[[x]][[4]] %>% 
      dplyr::filter(year > 2060) %>% 
      group_by(year,trial) %>% 
      dplyr::summarise(SSB=sum(number*mean_weight)/1e6) %>% 
      group_by(trial) %>% 
      dplyr::summarise(SSB=mean(SSB))
    mF %>% 
      left_join(mLND) %>% 
      left_join(rec) %>% 
      left_join(mSSB)
  },mc.cores = detectCores(logical = TRUE))
  

res2 <- 
  bind_rows(res_adv,.id='rate') %>% 
  filter(!(rate %in% c('0.43','0.42'))) %>% ## weird stuff happened here
  group_by(rate) %>% 
  dplyr::summarise(mssb=median(SSB),
                   ussb=quantile(SSB,0.975),
                   lssb=quantile(SSB,0.025),
                   mrec=median(rec),
                   urec=quantile(rec,0.975),
                   lrec=quantile(rec,0.025),
                   mlnd=median(ML),
                   ulnd=quantile(ML,0.975),
                   llnd=quantile(ML,0.025),
                   mF=median(mF),
                   uF=quantile(mF,0.975),
                   lF=quantile(mF,0.025))
res2 %>% ggplot(aes(as.numeric(rate),mlnd,group=1)) + geom_ribbon(aes(ymin=llnd,ymax=ulnd),fill='gold') + geom_line() 

