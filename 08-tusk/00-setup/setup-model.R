## Useful constansts

## weight length relationship
lw.constants <- 
  mfdb_dplyr_sample(mdb) %>% 
  filter(species == defaults$species,
         sampling_type == 'IGFS',
         !is.na(weight)) %>% 
  select(length,weight) %>% 
  collect(n=Inf) %>% 
  lm(log(weight/1e3)~log(length),.) %>% 
  broom::tidy() %>% 
  select(estimate)
## transport back to right dimension
lw.constants$estimate[1] <- exp(lw.constants$estimate[1])

## initial conditions sigma
init.sigma <- 
  mfdb_dplyr_sample(mdb) %>% 
  dplyr::filter(species == defaults$species,age >0,!is.na(length))  %>% 
  dplyr::select(age,length) %>% 
  dplyr::collect(n=Inf) %>% 
  dplyr::group_by(age) %>% 
  dplyr::summarise(n=n(),ml=mean(length,na.rm=TRUE),ms=sd(length,na.rm=TRUE)) %>% 
  dplyr::mutate(ms=ifelse(age>14,7.5,ms))

## initial guess for the maturity ogive:
mat.l50 <- 
  mfdb_dplyr_sample(mdb) %>% 
  filter(species == defaults$species,
         sampling_type == 'IGFS',
         !is.na(maturity_stage)) %>% 
  select(length,maturity_stage) %>% 
  group_by(length,maturity_stage) %>% 
  dplyr::summarise(n=n()) %>% 
  group_by(length) %>% 
  dplyr::mutate(p=n/sum(n)) %>% 
  ungroup() %>% 
  filter(maturity_stage=='2',p>0.50,length>25) %>% 
  dplyr::summarise(l50=min(length)) %>% 
  collect(n=Inf)

## investigate
# mfdb_dplyr_sample(mdb) %>%
#   filter(species == defaults$species,
#          sampling_type == 'IGFS',
#          !is.na(maturity_stage),
#          year > 2003) %>%
#   select(age,maturity_stage) %>%
#   group_by(age,maturity_stage) %>%
#   dplyr::summarise(n=n()) %>%
#   group_by(age) %>%
#   dplyr::mutate(p=n/sum(n)) %>%
#   dplyr::filter(maturity_stage=='2') %>%
#   collect(n=Inf) %>%
#   ggplot(aes(age,p)) + geom_point()
# 
# mfdb_dplyr_sample(mdb) %>%
#   filter(species == defaults$species,
#          sampling_type == 'IGFS',
#          !is.na(maturity_stage),
#          year > 2003,age!=0) %>%
#   select(length,age) %>%
#   collect(n=Inf) %>%
#   ggplot(aes(age,length,group=round(age))) + geom_boxplot()


## setup the immature stock first
tusk.imm <- 
  gadgetstock(sprintf('%simm',species_name),gd$dir,missingOkay = TRUE) %>%
  gadget_update('stock',
                minage = 1,
                maxage = 15,
                minlength = 4,
                maxlength = 110,
                dl = 2,
                livesonareas = 1) %>%
  gadget_update('doesgrow', ## note to self the order of these parameters make difference
                growthparameters=c(linf=sprintf('#%s.Linf',species_name), 
                                   k=to.gadget.formulae(quote(0.001*tusk.k)),
                                   alpha = sprintf('#%s.walpha',.[[1]]$stockname),
                                   beta = sprintf('#%s.wbeta',.[[1]]$stockname)),
                beta = to.gadget.formulae(quote(10*tusk.bbin))) %>% 
  gadget_update('initialconditions',
                normalparam = data_frame(age = .[[1]]$minage:.[[1]]$maxage,
                                         area = 1,
                                         age.factor = parse(text=sprintf('exp(-1*(%2$s.M+%3$s.init.F)*%1$s)*%2$s.init.%1$s',age,
                                                                         .[[1]]$stockname,species_name)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),   
                                         area.factor = sprintf('#%s.init.scalar',.[[1]]$stockname),
                                         mean = von_b_formula(age,linf=sprintf('%s.Linf',species_name),
                                                                              k=sprintf('%s.k',species_name),
                                                                              recl=sprintf('%s.recl',species_name)),
                                         stddev = init.sigma$ms[age],
                                         alpha = sprintf('#%s.walpha',.[[1]]$stockname),
                                         beta = sprintf('#%s.wbeta',.[[1]]$stockname))) %>% 
  ## does"something" updates should also allow for other names, e.g. doesrenew -> recruitment etc..
  gadget_update('refweight',
                data=data_frame(length=seq(.[[1]]$minlength,.[[1]]$maxlength,.[[1]]$dl),
                                mean=lw.constants$estimate[1]*length^lw.constants$estimate[2])) %>% 
  gadget_update('iseaten',1) %>% 
  gadget_update('doesmature', 
                maturityfunction = 'continuous',
                maturestocksandratios = sprintf('%smat 1',species_name),
                coefficients = sprintf('( * 0.001 #%1$s.mat1) #%1$s.mat2 0 0',species_name)) %>% 
  gadget_update('doesmove',
                transitionstocksandratios = sprintf('%smat 1',species_name),
                transitionstep = 4) %>% 
  gadget_update('doesrenew',
                normalparam = data_frame(year = year_range,
                                         step = 1,
                                         area = 1,
                                         age = .[[1]]$minage,
                                         number = parse(text=sprintf('%1$s.rec.scalar*%1$s.rec.%2$s',species_name,year)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),
                                         mean = von_b_formula(age,linf=sprintf('%s.Linf',species_name),
                                                              k=sprintf('%s.k',species_name),
                                                              recl=sprintf('%s.recl',species_name)),
                                         stddev = sprintf('#%s.rec.sd',species_name),
                                         alpha = sprintf('#%s.walpha',.[[1]]$stockname),
                                         beta = sprintf('#%s.wbeta',.[[1]]$stockname)))




tusk.mat <-
  gadgetstock(sprintf('%smat',species_name),
              gd$dir,missingOkay = TRUE) %>%
  gadget_update('stock',
                minage = 6,
                maxage = 18,
                minlength = 20,
                maxlength = 110,
                dl = 2,
                livesonareas = 1) %>%
  gadget_update('doesgrow', ## note to self the order of these parameters make difference
                growthparameters=c(linf=sprintf('#%s.Linf',species_name), 
                                   k=to.gadget.formulae(quote(0.001*tusk.k)),
                                   alpha = sprintf('#%s.walpha',.[[1]]$stockname),
                                   beta = sprintf('#%s.wbeta',.[[1]]$stockname)),
                beta = to.gadget.formulae(quote(10*tusk.bbin))) %>% 
  gadget_update('initialconditions',
                normalparam = data_frame(age = .[[1]]$minage:.[[1]]$maxage,
                                         area = 1,
                                         age.factor = parse(text=sprintf('exp(-1*(%2$s.M+%3$s.init.F)*%1$s)*%2$s.init.%1$s',age,
                                                                         .[[1]]$stockname,species_name)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),   
                                         area.factor = sprintf('#%s.init.scalar',.[[1]]$stockname),
                                         mean = von_b_formula(age,linf=sprintf('%s.Linf',species_name),
                                                              k=sprintf('%s.k',species_name),
                                                              recl=sprintf('%s.recl',species_name)),
                                         stddev = init.sigma$ms[age],
                                         alpha = sprintf('#%s.walpha',.[[1]]$stockname),
                                         beta = sprintf('#%s.wbeta',.[[1]]$stockname))) %>% 
  ## does"something" updates should also allow for other names, e.g. doesrenew -> recruitment etc..
  gadget_update('refweight',
                data=data_frame(length=seq(.[[1]]$minlength,.[[1]]$maxlength,.[[1]]$dl),
                                mean=lw.constants$estimate[1]*length^lw.constants$estimate[2])) %>% 
  gadget_update('iseaten',1) 


## write to file
tusk.imm %>% 
  write.gadget.file(gd$dir)

tusk.mat %>% 
  write.gadget.file(gd$dir)



Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd$dir))
callGadget(s=1,log = 'init.log') #ignore.stderr = FALSE,

## update the input parameters with sane initial guesses
read.gadget.parameters(sprintf('%s/params.out',gd$dir)) %>% 
  init_guess('rec.[0-9]|init.[0-9]',1,0.001,100,1) %>%
  init_guess('recl',12,4,20,1) %>% 
  init_guess('rec.sd',5, 4, 20,1) %>% 
  init_guess('Linf',110, 100, 160,1) %>% 
  init_guess('k$',90, 40, 100,1) %>% 
  init_guess('bbin',6, 1e-08, 100, 1) %>% 
  init_guess('alpha', 0.5,  0.01, 3, 1) %>% 
  init_guess('l50',50,10,100,1) %>% 
  init_guess('walpha',lw.constants$estimate[1], 1e-10, 1,0) %>% 
  init_guess('wbeta',lw.constants$estimate[2], 2, 4,0) %>% 
  init_guess('M$',0.15,0.001,1,0) %>% 
  init_guess('rec.scalar',400,1,500,1) %>% 
  init_guess('init.scalar',200,1,300,1) %>% 
  init_guess('mat2',mat.l50$l50,0.75*mat.l50$l50,1.25*mat.l50$l50,1) %>% 
  init_guess('mat1',70,  10, 200, 1) %>% 
  init_guess('init.F',0.4,0.1,1,1) %>% 
  write.gadget.parameters(.,file=sprintf('%s/params.in',gd$dir))
