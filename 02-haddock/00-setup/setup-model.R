## Useful constansts

## weight length relationship
lw.constants <- 
  mfdb_dplyr_sample(mdb) %>% 
  filter(species == defaults$species,
         sampling_type == 'IGFS',
         !is.na(weight), 
         length >= 10,
         weight >= 8) %>% 
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
  dplyr::filter(species == defaults$species,age >= 0,!is.na(length)) %>% 
  dplyr::select(age,length) %>% 
  dplyr::collect(n=Inf) %>% 
  dplyr::group_by(age) %>% 
  dplyr::summarise(ml=mean(length,na.rm=TRUE),ms=sd(length,na.rm=TRUE))

  
  
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
  filter(maturity_stage=='2',p>0.50,length>10) %>% 
  dplyr::summarise(l50=min(length)) %>% 
  collect(n=Inf)


## setup the immature stock first

had.imm <- 
  gadgetstock('hadimm',gd$dir,missingOkay = TRUE) %>%
  gadget_update('stock',
                minage = 0,
                maxage = 10,
                minlength = 1,
                maxlength = 100,
                dl = 1,
                livesonareas = 1) %>%
  gadget_update('doesgrow', ## note to self the order of these parameters make difference
                growthparameters=c(linf='#had.Linf', 
                                   k='Modelfiles/timevariableK.imm',
                                   alpha = '#hadimm.walpha',
                                   beta = '#hadimm.wbeta'),
                beta = to.gadget.formulae(quote(10*hadimm.bbin)), 
                maxlengthgroupgrowth = 10) %>% 
  gadget_update('initialconditions',
                normalparam = data_frame(age = .[[1]]$minage:.[[1]]$maxage,
                                         area = 1,
                                         age.factor = parse(text=sprintf('exp(-1*(hadimm.M+had.init.F)*%1$s)*hadimm.init.%1$s',age)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),   
                                         area.factor = '#hadimm.init.scalar',
                                         mean = von_b_formula(age,linf='had.Linf',k='hadimm.k.1',recl='had.recl.1979'),
                                         stddev = init.sigma$ms[age+1],#modify indexing with age 0 fish
                                         alpha = '#hadimm.walpha',
                                         beta = '#hadimm.wbeta')) %>% 
  ## does"something" updates should also allow for other names, e.g. doesrenew -> recruitment etc..
  gadget_update('refweight',
                data=data_frame(length=seq(.[[1]]$minlength,.[[1]]$maxlength,.[[1]]$dl),
                                mean=lw.constants$estimate[1]*length^lw.constants$estimate[2])) %>% 
  gadget_update('iseaten',1) %>% 
  gadget_update('doesmature', 
                maturityfunction = 'continuous',
                maturestocksandratios = 'hadmat 1',
                coefficients = '( * 0.001 #had.mat1) #had.mat2 0 0') %>% 
  gadget_update('doesmove',
                transitionstocksandratios = 'hadmat 1',
                transitionstep = 6) %>% 
  gadget_update('doesrenew',
                normalparam = data_frame(year = year_range,
                                         step = 4, #changed this so new recruits should not be detected in spring survey
                                         area = 1,
                                         age = .[[1]]$minage,
                                         number = parse(text=sprintf('had.rec.scalar*exp(had.rec.%s)',year)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),
                                         mean = parse(text=sprintf('had.recl.%s',year)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(), 
                                           #von_b_formula(age,linf='had.Linf',k='had.k',recl='had.recl'),
                                         stddev = '#had.rec.sd',
                                         alpha = '#hadimm.walpha',
                                         beta = '#hadimm.wbeta')) 




had.mat <-
  gadgetstock('hadmat',gd$dir,missingOkay = TRUE) %>%
  gadget_update('stock',
                minage = 1,
                maxage = 15,
                minlength = 1,
                maxlength = 100,
                dl = 1,
                livesonareas = 1) %>%
  gadget_update('doesgrow', ## note to self the order of these parameters make difference
                growthparameters=c(linf='#had.Linf', 
                                   k='Modelfiles/timevariableK.mat',
                                   alpha = '#hadmat.walpha',
                                   beta = '#hadmat.wbeta'),
                beta = to.gadget.formulae(quote(10*hadmat.bbin)),
                maxlengthgroupgrowth = 10) %>% 
  
  gadget_update('initialconditions',
                normalparam = data_frame(age = .[[1]]$minage:.[[1]]$maxage,
                                         area = 1,
                                         age.factor = parse(text=sprintf('exp(-1*(hadmat.M+had.init.F)*%1$s)*hadmat.init.%1$s',age)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),
                                         area.factor = '#hadmat.init.scalar',
                                         mean = von_b_formula(age,linf='had.Linf',k='hadimm.k.1',recl='had.recl.1979'),
                                         stddev = init.sigma$ms[age],
                                         alpha = '#hadmat.walpha',
                                         beta = '#hadmat.wbeta')) %>% 
  ## does"something" updates should also allow for other names, e.g. doesrenew -> recruitment etc..
  gadget_update('refweight',
                data=data_frame(length=seq(.[[1]]$minlength,.[[1]]$maxlength,.[[1]]$dl),
                                mean=lw.constants$estimate[1]*length^lw.constants$estimate[2])) %>% 
  gadget_update('iseaten',1) 


## write to file
had.imm %>% 
  write.gadget.file(gd$dir)

had.mat %>% 
  write.gadget.file(gd$dir)




Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd$dir))
callGadget(s=1,log = 'init.log') #ignore.stderr = FALSE,

## update the input parameters with sane initial guesses
read.gadget.parameters(sprintf('%s/params.out',gd$dir)) %>% 
  init_guess('rec.[0-9]',2,0,10,1) %>%
  init_guess('init.[0-9]',1,0.001,1000,1) %>%
  init_guess('recl',11,10,12,1) %>% 
  init_guess('rec.sd',2,0.1,3,1) %>% 
  init_guess('Linf',75, 65, 500,1) %>% 
  init_guess('k.[0-9]',100, 1, 500,1) %>% 
  init_guess('bbin',6, 1e-08, 100, 1) %>% 
  init_guess('alpha.[0-9]', 0.5,  0.01, 3, 1) %>% 
  init_guess('l50.[0-9]',45,10,100,1) %>% 
  init_guess('walpha',lw.constants$estimate[1], 1e-10, 1,0) %>% 
  init_guess('wbeta',lw.constants$estimate[2], 2, 4,0) %>% 
  init_guess('M$',0.2,0.001,1,0) %>% 
  init_guess('M00',0.55,0.001,1,0) %>% 
  init_guess('M01',0.45,0.001,1,0) %>% 
  init_guess('M04',0.23,0.001,1,0) %>% 
  init_guess('M05',0.25,0.001,1,0) %>% 
  init_guess('M06',0.27,0.001,1,0) %>% 
  init_guess('M07',0.3,0.001,1,0) %>% 
  init_guess('M08',0.33,0.001,1,0) %>% 
  init_guess('M09',0.36,0.001,1,0) %>% 
  init_guess('M10',0.4,0.001,1,0) %>% 
  init_guess('M11',0.44,0.001,1,0) %>% 
  init_guess('M13',0.49,0.001,1,0) %>% 
  init_guess('M13',0.54,0.001,1,0) %>% 
  init_guess('M14',0.59,0.001,1,0) %>% 
  init_guess('M15',0.65,0.001,1,0) %>% 
  init_guess('rec.scalar',1,1,500,1) %>% 
  init_guess('init.scalar',1,0.1,300,1) %>% 
  init_guess('mat2',mat.l50$l50,0.75*mat.l50$l50,1.25*mat.l50$l50,1) %>% 
  init_guess('mat1',35,  10, 200, 1) %>% 
  init_guess('init.F',0.4,0.1,1.5,1) %>% 
  init_guess('p0.[0-9]',0,0,1,1) %>% 
  init_guess('p2.[0-9]',1,0,1,1) %>% 
  init_guess('p3.[0-9]',1,0.01,100,1) %>% 
  init_guess('p4.[0-9]',1,0.01,100,1) %>% 
  init_guess('mode.[0-9]',45,20,70,1) %>% 
  init_guess('p0',0,0,1,1) %>% 
  init_guess('p2',1,0,1,1) %>% 
  init_guess('p3',1,0.01,100,1) %>% 
  init_guess('p4',1,0.01,100,1) %>% 
  init_guess('mode',45,20,70,1) %>% 
  write.gadget.parameters(.,file=sprintf('%s/params.in',gd$dir))
