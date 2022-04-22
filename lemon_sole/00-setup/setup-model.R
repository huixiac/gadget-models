## Useful constansts

## weight length relationship
lw.constants <- 
  mfdb_dplyr_sample(mdb) %>% 
  filter(species == 'LEM',
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
  dplyr::filter(species == 'LEM',age >0,!is.na(length))  %>% 
  dplyr::select(age,length) %>% 
  dplyr::collect(n=Inf) %>% 
  dplyr::group_by(age) %>% 
  dplyr::summarise(ml=mean(length,na.rm=TRUE),ms=sd(length,na.rm=TRUE))

init.sigma$ms[init.sigma$age==1]<-init.sigma$ms[init.sigma$age==2]
init.sigma$ms[init.sigma$age==16]<-init.sigma$ms[init.sigma$age==15]

## initial guess for the maturity ogive:
mat.l50 <- 
  mfdb_dplyr_sample(mdb) %>% 
  filter(species == 'LEM',
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
lem.imm <- 
  gadgetstock('lemimm',gd,missingOkay = TRUE) %>%
  gadget_update('stock',
                minage = 1,
                maxage = 4,
                minlength = 3,
                maxlength = 70,
                dl = 2,
                livesonareas = 1) %>%
  gadget_update('doesgrow', ## note to self the order of these parameters make difference
                growthparameters=c(linf='#lemon.Linf', 
                                   k=to.gadget.formulae(quote(0.001*lemon.k)),
                                   alpha = '#lemonimm.walpha',
                                   beta = '#lemonimm.wbeta'),
                maxlengthgroupgrowth = 2,
                beta = to.gadget.formulae(quote(10*lemon.bbin))) %>% 
  gadget_update('initialconditions',
                normalparam = data_frame(age = .[[1]]$minage:.[[1]]$maxage,
                                         area = 1,
                                         age.factor = parse(text=sprintf('exp(-1*(lemonimm.M+lemon.init.F)*%1$s)*lemonimm.init.%1$s',age)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),   
                                         area.factor = '#lemonimm.init.scalar',
                                         mean = von_b_formula(age,linf='lemon.Linf',k='lemon.k',recl='lemon.recl'),
                                         #'(/ #lemon.Linf 4) (/ #lemon.Linf 3) (/ #lemon.Linf 2) #lemon.Linf',  
                                         stddev = init.sigma$ms[age],
                                         alpha = '#lemonimm.walpha',
                                         beta = '#lemonimm.wbeta')) %>% 
  ## does"something" updates should also allow for other names, e.g. doesrenew -> recruitment etc..
  gadget_update('refweight',
                data=data_frame(length=seq(.[[1]]$minlength,.[[1]]$maxlength,.[[1]]$dl),
                                mean=lw.constants$estimate[1]*length^lw.constants$estimate[2])) %>% 
  gadget_update('iseaten',1) %>% 
  gadget_update('doesmature', 
                maturityfunction = 'continuous',
                maturestocksandratios = 'lemmat 1',
                coefficients = '( * 0.001 #lemon.mat1) #lemon.mat2 0 0') %>% 
  gadget_update('doesmove',
                transitionstocksandratios = 'lemmat 1',
                transitionstep = 4) %>% 
  gadget_update('doesrenew',
                normalparam = data_frame(year = year_range,
                                         step = 1,
                                         area = 1,
                                         age = .[[1]]$minage,
                                         number = parse(text=sprintf('lemon.rec.scalar*lemon.rec.%s',year)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),
                                         mean = von_b_formula(age,linf='lemon.Linf',k='lemon.k',recl='lemon.recl'),
                                         stddev = '#lemon.rec.sd',
                                         alpha = '#lemonimm.walpha',
                                         beta = '#lemonimm.wbeta')) 




lem.mat <-
  gadgetstock('lemmat',gd,missingOkay = TRUE) %>%
  gadget_update('stock',
                minage = 3,
                maxage = 15,
                minlength = 15,
                maxlength = 70,
                dl = 2,
                livesonareas = 1) %>%
  gadget_update('doesgrow', ## note to self the order of these parameters make difference
                growthparameters=c(linf='#lemon.Linf', 
                                   k=to.gadget.formulae(quote(0.001*lemon.k)),
                                   alpha = '#lemonimm.walpha',
                                   beta = '#lemonimm.wbeta'),
                maxlengthgroupgrowth = 2,
                beta = to.gadget.formulae(quote(10*lemon.bbin))) %>% 
  gadget_update('initialconditions',
                normalparam = data_frame(age = .[[1]]$minage:.[[1]]$maxage,
                                         area = 1,
                                         age.factor = parse(text=sprintf('exp(-1*(lemmat.M+lemon.init.F)*%1$s)*lemmat.init.%1$s',age)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),
                                         area.factor = '#lemmat.init.scalar',
                                         mean = von_b_formula(age,linf='lemon.Linf',k='lemon.k',recl='lemon.recl'),
                                         stddev = init.sigma$ms[age],
                                         alpha = '#lemmat.walpha',
                                         beta = '#lemmat.wbeta')) %>% 
  ## does"something" updates should also allow for other names, e.g. doesrenew -> recruitment etc..
  gadget_update('refweight',
                data=data_frame(length=seq(.[[1]]$minlength,.[[1]]$maxlength,.[[1]]$dl),
                                mean=lw.constants$estimate[1]*length^lw.constants$estimate[2])) %>% 
  gadget_update('iseaten',1) 


## write to file
lem.imm %>% 
  write.gadget.file(gd)

lem.mat %>% 
  write.gadget.file(gd)



Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd))
callGadget(s=1,log = 'init.log') #ignore.stderr = FALSE,

## update the input parameters with sane initial guesses
read.gadget.parameters(sprintf('%s/params.out',gd)) %>% 
  init_guess('rec.[0-9]|init.[0-9]',1,0.001,1000,1) %>%
  init_guess('recl',5,4,25,1) %>% 
  init_guess('rec.sd',5, 1, 20,1) %>% 
  init_guess('Linf',50, 10, 200,1) %>% 
  init_guess('k$',10, 5, 300,1) %>% 
  init_guess('bbin',6, 1e-08, 100, 1) %>% 
  init_guess('alpha', 0.5,  0.01, 3, 1) %>% 
  init_guess('l50',10,3,70,1) %>% 
  init_guess('walpha',lw.constants$estimate[1], 1e-10, 1,0) %>% 
  init_guess('wbeta',lw.constants$estimate[2], 2, 4,0) %>% 
  init_guess('M$',0.15,0.001,1,0) %>% 
  init_guess('rec.scalar',10,1,50,1) %>% 
  init_guess('init.scalar',10,1,30,1) %>% 
#  init_guess('mat2',mat.l50$l50,0.75*mat.l50$l50,1.25*mat.l50$l50,0) %>% 
  init_guess('mat2',20,16,26,0) %>% 
  init_guess('mat1',20, 10, 500, 1) %>% #check on website - maybe  0.5,  0.01, 3,
  init_guess('init.F',0.4,0.1,1,1) %>% 
  # init_guess('p0',0,0,1,1) %>% 
  # init_guess('p2',1,0,1,1) %>% 
  # init_guess('p3',1,0.01,100,1) %>% 
  # init_guess('p4',1,0.01,100,1) %>% 
  # init_guess('mode',20,10,90,1) %>% 
  write.gadget.parameters(.,file=sprintf('%s/params.in',gd))

