## Useful constansts

## weight length relationship
lw.constants <- 
  mfdb_dplyr_sample(mdb) %>% 
  filter(species == 'LIN',
         sampling_type == 'IGFS',
         !is.na(weight)) %>% 
  select(length,weight) %>% 
  collect(n=Inf) %>% 
  lm(log(weight)~log(length),.) %>% 
  broom::tidy() %>% 
  select(estimate)


## initial conditions sigma
init.sigma <- 
  mfdb_dplyr_sample(mdb) %>% 
  dplyr::filter(species == 'LIN',age >0,!is.na(length))  %>% 
  dplyr::select(age,length) %>% 
  dplyr::collect(n=Inf) %>% 
  dplyr::group_by(age) %>% 
  dplyr::summarise(ml=mean(length,na.rm=TRUE),ms=sd(length,na.rm=TRUE))


## convenience functions
von_b_formula <- function(a,linf='Linf',k='k',recl='recl'){
  a %>% 
    map(~infuser::infuse("{{linf}} * (1 - exp(-1 * (0.001 * {{k}}) * ({{a}} - (1 + log(1 - {{recl}}/{{linf}})/(0.001 * k)))))",
                         a=.,linf=linf,k=k,recl=recl)) %>% 
    map(~parse(text=.) %>% 
          map(to.gadget.formulae)) %>% 
    unlist()
}

## setup the immature stock first
ling.imm <- 
  gadgetstock('lingimm',gd$dir,missingOkay = TRUE) %>%
  gadget_update('stock',
                minage = 3,
                maxage = 10,
                minlength = 20,
                maxlength = 160,
                dl = 4,
                livesonareas = 1) %>%
  gadget_update('initialconditions',
                normalparam = data_frame(age = 3:10,
                                         area = 1,
                                         age.factor = 1,
                                         area.factor = 1,
                                         number = parse(text=sprintf('lingimm.init.scalar*lingimm.init.%s',age)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),
                                         mean = von_b_formula(age,linf='ling.linf',k='ling.k',recl='ling.recl'),
                                         stddev = init.sigma$ms[age],
                                         alpha = '#ling.walpha',
                                         beta = '#ling.wbeta')) %>% 
  ## does"something" updates should also allow for other names, e.g. doesrenew -> recruitment etc..
  gadget_update('doesgrow',  
                growthfunction = 'lengthvbsimple',
                growthparameters = c(linf='#ling.linf',k='( * 0.001 #ling.k)', '#ling.walpha','#ling.wbeta'),
                beta='(* 10 #ling.bbin)', 
                maxlengthgroupgrowth=15) %>%
  gadget_update('naturalmortality',   
                rep('#ling.M',8)) %>%
  gadget_update('refweight',
                data=data_frame(length=seq(20,160,4),
                                mean=lw.constants$estimate[1]*length^lw.constants$estimate[2])) %>% 
  gadget_update('iseaten',1) %>% 
  gadget_update('doesmature', 
                maturityfunction = 'continuous',
                maturestocksandratios = 'lingmat 1',
                coefficients = '( * 0.001 #ling.mat1) #ling.mat2 0 0') %>% 
  gadget_update('doesmove',
                transitionstockandratios = 'lingmat 1',
                transitionstep = 4) %>% 
  gadget_update('doesrenew',
                normalparam = data_frame(year = year_range,
                                         step = 1,
                                         area = 1,
                                         age = 3,
                                         number = parse(text=sprintf('ling.rec.scalar*ling.rec.%s',year)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),
                                         mean = von_b_formula(age,linf='ling.linf',k='ling.k',recl='ling.recl'),
                                         stddev = '#ling.rec.sd',
                                         alpha = '#ling.walpha',
                                         beta = '#ling.wbeta')) 




ling.mat <-
  gadgetstock('lingmat',gd$dir,missingOkay = TRUE) %>%
  gadget_update('stock',
                minage = 5,
                maxage = 15,
                minlength = 20,
                maxlength = 160,
                dl = 4,
                livesonareas = 1) %>%
  gadget_update('initialconditions',
                normalparam = data_frame(age = 5:1510,
                                         area = 1,
                                         age.factor = 1,
                                         area.factor = 1,
                                         number = parse(text=sprintf('lingmat.init.scalar*lingmat.init.%s',age)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),
                                         mean = von_b_formula(age,linf='ling.linf',k='ling.k',recl='ling.recl'),
                                         stddev = init.sigma$ms[age],
                                         alpha = '#ling.walpha',
                                         beta = '#ling.wbeta')) %>% 
  ## does"something" updates should also allow for other names, e.g. doesrenew -> recruitment etc..
  gadget_update('doesgrow',  
                growthfunction = 'lengthvbsimple',
                growthparameters = c(linf='#ling.linf',k='( * 0.001 #ling.k)', '#ling.walpha','#ling.wbeta'),
                beta='(* 10 #ling.bbin)', 
                maxlengthgroupgrowth=15) %>%
  gadget_update('naturalmortality',   
                rep('#ling.M',11)) %>%
  gadget_update('refweight',
                data=data_frame(length=seq(20,160,4),
                                mean=lw.constants$estimate[1]*length^lw.constants$estimate[2])) %>% 
  gadget_update('iseaten',1) 


## write to file
ling.imm %>% 
  write.gadget.file(gd$dir)

ling.mat %>% 
  write.gadget.file(gd$dir)






if(FALSE){

## lets find proportion mature @age
matatage <- mfdb_sample_count(mdb, c('maturity_stage','age'),
                             append(list(species = 'LIN'),
                                    list(sampling_type='IGFS',
                                         age=mfdb_step_interval('',by=1,from=1,to=20),
                                         maturity_stage = mfdb_group(lingimm = 1, lingmat = 2:5))))[[1]] %>%
  dplyr::mutate(age=as.numeric(age)) %>% 
  dplyr::group_by(age) %>% 
  dplyr::mutate(prop = number/sum(number)) %>% 
  select(stock=maturity_stage,age,prop)

p.imm <- (filter(matatage,stock=='lingimm') %>% arrange(age))[['prop']][1:9] ## recruits are thrown away
p.mat <- (filter(matatage,stock=='lingmat') %>% arrange(age))[['prop']] 

## populate the model with sane defaults
opt <- gadget.options(type='simple2stock')

## adapt to Ling
weight.alpha <-  as.numeric(lw.tmp[1]) #0.00000495
weight.beta <- as.numeric(lw.tmp[2]) #3.01793

opt$area$numofareas <- 1
opt$time$firstyear <- min(year_range)
opt$time$lastyear <- max(year_range)



## immature stock
opt$stocks$imm <-
  within(opt$stocks$imm,{
    name <- 'lingimm'
    minage <- 1
    maxage <- 10
    minlength <- 4
    maxlength <- 180
    dl <- 2
    growth <- c(linf='#ling.Linf',k='( * 0.001 #ling.k)',
                beta='(* 10 #ling.bbin)', binn=15,recl='#ling.recl')
    weight <- c(a=weight.alpha, b=weight.beta)
    init.abund <- sprintf('(* 0.01 %s %s)',c(0,p.imm),
                          c(0,sprintf('#ling.age%s',2:10)))
    n <- sprintf('(* 1000 #ling.rec%s)',year_range)
    doesmature <- 1
    maturityfunction <- 'continuous'
    maturestocksandratios <- 'lingmat 1'
    maturity.coefficients <- '( * 0.001 #ling.mat1) #ling.mat2 0 0'
    sigma <- c(init.sigma$ms[1],head(init.sigma$ms,14),rep(init.sigma$ms[14],5))
      #list(alpha='( * 0.001 #mat1)',
                            #      l50='#mat2', beta=0,
                            #      a50=0)

    ## using the same M-formulation as cod (is that a good idea, Muggur?)
    M <- rep(0.15,10) #c(0.5,  0.35, 0.2, 0.2,  0.2, 0.2, 0.2, 0.2, 0.2, 0.3)
    
    maturitysteps <- '0'
    doesmove <- 1
    transitionstep <- 4
    transitionstocksandratios <- "lingmat 1"
    doesmigrate <- 0
    doesrenew <- 1
    renewal <- list(minlength=4, maxlength=28)
  })


## mature stock
opt$stocks$mat <-
  within(opt$stocks$mat,{
    name <- 'lingmat'
    minage <- 3
    maxage <- 20
    minlength <- 20
    maxlength <- 180
    dl <- 2
    M <- rep(0.15, 18) #c(0.2,  0.2, 0.2, 0.2, 0.2, 0.2, 0.2, 0.3, 0.5, 0.7)
    growth <- c(linf='#ling.Linf',k='( * 0.001 #ling.k)',
                beta='(* 10 #ling.bbin)', binn=15,recl='#ling.recl')
    weight <- c(a=weight.alpha, b=weight.beta)
    init.abund <- sprintf('(* 0.01 %s %s)',
                          c(p.mat,  exp(-0.5*1:3)*0.0001),
                          c(0,sprintf('#ling.age%s',4:10),rep('#ling.age10',10)))
    sigma <- c(init.sigma$ms[1],head(init.sigma$ms,14),rep(init.sigma$ms[14],5))
    doesmature <- 0
    doesmigrate <- 0
  })


## create the gadget skeleton
gm <- gadget.skeleton(time=opt$time, area=opt$area,
                      stocks=opt$stocks,opt$fleets)

gm@stocks$imm@initialdata$area.factor <- '( * 100 #ling.mult)'
gm@stocks$mat@initialdata$area.factor <- '( * 100 #ling.mult)'

gm@fleets <- list(igfs.fleet,#aut.fleet,
                  lln.fleet,gil.fleet,bmt.fleet,foreign.fleet)
gd.list <- list(dir=gd$dir)
Rgadget:::gadget_dir_write(gd.list,gm)

curr.dir <- getwd()
setwd(gd$dir)
callGadget(s=1,ignore.stderr = FALSE)

init.params <- read.gadget.parameters('params.out')

init.params[c('ling.Linf','ling.k','ling.bbin','ling.mult',
              grep('age',rownames(init.params),value=TRUE),
              'ling.mat1','ling.mat2'),] <- 
  read.table(text='switch	 value 		lower 	upper 	optimise
ling.Linf	         180	      80     200        0
ling.k	          90	       60      100        1
ling.bbin	         6	   1e-08    100        1
ling.mult	         100	     0.1      100        1
ling.age2	         35	    0.01     150        1
ling.age3	         25	    0.01     120        1
ling.age4	         15	   0.001     100        1
ling.age5	          7	  0.0001     100        1
ling.age6	          7	   1e-05     100        1
ling.age7	          5	   1e-08     100        1
ling.age8	          5	   1e-10     100        1
ling.age9	         25	   1e-12     100        1
ling.age10	         10	   1e-15     100        1
ling.mat1	          70	      10      200        1
ling.mat2	          100	      60      150        1',header=TRUE) 

init.params$switch <- rownames(init.params)

init.params[grepl('rec[0-9]',init.params$switch),'value'] <- 1
init.params[grepl('rec[0-9]',init.params$switch),'upper'] <- 4
init.params[grepl('rec[0-9]',init.params$switch),'lower'] <- 0.001
init.params[grepl('rec[0-9]',init.params$switch),'optimise'] <- 1

init.params['ling.recl',-1] <- c(12, 4, 20,1)

init.params[grepl('alpha',init.params$switch),'value'] <- 0.5
init.params[grepl('alpha',init.params$switch),'upper'] <- 3
init.params[grepl('alpha',init.params$switch),'lower'] <- 0.01
init.params[grepl('alpha',init.params$switch),'optimise'] <- 1

init.params[grepl('l50',init.params$switch),'value'] <- 50
init.params[grepl('l50',init.params$switch),'upper'] <- 100
init.params[grepl('l50',init.params$switch),'lower'] <- 10
init.params[grepl('l50',init.params$switch),'optimise'] <- 1

write.gadget.parameters(init.params,file='params.in')
setwd(curr.dir)
}