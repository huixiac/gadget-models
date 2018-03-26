#devtools::install_github('Hafro/rgadget')
#library(Rgadget)

timestamp()

setwd('/net/hafkaldi/export/home/haf/pamela/fishvice/gadget-models/06-ling/12-new_ass')
print('running ling')

source('run.R')

closeAllConnections()
timestamp()


setwd('/net/hafkaldi/export/home/haf/pamela/fishvice/gadget-models/08-tusk/01-new_ass')
print('running tusk')

source('run.R')

closeAllConnections()
timestamp()

setwd('/net/hafkaldi/export/home/haf/pamela/fishvice/gadget-models/08-tusk/01-new_ass')
