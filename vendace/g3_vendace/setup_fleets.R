## create fleets

fleet_actions <- 
  list(
    acoven %>% 
      g3a_predate_totalfleet(list(ven_imm, ven_mat),
                             suitabilities = list(
                               ven_imm = g3_suitability_exponentiall50(~g3_param('ven.aco.alpha'), ~g3_param('ven.aco.l50')),
                               ven_mat = g3_suitability_exponentiall50(~g3_param('ven.aco.alpha'), ~g3_param('ven.aco.l50'))),
                             amount_f = g3_timeareadata('aco_catch', value_field="number", aco.catch %>% 
                                                          mutate(area = as.numeric(area),
                                                                 step = as.numeric(step),
                                                                 year = as.numeric(year)))),
    comven1 %>% 
      g3a_predate_totalfleet(list(ven_imm, ven_mat),
                             suitabilities = list(
                               ven_imm = g3_suitability_exponentiall50(~g3_param('ven.com1.alpha'), ~g3_param('ven.com1.l50')),
                               ven_mat = g3_suitability_exponentiall50(~g3_param('ven.com1.alpha'), ~g3_param('ven.com1.l50'))),
                             amount_f = g3_timeareadata('comven1_catch', value_field="biomass", com.catch.ven %>%
                                                          filter(fleet == "comven1") %>%
                                                          mutate(area = as.numeric(area),
                                                                 step = as.numeric(step),
                                                                 year = as.numeric(year)))),
    comven2 %>% 
      g3a_predate_totalfleet(list(ven_imm, ven_mat),
                             suitabilities = list(
                               ven_imm = g3_suitability_exponentiall50(~g3_param('ven.com2.alpha'), ~g3_param('ven.com2.l50')),
                               ven_mat = g3_suitability_exponentiall50(~g3_param('ven.com2.alpha'), ~g3_param('ven.com2.l50'))),
                             amount_f = g3_timeareadata('comven2_catch', value_field="biomass", com.catch.ven %>% 
                                                          filter(fleet == "comven2") %>%
                                                          mutate(area = as.numeric(area),
                                                                 step = as.numeric(step),
                                                                 year = as.numeric(year)))),
    sealven %>% 
      g3a_predate_totalfleet(list(ven_imm, ven_mat),
                             suitabilities = list(
                               ven_imm = g3_suitability_exponentiall50(~g3_param('ven.sea.alpha'), ~g3_param('ven.sea.l50')),
                               ven_mat = g3_suitability_exponentiall50(~g3_param('ven.sea.alpha'), ~g3_param('ven.sea.l50'))),
                             amount_f = g3_timeareadata('sealven_cons', value_field="biomass", seal.cons.ven %>% 
                                                          mutate(area = as.numeric(area),
                                                                 step = as.numeric(step),
                                                                 year = as.numeric(year)))))
