## stock actions



## setup the immature stock
ven_imm_actions <- list(
  g3a_initialconditions_normalparam(ven_imm,
                                    # NB: area & age factor together (gadget2 just multiplied them)
                                    factor_f = ~g3_param("venimm.init.scalar") * exp(-1 * (g3_param_table("venimm.M", data.frame(age = seq(ven_imm__minage, ven_imm__maxage) )) + g3_param("ven.init.F")) * age) * g3_param_table("venimm.init", data.frame(age = seq(ven_imm__minage, ven_imm__maxage))),
                                    mean_f = ~g3_param("ven.Linf") * (1 - exp(-1 * (0.001 * g3_param("ven.k")) * (age - (1 + log(1 - g3_param("ven.recl")/g3_param("ven.Linf"))/(0.001 * g3_param("ven.k")))))),
                                    stddev_f = ~g3_param_vector("ven.init.sd")[[age+1]],
                                    alpha_f = ~g3_param("venimm.walpha"),
                                    beta_f = ~g3_param("venimm.wbeta")),
                                    ## run_f = ~cur_step == 1 && age == 1),
  g3a_renewal_normalparam(ven_imm,
                          factor_f = ~g3_param("ven.rec.scalar") * g3_param_table("ven.rec", data.frame(cur_year = seq(start_year, end_year))),
                          mean_f = ~g3_param("ven.Linf") * (1 - exp(-1 * (0.001 * g3_param("ven.k")) * (age - (1 + log(1 - g3_param("ven.recl")/g3_param("ven.Linf"))/(0.001 * g3_param("ven.k")))))),
                          stddev_f = ~g3_param_vector('ven.init.sd')[[age+1]],
                          alpha_f = ~g3_param("venimm.walpha"),
                          beta_f = ~g3_param("venimm.wbeta"),
                          run_f = ~cur_step == 2 && age == 0),
  g3a_growmature(ven_imm,
                 impl_f = g3a_grow_impl_bbinom(
                    g3a_grow_lengthvbsimple(~g3_param("ven.Linf"), ~g3_param("ven.k") * 0.001),
                    g3a_grow_weightsimple(~g3_param("venimm.walpha"), ~g3_param("venimm.wbeta")),
                    beta_f = ~g3_param("ven.bbin") * 1e3,
                    maxlengthgroupgrowth = 4),
                 maturity_f = g3a_mature_constant(
                   alpha = ~1 * g3_param("ven.mat1"),  #***CHECK multiplier
                   l50 = ~g3_param("ven.mat2")),
                   #run_f = ~cur_step == 1,
                 output_stocks = list(ven_mat)),
  g3a_naturalmortality(ven_imm,
                       g3a_naturalmortality_exp(~g3_param_table("venimm.M", data.frame(age = seq(ven_imm__minage, ven_imm__maxage))))),
  g3a_age(ven_imm,
          output_stocks = list(ven_mat)),
  list())

## setup the mature stock
ven_mat_actions <- list(
  g3a_initialconditions_normalparam(ven_mat,
                                    # NB: area & age factor together (gadget2 just multiplied them)
                                    factor_f = ~g3_param("venmat.init.scalar") * exp(-1 * (g3_param_table("venmat.M", data.frame(age = seq(ven_mat__minage, ven_mat__maxage))) + g3_param("ven.init.F")) * age) * g3_param_table("venmat.init", data.frame(age = seq(ven_mat__minage, ven_mat__maxage))),
                                    mean_f = ~g3_param("ven.Linf") * (1 - exp(-1 * (0.001 * g3_param("ven.k")) * (age - (1 + log(1 - g3_param("ven.recl")/g3_param("ven.Linf"))/(0.001 * g3_param("ven.k")))))),
                                    stddev_f = ~g3_param_vector('ven.init.sd')[[age+1]],
                                    alpha_f = ~g3_param("venmat.walpha"),
                                    beta_f = ~g3_param("venmat.wbeta")),
                                    ## run_f = ~cur_step == 1 && age >= 1 && age <= 8),
  g3a_growmature(ven_mat,
                 impl_f = g3a_grow_impl_bbinom(
                    g3a_grow_lengthvbsimple(~g3_param("ven.Linf"), ~g3_param("ven.k") * 0.001),
                    g3a_grow_weightsimple(~g3_param("venmat.walpha"), ~g3_param("venmat.wbeta")),
                    beta_f = ~g3_param("ven.bbin") * 1e3,
                    maxlengthgroupgrowth = 4)),
  g3a_naturalmortality(ven_mat,
                       g3a_naturalmortality_exp(~g3_param_table("venmat.M", data.frame(age = seq(ven_mat__minage, ven_mat__maxage))))),
  g3a_age(ven_mat),
  list())
