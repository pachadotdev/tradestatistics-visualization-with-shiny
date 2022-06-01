# Simulate ----

wt_sim <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)

## 1. read from SQL ----

df_dtl_sim <- reactive({
  print("Collecting simulation data...")
  wt_sim$start()
  
  ### 3.1. apply filters ----
  
  d <- tbl(con, "yrp") %>%
    filter(year %in% !!inp_sim_y())
  
  ### 3.2. aggregate and transform data ----
  
  d <- d %>%
    select(year, importer = reporter_iso, exporter = partner_iso, 
           trade = trade_value_usd_imp)
  
  # add GRAVITY variables
  
  d <- d %>%
    mutate(
      country1 = pmin(importer, exporter, na.rm = T),
      country2 = pmax(importer, exporter, na.rm = T)
    ) %>%
    inner_join(
      tbl(con, "distances") %>% 
        select(country1, country2, dist, contig, comlang_off, colony),
      by = c("country1", "country2")
    )
  
  # add RTA data
  
  d <- d %>%
    left_join(
      tbl(con, "rtas") %>%
        filter(year %in% !!inp_sim_y()),
      by = c("year", "country1", "country2")
    ) %>%
    mutate(
      rta = case_when(
        is.na(rta) ~ 0L,
        TRUE ~ rta
      )
    ) %>%
    select(-c(country1,country2))
  
  # transform factors
  
  d <- d %>% 
    mutate(
      intl = ifelse(importer != exporter, 1, 0),
      importer = case_when(
        importer == !!imp_sim_reference() ~ paste0("0_", !!imp_sim_reference()),
        TRUE ~ importer
      ),
      exporter = case_when(
        exporter == !!imp_sim_reference() ~ paste0("0_", !!imp_sim_reference()),
        TRUE ~ exporter
      )
    )
  
  d <- d %>% 
    # Create Eit
    group_by(importer, year) %>%
    mutate(e = sum(trade, na.rm = T)) %>% 
    
    # Create Yit
    group_by(exporter, year) %>%
    mutate(y = sum(trade, na.rm = T)) %>%
    
    # Create Er
    group_by(year) %>%
    mutate(e_r = max(
      case_when(
        importer == paste0("0_", !!imp_sim_reference()) ~ e,
        TRUE ~ NA_real_
      ), 
      na.rm = T
    ))
  
  d <- d %>% 
    # Pairing variable for the internal dyads for the fixed effects
    mutate(
      exp_year = paste(exporter, year, sep = "_"),
      imp_year = paste(importer, year, sep = "_"),
      importer_exporter = paste(importer, exporter, sep = "_"),
      importer_exporter2 = case_when(
        exporter == importer ~ "0_intra",
        TRUE ~ importer_exporter
      )
    )
    
  d <- d %>% 
    # To filter the cases where the sum by dyad is zero
    group_by(importer_exporter) %>%
    mutate(sum_trade = sum(trade, na.rm = T)) %>% 
    ungroup()
  
  ### 3.3 collect data ----
  
  d <- d %>% 
    collect() %>% 
    arrange(year, importer, exporter)

  wt_sim$inc(1)
  
  ### 3.4. convert dollars in time ----
  
  if (inp_sim_convert_dollars() != "No conversion") {
    d <- gdp_deflator_adjustment(d, as.integer(inp_sim_convert_dollars()))
  }

  wt_sim$inc(1)
  
  gc()
  
  return(d)
}) %>% 
  bindCache(
    inp_sim_y(), imp_sim_reference(), inp_sim_convert_dollars()
  ) %>%
  bindEvent(input$sim_go)

## 2. Fit model ----

fit_sim <- eventReactive(input$sim_go, {
  print("Fitting model...")
  
  ### Step I: Solve the baseline gravity model ----
  
  d <- df_dtl_sim()
  
  fit_baseline <- fepois(
    trade ~ rta | exp_year + imp_year + importer_exporter2,
    data = filter(d, sum_trade > 0),
    glm.iter = 500
  )
  
  d <- d %>%
    mutate(
      fe_exporter_bln = fixef(fit_baseline)$exp_year[exp_year],
      fe_importer_bln = fixef(fit_baseline)$imp_year[imp_year],
      fe_pair_id_2_bln = fixef(fit_baseline)$importer_exporter2[importer_exporter2]
    )
  
  d <- d %>%
    mutate(
      tij_bar = exp(fe_pair_id_2_bln),
      tij_bln = exp(fe_pair_id_2_bln + fit_baseline$coefficients["rta"] * rta)
    )
  
  d_slice <- d %>%
    filter(year == !!imp_sim_y2(), exporter != importer)
  
  fit_costs <- fepois(
    tij_bar ~ log(dist) + contig + comlang_off + colony | exporter + importer,
    data = d_slice,
    glm.iter = 500
  )
  
  d_slice <- d_slice %>%
    mutate(tij_no_rta = predict(fit_costs, d_slice)) %>%
    select(exporter, importer, tij_no_rta)
  
  d <- d %>%
    filter(year == !!imp_sim_y2()) %>%
    left_join(d_slice, by = c("importer", "exporter")) %>%
    mutate(
      tij_bar = ifelse(is.na(tij_bar), tij_no_rta, tij_bar),
      tij_bln = ifelse(is.na(tij_bln), tij_bar * exp(fit_baseline$coefficients["rta"] * rta), tij_bln)
    ) %>%
    select(-tij_no_rta)
  
  fit_constrained <- fepois(
    trade ~ 0 | exporter + importer,
    data = d,
    offset = ~log(tij_bln),
    glm.iter = 500
  )
  
  d <- d %>%
    mutate(tradehat_bln = predict(fit_constrained, d)) %>%
    group_by(exporter) %>%
    mutate(xi_bln = sum(tradehat_bln * (exporter != importer))) %>%
    ungroup()
  
  d <- d %>%
    mutate(
      fe_exporter_cns = fixef(fit_constrained)$exporter[exporter],
      fe_importer_cns = fixef(fit_constrained)$importer[importer]
    )
  
  d <- d %>%
    mutate(
      omr_bln = y * e_r/ exp(fe_exporter_cns),
      imr_bln = e / (exp(fe_importer_cns) * e_r)
    )
  
  ### Step II: Define a counterfactual scenario ----
  
  region <- imp_sim_countries()
  
  d <- d %>%
    mutate(
      rta_no_region = ifelse(exporter %in% region & importer %in% region, 0, rta),
      tij_cfl = tij_bar * exp(fit_baseline$coefficients["rta"] * rta_no_region)
    )
  
  ### Step III: Solve the counterfactual model ----
  
  fit_counterfactual <- fepois(
    trade ~ 0 | exporter + importer,
    data = d,
    offset = ~log(tij_cfl),
    glm.iter = 500
  )
  
  d <- d %>%
    mutate(
      fe_exporter_cfl = fixef(fit_counterfactual)$exporter[exporter],
      fe_importer_cfl = fixef(fit_counterfactual)$importer[importer]
    )
  
  d <- d %>%
    mutate(
      omr_cfl = y * e_r / exp(fe_exporter_cfl),
      imr_cfl = e / (exp(fe_importer_cfl) * e_r)
    )
  
  d <- d %>%
    mutate(tradehat_cfl = predict(fit_counterfactual, ch2_application2)) %>%
    group_by(exporter) %>%
    mutate(xi_cfl = sum(tradehat_cfl * (exporter != importer))) %>%
    ungroup()
  
  sigma <- imp_sim_sigma()
  
  d <- d %>%
    mutate(
      change_tij = tij_cfl / tij_bln,
      phi = ifelse(importer == exporter, e / y, 0)
    ) %>%
    group_by(exporter) %>%
    mutate(phi = max(phi)) %>%
    ungroup()
  
  d <- d %>%
    group_by(exporter) %>%
    mutate(change_p_i = ((exp(fe_exporter_cfl) / e_r) / (exp(fe_exporter_cns) / e_r))^(1 /(1 - sigma))) %>%
    ungroup() %>%
    
    group_by(importer) %>%
    mutate(
      change_p_j = ifelse(importer == exporter, change_p_i, 0),
      change_p_j = max(change_p_j)
    ) %>%
    ungroup()
  
  d <- d %>%
    mutate(trade_cfl = tradehat_cfl * change_p_i * change_p_j)
  
  d <- d %>%
    mutate(
      omr_cfl_0 = omr_cfl,
      imr_cfl_0 = imr_cfl,
      change_imr_full_0 = 1,
      change_omr_full_0 = 1,
      change_p_i_0 = change_p_i,
      change_p_j_0 = change_p_j,
      fe_exporter_cfl_0 = fe_exporter_cfl,
      fe_importer_cfl_0 = fe_importer_cfl,
      tradehat_0 = tradehat_cfl,
      e_r_cfl_0 = e_r
    )
  
  # set parameters
  max_dif <- 1
  sd_dif <- 1
  change_price_i_old <- 0
  
  i2 <- 1
  while(sd_dif > 1e-3 | max_dif > 1e-3) {
    cat(paste0(i," "))
    d <- d %>%
      mutate(trade_1 = tradehat_0 * change_p_i_0 * change_p_j_0 / (change_omr_full_0 * change_imr_full_0))
    
    # repeat the counterfactual model
    fit_counterfactual_2 <- fepois(
      trade_1 ~ 0 | exporter + importer,
      data = d,
      offset = ~log(tij_cfl),
      glm.iter = 500
    )
    
    d <- d %>%
      mutate(
        fe_exporter_cfl = fixef(fit_counterfactual_2)$exporter[exporter],
        fe_importer_cfl = fixef(fit_counterfactual_2)$importer[importer]
      )
    
    # compute the conditional general equilibrium effects of trade
    d <- d %>%
      mutate(tradehat_1 = predict(fit_counterfactual_2, d)) %>%
      group_by(exporter) %>%
      mutate(y_cfl_1 = sum(tradehat_1)) %>%
      ungroup() %>%
      
      mutate(e_cfl_1 = ifelse(importer == exporter, phi * y_cfl_1, 0)) %>%
      group_by(importer) %>%
      mutate(e_cfl_1 = max(e_cfl_1)) %>%
      ungroup() %>%
      
      mutate(
        e_r_cfl_1 = case_when(
          importer == paste0("0_", imp_sim_reference()) ~ e_cfl_1,
          TRUE ~ 0
        ),
        e_r_cfl_1 = max(e_r_cfl_1)
      )
    
    # compute the change in prices for exporters and importers
    D <- D %>%
      mutate(change_p_i_1 = ((exp(fe_exporter_cfl) / e_r_cfl_1) /
                               (exp(fe_exporter_cfl_0) / e_r_cfl_0))^(1 / (1 - sigma)))
    
    # compute the change in prices for exporters and importers
    d <- d %>%
      group_by(importer) %>%
      mutate(
        change_p_j_1 = ifelse(importer == exporter, change_p_i_1, 0),
        change_p_j_1 = max(change_p_j_1)
      ) %>%
      ungroup()
    
    # compute both outward and inward multilateral resistance
    d <- d %>%
      mutate(
        omr_cfl_1 = (y_cfl_1 * e_r_cfl_1) / exp(fe_exporter_cfl),
        imr_cfl_1 = e_cfl_1 / (exp(fe_importer_cfl) * e_r_cfl_1)
      )
    
    # update the differences
    max_dif <- abs(max(d$change_p_i_0 - change_price_i_old))
    sd_dif <- sd(d$change_p_i_0 - change_price_i_old)
    change_price_i_old <- d$change_p_i_0
    
    # compute changes in outward and inward multilateral resistance
    d <- d %>%
      mutate(
        change_omr_full_1 = omr_cfl_1 / omr_cfl_0,
        change_imr_full_1 = imr_cfl_1 / imr_cfl_0,
        omr_cfl_0 = omr_cfl_1,
        imr_cfl_0 = imr_cfl_1,
        change_omr_full_0 = change_omr_full_1,
        change_imr_full_0 = change_imr_full_1,
        change_p_i_0 = change_p_i_1,
        change_p_j_0 = change_p_j_1,
        fe_exporter_cfl_0 = fe_exporter_cfl,
        fe_importer_cfl_0 = fe_importer_cfl,
        tradehat_0 = tradehat_1,
        e_r_cfl_0 = e_r_cfl_1
      ) %>%
      select(-fe_exporter_cfl, -fe_importer_cfl)
    
    i2 <- i2 + 1
  }
  
  d <- d %>%
    mutate(
      change_p_i_full = ((exp(fe_exporter_cfl_0) / e_r_cfl_0) /
                           (exp(fe_exporter_cns) / e_r))^(1 / (1 - sigma)),
      change_p_j_full = change_p_i_full * (exporter == importer)
    ) %>%
    group_by(importer) %>%
    mutate(change_p_j_full = max(change_p_j_full)) %>%
    ungroup() %>%
    mutate(y_full = change_p_i_full * y)
  
  d <- d %>%
    mutate(e_full = change_p_j_full * e * (exporter == importer)) %>%
    group_by(importer) %>%
    mutate(e_full = max(e_full, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      e_full_r = e_full * (importer == "0-DEU"),
      e_full_r = max(e_full_r)
    )
  
  d <- d %>%
    mutate(
      omr_full = y_full * e_r_cfl_0 / exp(fe_exporter_cfl_0),
      imr_full = e_cfl_1 / (exp(fe_importer_cfl_0) * e_r_cfl_0)
    )
  
  d <- d %>%
    mutate(x_full = (y_full * e_full * tij_cfl) / (imr_full * omr_full)) %>%
    group_by(exporter) %>%
    mutate(xi_full = sum(x_full * (importer != exporter))) %>%
    ungroup()
  
  wt_sim$close() 
  return(d)
})
