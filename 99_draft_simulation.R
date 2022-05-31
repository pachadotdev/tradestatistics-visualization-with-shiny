# Simulate ----

wt_sim <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)

## 1. read from SQL ----

df_dtl_sim <- reactive({
  print("Collecting simulation data...")
  wt_sim$start()
  
  ### 3.1. apply filters ----
  
  tbl_sql <- if (any(inp_sim_product_filter() != "all")) {
    "yrpc"
  } else {
    "yrp"
  }
  
  d <- tbl(con, tbl_sql)
  
  d <- d %>%
    filter(
      year %in% !!inp_sim_y()
    )
  
  wt_sim$inc(1)
  
  if (any(inp_sim_product_filter() %in% "vaccine")) {
    d <- d %>% 
      left_join(
        tbl(con, "vaccine_inputs")
      ) %>% 
      mutate(
        section_code = case_when(
          is_vaccine_input == 1L ~ "vaccine",
          TRUE ~ section_code
        )
      )
  }
  
  if (any(inp_sim_product_filter() != "all")) {
    d <- d %>%
      filter(section_code %in% !!inp_sim_product_filter())
  }
  
  wt_sim$inc(1)
  
  #### aggregate and transform data ----
  
  d <- d %>%
    select(year, importer = reporter_iso, exporter = partner_iso, trade = trade_value_usd_imp) %>%
    group_by(year, importer, exporter) %>%
    summarise(trade = sum(trade, na.rm = T)) %>%
    ungroup()
  
  d <- d %>% 
    mutate(
      intl = ifelse(importer != exporter, 1, 0),
      importer = case_when(
        importer == imp_sim_reference() ~ paste0("0-", imp_sim_reference())
        TRUE ~ imp_sim_reference()
      ),
      exporter = case_when(
        exporter == imp_sim_reference() ~ paste0("0-", imp_sim_reference())
        TRUE ~ imp_sim_reference()
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
        importer == paste0("0-", imp_sim_reference()) ~ e,
        TRUE ~ NA_real_
      ), 
      na.rm = T
    ))
  
  d <- d %>% 
    # Pairing variable for the internal dyads for the fixed effects
    mutate(
      exp_year = paste0(exporter, year),
      imp_year = paste0(importer, year),
      pair_id = paste(importer, exporter, sep = "_"),
      pair_id_2 = case_when(
        exporter == importer ~ "0-intra",
        TRUE ~ pair_id
      )
    ) %>% 
    
    # To filter the cases where the sum by dyad is zero
    group_by(pair_id) %>%
    mutate(sum_trade = sum(trade))
  
  #### collect data ----
  
  d <- d %>% collect()
  
  wt_sim$inc(1)
  
  ### 3.2. add geo dist data ----
  
  d <- d %>%
    mutate(
      country1 = pmin(reporter_iso, partner_iso),
      country2 = pmax(reporter_iso, partner_iso)
    ) %>%
    inner_join(
      tbl(con, "distances") %>% collect(),
      by = c("country1", "country2")
    ) %>%
    select(-c(country1,country2))
  
  wt_sim$inc(1)
  
  ### 3.3. add RTA data ----
  
  if (any(rhs_sim() %in% "rta")) {
    d <- d %>%
      mutate(
        country1 = pmin(reporter_iso, partner_iso),
        country2 = pmax(reporter_iso, partner_iso)
      ) %>%
      left_join(
        tbl(con, "rtas") %>%
          filter(year %in% !!inp_sim_y()) %>%
          collect(),
        by = c("year", "country1", "country2")
      ) %>%
      mutate(
        rta = case_when(
          is.na(rta) ~ 0L,
          TRUE ~ rta
        )
      ) %>%
      select(-c(country1,country2))
  }
  
  wt_sim$inc(1)
  
  ### 3.4. create remoteness indexes ----
  
  if (inp_sim_type() == "olsrem") {
    d <- d %>%
      # Replicate total_e
      group_by(reporter_iso, year) %>%
      mutate(total_e = sum(trade_value_usd_exp, na.rm = T)) %>%
      group_by(year) %>%
      mutate(total_e = max(total_e, na.rm = T)) %>%
      # Replicate rem_exp
      group_by(reporter_iso, year) %>%
      mutate(
        remoteness_exp = sum(dist *  total_e / trade_value_usd_exp, na.rm = T)
      )
    
    d <- d %>%
      # Replicate total_y
      group_by(partner_iso, year) %>%
      mutate(total_y = sum(trade_value_usd_imp, na.rm = T)) %>%
      group_by(year) %>%
      mutate(total_y = max(total_y, na.rm = T)) %>%
      # Replicate rem_imp
      group_by(partner_iso, year) %>%
      mutate(
        remoteness_imp = sum(dist / (trade_value_usd_imp / total_y), na.rm = T)
      )
    
    d <- d %>%
      select(-c(total_e, total_y))
  }
  
  wt_sim$inc(1)
  
  ### 3.5. create fixed effects ----
  
  if (inp_sim_type() == "olsfe") {
    d <- d %>%
      mutate(
        reporter_yr = paste0(reporter_iso, year),
        partner_yr = paste0(partner_iso, year)
      )
  }
  
  wt_sim$inc(.5)
  
  ### 3.5. create clustering variable ----
  
  if (inp_sim_cluster() == "yes") {
    d <- d %>%
      mutate(cluster_pairs = paste(reporter_iso, partner_iso, sep = "_"))
  }
  
  wt_sim$inc(.5)
  
  ### 3.6. convert dollars in time ----
  
  if (inp_sim_convert_dollars() != "No conversion") {
    d <- gdp_deflator_adjustment(d, as.integer(inp_sim_convert_dollars()))
  }
  
  ### 3.7. add MFN data ----
  
  if (any(raw_rhs_sim() %in% "mfn")) {
    tar <- tbl(con, "tariffs") %>%
      filter(
        year %in% !!inp_sim_y()
      )
    
    trd <- tbl(con, "yrpc") %>% 
      filter(
        year %in% !!inp_sim_y() & reporter_iso %in% !!inp_sim_riso()
      )
    
    if (any(inp_sim_piso() != "all")) {
      tar <- tar %>%
        filter(
          # here we need the applied tariffs when the product gets to destination
          reporter_iso %in% !!inp_sim_piso()
        )
      
      trd <- tbl(con, "yrpc") %>% 
        filter(
          partner_iso %in% !!inp_sim_riso()
        )
    }
    
    if (any(inp_sim_product_filter() %in% "vaccine")) {
      trd <- trd %>% 
        left_join(
          tbl(con, "vaccine_inputs")
        ) %>% 
        mutate(
          section_code = case_when(
            is_vaccine_input == 1L ~ "vaccine",
            TRUE ~ section_code
          )
        )
    }
    
    if (length(inp_sim_product_filter()) > 0) {
      trd <- trd %>%
        filter(section_code %in% !!inp_sim_product_filter())
    }
    
    trd <- trd %>%
      select(year, reporter_iso, partner_iso, commodity_code, trade_value_usd_exp) %>% 
      inner_join(
        tar %>% 
          select(year, partner_iso = reporter_iso, commodity_code, mfn = simple_average),
        by = c("year", "partner_iso", "commodity_code")
      )
    
    trd <- trd %>%
      select(year, reporter_iso, partner_iso, trade_value_usd_exp, mfn) %>%
      collect() %>%
      group_by(year, reporter_iso, partner_iso) %>%
      summarise(
        mfn = weighted.mean(mfn, trade_value_usd_exp, na.rm = T)
      ) %>%
      ungroup()
    
    rm(tar)
    
    d <- d %>% inner_join(trd); rm(trade)
  }
  
  wt_sim$inc(1)
  
  gc()
  
  print(c(raw_lhs_sim(), raw_rhs_sim()))
  
  return(
    # this is not elegant, but works well with polynomials, logs, etc in formulas
    d[,
      colnames(d) %in% 
        c("year", "reporter_iso", "partner_iso",
          lhs_sim(), rhs_sim(), raw_lhs_sim(), raw_rhs_sim(),
          "remoteness_exp", "remoteness_imp", "cluster_pairs"
        )
    ]
  )
}) %>% 
  bindCache(
    inp_sim_y(), inp_sim_riso(), inp_sim_piso(), inp_sim_type(), inp_sim_zero(),
    inp_sim_convert_dollars(), inp_sim_cluster(), inp_sim_product_filter(),
    fml_sim(), lhs_sim(), rhs_sim(), raw_lhs_sim(), raw_rhs_sim()
  ) %>%
  bindEvent(input$sim_go)

df_dtl_2_sim <- eventReactive(input$sim_go, {
  ### 3.8. join with custom data ----
  
  if (nrow(sim_custom_data()) > 0) {
    d <- df_dtl_sim() %>% inner_join(sim_custom_data())
  } else {
    d <- df_dtl_sim()
  }
  
  return(d)
})

## 4. Fit model ----

fit_sim <- eventReactive(input$sim_go, {
  print("Fitting model...")
  
  fml <- as.formula(fml_sim())
  
  if (any(inp_sim_type() %in% c("ols", "olsrem", "olsfe"))) {
    if (inp_sim_cluster() == "yes") {
      m <- tryCatch(
        feols(fml, df_dtl_2_sim(), cluster = ~cluster_pairs),
        error = function(e) { custom_regression_error() }
      )
    } else {
      m <- tryCatch(
        feols(fml, df_dtl_2_sim()),
        error = function(e) { custom_regression_error() }
      )
    }
  }
  
  if (inp_sim_type() == "ppml") {
    if (inp_sim_cluster() == "yes") {
      m <- tryCatch(
        feglm(fml, df_dtl_2_sim(), cluster = ~cluster_pairs, family = quasipoisson(link = "log")),
        error = function(e) { custom_regression_error() }
      )
    } else {
      m <- tryCatch(
        feglm(fml, df_dtl_2_sim(), family = quasipoisson(link = "log")),
        error = function(e) { custom_regression_error() }
      )
    }
  }
  
  wt_sim$inc(2)
  gc()
  
  wt_sim$close() 
  return(m)
})
