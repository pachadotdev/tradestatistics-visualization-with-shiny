#' @title Available Server parameters expressed as functions
available_formats <- function() {
  c("csv", "tsv", "xlsx", "sav", "dta")
}

#' @title Open SQL connection
open_con <- function() {
  dbConnect(
    drv = Postgres(),
    dbname = Sys.getenv("TRADESTATISTICS_SQL_NAME"),
    host = Sys.getenv("TRADESTATISTICS_SQL_HOST"),
    user = Sys.getenv("TRADESTATISTICS_SQL_USER"),
    password = Sys.getenv("TRADESTATISTICS_SQL_PASSWORD"),
    port = Sys.getenv("TRADESTATISTICS_SQL_PORT")
  )
}

#' @title Close SQL connection
#' @param con SQL connection (PostgreSQL)
close_con <- function(con) {
  if (!is.null(con) && dbIsValid(con)) {
    dbDisconnect(con)
  }
}

# TIME ----

#' @title Get Current Year
get_year <- function() {
  as.numeric(format(Sys.Date(), "%Y"))
}

#' @title Correct the year slider (and bookmarkable URL) to a valid range
#' @description `updateSliderInput()` alone fixes the slider's visible
#'   position/badge, but the "y" parameter mirrored into the URL by
#'   `syncUrl()` (see app_server.R) only updates from real client-side
#'   interaction with the slider - not from a server-pushed correction like
#'   this one - so left alone it would keep showing the raw, out-of-range
#'   selection even after the slider itself has been corrected. This sends an
#'   extra small custom message (handled by the inline script added in
#'   `add_external_resources()`, app_ui.R) that fixes the URL query string to
#'   match.
#' @param session module session (its `$ns()` is used to namespace `inputId`)
#' @param inputId slider input id, unnamespaced (e.g. `"y"`)
#' @param min,max,value new slider bounds/value, as for `updateSliderInput()`
sync_year_slider <- function(session, inputId, min, max, value) {
  updateSliderInput(session, inputId, min = min, max = max, value = value)
  id <- if (is.function(session[["ns"]])) session$ns(inputId) else inputId
  session$sendCustomMessage("tabler-syncYearUrl", list(id = id, value = value))
}

# DATA VALIDATION ----

#' @title Ensure data frame contains expected columns to avoid mutate/group_by errors
#' @param df input data frame
#' @param cols vector of expected column names
ensure_cols <- function(df, cols) {
  for (c in cols) {
    if (!c %in% names(df)) df[[c]] <- NA_character_
  }
  df
}

# ORIGIN/DESTINATION TREEMAPS -----

#' @title Origin-Destination Treemap
#' @param d input dataset for values
#' @param d2 input dataset for colours
#' @param title title for the treemap
od_treemap <- function(d, d2, title = NULL) {
  d <- setDT(copy(d))
  d2 <- setDT(copy(d2))

  if (nrow(d) == 0L || nrow(d2) == 0L || !("continent_name" %in% names(d))) {
    return(NULL)
  }

  d$continent_name <- factor(d$continent_name, levels = d2$continent_name)
  setorder(d, continent_name)

  # Compute level order by total trade value
  lvl_dt <- d[, .(trade_value = sum(trade_value, na.rm = TRUE)), by = .(continent_name, country_name)]
  lvl_dt$continent_name <- as.character(lvl_dt$continent_name)
  setorder(lvl_dt, -trade_value)
  new_lvls <- unique(lvl_dt$continent_name)

  # Build ordered colors vector
  d2_ord <- copy(d2)
  d2_ord$continent_name <- factor(d2_ord$continent_name, levels = new_lvls)
  setorder(d2_ord, continent_name)
  new_colors <- d2_ord$country_color

  # Aggregate dd and join colors
  dd <- d[, .(trade_value = sum(trade_value, na.rm = TRUE)), by = .(continent_name, country_name)]
  colors_join <- copy(d2)[, .(continent_name, color = country_color)]
  dd <- merge(dd, colors_join, by = "continent_name", all.x = TRUE)

  d3po(dd) |>
    po_treemap(
      daes(
        size = .data$trade_value,
        group = .data$continent_name,
        subgroup = .data$country_name,
        color = .data$color,
        tiling = "binary"
      )
    ) |>
    po_labels(
      align = "left-top",
      title = title,
      subtitle = JS(
        "function(_v, row) { if (row && row.mode === 'drilled') return 'Displaying Countries'; return 'Displaying Continents'; }"
      ),
      labels = JS(
        "function(percentage, row) {
          var pct = (percentage).toFixed(2) + '%';

          function stripZeros(s) {
            if (s.slice(-3) === '.00') return s.slice(0, -3);
            if (s.slice(-2) === '.0') return s.slice(0, -2);
            return s;
          }

          function formatBillion(v) {
            var s = (Number(v) / 1e9).toFixed(2);
            return stripZeros(s) + 'B';
          }

          var group = (row && (row.group || row.continent_name || row.name)) ? (row.group || row.continent_name || row.name) : '';
          var subgroup = (row && (row.subgroup || row.country_name)) ? (row.subgroup || row.country_name) : '';
          var rawValue = row && (row.trade_value != null ? row.trade_value : (row.count != null ? row.count : (row.value != null ? row.value : '')));
          var value = formatBillion(rawValue);

          if (!row || !subgroup) {
            return group + '<br/>' + value + '<br/>' + pct;
          }

          return subgroup + '<br/>' + value + '<br/>' + pct;
        }"
      )
    ) |>
    po_tooltip(JS(
      "function(percentage, row) {
        var pct = (percentage).toFixed(2) + '%';

        var forceBillions = true;
        function formatNumber(v) {
          if (v === null || v === undefined || v === '') return '';
          var n = Number(v);
          if (isNaN(n)) return String(v);
          var abs = Math.abs(n);
          function stripZeros(s) {
            if (s.slice(-3) === '.00') return s.slice(0, -3);
            if (s.slice(-2) === '.0') return s.slice(0, -2);
            return s;
          }
          if (forceBillions) {
            var s = (n / 1e9).toFixed(2);
            return stripZeros(s) + 'B';
          }
          if (abs >= 1e9) { var s = (n / 1e9).toFixed(2); return stripZeros(s) + 'B'; }
          if (abs >= 1e6) { var s = (n / 1e6).toFixed(2); return stripZeros(s) + 'M'; }
          if (abs >= 1e3) { var s = (n / 1e3).toFixed(1); return stripZeros(s) + 'k'; }
          return n.toLocaleString(undefined, {maximumFractionDigits: 2});
        }

        var group = (row && (row.group || row.continent_name || row.name)) ? (row.group || row.continent_name || row.name) : '';
        var subgroup = (row && (row.subgroup || row.country_name)) ? (row.subgroup || row.country_name) : '';
        var raw = row && (row.trade_value != null ? row.trade_value : (row.count != null ? row.count : (row.value != null ? row.value : '')));
        var value = formatNumber(raw);

        if (!row || !subgroup) {
          return '<b>' + group + '</b><br/>Value: ' + value + '<br/>Percentage: ' + pct;
        }

        return '<b>' + subgroup + '</b><br/>Value: ' + value + '<br/>Percentage: ' + pct;
      }"
    ))
}

# SECTOR TREEMAPS ----

#' @title Add Percentages to Sections
#' @param d input dataset
#' @param col column to collapse
#' @param con SQL connection
se_aggregate_by_sector <- function(d, col, con) {
  d <- setDT(copy(d))
  d <- d[, c("industry_id", "broad_sector_id", col), with = FALSE]
  setnames(d, col, "trade_value")

  d <- se_aggregate_sectors(d, con = con)

  d <- d[, .(trade_value = sum(trade_value, na.rm = TRUE)), by = .(broad_sector, commodity_name)]

  d[, sum_trade_value := sum(trade_value, na.rm = TRUE), by = .(broad_sector)]
  setorder(d, -sum_trade_value)
  d[, sum_trade_value := NULL]

  sections <- unique(d[["broad_sector"]])
  if (length(sections) == 0L) {
    return(d)
  }
  d <- rbindlist(lapply(sections, function(s) {
    setorder(d[broad_sector == s], -trade_value)
  }))

  return(d)
}

#' @title Colorize Sectors
#' @param d input dataset
#' @param con SQL connection
se_colors <- function(d, con) {
  d <- setDT(copy(d))
  if (!("broad_sector" %in% names(d)) || nrow(d) == 0L) {
    return(data.table())
  }
  sectors <- unique(d[["broad_sector"]])
  colors_ref <- setDT(dbGetQuery(
    con,
    "SELECT s.broad_sector AS broad_sector, c.colour AS sector_color
     FROM itpd_sectors s JOIN itpd_colours c ON s.broad_sector_id = c.broad_sector_id"
  ))
  colors_ref[broad_sector %in% sectors]
}

#' @title Aggregate Sectors
#' @param d input dataset
#' @param con SQL connection
se_aggregate_sectors <- function(d, con) {
  industries_ref <- setDT(dbGetQuery(
    con,
    "SELECT industry_id, industry_descr AS commodity_name FROM itpd_industries"
  ))
  sectors_ref <- setDT(dbGetQuery(
    con,
    "SELECT broad_sector_id, broad_sector AS broad_sector FROM itpd_sectors"
  ))
  colours_ref <- setDT(dbGetQuery(
    con,
    "SELECT broad_sector_id, colour AS sector_color FROM itpd_colours"
  ))

  d <- setDT(copy(d))
  d <- merge(d, industries_ref, by = "industry_id")
  d <- merge(d, sectors_ref, by = "broad_sector_id")
  d <- merge(d, colours_ref, by = "broad_sector_id")
  d <- d[, .(trade_value = sum(trade_value, na.rm = TRUE)),
    by = .(industry_id, broad_sector_id, sector_color, broad_sector, commodity_name)
  ]
  return(d)
}

#' @title Sector Treemap
#' @param d input dataset for values
#' @param d2 input dataset for colours
#' @param title title for the treemap
se_treemap <- function(d, d2, title = NULL) {
  d <- setDT(copy(d))
  d2 <- setDT(copy(d2))

  if (nrow(d) == 0L || nrow(d2) == 0L || !("broad_sector" %in% names(d))) {
    return(NULL)
  }

  d$broad_sector <- factor(d$broad_sector, levels = d2$broad_sector)
  setorder(d, broad_sector)

  # Compute new level order by total trade value per section
  lvl_dt <- d[, .(trade_value = sum(trade_value, na.rm = TRUE)), by = .(broad_sector, commodity_name)]
  lvl_dt$broad_sector <- as.character(lvl_dt$broad_sector)
  setorder(lvl_dt, -trade_value)
  new_lvls <- unique(lvl_dt$broad_sector)

  # Build ordered colors vector
  d2_ord <- copy(d2)
  d2_ord$broad_sector <- factor(d2_ord$broad_sector, levels = new_lvls)
  setorder(d2_ord, broad_sector)
  new_colors <- d2_ord$sector_color

  # Aggregate dd and join colors
  dd <- d[, .(trade_value = sum(trade_value, na.rm = TRUE)), by = .(broad_sector, commodity_name)]
  colors_join <- copy(d2)[, .(broad_sector, color = sector_color)]
  dd <- merge(dd, colors_join, by = "broad_sector", all.x = TRUE)

  d3po(dd) |>
    po_treemap(
      daes(
        size = .data$trade_value,
        group = .data$broad_sector,
        subgroup = .data$commodity_name,
        color = .data$color,
        tiling = "binary"
      )
    ) |>
    po_labels(
      align = "left-top",
      title = title,
      subtitle = JS(
        "function(_v, row) {
          if (row && row.mode === 'drilled') {
            return 'Displaying Industries';
          } else {
            return 'Displaying Sectors';
          }
        }"
      ),
      labels = JS(
        "function(percentage, row) {
          // format percentage with two decimals
          var pct = (percentage).toFixed(2) + '%';

          function stripZeros(s) {
            if (s.slice(-3) === '.00') return s.slice(0, -3);
            if (s.slice(-2) === '.0') return s.slice(0, -2);
            return s;
          }

          function formatBillion(v) {
            var s = (Number(v) / 1e9).toFixed(2);
            return stripZeros(s) + 'B';
          }

          // prefer row.group/row.subgroup fields from the d3po/po_treemap internals
          var section = (row && (row.group || row.sector_name || row.name)) ? (row.group || row.sector_name || row.name) : '';
          var commodity = (row && (row.subgroup || row.commodity_name)) ? (row.subgroup || row.commodity_name) : '';
          var rawValue = row && (row.trade_value != null ? row.trade_value : (row.count != null ? row.count : (row.value != null ? row.value : '')));
          var value = formatBillion(rawValue);

          // If no subgroup present (level 1), show only the section
          if (!row || !commodity) {
            return section + '<br/>' + value + '<br/>' + pct;
          }

          // Level 2: show commodity only (not repeated section + commodity)
          return commodity + '<br/>' + value + '<br/>' + pct;
        }"
      )
    ) |>
    po_tooltip(JS(
      "function(percentage, row) {
        var pct = (percentage).toFixed(2) + '%';

        // tooltip formatter: duplicate the same formatter used by labels to avoid R escaping issues
        var forceBillions = true;
        function formatNumber(v) {
          if (v === null || v === undefined || v === '') return '';
          var n = Number(v);
          if (isNaN(n)) return String(v);
          var abs = Math.abs(n);
          function stripZeros(s) {
            if (s.slice(-3) === '.00') return s.slice(0, -3);
            if (s.slice(-2) === '.0') return s.slice(0, -2);
            return s;
          }
          if (forceBillions) {
            var s = (n / 1e9).toFixed(2);
            return stripZeros(s) + 'B';
          }
          if (abs >= 1e9) { var s = (n / 1e9).toFixed(2); return stripZeros(s) + 'B'; }
          if (abs >= 1e6) { var s = (n / 1e6).toFixed(2); return stripZeros(s) + 'M'; }
          if (abs >= 1e3) { var s = (n / 1e3).toFixed(1); return stripZeros(s) + 'k'; }
          return n.toLocaleString(undefined, {maximumFractionDigits: 2});
        }

        // prefer row.group/row.subgroup fields from the d3po/po_treemap internals
        var section = (row && (row.group || row.sector_name || row.name)) ? (row.group || row.sector_name || row.name) : '';
        var commodity = (row && (row.subgroup || row.commodity_name)) ? (row.subgroup || row.commodity_name) : '';
        var raw = row && (row.trade_value != null ? row.trade_value : (row.count != null ? row.count : (row.value != null ? row.value : '')));
        var value = formatNumber(raw);

        // If no subgroup present (level 1), show only the section
        if (!row || !commodity) {
          return '<b>' + section + '</b><br/>Value: ' + value + '<br/>Percentage: ' + pct;
        }

        // Level 2: show commodity only (not repeated section + commodity)
        return '<b>' + commodity + '</b><br/>Value: ' + value + '<br/>Percentage: ' + pct;
      }"
    ))
}

# PARTNER RANKING BAR CHARTS ----

#' @title Top-partner bar chart (top partners + optional highlighted partner + rest of the world)
#' @description Builds a bar chart of a reporter's top trading partners. With `highlight_code`
#' left as `NULL` this is a plain top 4 + "Rest of the world" breakdown (multilateral view). When
#' `highlight_code` is supplied (bilateral view), it shows the top 3 *other* partners plus the
#' selected partner shown separately (labelled "(selected)"), so it can be compared against the
#' top partners regardless of its own rank.
#' @param d input data.table with columns `partner` (dynamic country code) and `trade` (raw trade value)
#' @param title chart title
#' @param bar_color bar color
#' @param highlight_code dynamic code of the selected partner to always show separately, or `NULL`
#' @param lookup_dt data.table with columns `code`/`country`, as built from `flatten_countries()`
partner_top_chart <- function(d, title, bar_color, highlight_code = NULL, lookup_dt) {
  d <- setDT(copy(d))
  d <- d[, .(trade = sum(trade, na.rm = TRUE)), by = .(partner)]
  d <- d[trade > 0]
  if (nrow(d) == 0L) {
    return(NULL)
  }
  setorder(d, -trade)
  # true rank among ALL partners, captured before collapsing the rest into
  # "REST" - so the highlighted partner (and the top-N ones) keep their
  # real position (e.g. 6/14/25) instead of their position within the
  # kept subset (which was always 1-4).
  d[, rank := .I]

  if (!is.null(highlight_code) && highlight_code %in% d$partner) {
    top_n <- d[partner != highlight_code][seq_len(min(3L, .N)), partner]
    keep <- c(top_n, highlight_code)
  } else {
    keep <- d[seq_len(min(4L, .N)), partner]
  }
  ranks <- d[partner %in% keep, .(partner, rank)]

  d[!(partner %in% keep), partner := "REST"]
  d <- d[, .(trade = sum(trade, na.rm = TRUE)), by = .(partner)]
  d <- merge(d, lookup_dt, by.x = "partner", by.y = "code", all.x = TRUE)
  d[partner == "REST", country := "Rest of the world"]
  d[is.na(country), country := partner]
  if (!is.null(highlight_code) && highlight_code %in% keep) {
    d[partner == highlight_code, country := paste0(country, " (selected)")]
  }
  d <- merge(d, ranks, by = "partner", all.x = TRUE)
  d[partner == "REST", rank := length(keep) + 1L]

  rest <- d[country == "Rest of the world"]
  others <- d[country != "Rest of the world"][order(-trade)]
  d <- rbindlist(list(rest, others), fill = TRUE)
  # Zero-pad the rank so d3po's "asc-y" sort (which sorts the y-axis
  # category labels alphabetically, as strings) matches numeric order -
  # otherwise e.g. "10 - Morocco" sorts between "1 - France" and
  # "2 - Germany" since "1" < "10" < "2" lexicographically. "Rest of the
  # world" is left unprefixed since it isn't a real rank.
  d[country != "Rest of the world", country := paste(sprintf("%03d", rank), country, sep = " - ")]
  d[, trade := round(trade / 1e9, 2)]
  d[, rank := NULL]
  d[, color := bar_color]

  d3po(d) |>
    po_bar(
      daes(
        y = .data$country,
        x = .data$trade,
        color = .data$color,
        sort = "asc-y"
      )
    ) |>
    po_labels(
      title = title,
      y = "Country",
      x = "Trade Value (USD billion)"
    ) |>
    po_format(
      x = format(.data$trade, big.mark = " ", scientific = FALSE, digits = 2),
      y = gsub("^0", "", gsub("^0", "", .data$country))
    ) |>
    # po_tooltip("{country}: {trade} billion")
    po_tooltip(JS(
      "function(percentage, row) {
          var country = row && row.country ? row.country.replace(/^\\d+\\s*-\\s*/, '') : '';
          var trade = row && row.trade != null ? row.trade : '';
          return country + ': ' + trade + ' billion';
        }"
    ))
}

#' @title Add definite article for reporter names
#' @description Grammar helper function that adds "the" for reporter names such as
#' "United Kingdom" and "United States"
#' @param name Character string of the reporter name
#' @return Character string: "the" for names requiring the article, empty string otherwise
r_add_the <- function(name = NULL) {
  # Return 'the' for reporter names that typically take the article
  if (is.null(name)) {
    return("")
  }
  if (substr(name, 1, 6) == "United" || substr(name, 1, 3) == "USA" || substr(name, 1, 7) == "Russian") {
    return("the")
  }
  ""
}

#' @title Add capitalized definite article for reporter names
#' @description Grammar helper function that adds "The" (capitalized) for reporter names that
#'  typically take the definite article, used at the beginning of sentences.
#' @param name Character string of the reporter name
#' @return Character string: "The" for names requiring the article, empty string otherwise
r_add_upp_the <- function(name = NULL) {
  v <- r_add_the(name)
  if (nchar(v) == 0) {
    return("")
  }
  # Capitalize only the first letter ("The") rather than returning all caps
  paste0(toupper(substr(v, 1, 1)), tolower(substr(v, 2, nchar(v))))
}

# FORMAT TEXTS ----

#' @title Format for Dollars
#' @param x input number
show_dollars <- function(x) {
  ifelse(x %/% 10e8 >= 1,
    paste0(round(x / 10e8, 2), "B"),
    paste0(round(x / 10e5, 2), "M")
  )
}

#' @title Format for Percentages
#' @param x input number
show_percentage <- function(x) {
  paste0(round(100 * x, 2), "%")
}

#' @title Compute Compound Annualized Growth Rate
#' @param p final value
#' @param q initial value
#' @param t time period
growth_rate <- function(p, q, t) {
  (p / q)^(1 / (max(t) - min(t))) - 1
}

# COUNTRY LOOKUPS ----

#' @title Flatten the region-grouped countries data into a single name -> code vector
#' @description `tradestatisticsdashboard::countries` is a list of named vectors
#' (one per region, name = country, value = dynamic code), used to build grouped
#' select inputs. This collapses it into one flat named vector for lookups.
flatten_countries <- function() {
  do.call(c, unname(tradestatisticsdashboard::countries))
}

#' @title Look up a country's display name from its dynamic code
#' @param code Dynamic country code (e.g. "GBR")
#' @param lookup Flat name -> code vector, as returned by `flatten_countries()`
#' @return The country name, or `code` itself if not found
country_name_from_code <- function(code, lookup) {
  if (is.null(code) || length(code) == 0 || is.na(code) || nchar(code) == 0) {
    return(code)
  }
  out <- names(lookup)[match(code, lookup)]
  if (length(out) == 0 || is.na(out[1]) || nchar(out[1]) == 0) {
    return(code)
  }
  out[1]
}

# NARRATIVE TEXT (BACKGROUND / SANCTIONS) ----

#' @title Join a vector of items into a human-readable "a, b and c" list
#' @param x list or vector (e.g., \code{letters[1:3]})
format_list_and <- function(x) {
  x <- unique(x)
  x <- x[!is.na(x) & nchar(x) > 0]
  if (length(x) == 0) {
    return("")
  }
  if (length(x) == 1) {
    return(x)
  }
  if (length(x) == 2) {
    return(paste(x, collapse = " and "))
  }
  paste0(paste(x[-length(x)], collapse = ", "), " and ", x[length(x)])
}

#' @title Human-readable labels for GSDB sanction objectives
sanction_objective_labels <- c(
  obj_democracy = "Promoting democracy",
  obj_destab_regime = "Destabilizing the government",
  obj_end_war = "Ending a war",
  obj_human_rights = "Addressing human rights violations",
  obj_policy_change = "Policy change",
  obj_prevent_war = "Preventing war",
  obj_territorial_conflict = "Resolving a territorial conflict",
  obj_terrorism = "Countering terrorism",
  obj_other = "Other objectives"
)

#' @title Fetch GSDB dyadic sanctions imposed against a country in a year range
#' @param con SQL connection
#' @param code Dynamic code of the sanctioned country
#' @param years Numeric vector of years to filter on (min/max are used)
fetch_sanctions <- function(con, code, years) {
  if (is.null(code) || is.na(code) || nchar(code) == 0) {
    return(data.table())
  }
  e <- gsub("'", "''", code)
  min_yr <- as.integer(min(years))
  max_yr <- as.integer(max(years))
  setDT(dbGetQuery(con, sprintf(
    "SELECT case_id, year, sanctioning_state_dynamic, trade, financial,
            obj_democracy, obj_destab_regime, obj_end_war, obj_human_rights,
            obj_policy_change, obj_prevent_war, obj_territorial_conflict,
            obj_terrorism, obj_other,
            suc_failed, suc_nego_settlement, suc_ongoing, suc_success_part, suc_success_total
     FROM gsdb_dyadic
     WHERE sanctioned_state_dynamic = '%s' AND year BETWEEN %d AND %d",
    e, min_yr, max_yr
  )))
}

#' @title Format a year, or a range of years, as "2017" or "2017-2022"
#' @param a start year
#' @param b end year
format_year_range <- function(a, b) {
  if (a == b) as.character(a) else paste0(a, "-", b)
}

#' @title Compute contiguous year ranges where a 0/1 flag column is active
#' @description Used to turn a per-year membership indicator (e.g. WTO/EU
#'   membership) into human-readable periods such as `"1995-2022"`, handling
#'   gaps by returning one range per contiguous run of years. `d` may contain
#'   duplicate rows per year (e.g. one row per trade partner); these are
#'   collapsed to a single flag per year before computing ranges.
#' @param d data.table with a `year` column and the flag column
#' @param col name of the 0/1 flag column to summarize
year_ranges <- function(d, col) {
  vals <- d[[col]]
  vals[is.na(vals)] <- 0
  yearly <- data.table(year = d$year, flag = vals)[, .(flag = max(flag)), by = year]
  yearly <- yearly[flag == 1]
  if (nrow(yearly) == 0) {
    return(character(0))
  }
  yearly <- yearly[order(year)]
  grp <- cumsum(c(1L, diff(yearly$year) != 1L))
  ranges <- yearly[, .(start = min(year), end = max(year)), by = grp]
  vapply(seq_len(nrow(ranges)), function(i) format_year_range(ranges$start[i], ranges$end[i]), character(1))
}

#' @title Build an HTML narrative describing sanctions applied against a country
#' @description Groups sanction cases by type (trade/financial) and stated
#'   objective, listing the sanctioning countries with the years the sanction
#'   was active and its outcome.
#' @param d data.table as returned by `fetch_sanctions()`
#' @param country_name Display name of the sanctioned country (article included, e.g. "the United Kingdom")
#' @param lookup Flat name -> code vector, as returned by `flatten_countries()`, used to name the senders
sanctions_narrative <- function(d, country_name, lookup) {
  if (is.null(d) || nrow(d) == 0) {
    return("")
  }

  obj_cols <- intersect(names(sanction_objective_labels), names(d))

  case_outcome <- function(row) {
    if (isTRUE(row$suc_success_total == 1)) {
      "successful"
    } else if (isTRUE(row$suc_success_part == 1)) {
      "partially successful"
    } else if (isTRUE(row$suc_nego_settlement == 1)) {
      "negotiated settlement"
    } else if (isTRUE(row$suc_ongoing == 1)) {
      "ongoing"
    } else if (isTRUE(row$suc_failed == 1)) {
      "failed"
    } else {
      "undetermined outcome"
    }
  }

  # Lower rank = takes precedence when the same sender/year is covered by more
  # than one case with a different outcome.
  outcome_priority <- c(
    "successful" = 1, "partially successful" = 2, "negotiated settlement" = 3,
    "ongoing" = 4, "failed" = 5, "undetermined outcome" = 6
  )

  # Merge one sender's per-year outcomes into "year_range, outcome" entries,
  # combining across cases (contiguous years with the same outcome collapse
  # into a single range, even if they came from different sanction cases).
  sender_entries <- function(rows) {
    rows <- copy(rows)
    rows[, outcome := vapply(seq_len(.N), function(i) case_outcome(rows[i]), character(1))]
    rows[, priority := outcome_priority[outcome]]
    setorder(rows, priority)
    rows <- unique(rows, by = "year")
    setorder(rows, year)
    n <- nrow(rows)
    grp <- cumsum(c(TRUE, diff(rows$year) != 1L | rows$outcome[-1] != rows$outcome[-n]))
    ranges <- rows[, .(start = min(year), end = max(year), outcome = outcome[1]), by = grp]
    vapply(seq_len(nrow(ranges)), function(i) {
      paste0(format_year_range(ranges$start[i], ranges$end[i]), ", ", ranges$outcome[i])
    }, character(1))
  }

  build_section <- function(type_col, type_label) {
    sub <- d[get(type_col) == 1]
    if (nrow(sub) == 0) {
      return(NULL)
    }

    items <- list()
    for (col in obj_cols) {
      rows <- sub[get(col) == 1]
      if (nrow(rows) == 0) {
        next
      }
      senders <- unique(rows$sanctioning_state_dynamic)
      entries <- unlist(lapply(senders, function(sender) {
        sender_rows <- rows[sanctioning_state_dynamic == sender]
        sender_name <- country_name_from_code(sender, lookup)
        sender_name <- gsub("\\s+\\(*.ISO.*\\)", "", sender_name)
        paste0(sender_name, " (", sender_entries(sender_rows), ")")
      }))
      items[[length(items) + 1]] <- tags$li(
        tags$strong(sanction_objective_labels[[col]]), ": ", paste(entries, collapse = ", ")
      )
    }

    if (length(items) == 0) {
      return(NULL)
    }

    tagList(
      tags$p(tags$strong(type_label)),
      do.call(tags$ul, items)
    )
  }

  trade_section <- build_section("trade", "Trade sanctions")
  financial_section <- build_section("financial", "Financial sanctions")

  if (is.null(trade_section) && is.null(financial_section)) {
    return("")
  }

  parts <- list(tags$p(paste0(country_name, " was under the following sanctions (the data sources cover up to 2023 for sanctions):")))
  if (!is.null(trade_section)) parts[[length(parts) + 1]] <- trade_section
  if (!is.null(financial_section)) parts[[length(parts) + 1]] <- financial_section

  as.character(do.call(tagList, parts))
}

#' @title Build an HTML narrative with population, GDP per capita, WTO/EU
#'   membership periods and bilateral gravity context
#' @param reporter_name Display name of the reporter country (article included)
#' @param reporter_info Single-row data.table from the `dgd` reporter-context query
#' @param membership_info data.table with one row per year (`year`, `member_wto_o`,
#'   `member_eu_o`) over the selected range, used to compute membership periods
#' @param max_year Latest year in the user-selected range, used to flag when the
#'   population/GDP figures come from an earlier year than requested
#' @param partner_name Display name of the partner country (article included), or `NULL` for multilateral profiles
#' @param bilateral_info Single-row data.table from the `dgd` bilateral-context query, or `NULL`
gravity_context_narrative <- function(reporter_name, reporter_info, membership_info = NULL, max_year = NULL,
                                      partner_name = NULL, bilateral_info = NULL) {
  parts <- list()

  if (!is.null(reporter_info) && nrow(reporter_info) > 0) {
    yr <- reporter_info$year[1]
    pop <- reporter_info$pop_o[1]
    gdp_cap <- reporter_info$gdp_wdi_cap_const_o[1]

    if (!is.na(pop) && !is.na(gdp_cap)) {
      pop_txt <- paste0(format(round(pop, 1), nsmall = 1), " million")
      gdp_cap_txt <- paste0("$", formatC(round(gdp_cap), format = "d", big.mark = ","))
      sentence <- glue("As of { yr }, { reporter_name } had a population of { pop_txt } and a GDP per capita of { gdp_cap_txt }. { yr } is the last year with population and GDP data in the sources.")
      sentence <- gsub(", The", ", the", sentence)
      parts[[length(parts) + 1]] <- tags$p(sentence)
    }
  }

  if (!is.null(membership_info) && nrow(membership_info) > 0) {
    wto_ranges <- year_ranges(membership_info, "member_wto_o")
    eu_ranges <- year_ranges(membership_info, "member_eu_o")

    membership_parts <- character(0)
    if (length(wto_ranges) > 0) {
      membership_parts <- c(membership_parts, glue("the World Trade Organization (WTO) for the period { format_list_and(wto_ranges) }"))
    }
    if (length(eu_ranges) > 0) {
      membership_parts <- c(membership_parts, glue("the European Union (EU) for the period { format_list_and(eu_ranges) }"))
    }
    if (length(membership_parts) > 0) {
      parts[[length(parts) + 1]] <- tags$p(glue("{ reporter_name } was a member of { format_list_and(membership_parts) }. 2019 is the last year with memberships data in the sources."))
    }
  }

  if (!is.null(bilateral_info) && nrow(bilateral_info) > 0 && !is.null(partner_name)) {
    bilateral_sentence <- ""
    dist <- bilateral_info$distance[1]
    if (!is.na(dist)) {
      dist_txt <- paste0(formatC(round(dist), format = "d", big.mark = ","), " km")
      bilateral_sentence <- paste0(bilateral_sentence, glue("{ reporter_name } and { partner_name } are separated by a distance of approximately { dist_txt }."))
    }
    if (isTRUE(bilateral_info$contiguity[1] == 1)) {
      bilateral_sentence <- paste0(bilateral_sentence, " The two countries share a land border.")
    }
    if (isTRUE(bilateral_info$common_language[1] == 1)) {
      bilateral_sentence <- paste0(bilateral_sentence, " They also share a common official language.")
    }
    if (isTRUE(bilateral_info$colony_ever[1] == 1)) {
      bilateral_sentence <- paste0(bilateral_sentence, glue(" { reporter_name } and { partner_name } have a shared colonial history."))
    }
    if (isTRUE(bilateral_info$common_colonizer[1] == 1)) {
      bilateral_sentence <- paste0(bilateral_sentence, " Both countries were once part of the same colonial empire.")
    }
    if (nchar(trimws(bilateral_sentence)) > 0) {
      parts[[length(parts) + 1]] <- tags$p(trimws(bilateral_sentence))
    }
  }

  if (length(parts) == 0) {
    return("")
  }

  as.character(do.call(tagList, parts))
}

#' @title Typing reactiveValues is too long
#' @param ... elements to pass to the function
#' @rdname reactives
rv <- function(...) tabler::reactiveValues(...)

#' @rdname reactives
rvtl <- function(...) tabler::reactiveValuesToList(...)
