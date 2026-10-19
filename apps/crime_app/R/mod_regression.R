# =====================================================================
# R/mod_regression.R  -  Regression tab (Phase 5)
# Reads  : data$regression (list saved by 05_regression.R) and data$districts
# Controls: model (OLS / spatial lag / spatial error), impacts switch
# Shows  : residual Moran's I for each model, residual map, coefficient
#          table, model comparison table, optional direct/indirect effects
# Needs  : theme.R and helpers.R (what_this_shows, stat_card, page_head,
#          crime_cols, lisa_cols)
# No model is refitted here and spatialreg is not needed: coefficients,
# standard errors and residuals are read from the saved objects.
# =====================================================================

# Optional: text the tab owner adds to the end of the "What this shows" box
reg_owner_note <- NULL

# Friendly names for the model terms. Names not listed are shown as saved.
reg_term_labels <- c(
  "(Intercept)"        = "Intercept",
  "pct_informal"       = "Informal dwellings",
  "pct_no_schooling"   = "No schooling",
  "pct_matric_plus"    = "Matric or higher",
  "pct_no_electricity" = "No electricity",
  "pct_urban"          = "Urban share",
  "pct_no_piped_water" = "No piped water",
  "pct_female"         = "Female",
  "pct_youth_15-34"    = "Youth aged 15-34",
  "pct_youth_15_34"    = "Youth aged 15-34",
  "avg_hh_size"        = "Household size",
  "rho"                = "rho (spatial lag term)",
  "lambda"             = "lambda (spatial error term)"
)

reg_models <- c("OLS (no spatial term)" = "ols",
                "Spatial lag"           = "lag",
                "Spatial error"         = "error")
reg_model_names <- c(ols = "OLS", lag = "Spatial lag", error = "Spatial error")

# ---- Helpers (prefixed so they cannot clash with other tabs) ---------------
reg_clean_term <- function(x) {
  x <- gsub("`", "", x)
  ifelse(x %in% names(reg_term_labels), unname(reg_term_labels[x]), x)
}

reg_fmt_p <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
}

reg_fmt <- function(x, digits = 3) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits, big.mark = ","))
}

# Coefficient table for one model, from the saved objects
reg_coef_table <- function(r, model) {
  if (model == "ols") {
    s <- tryCatch(summary(r$ols)$coefficients, error = function(e) NULL)
    if (!is.null(s)) {
      out <- data.frame(Term = rownames(s), Estimate = s[, 1], SE = s[, 2],
                        Stat = s[, 3], p = s[, 4], stringsAsFactors = FALSE)
    } else {
      # Fallback: the saved OLS comparison table has estimates and p-values only
      cc <- as.data.frame(r$coef_compare)
      cc <- cc[!is.na(cc$ols_estimate), , drop = FALSE]
      out <- data.frame(Term = cc$term, Estimate = cc$ols_estimate, SE = NA_real_,
                        Stat = NA_real_, p = cc$ols_p, stringsAsFactors = FALSE)
    }
  } else {
    m <- if (model == "lag") r$lag else r$error
    est <- as.numeric(m$coefficients)
    se  <- as.numeric(m$rest.se)
    out <- data.frame(Term = names(m$coefficients), Estimate = est, SE = se,
                      Stat = est / se, p = 2 * stats::pnorm(-abs(est / se)),
                      stringsAsFactors = FALSE)
    # spatial parameter: rho (lag) or lambda (error)
    sp_name <- if (model == "lag") "rho" else "lambda"
    sp_est  <- unname(as.numeric(if (model == "lag") m$rho else m$lambda))
    sp_se   <- unname(as.numeric(if (model == "lag") m$rho.se else m$lambda.se))
    out <- rbind(out, data.frame(Term = sp_name, Estimate = sp_est, SE = sp_se,
                                 Stat = sp_est / sp_se,
                                 p = 2 * stats::pnorm(-abs(sp_est / sp_se)),
                                 stringsAsFactors = FALSE))
  }
  rownames(out) <- NULL
  out
}

# Residual map: blue = below prediction, amber = above prediction
reg_residual_map <- function(d, res, legend_title) {
  lim <- as.numeric(stats::quantile(abs(res), 0.99, na.rm = TRUE))
  clamped <- pmax(pmin(res, lim), -lim)
  pal <- colorNumeric(c(lisa_cols[["Low-Low"]], crime_cols$text, crime_cols$accent),
                      domain = c(-lim, lim), na.color = crime_cols$border)
  labs <- sprintf("<strong>%s</strong><br/>Residual: %s",
                  htmltools::htmlEscape(d$police_district),
                  formatC(res, format = "f", digits = 2))

  leaflet(d, options = leafletOptions(minZoom = 4)) |>
    addProviderTiles(providers$CartoDB.DarkMatter) |>
    addPolygons(
      fillColor = pal(clamped), fillOpacity = 0.9,
      color = crime_cols$bg, weight = 0.3,
      label = lapply(labs, htmltools::HTML),
      highlightOptions = highlightOptions(color = "#FFFFFF", weight = 2,
                                          bringToFront = TRUE)
    ) |>
    addLegend(position = "bottomright", pal = pal, values = c(-lim, lim),
              title = legend_title, opacity = 0.95)
}

# ---- UI ----------------------------------------------------------------------
mod_regression_ui <- function(id) {
  ns <- NS(id)

  div(class = "page-wrap",
      page_head("Regression",
                "Does a spatial model describe violent crime better than ordinary regression?"),

      layout_sidebar(
        fillable = FALSE,
        sidebar = sidebar(
          width = 300,
          radioButtons(ns("model"), "Model", choices = reg_models, selected = "lag"),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'lag'", ns("model")),
            checkboxInput(ns("show_impacts"),
                          "Show direct, indirect and total effects", value = FALSE)
          ),
          helpText("The outcome is the log of the smoothed violent crime rate. ",
                   "Residuals show what each model gets wrong for each district.")
        ),

        uiOutput(ns("moran_cards")),

        card(card_header(textOutput(ns("map_title"))),
             leafletOutput(ns("map"), height = 480)),

        layout_columns(
          col_widths = c(7, 5),
          card(card_header(textOutput(ns("coef_title"))),
               tableOutput(ns("coef")),
               uiOutput(ns("coef_note"))),
          card(card_header("Model comparison"), tableOutput(ns("compare")))
        ),

        uiOutput(ns("impacts_card")),
        uiOutput(ns("note"))
      )
  )
}

# ---- Server --------------------------------------------------------------------
mod_regression_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    d <- data$districts

    reg <- reactive({
      validate(need(!is.null(data$regression),
                    "Regression results are not available yet."))
      data$regression
    })

    # Open on the model the Phase 5 tests selected
    observeEvent(TRUE, {
      r <- data$regression
      if (!is.null(r) && !is.null(r$final_choice) && r$final_choice %in% c("lag", "error")) {
        updateRadioButtons(session, "model", selected = r$final_choice)
      }
    }, once = TRUE)

    model_key <- reactive({ req(input$model); input$model })

    residuals_of <- reactive({
      r <- reg(); m <- model_key()
      obj <- switch(m, ols = r$ols, lag = r$lag, error = r$error)
      res <- unname(as.numeric(obj$residuals))
      validate(need(length(res) == nrow(d),
                    "The residuals do not match the district data."))
      res
    })

    # ---- Residual Moran's I before and after ---------------------------------
    output$moran_cards <- renderUI({
      r <- reg(); m <- model_key()
      fs <- r$fit_summary
      validate(need(!is.null(fs), "Model comparison is not available."))

      card_for <- function(key) {
        row <- fs[fs$model == reg_model_names[[key]], , drop = FALSE]
        if (nrow(row) == 0) return(NULL)
        stat_card(
          sprintf("Residual Moran's I, %s (p %s)", reg_model_names[[key]],
                  ifelse(row$resid_moran_p < 0.001, "< 0.001",
                         paste("=", formatC(row$resid_moran_p, format = "f", digits = 2)))),
          formatC(row$resid_moran_I, format = "f", digits = 3),
          accent = identical(key, m)
        )
      }
      layout_columns(col_widths = c(4, 4, 4),
                     card_for("ols"), card_for("lag"), card_for("error"))
    })

    # ---- Map -------------------------------------------------------------------
    output$map_title <- renderText({
      paste("Residuals:", reg_model_names[[model_key()]])
    })

    output$map <- renderLeaflet({
      res <- residuals_of()
      reg_residual_map(d, res, "Residual (log scale)")
    })

    # ---- Coefficient table ---------------------------------------------------------
    output$coef_title <- renderText({
      paste("Coefficients:", reg_model_names[[model_key()]])
    })

    output$coef <- renderTable({
      r <- reg(); m <- model_key()
      tab <- reg_coef_table(r, m)
      data.frame(
        Term = reg_clean_term(tab$Term),
        Estimate = reg_fmt(tab$Estimate, 3),
        `Std. error` = reg_fmt(tab$SE, 3),
        `t / z` = reg_fmt(tab$Stat, 2),
        `p-value` = reg_fmt_p(tab$p),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }, striped = TRUE, spacing = "s", width = "100%")

    output$coef_note <- renderUI({
      m <- model_key()
      txt <- "The covariates are proportions from 0 to 1, so divide a coefficient by 10 to read it as the change for a 10 percentage point difference."
      if (m == "lag") {
        txt <- paste(txt, "In the spatial lag model a change in one district also spreads to its neighbours, so use the direct, indirect and total effects for interpretation.")
      }
      helpText(txt)
    })

    # ---- Model comparison ------------------------------------------------------------
    output$compare <- renderTable({
      fs <- reg()$fit_summary
      validate(need(!is.null(fs), "Model comparison is not available."))
      data.frame(
        Model = fs$model,
        AIC = reg_fmt(fs$AIC, 1),
        `Log-likelihood` = reg_fmt(fs$logLik, 1),
        `Residual Moran's I` = reg_fmt(fs$resid_moran_I, 3),
        `p-value` = reg_fmt_p(fs$resid_moran_p),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }, striped = TRUE, spacing = "s", width = "100%")

    # ---- Direct, indirect and total effects (lag model only) -----------------------------
    output$impacts_card <- renderUI({
      r <- reg()
      req(identical(model_key(), "lag"), isTRUE(input$show_impacts))
      validate(need(!is.null(r$impacts), "Effects are not available."))
      card(card_header("Direct, indirect and total effects (spatial lag model)"),
           tableOutput(session$ns("impacts")),
           helpText("Direct = effect on the district's own rate. Indirect = effect passed on through neighbours. Total = direct + indirect."))
    })

    output$impacts <- renderTable({
      imp <- as.data.frame(reg()$impacts, stringsAsFactors = FALSE)
      first <- names(imp)[1]
      if (is.character(imp[[first]]) || is.factor(imp[[first]])) {
        imp[[first]] <- reg_clean_term(as.character(imp[[first]]))
      }
      for (nm in names(imp)[-1]) {
        if (is.numeric(imp[[nm]])) {
          imp[[nm]] <- if (grepl("(^|_)p($|_)|p_?val", nm, ignore.case = TRUE))
            reg_fmt_p(imp[[nm]]) else reg_fmt(imp[[nm]], 3)
        }
      }
      names(imp) <- gsub("_", " ", names(imp))
      names(imp)[1] <- "Variable"
      imp
    }, striped = TRUE, spacing = "s", width = "100%")

    # ---- What this shows -------------------------------------------------------------------
    output$note <- renderUI({
      r <- reg(); m <- model_key()
      fs <- r$fit_summary
      validate(need(!is.null(fs), "Model comparison is not available."))
      pick <- if (m == "ols") r$final_choice else m
      if (is.null(pick) || !pick %in% c("lag", "error")) pick <- "lag"
      ols <- fs[fs$model == "OLS", ]
      sp  <- fs[fs$model == reg_model_names[[pick]], ]
      what_this_shows(
        sprintf("Ordinary regression leaves spatial pattern in its residuals (Moran's I = %s, p %s); the %s model reduces it to %s (p %s). ",
                formatC(ols$resid_moran_I, format = "f", digits = 3),
                ifelse(ols$resid_moran_p < 0.001, "< 0.001",
                       paste("=", formatC(ols$resid_moran_p, format = "f", digits = 2))),
                tolower(reg_model_names[[pick]]),
                formatC(sp$resid_moran_I, format = "f", digits = 3),
                ifelse(sp$resid_moran_p < 0.001, "< 0.001",
                       paste("=", formatC(sp$resid_moran_p, format = "f", digits = 2)))),
        sprintf("AIC is %s for OLS and %s for the %s model (lower is better). These are associations between district averages, not causes.",
                formatC(ols$AIC, format = "f", digits = 0, big.mark = ","),
                formatC(sp$AIC, format = "f", digits = 0, big.mark = ","),
                tolower(reg_model_names[[pick]])),
        reg_owner_note
      )
    })
  })
}
