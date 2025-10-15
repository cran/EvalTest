library(shiny)
library(shinydashboard)
library(pROC)
library(ggplot2)
library(DT)
library(readxl)
library(openxlsx)
library(ggpubr)
library(stats)
library(binom)

ui <- dashboardPage(
  dashboardHeader(
    title = "EvalTest",
    tags$li(
      class = "dropdown",
      tags$span(
        "Evaluation of diagnostic test performance",
        style = "color: white; font-size: 22px; margin-right: 15px; line-height: 50px;"
      )
    ),
    tags$li(class = "dropdown")
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      box(title = "Data import & parameters setting", width = 4, status = "primary", solidHeader = TRUE,
          fileInput("file", "Import data (.xlsx)", accept = ".xlsx"),
          radioButtons("test_type", "Test variable type",
                       choices = c("Qualitative (binary 1/0 as 1 for positive test)" = "qual",
                                   "Quantitative" = "quant"),
                       selected = "qual"),
          selectInput("var_test", "Test variable", choices = NULL),
          selectInput("var_ref", "Reference variable (binary 1/0 as 1 for disease: yes)", choices = NULL),
          numericInput("prev", "Disease prevalence (in the population)", value = 0.1, min = 0, max = 1, step = 0.01),
          actionButton("analyser", "Run analysis", icon = icon("play"), class = "btn-warning"),
          br()
      ),
      box(title = "ROC curve", width = 8, status = "info", solidHeader = TRUE,
          downloadButton("download_plot", "Download plot", class = "btn-success", style = "float: right; margin-top: -5px;"),
          div(style = "height: 439px;",plotOutput("roc_plot", height = "90%"))
      )
    ),
    fluidRow(
      box(title = "Confusion matrix", width = 6, status = "info", solidHeader = TRUE,
          downloadButton("download_report", "Download confusion matrix", class = "btn-success", style = "float: right; margin-top: -5px;"),
          tableOutput("contingence"),
          tableOutput("valeurs_brutes")
      ),
      box(title = "Performance indicators", width = 6, status = "info", solidHeader = TRUE,
          downloadButton("download_indicators", "Download outputs", class = "btn-success", style = "float: right; margin-top: -5px;"),
          DTOutput("res_table")
      )
    ),
    div(
      style = "text-align: center; margin-top: 20px; font-size: 14px; color: #555;",
      HTML("<strong> \u00A9 2025 Nassim AYAD — Licensed under the MIT License.</strong>")
    )
  )
)

server <- function(input, output, session) {
  df <- reactiveVal()

  observeEvent(input$file, {
    req(input$file)
    data <- read_excel(input$file$datapath)
    df(data)
    updateSelectInput(session, "var_test", choices = names(data))
    updateSelectInput(session, "var_ref", choices = names(data))
  })

  results <- eventReactive(input$analyser, {
    req(df(), input$var_test, input$var_ref)
    data <- df()

    ref  <- data[[input$var_ref]]
    test <- data[[input$var_test]]

    if (!all(ref %in% c(0, 1), na.rm = TRUE)) {
      showModal(modalDialog(
        title = "Error",
        "The reference variable must contain only 0 and 1 values.",
        easyClose = TRUE
      ))
      return(NULL)
    }
    if (input$test_type == "qual" && !all(test %in% c(0, 1), na.rm = TRUE)) {
      showModal(modalDialog(
        title = "Error",
        "For qualitative tests, the test variable must contain only 0 and 1 values.",
        easyClose = TRUE
      ))
      return(NULL)
    }
    if (input$test_type == "quant" && !is.numeric(test)) {
      showModal(modalDialog(
        title = "Error",
        "For quantitative tests, the test variable must be numeric.",
        easyClose = TRUE
      ))
      return(NULL)
    }

    cc <- stats::complete.cases(ref, test)
    ref_cc  <- ref[cc]
    test_cc <- test[cc]

    if (length(unique(ref_cc)) < 2) {
      showModal(modalDialog(
        title = "Error",
        "The reference variable must contain both 0 and 1 after removing missing values.",
        easyClose = TRUE
      ))
      return(NULL)
    }

    roc_obj <- pROC::roc(response = ref_cc, predictor = test_cc, quiet = TRUE, direction = "auto")
    best <- pROC::coords(
      roc_obj,
      x = "best",
      best.method = "closest.topleft",
      ret = c("threshold", "sensitivity", "specificity", "tp", "fp", "tn", "fn"),
      transpose = FALSE
    )
    best_cutoff <- as.numeric(best$threshold[1])
    Se <- as.numeric(best$sensitivity[1])
    Sp <- as.numeric(best$specificity[1])

    best_cutoff <- as.numeric(best$threshold[1])
    Se <- as.numeric(best$sensitivity[1])
    Sp <- as.numeric(best$specificity[1])
    TP <- as.integer(best$tp[1])
    FP <- as.integer(best$fp[1])
    TN <- as.integer(best$tn[1])
    FN <- as.integer(best$fn[1])

    Prev <- input$prev
    denom_ppv <- (Se * Prev) + ((1 - Sp) * (1 - Prev))
    denom_npv <- ((1 - Se) * Prev) + (Sp * (1 - Prev))
    PPV <- if (denom_ppv == 0) NA_real_ else (Se * Prev) / denom_ppv
    NPV <- if (denom_npv == 0) NA_real_ else (Sp * (1 - Prev)) / denom_npv
    LRp <- if (Sp == 1) NA_real_ else Se / (1 - Sp)
    LRn <- if (Sp == 0) NA_real_ else (1 - Se) / Sp
    denom_acc <- TP + FP + FN + TN
    Acc <- if (denom_acc == 0) NA_real_ else (TP + TN) / denom_acc
    Youden <- Se + Sp - 1

    binom_ci <- function(x, n, conf = 0.95) {
      if (n == 0) return(c(NA, NA))
      ci <- binom.confint(x, n, conf.level = conf, methods = "wilson")
      c(ci$lower, ci$upper)
    }

    ci_se <- binom_ci(TP, TP + FN)
    ci_sp <- binom_ci(TN, TN + FP)

    ci_acc <- binom_ci(TP + TN, denom_acc)

    ppv_fun <- function(Se, Sp, Prev) {
      denom <- (Se * Prev) + ((1 - Sp) * (1 - Prev))
      if (is.na(denom) || denom == 0) return(NA_real_)
      (Se * Prev) / denom
    }
    npv_fun <- function(Se, Sp, Prev) {
      denom <- ((1 - Se) * Prev) + (Sp * (1 - Prev))
      if (is.na(denom) || denom == 0) return(NA_real_)
      (Sp * (1 - Prev)) / denom
    }

    se_bounds <- ci_se[1:2]
    sp_bounds <- ci_sp[1:2]
    ppv_vals <- outer(se_bounds, sp_bounds,
                      Vectorize(function(se, sp) ppv_fun(se, sp, Prev)))
    npv_vals <- outer(se_bounds, sp_bounds,
                      Vectorize(function(se, sp) npv_fun(se, sp, Prev)))
    ci_ppv <- range(ppv_vals, na.rm = TRUE)
    ci_npv <- range(npv_vals, na.rm = TRUE)

    se_lr_p <- if (!is.na(LRp) && TP > 0 && FP > 0) sqrt((1- Se)/(TP) + Sp/(FP)) else NA_real_
    se_lr_n <- if (!is.na(LRn) && FN > 0 && TN > 0) sqrt(Se/(FN) + (1-Sp)/(TN)) else NA_real_
    ci_lr_p <- if (!is.na(LRp) && !is.na(se_lr_p)) exp(log(LRp) + c(-1,1) * 1.96 * se_lr_p) else c(NA,NA)
    ci_lr_n <- if (!is.na(LRn) && !is.na(se_lr_n)) exp(log(LRn) + c(-1,1) * 1.96 * se_lr_n) else c(NA,NA)

    se_youden <- if ((TP + FN) > 0 && (TN + FP) > 0) sqrt((Se*(1-Se))/(TP+FN) + (Sp*(1-Sp))/(TN+FP)) else NA_real_
    ci_youden <- if (!is.na(se_youden)) Youden + c(-1,1)*1.96*se_youden else c(NA,NA)

    f3 <- function(x) formatC(x, format = "f", digits = 3)
    indicateurs <- data.frame(
      Indicator = c("Sensitivity (Se)", "Specificity (Sp)",
                    "Positive predictive value (PPV)", "Negative predictive value (NPV)",
                    "Positive likelihood ratio (LR+)", "Negative likelihood ratio (LR-)",
                    "Accuracy", "Youden index"),
      Estimate = f3(c(Se, Sp, PPV, NPV, LRp, LRn, Acc, Youden)),
      `95% CI` = paste0("[",
                        f3(c(ci_se[1], ci_sp[1], ci_ppv[1], ci_npv[1],
                                ci_lr_p[1], ci_lr_n[1], ci_acc[1], ci_youden[1])),
                        " – ",
                        f3(c(ci_se[2], ci_sp[2], ci_ppv[2], ci_npv[2],
                                ci_lr_p[2], ci_lr_n[2], ci_acc[2], ci_youden[2])),
                        "]")
    )
    colnames(indicateurs) <- c("Indicator", "Estimate", "95% Confidence Interval")
    list(
      seuil = best_cutoff,
      Se = Se, Sp = Sp,
      TP = TP, FP = FP, FN = FN, TN = TN,
      table = matrix(c(TP, FP, FN, TN), nrow = 2, byrow = TRUE,
                     dimnames = list("Test" = c("Positive", "Negative"),
                                     "Truth" = c("Disease", "No disease"))),
      indicateurs = indicateurs,
      roc = roc_obj
    )
  })

  output$contingence <- renderTable({
    req(results())
    results()$table
  }, rownames = TRUE)

  output$valeurs_brutes <- renderTable({
    req(results())
    data.frame(
      Value = c("True positives (TP)", "False positives (FP)", "False negatives (FN)", "True negatives (TN)"),
      Count = c(results()$TP, results()$FP, results()$FN, results()$TN)
    )
  }, rownames = FALSE)

  output$res_table <- renderDT({
    req(results())
    datatable(results()$indicateurs, options = list(dom = 't', paging = FALSE), rownames = FALSE)
  })

  output$roc_plot <- renderPlot({
    req(results())
    roc_obj <- results()$roc
    best <- pROC::coords(
      roc_obj, x = "best", best.method = "closest.topleft",
      ret = c("threshold", "sensitivity", "specificity"), transpose = FALSE
    )
    auc_val <- pROC::auc(roc_obj)
    auc_ci  <- pROC::ci.auc(roc_obj)
    specs <- roc_obj$specificities
    ci_obj <- pROC::ci.se(roc_obj, specificities = specs, bootn = 5000, conf.level = 0.95)
    ci_df  <- data.frame(
      specificities = as.numeric(rownames(ci_obj)),
      ci.lower = ci_obj[, 1],
      ci.upper = ci_obj[, 3]
    )

    ggroc(roc_obj, color = "blue", size = 1.2) +
      geom_ribbon(data = ci_df,
                  aes(x = specificities, ymin = ci.lower, ymax = ci.upper),
                  fill = "lightblue", alpha = 0.3, inherit.aes = FALSE) +
      annotate("point", x = best$specificity, y = best$sensitivity,
               color = "red", size = 3) +
      annotate("text", x = best$specificity + 0.03, y = best$sensitivity + 0.05,
               label = paste0("Cut-off:", round(best$threshold, 3)),
               color = "red", size = 5, fontface = "bold") +
      annotate("text", x = 0.25, y = 0.10,
               label = paste0("AUC = ", round(auc_val, 3),
                              " [", round(auc_ci[1], 3), " – ", round(auc_ci[3], 3), "]"),
               size = 5, color = "black") +
      annotate("segment", x = best$specificity, xend = best$specificity,
               y = 0, yend = best$sensitivity,
               linetype = "dotted", color = "darkgreen") +
      annotate("segment", x = 1, xend = best$specificity,
               y = best$sensitivity, yend = best$sensitivity,
               linetype = "dotted", color = "darkgreen") +
      annotate("segment", x = 0, y = 1, xend = 1, yend = 0,
               linetype = "dashed", color = "grey40") +
      coord_cartesian(xlim = c(1, 0), ylim = c(0, 1)) +
      labs(x = "Specificity", y = "Sensitivity") +
      ggtitle("ROC curve") +
      theme_classic2(base_size = 16)
  })

  output$download_report <- downloadHandler(
    filename = function() {
      paste0("Diagnostic tests evaluation_confusion matrix_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Contingency table", tabColour = "green")
      writeData(wb, "Contingency table", results()$table, rowNames = TRUE)
      addWorksheet(wb, "Raw values", tabColour = "blue")
      writeData(wb, "Raw values", data.frame(
        Value = c("True positives", "False positives", "False negatives", "True negatives"),
        Count = c(results()$TP, results()$FP, results()$FN, results()$TN)
      ))
      saveWorkbook(wb, file)
    }
  )

  output$download_indicators <- downloadHandler(
    filename = function() {
      paste0("Diagnostic tests evaluation_Indicators_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Indicators",tabColour = "yellow")
      writeData(wb, "Indicators", results()$indicateurs)
      saveWorkbook(wb, file)
    }
  )

  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("ROC_curve_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(results())
      roc_obj <- results()$roc
      best <- pROC::coords(
        roc_obj, x = "best", best.method = "closest.topleft",
        ret = c("threshold", "sensitivity", "specificity"), transpose = FALSE
      )
      auc_val <- pROC::auc(roc_obj)
      auc_ci  <- pROC::ci.auc(roc_obj)
      specs <- roc_obj$specificities
      ci_obj <- pROC::ci.se(roc_obj, specificities = specs, bootn = 5000, conf.level = 0.95)
      ci_df  <- data.frame(
        specificities = as.numeric(rownames(ci_obj)),
        ci.lower = ci_obj[, 1],
        ci.upper = ci_obj[, 3]
      )
      g <- ggroc(roc_obj, color = "blue", size = 1.2) +
        geom_ribbon(data = ci_df,
                    aes(x = specificities, ymin = ci.lower, ymax = ci.upper),
                    fill = "lightblue", alpha = 0.3, inherit.aes = FALSE) +
        annotate("point", x = best$specificity, y = best$sensitivity,
                 color = "red", size = 3) +
        annotate("text", x = best$specificity + 0.03, y = best$sensitivity + 0.05,
                 label = paste0("Cut-off:", round(best$threshold, 3)),
                 color = "red", size = 4, fontface = "bold") +
        annotate("text", x = 0.2, y = 0.10,
                 label = paste0("AUC = ", round(auc_val, 3),
                                " [", round(auc_ci[1], 3), " – ", round(auc_ci[3], 3), "]"),
                 size = 5, color = "black") +
        annotate("segment", x = best$specificity, xend = best$specificity,
                 y = 0, yend = best$sensitivity,
                 linetype = "dotted", color = "darkgreen") +
        annotate("segment", x = 1, xend = best$specificity,
                 y = best$sensitivity, yend = best$sensitivity,
                 linetype = "dotted", color = "darkgreen") +
        annotate("segment", x = 0, y = 1, xend = 1, yend = 0,
                 linetype = "dashed", color = "grey40") +
        coord_cartesian(xlim = c(1, 0), ylim = c(0, 1)) +
        labs(x = "Specificity", y = "Sensitivity") +
        ggtitle("ROC curve") +
        theme_classic2()
      ggsave(file, plot = g, width = 8, height = 6, dpi = 300)
    }
  )
}

shinyApp(ui, server)
