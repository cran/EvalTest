#' Compute diagnostic test indicators
#'
#' This function computes sensitivity, specificity, predictive values,
#' likelihood ratios, accuracy, and Youden index with confidence intervals
#' based on a 2x2 table of diagnostic test results.
#'
#' @param tp True positives
#' @param fp False positives
#' @param fn False negatives
#' @param tn True negatives
#' @param prev Prevalence (numeric between 0 and 1)
#' @param conf Confidence level (default 0.95)
#'
#' @return A list with all diagnostic indicators and confidence intervals
#' @export
#' @importFrom binom binom.confint
#'
#' @examples
#' compute_indicators(50, 10, 5, 100, prev = 0.1)
compute_indicators <- function(tp, fp, fn, tn, prev, conf = 0.95) {
  stopifnot(tp >= 0, fp >= 0, fn >= 0, tn >= 0)

  total <- tp + fp + fn + tn
  Se <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
  Sp <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_

  # Predictive values
  denom_ppv <- (Se * prev) + ((1 - Sp) * (1 - prev))
  PPV <- if (denom_ppv == 0) NA_real_ else (Se * prev) / denom_ppv

  denom_npv <- ((1 - Se) * prev) + (Sp * (1 - prev))
  NPV <- if (denom_npv == 0) NA_real_ else (Sp * (1 - prev)) / denom_npv

  # Likelihood ratios
  LRp <- if (Sp == 1) NA_real_ else Se / (1 - Sp)
  LRn <- if (Sp == 0) NA_real_ else (1 - Se) / Sp

  # Accuracy
  Acc <- if (total == 0) NA_real_ else (tp + tn) / total

  # Youden index
  Youden <- Se + Sp - 1

  # Binomial confidence intervals
  ci_binom <- function(x, n, conf = 0.95) {
    if (n == 0) return(c(NA, NA))
    ci <- binom::binom.confint(x, n, conf.level = conf, methods = "wilson")
    c(ci$lower, ci$upper)
  }

  ci_se <- ci_binom(tp, tp + fn, conf)
  ci_sp <- ci_binom(tn, tn + fp, conf)
  ci_acc <- ci_binom(tp + tn, total, conf)

  # CI for predictive values (approx by sensitivity/specificity bounds)
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
                    Vectorize(function(se, sp) ppv_fun(se, sp, prev)))
  npv_vals <- outer(se_bounds, sp_bounds,
                    Vectorize(function(se, sp) npv_fun(se, sp, prev)))
  ci_ppv <- range(ppv_vals, na.rm = TRUE)
  ci_npv <- range(npv_vals, na.rm = TRUE)

  # CI for LR+ and LR- (log method, delta approximation)
  se_lr_p <- if (!is.na(LRp) && tp > 0 && fp > 0) sqrt((1 - Se) / tp + Sp / fp) else NA_real_
  se_lr_n <- if (!is.na(LRn) && fn > 0 && tn > 0) sqrt(Se / fn + (1 - Sp) / tn) else NA_real_
  ci_lr_p <- if (!is.na(LRp) && !is.na(se_lr_p)) exp(log(LRp) + c(-1, 1) * 1.96 * se_lr_p) else c(NA, NA)
  ci_lr_n <- if (!is.na(LRn) && !is.na(se_lr_n)) exp(log(LRn) + c(-1, 1) * 1.96 * se_lr_n) else c(NA, NA)

  # CI for Youden index
  se_youden <- if ((tp + fn) > 0 && (tn + fp) > 0) {
    sqrt((Se * (1 - Se)) / (tp + fn) + (Sp * (1 - Sp)) / (tn + fp))
  } else NA_real_
  ci_youden <- if (!is.na(se_youden)) Youden + c(-1, 1) * 1.96 * se_youden else c(NA, NA)

  list(
    Se = Se, ci_Se = ci_se,
    Sp = Sp, ci_Sp = ci_sp,
    PPV = PPV, ci_PPV = ci_ppv,
    NPV = NPV, ci_NPV = ci_npv,
    LRp = LRp, ci_LRp = ci_lr_p,
    LRn = LRn, ci_LRn = ci_lr_n,
    Acc = Acc, ci_Acc = ci_acc,
    Youden = Youden, ci_Youden = ci_youden
  )
}
