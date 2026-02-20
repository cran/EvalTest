#' Compute diagnostic test indicators
#'
#' This function computes sensitivity, specificity, predictive values,
#' likelihood ratios, accuracy, and Youden index with their confidence intervals
#' (with desired confidence level), based on a 2x2 table of diagnostic test results.
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
#' compute_indicators(58, 15, 14, 26, prev = 0.1, conf = 0.95)
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

  # Binomial confidence intervals function for Se, Sp, and Accuracy
  ci_binom <- function(x, n, conf = 0.95) {
    if (n == 0) return(c(NA, NA))
    ci <- binom::binom.confint(x, n, conf.level = conf, methods = "wilson")
    c(ci$lower, ci$upper)
  }

  ci_se <- ci_binom(tp, tp + fn, conf)
  ci_sp <- ci_binom(tn, tn + fp, conf)
  ci_acc <- ci_binom(tp + tn, total, conf)

  # CI for predictive values (approx by sensitivity/specificity IC97.5% bounds)
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

  # Binomial confidence intervals for Se and Sp function used to compute CIs for PPV and NPV
  ci_binom_2 <- function(x, n, conff = sqrt(conf)) {
      if (n == 0) return(c(NA, NA))
      ci <- binom.confint(x, n, conf.level = conff, methods = "wilson")
      c(ci$lower, ci$upper)
  }

  ci_se_2 <- ci_binom_2(tp, tp + fn)
  ci_sp_2 <- ci_binom_2(tn, tn + fp)

  se_bounds <- ci_se_2[1:2]
  sp_bounds <- ci_sp_2[1:2]
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
    Sensitivity_Se = Se, CI_Se = ci_se,
    Specificity_Sp = Sp, CI_Sp = ci_sp,
    Positive_predictive_value_PPV = PPV, CI_PPV = ci_ppv,
    Negative_predictive_value_NPV = NPV, CI_NPV = ci_npv,
    Positive_likelihood_ratio_LRp = LRp, CI_LRp = ci_lr_p,
    Negative_likelihood_ratio_LRn = LRn, CI_LRn = ci_lr_n,
    Accuracy_Acc = Acc, CI_Acc = ci_acc,
    Youden_index = Youden, CI_Youden = ci_youden
  )
}
