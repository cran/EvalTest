test_that("compute_indicators works with simple example", {
  res <- compute_indicators(tp = 50, fp = 10, fn = 5, tn = 100, prev = 0.1)

  # Sensitivity = 50 / (50+5) = 0.909...
  expect_equal(round(res$Se, 2), 0.91)

  # Specificity = 100 / (100+10) = 0.909...
  expect_equal(round(res$Sp, 2), 0.91)

  # Accuracy = (50+100) / (50+10+5+100) = 150/165 ≈ 0.91
  expect_equal(round(res$Acc, 2), 0.91)

  # Youden = Se + Sp - 1 ≈ 0.82
  expect_equal(round(res$Youden, 2), 0.82)

  # Check outputs are not NULL
  expect_false(is.null(res$ci_Se))
  expect_false(is.null(res$ci_Sp))
  expect_false(is.null(res$ci_PPV))
  expect_false(is.null(res$ci_NPV))
  expect_false(is.null(res$ci_LRp))
  expect_false(is.null(res$ci_LRn))
  expect_false(is.null(res$ci_Acc))
  expect_false(is.null(res$ci_Youden))
})
