test_that("compute_indicators works with simple example", {
  res <- compute_indicators(tp = 58, fp = 15, fn = 14, tn = 26, prev = 0.1, conf = 0.95)

  # Sensitivity = 58 / (58+14) = 0.806...
  expect_equal(round(res$Sensitivity_Se, 3), 0.806)

  # Specificity = 26 / (26+15) = 0.634...
  expect_equal(round(res$Specificity_Sp, 3), 0.634)

  # Accuracy = (58+26) / (58+15+14+26) = 0.743
  expect_equal(round(res$Accuracy_Acc, 3), 0.743)

  # Youden = Se + Sp - 1 ≈ 0.440
  expect_equal(round(res$Youden_index, 3), 0.440)

  # Check outputs are not NULL
  expect_false(is.null(res$CI_Se))
  expect_false(is.null(res$CI_Sp))
  expect_false(is.null(res$CI_PPV))
  expect_false(is.null(res$CI_NPV))
  expect_false(is.null(res$CI_LRp))
  expect_false(is.null(res$CI_LRn))
  expect_false(is.null(res$CI_Acc))
  expect_false(is.null(res$CI_Youden))
})
