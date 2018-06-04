library("regressoR.direct")
context("default")

test_that("Test default", {
  set.seed(124L);

  sl <- regressoR.direct.default();
  f <- function(x) 3 - 2*x + 0.3*x*x
  x <- runif(1000, min=0, max=6);
  y <- f(x);
  metric <- regressoR.quality::RegressionQualityMetric.default(x, y);

  total <- 0L;
  count <- 0L;

  for(s in sl) {
    sfr <- s(metric, NULL, NULL, NULL, 0);
    total <- total + 1L;
    if(!(is.null(sfr))) {
      count <- count + 1L;
      expect_is(sfr, "DirectFitModel");
      expect_lte(sfr@size, length(x) + 3);
      expect_true(all(is.finite(sfr@f(x))));
      if(sfr@size > 15L) {
        expect_lt(metric@quality(sfr@f), 4);
        expect_lt(sum(abs(sfr@f(x) - y)) / length(x), 4)
      }
    }
  }

  expect_gt((count/total), 0.5);

  total <- 0L;
  count <- 0L;
  y <- rnorm(n=length(x), mean=f(x), s=0.3);
  metric <- regressoR.quality::RegressionQualityMetric.default(x, y);

  for(s in sl) {
    sfr <- s(metric, NULL, NULL, NULL, 0);
    total <- total + 1L;
    if(!(is.null(sfr))) {
      count <- count + 1L;
      expect_lte(sfr@size, length(x)+3);
      expect_true(all(is.finite(sfr@f(x))));
      if(sfr@size > 4L) {
        expect_lt(metric@quality(sfr@f), 2.75);
        expect_lt(sum(abs(sfr@f(x) - y)) / length(x), 3)
      }
    }
  }
  expect_gt((count/total), 0.5);
})
