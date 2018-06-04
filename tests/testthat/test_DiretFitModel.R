library("regressoR.direct")
context("DirectFitModel")

test_that("Test DirectFitModel constructor", {
  f <- function(x) x
  quality <- 12;
  size <- 7L;
  instance <- new("DirectFitModel", f=f, quality=quality, size=size, name="bla");
  methods::validObject(instance);
  expect_identical(instance@quality, quality);
  expect_identical(instance@f, f);
  expect_identical(instance@size, size);
  expect_identical(instance@name, "bla");

  str <- as.character(instance);
  expect_identical(str, "bla");
})

test_that("Test DirectFitModel constructor error", {
  f <- function(y) y
  quality <- 12;
  size <- 7L;
  expect_error(new("DirectFitModel", f=f, quality=quality, size=size, name="bla"));

  f <- function(x) x
  quality <- -12;
  size <- 7L;
  expect_error(new("DirectFitModel", f=f, quality=quality, size=size, name="bla"));

  f <- function(x) x
  quality <- 12;
  size <- -7L;
  expect_error(new("DirectFitModel", f=f, quality=quality, size=size, name="bla"));

  f <- function(x) x
  quality <- 12;
  size <- 7L;
  expect_error(new("DirectFitModel", f=f, quality=quality, size=size));
  expect_error(new("DirectFitModel", f=f, size=size, name="bla"));
  expect_error(new("DirectFitModel", quality=quality, size=size, name="bla"));
  expect_error(new("DirectFitModel", f=f, name="bla"));
  expect_error(new("DirectFitModel", quality=quality, name="bla"));
  expect_error(new("DirectFitModel", size=size, name="bla"));
  instance <- new("DirectFitModel");
  expect_error(validObject(instance));
})


test_that("Test DirectFitModel.new", {
  f <- function(x) x
  quality <- 12;
  size <- 7L;
  instance <- DirectFitModel.new(f=f, quality=quality, size=size, name="bla");
  methods::validObject(instance);
  expect_identical(instance@quality, quality);
  expect_identical(instance@f, f);
  expect_identical(instance@size, size);
  expect_identical(instance@name, "bla");

  str <- as.character(instance);
  expect_gt(nchar(str), 0);
  expect_identical(str, "bla");
})

test_that("Test DirectFitModel.new error", {
  f <- function(y) y
  quality <- 12;
  size <- 7L;
  expect_error(DirectFitModel.new(f=f, quality=quality, size=size, name="bla"));

  f <- function(x) x
  quality <- -12;
  size <- 7L;
  expect_error(DirectFitModel.new(f=f, quality=quality, size=size, name="bla"));

  f <- function(x) x
  quality <- 12;
  size <- -7L;
  expect_error(DirectFitModel.new(f=f, quality=quality, size=size, name="bla"));

  f <- function(x) x
  quality <- 12;
  size <- 7L;
  expect_error(DirectFitModel.new(f=f, quality=quality, size=size));
  expect_error(DirectFitModel.new(f=f, size=size, name="bla"));
  expect_error(DirectFitModel.new(quality=quality, size=size, name="bla"));
  expect_error(DirectFitModel.new(f=f, name="bla"));
  expect_error(DirectFitModel.new(quality=quality, name="bla"));
  expect_error(DirectFitModel.new(f=f, quality=quality, name="bla"));
  expect_error(DirectFitModel.new(size=size, name="bla"));
  expect_error(DirectFitModel.new());
})
