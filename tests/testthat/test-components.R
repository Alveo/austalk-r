context("test-components")

test_that("can get a list of components", {
  cmp = components()

  expect_is(cmp, "data.frame")

  # result should have some column names
  expect_named(cmp)
  expect_true('shortname' %in% names(cmp))
  expect_true('name' %in% names(cmp))
  # result should have some rows
  expect_gt(dim(cmp)[1], 1)
  })

test_that("can get the prompts for a component", {

  p = prompts('digits')

  expect_is(p, 'data.frame')
  expect_named(p)
  expect_true('prompt' %in% names(p))
  expect_true('itemid' %in% names(p))
  expect_true('componentname' %in% names(p))
  expect_gt(dim(p)[1], 1)
})
