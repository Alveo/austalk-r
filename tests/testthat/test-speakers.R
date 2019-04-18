context("test-speakers")

test_that("can retrieve a list of speakers", {
  spk = speakers()

  expect_is(spk, "data.frame")

  # result should have some column names
  expect_named(spk)
  expect_true('identifier' %in% names(spk))
  expect_true('gender' %in% names(spk))
  expect_true('age' %in% names(spk))
  # result should have some rows
  expect_gt(dim(spk)[1], 1)
  })



test_that("can get a count of items for speakers", {
  spk = speaker_items(c('digits', 'words-1', 'words-1-2'))

  expect_is(spk, "data.frame")

  # result should have some column names
  expect_named(spk)
  expect_true('speaker' %in% names(spk))
  expect_true('digits' %in% names(spk))
  expect_true('words_1' %in% names(spk))
  expect_true('words_1_2' %in% names(spk))
  # result should have some rows
  expect_gt(dim(spk)[1], 1)
})

