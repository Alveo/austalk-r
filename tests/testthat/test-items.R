context("test-items")

test_that("can get items for a component", {
  with_mock_api({
    items <- componentItems(c('1_10',  '1_1'), 'digits')

    expect_is(items, 'data.frame')
    # result should have some column names
    expect_named(items)
    expect_true('speaker' %in% names(items))
    expect_true('item_url' %in% names(items))
    expect_true('itemid' %in% names(items))
    # result should have some rows
    expect_gt(dim(items)[1], 1)
  })
})
