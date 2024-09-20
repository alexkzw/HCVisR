test_that("app_ui creates a valid Shiny UI object", {
    ui <- app_ui()
    expect_is(ui, "shiny.tag.list")

    # Check for UI elements
    expect_true("titlePanel" %in% names(ui[[1]]))
    expect_true("sidebarLayout" %in% names(ui[[2]]))
    expect_true("mainPanel" %in% names(ui[[2]]$children))
})
