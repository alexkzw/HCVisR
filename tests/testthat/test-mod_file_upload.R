test_that("File upload UI works", {
    ui <- mod_file_upload_ui("test")
    expect_is(ui, "shiny.tag.list")

    expect_true("fileInput" %in% names(ui[[1]]))
})

test_that("mod_file_upload_server processes files correctly", {
    shiny::testServer(mod_file_upload_server, {
        # Upload a dummy CSV file
        file <- tempfile()
        write.csv(data.frame(x = 1:10), file, row.names = FALSE)

        session$setInputs(file = list(datapath = file))
        session$setInputs(process_file = 1)

        # Check if the available series is updated
        expect_length(available_series(), 1)
    })
})
