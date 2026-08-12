test_that("syncAppMetadata updates deployment records", {
  local_temp_config()
  addTestServer()
  addTestAccount("ron")

  app <- local_temp_app()
  addTestDeployment(app, appId = "123", metadata = list(when = 123))
  local_mocked_bindings(clientForAccount = function(...) {
    list(
      getApplication = function(...) list(title = "newtitle", url = "newurl")
    )
  })

  syncAppMetadata(app)
  deps <- deployments(app)
  expect_equal(deps$title, "newtitle")
  expect_equal(deps$url, "newurl")
  expect_equal(deps$when, NULL)
})

test_that("syncAppMetadata deletes deployment records if needed", {
  local_temp_config()
  addTestServer()
  addTestAccount("ron")

  app <- local_temp_app()
  addTestDeployment(app, appId = "123", metadata = list(when = 123))
  local_mocked_bindings(clientForAccount = function(...) {
    list(
      getApplication = function(...) abort(class = "rsconnect_http_404")
    )
  })

  expect_snapshot(syncAppMetadata(app))
  expect_equal(nrow(deployments(app)), 0)
})

test_that("applications() returns a data frame for PCC accounts", {
  local_temp_config()
  addTestServer(url = "https://connect.posit.cloud", name = "connect.posit.cloud")
  addTestAccount("myaccount", server = "connect.posit.cloud")

  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplications = function(accountId, ...) {
        list(list(
          id = "abc-123",
          title = "My App",
          name = "My App",
          current_revision = list(url = "https://connect.posit.cloud/myaccount/content/abc-123"),
          created_time = "2024-01-01T00:00:00Z",
          updated_time = "2024-01-02T00:00:00Z"
        ))
      }
    )
  })

  result <- applications(account = "myaccount", server = "connect.posit.cloud")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$title, "My App")
  expect_equal(result$url, "https://connect.posit.cloud/myaccount/content/abc-123")
  expect_true(grepl("myaccount/content/abc-123", result$config_url))
})

test_that("applications() returns empty data frame for PCC account with no content", {
  local_temp_config()
  addTestServer(url = "https://connect.posit.cloud", name = "connect.posit.cloud")
  addTestAccount("myaccount", server = "connect.posit.cloud")

  local_mocked_bindings(clientForAccount = function(...) {
    list(listApplications = function(...) list())
  })

  result <- applications(account = "myaccount", server = "connect.posit.cloud")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_named(result, c("id", "name", "title", "url", "status", "size",
                          "instances", "config_url", "created_time", "updated_time", "guid"))
})
