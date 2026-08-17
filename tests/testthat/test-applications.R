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
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  # Local alias "myaccount" intentionally differs from the server-side slug
  # "real-slug" to confirm that url/config_url use the resolved slug, not the alias.
  # userId is passed as accountId in registerAccount() via addTestAccount().
  addTestAccount("myaccount", server = "connect.posit.cloud", userId = "acct-1")

  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplications = function(accountId, ...) {
        # Real GET /contents response shape: id, title, account_id, timestamps.
        # No current_revision$url — that field is not documented on the content
        # list endpoint and should not be relied on.
        list(list(
          id = "abc-123",
          title = "My App",
          account_id = "acct-1",
          created_time = "2024-01-01T00:00:00Z",
          updated_time = "2024-01-02T00:00:00Z"
        ))
      },
      getAccounts = function() {
        list(data = list(list(id = "acct-1", name = "real-slug")))
      }
    )
  })

  result <- applications(account = "myaccount", server = "connect.posit.cloud")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$title, "My App")
  # url = content view page (server-side slug, not the local alias).
  # config_url = the content settings page.
  expect_equal(
    result$url,
    "https://connect.posit.cloud/real-slug/content/abc-123"
  )
  expect_equal(
    result$config_url,
    "https://connect.posit.cloud/real-slug/content/abc-123/settings/info"
  )
})

test_that("applications() returns empty data frame for PCC account with no content", {
  local_temp_config()
  addTestServer(
    url = "https://connect.posit.cloud",
    name = "connect.posit.cloud"
  )
  addTestAccount("myaccount", server = "connect.posit.cloud")

  local_mocked_bindings(clientForAccount = function(...) {
    list(listApplications = function(...) list())
  })

  result <- applications(account = "myaccount", server = "connect.posit.cloud")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_named(
    result,
    c(
      "id",
      "name",
      "title",
      "url",
      "status",
      "size",
      "instances",
      "config_url",
      "created_time",
      "updated_time",
      "guid"
    )
  )
})
