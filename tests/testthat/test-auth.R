test_that("showUsers emits deprecation and returns data frame for PCC", {
  local_temp_config()
  addTestServer(url = "https://connect.posit.cloud", name = "connect.posit.cloud")
  addTestAccount("myaccount", server = "connect.posit.cloud")

  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplications = function(accountId, ...) {
        list(list(name = "myapp", id = "content-uuid-123"))
      },
      listApplicationAuthorization = function(appId) {
        list(
          list(user = list(id = "user-uuid-1", email = "alice@example.com")),
          list(user = list(id = "user-uuid-2", email = "bob@example.com"))
        )
      }
    )
  })

  expect_warning(
    result <- showUsers(appName = "myapp", account = "myaccount",
                        server = "connect.posit.cloud"),
    regexp = "deprecated"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(result$email, c("alice@example.com", "bob@example.com"))
})

test_that("removeAuthorizedUser emits deprecation and calls removeApplicationUser on PCC", {
  local_temp_config()
  addTestServer(url = "https://connect.posit.cloud", name = "connect.posit.cloud")
  addTestAccount("myaccount", server = "connect.posit.cloud")

  removed_user_id <- NULL
  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplications = function(accountId, ...) {
        list(list(name = "myapp", id = "content-uuid-123"))
      },
      listApplicationAuthorization = function(appId) {
        list(list(user = list(id = "user-uuid-1", email = "alice@example.com")))
      },
      removeApplicationUser = function(appId, userId) {
        removed_user_id <<- userId
        invisible(TRUE)
      }
    )
  })

  expect_warning(
    removeAuthorizedUser("alice@example.com", appName = "myapp",
                          account = "myaccount", server = "connect.posit.cloud"),
    regexp = "deprecated"
  )
  expect_equal(removed_user_id, "user-uuid-1")
})
