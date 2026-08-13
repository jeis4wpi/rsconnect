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

test_that("showUsers returns empty data frame with correct columns when no users authorized", {
  local_temp_config()
  addTestServer(url = "https://connect.posit.cloud", name = "connect.posit.cloud")
  addTestAccount("myaccount", server = "connect.posit.cloud")

  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplications = function(accountId, ...) {
        list(list(name = "myapp", id = "content-uuid-123"))
      },
      listApplicationAuthorization = function(appId) list()
    )
  })

  expect_warning(
    result <- showUsers(appName = "myapp", account = "myaccount",
                        server = "connect.posit.cloud"),
    regexp = "deprecated"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
  expect_named(result, c("id", "email", "account"))
})

test_that("addAuthorizedUser emits deprecation and calls inviteApplicationUser on PCC", {
  local_temp_config()
  addTestServer(url = "https://connect.posit.cloud", name = "connect.posit.cloud")
  addTestAccount("myaccount", server = "connect.posit.cloud")

  invited <- list()
  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplications = function(accountId, ...) {
        list(list(name = "myapp", id = "content-uuid-123"))
      },
      inviteApplicationUser = function(appId, email, sendEmail, emailMessage) {
        invited[[length(invited) + 1]] <<- list(appId = appId, email = email)
        invisible(TRUE)
      }
    )
  })

  expect_warning(
    addAuthorizedUser("alice@example.com", appName = "myapp",
                      account = "myaccount", server = "connect.posit.cloud"),
    regexp = "deprecated"
  )
  expect_length(invited, 1)
  expect_equal(invited[[1]]$email, "alice@example.com")
})

test_that("showInvited emits deprecation and maps PCC email_address/is_expired fields", {
  local_temp_config()
  addTestServer(url = "https://connect.posit.cloud", name = "connect.posit.cloud")
  addTestAccount("myaccount", server = "connect.posit.cloud")

  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplications = function(accountId, ...) {
        list(list(name = "myapp", id = "content-uuid-123"))
      },
      listApplicationInvitations = function(appId) {
        list(list(
          id = "invite-uuid-1",
          email_address = "alice@example.com",
          is_expired = FALSE
        ))
      }
    )
  })

  expect_warning(
    result <- showInvited(appName = "myapp", account = "myaccount",
                          server = "connect.posit.cloud"),
    regexp = "deprecated"
  )
  expect_equal(result$email, "alice@example.com")
  expect_true(is.na(result$link))
  expect_false(result$expired)
})

test_that("addAuthorizedUser warns when sendEmail is non-NULL on PCC", {
  local_temp_config()
  addTestServer(url = "https://connect.posit.cloud", name = "connect.posit.cloud")
  addTestAccount("myaccount", server = "connect.posit.cloud")

  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplications = function(accountId, ...) {
        list(list(name = "myapp", id = "content-uuid-123"))
      },
      inviteApplicationUser = function(appId, email, sendEmail, emailMessage) {
        invisible(TRUE)
      }
    )
  })

  msgs <- character(0)
  withCallingHandlers(
    addAuthorizedUser(
      "alice@example.com",
      appName = "myapp",
      account = "myaccount",
      server = "connect.posit.cloud",
      sendEmail = FALSE
    ),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("deprecated", msgs, ignore.case = TRUE)))
  expect_true(any(grepl("sendEmail.*ignored|ignored.*sendEmail|PCC always", msgs)))
})

test_that("showInvited returns NA for invitation records missing email and expired fields", {
  local_temp_config()
  addTestServer(url = "https://connect.posit.cloud", name = "connect.posit.cloud")
  addTestAccount("myaccount", server = "connect.posit.cloud")

  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplications = function(accountId, ...) {
        list(list(name = "myapp", id = "content-uuid-123"))
      },
      listApplicationInvitations = function(appId) {
        # record with neither email_address/email nor is_expired/expired
        list(list(id = "invite-uuid-missing"))
      }
    )
  })

  expect_warning(
    result <- showInvited(
      appName = "myapp",
      account = "myaccount",
      server = "connect.posit.cloud"
    ),
    regexp = "deprecated"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$email))
  expect_true(is.na(result$expired))
})

test_that("showUsers returns NA for user records with absent id and email fields", {
  local_temp_config()
  addTestServer(url = "https://connect.posit.cloud", name = "connect.posit.cloud")
  addTestAccount("myaccount", server = "connect.posit.cloud")

  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplications = function(accountId, ...) {
        list(list(name = "myapp", id = "content-uuid-123"))
      },
      listApplicationAuthorization = function(appId) {
        # record with no user$id or user$email fields
        list(list(user = list()))
      }
    )
  })

  expect_warning(
    result <- showUsers(
      appName = "myapp",
      account = "myaccount",
      server = "connect.posit.cloud"
    ),
    regexp = "deprecated"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$id))
  expect_true(is.na(result$email))
})

test_that("showUsers errors on non-shinyapps non-PCC server without emitting deprecation", {
  local_temp_config()
  addTestServer(url = "https://connect.example.com", name = "connect.example.com")
  addTestAccount("myaccount", server = "connect.example.com")

  saw_deprecation <- FALSE
  expect_error(
    withCallingHandlers(
      showUsers(appName = "myapp", account = "myaccount",
                server = "connect.example.com"),
      warning = function(w) {
        if (grepl("deprecated", conditionMessage(w), ignore.case = TRUE)) {
          saw_deprecation <<- TRUE
        }
        invokeRestart("muffleWarning")
      }
    ),
    regexp = "shinyapps\\.io"
  )
  expect_false(saw_deprecation)
})

test_that("resendInvitation emits deprecation and calls resendApplicationInvitation on PCC", {
  local_temp_config()
  addTestServer(url = "https://connect.posit.cloud", name = "connect.posit.cloud")
  addTestAccount("myaccount", server = "connect.posit.cloud")

  resent_id <- NULL
  local_mocked_bindings(clientForAccount = function(...) {
    list(
      listApplications = function(accountId, ...) {
        list(list(name = "myapp", id = "content-uuid-123"))
      },
      listApplicationInvitations = function(appId) {
        list(list(
          id = "invite-uuid-1",
          email_address = "alice@example.com",
          is_expired = FALSE
        ))
      },
      resendApplicationInvitation = function(inviteId, regenerate) {
        resent_id <<- inviteId
        invisible(TRUE)
      }
    )
  })

  expect_warning(
    resendInvitation("alice@example.com", appName = "myapp",
                     account = "myaccount", server = "connect.posit.cloud"),
    regexp = "deprecated"
  )
  expect_equal(resent_id, "invite-uuid-1")
})
