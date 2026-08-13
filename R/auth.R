cleanupPasswordFile <- function(appDir) {
  check_directory(appDir)
  appDir <- normalizePath(appDir)

  # get data dir from appDir
  dataDir <- file.path(appDir, "shinyapps")

  # get password file
  passwordFile <- file.path(dataDir, paste("passwords", ".txt", sep = ""))

  # check if password file exists
  if (file.exists(passwordFile)) {
    message(
      "WARNING: Password file found! This application is configured to use scrypt ",
      "authentication, which is no longer supported.\nIf you choose to proceed, ",
      "all existing users of this application will be removed, ",
      "and will NOT be recoverable.\nFor for more information please visit: ",
      "http://shiny.rstudio.com/articles/migration.html"
    )
    response <- readline("Do you want to proceed? [Y/n]: ")
    if (tolower(substring(response, 1, 1)) != "y") {
      stop("Cancelled", call. = FALSE)
    } else {
      # remove old password file
      file.remove(passwordFile)
    }
  }

  invisible(TRUE)
}

#' Add authorized user to application
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Add authorized user to application
#'
#' Supported servers: ShinyApps, Posit Connect Cloud
#'
#' @param email Email address of user to add.
#' @param appDir Directory containing application. Defaults to
#'   current working directory.
#' @param appName Name of application.
#' @inheritParams deployApp
#' @param sendEmail Send an email letting the user know the application
#'   has been shared with them.
#' @param emailMessage Optional character vector of length 1 containing a
#'   custom message to send in email invitation. Defaults to NULL, which
#'   will use default invitation message.
#' @seealso [removeAuthorizedUser()] and [showUsers()]
#' @note This function works for ShinyApps and Posit Connect Cloud. On Posit
#'   Connect Cloud, the content's account must be an organization account.
#'   The \code{sendEmail} argument is ignored on Posit Connect Cloud;
#'   PCC always sends an invitation email.
#' @export
addAuthorizedUser <- function(
  email,
  appDir = getwd(),
  appName = NULL,
  account = NULL,
  server = NULL,
  sendEmail = NULL,
  emailMessage = NULL
) {
  accountDetails <- accountInfo(account, server)
  lifecycle::deprecate_warn(
    "1.10.1.9000",
    "addAuthorizedUser()",
    details = if (isPositConnectCloudServer(accountDetails$server)) {
      paste0("Manage collaborators directly in Posit Connect Cloud at ",
             "<https://connect.posit.cloud>.")
    } else {
      paste0("Manage collaborators directly in shinyapps.io at ",
             "<https://www.shinyapps.io>.")
    }
  )
  if (!isPositConnectCloudServer(accountDetails$server)) {
    checkShinyappsServer(accountDetails$server)
  }

  # resolve application
  if (is.null(appName)) {
    appName <- basename(appDir)
  }
  application <- resolveApplication(accountDetails, appName)

  # check for and remove password file
  cleanupPasswordFile(appDir)

  # PCC always emails invitees; warn if caller explicitly passed sendEmail
  if (isPositConnectCloudServer(accountDetails$server) && !is.null(sendEmail)) {
    cli::cli_warn(
      "{.arg sendEmail} is ignored on Posit Connect Cloud; PCC always sends an invitation email."
    )
  }

  # fetch authorization list
  api <- clientForAccount(accountDetails)
  api$inviteApplicationUser(
    application$id,
    validateEmail(email),
    sendEmail,
    emailMessage
  )

  message(paste("Added:", email, "to application", sep = " "))

  invisible(TRUE)
}

#' Remove authorized user from an application
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Remove authorized user from an application
#'
#' Supported servers: ShinyApps, Posit Connect Cloud
#'
#' @param user The user to remove. Can be id or email address.
#' @param appDir Directory containing application. Defaults to
#' current working directory.
#' @param appName Name of application.
#' @inheritParams deployApp
#' @seealso [addAuthorizedUser()] and [showUsers()]
#' @note This function works for ShinyApps and Posit Connect Cloud.
#' @export
removeAuthorizedUser <- function(
  user,
  appDir = getwd(),
  appName = NULL,
  account = NULL,
  server = NULL
) {
  accountDetails <- accountInfo(account, server)
  lifecycle::deprecate_warn(
    "1.10.1.9000",
    "removeAuthorizedUser()",
    details = if (isPositConnectCloudServer(accountDetails$server)) {
      paste0("Manage collaborators directly in Posit Connect Cloud at ",
             "<https://connect.posit.cloud>.")
    } else {
      paste0("Manage collaborators directly in shinyapps.io at ",
             "<https://www.shinyapps.io>.")
    }
  )
  if (!isPositConnectCloudServer(accountDetails$server)) {
    checkShinyappsServer(accountDetails$server)
  }

  # resolve application
  if (is.null(appName)) {
    appName <- basename(appDir)
  }
  application <- resolveApplication(accountDetails, appName)

  # check and remove password file
  cleanupPasswordFile(appDir)

  # get users
  users <- showUsers(appDir, appName, account, server)

  if (is.numeric(user)) {
    # lookup by id
    if (user %in% users$id) {
      user <- users[users$id == user, ]
    } else {
      stop("User ", user, " not found", call. = FALSE)
    }
  } else {
    # lookup by email
    if (user %in% users$email) {
      user <- users[users$email == user, ]
    } else {
      stop("User \"", user, "\" not found", call. = FALSE)
    }
  }

  # remove user
  api <- clientForAccount(accountDetails)
  api$removeApplicationUser(application$id, user$id)

  message(paste("Removed:", user$email, "from application", sep = " "))

  invisible(TRUE)
}

#' List authorized users for an application
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' List authorized users for an application
#'
#' Supported servers: ShinyApps, Posit Connect Cloud
#'
#' @param appDir Directory containing application. Defaults to
#'   current working directory.
#' @param appName Name of application.
#' @inheritParams deployApp
#' @seealso [addAuthorizedUser()] and [showInvited()]
#' @note This function works for ShinyApps and Posit Connect Cloud.
#' @export
showUsers <- function(
  appDir = getwd(),
  appName = NULL,
  account = NULL,
  server = NULL
) {
  accountDetails <- accountInfo(account, server)
  lifecycle::deprecate_warn(
    "1.10.1.9000",
    "showUsers()",
    details = if (isPositConnectCloudServer(accountDetails$server)) {
      paste0("Manage collaborators directly in Posit Connect Cloud at ",
             "<https://connect.posit.cloud>.")
    } else {
      paste0("Manage collaborators directly in shinyapps.io at ",
             "<https://www.shinyapps.io>.")
    }
  )
  if (!isPositConnectCloudServer(accountDetails$server)) {
    checkShinyappsServer(accountDetails$server)
  }

  # resolve application
  if (is.null(appName)) {
    appName <- basename(appDir)
  }
  application <- resolveApplication(accountDetails, appName)

  # fetch authorization list
  api <- clientForAccount(accountDetails)
  res <- api$listApplicationAuthorization(application$id)

  # get interesting fields; build rows as data.frames so rbind yields atomic
  # columns (do.call(rbind, list_of_lists) produces list-matrix columns).
  rows <- lapply(res, function(x) {
    data.frame(
      id      = as.character(x$user$id),
      email   = as.character(x$user$email),
      account = if (!is.null(x$account)) as.character(x$account) else NA_character_,
      stringsAsFactors = FALSE
    )
  })

  if (length(rows) == 0L) {
    return(data.frame(
      id      = character(),
      email   = character(),
      account = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

#' List invited users for an application
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' List invited users for an application
#'
#' Supported servers: ShinyApps, Posit Connect Cloud
#'
#' @param appDir Directory containing application. Defaults to
#'   current working directory.
#' @param appName Name of application.
#' @inheritParams deployApp
#' @seealso [addAuthorizedUser()] and [showUsers()]
#' @note This function works for ShinyApps and Posit Connect Cloud. On Posit
#'   Connect Cloud, the \code{link} column is always \code{NA} because the
#'   accept link is only emailed to the recipient and is never returned by
#'   the API.
#' @export
showInvited <- function(
  appDir = getwd(),
  appName = NULL,
  account = NULL,
  server = NULL
) {
  accountDetails <- accountInfo(account, server)
  lifecycle::deprecate_warn(
    "1.10.1.9000",
    "showInvited()",
    details = if (isPositConnectCloudServer(accountDetails$server)) {
      paste0("Manage collaborators directly in Posit Connect Cloud at ",
             "<https://connect.posit.cloud>.")
    } else {
      paste0("Manage collaborators directly in shinyapps.io at ",
             "<https://www.shinyapps.io>.")
    }
  )
  if (!isPositConnectCloudServer(accountDetails$server)) {
    checkShinyappsServer(accountDetails$server)
  }

  # resolve application
  if (is.null(appName)) {
    appName <- basename(appDir)
  }
  application <- resolveApplication(accountDetails, appName)

  # fetch invitation list
  api <- clientForAccount(accountDetails)
  res <- api$listApplicationInvitations(application$id)

  # get interesting fields; PCC uses email_address / is_expired; shinyapps uses email / expired.
  # Build rows as data.frames so rbind yields atomic columns (do.call(rbind, list_of_lists)
  # produces list-matrix columns).
  rows <- lapply(res, function(x) {
    data.frame(
      id      = as.character(x$id),
      email   = as.character(x$email_address %||% x$email %||% NA_character_),
      link    = as.character(x$link %||% NA_character_),
      expired = as.logical(x$is_expired %||% x$expired %||% NA),
      stringsAsFactors = FALSE
    )
  })

  if (length(rows) == 0L) {
    return(data.frame(
      id      = character(),
      email   = character(),
      link    = character(),
      expired = logical(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

#' Resend invitation for invited users of an application
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Resend invitation for invited users of an application
#'
#' Supported servers: ShinyApps, Posit Connect Cloud
#'
#' @param invite The invitation to resend. Can be id or email address.
#' @param regenerate Regenerate the invite code. Can be helpful is the
#' invitation has expired.
#' @param appDir Directory containing application. Defaults to
#'   current working directory.
#' @param appName Name of application.
#' @inheritParams deployApp
#' @seealso [showInvited()]
#' @note This function works for ShinyApps and Posit Connect Cloud. On Posit
#'   Connect Cloud, look up the invitation by email address; the
#'   \code{regenerate} argument has no effect.
#' @export
resendInvitation <- function(
  invite,
  regenerate = FALSE,
  appDir = getwd(),
  appName = NULL,
  account = NULL,
  server = NULL
) {
  accountDetails <- accountInfo(account, server)
  lifecycle::deprecate_warn(
    "1.10.1.9000",
    "resendInvitation()",
    details = if (isPositConnectCloudServer(accountDetails$server)) {
      paste0("Manage collaborators directly in Posit Connect Cloud at ",
             "<https://connect.posit.cloud>.")
    } else {
      paste0("Manage collaborators directly in shinyapps.io at ",
             "<https://www.shinyapps.io>.")
    }
  )
  if (!isPositConnectCloudServer(accountDetails$server)) {
    checkShinyappsServer(accountDetails$server)
  }

  # get invitations
  invited <- showInvited(appDir, appName, account, server)

  if (is.numeric(invite)) {
    # lookup by id
    if (invite %in% invited$id) {
      invite <- invited[invited$id == invite, ]
    } else {
      stop("Invitation \"", invite, "\" not found", call. = FALSE)
    }
  } else {
    # lookup by email
    if (invite %in% invited$email) {
      invite <- invited[invited$email == invite, ]
    } else {
      stop("Invitation for \"", invite, "\" not found", call. = FALSE)
    }
  }

  # resend invitation
  api <- clientForAccount(accountDetails)
  api$resendApplicationInvitation(invite$id, regenerate)

  message(paste("Sent invitation to", invite$email, "", sep = " "))

  invisible(TRUE)
}

# Previously exported, but deprecated since 2015
authorizedUsers <- function(appDir = getwd()) {
  # read password file
  path <- getPasswordFile(appDir)
  if (file.exists(path)) {
    passwords <- readPasswordFile(path)
  } else {
    passwords <- NULL
  }

  return(passwords)
}

validateEmail <- function(email) {
  if (is.null(email) || !grepl(".+\\@.+\\..+", email)) {
    stop("Invalid email address.", call. = FALSE)
  }

  invisible(email)
}

getPasswordFile <- function(appDir) {
  check_directory(appDir)

  file.path(normalizePath(appDir), "shinyapps", "passwords.txt")
}

readPasswordFile <- function(path) {
  # open and read file
  lines <- readLines(path)

  # extract fields
  fields <- do.call(rbind, strsplit(lines, ":"))
  users <- fields[, 1]
  hashes <- fields[, 2]

  # convert to data frame
  df <- data.frame(user = users, hash = hashes, stringsAsFactors = FALSE)

  # return data frame
  return(df)
}

writePasswordFile <- function(path, passwords) {
  # open and file
  f <- file(path, open = "w")
  defer(close(f))

  # write passwords
  apply(passwords, 1, function(r) {
    l <- paste(r[1], ":", r[2], "\n", sep = "")
    cat(l, file = f, sep = "")
  })
  message(
    "Password file updated. You must deploy your application for these changes to take effect."
  )
}
