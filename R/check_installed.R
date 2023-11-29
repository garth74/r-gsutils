#' @title Check whether a package is installed.
#'
#' @param pkg A character vector naming the package(s), whose installation
#' needs to be checked in any of the libraries. Note that the character vector
#' should be length 1.
#' @param strict If `strict` is `TRUE` and the package is not installed, the
#' function with throw an error. Otherwise, a `logical` value is returned.
#' @param prompt If `TRUE`, will prompt the user to install needed package.
#' @param ... Currently ignored
#'
#' @return If `strict = TRUE`, and `package` is not yet installed, the
#' function stops and throws an error. Else, a logical vector is
#' returned, indicating whether the package is installed.
#'
#' @export
gs_check_installed <- function(pkg,
                               strict = TRUE,
                               prompt = interactive(),
                               ...) {
  checkmate::qassert(pkg, "S1")
  # https://stackoverflow.com/a/62809204
  is_installed <- !identical(find.package(pkg, quiet = TRUE), character(0))

  if (isFALSE(is_installed)) {
    # The package isn't installed, but does anything need to happen?

    if (isTRUE(strict)) {
      # We need this package but maybe we can prompt the user to install it.
      if (!is.null(sys.calls())) {
        caller_fn_name <- deparse(sys.call(-1L))
        message <- sprintf("Calling `%s` requires %s.", caller_fn_name, pkg)
      } else {
        message <- sprintf("%s is required.", pkg)
      }
      answer <- 0 # set default in case it isn't an interactive session
      if (isTRUE(prompt)) {
        title <- sprintf("%s\n\nDo you want to install it?", message)
        answer <- utils::menu(c("Yes", "No"), title)
      }
      # no install is an error
      if (answer != 1) stop(message)
      install.packages(pkg)
      return(TRUE)
    }

    # This isn't a strict situation
    return(FALSE)
  }
  return(TRUE)
}
