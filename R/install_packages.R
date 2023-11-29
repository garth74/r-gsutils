#' @inherit utils::install.packages
#' @export
gs_install <- function(pkgs,
                       lib,
                       repos = getOption("repos"),
                       contriburl = contrib.url(repos, type),
                       method,
                       available = NULL,
                       destdir = NULL,
                       dependencies = NA,
                       type = getOption("pkgType"),
                       configure.args = getOption("configure.args"),
                       configure.vars = getOption("configure.vars"),
                       clean = FALSE,
                       Ncpus = getOption("Ncpus", 1L),
                       verbose = getOption("verbose"),
                       libs_only = FALSE,
                       INSTALL_opts,
                       quiet = FALSE,
                       keep_outputs = FALSE,
                       ...) {
  # first call `utils::install.packages` with all the arguments
  do.call(utils::install.packages, as.list(match.call()[-1]))

  # If rio was installed call the install formats function
  if ("rio" %in% pkgs && isTRUE(interactive())) {
    message <- "
    The rio package works best if you have the packages used to read files installed.
    Would you like to install them now?
    "
    if (.prompt_yes_no(message) == 1L) {
      rio::install_formats()
      unloadNamespace("package:rio")
    }
  }
}


.prompt_yes_no <- function(title) {
  utils::menu(c("Yes", "No"), title = title)
}
