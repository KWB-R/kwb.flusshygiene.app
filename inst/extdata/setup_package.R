# Set the name of your (!) new package
package <- "kwb.flusshygiene.app"

# Set the path to your (!) local folder to which GitHub repositories are cloned
repo_dir <- "~/github-repos"

# Set the path to the package directory
pkg_dir <- file.path(repo_dir, package)

# Create directory for R package
kwb.pkgbuild::create_pkg_dir(pkg_dir)

# Create a default package structure
withr::with_dir(pkg_dir, {kwb.pkgbuild::use_pkg_skeleton(package)})

author <- list(
  name = "Wolfgang Seis",
  orcid = "0000-0002-7436-8575"
)

description <- list(
  name = package,
  title = "R Package Implementing the Flusshygiene Application",
  desc  = paste(
    "R Package Implementing the Flusshygiene Application. This package",
    "provides the functions required to setup the web application as it was",
    "developed during the KWB project 'Flusshygiene'."
  )
)

setwd(pkg_dir)

kwb.pkgbuild::use_pkg(
  author,
  description,
  version = "0.0.0.9000",
  stage = "experimental"
)

