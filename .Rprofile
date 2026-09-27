# Project-level R profile.
#
# Deliberately minimal: a research compendium must behave identically in
# interactive sessions, `Rscript`, `R CMD check` and CI. Nothing here changes
# numerical results, installs packages or defines global objects.

# Use the user's own profile as well, if they have one.
if (file.exists("~/.Rprofile")) {
  source("~/.Rprofile")
}

if (interactive()) {
  options(
    warnPartialMatchArgs = TRUE,
    warnPartialMatchAttr = TRUE,
    warnPartialMatchDollar = TRUE
  )
}
