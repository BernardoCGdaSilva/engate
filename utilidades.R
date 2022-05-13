### Set-up

library(devtools)
library(testthat)
check()

#_____________________________________________________________________________________________________________________
### Construção de Site

# Run once to configure package to use pkgdown
usethis::use_pkgdown()
# Run to build the website
pkgdown::build_site()

# If you’re using GitHub, we also recommend setting up GitHub actions to automatically build and publish your site:
usethis::use_pkgdown_github_pages()

