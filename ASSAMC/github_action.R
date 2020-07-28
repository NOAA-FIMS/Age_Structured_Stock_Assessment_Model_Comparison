#### GitHub Action ####
#devtools::install_github("r-lib/usethis")
usethis::use_github_action_check_release()
usethis::use_github_action_check_full()
usethis:::use_github_action('check-release.yaml')
usethis::use_github_links()
usethis::use_github_action_check_release()

#### Action to deploy a pkgdown site ####
## Add auto-generating website documentation for a package
## The yaml file is located in .github/workflows/pkgdown.yml

#install.packages("pkgdown")
usethis::use_pkgdown()
usethis::use_github_action(url = "https://raw.githubusercontent.com/r-lib/actions/master/examples/pkgdown.yaml")

#### Action to deploy a bookdown site ####
## Located in .github/workflow/deploy_bookdown.yml
#install.packages("bookdown")
usethis::use_github_action(url = "https://raw.githubusercontent.com/ropenscilabs/actions_sandbox/master/.github/workflows/deploy_bookdown.yml")

#### Action to deploy a blogdown site ####
## Located in  .github/workflow/deploy_blogdown.yml
usethis::use_github_action(url = "https://raw.githubusercontent.com/ropenscilabs/actions_sandbox/master/.github/workflows/deploy_blogdown.yml")
