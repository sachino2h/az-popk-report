################################################################################
## qctools Overview
##
## Note:
##
## * The qctools package provides functions to help the scientist review code
##   and deliverables. This file provides a demonstration for these functions.
## * These functions will be helpful to use throughout a project, however this 
##   file is not meant to be versioned or submitted for QC at any point.
## * To illustrate how these functions work in a Git repository, the `demoRepo`
##   from qctools is used. This creates a temporary Git repo with versioned 
##   dummy files.
##
## Purpose:
##
## * Review code changes using the `diff-` functions from qctools.
## * Compare versions of delivarables (tables & figures) using the `compare-`
##   functions from qctools.
## * Leverage git helpers to easily access a file's version history.
##
################################################################################

library(qctools)
library(here)

# Use the demoRepo() function to create a temporary Git repository
repo <- demoRepo()
repo # Notice, this returns the path to the temporary repository
# Set working directory to the temporary repository
setwd(repo)

# View the contents of the repository
rstudioapi::filesPaneNavigate(repo) # Note: The files in `script` are not intended to be run

###################
# Diff Functions
###################

# Review the commit history of `script/data-assembly.R` using gitLog()
file_of_interest <- "script/data-assembly.R"
file_log <- gitLog(file_of_interest)
file_log

# Every time an update to a file is checked into Git, a commit hash is assigned to it
# These hashes can be used to view the file at different points in its version history
# The qctools package has a variety of `diff-` functions that leverage this

# To see the difference between the current version of a file to it's initial version, use diffOriginal()
diffOriginal(file = file_of_interest)
# Note: green shaded lines indicate lines of code that have been added since the first version

# To see the difference between the current version of a file to a specific previous version, use diffPreviousVersions()
diffPreviousVersions(
  file = file_of_interest, 
  previous_version = file_log$last_commit[3]
)

# To see the difference between two past versions of a file, specify both the current and previous version of interest
diffPreviousVersions(
  file = file_of_interest,
  current_version = file_log$last_commit[2],
  previous_version = file_log$last_commit[4]
)

# Note: If lines of code have been removed from the file, they'll appear shaded in red
diffOriginal("script/combine-da.R")

###################
# Compare Functions
###################

# When updating figures or tables, use compareFigures() and compareTables() to 
# compare them to the versions currently checked into the repository.
# Provide either a directory of figure/tables or an individual file 

# Note: compareFigures() will work with only pdf and png files
compareFigures("deliv/figure")
compareFigures("deliv/figure/example-pdf1.pdf")

# Note: compareTables() will work with only tex files
compareTables("deliv/table")

# These can also be used to compare two local files or directories

# To demonstrate, create a second figure folder 
fs::dir_create("deliv/figure2")
fs::file_copy("deliv/figure/example-pdf1.pdf", "deliv/figure2")

# Specify the two directories in the .path_previous and .path_current arguments
compareFigures(.path_previous = "deliv/figure", .path_current = "deliv/figure2")
