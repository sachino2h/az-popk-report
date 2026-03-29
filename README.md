AZ MeRGE (Metrum Research Group Ecosystem) Project template

This code repository is the latest release (2024-07-16) of the AZ-specific MeRGE code repository for popPK analysis. It includes scripts for exploratory data analysis, model management (using bbr), goodness-of-fit templates, bootstrap, and VPCs. It makes use of renv/pkgr for automatic version handling of R packages for traceability and reproducability.

The script repo is purposed for submission popPK analysis, but can be also be used in earlier phases.

The setup the MeRGE repo consists of the following steps

1.	Create a repository from the template
2.	Setup the repository on Metworx
3.	Installing needed R packages 
4.	Installing bbi
5.	Build a yspec YAML file from AZ data spec XLSX files (using build-yspec.R)

After the setup is completed, the recommended running order of the scripts are:

1.  build-yspec.R           
2.  eda-figures.R
3.  eda-tables.R
4.  model-management.Rmd
5.  model-diagnostics.R
6.  model-overview.R
7.  pk-parameter-key.yaml
8.  parameter-table-base.R
9.  parameter-table-final.R
10. bootstrap.R
11. bootstrap-plot.R
12. parameter-table-boot-final.R
13. forest-plot.R
14. vpc.R
15. run-log-key.yaml
16. run-log.R
17. mrgsolve-validate.R
18. pk-ebe-simulate.R
19. qctools-overview.R

A more comprehensive explanatory guide to MeRGE setup can be found on SPOL in the following link: https://azcollaboration.sharepoint.com/sites/QCPPharmacometrics2/Shared%20Documents/1%20Metworx%20and%20GHE/MeRGE%20code%20repository/MeRGE%20code%20repository%20setup.pdf?csf=1&web=1&e=pFCTlE

