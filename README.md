# Benefits Cliff
### Special issue report on the benefits cliff.

This repo contains the code used in the Forsyth Futures special issue report on the benefits cliff.  The code allows users to replicate the simulation of benefit levels and the simulation of income levels.  Users can also recreate the interactive visualtions.  The analysis was conducted in R and the visualizations were created with D3.

<!-- TOC START -->
- [Organization of repo](##Organization-of-rep)
- [Methodology](##Methodology)
<!-- TOC END -->

## Organization of repo

`benefits_tables` folder:

This folder contains R scripts that calculate benefit levels as a function of monthly household income for 2019, for incomes from $0 to $6000 per month.  Each file represents a different benefit.  Users should run the `base_table.R` file first and the `federal_poverty_guidelines.R` file second because the other files depend on the output of these two files.  `master_dataset.R` should be run last because it combines the output from the individual files in to one dataframe.  The resulting dataset is used in the first visualtion, created with the file `benefits_cliff.html`.

`tax_liability` folder:

This folder simulates 2019 after-tax income and after-tax income plus benefits at all income levels from $0 to $6000 in income per month.  `create_tax_data.R` should be run first and it creates the pre-tax dataset.  This dataset is then fed to the tax simulator [Tax-Calculator](https://pslmodels.github.io/Tax-Calculator/).  Tax-calculator is a Python comamnd line tool that simulates federal tax liabilities, including payroll taxes.  The output from tax-calculator is renamed `tax_output.csv`.

`create_tax_outputs.R` takes the output of `create_tax_data.R` and creates the dataset that includes pre-tax income, after-tax income, and after-tax plus benefits income.  This dataset is used in the second visualization, created with the file `benefits_income.html`.

`income_cliff` folder:

This folder has a single script that creates a dataset representing the cumulative sum of the number of people at each point of household income, faceted by household size.  The output is used in the third plot, created with the file `cliff_cdf.html'.

The dataset of household incomes comes from the 2017 US Census Public Use Microdata, 5 year sample, accessed via [IPUMS](www.ipums.org).  The data is stored in a public Amazon AWS s3 bucket at this address: https://forsyth-futures.s3.amazonaws.com/total_income_counts.csv.gz. The R script automatically imports the data from AWS, there is no need to manually import.  The codebook, available as `total_income_coounts.pdf` shows the variables that were imported from IPUMS.

## Methodology
