# Benefits Cliff

The Benefits Cliff is a condition in which a pay increase results in an overall loss of combined income and benefits. When a person experiences the Benefits Cliff, they suddenly have less money in their monthly budget to meet their household’s essential needs like food, healthcare, child care, and transportation, as examples.

This repository contains the code used in the Forsyth Futures' reports on the benefits cliff for Forsyth and Buncombe County, North Carolina.  The code allows users to replicate the simulation of benefit levels and the simulation of income levels. Users can also recreate the interactive visualtions. The analysis was conducted in R and the visualizations were created with D3.

Forsyth County, NC Benefits Cliff link: https://abcforsyth.org/bc-forsyth-county/

Buncombe County, NC Benefits Cliff link: https://www.justeconomicswnc.org/buncombe-benefits/

# Repository Organization

#### `benefits_tables` folder

This folder contains R scripts that calculate benefit levels as a function of monthly household income, for incomes from $0 to $7000 per month.  Each file represents a different benefit.  Users should run the `base_table.R` file first and the `federal_poverty_guidelines.R` file second because the other files depend on the output of these two files.  `master_dataset.R` should be run last because it combines the output from the individual files in to one dataframe.  The resulting dataset is used in one visualization, created with the file `benefits_cliff.html`.

#### `tax_liability` folder

This folder simulates after-tax income and after-tax income plus benefits at all income levels from $0 to $7000 in income per month.  `create_tax_data.R` should be run first and it creates the pre-tax dataset.  This dataset is then fed to the tax simulator [Tax-Calculator](https://pslmodels.github.io/Tax-Calculator/).  Tax-calculator is a Python comamnd line tool that simulates federal tax liabilities, including payroll taxes.  The output from tax-calculator is renamed `tax_output.csv`.

`create_tax_outputs.R` takes the output of `create_tax_data.R` and creates the dataset that includes pre-tax income, after-tax income, and after-tax plus benefits income.  This dataset is used in another visualization, created with the file `benefits_income.html`.

#### `income_cliff` folder

This folder has a single script that creates a dataset representing the the number of individuals either below/above the 200% FPL and whether they are enrolled in FNS, and a breakout by race/ethncity and household size for those who are receiving FNS.  The output is used in another plot, created with the file `test_plot_together_refreshed.html` (not available in the Forsyth County folders currently).

The dataset of household incomes comes from the US Census Public Use Microdata, 5 year sample, accessed via [IPUMS](www.ipums.org).  The data is stored in the Forsyth Futures' Google Drive. The R script automatically imports the data from the Drive.
