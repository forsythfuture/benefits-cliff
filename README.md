# Benefits Cliff

This repo contains the code used in the Forsyth Futures reports on the benefits cliff for Forsyth and Buncombe County, North Carolina.  The code allows users to replicate the simulation of benefit levels and the simulation of income levels.  Users can also recreate the interactive visualtions.  The analysis was conducted in R and the visualizations were created with D3.

<!-- TOC START -->
- [Organization of repo](#Organization-of-rep)
- [Methodology](#Methodology)
   - [NC Child Care Subsidy](#NC-child-care)
   - [FNS](#fns)
   - [Housing Choice Voucher](#housing)
   - [Medical](#medical)
   - [TANF](#tanf)
   - [WIC](#wic)
<!-- TOC END -->

<a name="Organization-of-rep"/>

## Organization of repo 

`benefits_tables` folder:

This folder contains R scripts that calculate benefit levels as a function of monthly household income, for incomes from $0 to $7000 per month.  Each file represents a different benefit.  Users should run the `base_table.R` file first and the `federal_poverty_guidelines.R` file second because the other files depend on the output of these two files.  `master_dataset.R` should be run last because it combines the output from the individual files in to one dataframe.  The resulting dataset is used in one visualization, created with the file `benefits_cliff.html`.

`tax_liability` folder:

This folder simulates after-tax income and after-tax income plus benefits at all income levels from $0 to $7000 in income per month.  `create_tax_data.R` should be run first and it creates the pre-tax dataset.  This dataset is then fed to the tax simulator [Tax-Calculator](https://pslmodels.github.io/Tax-Calculator/).  Tax-calculator is a Python comamnd line tool that simulates federal tax liabilities, including payroll taxes.  The output from tax-calculator is renamed `tax_output.csv`.

`create_tax_outputs.R` takes the output of `create_tax_data.R` and creates the dataset that includes pre-tax income, after-tax income, and after-tax plus benefits income.  This dataset is used in another visualization, created with the file `benefits_income.html`.

`income_cliff` folder:

This folder has a single script that creates a dataset representing the the number of individuals either below/above the 200% FPL and whether they are enrolled in FNS, and a breakout by race/ethncity and household size for those who are receiving FNS.  The output is used in another plot, created with the file `test_plot_together_refreshed.html` (not available in the Forsyth County folders currently).

The dataset of household incomes comes from the US Census Public Use Microdata, 5 year sample, accessed via [IPUMS](www.ipums.org).  The data is stored in the Forsyth Futures' Google Drive. The R script automatically imports the data from the Drive.

<a name="Methodology"/>

## Methodology

*Benefit amounts and eligibility depend on factors beyond income.  In calculating benefit amounts, we made assumptions about these varying factors and below are our assumptions.*

<a name="NC-child-care"/>

### NC Child Care Subsidies

An overview of the eligibility requirements for subsidies is on the [state's website](https://ncchildcare.ncdhhs.gov/Services/Financial-Assistance/Do-I-Qualify).  The administrative regulations are [also available](http://reports.oah.state.nc.us/ncac.asp?folderName=%5CTitle%2010A%20-%20Health%20and%20Human%20Services%5CChapter%2010%20-%20Subsidized%20Child%20Care).  We used the administrative regulations when conducting our analysis.

**Eligibility**

* [Initial eligibility](http://reports.oah.state.nc.us/ncac/title%2010a%20-%20health%20and%20human%20services/chapter%2010%20-%20subsidized%20child%20care/10a%20ncac%2010%20.1002.html)
    * 0 to 5 year olds: household income less than 200% of the federal poverty guidelines
    * 6 to 12 year olds: household income less than 133% of the federal poverty guidelines
 * [Continued eligibility](http://reports.oah.state.nc.us/ncac/title%2010a%20-%20health%20and%20human%20services/chapter%2010%20-%20subsidized%20child%20care/10a%20ncac%2010%20.1007.html)
    * All ages: household income less than 85% of the state median income

Since our focus is on when families lose benefits, we use the continued eligibility threshold when simulating the benefits cliff.

**Market value of benefit**

The market value of NC Child Care subsidies is based on the [2019 subsidized NC market rate](https://ncchildcare.ncdhhs.gov/Portals/0/documents/pdf/R/Revised-8-16-Market_Rate_Centers_Eff-10-1-18.pdf?ver=2018-08-28-105655-863) for 4-star child care centers in Forsyth County.

Subsidy recipients must also [pay 10% of their gross income](http://reports.oah.state.nc.us/ncac/title%2010a%20-%20health%20and%20human%20services/chapter%2010%20-%20subsidized%20child%20care/10a%20ncac%2010%20.1101.html) as a copay.  Therefore, to calculate the market value of the benefit, we subtract 10% of household income from the 2018 NC market rate of the subsidy.

We did not incorporate other child care programs such as NC PreK because children in programs such as these are only in the program for a year.  As a result, families cannot phase out due to passing an income threshold.  Instead, they phase out of the program when their year expires.

<a name="fns"/>

### Food and Nutrition Service (FNS)

Our information on FNS came from the [program manual](chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://fns-prod.azureedge.us/sites/default/files/resource-files/COLA%20Memo%20FY2022%20Without%20Maximum%20Allotments.pdf).

**Eligibility**

The [household income threshold](https://www.ncdhhs.gov/assistance/low-income-services/food-nutrition-services-food-stamps) for FNS benefits is 130% of the federal poverty guidelines for most households and 200% for those categorically eligible.  Since most recipients are not categorically eligible, we use the 130% threshold.

**Market Value of the Benefit**

FNS beneficiaries receive benefits proportional to their monthly net income.   To calculate net income, we started with gross income and take the following deductions, which are outlined in the [benefits level manual](chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://policies.ncdhhs.gov/divisional/social-services/food-and-nutrition-services/policy-manuals/fns-360-determining-benefit-levels.pdf).  All citations below at to the benefits level manual.
* [Excess shelter deduction](https://dssmanuals.mo.gov/food-stamps/1115-000-00/1115-035-00/1115-035-25/) is shelter and/or utility expenses exceeding 50% of the monthly income after other deduction have been subtracted, with a max of $597, 360.01(F).
  * Utility allowances based on the table in 360.01(A);
  * Rent for all families is estimated at $600, which represents the 25th percentile rental value in Forsyth County;
* Standard deduction based on the table in 360.01(B);
* Earned income deduction of 20%, 285.04(B)(12);
* Dependent care deduction of $200 per child per month, 360.01(E);

We can determine the benefit amount once net income is calculated.  In general, households are expected to spend 30% of their own net income on food, 364.04(E)(27), with FNS covering the rest. So, we take the maximum monthly income allotment, as specified in 360.02(B), and subtract 30% of net income from this amount to arrive at the value of the benefit.

One and two person households must receive benefits of at least $20 to receive anything, 285.04(E)(28).

More information on calculating FNS benefits, called SNAP federally, can be found in this review by the [Center on Budget and Priorities](https://www.cbpp.org/research/food-assistance/a-quick-guide-to-snap-eligibility-and-benefits).

<a name="housing"/>

### NC Housing Choice Vouchers

**Eligibility**

The initial eligibility threshold is 50% of an area's median income.  But, once a household qualifies, they continue receiving the voucher.  Since families must pay 30% of their income in rent, they ease themselves off the program when their share of rent equals their actual rent payments.

**Market Value of the Benefit**

The market value of the benefit is the market value of the rental unit minus the voucher recipient's share of rent.  [Fair market rental rates](https://www.huduser.gov/portal/datasets/fmr.html#2021) for reimbursement are capped by HUD.  Since we know no rental rates are above these amounts, we assume that rental rates are 80% of the fair market rental.

Calculating the recipient's share of rent first requires calculating the recipient's total tenant payment.  Information on the total tenat payment and the recipient's share comes from the [HUD manual](chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://www.hud.gov/sites/documents/DOC_35649.PDF).

Total tenant payment is the greater of (HUD manual, pg. 5-67):
* 30% of monthly adjusted income,
* 10% of monthly gross income, or
* $25.

We make the following deductions and assumptions in calculating monthly adjusted income:
*  Dependent deduction: $480 year ($40 / month) for each child under 18 (HUD manual, pg. 5-41);
*  Child Care Deduction: $4000 per year / per child ($333.333 per month), but 10 year old in three person home will not have any child care costs;
  * The value comes from the example on page 5-43

To calculate tenant rent, we must subtract the utility allowance from the total tenant payment (HUD manual, pg. 5-67).  The [average Forsyth County utility allowance](https://www.huduser.gov/portal/datasets/assthsg.html#null) in 2021 was $174, so all families were assumed to receive this allowance.

Now we can calculate the fair market value of the benefit, which is the rental rate minus the tenant rent.

<a name="medical"/>

### Medical

Medical is a combination of three health insurance programs: (1) Medicad for Families with Dependant Children, (2) Medicaid for Infants and Children, and (3) NC Health Choice.

**Eligibility**

Income eligibility thresholds for all programs were retrieved from the 2020 [Basic Medicaid Eligibility](chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://files.nc.gov/ncdma/documents/files/Basic-Medicaid-Eligibility-Chart-2020.pdf) chart.

**Market Value of the Benefit**

The market value of all insurance programs were assumed to equal the equivalent price of a 2020 silver plan in Forsyth County on the ACA marketplace.  We pulled plan prices from the Kaiser Family Foundation's [health insurance marketplace calculator](https://www.kff.org/interactive/subsidy-calculator/).

In calculating the value of the silver plan, all adults were assumed to be 30 years old and non-tobacco users.  Children's ages depend on family size and break down as follows:
* 1 child: 2 years old,
* 2 children: 2 and four years old,
* 3 children: 2, 4, and 10 years old.

<a name="tanf"/>

### Work First Cash Assistance (TANF)

Our information for Work First comes from the [NC TANF State Plan](chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://files.nc.gov/ncdhhs/documents/files/2019-2022%20TANF%20STATE%20PLAN%20DRAFT.pdf), hereinafter "State Plan".

**Eligibility**

Only families with children 17 or younger are eligible for Work First Financial Assistance, State Plan pg. 12. We assume all children are 17 or younger.

**Market Value of the Benefit**

The benefit's market value is the payment amount.  This amount is 50% of the difference between total countable income and the need standard listed on pg. 18 of the State Plan.  But, payments must be at least $25.

<a name="wic"/>

### Women, infants, and children (WIC)

**Eligibility**

Mothers and children up to 5 [can receive WIC benefits](https://www.nutritionnc.com/wic/) if their household income is less than 185% of the federal poverty guidelines.  We assume that the parent is a mother in single parent households.  The ages of children are assumed as follows:
* 1 child: 2 years old,
* 2 children: 2 and four years old,
* 3 children: 2, 4, and 10 years old.

Therefore, one child gets benefits in one child households and two children receive benefits in two and three child households.

**Market Value of the Benefit**

Average monthly food benefits per person in 2021 for North Carolina were $31.85.  To verify, go to the [USDA's WIC data site](https://www.fns.usda.gov/pd/wic-program), scroll down to "Annual State Level Data:", and then open the dataset labeled "Average Monthly Food Cost Per Person."  We assume all eligible family members receive WIC benefits in this amount.
