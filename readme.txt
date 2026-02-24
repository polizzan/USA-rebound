# Instructions for using the repository `USA-rebound` to replicate the analysis presented in 'The U.S. Life Expectancy Rebound: Reasons for Optimism?'
1. Download the repository `USA-rebound` from OSF and unzip.
2. Open the file `USA-rebound.Rproj` in RStudio.
3. Open and run the file `99_setup.R` in the `scripts` folder. This file installs all R packages necessary for the data analysis and automatically creates the required folders `in-data`, `out-data`, and `out-figures`. Keep RStudio open.
4. Open your internet browser and download the 2019-2024 Multiple Cause of Death Files from the National Center for Health Statistics [website](https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm#Mortality_Multiple). Place the six `.zip` files into the `in-data` folder and unzip.
5. Back in RStudio, open and run the file `00_main.R`. This file automatically downloads the required mid-year population estimates from the U.S. Census Bureau and the required U.S. 2000 standard population figures. (Should the automatic download fail, follow the links in the `00_main.R` file and download the relevant `.xlsx` and `.csv` files manually. Place these files into the `in-data` folder.) The file `00_main.R` automatically executes the files `01_deaths.R` and `02_exposures.R`, which prepare the death and mid-year population data, respectively. The file `00_main.R` also automatically executes the files `03_ASDR.R` and `04_plot.R`, which calculate and plot the age-standardized death rates for 17 causes of death. The plot of the age-standardized death rates is automatically saved in the folder `out-figures` in both `.pdf` and `.svg` format.



