# US Mortgage Approvals Case Study
<div align="center">
  <a href="https://d224eigjcmsomk.cloudfront.net/Pictures/1024x536/7/0/0/1969700_shutterstock_1397767763_281383.jpg">
    <img src="https://d224eigjcmsomk.cloudfront.net/Pictures/1024x536/7/0/0/1969700_shutterstock_1397767763_281383.jpg" alt="" width="500" height="250">
  </a>
</div>
This repository contains a detailed report and the associated code for a case study on mortgage approvals in the United States, focusing on a dataset of mortgage applications from a (unknown) 1990 U.S. city. The analysis explores factors affecting the outcome of mortgage applications, including financial ratios, demographic details, and the 1989 state unemployment rate in the applicant's industry.

## Overview

The study employs logistic regression to model the binary outcome of mortgage application approvals, with an emphasis on understanding how various variables such as housing expenses to income ratio (HIR), loan-to-value ratio (LVR), mortgage credit score (MCS), and demographic characteristics influence mortgage approval decisions.

## Contents

- **Report.pdf**: A comprehensive analysis report detailing the exploratory data analysis, modeling approach, results, limitations, and conclusions of the US mortgage approvals case study.
- **case_study_us_mortgage.R**: The R script used for data analysis, including data cleaning, exploratory data analysis, logistic regression modeling, model diagnostics, and visualization of results.

## Key Findings

- Financial ratios such as HIR, LVR, and other debts to income ratio (ODIR) play significant roles in the approval process, with higher ratios generally leading to lower approval probabilities.
- Demographic factors, including self-employment, marital status, and ethnicity, also influence mortgage approval outcomes, with notable disparities observed.
- The logistic regression model highlighted the importance of mortgage credit scores, where applicants with better scores had higher odds of approval.

## Usage

To replicate the analysis or explore the dataset further:

1. **Prerequisites**: Ensure you have R installed on your machine, along with necessary packages such as `ggplot2` for visualization, `dplyr` for data manipulation, and `glm` for logistic regression modeling.
2. **Run the Analysis**: Open the `AnalysisCode.R` script in RStudio or your preferred R environment, and execute the code to perform the analysis. You may need to adjust file paths based on your directory structure.

## Contributing

Contributions to this project are welcome. Please open an issue to discuss proposed changes or improvements before making a pull request.

## License
This project is licensed under the MIT License - see the LICENSE file for details.

