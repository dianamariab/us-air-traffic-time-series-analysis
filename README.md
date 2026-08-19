# US Air Traffic Time Series Analysis

This project was developed as part of my Time Series Analysis coursework. The main objective was to analyze the evolution of US air traffic and explore its relationship with economic variables using time series methods in R.

The project consists of two parts: a univariate analysis focused on air traffic and a multivariate analysis that examines the relationship between air traffic, oil prices and the US unemployment rate.

## Tools Used

- R
- RStudio
- Time series and econometric packages in R

## Project Overview

The project combines time series analysis, forecasting and econometric methods to study US air traffic over time.

The first part focuses on understanding and forecasting air traffic as an individual time series, while the second part extends the analysis by including oil prices and unemployment as additional economic variables.

## Univariate Analysis

The univariate analysis focuses on monthly US air traffic data.

The main steps included:

- exploring the evolution of air traffic over time
- identifying trend and seasonality
- analyzing autocorrelation patterns
- testing for stationarity
- examining the structural break associated with the COVID-19 period
- estimating and comparing SARIMA models
- checking model residuals
- generating forecasts and evaluating their accuracy

Several stationarity tests were used, including ADF, KPSS and Phillips-Perron. Additional tests were used to examine seasonal stationarity and possible structural changes in the series.

Different SARIMA specifications were compared using information criteria and residual diagnostics before selecting the final forecasting model.

### Univariate Analysis Workflow

![Univariate Analysis Workflow](images/univariate-analysis-workflow.png)

## Multivariate Analysis

The second part of the project extends the analysis to investigate the relationship between three monthly time series:

- US air traffic
- WTI oil price
- US unemployment rate

These variables were selected to explore how air travel may interact with broader economic conditions.

The analysis included:

- descriptive statistics and correlation analysis
- seasonal adjustment
- stationarity testing
- cross-correlation analysis
- lag selection
- cointegration testing
- VECM/VAR modelling
- Granger causality testing
- model diagnostics
- impulse response functions (IRF)
- forecast error variance decomposition (FEVD)

The analysis distinguishes between short-term dynamics and possible long-term relationships between the variables.

### Multivariate Analysis Workflow

![Multivariate Analysis Workflow](images/multivariate-analysis-workflow.jpeg)

## Methods Used

Some of the main statistical and econometric methods used in the project were:

- ADF, KPSS and Phillips-Perron stationarity tests
- HEGY seasonal unit root test
- Zivot-Andrews structural break test
- SARIMA modelling
- Johansen cointegration test
- VAR and VECM models
- Granger causality
- Impulse Response Functions (IRF)
- Forecast Error Variance Decomposition (FEVD)
- residual diagnostic tests

## Repository Structure

```text
us-air-traffic-time-series-analysis/
│
├── data/
│   ├── USCarrier_Traffic.csv
│   └── date_multivariate.csv
│
├── R/
│   ├── univariate_analysis.R
│   └── multivariate_analysis.R
│
├── images/
│   ├── univariate-analysis-workflow.png
│   └── multivariate-analysis-workflow.jpeg
│
└── README.md
```

The `data` folder contains the datasets used in the analysis, while the `R` folder contains the scripts for the univariate and multivariate parts of the project. The `images` folder contains the workflow diagrams used to summarize the main stages of each analysis.

## What I Learned

This project helped me better understand how time series data can be analyzed beyond simple trends and descriptive statistics.

I gained practical experience working with time series in R, testing statistical assumptions, comparing forecasting models and interpreting relationships between multiple economic variables.

The project also helped me understand the importance of checking stationarity, seasonality, structural changes and model diagnostics before interpreting results or generating forecasts.

## Project Context

This project was developed as part of my university coursework in Time Series Analysis and was completed in a team of three students.
