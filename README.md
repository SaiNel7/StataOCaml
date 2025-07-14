# StataOCaml

A statistical analysis library and command-line tool for OCaml, inspired by Stata. StataOCaml provides comprehensive data exploration, statistical analysis, and visualization capabilities for CSV datasets through an interactive command-line interface.

## Features

### Core Statistical Analysis
- **Descriptive Statistics**: Mean, median, mode, standard deviation, min/max, quartiles (IQR)
- **Correlation & Covariance**: Sample covariance and correlation coefficients between variables
- **Data Exploration**: Column browsing, head/tail viewing, summary statistics tables

### Linear Regression
- **Bivariate Regression**: Full linear regression analysis with:
  - Coefficient estimation (β₀, β₁) 
  - Model fit metrics (R², MAE, MSE)
  - Statistical inference (t-values, p-values, confidence intervals)
  - Prediction capabilities for new data points

### Data Visualization
- **Histograms**: Customizable bin counts with frequency distributions
- **Scatter Plots**: ASCII-based scatter plots for variable relationships
- **Heatmaps**: Density visualizations with color-coded intensity
- **Box-and-Whisker Plots**: Distribution visualization with quartiles and outliers

### Interactive Interface
- **Command-Line Tool**: Single-line commands for all operations
- **Error Handling**: Comprehensive error messages for invalid data or operations
- **Data Browsing**: Paginated viewing for large datasets
- **Help System**: Built-in command reference and usage examples

## Usage

```bash
dune exec bin/main.exe data/ < csv file >
```

Once loaded, use commands like:
- `mean column_name` - Calculate column mean
- `regress y_col x_col` - Perform linear regression
- `scatter x_col y_col` - Generate scatter plot
- `summary column_name` - Get comprehensive statistics
- `help` - View all available commands

## Dependencies

- OCaml with Dune build system
- CSV library for data parsing
- ANSITerminal for colored output
- BatList for extended list operations

StataOCaml makes statistical analysis accessible through simple commands while providing the depth and rigor expected in statistical software.