# Airbnb Listings Histogram - Shiny App

## Overview
This Shiny application visualizes Airbnb listing prices in Rio de Janeiro, Brazil, using a histogram. Users can customize the number of bins, the histogram color, and the plot title.

## Features
- **Data Cleaning**: The app processes Airbnb price data, removing non-numeric characters and filtering outliers.
- **Interactive UI**:
  - Adjust the number of bins in the histogram.
  - Choose the color of the histogram.
  - Modify the title of the histogram.
- **Responsive Plot**: The histogram updates dynamically based on user input.

## Installation
To run this application, ensure you have R and the following packages installed:

```r
install.packages(c("shiny", "tidyverse", "readr"))
```


## Data Source
The dataset (`data/listings.csv`) contains Airbnb listings and their prices. The data undergoes preprocessing to ensure consistency and accuracy.

## Code Structure
- **UI (`ui`)**: Defines the layout and input controls.
- **Server (`server`)**: Handles data processing and visualization.
- **Shiny App (`shinyApp(ui, server)`)**: Runs the application.

## Contributions
Feel free to fork this repository and submit pull requests with improvements!

## License
This project is licensed under the MIT License.