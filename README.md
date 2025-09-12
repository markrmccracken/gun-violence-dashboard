# Gun Violence Dashboard

An interactive R Shiny web application analyzing mass shooting data in the United States with real-time social media sentiment analysis via Bluesky integration.

## Live Application

[View the dashboard](https://markmccracken.shinyapps.io/gun-violence-dashboard/)

## Overview

This dashboard explores mass shooting incidents in the United States to address key research questions about gun violence patterns, trends, and public discourse. Originally developed as an MBA program project and modernized for 2025 with updated social media integration.

## Features

### Interactive Data Visualization
- **Geographic Mapping**: Interactive leaflet map showing mass shooting locations with year filtering
- **Trend Analysis**: Time series charts with outlier detection and analysis
- **Demographic Analysis**: Breakdown of incidents by shooter characteristics including:
  - Age, race, and gender demographics  
  - Mental health indicators
  - Weapon type and legality
  - Location characteristics

### Real-Time Social Media Integration
- **Bluesky API Integration**: Live word cloud generation from current social media posts
- **Multiple Hashtag Analysis**: Track conversations around #gunviolence, #guncontrol, #gunrights, etc.
- **Sentiment Visualization**: Dynamic word clouds showing current public discourse

## Data Sources

- **Primary Dataset**: Mother Jones mass shooting database
- **Social Media**: Bluesky AT Protocol API for real-time sentiment analysis
- **Coverage**: Mass shooting incidents from 1982-2024

## Research Questions Addressed

1. What is the human cost of gun violence when examining specifically mass shootings?
2. What kind of people contribute most to the casualty and death tolls of mass shootings?
3. What legislation can the U.S. government consider implementing that might help reduce casualties?
4. What are people saying right now in the national conversation around gun violence on social media?

## Technology Stack

- **R/Shiny**: Core application framework
- **Leaflet**: Interactive mapping
- **Plotly**: Interactive charts and visualizations  
- **Bluesky AT Protocol**: Social media API integration
- **shinydashboard**: UI framework

## Installation & Local Development

### Prerequisites
- R (4.0+)
- RStudio (recommended)

### Setup
```r
# Install required packages
install.packages(c("shiny", "shinydashboard", "ggplot2", "leaflet", 
                   "dplyr", "plotly", "wordcloud2", "httr2", "jsonlite", 
                   "tm", "stringr", "RColorBrewer"))

# Clone the repository
git clone https://github.com/yourusername/gun-violence-dashboard.git
cd gun-violence-dashboard

# Create environment file for Bluesky credentials (optional)
# Add your Bluesky handle and app password to .env file:
BLUESKY_USERNAME=your.handle.bsky.social  
BLUESKY_PASSWORD=your-app-password
```

### Running Locally
```r
# Open in RStudio and click "Run App" or:
shiny::runApp()
```

## Project Structure

```
gun-violence-dashboard/
├── app.R                    # Main Shiny application
├── R/
│   └── bluesky_functions.R  # Bluesky API integration functions
├── www/
│   └── MJFull.csv          # Mass shooting dataset
├── .env                    # Environment variables (local only)
├── .gitignore             # Git exclusions
└── README.md              # This file
```

## Key Features & Analysis

### Geographic Patterns
The interactive map reveals concentration patterns in populated areas with the ability to filter by time periods to observe changes in incident distribution.

### Temporal Trends  
Charts show concerning increases in both frequency and severity of mass shooting incidents over recent decades, with options to analyze data with and without statistical outliers.

### Demographic Analysis
Detailed breakdowns reveal patterns in shooter characteristics and their relationship to victim counts, informing discussions about prevention and policy responses.

### Social Media Sentiment
Real-time analysis of public discourse provides insight into current conversations and sentiment around gun violence policy and prevention.

## Development Notes

This application was modernized in 2025 from an original MBA project, with key updates including:
- Migration from Twitter API to Bluesky AT Protocol
- Updated R packages and improved error handling
- Enhanced cross-platform compatibility
- Improved data processing and visualization

## Contributing

This is an academic/research project. For suggestions or issues, please open an issue on GitHub.

## License

This project is for educational and research purposes. Data sources retain their original licensing.

## Acknowledgments

- Mother Jones for maintaining the comprehensive mass shooting database
- Johns Hopkins Carey Business School MBA program
- Bluesky for an accessible social media API
- R/Shiny community for excellent visualization frameworks

## Contact

Mark McCracken - mmccrac2@alumni.jh.edu
Project Link: [https://github.com/markrmccracken/gun-violence-dashboard](https://github.com/markrmccracken/gun-violence-dashboard)