----
# AirQualityWatch
## A Shiny app which visualises air pollution data collected in the Czech Republic from 2013 to 2019. 
### The data is available from [European Environmental Agency](https://www.eea.europa.eu/themes/air). 
* The file Stations.csv contains information about each station.
* There is one CSV file for each station and each pollutant (PM2.5, PM10, SO2 and NO2) measured at the station (not every pollutant is measured at every station). 
* The filenames are of the form <EoICode>_<PollutantCode>.csv. 
* Each row of these files contains hourly measurements of that pollutant at that station. 
* For some stations there only is data for a subset of the time points.

**The current air quality standards in the EU are:**

1. Fine particulates (PM2.5) Yearly average of at most 25µg/m3.
2. Particulates (PM10) Daily average exceeding 50µg/m3 observed on at most 35 days a year, and
yearly average of at most 40µg/m3.
3. Sulphur dioxide (SO2) Hourly concentration exceeding 350µg/m3 for at most 24 hours per year, and average daily concentration exceeding 125µg/m3 on at most 3 days per year.
4. Nitrogen dioxide (NO2) Hourly concentration exceeding 200µg/m3
for at most 18 hours per year, and average yearly concentration of at most 40µg/m3.

Our task consists of developing a Shiny app visualising the data with the following functionality.

• The core functionality of the app is to produce time series plots of relevant summary statistics against time.

• The user should be able to select for which pollutant and for which stations (at most 4) the quantities should be shown.

• The user should either be informed if the pollutant is not available for this measuring station or, after selecting the
pollutant, the app should only show stations at which the pollutant was measured.

• The user should be able to decide what aggregation to plot. This should include the following aggregations:

– Raw hourly data (no aggregation);

– Daily averages;

– Daily maxima;

– Number of hours per day for which a given threshold is exceeded;

– Number of hours per year for which a given threshold is exceeded; and

– Number of days per year for which the daily average concentration exceeds a given threshold.
For aggregations involving a threshold, the user should be able to choose this threshold.
1

• The user should be able to choose how time is to be handled and what the x-Axis should represent. Choices should
include

– Calendar time;

– Date within the year (going from Jan 1st to Dec 31st);

– Day or hour within the week (going from 0 to 7 (days) or 0 to 168 (hours)); and
– Hour in the day (going from 0 to 24 (hours)).
For calendar time the plot should be a line plot, but for the other three choices the plot should be a scatter plot.

If data is to be aggregated over years then the latter three plots are not meaningful. The app should handle this suitably.

• If the choice of pollutant and aggregation matches a criterion from the EU air quality standard, then the plot should
show a horizontal line at the threshold for that criterion. For example, if the user wants to plot the raw hourly data for
SO2, then a horizontal line should be drawn at y = 350.

• Make sure that suitable axis labels and legends are used.
• The app should also show a second plot with the location of the measuring station(s). You can either use the package
maps or ggmaps for this. The code below shows the location of all measuring stations together with a map of the
Czech Republic (assuming the data from __Stations.csv is stored in a data frame stations).


• Below the plot there should be showing the data used in the plot in wide format (i.e. columns should correspond
to measuring stations).

• The user should be able to download the table as a CSV file and the plots and table together as a Word document
(generated using rmarkdown).
We can either use classical R plots or ggplot2
