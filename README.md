Time series analysis Shiny application

Authors Grzegorz Chadysz, Konrad Archiciński

The main goal of our project was to develop an app which would provide the user with some basic analysis, both graphical and statistical, of time series. The app enables to load a csv or txt file of 2 columns and the displays the observations and values, prints graphs with possibility of saving them, decomposes the time series and shows some basic descriptive statistics along with stationarity test.

Instruction

The zipped file contains server.R and ui.R files. It also contains data folder which has 4 csv files in it. First two correspond to non-seasonal and seasonal time series, third one is a check for whether our defensive mechanism correctly recognizes date and value columns and last one is for checking whether a file with 3 columns would be accepted.

After loading the file, one should press „Show observations&quot; button to see the whole data table of observations or use the slider to pick how many observations the app should show. Summary tab shows some basic descriptive statistics and a ADF stationarity test. In the plot tab one can see two plots, both of which are downloadable. To change the range of plotting the calendar should be used. Adding axis labels and titles to both plots is possible via the most bottom widgets. Decomposition tab is self-explanatory and shows the plots of decomposed time series.

List of techniques used

- Writing own functions in R (including defensive programming)
- Shiny basics
- Creating analytical dashboards
- Use of C++ in R (Rcpp)

We think that our application might find use among people who research time series as it enables the user to take the first step into time series analysis. Having multiple series to analyze, one could just load their files into our app, take a glimpse, get some understanding about his data and what is important - not repeat it every single time he needs to do some time series EDA.