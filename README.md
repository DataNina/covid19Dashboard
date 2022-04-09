# covid19Dashboard
I developed an interactive R Shiny Dashboard for analysing Covid19 data which is automatically updated on a dialy basis.

## Content
The dashboard allows to distiguish between between "confirmed"/"recovered"/"active"/"death"/"new" cases for different countries and time periods.
In addition, also the reproduction number and the 7-days incidence is visulaied depending on the users choice for the country or countries. 

## Datasource
Row data regarding covid19 is provided by the John Hopkins University and can be downloaded on github (see https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data). Row data for the total population is provided by the World Bank and covers the year 2019.

## Datapreparation
The reproduction rate is calcualted taking into account the new infections of today and the last seven days by divinding the sum of the second half and the first half of the values. The 7-days incidence per 100 000 inhabitants is calculated by using the sum of the new infections over the last seven days by diving through the total populatio multiplied by 100 000. 

