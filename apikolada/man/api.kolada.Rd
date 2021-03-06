% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apikolada.R
\docType{class}
\name{api.kolada}
\alias{api.kolada}
\title{Class to interface with web API Kolada}
\source{
Details about the API and its database can be found at the kolada
    API web page \url{https://www.kolada.se/index.php?_p=index/API}
}
\value{
Returns the data from Kolada API and few summary and plot functions
}
\description{
This class is implemented to interface with API Kolada and
    get KPI (Key Performance Indicator) data of different municipalities
    and organizational unis and plot some graphs for further analysis.
    Please refer the methods section of documentation for information
    on each method
}
\details{
Advanced Programming in R - Lab 5
Package to connect with the web API Kolada
}
\section{Methods}{

\describe{
\item{\code{get.kpi.group()}}{Get all KPI groups from API}

\item{\code{get.kpi.id(kpigroup, kpi)}}{Get the KPI ID for a given KPI title from API}

\item{\code{get.kpi.member(kpigroup)}}{Get the KPI indicators within the selected KPI group}

\item{\code{get.muni()}}{Get all the municipality codes from API}

\item{\code{get.muni.id(muni)}}{Get the municipality code for the given municipality title}

\item{\code{get.muni.kpi(kpigroup, muni, kpi, gender)}}{Get KPI data for a municipality based on the chosen KPI indicator from API}

\item{\code{get.ou.id(muni, ou)}}{Get the organizational unit ID for a given organizational unit from API}

\item{\code{get.ou.muni(muni)}}{Get all the organizational units in a municipality from API}

\item{\code{getdata.api(url)}}{Get relevant data from API by passing the url}

\item{\code{plot.muni.kpi(period_val, kpi_val, kpi_label)}}{Plot KPI values of the municipality over the years}

\item{\code{summary.kpi(kpi_df)}}{Summary statistics of municipality KPI values}
}}

\examples{
\dontrun{
obj = api.kolada()
obj$get.muni()
obj$get.ou.muni("Stockholm")
obj$get.kpi.member("Befolkning")
obj$get.muni.kpi("Befolkning", "LinkÃ¶ping", "InvÃ¥nare 65-79 Ã¥r, antal", "T")
obj$plot.muni.kpi(muni.kpi$period, muni.kpi$kpi, "InvÃ¥nare 65-79 Ã¥r, antal")
}
}
