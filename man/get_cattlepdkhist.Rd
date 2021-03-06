% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_cattlepdkhist.R
\name{get_cattlepdkhist}
\alias{get_cattlepdkhist}
\title{Retrieve cattle paddock history information from the DataMuster MongoDB database.}
\usage{
get_cattlepdkhist(
  property,
  paddock = NULL,
  archives = NULL,
  start = NULL,
  end = NULL,
  username = NULL,
  password = NULL
)
}
\arguments{
\item{property}{the name of the property to search for}

\item{paddock}{the name of the paddock/s to search for. If not value is entered, then all paddocks are returned}

\item{archives}{if TRUE, the list of animals returned will include those that are no longer on the property}

\item{start}{a start date to be returned in date format, default is "2014-09-01"}

\item{end}{an end date to be returned in date format, default is today's date}

\item{username}{if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring}

\item{password}{if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access}
}
\value{
a list of cattle RFID numbers and associated cattle paddock history
}
\description{
This function provides a search tool to retrieve cattle paddock history from the Cattle collection in the DataMuster MongoDB database. If you need assistance please email \email{info@datamuster.net.au} to seek help or suggest improvements.
}
\author{
Anita Chang \email{a.chang@cqu.edu.au}
}
