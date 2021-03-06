% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_weeklywts.R
\name{calc_weeklywts}
\alias{calc_weeklywts}
\title{Calculate average weekly weights from the DataMuster MongoDB database.}
\usage{
calc_weeklywts(
  RFID = NULL,
  start = NULL,
  end = NULL,
  values = NULL,
  s.d = NULL,
  minwt = NULL,
  location = NULL,
  currentALMS = NULL,
  username = NULL,
  password = NULL
)
}
\arguments{
\item{RFID}{a list of cattle RFID number/s}

\item{start}{a start date to be returned in date format, default is "2014-09-01"}

\item{end}{an end date to be returned in date format, default is today's date}

\item{values}{the minimum number of daily weight values required to calculate an average weekly weight, default is 4}

\item{s.d}{the minimin standard deviation between daily weight values required to calculate an average weekly weight, default is 25}

\item{minwt}{the minimum daily weight (kg) required to be included to calculate an average weekly weight, default is 10}

\item{location}{the filename of the ALMS unit to search for}

\item{currentALMS}{whether or not cattle are currently allocated to an ALMS unit or not}

\item{username}{if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring}

\item{password}{if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access}
}
\value{
a dataframe of cattle RFID numbers and weekly weight statistics
}
\description{
This function calculates average weekly weights from daily ALMS weights for individual or groups of cattle using the DataMuster MongoDB database. If you need assistance please email \email{info@datamuster.net.au} to seek help or suggest improvements.
}
\author{
Dave Swain \email{d.swain@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@cqu.edu.au}
}
