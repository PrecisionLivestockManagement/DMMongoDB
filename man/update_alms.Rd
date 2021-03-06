% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_alms.R
\name{update_alms}
\alias{update_alms}
\title{Update cattle allocation to ALMS units in the DataMuster MongoDB database.}
\usage{
update_alms(
  RFID,
  property,
  alms,
  date = NULL,
  username = NULL,
  password = NULL
)
}
\arguments{
\item{RFID}{a list of cattle RFID number/s}

\item{property}{the name of the property to search for}

\item{date}{the date that the new paddock allocation was applied in date formate, default is today's date}

\item{username}{if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring}

\item{password}{if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access}

\item{almsid}{the asset_id of the ALMS unit the cattle are to be allocated}
}
\value{
a message that indicates the cattle have been successfully allocated to an ALMS unit
}
\description{
This function updates allocates individual or groups of cattle to ALMS units in the DataMuster MongoDB database. If you need assistance please email \email{info@datamuster.net.au} to seek help or suggest improvements.
}
\author{
Dave Swain \email{d.swain@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@cqu.edu.au}
}
