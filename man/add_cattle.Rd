% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_cattle.R
\name{add_cattle}
\alias{add_cattle}
\title{Add cattle to the DataMuster MongoDB database.}
\usage{
add_cattle(
  RFID,
  MTag,
  category,
  property,
  paddock,
  weaned,
  date = NULL,
  breed = NULL,
  brand = NULL,
  horn = NULL,
  colour = NULL,
  sex = NULL,
  desexed = NULL,
  DOB = NULL,
  birthWeight = NULL,
  registration = NULL,
  damRFID = NULL,
  damMTag = NULL,
  sireRFID = NULL,
  sireMTag = NULL,
  animalID = NULL,
  username = NULL,
  password = NULL
)
}
\arguments{
\item{RFID}{a list of cattle RFID number/s}

\item{MTag}{a list of cattle management tag number/s}

\item{category}{the class of animal (breeding or growing)}

\item{property}{the name of the property to add the animal/s}

\item{paddock}{the name of the paddock to add the animal/s}

\item{weaned}{TRUE if the animal/s are weaned or FALSE if animal/s are not weaned, default is TRUE}

\item{date}{the date that the animal/s were added in date format, default is today's date}

\item{breed}{the animal's breed}

\item{brand}{the animal's brand}

\item{horn}{the animal's horn status (e.g. horn, poll, scur)}

\item{colour}{the animal's colour}

\item{sex}{the animal's sex (male or female)}

\item{desexed}{TRUE if the animal/s are castrated/spayed or FALSE if the animal/s are entire, default is FALSE}

\item{DOB}{the animal's date of bith in date format}

\item{birthWeight}{the animal's weight at birth}

\item{registration}{the animal's society registration number}

\item{damRFID}{the dam's RFID number}

\item{damMTag}{the dam's management tag number}

\item{sireRFID}{the sire's RFID number}

\item{sireMTag}{the sire's management tag number}

\item{animalID}{only applicable to a select few original animals, a unique database identification number}

\item{username}{if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring}

\item{password}{if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access}
}
\value{
a message that indicates the cattle have been successfully added
}
\description{
This function adds individual or groups of cattle to the DataMuster MongoDB database. If you need assistance please email \email{info@datamuster.net.au} to seek help or suggest improvements.
}
\author{
Dave Swain \email{d.swain@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@cqu.edu.au}
}
