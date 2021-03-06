% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_users.R
\name{add_users}
\alias{add_users}
\title{Add a user to the DataMuster MongoDB database.}
\usage{
add_users(
  user,
  email,
  accesslevel,
  writeaccess,
  property,
  username = NULL,
  password = NULL
)
}
\arguments{
\item{user}{the name of the user to be added to the DataMuster MongoDB Altas server, preferably without a space between the first and last name}

\item{email}{the email address of the user that was used to register via the DataMuster website}

\item{accesslevel}{the MongoDB user access level, either "user" or "admin"}

\item{writeaccess}{the MongoDB user write access level, either "1" for write access or "0" for no write access}

\item{property}{a list of the properties the user will be able to access in MongoDB. If access is "admin", the user will be provided access to all existing properties in the database}

\item{username}{if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring}

\item{password}{if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access}
}
\value{
This function writes directly to the database. A message will appear to indicate the number of documents that have been successfully added to the MongoDB database

a message that indicates that the user has been successfully added
}
\description{
This function adds a user to the DataMuster MongoDB database. You can only access this function if you have read and write permissions.
}
\author{
Lauren O'Connor \email{l.r.oconnor@cqu.edu.au}
}
