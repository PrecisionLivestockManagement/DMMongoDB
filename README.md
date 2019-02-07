# DMMongoDB

A package to enable interactions with the DataMuster cattle database.

To download the package firstly install the devtools package and then run:

install.github("PrecisionLivestockManagement/DMMongoDB")

Once the package is downloaded install the library. Before you can use the package you will need to get access credentials by emailing info@datamuster.net.au and run the dmaccess function using dmaccess('username') and then enter your allocated password at the prompt. If you update the package you may need to restart RStudio and run the dmaccess('username') to reset the password. 

The functions provide access to specific cattle data and the help files provide details on the what each of the functions can do. This is an ongoing project and is specifically focussed on researchers, data analysts and partners of DataMuster and is focussed on their specific needs. If there are new opportunities for standard data processign tools based on the DataMuster outputs from this package we are keen to integrate them as tools for further development.
