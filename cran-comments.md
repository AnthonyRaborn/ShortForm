## Resubmission
This is a resubmission. In this version I have:

* Added the Tabu search, which includes 5 .R files

* Updated the description file to reflect this addition

* Added more clear authorship and URLs for the ACO functions


## Test Environments

* local Windows 10 Home install, R 3.4.4

* ubuntu 14.04 trusty (travis ci), R 3.4.4

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking dependencies in R code ... NOTE
Unexported objects imported by ':::' calls:
  'lavaan:::lav_partable_full' 'lavaan:::vnames'
  See the note in ?`:::` about the use of this operator.
  
This note is unfortunately necessary due to the reliance of this
unexported functions inside the package.

## Downstream dependencies
There are currently no downstream dependencies for this package.