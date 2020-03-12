## Resubmission
This is a resubmission. In this version I have expanded the internal fit measure check function to include fit measures for scales with ordered items. I have also fixed a bug within the simulatedAnnealing function where improper items were sometimes used, and another bug within the tabuShortForm function where items were sometimes completely removed from consideration.

## Test Environments

* local Windows 10 Home install, R 3.6.2

* Ubuntu 16.04.5 LTS xenial (travis ci), R 3.6.1

* r-hub and win_check

## R CMD check results
0 errors | 0 warnings | 0 notes

R CMD check succeeded

## Downstream dependencies
There are currently no downstream dependencies for this package.
