## Resubmission

This is a major release (1.0.0) that standardizes the arguments of the
package's three primary algorithm functions so equivalent options (e.g.
item/factor specification, iteration limits, the search criterion) are
named and typed consistently across all three; two functions were renamed
to match the third's naming convention (`antcolony.lavaan()` ->
`antColony()`, `tabuShortForm()` -> `tabuSearch()`). It also fixes several
crashes, most notably one triggered whenever a candidate model's fit value
could not be computed. See NEWS.md for the full list of changes.

## Test Environments

* local MacOS Darwin Kernel Version 25.5.0

* GitHub actions (Docker, macOS-13, macOS-arm64, windows, ubuntu-latest) and check_win_devel

## R CMD check results
0 errors | 0 warnings | 0 notes

R CMD check succeeded

## Downstream dependencies
There are currently no downstream dependencies for this package.
