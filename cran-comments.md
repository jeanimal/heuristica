## Test environments
* local: OS X 11.4, R 4.1.1 
* travis-ci: Ubuntu 16.04.6 LTS, R 4.15 
* rhub: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* win-builder: x86_64-w64-mingw32 (64-bit), R (unstable) 2021-08-20 r80804

## R CMD check results
ERRORs: 0
WARNINGS: 0
NOTES: 1 
  New submission
  
  
  Package was archived on CRAN
  
  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2021-06-06 as check problems were not
      corrected in time.
  Maintainer: 'Jean Whitmore <jeanimal@gmail.com>'

  > Package problem was corrected by using correct number of arguments to
  > matrix constructor (which went from warning to error on some OS).
  > https://github.com/jeanimal/heuristica/commit/ad9e6f6a70538321a6dc4ead1409d32d00f72a1b

   Possibly misspelled words in DESCRIPTION:
     Gigerenzer (16:58, 19:5)
     Goldstein (17:7)

  > These are not misspelled.

## Downstream dependencies
None.

