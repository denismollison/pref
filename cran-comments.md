## R CMD check results

0 errors | 0 warnings | 1 note

The note is because the functions stv and stv.wig with default settings each create an output directory within the working directory. That seems the most user-friendly choice, and should not be a problem for the package in normal use.

## Other NOTES

The longest-running example may take a little over 5 seconds.
I hope this acceptable: it is in the spirit of the 
package to give real-life examples, and for STV with
equal preferences there's very little choice.

* This is a new release.
