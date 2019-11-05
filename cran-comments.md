## Test environments
* Ubuntu Linux 16.04 LTS
* Windows 10

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking CRAN incoming feasibility

  The Date field is not in ISO 8601 yyyy-mm-dd format.

## R submission reviewer comments

### v0.0.2

Thanks, please explain all acronyms (e.g. CMap) in your Description text to avoid misunderstandings. Do you mean a connectivity map?
You have
sheet <- "by cmap name and cell line"
in your function.
What will happen if the user's sheet has another name?

Please fix and resubmit. 

### v0.0.3


Flavor: r-devel-windows-ix86+x86_64
Check: for unstated dependencies in examples, Result: WARNING
  Warning: parse error in file 'lines':
  6: unexpected symbol
  23: #molecule position with respect of dotted line is the specificity of the molecule itself:
  24: left side
  
