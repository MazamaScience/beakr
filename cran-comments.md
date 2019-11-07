### Test Enviroments
* Local Ubuntu-Linux 19.04, R 3.6.1
* Rhub Windows Server 2012, R-devel, Rtools4.0, 32/64 bit (experimental)
* check_win_release
* check_win_oldrelease 
* check_win_devel
* Rhub macOS 10.11 El Capitan, R-release (experimental)
* Rhub check_for_cran()
* travis-ci

### R CMD check via Rstudio results 
* 0 errors ✔ | 0 warnings ✔ | 0 notes ✔
* R CMD check succeeded

### R CMD check via CLI 
* When building via CLI, an error is produced for creating PDF's on Ubuntu 19.04:
  ```
  LaTeX errors when creating PDF version.
  This typically indicates Rd problems.
  * checking PDF version of manual without hyperrefs or index ... ERROR
  Re-running with no redirection of stdout/stderr.
  Hmm ... looks like a package
  Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  :
    pdflatex is not available
  Error in texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet,  :
    pdflatex is not available
  Error in running tools::texi2pdf()
  ```

### R CMD check via win-builder
* win-builder left 2 NOTES, both regarding an example in the documentation. 
  This has to do with the nature of the package and hosting a local server. 
* The mis-spelled words in the description are not mis-spelled. 
  ```
  New submission
  
  Possibly mis-spelled words in DESCRIPTION:
    APIs (12:56)
    Webframework (3:21)
    middleware (14:63)

  Package has a VignetteBuilder field but no prebuilt vignette index.

  Found the following (possibly) invalid URLs:
    URL: http://127.0.0.1:1234/predict-species
      From: README.md
      Status: Error
      Message: libcurl error code 7:
      	Failed to connect to 127.0.0.1 port 1234: Connection refused
    URL: http://127.0.0.1:8080/usa
      From: README.md
      Status: Error
      Message: libcurl error code 7:
      	  Failed to connect to 127.0.0.1 port 8080: Connection refused
    ```
### R check via rhub::check_for_cran()
* Notes produced match the above notes produced by win-builder.

### Downstream dependencies 
* There are no downstream dependencies. 
