### Test Enviroments
* Local Ubuntu-Linux 19.10, R 3.6.2
* Fedora Linux, R-devel, clang, gfortran
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

### R CMD check via check_win_release/devel/oldrelease
* 1 NOTE: 
  * checking CRAN incoming feasibility ... NOTE
  New submission
  
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
* REASON: 
  * This has to do with the README examples. These links are local host 
  being served with beakr. This is not an error. 
  
### R CMD check via r-hub check_for_cran
* 1 NOTE: 
  * checking CRAN incoming feasibility ... NOTE
  New submission

### Downstream dependencies 
* There are no downstream dependencies. 
