# beakr 0.3.1

* Removed `httpuv_~()` function wrappers in favor of importing the functions 
directly from *httpuv*.
* `serveStaticFiles()` now works.

# beakr 0.3.0

Lots of refactoring with an eye toward:
 * core functionality only for an initial release
 * consistent, predictable naming
 * more documentation and examples

* Renamed `App` object back to `Beakr` for internal consistency.
* Renamed `beakr()` to `newBeakr()` to avoid confusion.
* Renamed `http_get()`, _et al_ to  to `httpGET()` _et al_.
* Renamed `kill()` to `startServer().
* Added `httpuv_kstopAllServers()`.
* Removed `.addListener()` and added functionality inside of `on()` function.
* Removed the following functions as superflous for an initial release:
`cors()`, `on()`, `include()`, `kill_all()`, `list_active()`, `logger()`, 
`use()`, `websocket()`
* Improved documentation and examples throughout.

# beakr 0.2.3

* Prepare for CRAN x3
* renamed http-methods to `http_get()`, `http_post()`, `http_put()`, `http_delete()`.
* renamed error-methods to `new_error()`, `error_handler()`
* renamed other utils and internal methods
* Fixed `static()` function to serve static files. 

# beakr 0.2.2

* Prepared for CRAN x2
* Updated README to avoid CRAN notes
* Updated http-methods functions to avoid CRAN suggestions
* Updated `createBeakr()` -> `beakr()` 
* Renamed `Beakr` class object to `App` to align with other popular webframeworks (Express.js, Flask)
* Renamed `handleErrors()` to `handler()`
* Renamed `onEvent()` to `on()`

# beakr 0.2.1 

* Prepare for CRAN
* Update the docs and export objects
* Fix deprecated local_examples/

# beakr 0.1.6

* Minor documentation tweaks.

# beakr 0.1.5 

* Added `onEvent()` event listening
* Fixed beakr instance information view
* Added `beakr` instance names
* Added support for more internal functionality 
* Added logging capability `logger()`.

# beakr 0.1.4

* Renamed http methods to upper case: `get()` ==> `GET()`, _etc._.
* Renamed `killall()` to `killAll()`.
* Restored documentation `@examples`.

# beakr 0.1.3

* Renamed `listen()` to `startBeakr()`.
* Internal refactoring and cleanup.

# beakr 0.1.2

* Changed `request`, `response`, and `error` object names to `req`, `res`, 
`err`, as more consistent with popular frameworks
* Added fancy beakr prints

# beakr 0.1.1

* CORS functionality
* Daemonized instances
* Proper JSON erorr handling
* Added other features, `kill()/killall()`, `active()`
* Massive bug fixes

# beakr 0.1

* Initial Release
