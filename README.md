# miniUI

Provides UI widget and layout functions for writing Shiny apps that work well on small screens.

Inspired by the lovely [Ratchet](http://goratchet.com/) CSS framework, though currently miniUI doesn't use any of Ratchet's CSS code.
    
## Installing

```r
devtools::install_github("rstudio/miniUI")
```

## Example

TODO

---

## Compiling Less source to CSS

**You generally don't need to do this.** This build step is only needed if you want to contribute to the development of the package by modifying the built-in CSS styles.

### One-time setup

**Prerequisite:** You must have Node.js installed.

```sh
# Install the grunt command if necessary
which grunt || sudo npm install -g grunt-cli

# Install dependencies
cd tools
npm install
```

### Building

```sh
cd tools
grunt
```
