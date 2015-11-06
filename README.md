# Shiny Gadgets

A package for R that helps you create interactive tools based on the Shiny web framework, that assist in data analysis tasks.

Shiny _apps_ are usually used for communicating results to other people.

Shiny _gadgets_ are used from the R console during data analysis, to assist in tedious or iterative tasks.

## Installing

```r
devtools::install_github("rstudio/shinygadgets")
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

### Building automatically when .less files change

```sh
cd tools
grunt watch
```
