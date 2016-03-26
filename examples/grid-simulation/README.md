Gschem grid simulation script
=============================

Installation
------------
  Copy the script *add-grid.scm* & *gafrc* into your projects' directory.

Setup
-----
  You can adjust color values in gafrc, color numbers & step sizes
  in the script itself.

Usage
-----
    gaf shell -s add-grid.scm input.sch output.sch && \
    gaf export -o output.pdf -c output.sch

Requirements
------------
  * geda-gaf 1.9.1
  * guile 2.0
