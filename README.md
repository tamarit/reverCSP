# reverCSP

reverCSP is a tool that allows us to evaluate a CSP specification in both directions: forwards (traditional computation) and backwards (using our reversible computation framework). reverCSP uses internally [csp_tracker](https://github.com/mistupv/csp_tracker) to produce tracks of the computation so one can better follows what is being computed so far (or henceforth if going backwards). 

Table of contents
=================

  * [reverCSP](#revercsp)
  * [Table of contents](#table-of-contents)
  * [Installation](#installation)
  * [Docker](#docker)
  * [Usage](#usage)

Installation
============

GNU/Linux
---------

reverCSP has the following dependencies: Erlang, `make` and Graphviz (optional). Without Graphviz, reverCSP will only output DOT files, instead of generating the corresponding PDFs. In Debian or Ubuntu-based distributions, the dependencies would be installed with the following command:

    # apt-get install build-essential erlang graphviz

To compile the program, clone this repository recursively and then run make:

    $ git clone --recursive https://github.com/tamarit/reverCSP.git
    $ cd reverCSP
    $ make compile
    
Windows and macOS
-----------------

reverCSP is not yet capable of running natively on this platform, but a [Docker](#docker) container is available.

Docker
======

We also provide a Dockerfile in case all previous steps are not available in your current environment. To build the docker image run the following command: 

    $ git clone --recursive https://github.com/tamarit/reverCSP.git
    $ cd reverCSP
    $ docker build -t revercsp .
    
Once the image is created, you can run a container using the following command:

    $ docker run --name csp -it -v $PWD/examples:/reverCSP/examples -v $PWD/output:/reverCSP/output --rm revercsp

Two folders are exposed to the docker container (see the `-v` option): `./examples`, which contains CSP specifications and `./output`, where PDFs representing the current track will be generated. When the container launches, the user is presented with a `bash` prompt, from which they can run reverCSP, as described in [Usage](#usage).

Usage
=====

The reverCSP tool needs a CSP specification as input. We can run the tool with the chosen CSP specification (e.g. examples/ex1.csp) using the following command:

    $ ./reverCSP examples/ex1.csp

Once launched, the user will be presented with a numbered menu, along with the current state of the CSP computation.

[![asciicast](https://asciinema.org/a/19TuoAdTapt2E7azcNAyzRgZg.svg)](https://asciinema.org/a/19TuoAdTapt2E7azcNAyzRgZg)
