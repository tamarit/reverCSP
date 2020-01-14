# reverCSP

reverCSP is a tool that allows to compute CSP specification in both ways, i.e. forward (traditional computation) and backwards (using our reversible computation framework). reverCSP uses internally [csp_tracker](https://github.com/mistupv/csp_tracker) to produce tracks of the computation so one can better follows whhat is being computed so far (or henceforth if going backwards). 

Table of contents
=================

  * [reverCSP](#revercsp)
  * [Table of contents](#table-of-contents)
  * [Installation](#installation)
  * [Docker](#docker)
  * [Usage](#usage)

Installation
============

reverCSP is built using Erlang and its installation is mandatory in order to run the tool. You can install it using:

* Ubuntu

        $ apt-get install -y build-essential erlang
* Mac OSX (using [homebrew](https://brew.sh/index_ca))

        $ brew install erlang

reverCSP optionally requires [graphviz](https://www.graphviz.org/) to produce PDF outputs of the tracks. Otherwise only DOT files will be produced. To install it simply run the following command:

* Ubuntu
        
        $ apt-get install -y graphviz 

* Mac OSX

        $ brew cask install graphviz
        
Finally, when all previous steps are done, thhe following command should be run:

* Ubuntu

        $ cd reverCSP \
            && make compile \
            && cd csp_tracker \
            && mv bin_linux/* . \
            && make \
            && cd .. \
            && make compile

* Mac OSX

        $ cd reverCSP \
            && make compile \
            && cd csp_tracker \
            && mv bin_macos/* . \
            && make \
            && cd .. \
            && make compile

Docker
======

We also provide a Dockerfile in case all previous steps are not available in your current environment. To build the docker image run the follwing command: 

    $ docker build -t revercsp .
    
Once thhe image is created, you can up a container using the following command:

    $ docker run --name csp -it -v $PWD/examples:/reverCSP/examples -v $PWD/output:/reverCSP/output --rm revercsp

Once inside the container the usage is as it is described in [Usage](#usage). The on-host examples folder can be used to modify or add new examples, while the on-host output folder will contain the tracks produced inside the container.

Usage
=====

The reverCSP tool needs a CSP specification as input. We can run the tool with the chosen CSP specficiation (e.g. examples/ex1.csp) using the following command:

    $ ./reverCSP examples/ex1.csp
