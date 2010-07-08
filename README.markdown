# Gotanda List

## Qualification

This software is developed with SBCL 1.0.32.

Depends on CLSQL and cl-ppcre.

## Installation

asdf-install should resolve dependencies, but not works better. So, you may have to ensure CLSQL is installed before. That's very problematic. Good luck.

    $ cd ~/.sbcl/site
    $ git clone git://github.com/fukamachi/gotanda.git
    $ ln -s gotanda/gotanda.asd ../systems
    $ cd /path/to/clsql-x.x.x/db-sqlite3
    $ patch -p0 < ~/.sbcl/site/gotanda/clsql-sqlite3-auto-increment.patch
    $ sbcl
    * (require 'asdf)
    * (asdf:oos 'asdf:load-op :gotanda)

Have another problem? Report me.

## Run

    * (asdf:oos 'asdf:load-op :gotanda)
    * (got:create-task :body "Buy Milk")

You can use from your terminal. "got" is a command line program.

    $ got create task --body "Buy Milk"
    New Task: Buy Milk

## Command Line Interface

* New Task

    Usage: got create task \[--body "BODY"\]\[--deadline "yyyy-MM-dd hh:mm:ss"\]
    
      --body: write some texts for a description of the task.
    
      --deadline: write it's deadline

* See Tasks

    Usage: got list task \[--tag "#hashtag"\]\[--deadline "pred yyyy-MM-dd"\]
    
      --tag: specify hashtag (begin with '#').
    
      --deadline: filter with a deadline, before or after. Below example shows a list of tasks which the deadline is after "2003-04-07".
          got list task --deadline "< 2003-04-07"

## Roadmap

* Command line program
* Synchronize with a Web server
* Web interface
* Port to ECL
* iPhone & Android client
* Bookmarklet
* iGoogle Gadget

## For Developper
### Unit Tests
Gotanda List uses [lisp-unit](http://www.cs.northwestern.edu/academics/courses/325/readings/lisp-unit.html) for testing.

    * (require 'asdf-install)
    * (asdf-install:install "http://repo.or.cz/w/lisp-unit.git/snapshot/HEAD.tar.gz")

## License

Copyright (C) 2010 Eitarow Fukamachi <e.arrows@gmail.com>
