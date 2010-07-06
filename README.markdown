# Gotanda List

## Qualification

This software is developed with SBCL 1.0.32.

Depends on CLSQL.
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
    * (got:find-task :id 1)

You can use from your terminal. "got" is a command line program.

    $ got create task --body "Buy Milk"
    New Task: Buy Milk
    $ got find task --id 1
    Found: (1 Buy Milk)

## Roadmap

* Command line program
* Synchronize with a Web server
* Web interface
* Port to ECL
* iPhone & Android client

## For Developper
### Unit Tests
Gotanda List uses [lisp-unit](http://www.cs.northwestern.edu/academics/courses/325/readings/lisp-unit.html) for testing.

    * (require 'asdf-install)
    * (asdf-install:install "http://repo.or.cz/w/lisp-unit.git/snapshot/HEAD.tar.gz")

## License

Copyright (C) 2010 Eitarow Fukamachi <e.arrows@gmail.com>
