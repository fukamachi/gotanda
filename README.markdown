# Gotanda List

## Qualification

This software is developed with SBCL 1.0.32.

Depends on CLSQL.

## Installation

    $ sbcl
    * (require 'asdf)
    * (require 'asdf-install)
    * (asdf-install:install "http://github.com/fukamachi/gotanda/tarball/master")

asdf-install should resolve dependencies, but not works better. So, you may have to ensure CLSQL is installed before. That's very problematic. Good luck.

Have another problem? Report me.

## Run

    * (asdf:oos 'asdf:load-op :gotanda)
    * (got:create-user :name "fukamachi" :pass "password")
    * (got:create-task :body "Buy Milk")

You can use from your terminal. "got" is a command line program.

    $ got create user --name "fukamachi" --pass "password"
    New User: fukamachi / password
    $ got create task --body "Buy Milk"
    New Task: Buy Milk

## License

Copyright (C) 2010 Eitarow Fukamachi <e.arrows@gmail.com>
