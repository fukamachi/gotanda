# Gotanda List

## Qualification

This software is developed with SBCL 1.0.32.

Depends on CLSQL.

## Installation

    $ sbcl
    * (require 'asdf)
    * (require 'asdf-install)
    * (asdf-install:install "http://github.com/fukamachi/gotanda/tarball/master")

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
