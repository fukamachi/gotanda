# Gotanda List

## Qualification

This software is developed with SBCL 1.0.32 on MacOS 10.6.

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

## Quick Start

    * (asdf:oos 'asdf:load-op :gotanda)
    * (got:create-task :body "Buy Milk")

## Command Line Interface

### Build

    $ sbcl --load src/cli/build.lisp
    ;; blah blah blah
    ;; ..created an executable script, "got"

### Run

    $ got
    >

">" is a prompt. Then you can do below actions.

* all
* tag #hashtag
* #hashtag (same as above)
* create "body" "yyyy-MM-dd(deadline)"
* edit
* delete
* complete (not implemented yet)

### Examples

First, start "got" and see all tasks.

    $ got
    > all
    1: Buy Milk #shopping
    2: Read "SoftwareDesign"
    3: Fix Bugs

The numbers on the left side of each lines are index for next action. You can choose a task to type the number.

If you were to edit 2nd task,

    > 2
    What Action?>

...the prompt of next line was changed.

    What Action?> edit
    Body?> Read "SoftwareDesign" carefully
    Deadline> [Return]
    >

So, the 2nd task is changed.

    > all
    1: Buy Milk #shopping
    2: Read "SoftwareDesign" carefully
    3: Fix Bugs

You can filter the tasks with #hashtag.

    > #shopping
    1: Buy Milk #shopping
    >

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
