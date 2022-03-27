# edgar

A command line utility to locally index and download filings from the SEC Edgar
database.

## Features

_edgar_ builds a local Postgres database of forms on SEC Edgar and then allows
you to download forms using queries based on any combination of CIK, company
name, form type (e.g. 10-K), start date, and end date. It is also possible
to download forms by the internal identifier in the database. `edgar --help`
provides information on all commands/sub-commands. Downloads are run in parallel
enabling _edgar_ to download data efficiently.

## Installation

1. _edgar_ application

    a. Download and install [Haskell Stack](https://www.stackage.org).

    b. Clone this repository

    c. CD to the local repository directory and type `stack install`.

    d. On occasion, `stack` may temporarily fail on the install due to the way
    it handles parallel installation of dependencies. Just run `stack install` 
    again and it will pick up where it left off.

2. Install Postgres and create a database to hold your form index, e.g., `create database edgar`.

3. On the command line type `edgar init`. You will need to specify a path if you
did not use the default database name (i.e. *edgar*). See getting help below.

## Creating and updating your form index

The form index is housed in Postgres. _edgar_ will keep your index up to date.
Each quarter, simply type `edgar update START END`. *START* and *END* are
year-quarters, each specified as YYYYqQ. For example, 1999 quarter 2 is as
*1999q2*. _edgar_ maintains unique indexes and will not duplicate forms should
you run the update command multiple times on the same year-quarter.


## Downloading forms

There are two modes to download forms. You may either specify conditions that
forms needs to satisfy or list IDs from the database.

1. **Conditional**: Type `edgar download query --help` to see a list of
conditions. You may download using any combination of CIK, company name, form
type, start date, and end date. For multiple possible CIKs, company names, or
form types, simply specify multiple option arguments.

2. **ID**: Type `edgar download id ID1 ID2 ...` where *IDX* is the internal id
identifier from the forms table in the edgar Postgres database.

_edgar_ supports simultaneous downloads (default is 4). See --help.


## Getting help

`edgar --help` will provide help on usage. Each sub-command provides
individualized help. For example, to get help on downloading, type `edgar
download --help`.

## Development status

_edgar_ is under active development. However, I may not notice issues
arising from changes to the SEC website immediately, as I access data
intermittently. Should an issue arise, please post an Issue and I will
endeavor to fix it.

# Revision history

* _0.1.1.2_
    - `download` subcommand now recognizes already downloaded forms
    that are stored compressed. Use the `--zip-extension` option to specify
    the appropriate compressed extension, e.g., `--zip-extension=zst`.
    - `download` subcommand may be limited to a specific number
    of forms using `--limit` option. This may be useful when downloaded
    a large number of forms by switching to a loop over a smaller number.
 
* _0.1.1.1_
    - Edgar's `download` subcommand updated due to changes to the SEC
    website. Previously only the index files were throttled, affecting
    the `update` subcommand. Now, forms are throttled, necessitating
    that the user provide an email address as part of the
    user agent in HTML requests. This subcommand now requires an
    email "address".

* _0.1.1.0_
    - Edgar's `Update` subcommand updated due to changes to the SEC
    website. The program was receiving "throttling" errors from the
    SEC because it was not providing an email address as part of the
    user agent in HTML requests. The subcommand now requires an
    email "address".
    - Minor bug fix to strip quotation marks from company names,
    which were not compatible with Postgres without escaping.

