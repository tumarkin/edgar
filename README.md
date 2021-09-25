# edgar

A command line utility to locally index and download filings from the SEC Edgar
database.

## Features

Edgar builds a local Postgres database of forms on Edgar and then allows you
to download forms using queries based on any combination of CIK, company name,
form type (e.g. 10-K), start date, and end date. It is also possible to download
forms by the internal identifier in the database. `edgar --help` provides
information on all commands/sub-commands. Downloads are run in parallel enabling
Edgar to download data efficiently.

## Installation

1. Edgar application

    a. Download and install [Haskell Stack](https://www.stackage.org).

    b. Clone this repository

    c. CD to the local repository directory and type `stack install`.

    d. On occassion, `stack` may temporarily fail on the install due to the way
    it handles parallel installation of dependencies. Just run `stack install` 
    again and it will pick up where it left off.

2. Install postgres and create a database to hold your form index, e.g., `create database edgar;`.

3. On the command line type `edgar init`. You will need to specify a path if you
did not use the default database name (i.e. *edgar*). See getting help below.

## Creating and updating your form index

The form index is housed in Postgres. *edgar* will keep your index up to date.
Each quarter, simply type `edgar update START END`. *START* and *END* are
year-quarters, each specified as YYYYqQ. For example, 1999 quarter 2 is as
*1999q2*.


## Downloading forms

There are two modes to download forms. You may either specify conditions that
forms needs to satisfy or list IDs from the database.

1. **Conditional**: Type `edgar download query --help` to see a list of
conditions. You may download using any combination of CIK, company name, form
type, start date, and end date. For multiple possible CIKs, company names, or
form types, simply specify multiple option arguments.

2. **ID**: Type `edgar download id ID1 ID2 ...` where *IDX* is the internal id
identifier from the forms table in the edgar postgres database.

*edgar* supports simultaneous downloads (default is 4). See --help.


## Getting help

`edgar --help` will provide help on usage. Each sub-command provides
individualized help. For example, to get help on downloading, type `edgar
download --help`.

# Revision history

* _0.1.1.0_
    - Edgar's `Update` subcommand updated due to changes to the SEC
    website. The program was receiving "throttling" errors from the
    SEC because it was not providing an email address as part of the
    user agent in HTML requests. The subcommand now requires an
    email address.
    - Minor bug fix to strip quotation marks from company names,
    which were not compatible with Postgres without escaping.

