# edgar
A simple command line utility to locally index and download filings from the SEC Edgar database.

# Installation

1. Edgar application

    a. Download and install [Haskell Stack](https://www.stackage.org).

    b. Clone this repository

    c. CD to the local repository directory and type `stack install`

2. Install postgres and create a database (Edgar expects a database named 'edgar')

3. On the command line type `edgar init`. You will need to specify a path if
   you did not use the default database name. See getting help below.

# Creating and updating your form index

The form index is housed in the Postgres Dada's. *edgar* will help you
keep your index up to date. Each quarter, simply type `edgar update YEAR QTR`
where *YEAR* is a 4-digit year and *QTR* is a 1-digit quarter. A batch
utility (written in the fish shell) is available in the utility directory
to initialize your database through the end of 2016.

# Downloading forms

There are two modes to download forms by specifying conditions that forms needs
to satisfy or by listing IDs from the database.

1. **Conditional**: Type `edgar download query --help` to see a list of
   conditions. You may download by any combination of CIK, company name, form
   type, start date, and end date.

2. **ID**: Type `edgar download id ID1 ID2 ...` where *IDX* is the internal id 
identifier from the forms table in the edgar postgres database.

*edgar* does not currently innately support simultaneous downloads. However, it
is possible to get this functionality using the command line and the fact taht
conditional downloads are delivered in a random order. For example, to get two
downloads simultaneously, open two command shells. In each, create an infinite
loop that executes a single `edgar download query` command using a small batch
size.


# Getting help

`edgar --help` will provide help on usage. Each sub-command provides individualized help. For example,
to get help on downloading, type `edgar download --help`. 


