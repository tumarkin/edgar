# edgar
A simple command line utility to locally index and download filings from the SEC Edgar database.

# Installation

1. Edgar application
  a. Download and install Haskell stack from [https://www.stackage.org]
  b. Clone this repository
  c. CD to the local repository directory and type `stack install`

2. Install postgres and create a database (Edgar expects a database named 'edgar')

3. On the command line type `edgar init`

# Creating and updating your document index

The document index is housed in the Postgres databse. *edgar* will help you
keep your index up to date. Each quarter, simply type `edgar update YEAR QTR`
where *YEAR* is a 4-digit year and *QTR* is a 1-digit quarter. A batch
utility (written in the fish shell) is available in the utility directory
to initialize your database through the end of 2016.

# Downloading forms


# Getting help

`edgar --help` will provide help on usage. Each subcommand provides individualized help. For example,
to get help on downloading, type `edgar download --help`. 


