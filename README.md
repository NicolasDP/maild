# Haskell Mail Daemon

Implement mail services.

Usage:

    ./maild <configuration path name>

The configuration file use the haskell package *configurator*. See the
documentation on http://hackage.haskell.org/package/configurator for more
information (as the syntax).

## SMTP

Provide a full Haskell implementation of SMTP service daemon.

### How to setup

Add in the configuration file the following options in the option group
"smtp":
* port: the port number to listen on
* connections: the number of accepted connections (will reject all connections
when this limit is reached).
* domain: the MX domain which point to the machine which hosts the daemon

Example:

    smtp {
        port = 25
        connections = 128
        domain = "smtp.my.private.domain.net"
    }

## MailStorage

*maild* provides a simple way to manages mails, domains, users and mailboxes.

The MailStorage directory is created initialized by *maild* is it does not
exist already.
This directory is build as follow:
* path/to/maildir
    * incoming: directory used by the SMTP thread to store incoming emails
within the connection. *You don't need to look at it*.
    * for-delivery: the directory used by the SMTP thread to store the
received and valide emails (they are moved from the _incoming_ directory
from _for-delivery_ when the connected client has finised to send the
emails DATA). Then, the *DeliveryManager* (not implemented yet) will be
notified that a new email is waiting for delivery and will dispatch it
to the local mailboxes (local Recipients) or will send it to the
recipient. *You don't need to look at it*.
    * users: the directory which contains the SMTP users information.
Documentation below.
    * domains: the directory which contains the domains. Documentation below.

### Domains

The SMTP server will manage the domains which are listed in the _domains_
directory. To add a new domain, you only need to create a new directory in the 
_domains_ directory.

For example, if you want the SMTP server to manage foo.io and mail.foo.io:
* _path/to/maildir_/domains
    * foo.io: the directory which contains mailboxes (\*@foo.io)
    * mail.foo.io: another directory which contains mailboxes (\*@mail.foo.io)

To create a new mailbox, you only need to create a sub directory in the
required domain.

For example:
* _path/to/maildir_/domains/foo.io
    * nicolas: directory for the mailbox (nicolas@foo.io)
    * bryan: directory for the mailbox (bryan@foo.io)
* _path/to/maildir_/domains/mail.foo.io
    * nicolas: (nicolas@mail.foo.io)
    * git: (git@mail.foo.io)
    * irc: (irc@mail.foo.io)

### Users

The SMTP server will accept every valide emails from every valide SMTP client
but won't accept to forward an email (i.e. if the recipient is not local, the
recipient is rejected).
*maild* will accept to forward email only from authentified users.

To create a new user, you only need to create a file in the *users* directory.
The file name is the unique user identifier.

For example:
* _path/to/maildir_/users
    * oneill: a user's configuration file associated to user "oneill"
    * carter: a user's configuration file for "carter"
    * etc.

A *User configuration file* contains the following values:
* firstname: a quoted string (unique)
* lastname: a quoted string  (unique)
* password: quoted string (can be a clear password or a digest, depends on
the required security) (unique)
* address: a list of email address (quoted string)

User configuration file uses *configurator* (the same tools used for maild's
configuration file). So you need to follow the same syntax as described in the
*configurator* documentation.

For example, the user "oneill" may have a file:

    firstname = "Jack"
    lastname  = "o'neill"
    password  = "Skaara1994"
    address   = [ "jack@sg1.io"
                , "jack@oneill.io"
                ]

The user "oneill" has 2 mailbox "jack": One in the domain "sg1.io", one in "oneill.io"

You don't need to reload maild to manage new users/mailboxes.

## Incoming features

* SMTP on port 465 (using TLS/SSL)
* E-SMTP:
    * AUTH: LOGIN and CRAM-MD5
    * STARTTLS: start a TLS connection even from the port 25
* DeliveryManager (to apply filter and forward/delivery to users' mailboxes)
* IMAP daemon

## Comments

SMTP implements some RFC (notably RFC5321). If you find some errors, report the
issue on github: http://github.com/NicolasDP/maild.
