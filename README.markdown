About
=====

diemap is an extensible IMAP server written in Erlang. Its goal is to provide a
generic IMAP frontend for dealing with the IMAP protocol and allow the user to
implement their own IMAP storage backend.

diemap is currently at a very early stage of development, but contributions of
any kind are welcome. The API is not stable at all yet.

diemap includes a PEG parser for the IMAP protocol which is generated using
[neotoma](https://github.com/seancribbs/neotoma). The parser is mostly complete
but isn't completely case-insensitive yet and may have some other bugs.

Status
======

diemap is capable of basic read-only mailboxes currently, anything involving
writing data to the server is not implemented yet. Nested folders, search, etc
are all also not implemented. diemap is also not very fast yet.

Example
=======

diemap ships with a sample filesystem based backend. It is "designed" (and I use
the term loosely) to work with the enron [email dataset](http://www.cs.cmu.edu/~enron/).

To test it, just download the dataset, untar it and move the 'maildir' folder
under the toplevel directory into the diemap directory. Then you can invoke
diemap with the 'imap_server_example' backend module by doing:

  erl -pa deps/cowboy/ebin -pa deps/dh_date/ebin -pa deps/gen_smtp/ebin -pa
  deps/iconv/ebin -pa ebin -eval "diemap_server:start(imap_server_example)."

Then you will be able to login as any user under the maildir folder (eg
skilling-j, lay-k, etc) and read their email. The password is not checked for
any valid username.

Live Instance
=============

There's a live instance running at mail.cataclysm-software.net on port 143. Feel
free to play with it but try not to kill my server.


