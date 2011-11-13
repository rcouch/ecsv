ECSV
====

ECSV is an Erlang CSV parser.

Composed of 2 modules:

 - **ecsv_reader**: read a file or a string and send a flow of characters to ecsv_parser
 - **ecsv_parser**: based on the characters sent by ecsv_reader, parse each line and send them to a process (ResultPid).

A ResultPid process will receive 2 messages:

- `{newline, NewLine}` for each parsed line
- `{done}` when the parsing is done (usually because eof has been sent)

---

This parser is based on the blog post written by *Andy Till* located
here [http://andrewtill.blogspot.com/2009/12/erlang-csv-parser.html](http://andrewtill.blogspot.com/2009/12/erlang-csv-parser.html).

This parser supports well formed csv files which are

- a set of lines ending with a `\n`
- each line contains a set of fields separated with a comma (`,`)
- each field value can be enclosed with single (`'`) or double quote (`"`)
- each field value can be empty
- any `\r` is ignored

Example:

    2,"Supply and use",9,"Barley",1,0.800,"United States",1,"Barley",34,"Imports, trade year",7,"1,000 metric tons",1960,3,"Annual",114,"MY Oct-Sep",248
    2,"Supply and use",9,"Barley",1,0.800,"United States",1,"Barley",34,"Imports, trade year",7,"1,000 metric tons",1961,3,"Annual",114,"MY Oct-Sep",326

Please note:

- This parser has no failsafe mechanism if the file is badly formed!
  But the line `a,,,,,\n` is perfectly fine.
- This parser doesn't allow a return (`\n`) in a field value!

