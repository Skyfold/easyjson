# Haskell JSON parser

- This was created to give better errors when parsing JSON
- It reads JSON into Aeson's internal Value type.
  - Not a drop in replacement for decode, (unless you ask me to change it)
- It comes with a simple executable that you can run on individual files

# Install

- clone this repo
- `stack test`

This will build and run the test suite.

# Executable Use

- `stack exec ckjson "path to file"`

# Differences from Aeson

There are two cases I found where Aeson's decode differs from the JSON spec.

- "text \\z text" is legal, since only the \ is escaped, but `decode` fails on this
  - I do accept this as valid
- `decode` also does not handle "\u0000"
  - I do not accept this as valid (can add easily)

# Comments Please

This is my first Haskell library that I did not write for myself. Please let me know what I am missing to turn this into a useful library. 

# Acknowledgments 

The library Text.Trifecta does most of the heavy lifting.
