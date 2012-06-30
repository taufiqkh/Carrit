# carrit

A chunk management library for [Minecraft](http://www.minecraft.net/).

Functionality is currently limited, as this library is under development. Due to the early state of development, the API has not solidified and incompatible changes are quite likely. The NBT functions in the carrit.named-binary-tag are usable but I think that the current data structure is too unwieldy and may change it.

## Dependencies

carrit requires log4j to be installed.

## Development

Feature pull requests are not currently being accepted, as the design and direction has not yet been decided. Bug fixes and code refactoring may be considered - also happy for people to tell me a section of code is bad, as long as it's accompanied by tests and/or explanations.

## Use
---
An example Java client is [CarritJava](https://github.com/taufiqkh/CarritJava), which demonstrates loading a save directory and extracting a chunk. 

### NBT
The carrit.named-binary-tag namespace contains NBT functions that are usable. Given a byte array, they will generate NamedBinaryTag records that contain information in the following structure:
    NamedBinaryTag {
      :type
      :data (nil|<data dependent on type>)
      [:name <string>]
      [:child-type <type> (lists only)]
    }

As seen above, :name is optional, as its presence is dependent on the context. The `:child-type` value is only not nil for `type-list`. For all list types, the `:data` value contains a sequence of NamedBinaryTag records, while for compound types the `:data` value contains a map of names to NamedBinaryTag records.

Types are listed in `carrit.named-binary-tag/type-names`.

## Planned Features

Still to do are:

* Creation of NBT data
* Completing 
* Saving chunks
* Fleshing out the concurrency model

Possible improvements:
* Less unwieldy NBT data