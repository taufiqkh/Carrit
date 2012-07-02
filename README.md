# carrit

A chunk management library for [Minecraft](http://www.minecraft.net/).

Functionality is currently limited, as this library is under development. Due to the early state of development, the API has not solidified and incompatible changes are quite likely. The NBT functions in the carrit.named-binary-tag are usable but I think that the current data structure is too unwieldy and may change it.

## Dependencies

carrit requires log4j to be installed.

## Development

Feature pull requests are not currently being accepted, as the design and direction has not yet been decided. Bug fixes and code refactoring may be considered - also happy for people to tell me a section of code is bad, as long as it's accompanied by tests and/or explanations.

## Use

An example Java client is [CarritJava](https://github.com/taufiqkh/CarritJava), which demonstrates loading a save directory and extracting a chunk. 

### NBT
The carrit.named-binary-tag namespace contains NBT functions that are usable. Given a byte array, they will generate `NamedBinaryTag` records that contain information in the following structure:

    NamedBinaryTag {
      :type type
      :data (nil|data dependent on type)
      [:name String]
      [:child-type type (lists only)]
    }

As seen above, `:name` is optional, as its presence is dependent on the context. The `:child-type` value is only not nil for `type-list`. For all list types, the `:data` value contains a sequence of `NamedBinaryTag` records, while for compound types the `:data` value contains a map of names to `NamedBinaryTag` records. It's somewhat clumsy to work with but I'd like to simplify it in the future once I'm more familiar with the language.

Types are listed in `carrit.named-binary-tag/type-names`.

## Planned Features

Still to do are:

* Creation of NBT data
* Completing the chunk interface
* Saving chunks
* Properly figuring out the concurrency model
* Better data checks
* Error handling. Haven't figured out the idiomatic Clojure way of doing this yet.

Possible improvements:

* Less unwieldy NBT data
* Removal of extracts; they are only there as an interim measure until I can restructure in such a way that I don't need to pass back both length and data.
* Rework of the file read->NBT creation pipeline, reducing array copying where possible. This will depend on what priority is placed on improving performance in this area.