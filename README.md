carrit
======

A chunk management library for [Minecraft](http://www.minecraft.net/).

Functionality is currently limited, as this library is under development.

Dependencies
------------
carrit requires log4j to be installed.

Development
-----------
Feature pull requests are not currently being accepted, as the design and
direction has not yet been decided. Bug fixes and code refactoring may be
considered - also happy for people to tell me a section of code is bad, as
long as it's accompanied by tests and/or explanations.

Use
---
An example Java client is [CarritJava](https://github.com/taufiqkh/CarritJava),
which demonstrates loading a save directory and extracting a chunk. 

Planned Features
----------------
Still to do are proper exposure of the NBT data, as well as saving chunks and fleshing out the concurrency model.

#1: Implement chunk