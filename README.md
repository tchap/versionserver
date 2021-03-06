versionserver
=============
[![Build Status](https://secure.travis-ci.org/tchap/versionserver.png?branch=master)](http://travis-ci.org/tchap/versionserver)

A tiny OTP application that generates subsequent build numbers for projects
according to the major, minor and release number specified.

To put it simply, it keeps a mapping of
`{Project, Major, Minor, Release} -> LastBuild` and increments it
with every new request.

The application spawns a new process for every project.
The process then keeps its `Version -> LastBuild` mapping in a DETS table.
