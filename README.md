Rust Bindings for Maya 2018
===========================

I'm currently developing these on Windows with Maya 2018. In theory you could compile on Mac or
Linux and with a different Maya version with some Maya tweaking, but I only have Maya on Windows
at my disposal so that's what we're going with :)

There are some hacks to get stuff on Windows working but they're disabled if your environment
isn't set up with the MSVC toolchain.

This uses a fork of rust-bindgen that adds support for operators to get bindings for Maya
operators, equals, etc.

Differences between Rust and C++ API
------------------------------------

-   MPx classes will be exposed as Rust traits. You won't be able to have a C++-style inheritance
    hierarchy, but you will be able to implement the virtuals just be implementing the trait
    in Rust.
-   Functions that return by argument will return normally in the Rust binding, using tuples if
    necessary. Functions that return an MStatus or have an MStatus pointer argument will return
    Result<ReturnType, MStatus> in Rust; this enforces handling of the MStatus in error cases.
-   MString's aren't exposed in the Rust API. You need to pass &str's or things convertible to
    &str. The API will take care of handling all conversions in the background.
