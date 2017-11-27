# Bond C++ Coding Guidelines

## Background

A consistent coding style benefits authors and readers. Authors have fewer,
trivial decisions to make, and readers' assumptions about overall structure
are more likely to be correct.

Here are the Bond C++ coding guidelines to help have a consistent style.

Guidelines are not hard and fast rules.

This is a work in progress.

### History

Bond has been developed by many people of many years. It is *not* internally
consistent, so you will very easily find places where these guidelines are
not followed.

### Tooling

Ideally, as many of these would be enforced via tooling to help with
uniformity and so that they don't get accidentally forgotten. Such tooling
has not yet been implemented.

### Improving to follow guidelines

Since these guidelines are not yet finished, we ask that you do not submit
patches that simply make code conform to guidelines.

## Guidelines

### When in Rome, do as the Romans do

Since Bond is not internally consistent, the local style (to a function, a
file, a module, &c.) should be followed in as much as that makes sense.

### Header file includes

The beginning of each header file is formulaic:

    // Copyright (c) Microsoft. All rights reserved.
    // Licensed under the MIT license. See LICENSE file in the project root for full license information.
    
    #pragma once
    
    #include <bond/core/config.h>
    
    // #include "current_module_header_a.h"
    // #include "current_module_header_b.h
    
    // #include <bond/some/header_a.h>
    // #include <bond/some/header_b.h>
    
    // #include <boost/some/header_a.h>
    // #include <boost/some/header_b.h>
    
    // #include <standard_library_a.h>
    // #include <standard_library_b.h>

A file starts with the copyright notice. Then there is a `#pragma once`
line. Bond does not use include guards. This is followed by `#include
<bond/core/config.h>`: all Bond headers--except config.h itself--must
include config.h before anything else.

Then there are a variable number `#include` sections.

To help make each header stand on its own, each header should include the
headers for what it uses. To help avoid implicit dependencies on indirectly
included headers, the includes are done in reverse order of generality:
first includes from the current module, then the rest of Bond, then other
libraries we use, then Boost, then the standard library and OS headers.
Additionally, within each section the headers are sorted in lexicographical
order.

If this results in errors, then the header likely needs to be fixed to
include/forward declare what it uses.

NB: headers generated from any of the IDL files that ship with Bond should
be included via full path (e.g., `<bond/core/bond_types.h>`). Depending on
how Bond is built, these headers are not guaranteed to be in the same
directory as the other Bond headers.

Any time this ordering cannot be followed, there should be a comment
indicating why.

More flexibility is permitted in "detail" headers.
