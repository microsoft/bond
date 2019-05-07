About
=====

Bond is an open source, cross-platform, cross-language framework for working 
with schematized data.

Bond is published on GitHub at https://github.com/microsoft/bond/

The C# documentation is available at http://microsoft.github.io/bond/manual/bond_cs.html

The C# examples are at https://github.com/microsoft/bond/tree/master/examples/cs/core

Bond codegen
============

The .bond schema files can be automatically built as part of a project by using 
the BondCodegen build action (note that after installing the Bond package you may 
need to reload the project before Visual Studio sees the new build action). The 
generated .cs files will be implicitly compiled as part of the project.

You can customize the code generation by defining the following 
properties/items in your project:

    @BondCodegen         : A Bond schema file (usually with a .bond extension)
      %Options           : Additional options to pass to the gbc compiler for this file
    @BondImportDirectory : Directories to search for imported schemas
    $BondOutputDirectory : Directory for generated files, by default IntermediateOutputPath
    $BondOptions         : Additional options to pass to the gbc compiler for this project
    $BondCodegenMode     : Code generation mode for gbc to use (default c#)

By default gbc is executed with `--jobs=-2` option to compile multiple .bond
files concurrently, using 2 fewer jobs than there are processor cores. You can
override this behaviour by setting a different value for `--jobs` option via
the `BondOptions` property.

For details see http://microsoft.github.io/bond/manual/bond_cs.html#code-generation

ReSharper
---------

ReSharper may not see the generated files as part of the project. As a workaround 
you can add the files to the project explicitly and exclude them from the build.
