# Bond Common Project System Integration Notes

Visual Studio 2017 and later use the [Common Project System][cps] for some
.NET-based projects, in particular .NET Core and .NET Standard project using
the simplified `.csproj` format. The way this integrated with the Visual
Studio UI differs from pervious versions.

# Implicit codegen

The expectation for the simplified `.csproj` format is that the files in the
project directory [are automatically compiled][implicit-items]. This
behavior is technically separate from the Common Project System, but most
developers encounter both at the same time.

The Bond NuGet packages follow this expectation, running codegen on all
`.bond` files in the project directory when the MSBuild property
`EnableDefaultCompileItems` is true. (This property is true by default for
the simplified `.csproj` format.)

A consequence of implicit codegen is that per-item `BondCodegen.Options`
**metadata** needs to be set using [`Update` instead of
`Include`][msbuild-item-update], as the `.bond` file has already be included
once.

```xml
<ItemGroup>
  <BondCodegen Update="foo.bond">
    <Options>--namespace=foo=common</Options>
  </BondCodegen>
  <BondCodegen Update="bar.bar">
    <Options>--namespace=bar=common</Options>
  </BodCodegen>
</ItemGroup>
```

# Visual Studio UI integration

The following sections are notes for the maintainers of the NuGet packages.
Consumers of the packages don't need to worry about these details.

Getting the Visual Studio 2017+ UI to display `BondCodegen` items is a
three-step process:

1. Define the [custom items types][custom-item-types].
1. [Write rules for the custom items][build-action] defining what
   properties to show in the UI.
    * This example [none.xaml][none-xaml] was used as the starting point for the Bond rules.
1. [Tell Visual Studio][add-xaml-rules] about the custom items and rules.

# References

* [Common Project System][cps]
* CPS [custom item types][custom-item-types]
* [Adding rules via MSBuild][add-xaml-rules]
* [Rules for custom build actions][build-action]
* GitHub issue about [adding a custom item with automatic globbing][add-custom-item-query]

[add-custom-item-query]: https://github.com/dotnet/project-system/issues/2875
[add-xaml-rules]: https://github.com/microsoft/VSProjectSystem/blob/master/doc/extensibility/adding_xaml_rules.md
[build-action]: https://github.com/microsoft/VSProjectSystem/issues/244#issuecomment-327268174
[cps]: https://github.com/microsoft/VSProjectSystem
[custom-item-types]: https://github.com/microsoft/VSProjectSystem/blob/master/doc/extensibility/custom_item_types.md
[implicit-items]: https://aka.ms/sdkimplicititems
[msbuild-item-update]: https://docs.microsoft.com/en-us/visualstudio/msbuild/item-element-msbuild#attributes-and-elements
[none-xaml]: https://github.com/microsoft/VSProjectSystem/blob/1c0a47aba5a22d3eb071dc097b73851bdeaf68db/samples/WindowsScript/WindowsScript/WindowsScript.ProjectType/BuildSystem/Rules/none.xaml
