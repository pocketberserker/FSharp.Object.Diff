namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyTitleAttribute("FSharp.Object.Diff")>]
[<assembly: AssemblyProductAttribute("FSharp.Obejct.Diff")>]
[<assembly: AssemblyDescriptionAttribute("")>]
[<assembly: InternalsVisibleToAttribute("FSharp.Object.Diff.Tests")>]
[<assembly: AssemblyVersionAttribute("0.8.0")>]
[<assembly: AssemblyFileVersionAttribute("0.8.0")>]
[<assembly: AssemblyInformationalVersionAttribute("0.8.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.8.0"
    let [<Literal>] InformationalVersion = "0.8.0"
