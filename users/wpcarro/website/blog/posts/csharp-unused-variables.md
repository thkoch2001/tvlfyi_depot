**Problem**: This morning we broke production because (believe it or not) an
unused variable went undetected.

**Solution**: Consume the variable in the relevant codepath.

**Further AI**: Treat unused variables as errors (which will block CI).

## Warning/Disclaimer

I am not a C# programmer. I know close to nothing about C#. But at `$WORK`, one
of our codebases is written in C#, so occasionally I interface with it.

## Treating Unused Variables as Errors

C# uses `.csproj` files to configure projects. The following changes to our
`.csproj` file WAI'd:

```diff
+    <!-- IDE0059: Remove unnecessary value assignment -->
+    <WarningsAsErrors>IDE0059</WarningsAsErrors>
+    <EnforceCodeStyleInBuild>true</EnforceCodeStyleInBuild>
```

However, supporting this turned out to be a ~1h adventure... Why was this
unexpectedly difficult? As it turns out, there are the 3x promising compiler
warnings that I had to discover/try:

- `CS0219`: doesn't WAI (see "Note" here: https://docs.microsoft.com/en-us/dotnet/csharp/misc/cs0219)
- `CA1804`: silently unsupported (replaced by `IDE0059`)
- `IDE0059`: WAIs

Legend:
- `CS`: stands for C#
- `CA`: stands for Code Analysis (I *think* a Visual Studio concept)
- `IDE`: stands for IDE (I think *also* a Visual Studio concept)

For `CA` and `IDE` prefixed warnings, `EnforceCodeStyleInBuild` must also be
enabled.
