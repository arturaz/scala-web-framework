This is a source-only module.

Use it as follows:

```scala
object YourScalaJsModule {
  override def sources = Task.Sources(
    "src",
    BuildCtx.workspaceRoot / "framework" / "clientSupabase" / "src",
  )
}
```
