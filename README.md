## Usage

1. Add this repository as a submodule to your project under the `framework` directory.

2. Use it in your `build.mill` file:

```scala
import build.framework

// Refer to the modules via `build.framework.shared`, etc.
//
// For example:
object appServerPrelude extends AppScalaModule {
  override def moduleDeps = Seq(appShared.jvm, build.framework.server)
}
```

See https://github.com/arturaz/scala-web-framework-build for an example project that uses this framework.

## Documentation

For now auto-generated documentation will have to do: https://deepwiki.com/arturaz/scala-web-framework
