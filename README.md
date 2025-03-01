## 🚧 WIP pure Scala implementation of Punycode

Without dependencies.

Sample usage

```scala
import punycode._
assert(Punycode.encode("hello 👋 world") == Punycode.decode("hello  world-m217k"))
````
