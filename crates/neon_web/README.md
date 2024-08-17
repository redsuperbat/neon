# Neon wasm bindings

This crate contains all of the wasm bindings needed to run neon in the browser.

The browser can execute neon programs by compiling to webassembly.

There are three interesting exports:

```ts
import { interpret_src, tokenize, compile } from "neon-web";

interpret_src("3 + 3"); // 6
tokenize("3 + 3");      // List of tokens
compile("3 + 3");       // Throws a diagnostic error if there is a syntax or symbol error
```
