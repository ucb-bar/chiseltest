# chisel-testers2
See [freechipsproject/chisel3#725](https://github.com/freechipsproject/chisel3/issues/725).
Discussion should happen on the issue above.

Testers2 will be split into this repository during development to avoid the need for users to run a custom chisel3 branch.
Independent JARs will also be published, so users can use managed dependencies.

However, the code will be merged back into chisel3 once there is a final 'release', as chisel3 needs a comprehensive test framework.

## Build Notes
This may rely on some fresh Chisel3 APIs. You may need to clone the latest Chisel3 and do `sbt publishLocal`.
