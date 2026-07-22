# zig-quickjs-ng

Zig build and bindings for [QuickJS-ng](https://github.com/quickjs-ng/quickjs).

**API coverage:** The bindings cover around 95% of the full quickjs-ng
API, including unit tests from Zig to verify the bindings work correctly
for just about every type and function.

**AI Disclaimer:** I use AI assistance to help write and review code, but
I've manually reviewed and tested everything, and I'm using these bindings
in an actual project. I'm an expert at Zig, have written dozens of Zig
bindings to C projects, and have contributed to Zig itself. I'm not 
generating slop here.

## Example

```zig
const quickjs = @import("quickjs");

pub fn main() !void {
    const rt: *quickjs.Runtime = try .init();
    defer rt.deinit();

    const ctx: *quickjs.Context = try .init(rt);
    defer ctx.deinit();

    const result = ctx.eval(
        \\(function() { return 40 + 2; })()
    , "<main>", .{});
    defer result.deinit(ctx);

    if (result.isException()) {
        // Handle error
        return error.JavaScriptError;
    }

    const value = try result.toInt32(ctx);
    std.debug.assert(value == 42);
}
```

## Usage

**Zig version: zig-quickjs-ng only works with the released version of Zig specified
in the `build.zig.zon` file.** We don't support nightly versions because the Zig
compiler is still changing too much.

### Add Dependency

To add zig-quickjs-ng to your `build.zig.zon`:

```sh
zig fetch --save git+https://github.com/mitchellh/zig-quickjs-ng
```

### Configure build.zig

In your `build.zig`:

```zig
const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "my-app",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // Get the quickjs dependency
    const dep = b.dependency("quickjs", .{
        .target = target,
        .optimize = optimize,
    });

    // Add the Zig module
    exe.root_module.addImport("quickjs", dep.module("quickjs"));

    // Link the C library
    exe.root_module.linkLibrary(dep.artifact("quickjs-ng"));

    b.installArtifact(exe);
}
```

## Documentation

Read the source code and header files - they are well commented.
