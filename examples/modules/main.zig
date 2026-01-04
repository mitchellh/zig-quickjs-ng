const std = @import("std");
const quickjs = @import("quickjs");

/// A generic module loader that dispatches to registered native modules.
///
/// Modules implement a simple interface:
/// - `name`: The module specifier (e.g., "logger")
/// - `exports`: List of export names to declare
/// - `init`: Function to set export values when module is instantiated
fn ModuleLoader(comptime modules: []const type) type {
    return struct {
        fn load(_: void, ctx: *quickjs.Context, name: [:0]const u8) ?*quickjs.ModuleDef {
            inline for (modules) |Module| {
                if (std.mem.eql(u8, name, Module.name)) {
                    return loadModule(Module, ctx);
                }
            }
            return null;
        }

        fn loadModule(comptime Module: type, ctx: *quickjs.Context) ?*quickjs.ModuleDef {
            const m = quickjs.ModuleDef.init(ctx, Module.name, Module.init) orelse return null;
            inline for (Module.exports) |exp| {
                _ = m.addExport(ctx, exp);
            }
            return m;
        }
    };
}

/// A simple logging module that can be imported from JavaScript.
///
/// Usage from JavaScript:
///   import { info, warn, error, LOG_LEVELS } from "logger";
///   info("Application started");
const Logger = struct {
    const name = "logger";
    const exports: []const [:0]const u8 = &.{ "info", "warn", "error", "LOG_LEVELS" };

    fn init(ctx: *quickjs.Context, m: *quickjs.ModuleDef) bool {
        if (!m.setExport(ctx, "info", .initCFunction(ctx, info, "info", 1))) return false;
        if (!m.setExport(ctx, "warn", .initCFunction(ctx, warn, "warn", 1))) return false;
        if (!m.setExport(ctx, "error", .initCFunction(ctx, @"error", "error", 1))) return false;

        const levels: quickjs.Value = .initObject(ctx);
        levels.setPropertyStr(ctx, "INFO", .initInt32(0)) catch return false;
        levels.setPropertyStr(ctx, "WARN", .initInt32(1)) catch return false;
        levels.setPropertyStr(ctx, "ERROR", .initInt32(2)) catch return false;
        if (!m.setExport(ctx, "LOG_LEVELS", levels)) return false;

        return true;
    }

    fn info(ctx: ?*quickjs.Context, _: quickjs.Value, args: []const quickjs.c.JSValue) quickjs.Value {
        return log(ctx.?, .info, args);
    }

    fn warn(ctx: ?*quickjs.Context, _: quickjs.Value, args: []const quickjs.c.JSValue) quickjs.Value {
        return log(ctx.?, .warn, args);
    }

    fn @"error"(ctx: ?*quickjs.Context, _: quickjs.Value, args: []const quickjs.c.JSValue) quickjs.Value {
        return log(ctx.?, .@"error", args);
    }

    const Level = enum { info, warn, @"error" };

    fn log(ctx: *quickjs.Context, level: Level, args: []const quickjs.c.JSValue) quickjs.Value {
        const prefix = switch (level) {
            .info => "\x1b[34m[INFO]\x1b[0m",
            .warn => "\x1b[33m[WARN]\x1b[0m",
            .@"error" => "\x1b[31m[ERROR]\x1b[0m",
        };

        std.debug.print("{s} ", .{prefix});
        for (args) |arg| {
            const val: quickjs.Value = .fromCVal(arg);
            const str = val.toCString(ctx) orelse "<unconvertible>";
            defer ctx.freeCString(str);
            std.debug.print("{s} ", .{str});
        }
        std.debug.print("\n", .{});

        return .undefined;
    }
};

pub fn main() !void {
    const rt: *quickjs.Runtime = try .init();
    defer rt.deinit();

    const ctx: *quickjs.Context = try .init(rt);
    defer ctx.deinit();

    rt.setModuleLoaderFunc(void, {}, null, ModuleLoader(&.{Logger}).load);

    const result = ctx.eval(
        \\import { info, warn, error, LOG_LEVELS } from "logger";
        \\
        \\info("Application started");
        \\info("Log levels available:", JSON.stringify(LOG_LEVELS));
        \\warn("Memory usage is high");
        \\error("Connection timeout after 30s");
    , "<example>", .{ .type = .module });
    defer result.deinit(ctx);

    if (result.isException()) {
        const exc = ctx.getException();
        defer exc.deinit(ctx);
        const msg = exc.toCString(ctx) orelse "unknown error";
        defer ctx.freeCString(msg);
        std.debug.print("Error: {s}\n", .{msg});
        return error.JavaScriptException;
    }

    std.debug.print("Logging complete!\n", .{});
}
