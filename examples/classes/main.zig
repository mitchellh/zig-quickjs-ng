const std = @import("std");
const quickjs = @import("quickjs");

/// A simple Counter class exposed to JavaScript.
///
/// Demonstrates:
/// - Custom class registration with finalizer
/// - Methods (increment, decrement, reset)
/// - Read-only property (value)
/// - Constructor with optional initial value
const Counter = struct {
    value: i32,
    allocator: std.mem.Allocator,

    var class_id: quickjs.ClassId = .invalid;

    fn init(allocator: std.mem.Allocator, initial: i32) !*Counter {
        const self = try allocator.create(Counter);
        self.* = .{ .value = initial, .allocator = allocator };
        return self;
    }

    fn deinit(self: *Counter) void {
        self.allocator.destroy(self);
    }

    /// Register the Counter class with the runtime and context.
    fn register(rt: *quickjs.Runtime, ctx: *quickjs.Context) !void {
        if (class_id == .invalid) class_id = .new(rt);

        const def: quickjs.ClassDef = .{
            .class_name = "Counter",
            .finalizer = @ptrCast(&finalizer),
        };
        try rt.newClass(class_id, &def);

        const proto: quickjs.Value = .initObject(ctx);
        try proto.setPropertyFunctionList(ctx, proto_funcs);
        ctx.setClassProto(class_id, proto);

        const ctor: quickjs.Value = .initCFunction2(ctx, constructor, "Counter", 1, .constructor, 0);
        const global = ctx.getGlobalObject();
        defer global.deinit(ctx);
        try global.setPropertyStr(ctx, "Counter", ctor);
    }

    const proto_funcs: []const quickjs.cfunc.FunctionListEntry = &.{
        quickjs.cfunc.FunctionListEntryHelpers.func("increment", 0, increment),
        quickjs.cfunc.FunctionListEntryHelpers.func("decrement", 0, decrement),
        quickjs.cfunc.FunctionListEntryHelpers.func("reset", 0, reset),
        quickjs.cfunc.FunctionListEntryHelpers.getset("value", getValue, null),
    };

    fn finalizer(rt: *quickjs.Runtime, val: quickjs.Value) callconv(.c) void {
        _ = rt;
        if (val.getOpaque(Counter, class_id)) |counter| {
            counter.deinit();
        }
    }

    fn constructor(
        ctx: ?*quickjs.Context,
        _: quickjs.Value,
        args: []const quickjs.c.JSValue,
    ) quickjs.Value {
        const c = ctx.?;

        const initial: i32 = if (args.len > 0)
            quickjs.Value.fromCVal(args[0]).toInt32(c) catch 0
        else
            0;

        const counter = Counter.init(
            std.heap.c_allocator,
            initial,
        ) catch return c.throwTypeError("Failed to allocate Counter");

        const obj: quickjs.Value = .initObjectClass(c, class_id);
        if (!obj.setOpaque(counter)) {
            counter.deinit();
            return c.throwTypeError("Failed to set opaque data");
        }

        return obj;
    }

    fn increment(
        ctx: ?*quickjs.Context,
        this: quickjs.Value,
        _: []const quickjs.c.JSValue,
    ) quickjs.Value {
        const c = ctx.?;
        const counter = this.getOpaque2(c, Counter, class_id) orelse
            return c.throwTypeError("Not a Counter instance");
        counter.value += 1;
        return .initInt32(counter.value);
    }

    fn decrement(
        ctx: ?*quickjs.Context,
        this: quickjs.Value,
        _: []const quickjs.c.JSValue,
    ) quickjs.Value {
        const c = ctx.?;
        const counter = this.getOpaque2(c, Counter, class_id) orelse
            return c.throwTypeError("Not a Counter instance");
        counter.value -= 1;
        return .initInt32(counter.value);
    }

    fn reset(
        ctx: ?*quickjs.Context,
        this: quickjs.Value,
        _: []const quickjs.c.JSValue,
    ) quickjs.Value {
        const c = ctx.?;
        const counter = this.getOpaque2(c, Counter, class_id) orelse
            return c.throwTypeError("Not a Counter instance");
        counter.value = 0;
        return .undefined;
    }

    fn getValue(ctx: ?*quickjs.Context, this: quickjs.Value) quickjs.Value {
        const c = ctx.?;
        const counter = this.getOpaque2(c, Counter, class_id) orelse
            return c.throwTypeError("Not a Counter instance");
        return .initInt32(counter.value);
    }
};

pub fn main() !void {
    const rt: *quickjs.Runtime = try .init();
    defer rt.deinit();

    const ctx: *quickjs.Context = try .init(rt);
    defer ctx.deinit();

    try Counter.register(rt, ctx);

    const result = ctx.eval(
        \\const c = new Counter(10);
        \\c.increment();
        \\c.increment();
        \\c.decrement();
        \\`Counter value: ${c.value}`
    , "<example>", .{});
    defer result.deinit(ctx);

    if (result.isException()) {
        const exc = ctx.getException();
        defer exc.deinit(ctx);
        const msg = exc.toCString(ctx) orelse "unknown error";
        defer ctx.freeCString(msg);
        std.debug.print("Error: {s}\n", .{msg});
        return error.JavaScriptException;
    }

    const str = result.toCString(ctx) orelse return error.NotAString;
    defer ctx.freeCString(str);
    std.debug.print("{s}\n", .{str});
}
