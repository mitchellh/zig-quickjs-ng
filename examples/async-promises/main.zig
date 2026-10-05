const std = @import("std");
const quickjs = @import("quickjs");

/// A simple async data fetcher that simulates fetching user data.
///
/// Demonstrates:
/// - Creating promises from Zig with initPromiseCapability
/// - Resolving/rejecting promises based on conditions
/// - Running the job queue to process async operations
/// - Using async/await and Promise.all from JavaScript
const UserService = struct {
    const User = struct {
        id: u32,
        name: [:0]const u8,
        email: [:0]const u8,
    };

    const users = [_]User{
        .{ .id = 1, .name = "Alice", .email = "alice@example.com" },
        .{ .id = 2, .name = "Bob", .email = "bob@example.com" },
        .{ .id = 3, .name = "Charlie", .email = "charlie@example.com" },
    };

    fn register(ctx: *quickjs.Context) !void {
        const global = ctx.getGlobalObject();
        defer global.deinit(ctx);
        try global.setPropertyStr(ctx, "fetchUser", .initCFunction(
            ctx,
            fetchUser,
            "fetchUser",
            1,
        ));
    }

    fn fetchUser(
        ctx: ?*quickjs.Context,
        _: quickjs.Value,
        args: []const quickjs.c.JSValue,
    ) quickjs.Value {
        const c = ctx.?;

        if (args.len == 0) return c.throwTypeError("fetchUser requires a user ID");
        const id = quickjs.Value.fromCVal(args[0]).toInt32(c) catch
            return c.throwTypeError("User ID must be a number");

        const promise = quickjs.Value.initPromiseCapability(c);
        defer promise.resolve.deinit(c);
        defer promise.reject.deinit(c);

        for (users) |user| {
            if (user.id != @as(u32, @intCast(id))) continue;
            const user_obj: quickjs.Value = .initObject(c);
            user_obj.setPropertyStr(c, "id", .initInt32(@intCast(user.id))) catch {};
            user_obj.setPropertyStr(c, "name", .initString(c, user.name)) catch {};
            user_obj.setPropertyStr(c, "email", .initString(c, user.email)) catch {};
            _ = promise.resolve.call(c, .undefined, &.{user_obj});
            user_obj.deinit(c);
            return promise.value;
        }

        const error_val: quickjs.Value = .initError(c);
        const msg: quickjs.Value = .initStringLen(c, "User not found");
        error_val.setPropertyStr(c, "message", msg) catch {};
        _ = promise.reject.call(c, .undefined, &.{error_val});
        error_val.deinit(c);

        return promise.value;
    }
};

fn runPendingJobs(rt: *quickjs.Runtime) !void {
    while (rt.isJobPending()) _ = try rt.executePendingJob();
}

pub fn main() !void {
    const rt: *quickjs.Runtime = try .init();
    defer rt.deinit();

    const ctx: *quickjs.Context = try .init(rt);
    defer ctx.deinit();

    try UserService.register(ctx);

    std.debug.print("=== Async Promises Example ===\n\n", .{});

    std.debug.print("1. Fetching user with ID 2...\n", .{});
    const result1 = ctx.eval(
        \\fetchUser(2).then(user => {
        \\    globalThis.result = `Found: ${user.name} <${user.email}>`;
        \\}).catch(err => {
        \\    globalThis.result = `Error: ${err.message}`;
        \\});
    , "<example>", .{});
    defer result1.deinit(ctx);

    try runPendingJobs(rt);

    const global = ctx.getGlobalObject();
    defer global.deinit(ctx);
    const output1 = global.getPropertyStr(ctx, "result");
    defer output1.deinit(ctx);
    if (output1.toCString(ctx)) |str1| {
        defer ctx.freeCString(str1);
        std.debug.print("   {s}\n\n", .{str1});
    } else {
        std.debug.print("   undefined\n\n", .{});
    }

    std.debug.print("2. Fetching non-existent user (ID 99)...\n", .{});
    const result2 = ctx.eval(
        \\fetchUser(99).then(user => {
        \\    globalThis.result2 = `Found: ${user.name}`;
        \\}).catch(err => {
        \\    globalThis.result2 = `Error: ${err.message}`;
        \\});
    , "<example>", .{});
    defer result2.deinit(ctx);

    try runPendingJobs(rt);

    const output2 = global.getPropertyStr(ctx, "result2");
    defer output2.deinit(ctx);
    if (output2.toCString(ctx)) |str2| {
        defer ctx.freeCString(str2);
        std.debug.print("   {s}\n\n", .{str2});
    } else {
        std.debug.print("   undefined\n\n", .{});
    }

    std.debug.print("3. Using async/await syntax...\n", .{});
    const result3 = ctx.eval(
        \\(async function() {
        \\    try {
        \\        const alice = await fetchUser(1);
        \\        const bob = await fetchUser(2);
        \\        globalThis.result3 = `Loaded ${alice.name} and ${bob.name}`;
        \\    } catch (err) {
        \\        globalThis.result3 = `Failed: ${err.message}`;
        \\    }
        \\})();
    , "<example>", .{});
    defer result3.deinit(ctx);

    try runPendingJobs(rt);

    const output3 = global.getPropertyStr(ctx, "result3");
    defer output3.deinit(ctx);
    if (output3.toCString(ctx)) |str3| {
        defer ctx.freeCString(str3);
        std.debug.print("   {s}\n\n", .{str3});
    } else {
        std.debug.print("   undefined\n\n", .{});
    }

    std.debug.print("4. Promise.all for parallel fetching...\n", .{});
    const result4 = ctx.eval(
        \\Promise.all([fetchUser(1), fetchUser(2), fetchUser(3)])
        \\    .then(users => {
        \\        globalThis.result4 = `Loaded ${users.length} users: ` +
        \\            users.map(u => u.name).join(', ');
        \\    })
        \\    .catch(err => {
        \\        globalThis.result4 = `Failed: ${err.message}`;
        \\    });
    , "<example>", .{});
    defer result4.deinit(ctx);

    try runPendingJobs(rt);

    const output4 = global.getPropertyStr(ctx, "result4");
    defer output4.deinit(ctx);
    if (output4.toCString(ctx)) |str4| {
        defer ctx.freeCString(str4);
        std.debug.print("   {s}\n", .{str4});
    } else {
        std.debug.print("   undefined\n", .{});
    }
}
