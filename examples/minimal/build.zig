const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const quickjs = b.dependency("quickjs", .{
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "minimal",
        .root_module = b.createModule(.{
            .root_source_file = b.path("main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{.{
                .name = "quickjs",
                .module = quickjs.module("quickjs"),
            }},
        }),
        // Zig 0.15 crashes without this.
        .use_llvm = true,
    });
    exe.linkLibrary(quickjs.artifact("quickjs-ng"));
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the example");
    run_step.dependOn(&run_cmd.step);
}
