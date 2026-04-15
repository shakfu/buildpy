//! buildpy - Build Python from source
//!
//! This is the public API for the buildpy library.
//! Use this module to programmatically build Python from source.

const std = @import("std");

// Re-export public modules
pub const errors = @import("errors.zig");
pub const platform = @import("platform.zig");
pub const log = @import("log.zig");
pub const shell = @import("shell.zig");
pub const config = @import("config.zig");
pub const project = @import("project.zig");

// Builder modules
pub const builders = struct {
    pub const builder = @import("builders/builder.zig");
    pub const openssl = @import("builders/openssl.zig");
    pub const bzip2 = @import("builders/bzip2.zig");
    pub const xz = @import("builders/xz.zig");
    pub const python = @import("builders/python.zig");
};

// Convenience re-exports
pub const BuildError = errors.BuildError;
pub const Platform = platform.Platform;
pub const Config = config.Config;
pub const Version = config.Version;
pub const Project = project.Project;
pub const PythonBuilder = builders.python.PythonBuilder;

/// Build Python from source with the specified configuration
pub fn buildPython(
    allocator: std.mem.Allocator,
    io: std.Io,
    version: []const u8,
    config_str: []const u8,
    jobs: u8,
    optimize: bool,
) !void {
    var proj = try Project.init(allocator, io);
    defer proj.deinit();

    var bldr = try PythonBuilder.init(
        allocator,
        io,
        &proj,
        version,
        config_str,
        jobs,
        optimize,
        true, // precompile
    );
    defer bldr.deinit();

    try bldr.process();
}

// Run all tests from submodules
test {
    // Import and run tests from all modules
    _ = @import("errors.zig");
    _ = @import("platform.zig");
    _ = @import("log.zig");
    _ = @import("shell.zig");
    _ = @import("config.zig");
    _ = @import("project.zig");
    _ = @import("builders/builder.zig");
    _ = @import("builders/openssl.zig");
    _ = @import("builders/bzip2.zig");
    _ = @import("builders/xz.zig");
    _ = @import("builders/python.zig");
}

test "public API sanity check" {
    // Just verify the types are accessible
    _ = BuildError;
    _ = Platform;
    _ = Config;
    _ = Version;
    _ = Project;
    _ = PythonBuilder;
}

test "platform detection" {
    const plat = Platform.current();
    // Should be a valid platform
    try std.testing.expect(plat.os != .unknown or plat.arch != .unknown);
}

test "version parsing" {
    const ver = try Version.parse("3.13.11");
    try std.testing.expectEqual(@as(u8, 3), ver.major);
    try std.testing.expectEqual(@as(u8, 13), ver.minor);
    try std.testing.expectEqual(@as(u8, 11), ver.patch);
}

test "config parsing" {
    const result = try Config.parseConfigString("static_max");
    try std.testing.expectEqual(config.BuildType.static, result.build_type);
    try std.testing.expectEqual(config.SizeType.max, result.size_type);
}
