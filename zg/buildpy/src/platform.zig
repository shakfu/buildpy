//! Platform detection and configuration
//!
//! Provides compile-time and runtime platform detection utilities.

const std = @import("std");
const builtin = @import("builtin");

/// Supported operating systems
pub const Os = enum {
    darwin,
    linux,
    windows,
    unknown,

    /// Get current OS at compile time
    pub fn current() Os {
        return switch (builtin.os.tag) {
            .macos => .darwin,
            .linux => .linux,
            .windows => .windows,
            else => .unknown,
        };
    }

    /// Get OS name as string
    pub fn name(self: Os) []const u8 {
        return switch (self) {
            .darwin => "darwin",
            .linux => "linux",
            .windows => "windows",
            .unknown => "unknown",
        };
    }

    /// Check if OS is Unix-like
    pub fn isUnix(self: Os) bool {
        return self == .darwin or self == .linux;
    }
};

/// Supported CPU architectures
pub const Arch = enum {
    x86_64,
    aarch64,
    unknown,

    /// Get current architecture at compile time
    pub fn current() Arch {
        return switch (builtin.cpu.arch) {
            .x86_64 => .x86_64,
            .aarch64 => .aarch64,
            else => .unknown,
        };
    }

    /// Get architecture name as string
    pub fn name(self: Arch) []const u8 {
        return switch (self) {
            .x86_64 => "x86_64",
            .aarch64 => "aarch64",
            .unknown => "unknown",
        };
    }
};

/// Platform information combining OS and architecture
pub const Platform = struct {
    os: Os,
    arch: Arch,

    /// Get current platform at compile time
    pub fn current() Platform {
        return .{
            .os = Os.current(),
            .arch = Arch.current(),
        };
    }

    /// Check if platform is Darwin (macOS)
    pub fn isDarwin(self: Platform) bool {
        return self.os == .darwin;
    }

    /// Check if platform is Linux
    pub fn isLinux(self: Platform) bool {
        return self.os == .linux;
    }

    /// Check if platform is Windows
    pub fn isWindows(self: Platform) bool {
        return self.os == .windows;
    }

    /// Check if platform is Unix-like
    pub fn isUnix(self: Platform) bool {
        return self.os.isUnix();
    }

    /// Get the static library suffix for this platform
    pub fn staticLibSuffix(self: Platform) []const u8 {
        return if (self.os == .windows) ".lib" else ".a";
    }

    /// Get the shared library suffix for this platform
    pub fn sharedLibSuffix(self: Platform) []const u8 {
        return switch (self.os) {
            .darwin => ".dylib",
            .linux => ".so",
            .windows => ".dll",
            .unknown => ".so",
        };
    }

    /// Get executable suffix for this platform
    pub fn exeSuffix(self: Platform) []const u8 {
        return if (self.os == .windows) ".exe" else "";
    }

    /// Get available build types for this platform
    pub fn getBuildTypes(self: Platform) []const []const u8 {
        return switch (self.os) {
            .darwin => &[_][]const u8{
                "local",
                "shared-ext",
                "static-ext",
                "framework-ext",
                "framework-pkg",
            },
            .linux => &[_][]const u8{
                "local",
                "shared-ext",
                "static-ext",
            },
            .windows => &[_][]const u8{
                "local",
                "windows-pkg",
            },
            .unknown => &[_][]const u8{"local"},
        };
    }

    /// Get platform string for naming (e.g., "darwin-x86_64")
    pub fn toString(self: Platform, allocator: std.mem.Allocator) ![]u8 {
        return std.fmt.allocPrint(allocator, "{s}-{s}", .{ self.os.name(), self.arch.name() });
    }
};

/// Global platform constant
pub const PLATFORM = Platform.current();

/// Setup platform-specific environment variables
pub fn setupEnvironment() void {
    if (PLATFORM.isDarwin()) {
        // Set MACOSX_DEPLOYMENT_TARGET if not already set
        const existing = std.posix.getenv("MACOSX_DEPLOYMENT_TARGET");
        if (existing == null) {
            // Note: In Zig we can't easily set env vars, this would need
            // to be done via child process spawning
        }
    }
}

test "platform detection" {
    const platform = Platform.current();
    // Platform should be one of the known types
    try std.testing.expect(platform.os != .unknown or builtin.os.tag != .macos);
}

test "os properties" {
    try std.testing.expect(Os.darwin.isUnix());
    try std.testing.expect(Os.linux.isUnix());
    try std.testing.expect(!Os.windows.isUnix());
}

test "platform suffixes" {
    const darwin_plat = Platform{ .os = .darwin, .arch = .x86_64 };
    try std.testing.expectEqualStrings(".dylib", darwin_plat.sharedLibSuffix());
    try std.testing.expectEqualStrings(".a", darwin_plat.staticLibSuffix());

    const linux_plat = Platform{ .os = .linux, .arch = .x86_64 };
    try std.testing.expectEqualStrings(".so", linux_plat.sharedLibSuffix());

    const win_plat = Platform{ .os = .windows, .arch = .x86_64 };
    try std.testing.expectEqualStrings(".dll", win_plat.sharedLibSuffix());
    try std.testing.expectEqualStrings(".exe", win_plat.exeSuffix());
}
