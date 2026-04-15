//! Project directory structure management
//!
//! Manages the build directory layout for buildpy.

const std = @import("std");
const shell = @import("shell.zig");
const errors = @import("errors.zig");
const log = @import("log.zig");

const BuildError = errors.BuildError;
const Shell = shell.Shell;
const Io = std.Io;
const Dir = Io.Dir;

/// Project directory paths
pub const Project = struct {
    allocator: std.mem.Allocator,
    io: Io,
    shell: Shell,
    logger: *log.Logger,

    /// Root directory (current working directory)
    root: []const u8,
    /// Build directory
    build: []const u8,
    /// Support directory (for packaged builds)
    support: []const u8,
    /// Downloads directory
    downloads: []const u8,
    /// Source directory
    src: []const u8,
    /// Install directory
    install: []const u8,
    /// Bin directory
    bin: []const u8,
    /// Lib directory
    lib: []const u8,

    pub fn init(allocator: std.mem.Allocator, io: Io) BuildError!Project {
        // Get cwd via libc
        var cwd_buf: [4096]u8 = undefined;
        const c_cwd = std.c.getcwd(&cwd_buf, cwd_buf.len) orelse return error.IoError;
        const cwd_slice = std.mem.sliceTo(c_cwd, 0);
        const cwd = allocator.dupe(u8, cwd_slice) catch return error.OutOfMemory;
        errdefer allocator.free(cwd);

        const build_dir = std.fs.path.join(allocator, &.{ cwd, "build" }) catch return error.OutOfMemory;
        errdefer allocator.free(build_dir);

        const support_dir = std.fs.path.join(allocator, &.{ cwd, "support" }) catch return error.OutOfMemory;
        errdefer allocator.free(support_dir);

        const downloads_dir = std.fs.path.join(allocator, &.{ build_dir, "downloads" }) catch return error.OutOfMemory;
        errdefer allocator.free(downloads_dir);

        const src_dir = std.fs.path.join(allocator, &.{ build_dir, "src" }) catch return error.OutOfMemory;
        errdefer allocator.free(src_dir);

        const install_dir = std.fs.path.join(allocator, &.{ build_dir, "install" }) catch return error.OutOfMemory;
        errdefer allocator.free(install_dir);

        const bin_dir = std.fs.path.join(allocator, &.{ build_dir, "bin" }) catch return error.OutOfMemory;
        errdefer allocator.free(bin_dir);

        const lib_dir = std.fs.path.join(allocator, &.{ build_dir, "lib" }) catch return error.OutOfMemory;
        errdefer allocator.free(lib_dir);

        return .{
            .allocator = allocator,
            .io = io,
            .shell = Shell.init(allocator, io),
            .logger = log.getLogger(),
            .root = cwd,
            .build = build_dir,
            .support = support_dir,
            .downloads = downloads_dir,
            .src = src_dir,
            .install = install_dir,
            .bin = bin_dir,
            .lib = lib_dir,
        };
    }

    pub fn deinit(self: *Project) void {
        self.allocator.free(self.root);
        self.allocator.free(self.build);
        self.allocator.free(self.support);
        self.allocator.free(self.downloads);
        self.allocator.free(self.src);
        self.allocator.free(self.install);
        self.allocator.free(self.bin);
        self.allocator.free(self.lib);
    }

    /// Create main project directories
    pub fn setup(self: *Project) BuildError!void {
        self.logger.debug("Setting up project directories", .{});
        try self.shell.makedirs(self.build);
        try self.shell.makedirs(self.downloads);
        try self.shell.makedirs(self.install);
        try self.shell.makedirs(self.src);
    }

    /// Reset project for a rebuild
    pub fn reset(self: *Project) void {
        self.logger.info("Resetting project", .{});
        self.shell.remove(self.src);

        // Remove installed python
        const python_install = std.fs.path.join(self.allocator, &.{ self.install, "python" }) catch return;
        defer self.allocator.free(python_install);
        self.shell.remove(python_install);
    }

    /// Get path relative to project root
    pub fn relativePath(self: *Project, subpath: []const u8) ![]u8 {
        return std.fs.path.join(self.allocator, &.{ self.root, subpath });
    }

    /// Get path relative to build directory
    pub fn buildPath(self: *Project, subpath: []const u8) ![]u8 {
        return std.fs.path.join(self.allocator, &.{ self.build, subpath });
    }

    /// Get path relative to source directory
    pub fn srcPath(self: *Project, subpath: []const u8) ![]u8 {
        return std.fs.path.join(self.allocator, &.{ self.src, subpath });
    }

    /// Get path relative to install directory
    pub fn installPath(self: *Project, subpath: []const u8) ![]u8 {
        return std.fs.path.join(self.allocator, &.{ self.install, subpath });
    }

    /// Check if build directory exists
    pub fn buildExists(self: *Project) bool {
        return self.shell.exists(self.build);
    }

    /// Check if downloads directory exists
    pub fn downloadsExists(self: *Project) bool {
        return self.shell.exists(self.downloads);
    }
};

test "project types" {
    // Compile-time test
    _ = Project;
}
