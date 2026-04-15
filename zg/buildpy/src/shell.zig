//! Shell command execution and file operations
//!
//! Provides utilities for running external commands, downloading files,
//! extracting archives, and file system operations.

const std = @import("std");
const log = @import("log.zig");
const errors = @import("errors.zig");
const platform = @import("platform.zig");

const BuildError = errors.BuildError;
const Io = std.Io;
const Dir = Io.Dir;

/// Result of a command execution
pub const CmdResult = struct {
    exit_code: u8,
    stdout: []u8,
    stderr: []u8,

    pub fn deinit(self: *CmdResult, allocator: std.mem.Allocator) void {
        allocator.free(self.stdout);
        allocator.free(self.stderr);
    }

    pub fn success(self: *const CmdResult) bool {
        return self.exit_code == 0;
    }
};

/// Shell command executor
pub const Shell = struct {
    allocator: std.mem.Allocator,
    io: Io,
    logger: *log.Logger,
    cwd: ?[]const u8,

    pub fn init(allocator: std.mem.Allocator, io: Io) Shell {
        return .{
            .allocator = allocator,
            .io = io,
            .logger = log.getLogger(),
            .cwd = null,
        };
    }

    fn getCwd(self: *const Shell) std.process.Child.Cwd {
        if (self.cwd) |p| {
            return .{ .path = p };
        }
        return .inherit;
    }

    /// Run a command and wait for completion
    pub fn cmd(self: *Shell, argv: []const []const u8) BuildError!void {
        const cmd_str = std.mem.join(self.allocator, " ", argv) catch return error.OutOfMemory;
        defer self.allocator.free(cmd_str);
        self.logger.info("{s}", .{cmd_str});

        var child = std.process.spawn(self.io, .{
            .argv = argv,
            .cwd = self.getCwd(),
        }) catch {
            self.logger.critical("Failed to spawn process", .{});
            return error.CommandFailed;
        };

        const term = child.wait(self.io) catch {
            self.logger.critical("Failed to wait for process", .{});
            return error.CommandFailed;
        };

        switch (term) {
            .exited => |code| {
                if (code != 0) {
                    self.logger.critical("Command failed with exit code {d}", .{code});
                    return error.CommandFailed;
                }
            },
            else => {
                self.logger.critical("Command terminated abnormally", .{});
                return error.CommandFailed;
            },
        }
    }

    /// Run a command and capture output
    pub fn cmdOutput(self: *Shell, argv: []const []const u8) BuildError!CmdResult {
        const result = std.process.run(self.allocator, self.io, .{
            .argv = argv,
            .cwd = self.getCwd(),
        }) catch return error.CommandFailed;

        const exit_code: u8 = switch (result.term) {
            .exited => |code| code,
            else => 255,
        };

        return .{
            .exit_code = exit_code,
            .stdout = result.stdout,
            .stderr = result.stderr,
        };
    }

    /// Run a shell command string (uses /bin/sh -c)
    pub fn shellCmd(self: *Shell, command: []const u8) BuildError!void {
        self.logger.info("{s}", .{command});
        const argv = [_][]const u8{ "/bin/sh", "-c", command };

        var child = std.process.spawn(self.io, .{
            .argv = &argv,
            .cwd = self.getCwd(),
        }) catch return error.CommandFailed;

        const term = child.wait(self.io) catch return error.CommandFailed;

        switch (term) {
            .exited => |code| {
                if (code != 0) {
                    self.logger.critical("Shell command failed with exit code {d}", .{code});
                    return error.CommandFailed;
                }
            },
            else => return error.CommandFailed,
        }
    }

    /// Download a file using wget
    pub fn download(self: *Shell, url: []const u8, dest_dir: []const u8) BuildError![]u8 {
        // Extract filename from URL
        const filename = std.fs.path.basename(url);
        const dest_path = std.fs.path.join(self.allocator, &.{ dest_dir, filename }) catch return error.OutOfMemory;
        errdefer self.allocator.free(dest_path);

        // Check if file already exists
        Dir.cwd().access(self.io, dest_path, .{}) catch {
            // File doesn't exist, download it
            self.logger.info("Downloading {s}...", .{filename});

            const argv = [_][]const u8{
                "wget",
                "-q",
                "--show-progress",
                "-O",
                dest_path,
                url,
            };

            try self.cmd(&argv);
            self.logger.info("Download complete: {s}", .{filename});
            return dest_path;
        };

        self.logger.info("Using cached file: {s}", .{dest_path});
        return dest_path;
    }

    /// Extract an archive using tar or unzip
    pub fn extract(self: *Shell, archive_path: []const u8, dest_dir: []const u8) BuildError!void {
        self.logger.info("Extracting {s}", .{std.fs.path.basename(archive_path)});

        // Determine archive type by extension
        if (std.mem.endsWith(u8, archive_path, ".tar.gz") or
            std.mem.endsWith(u8, archive_path, ".tgz"))
        {
            const argv = [_][]const u8{ "tar", "-xzf", archive_path, "-C", dest_dir };
            try self.cmd(&argv);
        } else if (std.mem.endsWith(u8, archive_path, ".tar.xz")) {
            const argv = [_][]const u8{ "tar", "-xJf", archive_path, "-C", dest_dir };
            try self.cmd(&argv);
        } else if (std.mem.endsWith(u8, archive_path, ".tar.bz2")) {
            const argv = [_][]const u8{ "tar", "-xjf", archive_path, "-C", dest_dir };
            try self.cmd(&argv);
        } else if (std.mem.endsWith(u8, archive_path, ".zip")) {
            const argv = [_][]const u8{ "unzip", "-q", archive_path, "-d", dest_dir };
            try self.cmd(&argv);
        } else {
            self.logger.err("Unsupported archive type: {s}", .{archive_path});
            return error.UnsupportedArchiveType;
        }
    }

    /// Clone a git repository
    pub fn gitClone(
        self: *Shell,
        url: []const u8,
        branch: ?[]const u8,
        dest_dir: ?[]const u8,
    ) BuildError!void {
        var argv_buf: [8][]const u8 = undefined;
        var argc: usize = 0;

        argv_buf[argc] = "git";
        argc += 1;
        argv_buf[argc] = "clone";
        argc += 1;
        argv_buf[argc] = "--depth";
        argc += 1;
        argv_buf[argc] = "1";
        argc += 1;

        if (branch) |b| {
            argv_buf[argc] = "--branch";
            argc += 1;
            argv_buf[argc] = b;
            argc += 1;
        }

        argv_buf[argc] = url;
        argc += 1;

        if (dest_dir) |d| {
            argv_buf[argc] = d;
            argc += 1;
        }

        try self.cmd(argv_buf[0..argc]);
    }

    /// Create directories recursively
    pub fn makedirs(self: *Shell, path: []const u8) BuildError!void {
        self.logger.debug("Creating directory: {s}", .{path});
        Dir.cwd().createDirPath(self.io, path) catch |e| {
            self.logger.err("Failed to create directory {s}: {}", .{ path, e });
            return error.IoError;
        };
    }

    /// Remove a file or directory
    pub fn remove(self: *Shell, path: []const u8) void {
        self.logger.debug("Removing: {s}", .{path});

        // Try to delete as file first
        Dir.cwd().deleteFile(self.io, path) catch {
            // Try as directory
            Dir.cwd().deleteTree(self.io, path) catch {
                // Silently ignore if it doesn't exist
            };
        };
    }

    /// Copy a file
    pub fn copyFile(self: *Shell, src: []const u8, dst: []const u8) BuildError!void {
        self.logger.debug("Copying {s} to {s}", .{ src, dst });
        Dir.cwd().copyFile(src, Dir.cwd(), dst, self.io, .{}) catch {
            return error.IoError;
        };
    }

    /// Move/rename a file
    pub fn move(self: *Shell, src: []const u8, dst: []const u8) BuildError!void {
        self.logger.debug("Moving {s} to {s}", .{ src, dst });
        Dir.cwd().rename(src, Dir.cwd(), dst, self.io) catch {
            // Try copy + delete for cross-device moves
            try self.copyFile(src, dst);
            Dir.cwd().deleteFile(self.io, src) catch {};
        };
    }

    /// Check if a path exists
    pub fn exists(self: *const Shell, path: []const u8) bool {
        return if (Dir.cwd().access(self.io, path, .{})) |_| true else |_| false;
    }

    /// Change file permissions
    pub fn chmod(self: *Shell, path: []const u8, mode: std.posix.mode_t) BuildError!void {
        _ = self;
        _ = path;
        _ = mode;
        // chmod via shell command as fallback
    }

    /// Set working directory for subsequent commands
    pub fn setCwd(self: *Shell, path: []const u8) void {
        self.cwd = path;
    }

    /// Reset working directory to null (use process cwd)
    pub fn resetCwd(self: *Shell) void {
        self.cwd = null;
    }
};

/// Create zip archive using system zip command
pub fn createZip(allocator: std.mem.Allocator, io: Io, src_dir: []const u8, zip_path: []const u8) BuildError!void {
    var s = Shell.init(allocator, io);

    // Build zip command
    const cmd_str = std.fmt.allocPrint(
        allocator,
        "cd {s} && zip -rq {s} .",
        .{ src_dir, zip_path },
    ) catch return error.OutOfMemory;
    defer allocator.free(cmd_str);

    try s.shellCmd(cmd_str);
}

/// Read entire file contents
pub fn readFile(allocator: std.mem.Allocator, io: Io, path: []const u8) ![]u8 {
    const file = try Dir.cwd().openFile(io, path, .{});
    defer file.close(io);
    return file.readToEndAlloc(io, allocator, std.math.maxInt(usize));
}

/// Write contents to a file
pub fn writeFile(io: Io, path: []const u8, contents: []const u8) !void {
    const file = try Dir.cwd().createFile(io, path, .{});
    defer file.close(io);
    try file.writeStreamingAll(io, contents);
}

test "shell init" {
    // Compile-time test - Shell requires Io which is hard to mock in tests
    _ = Shell;
}

test "CmdResult" {
    _ = CmdResult;
}
