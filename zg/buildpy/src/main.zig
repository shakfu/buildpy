//! buildpy - Build Python from source
//!
//! A tool for downloading, configuring, and building Python from source
//! with customization options for reducing build size and creating
//! relocatable installations.

const std = @import("std");
const log = @import("log.zig");
const config = @import("config.zig");
const project_mod = @import("project.zig");
const platform = @import("platform.zig");
const errors = @import("errors.zig");
const python_builder = @import("builders/python.zig");

const BuildError = errors.BuildError;
const PythonBuilder = python_builder.PythonBuilder;
const Project = project_mod.Project;
const Io = std.Io;

const VERSION = "0.1.0";

/// Command-line arguments
const Args = struct {
    version: []const u8,
    config_name: []const u8,
    jobs: u8,
    optimize: bool,
    precompile: bool,
    reset: bool,
    write_config: bool,
    help: bool,
    show_version: bool,

    pub fn default() Args {
        return .{
            .version = config.DEFAULT_PY_VERSION,
            .config_name = "shared_mid",
            .jobs = 4,
            .optimize = false,
            .precompile = true,
            .reset = false,
            .write_config = false,
            .help = false,
            .show_version = false,
        };
    }
};

fn printUsage() void {
    const usage =
        \\buildpy - Build Python from source
        \\
        \\USAGE:
        \\    buildpy [OPTIONS]
        \\
        \\OPTIONS:
        \\    -v, --version <VERSION>    Python version to build (default: 3.13.11)
        \\    -c, --config <CONFIG>      Build configuration (default: shared_mid)
        \\                               Available: static_max, static_mid, static_tiny,
        \\                                          shared_max, shared_mid,
        \\                                          framework_max, framework_mid
        \\    -j, --jobs <N>             Number of parallel build jobs (default: 4)
        \\    -o, --optimize             Enable build optimizations (--enable-optimizations)
        \\    -p, --precompile           Precompile stdlib to bytecode (default: true)
        \\    -r, --reset                Reset build directory before building
        \\    -w, --write                Write configuration file only (no build)
        \\    -h, --help                 Show this help message
        \\    --show-version             Show buildpy version
        \\
        \\EXAMPLES:
        \\    buildpy                           Build Python 3.13.11 with shared_mid config
        \\    buildpy -v 3.12.12 -c static_max  Build Python 3.12.12 with static_max config
        \\    buildpy -j 8 -o                   Build with 8 jobs and optimizations
        \\    buildpy -w -c framework_max       Write framework_max configuration
        \\
        \\CONFIGURATIONS:
        \\    static_max       Static build, maximum modules
        \\    static_mid       Static build, medium modules (no decimal)
        \\    static_tiny      Static build, minimal modules
        \\    shared_max       Shared library build, maximum modules
        \\    shared_mid       Shared library build, medium modules
        \\    framework_max    macOS Framework build, maximum modules
        \\    framework_mid    macOS Framework build, medium modules
        \\
    ;
    std.debug.print("{s}", .{usage});
}

fn printVersion() void {
    std.debug.print("buildpy {s}\n", .{VERSION});
}

fn parseArgs(argv: []const [*:0]const u8) Args {
    var args = Args.default();

    // argv[0] is program name, start from 1
    var i: usize = 1;
    while (i < argv.len) : (i += 1) {
        const arg = std.mem.sliceTo(argv[i], 0);
        if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            args.help = true;
            return args;
        } else if (std.mem.eql(u8, arg, "--show-version")) {
            args.show_version = true;
            return args;
        } else if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--version")) {
            i += 1;
            if (i < argv.len) {
                args.version = std.mem.sliceTo(argv[i], 0);
            }
        } else if (std.mem.eql(u8, arg, "-c") or std.mem.eql(u8, arg, "--config")) {
            i += 1;
            if (i < argv.len) {
                args.config_name = std.mem.sliceTo(argv[i], 0);
            }
        } else if (std.mem.eql(u8, arg, "-j") or std.mem.eql(u8, arg, "--jobs")) {
            i += 1;
            if (i < argv.len) {
                args.jobs = std.fmt.parseInt(u8, std.mem.sliceTo(argv[i], 0), 10) catch 4;
            }
        } else if (std.mem.eql(u8, arg, "-o") or std.mem.eql(u8, arg, "--optimize")) {
            args.optimize = true;
        } else if (std.mem.eql(u8, arg, "-p") or std.mem.eql(u8, arg, "--precompile")) {
            args.precompile = true;
        } else if (std.mem.eql(u8, arg, "-r") or std.mem.eql(u8, arg, "--reset")) {
            args.reset = true;
        } else if (std.mem.eql(u8, arg, "-w") or std.mem.eql(u8, arg, "--write")) {
            args.write_config = true;
        }
    }

    return args;
}

pub fn main(init: std.process.Init) !void {
    const allocator = init.gpa;
    const io = init.io;

    // Parse arguments
    const args = parseArgs(init.minimal.args.vector);

    if (args.help) {
        printUsage();
        return;
    }

    if (args.show_version) {
        printVersion();
        return;
    }

    // Initialize logging
    const debug_env = std.c.getenv("DEBUG");
    const log_level: log.Level = if (debug_env != null) .debug else .info;
    log.init(.{ .level = log_level, .name = "buildpy" });

    const logger = log.getLogger();
    logger.info("buildpy {s}", .{VERSION});
    logger.info("Platform: {s}-{s}", .{ platform.PLATFORM.os.name(), platform.PLATFORM.arch.name() });
    logger.info("Building Python {s} with {s} configuration", .{ args.version, args.config_name });

    // Check platform support
    if (!platform.PLATFORM.isUnix()) {
        logger.critical("Only Unix platforms (macOS, Linux) are currently supported", .{});
        return;
    }

    // Initialize project
    var project = Project.init(allocator, io) catch |e| {
        logger.critical("Failed to initialize project: {s}", .{errors.formatError(e)});
        return;
    };
    defer project.deinit();

    // Handle reset
    if (args.reset) {
        logger.info("Resetting build directory", .{});
        project.reset();
    }

    // Write config only mode
    if (args.write_config) {
        const parsed = config.Config.parseConfigString(args.config_name) catch |e| {
            logger.critical("Invalid config string: {s}", .{errors.formatError(e)});
            return;
        };

        var cfg = config.Config.init(allocator, io, args.version, parsed.build_type, parsed.size_type) catch |e| {
            logger.critical("Failed to create config: {s}", .{errors.formatError(e)});
            return;
        };
        defer cfg.deinit();

        // Create patch directory if needed
        const mkdir_argv = [_][]const u8{ "mkdir", "-p", "patch" };
        var mkdir_child = std.process.spawn(io, .{ .argv = &mkdir_argv }) catch {
            logger.critical("Failed to create patch directory", .{});
            return;
        };
        _ = mkdir_child.wait(io) catch {};

        // Normalize config name for filename
        var config_filename: [64]u8 = undefined;
        var i: usize = 0;
        for (args.config_name) |c| {
            if (i >= config_filename.len - 1) break;
            config_filename[i] = if (c == '_') '.' else c;
            i += 1;
        }

        const cfg_path = std.fs.path.join(allocator, &.{ "patch", config_filename[0..i] }) catch {
            logger.critical("Failed to create config path", .{});
            return;
        };
        defer allocator.free(cfg_path);

        cfg.write(cfg_path) catch |e| {
            logger.critical("Failed to write config: {s}", .{errors.formatError(e)});
            return;
        };

        logger.info("Configuration written to {s}", .{cfg_path});
        return;
    }

    // Build Python
    var builder = PythonBuilder.init(
        allocator,
        io,
        &project,
        args.version,
        args.config_name,
        args.jobs,
        args.optimize,
        args.precompile,
    ) catch |e| {
        logger.critical("Failed to initialize builder: {s}", .{errors.formatError(e)});
        return;
    };
    defer builder.deinit();

    builder.process() catch |e| {
        logger.critical("Build failed: {s}", .{errors.formatError(e)});
        return;
    };

    logger.info("Build completed successfully!", .{});
}

test "parse args default" {
    const args = Args.default();
    try std.testing.expectEqualStrings("3.13.11", args.version);
    try std.testing.expectEqualStrings("shared_mid", args.config_name);
    try std.testing.expectEqual(@as(u8, 4), args.jobs);
}
