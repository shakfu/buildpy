//! Colored logging utilities for buildpy
//!
//! Provides formatted, colorized log output similar to Python's logging module.

const std = @import("std");
const c = @cImport({
    @cInclude("time.h");
});

/// ANSI color codes
pub const Color = struct {
    pub const reset = "\x1b[0m";
    pub const white = "\x1b[97;20m";
    pub const grey = "\x1b[38;20m";
    pub const green = "\x1b[32;20m";
    pub const cyan = "\x1b[36;20m";
    pub const yellow = "\x1b[33;20m";
    pub const red = "\x1b[31;20m";
    pub const bold_red = "\x1b[31;1m";
};

/// Log levels
pub const Level = enum(u8) {
    debug = 0,
    info = 1,
    warning = 2,
    err = 3,
    critical = 4,

    pub fn toString(self: Level) []const u8 {
        return switch (self) {
            .debug => "DEBUG",
            .info => "INFO",
            .warning => "WARNING",
            .err => "ERROR",
            .critical => "CRITICAL",
        };
    }

    pub fn color(self: Level) []const u8 {
        return switch (self) {
            .debug => Color.grey,
            .info => Color.green,
            .warning => Color.yellow,
            .err => Color.red,
            .critical => Color.bold_red,
        };
    }
};

/// Logger configuration
pub const LogConfig = struct {
    level: Level = .info,
    use_color: bool = true,
    name: []const u8 = "buildpy",
};

/// Get current time in milliseconds (monotonic)
fn currentTimeMs() i64 {
    var ts: c.struct_timespec = undefined;
    _ = c.clock_gettime(c.CLOCK_MONOTONIC, &ts);
    return @as(i64, ts.tv_sec) * 1000 + @divFloor(@as(i64, ts.tv_nsec), 1_000_000);
}

/// Main logger struct
pub const Logger = struct {
    config: LogConfig,
    start_time: i64,

    pub fn init(lc: LogConfig) Logger {
        return .{
            .config = lc,
            .start_time = currentTimeMs(),
        };
    }

    /// Format elapsed time as HH:MM:SS
    fn formatElapsed(self: *const Logger, buf: *[9]u8) []const u8 {
        const elapsed_ms = currentTimeMs() - self.start_time;
        const elapsed_secs: u64 = @intCast(@divFloor(elapsed_ms, 1000));
        const hours = elapsed_secs / 3600;
        const minutes = (elapsed_secs % 3600) / 60;
        const seconds = elapsed_secs % 60;
        return std.fmt.bufPrint(buf, "{d:0>2}:{d:0>2}:{d:0>2}", .{
            hours,
            minutes,
            seconds,
        }) catch "??:??:??";
    }

    fn shouldLog(self: *const Logger, level: Level) bool {
        return @intFromEnum(level) >= @intFromEnum(self.config.level);
    }

    fn logInternal(self: *const Logger, level: Level, comptime fmt: []const u8, args: anytype) void {
        if (!self.shouldLog(level)) return;

        var time_buf: [9]u8 = undefined;
        const elapsed = self.formatElapsed(&time_buf);

        if (self.config.use_color) {
            std.debug.print("{s}{s}{s} - {s}{s}{s} - {s}{s}{s} - {s}" ++ fmt ++ "{s}\n", .{
                Color.white,
                elapsed,
                Color.reset,
                level.color(),
                level.toString(),
                Color.reset,
                Color.white,
                self.config.name,
                Color.reset,
                Color.grey,
            } ++ args ++ .{Color.reset});
        } else {
            std.debug.print("{s} - {s} - {s} - " ++ fmt ++ "\n", .{
                elapsed,
                level.toString(),
                self.config.name,
            } ++ args);
        }
    }

    pub fn debug(self: *const Logger, comptime fmt: []const u8, args: anytype) void {
        self.logInternal(.debug, fmt, args);
    }

    pub fn info(self: *const Logger, comptime fmt: []const u8, args: anytype) void {
        self.logInternal(.info, fmt, args);
    }

    pub fn warning(self: *const Logger, comptime fmt: []const u8, args: anytype) void {
        self.logInternal(.warning, fmt, args);
    }

    pub fn err(self: *const Logger, comptime fmt: []const u8, args: anytype) void {
        self.logInternal(.err, fmt, args);
    }

    pub fn critical(self: *const Logger, comptime fmt: []const u8, args: anytype) void {
        self.logInternal(.critical, fmt, args);
    }
};

/// Default logger instance
var default_logger: ?Logger = null;

/// Initialize the default logger
pub fn init(lc: LogConfig) void {
    default_logger = Logger.init(lc);
}

/// Get the default logger, initializing if necessary
pub fn getLogger() *Logger {
    if (default_logger == null) {
        default_logger = Logger.init(.{});
    }
    return &default_logger.?;
}

/// Convenience functions using the default logger
pub fn debug(comptime fmt: []const u8, args: anytype) void {
    getLogger().debug(fmt, args);
}

pub fn info(comptime fmt: []const u8, args: anytype) void {
    getLogger().info(fmt, args);
}

pub fn warning(comptime fmt: []const u8, args: anytype) void {
    getLogger().warning(fmt, args);
}

pub fn err(comptime fmt: []const u8, args: anytype) void {
    getLogger().err(fmt, args);
}

pub fn critical(comptime fmt: []const u8, args: anytype) void {
    getLogger().critical(fmt, args);
}

test "logger level filtering" {
    var logger = Logger.init(.{ .level = .warning, .use_color = false });
    // These should compile without issues
    logger.debug("debug message", .{});
    logger.info("info message", .{});
    logger.warning("warning message", .{});
}

test "log level toString" {
    try std.testing.expectEqualStrings("DEBUG", Level.debug.toString());
    try std.testing.expectEqualStrings("INFO", Level.info.toString());
    try std.testing.expectEqualStrings("WARNING", Level.warning.toString());
    try std.testing.expectEqualStrings("ERROR", Level.err.toString());
    try std.testing.expectEqualStrings("CRITICAL", Level.critical.toString());
}
