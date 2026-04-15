//! Bzip2 builder
//!
//! Builds bzip2 from source for Python's compression support.

const std = @import("std");
const builder_mod = @import("builder.zig");
const project_mod = @import("../project.zig");
const errors = @import("../errors.zig");
const log = @import("../log.zig");

const BuildError = errors.BuildError;
const AbstractBuilder = builder_mod.AbstractBuilder;
const Project = project_mod.Project;
const Io = std.Io;

/// Bzip2 builder configuration
pub const Bzip2Builder = struct {
    base: AbstractBuilder,

    pub const NAME = "bzip2";
    pub const VERSION = "1.0.8";
    pub const DOWNLOAD_ARCHIVE_TEMPLATE = "bzip2-{ver}.tar.gz";
    pub const DOWNLOAD_URL_TEMPLATE = "https://sourceware.org/pub/bzip2/{archive}";
    pub const LIB_PRODUCTS = [_][]const u8{"libbz2.a"};

    pub fn init(allocator: std.mem.Allocator, io: Io, project: *Project, version: ?[]const u8) Bzip2Builder {
        return .{
            .base = AbstractBuilder.init(
                allocator,
                io,
                project,
                NAME,
                version orelse VERSION,
                DOWNLOAD_URL_TEMPLATE,
                DOWNLOAD_ARCHIVE_TEMPLATE,
                &LIB_PRODUCTS,
            ),
        };
    }

    pub fn build(self: *Bzip2Builder) BuildError!void {
        if (try self.base.libProductsExist()) {
            self.base.logger.info("Bzip2 already built, skipping", .{});
            return;
        }

        const src_dir = try self.base.srcDir();
        defer self.base.allocator.free(src_dir);

        const pfx = try self.base.prefix();
        defer self.base.allocator.free(pfx);

        self.base.logger.info("Building Bzip2...", .{});

        // Build with -fPIC for position-independent code
        const build_cmd = try std.fmt.allocPrint(
            self.base.allocator,
            "make install PREFIX={s} CFLAGS='-fPIC'",
            .{pfx},
        );
        defer self.base.allocator.free(build_cmd);

        self.base.shell.setCwd(src_dir);
        defer self.base.shell.resetCwd();

        try self.base.shell.shellCmd(build_cmd);
        self.base.logger.info("Bzip2 build complete", .{});
    }

    pub fn process(self: *Bzip2Builder) BuildError!void {
        try self.base.preProcess();
        try self.base.setup();
        try self.build();
        try self.base.postProcess();
    }
};

test "bzip2 builder init" {
    // Compile-time test
    _ = Bzip2Builder;
}
