//! XZ/LZMA builder
//!
//! Builds xz/liblzma from source for Python's LZMA compression support.

const std = @import("std");
const builder_mod = @import("builder.zig");
const project_mod = @import("../project.zig");
const errors = @import("../errors.zig");
const log = @import("../log.zig");

const BuildError = errors.BuildError;
const AbstractBuilder = builder_mod.AbstractBuilder;
const Project = project_mod.Project;
const Io = std.Io;

/// XZ builder configuration
pub const XzBuilder = struct {
    base: AbstractBuilder,

    pub const NAME = "xz";
    pub const VERSION = "5.8.2";
    pub const DOWNLOAD_ARCHIVE_TEMPLATE = "xz-{ver}.tar.gz";
    pub const DOWNLOAD_URL_TEMPLATE = "https://github.com/tukaani-project/xz/releases/download/v{ver}/xz-{ver}.tar.gz";
    pub const LIB_PRODUCTS = [_][]const u8{"liblzma.a"};

    pub fn init(allocator: std.mem.Allocator, io: Io, project: *Project, version: ?[]const u8) XzBuilder {
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

    pub fn build(self: *XzBuilder) BuildError!void {
        if (try self.base.libProductsExist()) {
            self.base.logger.info("XZ already built, skipping", .{});
            return;
        }

        const src_dir = try self.base.srcDir();
        defer self.base.allocator.free(src_dir);

        const pfx = try self.base.prefix();
        defer self.base.allocator.free(pfx);

        self.base.logger.info("Configuring XZ...", .{});

        // Make configure executable via shell
        const chmod_cmd = try std.fmt.allocPrint(
            self.base.allocator,
            "chmod +x {s}/configure {s}/build-aux/install-sh",
            .{ src_dir, src_dir },
        );
        defer self.base.allocator.free(chmod_cmd);
        self.base.shell.shellCmd(chmod_cmd) catch {};

        // Configure command
        const config_cmd = try std.fmt.allocPrint(
            self.base.allocator,
            "/bin/sh configure --disable-dependency-tracking --disable-xzdec --disable-lzmadec --disable-nls --enable-small --disable-shared --prefix={s}",
            .{pfx},
        );
        defer self.base.allocator.free(config_cmd);

        self.base.shell.setCwd(src_dir);
        defer self.base.shell.resetCwd();

        try self.base.shell.shellCmd(config_cmd);

        self.base.logger.info("Building XZ...", .{});
        try self.base.shell.shellCmd("make && make install");
        self.base.logger.info("XZ build complete", .{});
    }

    pub fn process(self: *XzBuilder) BuildError!void {
        try self.base.preProcess();
        try self.base.setup();
        try self.build();
        try self.base.postProcess();
    }
};

test "xz builder init" {
    // Compile-time test
    _ = XzBuilder;
}
