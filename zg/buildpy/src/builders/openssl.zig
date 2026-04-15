//! OpenSSL builder
//!
//! Builds OpenSSL from source for Python's SSL support.

const std = @import("std");
const builder_mod = @import("builder.zig");
const project_mod = @import("../project.zig");
const errors = @import("../errors.zig");
const log = @import("../log.zig");

const BuildError = errors.BuildError;
const AbstractBuilder = builder_mod.AbstractBuilder;
const Project = project_mod.Project;
const Io = std.Io;

/// OpenSSL builder configuration
pub const OpensslBuilder = struct {
    base: AbstractBuilder,

    pub const NAME = "openssl";
    pub const VERSION = "1.1.1w";
    pub const DOWNLOAD_ARCHIVE_TEMPLATE = "openssl-{ver}.tar.gz";
    pub const DOWNLOAD_URL_TEMPLATE = "https://www.openssl.org/source/old/1.1.1/{archive}";
    pub const LIB_PRODUCTS = [_][]const u8{ "libssl.a", "libcrypto.a" };

    pub fn init(allocator: std.mem.Allocator, io: Io, project: *Project, version: ?[]const u8) OpensslBuilder {
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

    pub fn build(self: *OpensslBuilder) BuildError!void {
        if (try self.base.libProductsExist()) {
            self.base.logger.info("OpenSSL already built, skipping", .{});
            return;
        }

        const src_dir = try self.base.srcDir();
        defer self.base.allocator.free(src_dir);

        const pfx = try self.base.prefix();
        defer self.base.allocator.free(pfx);

        self.base.logger.info("Configuring OpenSSL...", .{});

        // Build configure command
        const config_cmd = try std.fmt.allocPrint(
            self.base.allocator,
            "./config no-shared no-tests --prefix={s}",
            .{pfx},
        );
        defer self.base.allocator.free(config_cmd);

        self.base.shell.setCwd(src_dir);
        defer self.base.shell.resetCwd();

        try self.base.shell.shellCmd(config_cmd);

        self.base.logger.info("Building OpenSSL...", .{});
        try self.base.shell.shellCmd("make install_sw");
        self.base.logger.info("OpenSSL build complete", .{});
    }

    pub fn process(self: *OpensslBuilder) BuildError!void {
        try self.base.preProcess();
        try self.base.setup();
        try self.build();
        try self.base.postProcess();
    }
};

test "openssl builder init" {
    // Compile-time test
    _ = OpensslBuilder;
}
