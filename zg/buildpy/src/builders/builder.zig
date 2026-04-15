//! Abstract builder base class
//!
//! Provides the common interface and functionality for all builders.

const std = @import("std");
const shell = @import("../shell.zig");
const project_mod = @import("../project.zig");
const config = @import("../config.zig");
const errors = @import("../errors.zig");
const log = @import("../log.zig");
const platform = @import("../platform.zig");

const BuildError = errors.BuildError;
const Shell = shell.Shell;
const Project = project_mod.Project;
const Version = config.Version;
const Io = std.Io;

/// Abstract builder interface
pub const AbstractBuilder = struct {
    allocator: std.mem.Allocator,
    io: Io,
    shell: Shell,
    project: *Project,
    logger: *log.Logger,
    version_str: []const u8,

    // Builder metadata (to be set by concrete implementations)
    name: []const u8,
    download_url_template: []const u8,
    download_archive_template: []const u8,
    lib_products: []const []const u8,

    const Self = @This();

    pub fn init(
        allocator: std.mem.Allocator,
        io: Io,
        project: *Project,
        name: []const u8,
        version_str: []const u8,
        download_url_template: []const u8,
        download_archive_template: []const u8,
        lib_products: []const []const u8,
    ) Self {
        return .{
            .allocator = allocator,
            .io = io,
            .shell = Shell.init(allocator, io),
            .project = project,
            .logger = log.getLogger(),
            .version_str = version_str,
            .name = name,
            .download_url_template = download_url_template,
            .download_archive_template = download_archive_template,
            .lib_products = lib_products,
        };
    }

    /// Get name-version string like "openssl-1.1.1w"
    pub fn nameVersion(self: *const Self) ![]u8 {
        return std.fmt.allocPrint(self.allocator, "{s}-{s}", .{ self.name, self.version_str });
    }

    /// Perform template substitution: replace {ver} and {archive} in a string
    fn substituteTemplate(self: *const Self, template: []const u8, archive_str: ?[]const u8) ![]u8 {
        var result: std.ArrayList(u8) = .empty;
        errdefer result.deinit(self.allocator);

        var i: usize = 0;
        while (i < template.len) {
            if (std.mem.startsWith(u8, template[i..], "{archive}")) {
                if (archive_str) |a| {
                    result.appendSlice(self.allocator, a) catch return error.OutOfMemory;
                }
                i += 9;
            } else if (std.mem.startsWith(u8, template[i..], "{ver}")) {
                result.appendSlice(self.allocator, self.version_str) catch return error.OutOfMemory;
                i += 5;
            } else {
                result.append(self.allocator, template[i]) catch return error.OutOfMemory;
                i += 1;
            }
        }
        return result.toOwnedSlice(self.allocator) catch return error.OutOfMemory;
    }

    /// Get download archive filename
    pub fn downloadArchive(self: *const Self) ![]u8 {
        return self.substituteTemplate(self.download_archive_template, null);
    }

    /// Get download URL
    pub fn downloadUrl(self: *const Self) ![]u8 {
        const archive = try self.downloadArchive();
        defer self.allocator.free(archive);
        return self.substituteTemplate(self.download_url_template, archive);
    }

    /// Get path to downloaded archive
    pub fn downloadedArchive(self: *const Self) ![]u8 {
        const archive = try self.downloadArchive();
        defer self.allocator.free(archive);
        return std.fs.path.join(self.allocator, &.{ self.project.downloads, archive });
    }

    /// Check if archive is already downloaded
    pub fn archiveIsDownloaded(self: *Self) !bool {
        const archive_path = try self.downloadedArchive();
        defer self.allocator.free(archive_path);
        return self.shell.exists(archive_path);
    }

    /// Get source directory path
    pub fn srcDir(self: *const Self) ![]u8 {
        const name_ver = try self.nameVersion();
        defer self.allocator.free(name_ver);
        return std.fs.path.join(self.allocator, &.{ self.project.src, name_ver });
    }

    /// Get prefix (install) directory
    pub fn prefix(self: *const Self) ![]u8 {
        var lower_name: [64]u8 = undefined;
        const lower_len = @min(self.name.len, 64);
        for (self.name[0..lower_len], 0..) |c, i| {
            lower_name[i] = std.ascii.toLower(c);
        }
        return std.fs.path.join(self.allocator, &.{ self.project.install, lower_name[0..lower_len] });
    }

    /// Get static library name
    pub fn staticLibName(self: *const Self) ![]u8 {
        const suffix = platform.PLATFORM.staticLibSuffix();
        return std.fmt.allocPrint(self.allocator, "lib{s}{s}", .{ self.name, suffix });
    }

    /// Get dynamic library name
    pub fn dylibName(self: *const Self) ![]u8 {
        const suffix = platform.PLATFORM.sharedLibSuffix();
        return std.fmt.allocPrint(self.allocator, "lib{s}{s}", .{ self.name, suffix });
    }

    /// Check if all library products exist
    pub fn libProductsExist(self: *Self) !bool {
        const pfx = try self.prefix();
        defer self.allocator.free(pfx);

        for (self.lib_products) |product| {
            const product_path = std.fs.path.join(self.allocator, &.{ pfx, "lib", product }) catch return error.OutOfMemory;
            defer self.allocator.free(product_path);
            if (!self.shell.exists(product_path)) {
                return false;
            }
        }
        return true;
    }

    // Build lifecycle methods (to be overridden by concrete implementations)

    pub fn preProcess(self: *Self) BuildError!void {
        _ = self;
    }

    pub fn setup(self: *Self) BuildError!void {
        try self.project.setup();

        // Download if needed
        if (!try self.archiveIsDownloaded()) {
            const url = try self.downloadUrl();
            defer self.allocator.free(url);
            const archive = try self.shell.download(url, self.project.downloads);
            self.allocator.free(archive);
        }

        // Extract if lib products don't exist
        if (!try self.libProductsExist()) {
            const src_dir = try self.srcDir();
            defer self.allocator.free(src_dir);

            if (self.shell.exists(src_dir)) {
                self.shell.remove(src_dir);
            }

            const archive_path = try self.downloadedArchive();
            defer self.allocator.free(archive_path);
            try self.shell.extract(archive_path, self.project.src);

            if (!self.shell.exists(src_dir)) {
                return error.SourceNotFound;
            }
        }
    }

    pub fn configure(self: *Self) BuildError!void {
        _ = self;
    }

    pub fn build(self: *Self) BuildError!void {
        _ = self;
    }

    pub fn install(self: *Self) BuildError!void {
        _ = self;
    }

    pub fn clean(self: *Self) BuildError!void {
        _ = self;
    }

    pub fn postProcess(self: *Self) BuildError!void {
        _ = self;
    }

    /// Main build process
    pub fn process(self: *Self) BuildError!void {
        try self.preProcess();
        try self.setup();
        try self.configure();
        try self.build();
        try self.install();
        try self.clean();
        try self.postProcess();
    }
};

test "abstract builder" {
    // This is just a compile-time test to ensure the types are correct
    _ = AbstractBuilder;
}
