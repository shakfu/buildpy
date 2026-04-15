//! Python builder
//!
//! Main builder for Python from source.

const std = @import("std");
const builder_mod = @import("builder.zig");
const openssl_mod = @import("openssl.zig");
const bzip2_mod = @import("bzip2.zig");
const xz_mod = @import("xz.zig");
const project_mod = @import("../project.zig");
const config_mod = @import("../config.zig");
const errors = @import("../errors.zig");
const log = @import("../log.zig");
const shell_mod = @import("../shell.zig");
const platform = @import("../platform.zig");

const BuildError = errors.BuildError;
const AbstractBuilder = builder_mod.AbstractBuilder;
const Project = project_mod.Project;
const Config = config_mod.Config;
const BuildType = config_mod.BuildType;
const SizeType = config_mod.SizeType;
const Version = config_mod.Version;
const Io = std.Io;

/// Python builder configuration
pub const PythonBuilder = struct {
    allocator: std.mem.Allocator,
    io: Io,
    shell: shell_mod.Shell,
    project: *Project,
    logger: *log.Logger,
    version: Version,
    config: Config,
    build_type: BuildType,
    size_type: SizeType,
    jobs: u8,
    optimize: bool,
    precompile: bool,

    pub const NAME = "Python";
    pub const DOWNLOAD_ARCHIVE_TEMPLATE = "Python-{ver}.tar.xz";
    pub const DOWNLOAD_URL_TEMPLATE = "https://www.python.org/ftp/python/{ver}/{archive}";

    /// Default configure options
    pub const CONFIG_OPTIONS = [_][]const u8{
        "--disable-test-modules",
    };

    /// Patterns to remove when cleaning
    pub const REMOVE_PATTERNS = config_mod.REMOVE_PATTERNS;

    pub fn init(
        allocator: std.mem.Allocator,
        io: Io,
        project: *Project,
        version_str: []const u8,
        config_str: []const u8,
        jobs: u8,
        optimize: bool,
        precompile: bool,
    ) BuildError!PythonBuilder {
        const parsed = try Config.parseConfigString(config_str);
        const ver = try Version.parse(version_str);
        const cfg = try Config.init(allocator, io, version_str, parsed.build_type, parsed.size_type);

        return .{
            .allocator = allocator,
            .io = io,
            .shell = shell_mod.Shell.init(allocator, io),
            .project = project,
            .logger = log.getLogger(),
            .version = ver,
            .config = cfg,
            .build_type = parsed.build_type,
            .size_type = parsed.size_type,
            .jobs = jobs,
            .optimize = optimize,
            .precompile = precompile,
        };
    }

    pub fn deinit(self: *PythonBuilder) void {
        self.config.deinit();
    }

    /// Get name-version string like "Python-3.13.11"
    pub fn nameVersion(self: *const PythonBuilder) ![]u8 {
        return std.fmt.allocPrint(self.allocator, "{s}-{s}", .{ NAME, self.version.full });
    }

    /// Get name-ver string like "python3.13"
    pub fn nameVer(self: *const PythonBuilder) ![]u8 {
        var buf: [8]u8 = undefined;
        const v = self.version.ver(&buf);
        return std.fmt.allocPrint(self.allocator, "python{s}", .{v});
    }

    /// Get download archive filename
    fn downloadArchive(self: *const PythonBuilder) ![]u8 {
        return std.fmt.allocPrint(self.allocator, "Python-{s}.tar.xz", .{self.version.full});
    }

    /// Get download URL
    fn downloadUrl(self: *const PythonBuilder) ![]u8 {
        const archive = try self.downloadArchive();
        defer self.allocator.free(archive);
        return std.fmt.allocPrint(self.allocator, "https://www.python.org/ftp/python/{s}/{s}", .{ self.version.full, archive });
    }

    /// Get path to downloaded archive
    fn downloadedArchive(self: *const PythonBuilder) ![]u8 {
        const archive = try self.downloadArchive();
        defer self.allocator.free(archive);
        return std.fs.path.join(self.allocator, &.{ self.project.downloads, archive });
    }

    /// Get source directory
    fn srcDir(self: *const PythonBuilder) ![]u8 {
        const name_ver = try self.nameVersion();
        defer self.allocator.free(name_ver);
        return std.fs.path.join(self.allocator, &.{ self.project.src, name_ver });
    }

    /// Get prefix (install) directory
    fn prefix(self: *const PythonBuilder) ![]u8 {
        const plat = platform.PLATFORM;
        if (plat.isDarwin() and self.build_type == .framework) {
            var buf: [8]u8 = undefined;
            const v = self.version.ver(&buf);
            return std.fmt.allocPrint(self.allocator, "{s}/Python.framework/Versions/{s}", .{ self.project.install, v });
        }
        const type_str = self.build_type.toString();
        return std.fmt.allocPrint(self.allocator, "{s}/python-{s}", .{ self.project.install, type_str });
    }

    /// Get path to Python executable
    fn executable(self: *const PythonBuilder) ![]u8 {
        const pfx = try self.prefix();
        defer self.allocator.free(pfx);
        return std.fs.path.join(self.allocator, &.{ pfx, "bin", "python3" });
    }

    /// Check if archive is downloaded
    fn archiveIsDownloaded(self: *PythonBuilder) !bool {
        const archive_path = try self.downloadedArchive();
        defer self.allocator.free(archive_path);
        return self.shell.exists(archive_path);
    }

    /// Build dependencies
    fn buildDependencies(self: *PythonBuilder) BuildError!void {
        self.logger.info("Building dependencies...", .{});

        // OpenSSL
        var openssl = openssl_mod.OpensslBuilder.init(self.allocator, self.io, self.project, null);
        try openssl.process();

        // Bzip2
        var bzip2 = bzip2_mod.Bzip2Builder.init(self.allocator, self.io, self.project, null);
        try bzip2.process();

        // XZ
        var xz = xz_mod.XzBuilder.init(self.allocator, self.io, self.project, null);
        try xz.process();
    }

    /// Setup build environment
    fn setup(self: *PythonBuilder) BuildError!void {
        try self.project.setup();

        // Download if needed
        if (!try self.archiveIsDownloaded()) {
            const url = try self.downloadUrl();
            defer self.allocator.free(url);
            const archive = try self.shell.download(url, self.project.downloads);
            self.allocator.free(archive);
        }

        // Extract source
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

    /// Configure Python build
    fn configure(self: *PythonBuilder) BuildError!void {
        self.logger.info("Configuring Python {s} ({s} build)...", .{ self.version.full, self.build_type.toString() });

        const src_dir = try self.srcDir();
        defer self.allocator.free(src_dir);

        const pfx = try self.prefix();
        defer self.allocator.free(pfx);

        // Write Setup.local
        const setup_local = try std.fs.path.join(self.allocator, &.{ src_dir, "Modules", "Setup.local" });
        defer self.allocator.free(setup_local);

        // Note: Using a mutable copy for the write method
        var cfg_copy = self.config;
        try cfg_copy.write(setup_local);

        // Build configure command
        var opts: std.ArrayList(u8) = .empty;
        defer opts.deinit(self.allocator);
        const a = self.allocator;

        opts.appendSlice(a, "./configure --prefix=") catch return error.OutOfMemory;
        opts.appendSlice(a, pfx) catch return error.OutOfMemory;

        // Add base options
        for (CONFIG_OPTIONS) |opt| {
            opts.append(a, ' ') catch return error.OutOfMemory;
            opts.appendSlice(a, opt) catch return error.OutOfMemory;
        }

        // Build type specific options
        switch (self.build_type) {
            .shared => {
                opts.appendSlice(a, " --enable-shared --without-static-libpython") catch return error.OutOfMemory;
            },
            .framework => {
                opts.appendSlice(a, " --enable-framework=") catch return error.OutOfMemory;
                opts.appendSlice(a, self.project.install) catch return error.OutOfMemory;
            },
            .static => {},
        }

        if (self.optimize) {
            opts.appendSlice(a, " --enable-optimizations") catch return error.OutOfMemory;
        }

        // No pip if not installing packages
        opts.appendSlice(a, " --without-ensurepip") catch return error.OutOfMemory;

        self.shell.setCwd(src_dir);
        defer self.shell.resetCwd();

        try self.shell.shellCmd(opts.items);

        // Post-configure fixup: Python's configure auto-detects system/Homebrew
        // OpenSSL and sets MODULE__SSL and MODULE__HASHLIB to build as shared
        // extensions with dynamic -lssl/-lcrypto. This conflicts with our
        // Setup.local static definitions (which use our custom-built OpenSSL
        // static archives). The shared .so modules would load at runtime and
        // fail to find symbols from the system's LibreSSL.
        // Fix: disable configure's SSL/hashlib module detection so only
        // Setup.local's static definitions are used.
        const makefile_path = try std.fs.path.join(a, &.{ src_dir, "Makefile" });
        defer a.free(makefile_path);
        const sed_cmd = try std.fmt.allocPrint(a,
            "sed -i '' " ++
            "-e 's/^MODULE__SSL_STATE=yes/MODULE__SSL_STATE=disabled/' " ++
            "-e 's/^MODULE__HASHLIB_STATE=yes/MODULE__HASHLIB_STATE=disabled/' " ++
            "{s}",
            .{makefile_path},
        );
        defer a.free(sed_cmd);
        try self.shell.shellCmd(sed_cmd);
    }

    /// Build Python
    fn build(self: *PythonBuilder) BuildError!void {
        self.logger.info("Building Python {s} (using {d} jobs)...", .{ self.version.full, self.jobs });

        const src_dir = try self.srcDir();
        defer self.allocator.free(src_dir);

        const make_cmd = try std.fmt.allocPrint(self.allocator, "make -j{d}", .{self.jobs});
        defer self.allocator.free(make_cmd);

        self.shell.setCwd(src_dir);
        defer self.shell.resetCwd();

        try self.shell.shellCmd(make_cmd);
        self.logger.info("Python {s} build complete", .{self.version.full});
    }

    /// Install Python
    fn install(self: *PythonBuilder) BuildError!void {
        const pfx = try self.prefix();
        defer self.allocator.free(pfx);

        if (self.shell.exists(pfx)) {
            self.shell.remove(pfx);
        }

        const src_dir = try self.srcDir();
        defer self.allocator.free(src_dir);

        self.shell.setCwd(src_dir);
        defer self.shell.resetCwd();

        try self.shell.shellCmd("make install");
    }

    /// Clean installed build
    fn clean(self: *PythonBuilder) BuildError!void {
        const pfx = try self.prefix();
        defer self.allocator.free(pfx);

        const name_ver = try self.nameVer();
        defer self.allocator.free(name_ver);

        // Remove unwanted files/directories
        const lib_dir = try std.fs.path.join(self.allocator, &.{ pfx, "lib", name_ver });
        defer self.allocator.free(lib_dir);

        // Clean patterns
        for (REMOVE_PATTERNS) |pattern| {
            const rm_path = try std.fs.path.join(self.allocator, &.{ lib_dir, pattern });
            defer self.allocator.free(rm_path);
            self.shell.remove(rm_path);
        }

        // Remove unwanted binaries
        const bins = [_][]const u8{ "2to3", "idle3", "pydoc3" };
        for (bins) |bin| {
            const bin_path = try std.fs.path.join(self.allocator, &.{ pfx, "bin", bin });
            defer self.allocator.free(bin_path);
            self.shell.remove(bin_path);
        }
    }

    /// Zip the standard library
    fn ziplib(self: *PythonBuilder) BuildError!void {
        const pfx = try self.prefix();
        defer self.allocator.free(pfx);

        const name_ver = try self.nameVer();
        defer self.allocator.free(name_ver);

        const lib_dir = try std.fs.path.join(self.allocator, &.{ pfx, "lib", name_ver });
        defer self.allocator.free(lib_dir);

        // Move lib-dynload out before zipping
        const lib_dynload = try std.fs.path.join(self.allocator, &.{ lib_dir, "lib-dynload" });
        defer self.allocator.free(lib_dynload);

        const build_dynload = try std.fs.path.join(self.allocator, &.{ self.project.build, "lib-dynload" });
        defer self.allocator.free(build_dynload);

        if (self.shell.exists(lib_dynload)) {
            try self.shell.move(lib_dynload, build_dynload);
        }

        // Precompile if requested
        if (self.precompile) {
            const exe = try self.executable();
            defer self.allocator.free(exe);

            const compile_cmd = try std.fmt.allocPrint(
                self.allocator,
                "{s} -m compileall -f -b -o 2 {s}",
                .{ exe, lib_dir },
            );
            defer self.allocator.free(compile_cmd);

            self.shell.shellCmd(compile_cmd) catch {};
        }

        // Move os.py out before zipping
        const os_py = try std.fs.path.join(self.allocator, &.{ lib_dir, "os.py" });
        defer self.allocator.free(os_py);

        const build_os = try std.fs.path.join(self.allocator, &.{ self.project.build, "os.py" });
        defer self.allocator.free(build_os);

        if (self.shell.exists(os_py)) {
            try self.shell.move(os_py, build_os);
        }

        // Create zip archive
        var ver_buf: [4]u8 = undefined;
        const ver_nodot = self.version.verNoDot(&ver_buf);

        const zip_path = try std.fmt.allocPrint(
            self.allocator,
            "{s}/lib/python{s}.zip",
            .{ pfx, ver_nodot },
        );
        defer self.allocator.free(zip_path);

        self.logger.info("Creating stdlib zip at {s}", .{zip_path});
        shell_mod.createZip(self.allocator, self.io, lib_dir, zip_path) catch {
            self.logger.warning("Failed to create zip, continuing anyway", .{});
        };

        // Remove original lib dir and recreate minimal structure
        self.shell.remove(lib_dir);
        try self.shell.makedirs(lib_dir);

        const site_packages = try std.fs.path.join(self.allocator, &.{ lib_dir, "site-packages" });
        defer self.allocator.free(site_packages);
        try self.shell.makedirs(site_packages);

        // Restore lib-dynload
        if (self.shell.exists(build_dynload)) {
            try self.shell.move(build_dynload, lib_dynload);
        }

        // Restore os.py
        if (self.shell.exists(build_os)) {
            try self.shell.move(build_os, os_py);
        }
    }

    /// Make build relocatable (macOS specific)
    fn makeRelocatable(self: *PythonBuilder) BuildError!void {
        const plat = platform.PLATFORM;
        if (!plat.isDarwin()) return;

        if (self.build_type == .shared) {
            const pfx = try self.prefix();
            defer self.allocator.free(pfx);

            const name_ver = try self.nameVer();
            defer self.allocator.free(name_ver);

            const dylib_name = try std.fmt.allocPrint(self.allocator, "lib{s}.dylib", .{name_ver});
            defer self.allocator.free(dylib_name);

            const dylib = try std.fs.path.join(self.allocator, &.{ pfx, "lib", dylib_name });
            defer self.allocator.free(dylib);

            // Change install name
            const id_cmd = try std.fmt.allocPrint(
                self.allocator,
                "install_name_tool -id @loader_path/../Resources/lib/{s} {s}",
                .{ dylib_name, dylib },
            );
            defer self.allocator.free(id_cmd);

            self.shell.shellCmd(id_cmd) catch {};

            // Update executable reference
            const exe = try std.fs.path.join(self.allocator, &.{ pfx, "bin", name_ver });
            defer self.allocator.free(exe);

            const change_cmd = try std.fmt.allocPrint(
                self.allocator,
                "install_name_tool -change {s} @executable_path/../lib/{s} {s}",
                .{ dylib, dylib_name, exe },
            );
            defer self.allocator.free(change_cmd);

            self.shell.shellCmd(change_cmd) catch {};
        }
    }

    /// Validate the built Python
    fn validateBuild(self: *PythonBuilder) bool {
        const exe = self.executable() catch return false;
        defer self.allocator.free(exe);

        if (!self.shell.exists(exe)) {
            self.logger.err("Python executable not found: {s}", .{exe});
            return false;
        }

        // Test version output
        const argv = [_][]const u8{ exe, "--version" };
        var result = self.shell.cmdOutput(&argv) catch return false;
        defer result.deinit(self.allocator);

        if (result.success()) {
            self.logger.info("Python version: {s}", .{std.mem.trim(u8, result.stdout, "\n\r ")});
            return true;
        }

        return false;
    }

    /// Check if build can run (dependencies built)
    fn canRun(self: *PythonBuilder) !bool {
        // Check if our build is already cached
        const exe = try self.executable();
        defer self.allocator.free(exe);

        if (self.shell.exists(exe)) {
            self.logger.info("Found existing Python build", .{});
            return false;
        }

        return true;
    }

    /// Main build process
    pub fn process(self: *PythonBuilder) BuildError!void {
        if (!try self.canRun()) {
            self.logger.info("Python already built, skipping", .{});
            return;
        }

        // Build dependencies first
        try self.buildDependencies();

        // Build Python
        try self.setup();
        try self.configure();
        try self.build();
        try self.install();
        try self.clean();
        try self.ziplib();

        // Post-process
        if (self.build_type == .shared or self.build_type == .framework) {
            try self.makeRelocatable();
        }

        // Validate
        if (!self.validateBuild()) {
            self.logger.warning("Build validation failed, but continuing", .{});
        }

        self.logger.info("{s}_{s} DONE", .{ self.build_type.toString(), self.size_type.toString() });
    }
};

test "python builder init" {
    // Compile-time test
    _ = PythonBuilder;
}
