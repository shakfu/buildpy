//! Python build configuration
//!
//! Defines build configurations for different Python versions and build types.

const std = @import("std");
const platform = @import("platform.zig");
const errors = @import("errors.zig");
const shell = @import("shell.zig");

const BuildError = errors.BuildError;
const Io = std.Io;

/// Default Python versions
pub const DEFAULT_PY_VERSION = "3.13.11";
pub const DEFAULT_PY_VERSIONS = .{
    .{ "3.14", "3.14.2" },
    .{ "3.13", "3.13.11" },
    .{ "3.12", "3.12.12" },
    .{ "3.11", "3.11.14" },
};

/// Python version parsed into components
pub const Version = struct {
    major: u8,
    minor: u8,
    patch: u8,
    full: []const u8,

    /// Parse version string like "3.13.11"
    pub fn parse(version_str: []const u8) BuildError!Version {
        var parts = std.mem.splitSequence(u8, version_str, ".");
        const major_str = parts.next() orelse return error.InvalidVersion;
        const minor_str = parts.next() orelse return error.InvalidVersion;
        const patch_str = parts.next() orelse return error.InvalidVersion;

        return .{
            .major = std.fmt.parseInt(u8, major_str, 10) catch return error.InvalidVersion,
            .minor = std.fmt.parseInt(u8, minor_str, 10) catch return error.InvalidVersion,
            .patch = std.fmt.parseInt(u8, patch_str, 10) catch return error.InvalidVersion,
            .full = version_str,
        };
    }

    /// Get short version "3.13"
    pub fn ver(self: Version, buf: *[8]u8) []const u8 {
        return std.fmt.bufPrint(buf, "{d}.{d}", .{ self.major, self.minor }) catch "?.?";
    }

    /// Get version without dots "313"
    pub fn verNoDot(self: Version, buf: *[4]u8) []const u8 {
        return std.fmt.bufPrint(buf, "{d}{d}", .{ self.major, self.minor }) catch "??";
    }
};

/// Build type enumeration
pub const BuildType = enum {
    static,
    shared,
    framework,

    pub fn fromString(s: []const u8) ?BuildType {
        if (std.mem.eql(u8, s, "static")) return .static;
        if (std.mem.eql(u8, s, "shared")) return .shared;
        if (std.mem.eql(u8, s, "framework")) return .framework;
        return null;
    }

    pub fn toString(self: BuildType) []const u8 {
        return switch (self) {
            .static => "static",
            .shared => "shared",
            .framework => "framework",
        };
    }
};

/// Size type enumeration
pub const SizeType = enum {
    max,
    mid,
    tiny,
    bootstrap,

    pub fn fromString(s: []const u8) ?SizeType {
        if (std.mem.eql(u8, s, "max")) return .max;
        if (std.mem.eql(u8, s, "mid")) return .mid;
        if (std.mem.eql(u8, s, "tiny")) return .tiny;
        if (std.mem.eql(u8, s, "bootstrap")) return .bootstrap;
        return null;
    }

    pub fn toString(self: SizeType) []const u8 {
        return switch (self) {
            .max => "max",
            .mid => "mid",
            .tiny => "tiny",
            .bootstrap => "bootstrap",
        };
    }
};

/// Header lines for Setup.local
pub const HEADERS = [_][]const u8{
    "DESTLIB=$(LIBDEST)",
    "MACHDESTLIB=$(BINLIBDEST)",
    "DESTPATH=",
    "SITEPATH=",
    "TESTPATH=",
    "COREPYTHONPATH=$(DESTPATH)$(SITEPATH)$(TESTPATH)",
    "PYTHONPATH=$(COREPYTHONPATH)",
    "OPENSSL=$(srcdir)/../../install/openssl",
    "BZIP2=$(srcdir)/../../install/bzip2",
    "LZMA=$(srcdir)/../../install/xz",
};

/// Core modules that are always built-in
pub const CORE_MODULES = [_][]const u8{
    "_abc",
    "_codecs",
    "_collections",
    "_functools",
    "_io",
    "_locale",
    "_operator",
    "_signal",
    "_sre",
    "_stat",
    "_symtable",
    "_thread",
    "_tracemalloc",
    "_weakref",
    "atexit",
    "errno",
    "faulthandler",
    "itertools",
    "posix",
    "pwd",
    "time",
};

/// Default static modules
pub const STATIC_MODULES = [_][]const u8{
    "_asyncio",
    "_bisect",
    "_blake2",
    "_bz2",
    "_contextvars",
    "_csv",
    "_datetime",
    "_decimal",
    "_elementtree",
    "_hashlib",
    "_heapq",
    "_json",
    "_lsprof",
    "_lzma",
    "_md5",
    "_multibytecodec",
    "_multiprocessing",
    "_opcode",
    "_pickle",
    "_posixshmem",
    "_posixsubprocess",
    "_queue",
    "_random",
    "_sha1",
    "_sha256",
    "_sha3",
    "_sha512",
    "_socket",
    "_sqlite3",
    "_ssl",
    "_statistics",
    "_struct",
    "_typing",
    "_uuid",
    "_zoneinfo",
    "array",
    "binascii",
    "cmath",
    "fcntl",
    "grp",
    "math",
    "mmap",
    "pyexpat",
    "readline",
    "select",
    "unicodedata",
    "zlib",
};

/// Default disabled modules
pub const DISABLED_MODULES = [_][]const u8{
    "_codecs_cn",
    "_codecs_hk",
    "_codecs_iso2022",
    "_codecs_jp",
    "_codecs_kr",
    "_codecs_tw",
    "_crypt",
    "_ctypes",
    "_curses",
    "_curses_panel",
    "_dbm",
    "_scproxy",
    "_tkinter",
    "_xxsubinterpreters",
    "audioop",
    "nis",
    "ossaudiodev",
    "resource",
    "spwd",
    "syslog",
    "termios",
    "xxlimited",
    "xxlimited_35",
};

/// Remove patterns for cleaning installed build
pub const REMOVE_PATTERNS = [_][]const u8{
    "*.exe",
    "*config-3*",
    "*tcl*",
    "*tdbc*",
    "*tk*",
    "__phello__",
    "__pycache__",
    "_codecs_*.so",
    "_test*",
    "_tk*",
    "_xx*.so",
    "distutils",
    "idlelib",
    "lib2to3",
    "libpython*",
    "LICENSE.txt",
    "pkgconfig",
    "pydoc_data",
    "site-packages",
    "test",
    "Tk*",
    "turtle*",
    "venv",
    "xx*.so",
};

/// Extension module definition (module name -> source files and flags)
pub const ExtensionDef = struct {
    name: []const u8,
    sources: []const []const u8,
};

/// HACL include flags used by hash modules in 3.12+
const HACL_INCLUDES = [_][]const u8{
    "-I$(srcdir)/Modules/_hacl/include",
    "-D_BSD_SOURCE",
    "-D_DEFAULT_SOURCE",
};

/// Get extension definitions for a module (version-aware)
pub fn getExtension(name: []const u8, minor: u8) ?[]const []const u8 {
    // Version-specific overrides for 3.12+ (HACL hash refactoring)
    if (minor >= 12) {
        // _sha2 replaces _sha256/_sha512 in 3.12+
        if (std.mem.eql(u8, name, "_sha2")) {
            return &[_][]const u8{ "sha2module.c", "-I$(srcdir)/Modules/_hacl/include", "_hacl/Hacl_Hash_SHA2.c", "-D_BSD_SOURCE", "-D_DEFAULT_SOURCE" };
        }
        // HACL variants for hash modules
        if (std.mem.eql(u8, name, "_md5")) {
            return &[_][]const u8{ "md5module.c", "-I$(srcdir)/Modules/_hacl/include", "_hacl/Hacl_Hash_MD5.c", "-D_BSD_SOURCE", "-D_DEFAULT_SOURCE" };
        }
        if (std.mem.eql(u8, name, "_sha1")) {
            return &[_][]const u8{ "sha1module.c", "-I$(srcdir)/Modules/_hacl/include", "_hacl/Hacl_Hash_SHA1.c", "-D_BSD_SOURCE", "-D_DEFAULT_SOURCE" };
        }
        if (std.mem.eql(u8, name, "_sha3")) {
            return &[_][]const u8{ "sha3module.c", "-I$(srcdir)/Modules/_hacl/include", "_hacl/Hacl_Hash_SHA3.c", "-D_BSD_SOURCE", "-D_DEFAULT_SOURCE" };
        }
    }

    // 3.13+ new modules
    if (minor >= 13) {
        if (std.mem.eql(u8, name, "_interpchannels")) {
            return &[_][]const u8{"_interpchannelsmodule.c"};
        }
        if (std.mem.eql(u8, name, "_interpqueues")) {
            return &[_][]const u8{"_interpqueuesmodule.c"};
        }
        if (std.mem.eql(u8, name, "_interpreters")) {
            return &[_][]const u8{"_interpretersmodule.c"};
        }
        if (std.mem.eql(u8, name, "_sysconfig")) {
            return &[_][]const u8{"_sysconfig.c"};
        }
    }

    // Base extensions (3.11 compatible)
    const extensions = .{
        .{ "_abc", &[_][]const u8{"_abc.c"} },
        .{ "_asyncio", &[_][]const u8{"_asynciomodule.c"} },
        .{ "_bisect", &[_][]const u8{"_bisectmodule.c"} },
        .{ "_blake2", &[_][]const u8{ "_blake2/blake2module.c", "_blake2/blake2b_impl.c", "_blake2/blake2s_impl.c" } },
        .{ "_bz2", &[_][]const u8{ "_bz2module.c", "-I$(BZIP2)/include", "-L$(BZIP2)/lib", "$(BZIP2)/lib/libbz2.a" } },
        .{ "_codecs", &[_][]const u8{"_codecsmodule.c"} },
        .{ "_collections", &[_][]const u8{"_collectionsmodule.c"} },
        .{ "_contextvars", &[_][]const u8{"_contextvarsmodule.c"} },
        .{ "_csv", &[_][]const u8{"_csv.c"} },
        .{ "_datetime", &[_][]const u8{"_datetimemodule.c"} },
        .{ "_decimal", &[_][]const u8{ "_decimal/_decimal.c", "-DCONFIG_64=1" } },
        .{ "_elementtree", &[_][]const u8{"_elementtree.c"} },
        .{ "_functools", &[_][]const u8{ "-DPy_BUILD_CORE_BUILTIN", "-I$(srcdir)/Include/internal", "_functoolsmodule.c" } },
        .{ "_hashlib", &[_][]const u8{ "_hashopenssl.c", "-I$(OPENSSL)/include", "-L$(OPENSSL)/lib", "$(OPENSSL)/lib/libcrypto.a" } },
        .{ "_heapq", &[_][]const u8{"_heapqmodule.c"} },
        .{ "_io", &[_][]const u8{ "_io/_iomodule.c", "_io/iobase.c", "_io/fileio.c", "_io/bytesio.c", "_io/bufferedio.c", "_io/textio.c", "_io/stringio.c" } },
        .{ "_json", &[_][]const u8{"_json.c"} },
        .{ "_locale", &[_][]const u8{ "-DPy_BUILD_CORE_BUILTIN", "_localemodule.c" } },
        .{ "_lsprof", &[_][]const u8{ "_lsprof.c", "rotatingtree.c" } },
        .{ "_lzma", &[_][]const u8{ "_lzmamodule.c", "-I$(LZMA)/include", "-L$(LZMA)/lib", "$(LZMA)/lib/liblzma.a" } },
        .{ "_md5", &[_][]const u8{"md5module.c"} },
        .{ "_multibytecodec", &[_][]const u8{"cjkcodecs/multibytecodec.c"} },
        .{ "_multiprocessing", &[_][]const u8{ "_multiprocessing/multiprocessing.c", "_multiprocessing/semaphore.c" } },
        .{ "_opcode", &[_][]const u8{"_opcode.c"} },
        .{ "_operator", &[_][]const u8{"_operator.c"} },
        .{ "_pickle", &[_][]const u8{"_pickle.c"} },
        .{ "_posixshmem", &[_][]const u8{"_multiprocessing/posixshmem.c"} },
        .{ "_posixsubprocess", &[_][]const u8{"_posixsubprocess.c"} },
        .{ "_queue", &[_][]const u8{"_queuemodule.c"} },
        .{ "_random", &[_][]const u8{"_randommodule.c"} },
        .{ "_scproxy", &[_][]const u8{"_scproxy.c"} },
        .{ "_sha1", &[_][]const u8{"sha1module.c"} },
        .{ "_sha256", &[_][]const u8{"sha256module.c"} },
        .{ "_sha3", &[_][]const u8{"_sha3/sha3module.c"} },
        .{ "_sha512", &[_][]const u8{"sha512module.c"} },
        .{ "_signal", &[_][]const u8{ "-DPy_BUILD_CORE_BUILTIN", "-I$(srcdir)/Include/internal", "signalmodule.c" } },
        .{ "_socket", &[_][]const u8{"socketmodule.c"} },
        .{ "_sqlite3", &[_][]const u8{ "_sqlite/blob.c", "_sqlite/connection.c", "_sqlite/cursor.c", "_sqlite/microprotocols.c", "_sqlite/module.c", "_sqlite/prepare_protocol.c", "_sqlite/row.c", "_sqlite/statement.c", "_sqlite/util.c" } },
        .{ "_sre", &[_][]const u8{ "_sre/sre.c", "-DPy_BUILD_CORE_BUILTIN" } },
        .{ "_ssl", &[_][]const u8{ "_ssl.c", "-I$(OPENSSL)/include", "-L$(OPENSSL)/lib", "$(OPENSSL)/lib/libcrypto.a", "$(OPENSSL)/lib/libssl.a" } },
        .{ "_stat", &[_][]const u8{"_stat.c"} },
        .{ "_statistics", &[_][]const u8{"_statisticsmodule.c"} },
        .{ "_struct", &[_][]const u8{"_struct.c"} },
        .{ "_symtable", &[_][]const u8{"symtablemodule.c"} },
        .{ "_testexternalinspection", &[_][]const u8{"_testexternalinspection.c"} },
        .{ "_thread", &[_][]const u8{ "-DPy_BUILD_CORE_BUILTIN", "-I$(srcdir)/Include/internal", "_threadmodule.c" } },
        .{ "_tracemalloc", &[_][]const u8{"_tracemalloc.c"} },
        .{ "_typing", &[_][]const u8{"_typingmodule.c"} },
        .{ "_uuid", &[_][]const u8{"_uuidmodule.c"} },
        .{ "_weakref", &[_][]const u8{"_weakref.c"} },
        .{ "_zoneinfo", &[_][]const u8{"_zoneinfo.c"} },
        .{ "array", &[_][]const u8{"arraymodule.c"} },
        .{ "atexit", &[_][]const u8{"atexitmodule.c"} },
        .{ "binascii", &[_][]const u8{"binascii.c"} },
        .{ "cmath", &[_][]const u8{"cmathmodule.c"} },
        .{ "errno", &[_][]const u8{"errnomodule.c"} },
        .{ "faulthandler", &[_][]const u8{"faulthandler.c"} },
        .{ "fcntl", &[_][]const u8{"fcntlmodule.c"} },
        .{ "grp", &[_][]const u8{"grpmodule.c"} },
        .{ "itertools", &[_][]const u8{"itertoolsmodule.c"} },
        .{ "math", &[_][]const u8{"mathmodule.c"} },
        .{ "mmap", &[_][]const u8{"mmapmodule.c"} },
        .{ "posix", &[_][]const u8{ "-DPy_BUILD_CORE_BUILTIN", "-I$(srcdir)/Include/internal", "posixmodule.c" } },
        .{ "pwd", &[_][]const u8{"pwdmodule.c"} },
        .{ "pyexpat", &[_][]const u8{ "expat/xmlparse.c", "expat/xmlrole.c", "expat/xmltok.c", "pyexpat.c", "-I$(srcdir)/Modules/expat", "-DHAVE_EXPAT_CONFIG_H", "-DUSE_PYEXPAT_CAPI", "-DXML_DEV_URANDOM" } },
        .{ "readline", &[_][]const u8{ "readline.c", "-lreadline", "-ltermcap" } },
        .{ "select", &[_][]const u8{"selectmodule.c"} },
        .{ "time", &[_][]const u8{ "-DPy_BUILD_CORE_BUILTIN", "-I$(srcdir)/Include/internal", "timemodule.c" } },
        .{ "unicodedata", &[_][]const u8{"unicodedata.c"} },
        .{ "zlib", &[_][]const u8{ "zlibmodule.c", "-lz" } },
    };

    inline for (extensions) |ext| {
        if (std.mem.eql(u8, ext[0], name)) {
            return ext[1];
        }
    }
    return null;
}

const StringList = std.ArrayList([]const u8);

/// Python configuration
pub const Config = struct {
    allocator: std.mem.Allocator,
    io: Io,
    version: Version,
    build_type: BuildType,
    size_type: SizeType,

    // Module lists (dynamically managed)
    core: StringList,
    static: StringList,
    shared: StringList,
    disabled: StringList,

    pub fn init(
        allocator: std.mem.Allocator,
        io: Io,
        version_str: []const u8,
        build_type: BuildType,
        size_type: SizeType,
    ) BuildError!Config {
        const version = try Version.parse(version_str);

        var cfg = Config{
            .allocator = allocator,
            .io = io,
            .version = version,
            .build_type = build_type,
            .size_type = size_type,
            .core = .empty,
            .static = .empty,
            .shared = .empty,
            .disabled = .empty,
        };

        // Initialize with defaults
        for (CORE_MODULES) |mod| {
            cfg.core.append(allocator, mod) catch return error.OutOfMemory;
        }
        for (STATIC_MODULES) |mod| {
            cfg.static.append(allocator, mod) catch return error.OutOfMemory;
        }
        for (DISABLED_MODULES) |mod| {
            cfg.disabled.append(allocator, mod) catch return error.OutOfMemory;
        }

        // Apply version-specific patches
        try cfg.patch();

        // Apply size-specific configuration
        try cfg.applySizeConfig();

        return cfg;
    }

    pub fn deinit(self: *Config) void {
        self.core.deinit(self.allocator);
        self.static.deinit(self.allocator);
        self.shared.deinit(self.allocator);
        self.disabled.deinit(self.allocator);
    }

    /// Apply version-specific patches
    fn patch(self: *Config) BuildError!void {
        const plat = platform.PLATFORM;

        // Platform-specific patches for 3.11
        if (plat.isDarwin()) {
            try self.enableStatic("_scproxy");
        } else if (plat.isLinux()) {
            try self.enableStatic("ossaudiodev");
        }

        // 3.12+ specific patches
        if (self.version.minor >= 12) {
            // Update SHA modules for 3.12
            self.removeFromList(&self.static, "_sha256");
            self.removeFromList(&self.static, "_sha512");
            self.static.append(self.allocator, "_sha2") catch return error.OutOfMemory;
            self.disabled.append(self.allocator, "_xxinterpchannels") catch return error.OutOfMemory;
        }

        // 3.13+ specific patches
        if (self.version.minor >= 13) {
            // Add interpreter modules
            self.static.append(self.allocator, "_interpchannels") catch return error.OutOfMemory;
            self.static.append(self.allocator, "_interpqueues") catch return error.OutOfMemory;
            self.static.append(self.allocator, "_interpreters") catch return error.OutOfMemory;
            self.static.append(self.allocator, "_sysconfig") catch return error.OutOfMemory;

            // Remove deprecated modules
            self.removeFromList(&self.disabled, "_crypt");
            self.removeFromList(&self.disabled, "_xxsubinterpreters");
            self.removeFromList(&self.disabled, "audioop");
            self.removeFromList(&self.disabled, "nis");
            self.removeFromList(&self.disabled, "ossaudiodev");
            self.removeFromList(&self.disabled, "spwd");

            self.disabled.append(self.allocator, "_testexternalinspection") catch return error.OutOfMemory;
        }

        // 3.14+ specific patches
        if (self.version.minor >= 14) {
            self.static.append(self.allocator, "_types") catch return error.OutOfMemory;
            self.disabled.append(self.allocator, "_remote_debugging") catch return error.OutOfMemory;
            self.disabled.append(self.allocator, "_zstd") catch return error.OutOfMemory;

            // Remove _contextvars (now built-in)
            self.removeFromList(&self.static, "_contextvars");
            self.removeFromList(&self.disabled, "_testexternalinspection");
        }
    }

    /// Apply size-type specific configuration
    fn applySizeConfig(self: *Config) BuildError!void {
        switch (self.size_type) {
            .max => {
                // static_max: no changes needed
                if (self.build_type == .shared or self.build_type == .framework) {
                    self.removeFromList(&self.disabled, "_ctypes");
                    try self.moveStaticToShared("_decimal");
                    try self.moveStaticToShared("_ssl");
                    try self.moveStaticToShared("_hashlib");
                }
            },
            .mid => {
                try self.disableStatic("_decimal");
            },
            .tiny => {
                try self.disableStatic("_bz2");
                try self.disableStatic("_decimal");
                try self.disableStatic("_csv");
                try self.disableStatic("_json");
                try self.disableStatic("_lzma");
                try self.disableStatic("_sqlite3");
                try self.disableStatic("_ssl");
                try self.disableStatic("pyexpat");
            },
            .bootstrap => {
                // Move all static to disabled, move core to static
                for (self.static.items) |mod| {
                    self.disabled.append(self.allocator, mod) catch return error.OutOfMemory;
                }
                self.static.clearRetainingCapacity();
                for (self.core.items) |mod| {
                    self.static.append(self.allocator, mod) catch return error.OutOfMemory;
                }
                self.core.clearRetainingCapacity();
            },
        }
    }

    fn removeFromList(self: *Config, list: *StringList, item: []const u8) void {
        for (list.items, 0..) |mod, i| {
            if (std.mem.eql(u8, mod, item)) {
                _ = list.orderedRemove(i);
                _ = self;
                return;
            }
        }
    }

    fn enableStatic(self: *Config, name: []const u8) BuildError!void {
        self.removeFromList(&self.disabled, name);
        self.static.append(self.allocator, name) catch return error.OutOfMemory;
    }

    fn disableStatic(self: *Config, name: []const u8) BuildError!void {
        self.removeFromList(&self.static, name);
        self.disabled.append(self.allocator, name) catch return error.OutOfMemory;
    }

    fn moveStaticToShared(self: *Config, name: []const u8) BuildError!void {
        self.removeFromList(&self.static, name);
        self.shared.append(self.allocator, name) catch return error.OutOfMemory;
    }

    /// Write Setup.local configuration file
    pub fn write(self: *Config, path: []const u8) BuildError!void {
        var output: std.ArrayList(u8) = .empty;
        defer output.deinit(self.allocator);
        const a = self.allocator;

        // Write header
        output.appendSlice(a, "# -*- makefile -*-\n") catch return error.IoError;
        for (HEADERS) |line| {
            output.appendSlice(a, line) catch return error.IoError;
            output.append(a, '\n') catch return error.IoError;
        }

        // Write core modules
        output.appendSlice(a, "\n# core\n") catch return error.IoError;
        for (self.core.items) |mod| {
            if (getExtension(mod, self.version.minor)) |sources| {
                output.appendSlice(a, mod) catch return error.IoError;
                for (sources) |src| {
                    output.append(a, ' ') catch return error.IoError;
                    output.appendSlice(a, src) catch return error.IoError;
                }
                output.append(a, '\n') catch return error.IoError;
            }
        }

        // Write shared section
        if (self.shared.items.len > 0) {
            output.appendSlice(a, "\n*shared*\n") catch return error.IoError;
            std.mem.sort([]const u8, self.shared.items, {}, struct {
                fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
                    return std.mem.lessThan(u8, lhs, rhs);
                }
            }.lessThan);
            for (self.shared.items) |mod| {
                if (getExtension(mod, self.version.minor)) |sources| {
                    output.appendSlice(a, mod) catch return error.IoError;
                    for (sources) |src| {
                        output.append(a, ' ') catch return error.IoError;
                        output.appendSlice(a, src) catch return error.IoError;
                    }
                    output.append(a, '\n') catch return error.IoError;
                }
            }
        }

        // Write static section
        if (self.static.items.len > 0) {
            output.appendSlice(a, "\n*static*\n") catch return error.IoError;
            std.mem.sort([]const u8, self.static.items, {}, struct {
                fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
                    return std.mem.lessThan(u8, lhs, rhs);
                }
            }.lessThan);
            for (self.static.items) |mod| {
                if (getExtension(mod, self.version.minor)) |sources| {
                    output.appendSlice(a, mod) catch return error.IoError;
                    for (sources) |src| {
                        output.append(a, ' ') catch return error.IoError;
                        output.appendSlice(a, src) catch return error.IoError;
                    }
                    output.append(a, '\n') catch return error.IoError;
                }
            }
        }

        // Write disabled section
        if (self.disabled.items.len > 0) {
            output.appendSlice(a, "\n*disabled*\n") catch return error.IoError;
            std.mem.sort([]const u8, self.disabled.items, {}, struct {
                fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
                    return std.mem.lessThan(u8, lhs, rhs);
                }
            }.lessThan);
            for (self.disabled.items) |mod| {
                output.appendSlice(a, mod) catch return error.IoError;
                output.append(a, '\n') catch return error.IoError;
            }
        }

        output.appendSlice(a, "# end \n") catch return error.IoError;

        // Write to file
        shell.writeFile(self.io, path, output.items) catch return error.IoError;
    }

    /// Parse config string like "static_max" into BuildType and SizeType
    pub fn parseConfigString(config_str: []const u8) BuildError!struct { build_type: BuildType, size_type: SizeType } {
        var parts = std.mem.splitSequence(u8, config_str, "_");
        const build_str = parts.next() orelse return error.ConfigError;
        const size_str = parts.next() orelse return error.ConfigError;

        const build_type = BuildType.fromString(build_str) orelse return error.InvalidBuildType;
        const size_type = SizeType.fromString(size_str) orelse return error.ConfigError;

        return .{ .build_type = build_type, .size_type = size_type };
    }
};

test "version parsing" {
    const v = try Version.parse("3.13.11");
    try std.testing.expectEqual(@as(u8, 3), v.major);
    try std.testing.expectEqual(@as(u8, 13), v.minor);
    try std.testing.expectEqual(@as(u8, 11), v.patch);
}

test "version formatting" {
    const v = try Version.parse("3.13.11");
    var buf1: [8]u8 = undefined;
    var buf2: [4]u8 = undefined;
    try std.testing.expectEqualStrings("3.13", v.ver(&buf1));
    try std.testing.expectEqualStrings("313", v.verNoDot(&buf2));
}

test "config string parsing" {
    const result = try Config.parseConfigString("static_max");
    try std.testing.expectEqual(BuildType.static, result.build_type);
    try std.testing.expectEqual(SizeType.max, result.size_type);
}

test "get extension" {
    const abc_ext = getExtension("_abc", 13);
    try std.testing.expect(abc_ext != null);
    try std.testing.expectEqualStrings("_abc.c", abc_ext.?[0]);
}

test "get extension version aware" {
    // 3.11: _sha3 uses old path
    const sha3_11 = getExtension("_sha3", 11);
    try std.testing.expect(sha3_11 != null);
    try std.testing.expectEqualStrings("_sha3/sha3module.c", sha3_11.?[0]);

    // 3.12+: _sha3 uses HACL path
    const sha3_12 = getExtension("_sha3", 12);
    try std.testing.expect(sha3_12 != null);
    try std.testing.expectEqualStrings("sha3module.c", sha3_12.?[0]);

    // 3.12+: _sha2 exists
    const sha2 = getExtension("_sha2", 12);
    try std.testing.expect(sha2 != null);
    try std.testing.expectEqualStrings("sha2module.c", sha2.?[0]);

    // 3.11: _sha2 does not exist
    try std.testing.expect(getExtension("_sha2", 11) == null);

    // 3.13+: new modules
    try std.testing.expect(getExtension("_sysconfig", 13) != null);
    try std.testing.expect(getExtension("_sysconfig", 12) == null);
}
