import SemVer

public class PythonBuilder: Builder {

    static var name: String = "Python"
    static var urlTemplate: String =
        "https://www.python.org/ftp/python/{version}/Python-{version}.tgz"
    static var staticLibs: [String] = []
    static var dependsOn: [Builder] = []

    var semver: Version
    var project: Project
    var shell: Shell

    public required init(version: String, project: Project) {
        self.semver = Version(version) ?? Version(major: 0, minor: 0, patch: 0)
        self.project = project
        self.shell = Shell()
    }

    var version: String {
        return self.semver.versionString()
    }

    var ver: String {
        return "\(self.semver.major).\(self.semver.minor)"
    }

    var verMajor: Int {
        return self.semver.major
    }

    var verMinor: Int {
        return self.semver.minor
    }

    var verPatch: Int {
        return self.semver.patch
    }

    var verNodot: String {
        return "\(self.semver.major).\(self.semver.minor)"
    }

    var nameVersion: String {
        return "\(Self.name)-\(self.version)"
    }

    var nameVer: String {
        return "\(Self.name.lowercased())\(self.ver)"
    }

    var url: String {
        return Self.urlTemplate.replacingOccurrences(of: "{version}", with: self.version)
    }

    var url2: String {
        return "https://www.python.org/ftp/python/\(self.version)/Python-\(self.version).tgz"
    }

    var sourceDir: String {
        return self.project.sourceDir.appending("/\(self.nameVersion)")
    }

    var buildDir: String {
        return self.project.buildDir.appending("/\(self.nameVer)")
    }

    var executableName: String {
        return "\(Self.name.lowercased())\(self.ver)"
    }

    var executable: String {
        return self.project.binDir.appending("/\(self.executableName)")
    }

    var libname: String {
        return "lib\(Self.name.lowercased())\(self.ver)"
    }

    var staticlibName: String {
        return "\(self.libname).a"
    }

    var dylibLinkname: String {
        return "\(self.libname).dylib"
    }

    var dylibName: String {
        return "\(self.libname).dylib"
    }

    var dylib: String {
        return self.project.libDir.appending("/\(self.dylibName)")
    }

    var dylibLink: String {
        return self.project.libDir.appending("/\(self.dylibLinkname)")
    }

    var staticLib: String {
        return self.project.libDir.appending("/\(self.staticlibName)")
    }

    var prefixDir: String {
        return self.project.installDir.appending("/\(Self.name.lowercased())")
    }

    // Dependencies for Python
    static var pythonDependsOn: [String: String] = [
        "openssl": "1.1.1",
        "bzip2": "1.0.8",
        "xz": "5.2.5"
    ]

    // Remove patterns for cleaning Python build
    var removePatterns: [String] {
        return [
            "__pycache__",
            "*.pyc",
            "test",
            "distutils",
            "lib2to3",
            "idlelib",
            "venv",
            "turtledemo",
            "pydoc_data",
            "*.exe",
            "*config-3*",
            "*tcl*",
            "*tk*",
            "_test*",
            "_ctypes_test*"
        ]
    }

    public func build() {
        shell.log.info("Building Python \(version)")

        // Build dependencies first
        buildDependencies()

        // Download Python source
        downloadPython()

        // Configure Python build
        configurePython()

        // Build Python
        buildPython()

        // Install Python
        installPython()

        // Clean Python build
        cleanPython()

        // Zip Python library
        zipPythonLib()

        shell.log.info("Python \(version) build complete")
    }

    private func buildDependencies() {
        shell.log.info("Building Python dependencies")

        let project = self.project

        // Build OpenSSL
        let opensslBuilder = OpenSSLBuilder(version: Self.pythonDependsOn["openssl"]!, project: project)
        opensslBuilder.build()

        // Build Bzip2
        let bzip2Builder = Bzip2Builder(version: Self.pythonDependsOn["bzip2"]!, project: project)
        bzip2Builder.build()

        // Build XZ
        let xzBuilder = XzBuilder(version: Self.pythonDependsOn["xz"]!, project: project)
        xzBuilder.build()
    }

    private func downloadPython() {
        shell.log.info("Downloading Python source")

        if !shell.exists(path: sourceDir) {
            let res = shell.cmd(exe: "/usr/bin/git", args: ["clone", "--depth=1", "--branch", "v\(version)", "https://github.com/python/cpython.git", sourceDir])
            if !res.err.isEmpty {
                shell.log.error("Git clone failed: \(res.err)")
                return
            }
        }
    }

    private func configurePython() {
        shell.log.info("Configuring Python build")

        shell.chdir(path: sourceDir)

        // Generate Setup.local file
        generateSetupLocal()

        // Configure Python
        let configureArgs = [
            "--prefix=\(prefixDir)",
            "--without-ensurepip",
            "--enable-optimizations"
        ]

        let result = shell.cmd(exe: "./configure", args: configureArgs)
        if !result.err.isEmpty {
            shell.log.error("Configure failed: \(result.err)")
        }
    }

    private func generateSetupLocal() {
        let setupLocalPath = shell.joinPath(path: sourceDir, part: "Modules/Setup.local")
        let opensslPrefix = project.installDir + "/openssl"
        let bzip2Prefix = project.installDir + "/bzip2"
        let xzPrefix = project.installDir + "/xz"

        let setupContent = """
        # -*- makefile -*-
        DESTLIB=$(LIBDEST)
        MACHDESTLIB=$(BINLIBDEST)
        DESTPATH=
        SITEPATH=
        TESTPATH=
        COREPYTHONPATH=$(DESTPATH)$(SITEPATH)$(TESTPATH)
        PYTHONPATH=$(COREPYTHONPATH)
        OPENSSL=\(opensslPrefix)
        BZIP2=\(bzip2Prefix)
        LZMA=\(xzPrefix)

        # Core modules
        *static*

        _ssl _ssl.c -I$(OPENSSL)/include -L$(OPENSSL)/lib $(OPENSSL)/lib/libssl.a $(OPENSSL)/lib/libcrypto.a
        _hashlib _hashopenssl.c -I$(OPENSSL)/include -L$(OPENSSL)/lib $(OPENSSL)/lib/libcrypto.a
        _bz2 _bz2module.c -I$(BZIP2)/include -L$(BZIP2)/lib $(BZIP2)/lib/libbz2.a
        _lzma _lzmamodule.c -I$(LZMA)/include -L$(LZMA)/lib $(LZMA)/lib/liblzma.a

        *disabled*
        _tkinter
        _ctypes
        _curses
        _curses_panel
        _dbm
        _gdbm
        nis
        ossaudiodev
        spwd
        syslog
        termios
        """

        shell.mkfile(path: setupLocalPath, text: setupContent)
        shell.log.info("Generated Setup.local file")
    }

    private func buildPython() {
        shell.log.info("Building Python")

        shell.chdir(path: sourceDir)
        let result = shell.cmd(exe: "/usr/bin/make", args: ["-j4"])
        if !result.err.isEmpty {
            shell.log.error("Build failed: \(result.err)")
        }
    }

    private func installPython() {
        shell.log.info("Installing Python")

        shell.chdir(path: sourceDir)
        let result = shell.cmd(exe: "/usr/bin/make", args: ["install"])
        if !result.err.isEmpty {
            shell.log.error("Install failed: \(result.err)")
        }
    }

    private func cleanPython() {
        shell.log.info("Cleaning Python build")

        let pythonLibDir = shell.joinPath(path: prefixDir, part: "lib/\(nameVer)")

        if shell.exists(path: pythonLibDir) {
            shell.globRemove(patterns: removePatterns, inPath: pythonLibDir)

            // Remove specific executables
            let binariesToRemove = [
                "2to3",
                "idle3",
                "idle\(ver)",
                "pydoc3",
                "pydoc\(ver)",
                "2to3-\(ver)"
            ]

            let binDir = shell.joinPath(path: prefixDir, part: "bin")
            for binary in binariesToRemove {
                let binaryPath = shell.joinPath(path: binDir, part: binary)
                if shell.exists(path: binaryPath) {
                    shell.log.info("Removing binary: \(binaryPath)")
                    shell.remove(path: binaryPath)
                }
            }
        }
    }

    private func zipPythonLib() {
        shell.log.info("Zipping Python library")

        let pythonLibDir = shell.joinPath(path: prefixDir, part: "lib/\(nameVer)")
        let libDynloadDir = shell.joinPath(path: pythonLibDir, part: "lib-dynload")
        let osModule = shell.joinPath(path: pythonLibDir, part: "os.py")

        let tempDir = project.buildDir
        let tempLibDynload = shell.joinPath(path: tempDir, part: "lib-dynload")
        let tempOsModule = shell.joinPath(path: tempDir, part: "os.py")

        // Move lib-dynload and os.py to temp location
        if shell.exists(path: libDynloadDir) {
            shell.move(src: libDynloadDir, dst: tempLibDynload)
        }
        if shell.exists(path: osModule) {
            shell.move(src: osModule, dst: tempOsModule)
        }

        // Create zip file
        let zipPath = shell.joinPath(path: prefixDir, part: "lib/python\(verNodot).zip")
        shell.zipDirectory(source: pythonLibDir, destination: zipPath)

        // Remove the library directory and recreate it
        shell.rmtree(path: pythonLibDir)
        shell.mkdir(path: pythonLibDir)
        shell.mkdir(path: shell.joinPath(path: pythonLibDir, part: "site-packages"))

        // Move back lib-dynload and os.py
        if shell.exists(path: tempLibDynload) {
            shell.move(src: tempLibDynload, dst: libDynloadDir)
        }
        if shell.exists(path: tempOsModule) {
            shell.move(src: tempOsModule, dst: osModule)
        }

        shell.log.info("Python library zipped successfully")
    }

}

class OpenSSLBuilder: Builder {

    static var name: String = "OpenSSL"
    static var urlTemplate: String = "https://github.com/openssl/openssl.git"
    static var staticLibs: [String] = ["libssl.a", "libcrypto.a"]
    static var dependsOn: [Builder] = []

    var semver: Version
    var project: Project
    var shell: Shell

    required init(version: String, project: Project) {
        // OpenSSL uses tag format like OpenSSL_1_1_1w
        let cleanVersion = version.replacingOccurrences(of: ".", with: "_")
        let _ = "OpenSSL_\(cleanVersion)"
        self.semver = Version(version) ?? Version(major: 1, minor: 1, patch: 1)
        self.project = project
        self.shell = Shell()
    }

    var version: String {
        return self.semver.versionString()
    }

    var ver: String {
        return "\(self.semver.major).\(self.semver.minor).\(self.semver.patch)"
    }

    var verMajor: Int { return self.semver.major }
    var verMinor: Int { return self.semver.minor }
    var verPatch: Int { return self.semver.patch }

    var verNodot: String {
        return "\(self.semver.major)\(self.semver.minor)\(self.semver.patch)"
    }

    var nameVersion: String {
        return "\(Self.name)-\(self.version)"
    }

    var nameVer: String {
        return "\(Self.name.lowercased())\(self.ver)"
    }

    var url: String {
        return Self.urlTemplate
    }

    var sourceDir: String {
        return self.project.sourceDir + "/\(Self.name.lowercased())"
    }

    var prefixDir: String {
        return self.project.installDir + "/\(Self.name.lowercased())"
    }

    func build() {
        shell.log.info("Building OpenSSL")
        // Clone from git
        let cleanVersion = version.replacingOccurrences(of: ".", with: "_")
        let tagVersion = "OpenSSL_\(cleanVersion)"

        if !shell.exists(path: sourceDir) {
            let res = shell.cmd(exe: "/usr/bin/git", args: "clone", "--depth=1", "--branch", tagVersion, url, sourceDir)
            if !res.err.isEmpty {
                shell.log.error("Git clone failed: \(res.err)")
                return
            }
        }

        // Configure and build
        shell.chdir(path: sourceDir)
        let _ = shell.cmd(exe: "./Configure", args: "darwin64-arm64-cc", "--prefix=\(prefixDir)", "--openssldir=\(prefixDir)/ssl")
        let _ = shell.cmd(exe: "make")
        let _ = shell.cmd(exe: "make", args: "install_sw")
    }
}

class Bzip2Builder: Builder {

    static var name: String = "Bzip2"
    static var urlTemplate: String = "https://gitlab.com/bzip2/bzip2.git"
    static var staticLibs: [String] = ["libbz2.a"]
    static var dependsOn: [Builder] = []

    var semver: Version
    var project: Project
    var shell: Shell

    required init(version: String, project: Project) {
        self.semver = Version(version) ?? Version(major: 1, minor: 0, patch: 8)
        self.project = project
        self.shell = Shell()
    }

    var version: String { return self.semver.versionString() }
    var ver: String { return "\(self.semver.major).\(self.semver.minor).\(self.semver.patch)" }
    var verMajor: Int { return self.semver.major }
    var verMinor: Int { return self.semver.minor }
    var verPatch: Int { return self.semver.patch }
    var verNodot: String { return "\(self.semver.major)\(self.semver.minor)\(self.semver.patch)" }

    var nameVersion: String { return "\(Self.name)-\(self.version)" }
    var nameVer: String { return "\(Self.name.lowercased())\(self.ver)" }
    var url: String { return Self.urlTemplate }

    var sourceDir: String {
        return self.project.sourceDir + "/\(Self.name.lowercased())"
    }

    var prefixDir: String {
        return self.project.installDir + "/\(Self.name.lowercased())"
    }

    func build() {
        shell.log.info("Building Bzip2")

        if !shell.exists(path: sourceDir) {
            let res = shell.cmd(exe: "/usr/bin/git", args: "clone", "--depth=1", "--branch", "bzip2-\(version)", url, sourceDir)
            if !res.err.isEmpty {
                shell.log.error("Git clone failed: \(res.err)")
                return
            }
        }

        shell.chdir(path: sourceDir)
        let _ = shell.cmd(exe: "make", args: "PREFIX=\(prefixDir)", "install")
    }
}

class XzBuilder: Builder {

    static var name: String = "Xz"
    static var urlTemplate: String = "https://github.com/tukaani-project/xz.git"
    static var staticLibs: [String] = ["liblzma.a"]
    static var dependsOn: [Builder] = []

    var semver: Version
    var project: Project
    var shell: Shell

    required init(version: String, project: Project) {
        self.semver = Version(version) ?? Version(major: 5, minor: 2, patch: 5)
        self.project = project
        self.shell = Shell()
    }

    var version: String { return self.semver.versionString() }
    var ver: String { return "\(self.semver.major).\(self.semver.minor).\(self.semver.patch)" }
    var verMajor: Int { return self.semver.major }
    var verMinor: Int { return self.semver.minor }
    var verPatch: Int { return self.semver.patch }
    var verNodot: String { return "\(self.semver.major)\(self.semver.minor)\(self.semver.patch)" }

    var nameVersion: String { return "\(Self.name)-\(self.version)" }
    var nameVer: String { return "\(Self.name.lowercased())\(self.ver)" }
    var url: String { return Self.urlTemplate }

    var sourceDir: String {
        return self.project.sourceDir + "/\(Self.name.lowercased())"
    }

    var prefixDir: String {
        return self.project.installDir + "/\(Self.name.lowercased())"
    }

    func build() {
        shell.log.info("Building Xz")

        if !shell.exists(path: sourceDir) {
            let res = shell.cmd(exe: "/usr/bin/git", args: "clone", "--depth=1", "--branch", "v\(version)", url, sourceDir)
            if !res.err.isEmpty {
                shell.log.error("Git clone failed: \(res.err)")
                return
            }
        }

        shell.chdir(path: sourceDir)
        let _ = shell.cmd(exe: "./autogen.sh")
        let _ = shell.cmd(exe: "./configure", args: "--prefix=\(prefixDir)", "--enable-small")
        let _ = shell.cmd(exe: "make")
        let _ = shell.cmd(exe: "make", args: "install")
    }
}
