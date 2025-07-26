import SemVer

class PythonBuilder: Builder {

    static var name: String = "Python"
    static var urlTemplate: String =
        "https://www.python.org/ftp/python/{version}/Python-{version}.tgz"
    static var staticLibs: [String] = []
    static var dependsOn: [Builder] = []

    var semver: Version
    var project: Project
    var shell: Shell

    required init(version: String, project: Project) {
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

}

// class OpenSSLBuilder: Builder {

//     static var name: String = "openssl"
//     static var urlTemplate: String = "https://www.openssl.org/source/openssl-{version}.tar.gz"
//     static var staticLibs: [String] = ["libssl.a", "libcrypto.a"]
//     static var dependsOn: [Builder] = []

// }

// class Bzip2Builder: Builder {

//     static var name: String = "bzip2"
//     static var urlTemplate: String = "https://www.sourceware.org/pub/bzip2/bzip2-{version}.tar.gz"
//     static var staticLibs: [String] = ["libbz2.a"]
//     static var dependsOn: [Builder] = []

// }

// class XzBuilder: Builder {

//     static var name: String = "xz"
//     static var urlTemplate: String = "https://tukaani.org/xz/xz-{version}.tar.gz"
//     static var staticLibs: [String] = ["liblzma.a"]
//     static var dependsOn: [Builder] = []

// }
