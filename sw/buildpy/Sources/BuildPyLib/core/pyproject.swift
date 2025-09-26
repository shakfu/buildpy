import Foundation
import Logging
import PathKit

public class pyProject : Project {

    let pwd: Path
    let fm: FileManager
    let log: Logger

    public init() {
        self.pwd = Path.current
        self.fm = FileManager.default
        self.log = Logger(label: "pybuild.PyProject") { _ in
            return Handler(
                formatter: BasicFormatter.buildpy,
                pipe: LoggerTextOutputStreamPipe.standardOutput
            )
        }
    }

    // instance properties
    public var cwd: String { self.fm.currentDirectoryPath }
    public var buildDir: String { "\(self.cwd)/build" }
    public var downloadsDir: String { "\(self.buildDir)/downloads" }
    public var sourceDir: String { "\(self.buildDir)/src" }
    public var installDir: String { "\(self.buildDir)/install" }
    public var binDir: String { "\(self.installDir)/bin" }
    public var libDir: String { "\(self.installDir)/lib" }
    public var libStaticDir: String { "\(self.installDir)/lib" }

    // methods
    public func setup() -> Bool {
        log.info("Setting up build environment")

        let shell = Shell()

        // Create build directory structure
        let dirs = [buildDir, downloadsDir, sourceDir, installDir, binDir, libDir]

        for dir in dirs {
            if !shell.exists(path: dir) {
                log.info("Creating directory: \(dir)")
                shell.mkdir(path: dir)
            }
        }

        log.info("Build environment setup complete")
        return true
    }

    public func clean() {
        log.info("Cleaning build environment")
        let shell = Shell()

        if shell.exists(path: buildDir) {
            log.info("Removing build directory: \(buildDir)")
            shell.rmtree(path: buildDir)
        }

        log.info("Build environment cleaned")
    }

}