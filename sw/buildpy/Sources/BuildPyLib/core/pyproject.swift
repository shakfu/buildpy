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
    var cwd: String { self.fm.currentDirectoryPath }
    var buildDir: String { "\(self.cwd)/build" }
    var downloadsDir: String { "\(self.buildDir)/downloads" }
    var sourceDir: String { "\(self.buildDir)/src" }
    var installDir: String { "\(self.buildDir)/install" }
    var binDir: String { "\(self.installDir)/bin" }
    var libDir: String { "\(self.installDir)/lib" }
    var libStaticDir: String { "\(self.installDir)/lib" }

    // methods
    public func setup() -> Bool {
    	return true
    }
    public func clean() {}

}