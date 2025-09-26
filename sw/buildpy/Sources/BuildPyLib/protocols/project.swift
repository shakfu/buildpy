public protocol Project {

    // instance properties
    var cwd: String { get }
    var buildDir: String { get }
    var downloadsDir: String { get }
    var sourceDir: String { get }
    var installDir: String { get }
    var binDir: String { get }
    var libDir: String { get }
    var libStaticDir: String { get }

    // methods
    func setup() -> Bool
    func clean()
}
