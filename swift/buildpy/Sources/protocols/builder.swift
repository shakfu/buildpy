protocol Builder {

    // class properties
    static var name: String { get set }
    static var urlTemplate: String { get set }
    static var staticLibs: [String] { get set }
    static var dependsOn: [Builder] { get set }

    // instance stored properties
    var version: String { get }
    var project: Project { get }

    // initializer
    init(version: String, project: Project)

    // computed properties:
    var ver: String { get }
    var verMajor: String { get }
    var verMinor: String { get }
    var verPatch: String { get }
    var verNodot: String { get }
    var nameVersion: String { get }
    var nameVer: String { get }
    var url: String { get }
    var sourceDir: String { get }
    var buildDir: String { get }
    var executableName: String { get }
    var executable: String { get }
    var libname: String { get }
    var staticlibName: String { get }
    var dylibLinkname: String { get }
    var dylibName: String { get }
    var dylib: String { get }
    var dylibLink: String { get }
    var staticLib: String { get }
    var prefixDir: String { get }

    // methods
    func staticLibsExist() -> Bool
    func preProcess()
    func setup()
    func configure()
    func build()
    func install()
    func clean()
    func postProcess()
    func process()
}
