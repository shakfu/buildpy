import SemVer

protocol Builder {

    // class properties
    static var name: String { get set }
    static var urlTemplate: String { get set }
    static var staticLibs: [String] { get set }
    static var dependsOn: [Builder] { get set }

    // instance stored properties
    var semver: Version { get }
    var project: Project { get }

    // initializer
    init(version: String, project: Project)

    // computed properties:
    var version: String { get }
    var ver: String { get }
    var verMajor: Int { get }
    var verMinor: Int { get }
    var verPatch: Int { get }
    var verNodot: String { get }
    var nameVersion: String { get }
    var nameVer: String { get }
    var url: String { get }

    // methods
    func build()
}
