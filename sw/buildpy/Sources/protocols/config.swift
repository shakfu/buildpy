protocol Config {

    // instance stored properties
    var name: String { get set }
    var version: String { get set }

    var headers: [String] { get set }
    var exts: [String: [String]] { get set }
    var core: Set<String> { get }
    var statik: Set<String> { get set }
    var shared: Set<String> { get set }
    var disabled: Set<String> { get set }

    // initializer
    init(name: String, version: String)
}
