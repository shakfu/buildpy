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

    // methods
    func move_entries(src: String, dst: String, names: String...)
    func enable_static(names: String...)
    func enable_shared(names: String...)
    func disable_static(names: String...)
    func disable_shared(names: String...)
    func move_static_to_shared(names: String...)
    func move_shared_to_static(names: String...)
    func write(method: String, to: String)
    func write_json(method: String, to: String)
}


