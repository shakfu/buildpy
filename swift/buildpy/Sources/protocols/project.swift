protocol Project {

    // class properties
    static var cwd: String { get set }
    static var build: String { get }
    static var downloads: String { get }
    static var src: String { get }
    static var install: String { get }

    // methods
    func setup() -> Bool
    func clean()
}
