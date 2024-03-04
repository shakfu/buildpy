protocol Builder {

	// class properties
	static var name: String {get set}
    static var url_template: String {get set}
    static var libs_static: [String] {get set}
    static var depends_on: [Builder] {get set}

    // instance stored properties
    var version: String {get}
    var project: Project {get}

    // initializer
    init(version: String, project: Project)

    // computed properties:
    var ver: String {get}
    var ver_major: String {get}
    var ver_minor: String {get}
    var ver_patch: String {get}
    var ver_nodot: String {get}
    var name_version: String {get}
    var name_ver: String {get}
    var url: String {get}
    var source_dir: String {get}
    var builc_dir: String {get}
    var executable_name: String {get}
    var executable: String {get}
    var libname: String {get}
    var staticlib_name: String {get}
    var dylib_linkname: String {get}
    var dylib_name: String {get}
    var dylib: String {get}
    var dylib_link: String {get}
    var staticlib: String {get}
    var prefix_dir: String {get}

	// methods
    func libs_static_exist() -> Bool
    func pre_process() -> ()
    func setup() -> ()
    func configure() -> ()
    func build() -> ()
    func install() -> ()
    func clean() -> ()
    func post_process() -> ()
    func process() -> ()
} 