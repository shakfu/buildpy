
protocol ShellCmd {

    // class properties
    // static var cwd: String { get set }
    static var build: String { get }
    static var downloads: String { get }
    static var src: String { get }
    static var install: String { get }

    // initialization
    init()

    // methods
    func cwd() -> String
    func chdir(path: String)
    func iterdir(path: String) -> [String]
    func userdir() -> String
    func tempdir() -> String
    func mkdir(path: String)
    func makedirs(path: String, mode: Int, exist_ok: Bool)
    func mkfile(path: String, text: String)
    func copy(src: String, dst: String)
    func move(src: String, dst: String)
    func remove(path: String, silent: Bool)
    func symlink(src: String, dst: String)
    func walk(path: String) -> [String]
    func glob_remove(root: String, patterns: [String], skip_dirs: [String])
    func cmd(exe: String, args: String...) -> (out: String, err: String)
    func download(url: String, tofolder: String)
    func extract(archive: String, tofolder: String)
    func fail(msg: String, msgs: String...)
    func git_clone(url: String, branch: Optional<String>, directory: Optional<String>, recurse: Bool, cwd: String)
    func getenv(key: String, default: Bool) -> Bool
    func chmod(path: String, perm: Int)
    func get(cwd: String, shell: Bool) -> String    
    func pip_install(reqs: Optional<String>, upgrade: Bool, pip: Optional<String>, pkgs: String...)
    func apt_install(update: Bool, pkgs: String...)
    func brew_install(update: Bool, pkgs: String...)

    // func cmake_config(src_dir: String, build_dir: String, *scripts, **options)
    // func cmake_build(build_dir: String, release: Bool)
    // func cmake_install(build_dir: String, prefix: Optional[String])

}

