import ArgumentParser
import BuildPyLib
import Foundation

@main
struct Buildpy: ParsableCommand {
    @Option(help: "python version")
    var version: String = "3.12.9"

    @Option(help: "name of build config")
    var config: String = "static.max"

    @Option(help: "configure options")
    var opts: [String] = []

    @Option(help: "python packages to install")
    var pkgs: [String] = []

    @Flag(help: "build debug python")
    var debug = false

    @Flag(help: "optimize build")
    var optimize = false

    @Flag(help: "reset build")
    var reset = false

    mutating func run() throws {
        print(
            """
            version: \(version)
            config: \(config)
            opts: \(opts)
            pkgs: \(pkgs)
            debug: \(debug)
            optimize: \(optimize)
            reset: \(reset)
            """
        )

        let shell = BuildPyLib.Shell()
        let res = shell.cmd(exe: "/usr/bin/git", args: "--version")
        let cwd = shell.cwd()
        print(cwd)
        print("output: \(res.out)")

        // print("error: \(res.err)")

        // for f in shell.iterdir(path: ".") {
        //     print(f)
        // }

        // print("userdir: \(shell.userdir())")

        for _ in Progress(1...3) {
            sleep(1)
        }
    }
}
