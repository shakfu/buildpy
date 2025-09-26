import ArgumentParser
import BuildPyLib
import Foundation

@main
struct Buildpy: ParsableCommand {
    static let configuration = CommandConfiguration(
        abstract: "A Python builder - builds Python from source with customizable options"
    )

    @Option(name: [.customShort("a"), .long], help: "Add config options")
    var cfgOpts: [String] = []

    @Option(name: [.customShort("c"), .long], help: "Build configuration (default: static_max)")
    var config: String = "static_max"

    @Flag(name: [.customShort("d"), .long], help: "Build debug python")
    var debug = false

    @Flag(name: [.customShort("o"), .long], help: "Optimize build")
    var optimize = false

    @Option(name: [.customShort("p"), .long], help: "Install packages")
    var pkgs: [String] = []

    @Flag(name: [.customShort("r"), .long], help: "Reset build")
    var reset = false

    @Option(name: [.customShort("v"), .long], help: "Python version (default: 3.12.9)")
    var version: String = "3.12.9"

    @Flag(name: [.customShort("w"), .long], help: "Write configuration")
    var write = false

    @Option(name: [.customShort("j"), .long], help: "Number of build jobs (default: 4)")
    var jobs: Int = 4

    @Option(name: [.customShort("s"), .long], help: "Serialize config to json file")
    var json: String?

    @Flag(help: "Show verbose output")
    var verbose = false

    mutating func run() throws {
        if verbose {
            print("Swift buildpy starting with options:")
            print("  version: \(version)")
            print("  config: \(config)")
            print("  cfgOpts: \(cfgOpts)")
            print("  pkgs: \(pkgs)")
            print("  debug: \(debug)")
            print("  optimize: \(optimize)")
            print("  reset: \(reset)")
            print("  write: \(write)")
            print("  jobs: \(jobs)")
            print("")
        }

        // Create project and setup build environment
        let project = pyProject()

        if reset {
            print("Resetting build environment...")
            project.clean()
        }

        if !project.setup() {
            throw ExitCode.failure
        }

        if write {
            print("Write configuration not yet implemented")
            return
        }

        // Validate config
        let validConfigs = ["static_max", "static_mid", "static_min", "shared_max", "shared_mid", "shared_min"]
        if !validConfigs.contains(config) {
            print("Error: Invalid config '\(config)'. Valid configs: \(validConfigs)")
            throw ExitCode.failure
        }

        // Create and run Python builder
        print("Building Python \(version) with config: \(config)")
        let pythonBuilder = PythonBuilder(version: version, project: project)
        pythonBuilder.build()

        print("Build completed successfully!")
    }
}
