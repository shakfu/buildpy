import Foundation
import Logging
import PathKit

public class Shell {

    let pwd: Path
    let fm: FileManager
    let log: Logger

    public init() {
        self.pwd = Path.current
        self.fm = FileManager.default
        self.log = Logger(label: "pybuild.Shell") { _ in
            return Handler(
                formatter: BasicFormatter.buildpy,
                pipe: LoggerTextOutputStreamPipe.standardOutput
            )
        }
    }

    /// Get the current working directory
    public func cwd() -> String {
        let _cwd = self.fm.currentDirectoryPath
        self.log.info("\(_cwd)")
        return _cwd
    }

    /// Change the current working directory
    public func chdir(path: String) {
        if !self.fm.changeCurrentDirectoryPath(path) {
            print("Could not change working diectory to \(path)")
        }
    }

    /// Iterate over the contents of a directory
    public func iterdir(path: String) -> [String] {
        self.log.info("iterating over: \(path)")
        do {
            let paths = try self.fm.contentsOfDirectory(atPath: path)
            return paths
        } catch {
            return []
        }
    }

    /// Get the user's home directory
    public func userdir() -> String {
        return self.fm.homeDirectoryForCurrentUser.absoluteString
    }

    /// Get the system's temporary directory
    public func tempdir() -> String {
        return self.fm.temporaryDirectory.absoluteString
    }

    /// Create a directory
    public func mkdir(path: String) {
        do {
            try self.fm.createDirectory(
                atPath: path,
                withIntermediateDirectories: true,
                attributes: nil)
        } catch {
            print("Error creating directory: \(error)")
        }
    }

    /// Create a file
    public func mkfile(path: String, text: String) {
        if !self.fm.createFile(atPath: path, contents: text.data(using: .utf8), attributes: [:]) {
            print("Could not create file at: \(path)")
        }
    }

    /// Copy a file
    public func copy(src: String, dst: String) {
        do {
            try self.fm.copyItem(atPath: src, toPath: dst)
        } catch {
            print("could not copy \(src) to \(dst)")
        }
    }

    /// Move a file
    public func move(src: String, dst: String) {
        do {
            try self.fm.moveItem(atPath: src, toPath: dst)
        } catch {
            print("could not move \(src) to \(dst)")
        }
    }

    /// Create a symbolic link
    public func symlink(src: String, dst: String) {
        do {
            try self.fm.createSymbolicLink(atPath: src, withDestinationPath: dst)
        } catch {
            print("could not symlink \(src) to \(dst)")
        }
    }

    /// Check if a path exists
    public func exists(path: String) -> Bool {
        // check if either file or directory exists
        return self.fm.fileExists(atPath: path)
    }

    /// Join two paths
    public func joinPath(path: String, part: String) -> String {
        return (Path(path) + Path(part)).string
    }

    /// Join two paths
    public func joinPath(path: Path, part: String) -> Path {
        return path + Path(part)
    }

    /// Check if a path is a directory
    public func isdir(path: String) -> Bool {
        return Path(path).isDirectory
    }

    /// Check if a path is a file
    public func isfile(path: String) -> Bool {
        return !Path(path).isDirectory
    }

    /// Check if a path is a symbolic link
    public func islink(path: String) -> Bool {
        return Path(path).isSymlink
    }

    /// Change the permissions of a file
    public func chmod(path: String, mode: String) {
        do {
            try self.fm.setAttributes([.posixPermissions: mode], ofItemAtPath: path)
        } catch {
            print("could not chmod \(path) to \(mode)")
        }
    }

    /// Remove a file
    public func remove(path: String) {
        do {
            try self.fm.removeItem(atPath: path)
        } catch {
            print("could not remove \(path)")
        }
    }

    /// Remove a file
    public func remove(path: Path) {
        do {
            try path.delete()
        } catch {
            print("could not remove \(path)")
        }
    }

    /// Remove a directory and all its contents
    public func rmtree(path: String) {
        do {
            try self.fm.removeItem(atPath: path)
        } catch {
            print("could not remove \(path)")
        }
    }

    #if os(macOS)
        /// Move a file to the trash
        public func trash(path: String) {
            guard let url = URL(string: path) else {
                print("could not convert \(path) to url")
                return
            }
            do {
                try self.fm.trashItem(at: url, resultingItemURL: nil)
            } catch {
                print("could not trash \(path)")
            }
        }
    #endif

    /// Walk a directory and return all files and directories
    public func walk(path: String) -> [String] {
        if let paths = self.fm.subpaths(atPath: path) {
            return paths
        } else {
            return []
        }
    }

    /// Run a command and return the output and error
    public func cmd(exe: String, args: String...) -> (out: String, err: String) {
        return cmdImpl(exe: exe, args: Array(args))
    }

    /// Run a command with array arguments and return the output and error
    public func cmd(exe: String, args: [String]) -> (out: String, err: String) {
        return cmdImpl(exe: exe, args: args)
    }

    private func cmdImpl(exe: String, args: [String]) -> (out: String, err: String) {
        let outputPipe = Pipe()
        let errorPipe = Pipe()

        let task = Process()
        task.executableURL = URL(fileURLWithPath: exe)
        task.standardOutput = outputPipe
        task.standardError = errorPipe
        task.arguments = args

        do {
            try task.run()
            let outputData = outputPipe.fileHandleForReading.readDataToEndOfFile()
            let errorData = errorPipe.fileHandleForReading.readDataToEndOfFile()
            let output = String(decoding: outputData, as: UTF8.self)
            let error = String(decoding: errorData, as: UTF8.self)
            return (out: output, err: error)
        } catch {
            print("ERROR")
        }
        return (out: "", err: "FAILURE: exe: \(exe) args: \(args)")
    }

    /// Remove files matching glob patterns in a directory
    public func globRemove(patterns: [String], inPath path: String) {
        log.info("Removing files matching patterns \(patterns) in \(path)")

        for subpath in walk(path: path) {
            let fullPath = joinPath(path: path, part: subpath)

            for pattern in patterns {
                if matchPattern(pattern: pattern, string: subpath) {
                    log.info("Removing: \(fullPath)")
                    remove(path: fullPath)
                    break
                }
            }
        }
    }

    /// Simple pattern matching (supports * wildcards)
    private func matchPattern(pattern: String, string: String) -> Bool {
        // Convert glob pattern to regex-like matching
        if pattern == string {
            return true
        }

        if pattern.contains("*") {
            let regexPattern = pattern
                .replacingOccurrences(of: "*", with: ".*")
                .replacingOccurrences(of: "?", with: ".")

            do {
                let regex = try NSRegularExpression(pattern: "^" + regexPattern + "$")
                let range = NSRange(location: 0, length: string.utf16.count)
                return regex.firstMatch(in: string, options: [], range: range) != nil
            } catch {
                return false
            }
        }

        return string.contains(pattern)
    }

    /// Create a zip archive from a directory
    public func zipDirectory(source: String, destination: String) {
        log.info("Creating zip archive: \(destination) from \(source)")

        let result = cmd(exe: "/usr/bin/zip", args: ["-r", destination, "."], chdir: source)
        if !result.err.isEmpty {
            log.error("Zip failed: \(result.err)")
        }
    }

    /// Run command in a specific directory
    public func cmd(exe: String, args: [String], chdir workingDir: String) -> (out: String, err: String) {
        let currentDir = cwd()
        chdir(path: workingDir)
        let result = cmd(exe: exe, args: args)
        chdir(path: currentDir)
        return result
    }
}
