import Foundation
import Logging


public class Shell {

    let fm: FileManager
    let log: Logger

    public init() {
        self.fm = FileManager.default
        self.log = Logger(label: "pybuild.Shell") { _ in
            return Handler(
                formatter: BasicFormatter.buildpy,
                pipe: LoggerTextOutputStreamPipe.standardOutput
            )
        }
    }

    public func cwd() -> String {
        let _cwd = self.fm.currentDirectoryPath
        self.log.info("\(_cwd)")
        return _cwd
    }

    public func chdir(path: String) {
        if !self.fm.changeCurrentDirectoryPath(path) {
            print("Could not change working diectory to \(path)")
        }
    }

    public func iterdir(path: String) -> [String] {
        self.log.info("iterating over: \(path)")
        do {
            let paths = try self.fm.contentsOfDirectory(atPath: path)
            return paths
        } catch {
            return []
        }
    }

    public func userdir() -> String {
        return self.fm.homeDirectoryForCurrentUser.absoluteString
    }

    public func tempdir() -> String {
        return self.fm.temporaryDirectory.absoluteString
    }

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

    public func mkfile(path: String, text: String) {
        if !self.fm.createFile(atPath: path, contents: text.data(using: .utf8), attributes: [:]) {
            print("Could not create file at: \(path)")
        }
    }

    public func copy(src: String, dst: String) {
        do {
            try self.fm.copyItem(atPath: src, toPath: dst)
        } catch {
            print("could not copy \(src) to \(dst)")
        }
    }

    public func move(src: String, dst: String) {
        do {
            try self.fm.moveItem(atPath: src, toPath: dst)
        } catch {
            print("could not move \(src) to \(dst)")
        }
    }

    public func symlink(src: String, dst: String) {
        do {
            try self.fm.createSymbolicLink(atPath: src, withDestinationPath: dst)
        } catch {
            print("could not symlink \(src) to \(dst)")
        }
    }

    public func exists(path: String) -> Bool {
        // check if either file or directory exists
        return self.fm.fileExists(atPath: path)
    }

    public func joinPath(path: String, part: String) -> String {
        let pathURL = URL(fileURLWithPath: path)
        let partURL = URL(fileURLWithPath: part)
        return partURL.appendingPathComponent(pathURL.lastPathComponent).path
    }

    // func isdir(path: String) -> Bool {
    //     return self.exists(path: path) && self.fm.isDirectory(atPath: path)
    // }

    // func isfile(path: String) -> Bool {
    //     return self.fm.fileExists(atPath: path) && !self.fm.isDirectory(atPath: path)
    // }

    // func islink(path: String) -> Bool {
    //     return self.fm.fileExists(atPath: path) && self.fm.isSymbolicLink(atPath: path)
    // }

    public func chmod(path: String, mode: String) {
        do {
            try self.fm.setAttributes([.posixPermissions: mode], ofItemAtPath: path)
        } catch {
            print("could not chmod \(path) to \(mode)")
        }
    }

    public func remove(path: String) {
        do {
            try self.fm.removeItem(atPath: path)
        } catch {
            print("could not remove \(path)")
        }

    }

    public func rmtree(path: String) {
        do {
            try self.fm.removeItem(atPath: path)
        } catch {
            print("could not remove \(path)")
        }
    }

#if os(macOS)
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

    public func walk(path: String) -> [String] {
        if let paths = self.fm.subpaths(atPath: path) {
            return paths
        } else {
            return []
        }
    }

    public func cmd(exe: String, args: String...) -> (out: String, err: String) {
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
}
