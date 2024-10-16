import Foundation
import Logging

import CLib


class Shell {

    let fm: FileManager
    let log: Logger

    init() {
        self.fm = FileManager.default
        self.log = Logger(label: "pybuild.Shell") { _ in
            return Handler(
                formatter: BasicFormatter.buildpy,
                pipe: LoggerTextOutputStreamPipe.standardOutput
            )
        }
    }

    // var cwd: String {
    //     return self.fm.currentDirectoryPath
    // }

    func add(x: Int32, y: Int32) -> Int32 {
        return CLib.add(x, y)
    }

    func cwd() -> String {
        let _cwd = self.fm.currentDirectoryPath
        self.log.info("\(_cwd)")
        return _cwd
    }

    func chdir(path: String) {
        if !self.fm.changeCurrentDirectoryPath(path) {
            print("Could not change working diectory to \(path)")
        }
    }

    func iterdir(path: String) -> [String] {
        self.log.info("iterating over: \(path)")
        do {
            let paths = try self.fm.contentsOfDirectory(atPath: path)
            return paths
        } catch {
            return []
        }
    }

    func userdir() -> String {
        return self.fm.homeDirectoryForCurrentUser.absoluteString
    }

    func tempdir() -> String {
        return self.fm.temporaryDirectory.absoluteString
    }

    func mkdir(path: String) {
        do {
            try self.fm.createDirectory(
                atPath: path,
                withIntermediateDirectories: true,
                attributes: nil)
        } catch {
            print("Error creating directory: \(error)")
        }
    }

    func mkfile(path: String, text: String) {
        if !self.fm.createFile(atPath: path, contents: text.data(using: .utf8), attributes: [:]) {
            print("Could not create file at: \(path)")
        }
    }

    func copy(src: String, dst: String) {
        do {
            try self.fm.copyItem(atPath: src, toPath: dst)
        } catch {
            print("could not copy \(src) to \(dst)")
        }
    }

    func move(src: String, dst: String) {
        do {
            try self.fm.moveItem(atPath: src, toPath: dst)
        } catch {
            print("could not move \(src) to \(dst)")
        }
    }

    func symlink(src: String, dst: String) {
        do {
            try self.fm.createSymbolicLink(atPath: src, withDestinationPath: dst)
        } catch {
            print("could not symlink \(src) to \(dst)")
        }
    }

    func exists(path: String) -> Bool {
        // check if either file or directory exists
        return self.fm.fileExists(atPath: path)
    }

    func remove(path: String) {
        do {
            try self.fm.removeItem(atPath: path)
        } catch {
            print("could not remove \(path)")
        }

    }

#if os(macOS)
    func trash(path: String) {
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

    func walk(path: String) -> [String] {
        if let paths = self.fm.subpaths(atPath: path) {
            return paths
        } else {
            return []
        }
    }

    func cmd(exe: String, args: String...) -> (out: String, err: String) {
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
