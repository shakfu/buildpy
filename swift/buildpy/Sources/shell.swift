import Foundation



class Shell {
    
    let fm: FileManager

    init() {
        self.fm = FileManager.default
    }

    func iterdir(path: String) -> [String] {
        do {
            let paths = try self.fm.contentsOfDirectory(atPath: path)
            return paths
        } catch {
            return []
        }
    }

    func remove(path: String) -> () {
        do {
            try self.fm.removeItem(atPath: path)
        } catch {
            print("could not remove \(path)")
        }

    }

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
        return (out:"", err: "FAILURE: exe: \(exe) args: \(args)")
    }
}