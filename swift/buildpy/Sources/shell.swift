import Foundation


func execute(exe: String, args: String...) -> (out: String, err: String) {
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
