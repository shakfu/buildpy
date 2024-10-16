/// This file is a slightly tweaked variant of 
/// https://github.com/Adorkable/swift-log-format-and-pipe
/// Created by Ian Grossberg on 7/26/19
/// The changes are
/// - addding color (see: `colored` function)
/// - conversion to a single file
/// - change dateformat
/// - added custom custom BasicFormater instance: `buildpy`

import Foundation
import Logging
import Rainbow

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
    import Darwin
#else
    import Glibc
#endif

public struct LoggerTextOutputStreamPipe: LogPipe {
    private let stream: TextOutputStream

    public init(_ stream: TextOutputStream) {
        self.stream = stream
    }

    public func handle(_ formattedLogLine: String) {
        var stream = self.stream
        stream.write("\(formattedLogLine)\n")
    }
}

extension LoggerTextOutputStreamPipe {

    public static var standardOutput: LoggerTextOutputStreamPipe {
        return LoggerTextOutputStreamPipe(StdioOutputStream.stdout)
    }

    public static var standardError: LoggerTextOutputStreamPipe {
        return LoggerTextOutputStreamPipe(StdioOutputStream.stderr)
    }

}

public struct StdioOutputStream: TextOutputStream {

    public let file: UnsafeMutablePointer<FILE>

    public func write(_ string: String) {
        string.withCString { ptr in
            flockfile(file)
            defer {
                funlockfile(file)
            }
            _ = fputs(ptr, file)
        }
    }

    internal static let stderr = StdioOutputStream(file: systemStderr)
    internal static let stdout = StdioOutputStream(file: systemStdout)
}

// Prevent name clashes
#if os(macOS) || os(tvOS) || os(iOS) || os(watchOS)
    let systemStderr = Darwin.stderr
    let systemStdout = Darwin.stdout
#else
    let systemStderr = Glibc.stderr!
    let systemStdout = Glibc.stdout!
#endif

public protocol LogPipe {
    func handle(_ formattedLogLine: String)
}

public struct Handler: LogHandler {

    public init(formatter: Formatter, pipe: LogPipe) {
        self.formatter = formatter
        self.pipe = pipe
    }

    public let formatter: Formatter

    public let pipe: LogPipe

    public var logLevel: Logger.Level = .info

    public func log(
        level: Logger.Level,
        message: Logger.Message,
        metadata: Logger.Metadata?,
        source: String,
        file: String,
        function: String, 
        line: UInt
    ) {
        let prettyMetadata =
            metadata?.isEmpty ?? true
            ? self.prettyMetadata
            : self.prettify(self.metadata.merging(metadata!, uniquingKeysWith: { _, new in new }))

        let formattedMessage = self.formatter.processLog(
            level: level, message: message, prettyMetadata: prettyMetadata, file: file,
            function: function, line: line)
        self.pipe.handle(formattedMessage)
    }

    private var prettyMetadata: String?

    public var metadata = Logger.Metadata() {
        didSet {
            self.prettyMetadata = self.prettify(self.metadata)
        }
    }

    public subscript(metadataKey metadataKey: String) -> Logger.Metadata.Value? {
        get {
            return self.metadata[metadataKey]
        }
        set {
            self.metadata[metadataKey] = newValue
        }
    }

    private func prettify(_ metadata: Logger.Metadata) -> String? {
        return !metadata.isEmpty ? metadata.map { "\($0)=\($1)" }.joined(separator: " ") : nil
    }
}

public enum LogComponent {
    case timestamp
    case level
    case message
    case metadata
    case file
    case function
    case line
    case text(String)
    case group([LogComponent])

    public static var allNonmetaComponents: [LogComponent] {
        return [
            .timestamp,
            .level,
            .message,
            .metadata,
            .file,
            .function,
            .line,
        ]
    }
}

public protocol Formatter {

    var timestampFormatter: DateFormatter { get }

    func processLog(
        level: Logger.Level,
        message: Logger.Message,
        prettyMetadata: String?,
        file: String, function: String, line: UInt
    ) -> String

}


func colored(_ level: Logger.Level) -> String {
    switch level {
    case .trace:
        return "TRCE".white
    case .debug:
        return "DBUG".green
    case .info:
        return "INFO".cyan
    case .notice:
        return "NOTE".blue
    case .warning:
        return "WARN".yellow
    case .error:
        return "EROR".magenta
    case .critical:
        return "CRTL".red
    }
}


extension Formatter {
    public func processComponent(
        _ component: LogComponent, now: Date, level: Logger.Level,
        message: Logger.Message,
        prettyMetadata: String?,
        file: String, function: String, line: UInt
    ) -> String {
        switch component {
        case .timestamp:
            return self.timestampFormatter.string(from: now)
        case .level:
            return colored(level)
        case .message:
            return "\(message)"
        case .metadata:
            return "\(prettyMetadata.map { "\($0)" } ?? "")"
        case .file:
            return "\(file)"
        case .function:
            return "\(function)"
        case .line:
            return "\(line)"
        case .text(let string):
            return string
        case .group(let logComponents):
            return logComponents.map({ (component) -> String in
                self.processComponent(
                    component, now: now, level: level, message: message,
                    prettyMetadata: prettyMetadata, file: file, function: function,
                    line: line)
            }).joined()
        }
    }
}

public struct BasicFormatter: Formatter {

    public let format: [LogComponent]

    public let separator: String?

    public let timestampFormatter: DateFormatter

    static public var timestampFormatter: DateFormatter {
        let result = DateFormatter()

        result.dateFormat = "HH:mm:ss"
        return result
    }

    public init(
        _ format: [LogComponent] = LogComponent.allNonmetaComponents, separator: String = " ",
        timestampFormatter: DateFormatter = BasicFormatter.timestampFormatter
    ) {
        self.format = format
        self.separator = separator
        self.timestampFormatter = timestampFormatter
    }

    public func processLog(
        level: Logger.Level,
        message: Logger.Message,
        prettyMetadata: String?,
        file: String, function: String, line: UInt
    ) -> String {
        let now = Date()

        return self.format.map({ (component) -> String in
            return self.processComponent(
                component, now: now, level: level, message: message, prettyMetadata: prettyMetadata,
                file: file, function: function, line: line)
        }).filter({ (string) -> Bool in
            return string.count > 0
        }).joined(separator: self.separator ?? "")
    }

    public static let apple = BasicFormatter(
        [
            .timestamp,
            .group([
                .level,
                .text(":"),
            ]),
            .message,
        ]
    )

    public static let adorkable = BasicFormatter(
        [
            .timestamp,
            .level,
            .group([
                .file,
                .text(":"),
                .line,
            ]),
            .function,
            .message,
            .metadata,
        ],
        separator: " ▶ "
    )

    public static let buildpy = BasicFormatter(
        [
            .timestamp,
            .level,
            .function,
            .message,
            .metadata,
        ],
        separator: " - "
    )
}
