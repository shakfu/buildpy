
//
//  https://github.com/jkandzi/Progress.swift
//
//  Created by Justus Kandzi on 27/12/15.
//  Copyright © 2015 Justus Kandzi. All rights reserved.
//
//  The MIT License (MIT)
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.


#if os(Linux)
    import Glibc
#else
    import Darwin.C
#endif

func getTimeOfDay() -> Double {
    var tv = timeval()
    gettimeofday(&tv, nil)
    return Double(tv.tv_sec) + Double(tv.tv_usec) / 1000000
}

extension Double {
    func format(_ decimalPartLength: Int, minimumIntegerPartLength: Int = 0) -> String {
        let value = String(self)
        let components = value
            .split() { $0 == "." }
            .map { String($0) }
        
        var integerPart = components.first ?? "0"
        
        let missingLeadingZeros = minimumIntegerPartLength - integerPart.count
        if missingLeadingZeros > 0 {
            integerPart = stringWithZeros(missingLeadingZeros) + integerPart
        }
        
        if decimalPartLength == 0 {
            return integerPart
        }
        
        var decimalPlaces = components.last?.substringWithRange(0, end: decimalPartLength) ?? "0"
        let missingPlaceCount = decimalPartLength - decimalPlaces.count
        decimalPlaces += stringWithZeros(missingPlaceCount)
        
        return "\(integerPart).\(decimalPlaces)"
    }
    
    fileprivate func stringWithZeros(_ count: Int) -> String {
        return Array(repeating: "0", count: count).joined(separator: "")
    }
}

extension String {
    func substringWithRange(_ start: Int, end: Int) -> String {
        var end = end
        if start < 0 || start > self.count {
            return ""
        }
        else if end < 0 || end > self.count {
            end = self.count
        }
        let range = self.index(self.startIndex, offsetBy: start) ..< self.index(self.startIndex, offsetBy: end)
        return String(self[range])
    }
}


public protocol ProgressElementType {
    func value(_ progressBar: ProgressBar) -> String
}


/// the progress bar element e.g. "[----------------------        ]"
public struct ProgressBarLine: ProgressElementType {
    let barLength: Int
    
    public init(barLength: Int = 30) {
        self.barLength = barLength
    }
    
    public func value(_ progressBar: ProgressBar) -> String {
        var completedBarElements = 0
        if progressBar.count == 0 {
            completedBarElements = barLength
        } else {
            completedBarElements = Int(Double(barLength) * (Double(progressBar.index) / Double(progressBar.count)))
        }
        
        var barArray = [String](repeating: "-", count: completedBarElements)
        barArray += [String](repeating: " ", count: barLength - completedBarElements)
        return "[" + barArray.joined(separator: "") + "]"
    }
}


/// the index element e.g. "2 of 3"
public struct ProgressIndex: ProgressElementType {
    public init() {}
    
    public func value(_ progressBar: ProgressBar) -> String {
        return "\(progressBar.index) of \(progressBar.count)"
    }
}


/// the percentage element e.g. "90.0%"
public struct ProgressPercent: ProgressElementType {
    let decimalPlaces: Int
    
    public init(decimalPlaces: Int = 0) {
        self.decimalPlaces = decimalPlaces
    }
    
    public func value(_ progressBar: ProgressBar) -> String {
        var percentDone = 100.0
        if progressBar.count > 0 {
            percentDone = Double(progressBar.index) / Double(progressBar.count) * 100
        }
        return "\(percentDone.format(decimalPlaces))%"
    }
}


/// the time estimates e.g. "ETA: 00:00:02 (at 1.00 it/s)"
public struct ProgressTimeEstimates: ProgressElementType {
    public init() {}
    
    public func value(_ progressBar: ProgressBar) -> String {
        let totalTime = getTimeOfDay() - progressBar.startTime
        
        var itemsPerSecond = 0.0
        var estimatedTimeRemaining = 0.0
        if progressBar.index > 0 {
            itemsPerSecond = Double(progressBar.index) / totalTime
            estimatedTimeRemaining = Double(progressBar.count - progressBar.index) / itemsPerSecond
        }
        
        let estimatedTimeRemainingString = formatDuration(estimatedTimeRemaining)
        
        return "ETA: \(estimatedTimeRemainingString) (at \(itemsPerSecond.format(2))) it/s)"
    }
    
    fileprivate func formatDuration(_ duration: Double) -> String {
        let duration = Int(duration)
        let seconds = Double(duration % 60)
        let minutes = Double((duration / 60) % 60)
        let hours = Double(duration / 3600)
        return "\(hours.format(0, minimumIntegerPartLength: 2)):\(minutes.format(0, minimumIntegerPartLength: 2)):\(seconds.format(0, minimumIntegerPartLength: 2))"
    }
}


/// an arbitrary string that can be added to the progress bar.
public struct ProgressString: ProgressElementType {
    let string: String
    
    public init(string: String) {
        self.string = string
    }
    
    public func value(_: ProgressBar) -> String {
        return string
    }
}

public protocol ProgressBarPrinter {
    mutating func display(_ progressBar: ProgressBar)
}

struct ProgressBarTerminalPrinter: ProgressBarPrinter {
    var lastPrintedTime = 0.0

    init() {
        // the cursor is moved up before printing the progress bar.
        // have to move the cursor down one line initially.
        print("")
    }
    
    mutating func display(_ progressBar: ProgressBar) {
        let currentTime = getTimeOfDay()
        if (currentTime - lastPrintedTime > 0.1 || progressBar.index == progressBar.count) {
            print("\u{1B}[1A\u{1B}[K\(progressBar.value)")
            lastPrintedTime = currentTime
        }
    }
}


public struct ProgressBar {
    private(set) public var index = 0
    public let startTime = getTimeOfDay()
    
    public let count: Int
    let configuration: [ProgressElementType]?

    public static var defaultConfiguration: [ProgressElementType] = [ProgressIndex(), ProgressBarLine(), ProgressTimeEstimates()]

    var printer: ProgressBarPrinter
    
    public var value: String {
        let configuration = self.configuration ?? ProgressBar.defaultConfiguration
        let values = configuration.map { $0.value(self) }
        return values.joined(separator: " ")
    }
    
    public init(count: Int, configuration: [ProgressElementType]? = nil, printer: ProgressBarPrinter? = nil) {
        self.count = count
        self.configuration = configuration
        self.printer = printer ?? ProgressBarTerminalPrinter()
    }
    
    public mutating func next() {
        guard index <= count else { return }
        let anotherSelf = self
        printer.display(anotherSelf)
        index += 1
    }

    public mutating func setValue(_ index: Int) {
        guard index <= count && index >= 0 else { return }
        self.index = index
        let anotherSelf = self
        printer.display(anotherSelf)
    }

}


public struct ProgressGenerator<G: IteratorProtocol>: IteratorProtocol {
    var source: G
    var progressBar: ProgressBar
    
    init(source: G, count: Int, configuration: [ProgressElementType]? = nil, printer: ProgressBarPrinter? = nil) {
        self.source = source
        self.progressBar = ProgressBar(count: count, configuration: configuration, printer: printer)
    }
    
    public mutating func next() -> G.Element? {
        progressBar.next()
        return source.next()
    }
}

public struct Progress<G: Sequence>: Sequence {
    let generator: G
    let configuration: [ProgressElementType]?
    let printer: ProgressBarPrinter?
    
    public init(_ generator: G, configuration: [ProgressElementType]? = nil, printer: ProgressBarPrinter? = nil) {
        self.generator = generator
        self.configuration = configuration
        self.printer = printer
    }
    
    public func makeIterator() -> ProgressGenerator<G.Iterator> {
        let count = generator.underestimatedCount
        return ProgressGenerator(source: generator.makeIterator(), count: count, configuration: configuration, printer: printer)
    }
}