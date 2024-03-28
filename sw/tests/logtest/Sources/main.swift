import Foundation
import SwiftyBeaver
let log = SwiftyBeaver.self

print("hello test")

let console = ConsoleDestination()
console.minLevel = .debug // Set the minimum log level
log.addDestination(console)

let file = FileDestination()
file.minLevel = .debug
file.logFileURL = URL(fileURLWithPath: "./log.txt")
log.addDestination(file)

log.info("this is info")
log.debug("debugging")
log.warning("o dear..")
log.error("bye")

print("bye test")