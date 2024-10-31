import Foundation
import CLib

struct Message {
	let name: String
	let age: Int
}

func add(x: Int32, y: Int32) -> Int32 {
    return CLib.add(x, y)
}

