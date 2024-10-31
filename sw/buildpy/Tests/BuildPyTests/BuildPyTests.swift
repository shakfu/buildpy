import Testing
import XCTest
@testable import BuildPyLib

// @Test func example() async throws {
//     // Write your test here and use APIs like `#expect(...)` to check expected conditions.

@Test
func test_clib_add() throws {
    let result = add(x: 1, y: 2)
    XCTAssertEqual(result, 3, "add 1 + 2 = 3")    
}

@Test
func test_shell_joinPath() throws {
    let shell = Shell()
    let result = shell.joinPath(path: "Hello", part: "World")
    XCTAssertEqual(result, "Hello/World", "join Hello and World = Hello/World")
}

// class ShellTests: XCTestCase {

//     private var shell: Shell!

//     override func setUp() {
//         super.setUp()
//         shell = Shell()
//     }

//     func test_joinPath() throws {
//         let result = shell.joinPath(path: "/tmp/Hello", part: "World")
//         XCTAssertEqual(result, "/tmp/Hello/World", "join Hello and World = Hello/World")    
//     }

//     override func tearDown() {
//         super.tearDown()
//     }
// }