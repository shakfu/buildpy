import SwiftUI

@main
struct ExampleApp: App {
    @NSApplicationDelegateAdaptor(AppDelegate.self) var appDelegate

    var body: some Scene {
        WindowGroup {
            ContentView()
        }
    }
}

class AppDelegate: NSObject, NSApplicationDelegate, ObservableObject {
    func applicationDidFinishLaunching(_ notification: Notification) {
        NSApp.setActivationPolicy(.regular)
        NSApp.activate(ignoringOtherApps: true)
        NSApp.windows.first?.makeKeyAndOrderFront(nil)
    }
}

struct ContentView: View {
    @State private var showDetails = false
    @State private var text = "Hello, world!"

    var body: some View {
        VStack {
            Text(self.text)
                .padding()
            TextField("Enter text", text: self.$text)
                .padding()
            Button("Build Python") {
                showDetails.toggle()
            }

            if showDetails {
                Text("Python is built!")
                    .font(.largeTitle)
            }
        }
    }
}