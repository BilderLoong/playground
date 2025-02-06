ObjC.import('AppKit');
ObjC.import('CoreGraphics');

// Function to capture the main screen and copy it to the clipboard
function captureScreenToClipboard() {
    // Get the main screen's frame
    var screen = $.NSScreen.mainScreen();
    var rect = screen.frame;

    // Capture the screen's image
    var cgImage = $.CGWindowListCreateImage(
        rect,
        $.kCGWindowListOptionOnScreenOnly,
        $.kCGNullWindowID,
        $.kCGWindowImageDefault
    );

    if (cgImage) {
        // Copy the screenshot to the clipboard
        var pasteboard = $.NSPasteboard.generalPasteboard;
        pasteboard.clearContents();
        var nsImage = $.NSImage.alloc().initWithCGImageSize(cgImage, rect.size);
        pasteboard.setDataForType(nsImage.TIFFRepresentation(), $.NSPasteboardTypeTIFF);
        console.log('Screenshot copied to clipboard.');
    } else {
        console.log('Failed to take screenshot.');
    }
}

// Call the function
captureScreenToClipboard();