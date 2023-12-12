import CoreGraphics
import Foundation

func main() -> Void {
   let displayID: CGDirectDisplayID = CGMainDisplayID()
    let image: CGImage? = CGDisplayCreateImage(displayID) 
    if let url = URL(fileURLWithPath: "../../res") {
        write(cgimage: CGImage, to: url)
    }else{

    }
}

func write(cgimage: CGImage, to: URL) throws {
    let cicontext = CIContext()
    let ciimage = CIImage(cgImage: cgimage)
    try cicontext.writePNGRepresentation(of: ciimage, to: url, format: .RGBA8, colorSpace: ciimage.colorSpace!)
}
main()
