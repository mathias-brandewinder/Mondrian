// (c) Mathias Brandewinder, 2013

open System
open System.Drawing

/// A Square box, identified by its corner coordinates
type Box = 
    { Top: int; 
      Bottom: int; 
      Left: int; 
      Right: int }
    member this.Width = this.Right - this.Left + 1
    member this.Height = this.Top - this.Bottom + 1
    member this.Surface = this.Width * this.Height

type Colorizer = Bitmap -> Box [] -> (Box * Color) []

/// Maximum points that will be sampled in a box.
let maxSample = 100

/// Converts an index into coordinates.
let coords (box: Box) index =
    let row = index / box.Width
    let col = index % box.Width
    (box.Left + col, box.Bottom + row)

/// Computes a sample of coordinates from a box
let sampler (box: Box) = seq {
    if box.Surface <= maxSample
    then
        for x in box.Left .. box.Right do
            for y in box.Bottom .. box.Top do yield (x, y) 
    else
        let step = (float)box.Surface / (float)maxSample
        for i in 0. .. step .. (float)(box.Surface - 1) do
            yield coords box ((int)i) }

/// Compute the "average" color of a box, 
/// by sampling random points in it.
let average (img: Bitmap) (box: Box) =
    let sample = 
        box 
        |> sampler
        |> Seq.map (fun (x, y) -> img.GetPixel(x, y))
    let red = sample |> Seq.averageBy (fun pix -> (float)pix.R)
    let green = sample |> Seq.averageBy (fun pix -> (float)pix.G)
    let blue = sample |> Seq.averageBy (fun pix -> (float)pix.B)
    (red, green, blue)

/// Computes the "distance" between 2 colors.
let inline distance (r1, g1, b1) (r2, g2, b2) =
    pown (r1 - r2) 2 + pown (g1 - g2) 2 + pown (b1 - b2) 2

/// Split a box into 2 parts, by attempting
/// random splits and taking the split that
/// maximizes color difference between the areas.
let split (img: Bitmap) margin box =
    // Construct a list of possible box splits
    let attempts = [
        // Constructs random vertical cuts.
        for cut in box.Left + margin .. box.Right - margin do
                let box1 = { box with Right = cut }
                let box2 = { box with Left = cut + 1 }
                yield (box1, box2)
        // Construct random horizontal cuts.
        for cut in box.Bottom + margin .. box.Top - margin do
                let box1 = { box with Top = cut }
                let box2 = { box with Bottom = cut + 1 }
                yield (box1, box2) ]
    // Extract the cut with largest color difference,
    // if a successful cut has been found.
    match attempts with
    | []    -> None
    | pairs ->
          pairs 
          |> List.maxBy (fun (box1, box2) -> 
                 distance (average img box1) (average img box2))
          |> Some

/// Given a current division of image into boxes,
/// create next generation by splitting a random Box 
/// from the current Boxes.
let spawn (rng: Random) 
          (splitter: Box -> (Box * Box) option) 
          (boxes: Box[]) =
    let count = Array.length boxes
    let boxIndex = rng.Next(count)
    [| for i in 0 .. (count - 1) do
           if i = boxIndex then 
               match (splitter boxes.[i]) with
               | None -> yield boxes.[i]
               | Some(box1, box2) -> 
                   yield box1
                   yield box2
           else yield boxes.[i] |]

/// Recursively create boxes that cover the starting image
let boxize (img: Bitmap) (rng: Random) margin (depth: int) =
    let width = img.Width
    let height = img.Height
    let box = { Left = 0; Right = width - 1; Top = height - 1; Bottom = 0}
    let splitter = split img margin
    let rec fragment boxes gen =
        match (gen >= depth) with
        | true -> boxes
        | false ->
              let moreBoxes = spawn rng splitter boxes
              fragment moreBoxes (gen + 1)
    fragment [| box |] 0

/// Measure whiteness (the higher r,g,b, the whiter).
let whiteness (color: float * float * float) = 
    let r, g, b = color
    min r g |> min b 
/// Round value to the closest multiple of grain .
let roundize value grain =
    let value = grain * (float)(round (value / grain)) |> (int)
    if value > 255 then 255 else value
/// Create a simplified RGB color, using restricted palette.
let contrastize grain (color: float * float * float) =
    let r, g, b = color
    Color.FromArgb(roundize r grain, roundize g grain, roundize b grain)

let colorsPicker (img: Bitmap) (boxes: Box []) =
    boxes
    |> Array.map (fun box -> box, (average img box) |> contrastize 32.)

let fill (img: Bitmap) (box: Box) (color: Color) =
    let graphics = Graphics.FromImage(img)   
    let brush = new SolidBrush(color)
    let rectangle = Rectangle(box.Left, box.Bottom, box.Width, box.Height)
    graphics.FillRectangle(brush, rectangle)      
    
/// Assign a color to each box and paint it.
let colorize (img: Bitmap) (boxes: Box[]) (colorizer: Colorizer) =
    colorizer img boxes
    |> Array.iter (fun (box, color) -> fill img box color)
    img

/// Paint the black borders around each Box.
let borderize (img: Bitmap) (margin: int) (boxes: Box[]) =
    let width = img.Width
    let height = img.Height
    let borders box = seq {
        if margin > 0 then
            if box.Bottom > 0 then
                yield { box with Top = box.Bottom + margin }
            if box.Top < (height - 1) then
                yield { box with Bottom = box.Top - margin }
            if box.Left > 0 then
                yield { box with Right = box.Left + margin }
            if box.Right < width - 1 then
                yield { box with Left = box.Right - margin } }
    boxes 
    |> Seq.collect (fun box -> borders box) 
    |> Seq.iter (fun box -> fill img box Color.Black)
    img

/// Utilities to determine adequate black margin width
let marginWidth width height = 
    1 + (min width height) / 200

/// Utility to determine adequate minimum box edge
let minWidth width height = 
    let borders = 2 * (marginWidth width height)
    let edge = (min width height) / 8
    max edge borders

[<EntryPoint>]
let main argv = 
    
    // Replace the image path by something adequate...
//    let sourceFile = @"C:\Users\Mathias Brandewinder\Desktop\MonaLisa.png"
    let sourceFile = @"C:\Users\Mathias Brandewinder\Desktop\van-gogh.jpg"
    let targetFile = @"C:\Users\Mathias Brandewinder\Desktop\Mondrianized.png"
    
    use image = new Bitmap(sourceFile)
    let width, height = image.Width, image.Height
    let margin = marginWidth width height
    let edges = minWidth width height

    let depth = 30 // "search" depth
    let white = 0. // proportion of boxes rendered white
    let contrast = 32. // rounding factor to simplify colors

    let rng = Random()

    let boxes = boxize image rng edges depth    
    let colorized = colorize image boxes colorsPicker
    let borderized = borderize colorized margin boxes

    borderized.Save(targetFile, Imaging.ImageFormat.Png)

    printfn "Done"

    0 // return an integer exit code