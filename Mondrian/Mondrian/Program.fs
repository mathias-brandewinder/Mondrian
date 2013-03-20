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

/// Maximum points that will be sampled in a box.
let maxSample = 100

/// Converts an index into coordinates.
let coords (box: Box) index =
    let row = index / box.Width
    let col = index % box.Width
    (box.Left + col, box.Bottom + row)

/// Computes a sample of coordinates from a box
let sampler (box: Box) =
    seq {
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
let split (img: Bitmap) (rng: Random) cuts margin box =
    // Construct a list of possible box splits
    let attempts = [
        // Constructs random vertical cuts.
        let minx = min box.Left box.Right
        let maxx = max box.Left box.Right
        if (minx + margin < maxx - margin) then
            for i in 1 .. cuts do
                let cut = rng.Next(minx + margin, maxx + 1 - margin)
                let box1 = { box with Right = cut }
                let box2 = { box with Left = cut + 1 }
                yield (box1, box2)
        // Construct random horizontal cuts.
        let miny = min box.Top box.Bottom
        let maxy = max box.Top box.Bottom
        if (miny + margin < maxy - margin) then
            for i in 1 .. cuts do
                let cut = rng.Next(miny + margin, maxy + 1 - margin)
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
          (splitter: Box -> (Box*Box) option) 
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
let boxize (img: Bitmap) (rng: Random) cuts margin (depth: int) =
    let width = img.Width
    let height = img.Height
    let box = { Left = 0; Right = width - 1; Top = height - 1; Bottom = 0}
    let splitter = split img rng cuts margin
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
/// Paint each box based on its average color.
/// A proportion of the clearest boxes are painted pure white. 
let colorize (img: Bitmap) (rng: Random) (white: float) (contrast: float) (boxes: Box[]) =
    let count = Array.length boxes
    let whitened = (white * (float)count) |> (int)
    let colors = 
        boxes 
        |> Array.map (fun box -> box, average img box)
        |> Array.sortBy (fun (box, color) -> - whiteness color)
        |> Array.mapi (fun i (box, color) ->
              if i < whitened 
              then (box, Color.White)
              else (box, contrastize contrast color))
        |> Array.iter (fun (box, color) ->
            for x in box.Left .. box.Right do
                for y in box.Bottom .. box.Top do
                    let pixel = img.GetPixel(x, y)
                    img.SetPixel(x, y, color))
    img

/// Paint the black borders around each Box.
let borderize (img: Bitmap) (margin: int) (boxes: Box[]) =
    let width = img.Width
    let height = img.Height
    let borders box = seq {
        if margin > 0 then
            if box.Bottom > 0 then
                for x in box.Left .. box.Right do 
                    for m in 0 .. margin do
                        yield (x, box.Bottom + m)
            if box.Top < (height - 1) then
                for x in box.Left .. box.Right do 
                    for m in 0 .. margin do
                        yield (x, box.Top - m) 
            if box.Left > 0 then
                for y in box.Bottom .. box.Top do 
                    for m in 0 .. margin do
                        yield (box.Left + m, y)
            if box.Right < width - 1 then
                for y in box.Bottom .. box.Top do 
                    for m in 0 .. margin do
                        yield (box.Right - m, y) } 
    boxes 
    |> Seq.collect (fun box -> borders box) 
    |> Seq.iter (fun (x, y) -> img.SetPixel(x, y, Color.Black))
    img

/// Utilities to determine adequate black margin width
let marginWidth width height = 
    (min width height) / 200

/// Utility to determine adequate minimum box edge
let minWidth width height = 
    let borders = 2 * (marginWidth width height)
    let edge = (min width height) / 10
    max edge borders

[<EntryPoint>]
let main argv = 
    
    // Replace the image path by something adequate...
    let sourceFile = @"C:\Users\Mathias Brandewinder\Desktop\MonaLisa.png"
    let targetFile = @"C:\Users\Mathias Brandewinder\Desktop\Mondrianized.png"
    
    use image = new Bitmap(sourceFile)
    let width, height = image.Width, image.Height
    let margin = marginWidth width height
    let edges = minWidth width height

    let cuts = 10 // random cuts attempted at each step
    let depth = 50 // "search" depth
    let white = 0.4 // proportion of boxes rendered white
    let contrast = 32. // rounding factor to simplify colors

    let rng = Random()

    let boxes = boxize image rng cuts edges depth    
    let colorized = colorize image rng white contrast boxes
    let borderized = borderize colorized margin boxes

    borderized.Save(targetFile, Imaging.ImageFormat.Png)

    printfn "Done"

    0 // return an integer exit code