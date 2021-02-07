module Symbol
open Fable.React
open Fable.React.Props
open Browser
open Elmish
open Elmish.React
open Helpers
open CommonTypes

type Msg = Unit // no messages needed

type Model = Unit // No model needed

//-------------------------Helper functions for Tick 3-------------------------------------//

// Helper functions that are useful should normally be put into Helpers module
// For Tick3 only put Helper functions here for ease of assessment and feedback
// The obvious helpers to simplify solution are those that create and manipulate SVG elements.
// SVG boilerplate should be greatly reduced in a well-written system.

let posOf x y = {X=x;Y=y} // helper


let makestringlist lst = 
    lst 
    |> List.map (fun i -> i.ToString())

// add your own functions as needed
let makeReactElementList comp lst= 
    let strList = makestringlist lst
    let index = List.indexed strList
    index
    |> List.map 
        (fun (x,y)-> 
            text [
                X (float comp.X + 450.)
                Y ( float comp.Y + (1000./float index.Length) + (float x * 35.))
                Style [
                TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                FontSize (1000 / (index.Length + 30)) 
                FontWeight "Bold"
                Fill "Black"]
                ] [str <| sprintf "%s" y]                
        ) 


//-----------------------------------------------------------------------------------------//


/// write this for Tick3 using your modified ComponentType
/// you may add to type definition in CommonTypes
let makeBusDecoderComponent (pos:XYPos) (w: int) (a: int) (n: int) = 
   {
       X = int pos.X
       Y = int pos.Y
       H = 0
       W = w
       A = a
       N = n
       Type = BusDecoder
   }
    //failwithf "Not implemented"

//-----------------------Elmish functions with no content in Tick3----------------------//

/// For this program init() generates the required result
let init () =
    (), Cmd.none

/// update function does nothing!
let update (msg : Msg) (model : Model): Model*Cmd<'a>  =
    match msg with
    | () -> model, Cmd.none // do nothing if we receive the (only) message

//----------------------------View Function for Symbol----------------------------//

/// Tick3 answer
let busDecoderView (comp: Component) = 
    let fX = float comp.X
    let fY = float comp.Y
    let lst = [comp.A..(comp.A + comp.N - 1)]
    let reactElementList = makeReactElementList comp lst

    let scaleFactor = 1.0 //to demonstrate SVG scaling 
    let rotation = 0
    g   [ Style [
            // the transform here does rotation, scaling, and translation
            // the rotation and scaling happens with TransformOrigin as fixed point first
                TransformOrigin "0px 0px" // so that rotation is around centre of line
                Transform (sprintf "translate(%fpx,%fpx) rotate(%ddeg) scale(%f) " fX fY rotation scaleFactor )
            ]       
        ] 
        [
            rect [ //a svg rectangle
                SVGAttr.Width "500"
                SVGAttr.Height "700"
                SVGAttr.StrokeWidth "2px"
                SVGAttr.Stroke "Black"
                SVGAttr.FillOpacity 0.1
                SVGAttr.Fill "Grey"
                SVGAttr.TextAnchor "middle"
                X fX
                Y fY
            ] []
            
            text [
                X (fX + 220.) 
                Y (fY + 50.)
                Style [
                TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                FontSize "30px"
                FontWeight "Bold"
                Fill "Black" ]// demo font color   
                ] [str <| sprintf "Bus Decode" ]

            text [
                X (fX + 30.)
                Y (fY + 300.)
                Style [
                TextAnchor "middle" // left/right/middle: horizontal algnment vs (X,Y)
                DominantBaseline "hanging" // auto/middle/hanging: vertical alignment vs (X,Y)
                FontSize "30px"
                FontWeight "Bold"
                Fill "Black" ]// demo font color   
                ] [str <| sprintf "In" ]

            g    [ Style [] ] reactElementList

             
        ] 
    
            
        
/// View function - in this case view is independent of model
let view (model : Model) (dispatch : Msg -> unit) =    
    [   // change for Tick3 answer
      makeBusDecoderComponent {X=0.; Y=20.} 4 5 5 // for Tick 3 two components
      makeBusDecoderComponent {X=300.; Y=20.} 3 0 8
    ] 
    |> List.map busDecoderView  // change for Tick3 answer
    |> (fun svgEls -> 
        svg [
            Style [
                Border "3px solid green"
                Height 1000.
                Width 1500.   
            ]
        ]   svgEls )



type ValidateError =
   | WIsInvalid // ignoring a,n
   | AIsInvalid // for given w, ignoring n
   | NIsInvalid // for given a,w
   | TypeInvalid

/// Tick3 answer
let busDecoderValidate (comp:Component) : Result<Component, ValidateError*string> =
    match comp.Type with 
    | BusDecoder -> 
        match comp.W with 
        | w when w <= 0 -> Error (WIsInvalid,"w is out of range!")
        | w ->
            match comp.A with 
            | a when a < 0 || float a > (2.0** float comp.W) - 1.0 -> Error (AIsInvalid, "a is out of range!")
            | a -> 
                match comp.N with 
                | n when n <= 0 || float n > (2.0 ** float comp.W) - float comp.A -> Error (NIsInvalid, "n is out of range!")
                | n -> Ok comp
    | _ -> Error (TypeInvalid, "Not a bus decoder component!")      
    
    



    


