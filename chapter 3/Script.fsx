let txt = @"Visual F#
=========
F# is a **programming language** that supports _functional_, as well as _object-oriented_ and _imperative_ programming styles. Hello world can be written as follows:

    `printfn ""Hello world!""` 

For more information, see the [F# home page] (http://fsharp.net) or  read [Real-World Func tional Programming](http://manning.com/petricek)  published by [Manning](http://manning.com)."

type MarkdownDocument = list<MarkdownBlock>
and MarkdownBlock =
| Heading of int * MarkdownSpans   
| Paragraph of MarkdownSpans   
| CodeBlock of list<string>  
and MarkdownSpans = list<MarkdownSpan>
and MarkdownSpan =
| Literal of string   
| InlineCode of string   
| Strong of MarkdownSpans   
| Emphasis of MarkdownSpans   
| HyperLink of MarkdownSpans * string

let rec parseInlineBody acc = function   
    | '`'::rest ->        
        Some(List.rev acc, rest)
    | c::chars  ->        
        parseInlineBody (c::acc) chars
    | [] -> None
    
let parseInline = function   
    |'`'::chars ->       
        parseInlineBody [] chars   
    | _ -> None   

"`code` and" |> List.ofSeq |> parseInline


let toString chars = 
    System.String(chars |> Array.ofList)  

let rec parseSpans acc chars = seq {
    let emitLiteral = seq {
        if acc <> [] then
            yield acc |> List.rev |> toString |> Literal }    
            
    match parseInline chars, chars with
    | Some(body, chars), _ -> 
        yield! emitLiteral
        yield body |> toString |> InlineCode
        yield! parseSpans [] chars   
    | _, c::chars ->
        yield! parseSpans (c::acc) chars
    | _, [] ->
        yield! emitLiteral }