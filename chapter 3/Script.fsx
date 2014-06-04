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
| HardLineBreak

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
//
//let rec parseSpans acc chars = seq {
//    let emitLiteral = seq {
//        if acc <> [] then
//            yield acc |> List.rev |> toString |> Literal }    
//            
//    match parseInline chars, chars with
//    | Some(body, chars), _ -> 
//        yield! emitLiteral
//        yield body |> toString |> InlineCode
//        yield! parseSpans [] chars   
//    | _, c::chars ->
//        yield! parseSpans (c::acc) chars
//    | _, [] ->
//        yield! emitLiteral     
//    }





let (|StartsWith|_|) prefix input =
    let rec loop = function
        | p::prefix, r::rest when p = r ->
            loop(prefix, rest)
        | [], rest ->
            Some(rest)
        | _ -> None
    loop(prefix, input)

let rec parseBracketedBody closing acc = function
    | StartsWith closing (rest) ->
        Some(List.rev acc, rest)
    | c::chars ->
        parseBracketedBody closing (c::acc) chars
    | _ -> None

let parseBracketed opening closing = function
    | StartsWith opening chars ->
        parseBracketedBody closing [] chars
    | _ -> None

let (|Delimited|_|) delim = parseBracketed delim delim

let (|Bracketed|_|) opening closing = parseBracketed opening closing

let rec parseSpans acc chars = seq {
    let emitLiteral = seq {
        if acc <> [] then
            yield acc |> List.rev |> toString |> Literal }    
    
    match chars with
    | Delimited ['`'] (body, chars) ->
        yield! emitLiteral
        yield body |> toString |> InlineCode
        yield! parseSpans [] chars
    | Delimited ['*';'*'] (body, chars)
    | Delimited ['_';'_'] (body, chars) ->
        yield! emitLiteral
        yield Strong(parseSpans [] body |> List.ofSeq)
        yield! parseSpans [] chars
    | Delimited ['*'] (body, chars)
    | Delimited ['_'] (body, chars) ->
        yield! emitLiteral
        yield Emphasis(parseSpans [] body |> List.ofSeq)
        yield! parseSpans [] chars
    // Exercise 1: Add parsing of hyperlinks
    | Bracketed ['['] [']'] (body, chars) ->        
        match chars with
        | Bracketed ['('] [')'] (url, chars) ->
            yield! emitLiteral
            yield HyperLink(parseSpans [] body |> List.ofSeq, url |> toString)
            yield! parseSpans [] chars
        | _ -> 
            yield! emitLiteral
            yield body |> toString |> sprintf "[%s]" |> Literal
            yield! parseSpans [] chars
    // Exercise 2: Add support for hard line breaks
    | StartsWith [' ';'\n';'\r'] chars 
    | StartsWith [' ';'\r'] chars 
    | StartsWith [' ';'\n'] chars ->
        yield! emitLiteral
        yield HardLineBreak
        yield! parseSpans [] chars
    | c::chars ->
        yield! parseSpans (c::acc) chars
    | [] ->
        yield! emitLiteral     
    }  


"where **is** the `code` _**Hello**_" |> List.ofSeq |> parseSpans [] |> Array.ofSeq
"" |> List.ofSeq |> parseSpans []

// Exercise 1: Add parsing of hyperlinks
"For [more] information, see [**F#** home page](http://fsharp.net). __Thank you!__" |> List.ofSeq |> parseSpans [] |> Array.ofSeq

// Exercise 2: Add support for hard line breaks
"hello \n\rworld \r!!!" |> List.ofSeq |> parseSpans [] |> Array.ofSeq