﻿let txt = """Visual F#
=========
F# is a **programming language** that supports _functional_, as well as _object-oriented_ and _imperative_ programming styles. Hello world can be written as follows:

    `printfn "Hello world!"` 

For more information, see the [F# home page] (http://fsharp.net) or  read [Real-World Func tional Programming](http://manning.com/petricek)  published by [Manning](http://manning.com)."""

type MarkdownDocument = list<MarkdownBlock>
and MarkdownBlock =
| Heading of int * MarkdownSpans   
| Paragraph of MarkdownSpans   
| CodeBlock of list<string>
| BlockQuote of list<MarkdownBlock>

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


module List = 
    let partitionWhile f = 
        let rec loop acc = function
            | x::xs when f x -> loop (x::acc) xs
            | xs -> List.rev acc, xs
        loop []

let (|PrefixedLines|) (prefix:string) (lines:list<string>) =
    let prefixed, other =
        lines |> List.partitionWhile (fun (line:string) -> line.StartsWith(prefix))
    [ for (line:string) in prefixed ->
        line.Substring(prefix.Length) ], other

let (|LineSeparated|) lines =
    let isWhite = System.String.IsNullOrWhiteSpace
    match List.partitionWhile (isWhite >> not) lines with
    | par, _::rest
    | par, ([] as rest) -> par, rest

let (|AsCharList|) (str:string) =
    List.ofSeq str


let (PrefixedLines "..." res) = ["...1";"...2";"3"]
let (PrefixedLines "..." res) = ["1";"...2";"...3"]
let (PrefixedLines "..." res) = ["...1";"2";"...3"]


// Exercise 3 : Improve and complete parsing of headings
let (|Heading'|_|) = function
    | AsCharList(StartsWith ['#'; ' '] heading)::lines -> Some(1,heading,lines) 
    | AsCharList(StartsWith ['#'; '#'; ' '] heading)::lines -> Some(2,heading,lines) 
    | heading::AsCharList(StartsWith ['-'; '-'; '-'] _)::lines -> Some(1,heading |> List.ofSeq,lines) 
    | heading::AsCharList(StartsWith ['='; '='; '='] _)::lines -> Some(2,heading |> List.ofSeq,lines) 
    | _ -> None

let rec parseBlocks lines = seq {
    match lines with
    | Heading'(size, heading, lines) ->
        yield Heading(size, parseSpans [] heading |> List.ofSeq)
        yield! parseBlocks lines
//    | AsCharList(StartsWith ['#'; ' '] heading)::lines ->
//        yield Heading(1, parseSpans [] heading |> List.ofSeq)
//        yield! parseBlocks lines
//    | AsCharList(StartsWith ['#'; '#'; ' '] heading)::lines ->
//        yield Heading(2, parseSpans [] heading |> List.ofSeq)
//        yield! parseBlocks lines
    | PrefixedLines "    " (body, lines) when body <> [] ->
        yield CodeBlock(body)
        yield! parseBlocks lines  
    // Exercise 4: Add support for block quotes
    | LineSeparated (body, lines) when body <> [] 
            && body.Head.StartsWith(">") 
            && (body |> List.filter(fun s -> s.StartsWith(">")) |> List.length) = 1 ->
        let head::rest = body
        let body = head.Substring(1)::rest
        yield BlockQuote(parseBlocks body |> List.ofSeq)
        yield! parseBlocks lines
    | PrefixedLines ">" (body, lines) when body <> [] ->        
        let body = body |> List.map (fun s -> s.Substring(1))
        yield BlockQuote(parseBlocks body |> List.ofSeq)
        yield! parseBlocks lines
    | LineSeparated (body, lines) when body <> [] ->
        let body = String.concat " " body |> List.ofSeq
        yield Paragraph(parseSpans [] body |> List.ofSeq)
        yield! parseBlocks lines               
    | line::lines when System.String.IsNullOrWhiteSpace(line) ->
        yield! parseBlocks lines    
    | _ -> ()
}

let sample = """# Introducing F#
F# is a _functional-first_ language,
which looks like this:

    let msg = "world"
    printfn "hello %s!" msg

This sample **prints** `hello world!`

## h2 heading

h1 heading
==========
h2 heading
----------
No heading

> This is a hand-written quotation
which consists of two paragraphs.

> The second paragraph is quite short.

> This is a quotation that contains F# code:
> 
>     printfn "Hello from quoted code!"
>     let msg = "world"
>     printfn "hello %s!" msg

> Another *test*
> More test

"""

let sampleDoc = sample.Split('\r','\n') |> List.ofSeq |> parseBlocks |> List.ofSeq

txt.Split('\r','\n') |> List.ofSeq |> parseBlocks |> List.ofSeq

let (PrefixedLines ">" res) = ["> This is a hand-written quotation";"which consists of two paragraphs."]
let (PrefixedLines ">" res) = ["> Another *test*";"> More test"]


"""
> This is a hand-written quotation
which consists of two paragraphs.
""".Split('\r','\n') |> List.ofSeq |> parseBlocks |> List.ofSeq  

"""
> The second paragraph is quite short.
""".Split('\r','\n') |> List.ofSeq |> parseBlocks |> List.ofSeq  

"""
> This is a quotation that contains F# code:
> 
>     printfn "Hello from quoted code!"
>     let msg = "world"
>     printfn "hello %s!" msg
""".Split('\r','\n') |> List.ofSeq |> parseBlocks |> List.ofSeq

"""
> Another *test*
> More test
""".Split('\r','\n') |> List.ofSeq |> parseBlocks |> List.ofSeq



// Markdown to HTML
open System.IO

let outputElement (output:TextWriter) tag attributes body =
    let attrString = 
        [ for k, v in attributes -> k + "=\"" + v + "\""]
        |> String.concat " "
    output.Write("<" + tag + attrString + ">")
    body()
    output.Write("</" + tag + ">")

let rec formatSpan (output:TextWriter) span = 
    let out = outputElement output
    let iter spans = (fun () -> spans |> List.iter (formatSpan output))
    match span with
    | Literal(str) ->
        output.Write(str)
    | Strong(spans) ->
        out "strong" [] (spans |> iter)
    | Emphasis(spans) ->
        out "em" [] (spans |> iter)
    | HyperLink(spans, url) ->
        out "a" ["href", url] (spans |> iter)
    | InlineCode(code) ->
        output.Write("<code>" + code + "</code>")

let rec formatBlock (output:TextWriter) block = 
    let out = outputElement output
    let iter spans = (fun () -> spans |> List.iter (formatSpan output))
    match block with
    | Heading(size, spans) ->
        out ("h" + size.ToString()) [] (spans |> iter)    
    | Paragraph(spans) ->
        out "p" [] (spans |> iter)    
    | CodeBlock(lines) ->
        out "pre" [] (fun() -> lines |> List.iter output.WriteLine)
    | BlockQuote(blocks) ->
        out "quote" [] (fun() -> blocks |> List.iter (formatBlock output))

let sb = System.Text.StringBuilder()
let output = new StringWriter(sb)

sampleDoc |> List.iter (formatBlock output)
sb.ToString()        

module Matching = 
    let (|SpanNode|_|) span =
        match span with
        | Strong spans | Emphasis spans | HyperLink(spans,_) ->
            Some(box span, spans)
        | _ -> None

    let SpanNode (span:obj, children) =
        match unbox span with
        | Strong _ -> Strong children
        | Emphasis _ -> Emphasis children
        | HyperLink(_, url) -> HyperLink(children, url)
        | _ -> invalidArg "" "Incorrect MarkdownSpan"

    let (|BlockNode|_|) block =
        match block with
        | Heading(_,spans)
        | Paragraph(spans) -> Some(box block, spans)
        | _ -> None

    let BlockNode (block:obj, spans) =
        match unbox block with
        | Heading(a, _) -> Heading(a, spans)
        | Paragraph(_) -> Paragraph(spans)
        | _ -> invalidArg "" "Incorrect MarkdownBlock"


let rec generateSpanRefs (refs:ResizeArray<_>) = function
    | HyperLink(body, url) as span -> 
        let id = sprintf "[%d]" (refs.Count + 1)
        refs.Add(id, url)
        [span; Literal(id)]
    | Matching.SpanNode(shape, children) ->        
        let children = children |> List.collect (generateSpanRefs refs)
        [Matching.SpanNode(shape, children)]
    | span -> [span]

let generateBlockRefs refs = function
    | Matching.BlockNode(shape, children) -> 
        let children = children |> List.collect (generateSpanRefs refs)
        Matching.BlockNode(shape, children)
    | block -> block

let doc = 
    [ """For **more** information, see the [F# home page](http://fsharp.net) or read [Real-World Functional Programming](http://manning.com/petricek) published by [Manning](http://manning.com)."""] 
    |> parseBlocks |> List.ofSeq

let refs = ResizeArray<_>()
let printable = doc |> List.map (generateBlockRefs refs)

// Exercise 5: Count the number of words
open System.Text
open System.Text.RegularExpressions

let countWords str = Regex.Matches(str, @"[\S]+").Count

let count = ref 0

let rec countWordsInSpan count = function
    | Literal(str) ->
        printfn "%s" str
        count := !count + (countWords str)
    | Matching.SpanNode(_, children) ->
        children |> List.iter (countWordsInSpan count)
    | _ -> ()

let countWordsInBlock count = function
    | Matching.BlockNode(_, children) ->         
        children |> List.iter (countWordsInSpan count)
    | _ -> ()

doc |> List.iter (countWordsInBlock count)
count := 0
sampleDoc |> List.iter (countWordsInBlock count)

txt.Split('\r','\n') 
|> List.ofSeq 
|> parseBlocks 
|> List.ofSeq
|> List.iter (countWordsInBlock count) // 45 words


