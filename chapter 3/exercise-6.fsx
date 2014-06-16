type MarkdownDocument = list<MarkdownBlock>
and MarkdownBlock =
| Heading of int * MarkdownSpans
| NumberHeading of int * MarkdownSpans
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

let toString chars = 
    System.String(chars |> Array.ofList)  


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
    // `let x = 0`
    | Delimited ['`'] (body, chars) ->
        yield! emitLiteral
        yield body |> toString |> InlineCode
        yield! parseSpans [] chars
    // **strong** __strong__
    | Delimited ['*';'*'] (body, chars)
    | Delimited ['_';'_'] (body, chars) ->
        yield! emitLiteral
        yield Strong(parseSpans [] body |> List.ofSeq)
        yield! parseSpans [] chars
    // *emphasis* _emphasis_
    | Delimited ['*'] (body, chars)
    | Delimited ['_'] (body, chars) ->
        yield! emitLiteral
        yield Emphasis(parseSpans [] body |> List.ofSeq)
        yield! parseSpans [] chars
    // [link](http://link)
    // or [literal]
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
    | PrefixedLines "    " (body, lines) when body <> [] ->
        yield CodeBlock(body)
        yield! parseBlocks lines  
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


let headings =  """# Heading 1
## Sub-heading 1
## Sub-heading 2 
# Heading 2"""

headings.Split('\r','\n')
|> List.ofSeq 
|> parseBlocks
|> List.ofSeq