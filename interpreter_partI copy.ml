(* util functions *)
let is_lower_case c =
  'a' <= c && c <= 'z'

let is_upper_case c =
  'A' <= c && c <= 'Z'

let is_alpha c =
  is_lower_case c || is_upper_case c

let is_digit c =
  '0' <= c && c <= '9'

let is_alphanum c =
  is_lower_case c ||
  is_upper_case c ||
  is_digit c

let is_blank c =
  String.contains " \012\n\r\t" c

let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
  String.of_seq (List.to_seq ls)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(* end of util functions *)

(* parser combinators *)

type 'a parser = char list -> ('a * char list) option

let parse (p : 'a parser) (s : string) : ('a * char list) option =
  p (explode s)

let pure (x : 'a) : 'a parser =
  fun ls -> Some (x, ls)

let fail : 'a parser = fun ls -> None

let bind (p : 'a parser) (q : 'a -> 'b parser) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> q a ls
  | None -> None

let (>>=) = bind
let (let*) = bind

let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None

let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then Some (x, ls)
    else None
  | _ -> None

let char (c : char) : char parser =
  satisfy (fun x -> x = c)

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

let (>>) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) ->
    (match p2 ls with
     | Some (_, ls) -> Some (x, ls)
     | None -> None)
  | None -> None

let (<<) = seq'

let alt (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls)  -> Some (x, ls)
  | None -> p2 ls

let (<|>) = alt

let map (p : 'a parser) (f : 'a -> 'b) : 'b parser =
  fun ls ->
  match p ls with
  | Some (a, ls) -> Some (f a, ls)
  | None -> None

let (>|=) = map

let (>|) = fun p c -> map p (fun _ -> c)

let rec many (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1 (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let rec many' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c
    then Some ((), ls)
    else None
  | _ -> None

let ws : unit parser =
  (many whitespace) >| ()

let ws1 : unit parser =
  (many1 whitespace) >| ()

let digit : char parser =
  satisfy is_digit

let natural : int parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) ->
    Some (int_of_string (implode xs), ls)
  | _ -> None

let literal (s : string) : unit parser =
  fun ls ->
  let cs = explode s in
  let rec loop cs ls =
    match cs, ls with
    | [], _ -> Some ((), ls)
    | c :: cs, x :: xs ->
      if x = c
      then loop cs xs
      else None
    | _ -> None
  in loop cs ls

let keyword (s : string) : unit parser =
  (literal s) >> ws >| ()

(* end of parser combinators *)
(* Interprets a program written in the Part1 Stack Language.
 * Required by the autograder, do not change its type. *)

type const =
  |Nat of int
  |Name of string
  |Unit

type command = 
  |Push of const |Trace
  |Add|Sub|Mul|Div
  |If_Else of commands*commands
and commands = command list

let parse_name: string parser =
  (satisfy is_alpha <|> char '_')  >>= fun fst ->
  many ((satisfy is_alphanum) <|> char '_' <|> char '\'') >>= fun rest ->
  pure ((implode (fst::rest)))

let parse_unit: const parser =  
  (keyword "()" >>= fun x-> pure (Unit))


let parse_const: const parser =
  (natural >>= fun x -> pure(Nat x)) <|>
  (parse_name >>= fun x -> pure(Name x) ) <|> 
  (parse_unit >>= fun x -> pure Unit)

let push_parser = 
  keyword "Push" >> 
  parse_const << ws >>= fun i ->
  pure (Push i)
let trace_parser = 
  keyword "Trace" >> 
  pure (Trace)
let add_parser = 
  ws >>= fun _ ->
  keyword "Add" >> 
  ws >>= fun _ ->
  pure (Add)
let sub_parser = 
  ws >>= fun _ ->
  keyword "Sub" >> 
  ws >>= fun _ ->
  pure (Sub)
let mult_parser = 
  ws >>= fun _ ->
  keyword "Mul" >> 
  ws >>= fun _ ->
  pure (Mul)
let div_parser = 
  ws >>= fun _ ->
  keyword "Div" >> 
  ws >>= fun _ ->
  pure (Div)


let rec parse_command () = 
  push_parser <|>trace_parser <|> add_parser <|> sub_parser <|> mult_parser <|> div_parser <|> (if_else_parser())
and if_else_parser ()= 
  ws >>= fun _ ->
  keyword "If" >>= fun _ -> 
  ws >>= fun _ ->
  parse_commands() >>= fun branch_t ->
  ws >>= fun _ ->
  keyword "Else" >>= fun _ ->
  ws >>= fun _ ->
  parse_commands() >>= fun branch_f ->
  ws >>= fun _ ->
  pure(If_Else (branch_t, branch_f))

and parse_commands() : (commands parser) = 
  many' parse_command

let string_of (con:const) = 
  match con with
  |Nat a -> string_of_int(a)
  |Name a -> a
  |Unit -> "()"

let absv(i: int) = 
  if i < 0 then (i * -1) else i

let rec eval (p:commands)(output: string list)(con: const list): string*string list = 
  match p,con with
  |Push v::p, con -> eval p output (v :: con)
  |Trace::p, h::t -> eval p (string_of(h)::output) (Unit::t)
  |Add::p, (Nat v2)::(Nat v1) :: con -> eval p output (Nat(v2+v1)::con)
  |Add::p, _ -> "Error",[]
  |Sub::p, (Nat v2)::(Nat v1) :: con ->  eval p output(Nat((absv(v2-v1)))::con)
  |Sub::p, _ -> "Error",[]
  |Mul::p, (Nat v2)::(Nat v1) :: con -> eval p output (Nat(v2*v1)::con)
  |Mul::p, _ -> "Error",[]
  |Div::p, (Nat v2)::(Nat v1) :: con -> if v2 > 0 then eval p output (Nat(v1 / v2)::con) else ("Error", [])
  |Div::p, _ -> "Error",[]
  |If_Else (comm1, comm2)::p,v2::v1:: con -> 
    if v2 = Nat(1) then eval (comm1@p) output con else eval (comm2@p) output con
  |[], con::t -> string_of(con),output
  |_ -> "Error",[]

(* how to delcare nat and other functions*)
let interpreter (src : string) : (string * string list) =
  match parse (parse_commands()) src with
  |Some(cmds, _ ) -> (eval cmds [] []) 
  |None -> ("Error",[])