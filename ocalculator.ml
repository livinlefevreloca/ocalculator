open Angstrom

type 'a tree =
    | Val of float
    | Add of ('a tree * 'a tree)
    | Sub of ('a tree * 'a tree)
    | Div of ('a tree * 'a tree)
    | Mul of ('a tree * 'a tree)
    | Paren of 'a tree
;;

let make_tree op f1 f2 =
    match op with
        | "+" -> Add (f1, f2)
        | "-" -> Sub (f1, f2)
        | "*" -> Mul (f1, f2)
        | "/" -> Div (f1, f2)
        | _ -> raise (Invalid_argument "Invalid operator")
;;

let lift_by_order_ops tree =
    match tree with
        (* a * (x + b) -> (a * x) + b *)
        | Mul (Val a, Add (t, Val b)) -> Add (Mul (Val a, t), Val b)
        (* a * (x - b) -> (a * x) - b *)
        | Mul (Val a, Sub (t, Val b)) -> Sub (Mul (Val a, t), Val b)
        (* (x + a) * b -> x + (a * b) *)
        | Mul (Add (t, Val a), Val b) -> Add (t, Mul (Val a, Val b))
        (* (x - a) * b -> x - (a * b) *)
        | Mul (Sub (t, Val a), Val b) -> Sub (t, Mul (Val a, Val b))
        (* a / (x + b) -> (a / x) + b *)
        | Div (Val a, Add (t, Val b)) -> Add (Div (Val a, t), Val b)
        (* a / (x - b) -> (a / x) - b *)
        | Div (Val a, Sub (t, Val b)) -> Sub (Div (Val a, t), Val b)
        (* (x + a) / b -> (x + (a / b) *)
        | Div (Add (t, Val a), Val b) -> Add (t, Div (Val a, Val b))
        (* (x - a) / b -> x - (a / b) *)
        | Div (Sub (t, Val a), Val b) -> Sub (t, Div (Val a, Val b))
        | t -> t

let rec eval tree =
    match tree with
        | Val f -> f
        | Add (f1, f2) -> eval f1 +. eval f2
        | Sub (f1, f2) -> eval f1 -. eval f2
        | Div (f1, f2) -> eval f1 /. eval f2
        | Mul (f1, f2) -> eval f1 *. eval f2
        | Paren ( f ) -> eval f
;;

let is_white_space c =
    match c with
    | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
    | _ -> false
;;

let whitespace = take_while is_white_space ;;

let is_unary_operator c =
    match c with
    | '-' | '+'  -> true
    | _ -> false
;;
let unary_operator_must = take_while1 is_unary_operator ;;
let unary_operator = take_while is_unary_operator ;;

let is_inner_operator c =
    match c with
    | '/' | '*'  -> true
    | _ -> false
;;
let inner_operator =
    take_while1 is_inner_operator
;;
let operator =
    inner_operator <|> unary_operator_must

let is_digit c =
    match c with
    | '0'..'9' -> true
    | _ -> false
;;
let digit = take_while1 is_digit ;;

let next_is c =
    peek_char >>=
    function
        | Some a when a = c -> return ()
        | Some a -> fail ("expected " ^ String.make 1 a)
        | _ -> fail "End of input"

let end_of_expr =
    end_of_input <|> next_is ')'

let take_next_if c =
    peek_char >>=
    function
        | Some a when a = c -> advance 1 >>| fun () -> true
        | _ -> return false


let number =
    unary_operator >>=
    fun op -> digit >>=
    fun num_part1 -> take_next_if '.' >>=
    function
        | false -> return (op ^ num_part1)
        | true -> digit >>=
            fun num_part2 -> return (op ^ num_part1 ^ "." ^ num_part2)
;;

let expr =
    let num_end = number <* whitespace >>= fun num -> end_of_expr >>= fun _ -> return (Val (float_of_string num)) in
    let num = number <* whitespace >>= fun num -> return (Val (float_of_string num)) in
    fix (
        fun expr ->
            let nested_expr = char '(' *> expr <* char ')' >>= fun tree -> return (Paren tree) in
            num_end <|> (
                num <|> nested_expr <* whitespace
                >>= fun exp1 -> operator <* whitespace
                >>= fun op -> num_end <|> nested_expr <|> expr
                >>= fun exp2 -> return (lift_by_order_ops (make_tree op exp1 exp2))
            ) <|> nested_expr
    )


let handle_line line =
    match (parse_string ~consume:Prefix expr line) with
    | Ok tree -> let result = eval tree in Printf.printf "%f\n" result
    | Error _ -> Printf.printf "Error Invalid input: %s\n" line

let rec read_line () =
    print_string "> ";
    flush stdout;
    let handle_line line =
        if line = "exit" then exit 0
        else handle_line line; read_line ()
    in
        let line = input_line stdin
    in
        handle_line line


let () = read_line ()
