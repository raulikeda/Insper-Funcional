(* Faça uma função que retorna o segundo maior elemento de uma lista. *)

#use "ex4.ml"

(* Usando a mesma técnica anterior *)

let rec second_max_element list =
  match list with
  | [] -> failwith "empty list"
  | [_] -> failwith "list with only one element"
  | h1 :: (h2 :: []) -> if h1 > h2 then h2 else h1
  | h1 :: (h2 :: t) ->
    let max_tail = max_element t in
    if h1 > max_tail && h2 > max_tail then 
      if h1 > h2 then h2 else h1
    else let h = if h1 > h2 then h1 else h2 in
    second_max_element (h :: t) ;;

(* Alternativa *)

let rec second_max_element_tail first second list =
  match list with
  | [] -> second
  | h :: t -> 
    if h > first then second_max_element_tail h first t
    else if h > second then second_max_element_tail first h t
    else second_max_element_tail first second t

(* Poderia colocar second_max_element_tail dentro também *)
let second_max_element = function
  | [] -> failwith "empty list"
  | [x] -> failwith "list with only one element"
  | h1 :: (h2 :: t) -> if h1 > h2 then second_max_element_tail h1 h2 t else second_max_element_tail h2 h1 t

